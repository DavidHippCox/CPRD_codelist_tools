rm(list = ls())
gc()
library(data.table)

readcode_tool <- function(fileA,
                          fileB,
                          colA = NULL,
                          colB = NULL,
                          mode = c("intersect", "union", "A_not_B", "B_not_A"),
                          show_source = FALSE,
                          normalise = TRUE,
                          save_dir = NULL,
                          output_file = NULL,
                          lookup_file = NULL,
                          lookup_readcode_col = NULL,
                          lookup_id_col = NULL,
                          log_file = NULL) {
  mode <- match.arg(mode)

  # ---- Read files ----
  A <- fread(fileA, sep = "\t", na.strings = "", quote = "", fill = TRUE, colClasses = "character")
  B <- fread(fileB, sep = "\t", na.strings = "", quote = "", fill = TRUE, colClasses = "character")

  # ---- Column selection ----
  pick_col <- function(dt, col, name) {
    if (!is.null(col)) {
      if (!col %in% names(dt)) {
        stop(paste0("Column '", col, "' not found in ", name))
      }
      return(col)
    }

    if ("CleansedReadCode" %in% names(dt)) {
      return("CleansedReadCode")
    }
    if ("readcode" %in% names(dt)) {
      return("readcode")
    }

    stop(paste0(name, " must contain 'readcode' or 'CleansedReadCode', or specify column manually"))
  }

  colA_used <- pick_col(A, colA, "File A")
  colB_used <- pick_col(B, colB, "File B")

  # ---- Extract ----
  A_codes <- A[, .(readcode = as.character(get(colA_used)))]
  B_codes <- B[, .(readcode = as.character(get(colB_used)))]

  # ---- Normalisation ----
  if (normalise) {
    A_codes[, readcode := toupper(trimws(readcode))]
    B_codes[, readcode := toupper(trimws(readcode))]
  }

  # ---- Deduplicate ----
  A_codes <- unique(A_codes)
  B_codes <- unique(B_codes)

  nA_readcodes <- nrow(A_codes)
  nB_readcodes <- nrow(B_codes)

  # ---- Operations ----
  if (mode == "intersect") {
    result <- A_codes[B_codes, on = "readcode", nomatch = 0][
      , source := "both"
    ]
  } else if (mode == "union") {
    result <- rbindlist(list(
      A_codes[, .(readcode, source = "A")],
      B_codes[, .(readcode, source = "B")]
    ))[
      , .(source = if (.N > 1) "both" else source[1]),
      by = readcode
    ]
  } else if (mode == "A_not_B") {
    result <- A_codes[!B_codes, on = "readcode"][
      , source := "A"
    ]
  } else if (mode == "B_not_A") {
    result <- B_codes[!A_codes, on = "readcode"][
      , source := "B"
    ]
  }


  # ---- Optional: attach lookup ----
  if (!is.null(lookup_file)) {
    lookup <- fread(lookup_file, sep = "\t", na.strings = "", quote = "", fill = TRUE, colClasses = "character")

    if (is.null(lookup_readcode_col)) {
      stop("You must supply lookup_readcode_col")
    }

    if (is.null(lookup_id_col)) {
      stop("You must supply lookup_id_col (e.g. 'MedCodeId' or 'medcode')")
    }

    if (!lookup_id_col %in% names(lookup)) {
      stop(paste0("Column '", lookup_id_col, "' not found in lookup file"))
    }

    # ---- Check uniqueness ----
    dup_ids <- lookup[duplicated(get(lookup_id_col)), get(lookup_id_col)]
    if (length(dup_ids) > 0) {
      stop(paste0(
        "Non-unique IDs in lookup column '", lookup_id_col,
        "'. Example: ", paste(head(unique(dup_ids), 5), collapse = ", ")
      ))
    }

    # ---- Normalise lookup ----
    if (normalise) {
      lookup[, (lookup_readcode_col) := toupper(trimws(get(lookup_readcode_col)))]
    }

    # ---- Safe join (preserves rows) ----
    result <- lookup[result, on = setNames("readcode", lookup_readcode_col)]
  }

  # ---- Timestamp ----
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  # ---- Auto filenames ----
  if (is.null(output_file) && !is.null(save_dir)) {
    output_file <- file.path(save_dir, paste0(mode, ".txt"))
  }

  if (is.null(log_file) && !is.null(save_dir)) {
    log_file <- file.path(save_dir, paste0(mode, "_log.txt"))
  }

  # ---- Write OUTPUT ----
  if (!is.null(output_file)) {
    base <- tools::file_path_sans_ext(output_file)
    ext <- tools::file_ext(output_file)

    out_file_ts <- if (nzchar(ext)) {
      paste0(base, "_", timestamp, ".", ext)
    } else {
      paste0(base, "_", timestamp, ".txt")
    }

    # ---- Sort before writing ----
    if (!is.null(lookup_file) && lookup_id_col %in% names(result)) {
      setorderv(result, lookup_id_col)
    } else {
      setorderv(result, "readcode")
    }

    write_dt <- if (show_source) result else result[, !"source"]
    fwrite(write_dt, sep = "\t", out_file_ts)
    attr(result, "output_file") <- out_file_ts
  }
  # ---- readcode & medcode count after lookup ----
  n_readcode <- uniqueN(result[, get(lookup_readcode_col)])

  if (!is.null(lookup_file) && lookup_id_col %in% names(result)) {
    n_medcode <- uniqueN(result[, get(lookup_id_col)])
  } else {
    n_medcode <- NA_integer_
  }

  # ---- Write LOG ----
  if (!is.null(log_file)) {
    base <- tools::file_path_sans_ext(log_file)
    ext <- tools::file_ext(log_file)

    log_file_ts <- if (nzchar(ext)) {
      paste0(base, "_", timestamp, ".", ext)
    } else {
      paste0(base, "_", timestamp, ".txt")
    }

    log_lines <- c(
      "---- readcode_tool summary ----",
      paste("Timestamp:", timestamp),
      paste("File A:", fileA),
      paste("File B:", fileB),
      paste("Mode:", mode),
      paste("Column used in A:", colA_used),
      paste("Column used in B:", colB_used),
      paste("Normalisation:", ifelse(normalise, "ON", "OFF")),
      paste("Unique readcodes in A:", nA_readcodes),
      paste("Unique readcodes in B:", nB_readcodes),
      paste("Unqiue MedCodeId/medcode in result:", n_medcode),
      paste("Unqiue CleansedReadCode/readcode in result:", n_readcode),
      "--------------------------------"
    )

    writeLines(log_lines, con = log_file_ts)
    attr(result, "log_file") <- log_file_ts
  }

  # ---- Return ----
  if (show_source) {
    return(result[])
  } else {
    return(result[, source := NULL])
  }
}
oxcprd_001b_codelist_tools <- getwd()

oxcprd_001b_gold_read <- file.path(oxcprd_001b_codelist_tools, "gold/readcodes")
oxcprd_001b_aurum_read <- file.path(oxcprd_001b_codelist_tools, "aurum/readcodes")

gold_txt <- file.path(oxcprd_001b_gold_read, "medical/medical_202601.txt")
aurum_txt <- file.path(oxcprd_001b_aurum_read, "medical/CPRDAurumMedical_202601.txt")

group_A <- file.path(oxcprd_001b_gold_read, "v1.5_dementia_codes_GOLD.txt")

group_B <- file.path(aurum_txt)

save_gold <- file.path(oxcprd_001b_gold_read, "compare")

save_aurum <- file.path(oxcprd_001b_aurum_read, "compare")

# A not B
readcode_tool(
  group_A,
  group_B,
  colA = "readcode",
  colB = "CleansedReadCode",
  mode = "A_not_B",
  lookup_file = gold_txt,
  lookup_readcode_col = "readcode",
  lookup_id_col = "medcode",
  save_dir = save_gold
)

# # Union
# readcode_tool(
#   group_A,
#   group_B,
#   colA = "readcode",
#   colB = "CleansedReadCode",
#   mode = "union",
#   lookup_file = aurum_txt,
#   lookup_readcode_col = "CleansedReadCode",
#   lookup_id_col = "MedCodeId",
#   save_dir = save_gold
# )

# # B not A
# readcode_tool(
#   group_A,
#   group_B,
#   colA = "readcode",
#   colB = "CleansedReadCode",
#   mode = "B_not_A",
#   lookup_file = aurum_txt,
#   lookup_readcode_col = "CleansedReadCode",
#   lookup_id_col = "MedCodeId",
#   save_dir = save_gold
# )

# # Intersection
# readcode_tool(
#   group_A,
#   group_B,
#   colA = "readcode",
#   colB = "CleansedReadCode",
#   mode = "intersect",
#   lookup_file = aurum_txt,
#   lookup_readcode_col = "CleansedReadCode",
#   lookup_id_col = "MedCodeId",
#   save_dir = save_gold
# )
