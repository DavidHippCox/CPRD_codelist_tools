rm(list = ls())
gc()
library(data.table)

dmdcode_tool <- function(fileA,
                         fileB,
                         colA = NULL,
                         colB = NULL,
                         mode = c("intersect", "union", "A_not_B", "B_not_A"),
                         show_source = FALSE,
                         save_dir = NULL,
                         output_file = NULL,
                         lookup_file = NULL,
                         lookup_dmdcode_col = NULL,
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

    if ("dmdid" %in% names(dt)) {
      return("dmdid")
    }
    if ("dmdcode" %in% names(dt)) {
      return("dmdcode")
    }

    stop(paste0(name, " must contain 'dmdcode' or 'dmdid', or specify column manually"))
  }

  colA_used <- pick_col(A, colA, "File A")
  colB_used <- pick_col(B, colB, "File B")

  # ---- Extract ----
  A_codes <- A[, .(dmdcode = as.character(get(colA_used)))]
  B_codes <- B[, .(dmdcode = as.character(get(colB_used)))]

  # ---- Deduplicate ----
  A_codes <- unique(A_codes)
  B_codes <- unique(B_codes)

  nA_dmdcodes <- nrow(A_codes)
  nB_dmdcodes <- nrow(B_codes)

  # ---- Operations ----
  if (mode == "intersect") {
    result <- A_codes[B_codes, on = "dmdcode", nomatch = 0][
      , source := "both"
    ]
  } else if (mode == "union") {
    result <- rbindlist(list(
      A_codes[, .(dmdcode, source = "A")],
      B_codes[, .(dmdcode, source = "B")]
    ))[
      , .(source = if (.N > 1) "both" else source[1]),
      by = dmdcode
    ]
  } else if (mode == "A_not_B") {
    result <- A_codes[!B_codes, on = "dmdcode"][
      , source := "A"
    ]
  } else if (mode == "B_not_A") {
    result <- B_codes[!A_codes, on = "dmdcode"][
      , source := "B"
    ]
  }


  # ---- Optional: attach lookup ----
  if (!is.null(lookup_file)) {
    lookup <- fread(lookup_file, sep = "\t", na.strings = "", quote = "", fill = TRUE, colClasses = "character")

    if (is.null(lookup_dmdcode_col)) {
      stop("You must supply lookup_dmdcode_col")
    }

    if (is.null(lookup_id_col)) {
      stop("You must supply lookup_id_col (e.g. 'ProdCodeId' or 'prodcode')")
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

    # ---- Safe join (preserves rows) ----
    result <- lookup[result, on = setNames("dmdcode", lookup_dmdcode_col)]
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

    write_dt <- if (show_source) result else result[, !"source"]
    fwrite(write_dt, sep = "\t", out_file_ts)
    attr(result, "output_file") <- out_file_ts
  }
  # ---- dmdcode & prodcode count after lookup ----
  n_dmdcode <- uniqueN(result[, get(lookup_dmdcode_col)])

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
      "---- dmdcode_tool summary ----",
      paste("Timestamp:", timestamp),
      paste("File A:", fileA),
      paste("File B:", fileB),
      paste("Mode:", mode),
      paste("Column used in A:", colA_used),
      paste("Column used in B:", colB_used),
      paste("Unique dmdcodes in A:", nA_dmdcodes),
      paste("Unique dmdcodes in B:", nB_dmdcodes),
      paste("Unqiue ProdCodeId/prodcode in result:", n_medcode),
      paste("Unqiue dmdid/dmdcode in result:", n_dmdcode),
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

oxcprd_001b_gold_dmd <- file.path(oxcprd_001b_codelist_tools, "gold/dmdcodes")
oxcprd_001b_aurum_dmd <- file.path(oxcprd_001b_codelist_tools, "aurum/dmdcodes")

gold_txt <- file.path(oxcprd_001b_gold_dmd, "product/product_202601.txt")
aurum_txt <- file.path(oxcprd_001b_aurum_dmd, "product/CPRDAurumProduct_202601.txt")

# group_A <- file.path(oxcprd_001b_gold_dmd, "v1.2_dementia_drugs_GOLD.txt")
group_A <- file.path(oxcprd_001b_aurum_dmd, "nhsbsa_0411.txt")

group_B <- file.path(aurum_txt)

save_gold <- file.path(oxcprd_001b_gold_dmd, "compare")

save_aurum <- file.path(oxcprd_001b_aurum_dmd, "compare")

# # A not B
# dmdcode_tool(
#   group_A,
#   group_B,
#   colA = "dmdid",
#   colB = "dmdid",
#   mode = "A_not_B",
#   lookup_file = aurum_txt,
#   lookup_dmdcode_col = "dmdid",
#   lookup_id_col = "ProdCodeId",
#   save_dir = save_aurum
# )

# # Union
# dmdcode_tool(
#   group_A,
#   group_B,
#   colA = "dmdcode",
#   colB = "dmdid",
#   mode = "union",
#   lookup_file = aurum_txt,
#   lookup_dmdcode_col = "dmdid",
#   lookup_id_col = "ProdCodeId",
#   save_dir = save_gold
# )

# # B not A
# dmdcode_tool(
#   group_A,
#   group_B,
#   colA = "dmdcode",
#   colB = "dmdid",
#   mode = "B_not_A",
#   lookup_file = aurum_txt,
#   lookup_dmdcode_col = "dmdid",
#   lookup_id_col = "ProdCodeId",
#   save_dir = save_aurum
# )

# Intersection
dmdcode_tool(
  group_A,
  group_B,
  colA = "dmdid",
  colB = "dmdid",
  mode = "intersect",
  lookup_file = aurum_txt,
  lookup_dmdcode_col = "dmdid",
  lookup_id_col = "ProdCodeId",
  save_dir = save_aurum
)
