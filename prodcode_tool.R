rm(list = ls())
gc()
library(data.table)

prodcode_tool <- function(fileA,
                          fileB,
                          mode = c("intersect", "union", "A_not_B", "B_not_A"),
                          show_source = FALSE,
                          save_dir = NULL,
                          output_file = NULL,
                          log_file = NULL,
                          lookup_file = NULL) {
  mode <- match.arg(mode)

  # ---- Read files ----
  A <- fread(fileA, sep = "\t", na.strings = "", quote = "", fill = TRUE, colClasses = "character")
  B <- fread(fileB, sep = "\t", na.strings = "", quote = "", fill = TRUE, colClasses = "character")

  # ---- Detect type ----
  get_type <- function(dt) {
    if ("prodcode" %in% names(dt)) {
      return("GOLD")
    }
    if ("ProdCodeId" %in% names(dt) || "prodcodeid" %in% names(dt)) {
      return("AURUM")
    }
    return(NA_character_)
  }

  typeA <- get_type(A)
  typeB <- get_type(B)

  if (is.na(typeA)) stop("File A must contain 'prodcode' or 'ProdCodeId'")
  if (is.na(typeB)) stop("File B must contain 'prodcode' or 'ProdCodeId'")

  if (typeA != typeB) {
    stop(paste0(
      "Mismatch: File A is ", typeA,
      " and File B is ", typeB,
      ". Cannot combine GOLD and AURUM codelists."
    ))
  }

  # ---- Standardise to prodcode ----
  standardise_codes <- function(dt, type) {
    if (type == "GOLD") {
      dt[, .(prodcode = as.character(prodcode))]
    } else {
      id_col <- if ("ProdCodeId" %in% names(dt)) "ProdCodeId" else "prodcodeid"
      dt[, .(prodcode = as.character(get(id_col)))]
    }
  }

  A_codes <- unique(standardise_codes(A, typeA))
  B_codes <- unique(standardise_codes(B, typeB))

  nA <- nrow(A_codes)
  nB <- nrow(B_codes)

  # ---- Operations ----
  if (mode == "intersect") {
    result <- A_codes[B_codes, on = "prodcode", nomatch = 0][
      , source := "both"
    ]
  } else if (mode == "union") {
    result <- rbindlist(list(
      A_codes[, .(prodcode, source = "A")],
      B_codes[, .(prodcode, source = "B")]
    ))[
      , .(source = if (.N > 1) "both" else source[1]),
      by = prodcode
    ]
  } else if (mode == "A_not_B") {
    result <- A_codes[!B_codes, on = "prodcode"][
      , source := "A"
    ]
  } else if (mode == "B_not_A") {
    result <- B_codes[!A_codes, on = "prodcode"][
      , source := "B"
    ]
  }

  n_result <- nrow(result)

  # ---- Optional: attach lookup ----
  if (!is.null(lookup_file)) {
    lookup <- fread(
      lookup_file,
      sep = "\t",
      na.strings = "",
      quote = "",
      fill = TRUE,
      colClasses = "character"
    )

    # ---- Detect lookup type ----
    lookup_type <- if ("prodcode" %in% names(lookup)) {
      "GOLD"
    } else if ("ProdCodeId" %in% names(lookup) || "prodcodeid" %in% names(lookup)) {
      "AURUM"
    } else {
      NA_character_
    }

    if (is.na(lookup_type)) {
      stop("Lookup file must contain 'prodcode' or 'ProdCodeId'")
    }

    # ---- Enforce same type ----
    if (lookup_type != typeA) {
      stop(paste0(
        "Lookup type mismatch: lookup is ", lookup_type,
        " but codelist type is ", typeA,
        ". Cannot mix GOLD and AURUM."
      ))
    }

    # ---- Standardise lookup ----
    if (lookup_type == "GOLD") {
      lookup[, prodcode := as.character(prodcode)]
    } else {
      id_col <- if ("ProdCodeId" %in% names(lookup)) "ProdCodeId" else "prodcodeid"
      setnames(lookup, id_col, "prodcode")
      lookup[, prodcode := as.character(prodcode)]
    }

    # ---- Check uniqueness ----
    dup_ids <- lookup[duplicated(prodcode), prodcode]
    if (length(dup_ids) > 0) {
      stop(paste0(
        "Non-unique IDs in lookup 'prodcode'. Example: ",
        paste(head(unique(dup_ids), 5), collapse = ", ")
      ))
    }

    # ---- Safe join ----
    result <- lookup[result, on = "prodcode"]
  }

  # ---- Restore original column name for AURUM ----
  if (typeA == "AURUM") {
    setnames(result, "prodcode", "ProdCodeId")
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

    fwrite(result, sep = "\t", out_file_ts)
    attr(result, "output_file") <- out_file_ts
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
      "---- prodcode_tool summary ----",
      paste("Timestamp:", timestamp),
      paste("File A:", fileA),
      paste("File B:", fileB),
      paste("Type:", typeA),
      paste("Mode:", mode),
      paste("Unique codes in A:", nA),
      paste("Unique codes in B:", nB),
      paste("Codes in result:", n_result),
      if (!is.null(lookup_file)) paste("Lookup file:", lookup_file),
      "--------------------------------"
    )

    writeLines(log_lines, con = log_file_ts)
    attr(result, "log_file") <- log_file_ts
  }

  attr(result, "codelist_type") <- typeA

  # ---- Return ----
  if (show_source) {
    return(result[])
  } else {
    if (typeA == "AURUM") {
      return(result[, .(ProdCodeId)])
    } else {
      return(result[, .(prodcode)])
    }
  }
}

oxcprd_001b_codelist_tools <- file.path("J:/cprd dementia/cprd projects/oxcprd_001b - cprd alcohol related dementia (case control)/codelist_tools")

oxcprd_001b_gold_prodcodes <- file.path(oxcprd_001b_codelist_tools, "gold/prodcodes")
oxcprd_001b_aurum_prodcodes <- file.path(oxcprd_001b_codelist_tools, "aurum/prodcodes")

gold_txt <- file.path(oxcprd_001b_gold_prodcodes, "product/product_202601.txt")
aurum_txt <- file.path(oxcprd_001b_aurum_prodcodes, "product/CPRDAurumProduct_202601.txt")

group_A <- file.path(oxcprd_001b_aurum_prodcodes, "nhsbsa_0401_0402_0403_aurum.txt")

group_B <- file.path(oxcprd_001b_aurum_prodcodes, "v0_medication_codes_AURUM.txt")

save_aurum <- file.path(oxcprd_001b_aurum_prodcodes, "compare")
# ---- Choose lookup depending on type ----
# lookup_file <- gold_txt
lookup_file <- aurum_txt


# # ---- A not B ----
# prodcode_tool(
#   group_A,
#   group_B,
#   mode = "A_not_B",
#   save_dir = save_aurum,
#   lookup_file = lookup_file
# )

# ---- Union ----
prodcode_tool(
  group_A,
  group_B,
  mode = "union",
  save_dir = save_aurum,
  lookup_file = lookup_file
)

# ---- B not A ----
# prodcode_tool(
#   group_A,
#   group_B,
#   mode = "B_not_A",
#   save_dir = save_aurum,
#   lookup_file = lookup_file
# )

# ---- Intersection ----
# prodcode_tool(
#   group_A,
#   group_B,
#   mode = "intersect",
#   save_dir = save_aurum,
#   lookup_file = lookup_file
# )
