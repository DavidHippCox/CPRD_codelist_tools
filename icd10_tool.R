rm(list = ls())
gc()
library(data.table)

icd10_tool <- function(fileA,
                       fileB,
                       mode = c("intersect", "union", "A_not_B", "B_not_A"),
                       show_source = TRUE,
                       normalise = TRUE,
                       save_dir = NULL,
                       output_file = NULL,
                       log_file = NULL,
                       lookup_file = NULL) {
  mode <- match.arg(mode)

  # ---- Read files ----
  A <- fread(
    fileA,
    sep = "\t",
    na.strings = "",
    quote = "",
    fill = TRUE,
    colClasses = "character"
  )

  B <- fread(
    fileB,
    sep = "\t",
    na.strings = "",
    quote = "",
    fill = TRUE,
    colClasses = "character"
  )

  # ---- Validate ----
  if (!"icd10" %in% names(A)) {
    stop("File A must contain an 'icd10' column")
  }

  if (!"icd10" %in% names(B)) {
    stop("File B must contain an 'icd10' column")
  }

  # ---- Normalisation helper ----
  clean_icd10 <- function(x) {
    x <- as.character(x)

    if (normalise) {
      x <- toupper(trimws(x))
    }

    return(x)
  }

  # ---- Standardise ----
  A_codes <- unique(A[, .(icd10 = clean_icd10(icd10))])
  B_codes <- unique(B[, .(icd10 = clean_icd10(icd10))])

  nA <- nrow(A_codes)
  nB <- nrow(B_codes)

  # ---- Operations ----
  if (mode == "intersect") {
    result <- A_codes[B_codes, on = "icd10", nomatch = 0][
      , source := "both"
    ]
  } else if (mode == "union") {
    result <- rbindlist(list(
      A_codes[, .(icd10, source = "A")],
      B_codes[, .(icd10, source = "B")]
    ))[
      , .(source = if (.N > 1) "both" else source[1]),
      by = icd10
    ]
  } else if (mode == "A_not_B") {
    result <- A_codes[!B_codes, on = "icd10"][
      , source := "A"
    ]
  } else if (mode == "B_not_A") {
    result <- B_codes[!A_codes, on = "icd10"][
      , source := "B"
    ]
  }

  n_result <- nrow(result)

  # ---- Optional: attach lookup metadata ----
  if (!is.null(lookup_file)) {
    lookup <- fread(
      lookup_file,
      sep = "\t",
      na.strings = "",
      quote = "",
      fill = TRUE,
      colClasses = "character"
    )

    if (!"icd10" %in% names(lookup)) {
      stop("Lookup file must contain an 'icd10' column")
    }

    # ---- Normalise lookup key ----
    lookup[, icd10 := clean_icd10(icd10)]

    # ---- Check uniqueness ----
    dup_ids <- lookup[duplicated(icd10), icd10]

    if (length(dup_ids) > 0) {
      stop(paste0(
        "Non-unique IDs in lookup 'icd10'. Example: ",
        paste(head(unique(dup_ids), 5), collapse = ", ")
      ))
    }

    # ---- Safe join: preserve all result rows ----
    result <- lookup[result, on = "icd10"]
  }

  # ---- Timestamp ----
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

  # ---- Auto-generate filenames ----
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

    fwrite(result, out_file_ts)
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
      "---- icd10_tool summary ----",
      paste("Timestamp:", timestamp),
      paste("File A:", fileA),
      paste("File B:", fileB),
      paste("Mode:", mode),
      paste("Normalisation:", ifelse(normalise, "ON", "OFF")),
      paste("Unique codes in A:", nA),
      paste("Unique codes in B:", nB),
      paste("Codes in result:", n_result),
      if (!is.null(lookup_file)) paste("Lookup file:", lookup_file),
      "--------------------------------"
    )

    writeLines(log_lines, con = log_file_ts)
    attr(result, "log_file") <- log_file_ts
  }

  attr(result, "codelist_type") <- "ICD10"

  # ---- Return ----
  if (show_source) {
    return(result[])
  } else {
    return(result[, .(icd10)])
  }
}

oxcprd_001b_codelist_tools <- file.path("J:/cprd dementia/cprd projects/oxcprd_001b - cprd alcohol related dementia (case control)/codelist_tools")
oxcprd_001b_icd10 <- file.path(oxcprd_001b_codelist_tools, "icd10")

icd10_txt <- file.path(oxcprd_001b_icd10, "icd10.txt")
group_A <- file.path(oxcprd_001b_gold, "")

group_B <- file.path(oxcprd_001b_gold, "")

save_gold <- file.path(oxcprd_001b_gold, "")

# A not B
medcode_tool(
  group_A,
  group_B,
  mode = "A_not_B",
  save_dir = save_gold
)

# Union
medcode_tool(
  group_A,
  group_B,
  mode = "union",
  save_dir = save_gold
)

# B not A
medcode_tool(
  group_A,
  group_B,
  mode = "B_not_A",
  save_dir = save_gold
)

# Intersection
medcode_tool(
  group_A,
  group_B,
  mode = "intersect",
  save_dir = save_gold
)
