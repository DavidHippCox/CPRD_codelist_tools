library(data.table)

icd10_tool <- function(fileA,
                       fileB,
                       mode = c("intersect", "union", "A_not_B", "B_not_A"),
                       show_source = TRUE,
                       normalise = TRUE) {
  library(data.table)

  mode <- match.arg(mode)

  # ---- Read files ----
  A <- fread(fileA)
  B <- fread(fileB)

  # ---- Validate ----
  if (!"icd10" %in% names(A)) {
    stop("File A must contain an 'icd10' column")
  }
  if (!"icd10" %in% names(B)) {
    stop("File B must contain an 'icd10' column")
  }

  # ---- Normalisation (important for ICD10) ----
  if (normalise) {
    clean_icd10 <- function(x) {
      x <- toupper(x)
      x <- trimws(x)
      return(x)
    }

    A[, icd10 := clean_icd10(icd10)]
    B[, icd10 := clean_icd10(icd10)]
  }

  # ---- Deduplicate ----
  A_codes <- unique(A[, .(icd10)])
  B_codes <- unique(B[, .(icd10)])

  # ---- Add provenance ----
  A_codes[, source := "A"]
  B_codes[, source := "B"]

  # ---- Operations ----
  if (mode == "intersect") {
    result <- merge(A_codes, B_codes, by = "icd10")
    result[, source := "both"]
    result <- result[, .(icd10, source)]
  } else if (mode == "union") {
    result <- rbindlist(list(A_codes, B_codes))
    result <- result[, .(
      source = if (.N > 1) "both" else source[1]
    ), by = icd10]
  } else if (mode == "A_not_B") {
    result <- A_codes[!B_codes, on = "icd10"]
    result[, source := "A"]
  } else if (mode == "B_not_A") {
    result <- B_codes[!A_codes, on = "icd10"]
    result[, source := "B"]
  }

  # ---- Return ----
  if (show_source) {
    return(result[])
  } else {
    return(result[, .(icd10)])
  }
}
