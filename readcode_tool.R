readcode_tool <- function(fileA,
                          fileB,
                          mode = c("intersect", "union", "A_not_B", "B_not_A"),
                          show_source = TRUE,
                          normalise = TRUE) {
  library(data.table)

  mode <- match.arg(mode)

  # ---- Read files ----
  A <- fread(fileA)
  B <- fread(fileB)

  # ---- Helper: extract readcode ----
  extract_readcode <- function(dt, name) {
    if ("cleansed_readcode" %in% names(dt)) {
      rc <- dt[, .(readcode = cleansed_readcode)]
    } else if ("readcode" %in% names(dt)) {
      rc <- dt[, .(readcode = readcode)]
    } else {
      stop(paste0(
        name,
        " must contain 'readcode' or 'cleansed_readcode'"
      ))
    }

    rc[, readcode := as.character(readcode)]
    return(rc)
  }

  # ---- Extract ----
  A_codes <- extract_readcode(A, "File A")
  B_codes <- extract_readcode(B, "File B")

  # ---- Normalisation (important for readcodes) ----
  if (normalise) {
    clean_readcode <- function(x) {
      x <- toupper(x)
      x <- trimws(x)
      return(x)
    }

    A_codes[, readcode := clean_readcode(readcode)]
    B_codes[, readcode := clean_readcode(readcode)]
  }

  # ---- Deduplicate ----
  A_codes <- unique(A_codes)
  B_codes <- unique(B_codes)

  # ---- Add provenance ----
  A_codes[, source := "A"]
  B_codes[, source := "B"]

  # ---- Operations ----
  if (mode == "intersect") {
    result <- merge(A_codes, B_codes, by = "readcode")
    result[, source := "both"]
    result <- result[, .(readcode, source)]
  } else if (mode == "union") {
    result <- rbindlist(list(A_codes, B_codes))
    result <- result[, .(
      source = if (.N > 1) "both" else source[1]
    ), by = readcode]
  } else if (mode == "A_not_B") {
    result <- A_codes[!B_codes, on = "readcode"]
    result[, source := "A"]
  } else if (mode == "B_not_A") {
    result <- B_codes[!A_codes, on = "readcode"]
    result[, source := "B"]
  }

  # ---- Return ----
  if (show_source) {
    return(result[])
  } else {
    return(result[, .(readcode)])
  }
}
