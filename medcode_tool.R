library(data.table)

codelist_tool <- function(fileA,
                          fileB,
                          mode = c("intersect", "union", "A_not_B", "B_not_A"),
                          show_source = TRUE) {
  library(data.table)

  mode <- match.arg(mode)

  # ---- Read files ----
  A <- fread(fileA)
  B <- fread(fileB)

  # ---- Detect type ----
  get_type <- function(dt) {
    if ("medcode" %in% names(dt)) {
      return("GOLD")
    }
    if ("medcodeid" %in% names(dt)) {
      return("AURUM")
    }
    return(NA_character_)
  }

  typeA <- get_type(A)
  typeB <- get_type(B)

  # ---- Validate presence ----
  if (is.na(typeA)) stop("File A must contain 'medcode' (GOLD) or 'medcodeid' (AURUM)")
  if (is.na(typeB)) stop("File B must contain 'medcode' (GOLD) or 'medcodeid' (AURUM)")

  # ---- Enforce same system ----
  if (typeA != typeB) {
    stop(paste0(
      "Mismatch: File A is ", typeA,
      " and File B is ", typeB,
      ". Cannot combine GOLD and AURUM codelists."
    ))
  }

  # ---- Standardise column name ----
  if (typeA == "GOLD") {
    A_codes <- A[, .(medcode = as.character(medcode))]
    B_codes <- B[, .(medcode = as.character(medcode))]
  } else {
    A_codes <- A[, .(medcode = as.character(medcodeid))]
    B_codes <- B[, .(medcode = as.character(medcodeid))]
  }

  # ---- Deduplicate ----
  A_codes <- unique(A_codes)
  B_codes <- unique(B_codes)

  # ---- Add provenance ----
  A_codes[, source := "A"]
  B_codes[, source := "B"]

  # ---- Operations ----
  if (mode == "intersect") {
    result <- merge(A_codes, B_codes, by = "medcode")
    result[, source := "both"]
    result <- result[, .(medcode, source)]
  } else if (mode == "union") {
    result <- rbindlist(list(A_codes, B_codes))
    result <- result[, .(
      source = if (.N > 1) "both" else source[1]
    ), by = medcode]
  } else if (mode == "A_not_B") {
    result <- A_codes[!B_codes, on = "medcode"]
    result[, source := "A"]
  } else if (mode == "B_not_A") {
    result <- B_codes[!A_codes, on = "medcode"]
    result[, source := "B"]
  }

  # ---- Attach metadata (nice touch) ----
  attr(result, "codelist_type") <- typeA

  # ---- Return ----
  if (show_source) {
    return(result[])
  } else {
    return(result[, .(medcode)])
  }
}

oxcprd_001b_gold <- file.path("J:/cprd dementia/cprd projects/oxcprd_001b - cprd alcohol related dementia (case control)/codelists/gold")

group_A <- file.path(oxcprd_001b_gold, "medcodes/v1_alcohol_related_psychiatric_codes_GOLD.txt")

group_B <- file.path(oxcprd_001b_gold, "medcodes/v2.1_alcohol_codes_GOLD.txt")

save_gold <- file.path(oxcprd_001b_gold, "compare")

# Intersection

intersection_AB <- codelist_tool(group_A, group_B, mode = "intersect")

# # Union
# codelist_tool("groupA.txt", "groupB.txt", mode = "union")

# # A not B
# codelist_tool("groupA.txt", "groupB.txt", mode = "A_not_B")

# # Only medcodes
# codelist_tool("groupA.txt", "groupB.txt",
#   mode = "union",
#   show_source = FALSE
# )


fwrite(intersection_AB, file.path(save_gold, "intersection_AB.txt"))
