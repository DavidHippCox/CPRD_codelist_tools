rm(list = ls())
gc()
library(data.table)

#----------------------------
# File paths
#----------------------------
icd10_backwards_csv <- file.path(
  "K:/QNAP2/CPRD/24_004516 BDI AT/NHS ICD-10 5th Edition lookup 2025.11.17/ICD10_Edition5_XML_20160401/Content/ICD10_Edition5_TableOfCodingEquivalencesWithDescriptionsBackward_GB_20160401.csv"
)

icd10_altcodes_csv <- file.path(
  "K:/QNAP2/CPRD/24_004516 BDI AT/NHS ICD-10 5th Edition lookup 2025.11.17/ICD10_Edition5_XML_20160401/Content/ICD10_Edition5_CodesAndTitlesAndMetadata_GB_20160401.csv"
)

#----------------------------
# Read data
#----------------------------
idc10_backwards <- fread(
  icd10_backwards_csv,
  sep = ",",
  colClasses = "character",
  na.strings = "",
)

idc10_altcodes <- fread(
  icd10_altcodes_csv,
  sep = ",",
  colClasses = "character",
  na.strings = "",
)

idc10_altcodes <- idc10_altcodes[, .(code, alt_code, usage)]

#----------------------------
# Prepare lookup table
#----------------------------
icd_lookup <- copy(idc10_backwards)

# Use code_from as main code
icd_lookup[, code := code_from]

message("rows of icd10_lookup: ", nrow(icd_lookup))
#----------------------------
# clean codes ending with .X or D
#----------------------------
clean_icd <- function(x) {
  # Remove .X plus anything after (e.g. .XA, .XD)
  x <- sub("\\.X.*$", "", x)

  # Then remove any trailing single letter (safety)
  x <- sub("[A-Z]$", "", x)

  x
}

icd_lookup[, code := clean_icd(code)]

#----------------------------
# Merge alt_code + usage onto lookup
#----------------------------
setkey(icd_lookup, code)

icd_lookup[
  idc10_altcodes,
  on = "code",
  `:=`(
    alt_code = i.alt_code,
    usage    = i.usage
  )
]

message("rows of icd10_lookup after merge: ", nrow(icd_lookup))

col_order <- c("code", "alt_code", "usage", "code_from", "description", "code_to", "different_target", "character", "targetdate", "srcdate")
setcolorder(icd_lookup, col_order)

fwrite(icd_lookup, sep = "\t", file.path("J:/cprd dementia/cprd projects/oxcprd_001b - cprd alcohol related dementia (case control)/codelist_tools/icd10/icd10.txt"))
#----------------------------
# sanity checks
#----------------------------

# Check for unmatched codes (should be minimal/none)
unmatched <- icd_lookup[is.na(alt_code)]
print(nrow(unmatched))
print(unmatched)

# Check duplicates in altcodes (important for data integrity)
dup_check <- idc10_altcodes[, .N, by = code][N > 1]
print(dup_check)
