rm(list = ls())
gc()

library(data.table)

oxcprd_001b_codelist_tools <- getwd()

oxcprd_001b_gold_medcodes <- file.path(oxcprd_001b_codelist_tools, "gold/medcodes")
oxcprd_001b_aurum_medcodes <- file.path(oxcprd_001b_codelist_tools, "aurum/medcodes")

gold_txt <- file.path(oxcprd_001b_gold_medcodes, "medical/medical_202601.txt")
aurum_txt <- file.path(oxcprd_001b_aurum_medcodes, "medical/CPRDAurumMedical_202601.txt")

snomed_dir <- file.path(oxcprd_001b_codelist_tools, "snomed")

medical <- fread(
  aurum_txt,
  sep = "\t",
  colClasses = "character",
  na.strings = "",
  quote = ""
)

dhc_tbi_file <- file.path(snomed_dir, "davidhi-tbi-3350eca9.txt")


dhc_tbi <- fread(
  dhc_tbi_file,
  sep = "\t",
  colClasses = "character",
  na.strings = "",
  select = "SnomedCTConceptId",
  fill = TRUE
)
message("unique SNOMED ConceptId: ", uniqueN(dhc_tbi$`SnomedCTConceptId`))

# Join SNOMED IDs to CPRD Aurum medical records
matched_medical <- medical[
  dhc_tbi,
  on = .(SnomedCTConceptId = `SnomedCTConceptId`),
  nomatch = 0L,
  allow.cartesian = TRUE
]

message("unique medcodeid:", uniqueN(matched_medical$MedCodeId))
message("unique SNOMED ConceptId:", uniqueN(matched_medical$SnomedCTConceptId))

cat("dhc_tbi but NOT in matched_medical:\n")
print(setdiff(
  dhc_tbi$`SnomedCTConceptId`,
  matched_medical$SnomedCTConceptId
))

cat("\nmatched_medical but NOT in dhc_tbi:\n")
print(setdiff(
  matched_medical$SnomedCTConceptId,
  dhc_tbi$`SnomedCTConceptId`
))


dhc_tbi_exclude <- unique(
  setdiff(
    dhc_tbi$`SnomedCTConceptId`,
    matched_medical$SnomedCTConceptId
  )
)

dhc_tbi_exclude_dt <- data.table(
  SnomedCTConceptId = dhc_tbi_exclude,
  group = "dhc_tbi_exclude"
)

fwrite(
  dhc_tbi_exclude_dt,
  file.path(snomed_dir, "dhc_tbi_exclude.txt"),
  sep = "\t"
)

fwrite(
  matched_medical,
  file.path(snomed_dir, "dhc_tbi_medcodeids.txt"),
  sep = "\t"
)
