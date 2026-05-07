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

smi_snomed_ci_csv <- file.path(snomed_dir, "QWeb  SNOMED (Group ID 3187) Severe men_v8.csv")


smi_snomed_ci <- fread(
  smi_snomed_ci_csv,
  sep = ",",
  colClasses = "character",
  na.strings = "",
  select = "SNOMED ConceptId",
  fill = TRUE
)
message("unique SNOMED ConceptId:", uniqueN(smi_snomed_ci$`SNOMED ConceptId`))


# Join qweb SNOMED IDs to CPRD Aurum medical records
matched_medical <- medical[
  smi_snomed_ci,
  on = .(SnomedCTConceptId = `SNOMED ConceptId`),
  nomatch = 0L,
  allow.cartesian = TRUE
]

message("unique medcodeid:", uniqueN(matched_medical$MedCodeId))
message("unique SNOMED ConceptId:", uniqueN(matched_medical$SnomedCTConceptId))

cat("smi_snomed_ci but NOT in matched_medical:\n")
print(setdiff(
  smi_snomed_ci$`SNOMED ConceptId`,
  matched_medical$SnomedCTConceptId
))

cat("\nmatched_medical but NOT in smi_snomed_ci:\n")
print(setdiff(
  matched_medical$SnomedCTConceptId,
  smi_snomed_ci$`SNOMED ConceptId`
))


smi_snomed_ci_exclude <- unique(
  setdiff(
    smi_snomed_ci$`SNOMED ConceptId`,
    matched_medical$SnomedCTConceptId
  )
)

smi_snomed_ci_exclude_dt <- data.table(
  SnomedCTConceptId = smi_snomed_ci_exclude,
  group = "smi_snomed_ci_exclude"
)

fwrite(
  smi_snomed_ci_exclude_dt,
  file.path(snomed_dir, "smi_snomed_ci_exclude.txt"),
  sep = "\t"
)

fwrite(
  matched_medical,
  file.path(snomed_dir, "smi_medcodeids_qweb.txt"),
  sep = "\t"
)
