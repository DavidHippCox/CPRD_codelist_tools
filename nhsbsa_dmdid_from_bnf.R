library(readxl)
library(data.table)

root <- file.path(
  "J:/cprd dementia/cprd projects/oxcprd_001b - cprd alcohol related dementia (case control)",
  "codelist_tools/aurum/dmdcodes"
)

path <- file.path(
  root,
  "nhsbsa_bnf_dmdid_snomed_mapping",
  "BNF Snomed Mapping data 20260422.xlsx"
)

# Read everything as text first (safe for IDs like SNOMED/dmd codes)
dt <- as.data.table(
  read_excel(path, col_types = "text")
)

dt_filtered <- dt[grepl("^(0401|0402|0403)", `BNF Code`)]

map <- c(
  "0401" = "anxiety_and_hypnotics",
  "0402" = "antipsychotics",
  "0403" = "antidepressants"
)

dt_filtered[, drugtypes := map[substr(`BNF Code`, 1, 4)]]

print(head(dt_filtered))

message(nrow(dt_filtered))

dt_final <- dt_filtered[, .(
  `BNF Code`,
  `SNOMED Code`,
  drugtypes,
  `DM+D: Product Description`,
  VTM,
  `VTM Name`
)]

setnames(dt_final, "SNOMED Code", "dmdid")

fwrite(dt_final, file.path(root, "nhsbsa_0401_0402_0403.txt"), sep = "\t")
