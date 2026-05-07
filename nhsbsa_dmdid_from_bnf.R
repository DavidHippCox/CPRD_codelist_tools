rm(list = ls())
gc()
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

dt_filtered <- dt[grepl("^(0411)", `BNF Code`)]


print(head(dt_filtered))

message(nrow(dt_filtered))

dt_final <- dt_filtered[, .(
  `BNF Code`,
  `SNOMED Code`,
  `DM+D: Product Description`,
  VTM,
  `VTM Name`
)]

setnames(dt_final, "SNOMED Code", "dmdid")

fwrite(dt_final, file.path(root, "nhsbsa_0411.txt"), sep = "\t")
