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

# Read everything as text first
dt <- as.data.table(read_excel(path, col_types = "text"))

# Rows with BNF code starting 0410010
dt_bf <- dt[grepl("^(0401|0402|0403)", `BNF Code`)]

message(nrow(dt_bf))

# Take all VTM names from dt_bf
vtm_names <- unique(dt_bf$`VTM Name`)
vtm_names <- vtm_names[!is.na(vtm_names) & vtm_names != ""]


# Match those VTM names in dt where BNF Code is missing
dt_vtm_name <- dt[
  (is.na(`BNF Code`) | `BNF Code` == "") &
    `VTM Name` %in% vtm_names
]
message(nrow(dt_vtm_name))

# Union of dt_bf and dt_vtm_name
dt_final <- unique(rbindlist(list(dt_bf, dt_vtm_name), fill = TRUE))
message(nrow(dt_final))

map <- c(
  "0401" = "anxiety_and_hypnotics",
  "0402" = "antipsychotics",
  "0403" = "antidepressants"
)

dt_final[, drugtypes := map[substr(`BNF Code`, 1, 4)]]


# Keep only the columns you want
dt_final <- dt_final[, .(
  `BNF Code`,
  `SNOMED Code`,
  drugtypes,
  `DM+D: Product Description`,
  VTM,
  `VTM Name`
)]

setnames(dt_final, "SNOMED Code", "dmdid")

fwrite(dt_final, file.path(root, "nhsbsa_0401_0402_0403.txt"), sep = "\t")
