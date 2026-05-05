rm(list = ls())
gc()
library(data.table)


library(data.table)

oxcprd_001b_codelist_tools <- getwd()

group_A_file <- file.path(oxcprd_001b_codelist_tools, "aurum/prodcodes/v0_medication_codes_AURUM.txt")
group_B_file <- file.path(oxcprd_001b_codelist_tools, "aurum/dmdcodes/nhsbsa_0401_0402_0403.txt")
new_group_file <- file.path(oxcprd_001b_codelist_tools, "aurum/prodcodes/union_20260501_110520.txt")


group_A <- fread(
  group_A_file,
  sep = "\t",
  na.strings = "",
  quote = "",
  fill = TRUE,
  select = c("ProdCodeId", "drugtypes", "alcohol_detoxification"),
  colClasses = "character"
)

group_B <- fread(
  group_B_file,
  sep = "\t",
  na.strings = "",
  quote = "",
  fill = TRUE,
  select = c("dmdid", "drugtypes"),
  colClasses = "character"
)

new_group <- fread(
  new_group_file,
  sep = "\t",
  na.strings = "",
  quote = "",
  fill = TRUE,
  colClasses = "character"
)

# Remove 'source' column safely
for (dt in list(group_A, group_B, new_group)) {
  if ("source" %in% names(dt)) {
    dt[, source := NULL]
  }
}

# Ensure columns exist before joins
if (!"drugtypes" %in% names(new_group)) {
  new_group[, drugtypes := NA_character_]
}
if (!"alcohol_detoxification" %in% names(new_group)) {
  new_group[, alcohol_detoxification := NA_character_]
}

# then join group_A onto that result by ProdCodeId
new_group[group_A,
  on = .(ProdCodeId),
  drugtypes := fcoalesce(drugtypes, i.drugtypes)
]

new_group[group_A,
  on = .(ProdCodeId),
  drugtypes := fcoalesce(alcohol_detoxification, i.alcohol_detoxification)
]

# update join: only fill missing drugtypes
new_group[group_B,
  on = .(dmdid),
  drugtypes := fcoalesce(drugtypes, i.drugtypes)
]

setorder(new_group, ProdCodeId)

fwrite(new_group, file.path(oxcprd_001b_codelist_tools, "aurum/prodcodes/v0_medication_codes_AURUM.txt"), sep = "\t")
