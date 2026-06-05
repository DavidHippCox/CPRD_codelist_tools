rm(list = ls())
gc()

library(data.table)

# --------------------------------------------------------------------
# Paths
# --------------------------------------------------------------------
project_root <- getwd()

aurum_prodcodes_dir <- file.path(project_root, "aurum/prodcodes")
aurum_products_file <- file.path(
  aurum_prodcodes_dir,
  "product/CPRDAurumProduct_202601.txt"
)

bnf_prefix <- "0411"

output_file <- file.path(
  aurum_prodcodes_dir,
  paste0("product_", bnf_prefix, ".txt")
)

# --------------------------------------------------------------------
# Helper functions
# --------------------------------------------------------------------
normalise_slash_text <- function(x) {
  x <- trimws(x)
  x <- gsub("\\s*/\\s*", "/", x)
  x
}

split_substances <- function(x) {
  if (length(x) == 0L || all(is.na(x))) return(character(0))
  values <- unlist(strsplit(x, "\\s*/\\s*"), use.names = FALSE)
  values <- trimws(values)
  values <- values[!is.na(values) & values != ""]
  values
}

# --------------------------------------------------------------------
# Read data
# --------------------------------------------------------------------
products <- fread(
  aurum_products_file,
  sep = "\t",
  colClasses = "character",
  na.strings = "",
  quote = ""
)

original_columns <- names(products)

# --------------------------------------------------------------------
# Normalise key text fields
# --------------------------------------------------------------------
products[, bnf_chapter_norm := normalise_slash_text(BNFChapter)]
products[, drug_substance_name_norm := normalise_slash_text(DrugSubstanceName)]

# --------------------------------------------------------------------
# Faster BNF prefix flag using vectorised regex
# Matches the prefix at the start of the field or immediately after "/"
# --------------------------------------------------------------------
bnf_pattern <- paste0("(^|/)", bnf_prefix)

products[, has_bnf_prefix := grepl(bnf_pattern, bnf_chapter_norm)]
products[is.na(has_bnf_prefix), has_bnf_prefix := FALSE]

products_with_prefix <- products[has_bnf_prefix == TRUE]

# --------------------------------------------------------------------
# Extract target substances from prefix-matched rows
# --------------------------------------------------------------------
target_substances <- unique(
  normalise_slash_text(
    split_substances(products_with_prefix$DrugSubstanceName)
  )
)

target_substances <- target_substances[
  !is.na(target_substances) & target_substances != ""
]

# --------------------------------------------------------------------
# Cache matching result for each unique DrugSubstanceName
# This avoids re-splitting the same text many times
# --------------------------------------------------------------------
unique_drug_names <- unique(products$drug_substance_name_norm)

drug_name_match_map <- vapply(
  unique_drug_names,
  function(x) {
    parts <- strsplit(x, "/", fixed = TRUE)[[1]]
    parts <- trimws(parts)
    parts <- parts[!is.na(parts) & parts != ""]
    any(parts %chin% target_substances)
  },
  logical(1)
)

names(drug_name_match_map) <- unique_drug_names

products[, has_matching_substance := unname(drug_name_match_map[drug_substance_name_norm])]
products[is.na(has_matching_substance), has_matching_substance := FALSE]

# Keep only rows not already selected by the BNF prefix
products_with_matching_substance <- products[
  has_matching_substance == TRUE & has_bnf_prefix == FALSE
]

# --------------------------------------------------------------------
# Combine and remove duplicates using only original columns
# --------------------------------------------------------------------
final_products <- rbindlist(
  list(products_with_prefix, products_with_matching_substance),
  fill = TRUE
)

final_products <- unique(final_products[, ..original_columns])

if ("ProdCodeId" %in% names(final_products)) {
  setorder(final_products, ProdCodeId)
}

# --------------------------------------------------------------------
# Write output
# --------------------------------------------------------------------
fwrite(
  final_products,
  file = output_file,
  sep = "\t",
  na = "",
  quote = FALSE
)