rm(list = ls())
gc()
library(data.table)

oxcprd_001b_codelist_tools <- getwd()


oxcprd_001b_gold_prodcodes <- file.path(oxcprd_001b_codelist_tools, "gold/prodcodes")
oxcprd_001b_aurum_prodcodes <- file.path(oxcprd_001b_codelist_tools, "aurum/prodcodes")

gold_txt <- file.path(oxcprd_001b_gold_prodcodes, "product/product_202601.txt")
aurum_txt <- file.path(oxcprd_001b_aurum_prodcodes, "product/CPRDAurumProduct_202601.txt")

bnf_prefix <- "0411"

output_path <- file.path(
  oxcprd_001b_aurum_prodcodes,
  paste0("products_", bnf_prefix, ".txt")
)

products <- fread(
  aurum_txt,
  sep = "\t",
  colClasses = "character",
  na.strings = "",
  quote = ""
)

# Flag rows where ANY split BNFChapter starts with the prefix
products[, has_prefix := {
  parts <- strsplit(BNFChapter, "/", fixed = TRUE)
  vapply(
    parts,
    function(x) any(startsWith(x, bnf_prefix)),
    logical(1)
  )
}]

products_filtered <- products[has_prefix == TRUE]

setorder(products_filtered, ProdCodeId)

fwrite(
  products_filtered,
  file = output_path,
  sep = "\t",
  na = "",
  quote = FALSE
)