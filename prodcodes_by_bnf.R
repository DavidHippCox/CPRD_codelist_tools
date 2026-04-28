rm(list = ls())
gc()
library(data.table)

oxcprd_001b_codelist_tools <- file.path("J:/cprd dementia/cprd projects/oxcprd_001b - cprd alcohol related dementia (case control)/codelist_tools")


oxcprd_001b_gold_prodcodes <- file.path(oxcprd_001b_codelist_tools, "gold/prodcodes")
oxcprd_001b_aurum_prodcodes <- file.path(oxcprd_001b_codelist_tools, "aurum/prodcodes")

gold_txt <- file.path(oxcprd_001b_gold_prodcodes, "product/product_202601.txt")
aurum_txt <- file.path(oxcprd_001b_aurum_prodcodes, "product/CPRDAurumProduct_202601.txt")

output_path <- file.path(oxcprd_001b_aurum_prodcodes, "products_04010.txt")


products <- fread(
  aurum_txt,
  sep = "\t",
  colClasses = "character",
  na.strings = "",
  quote = "",
)

# Flag rows where ANY split bnfcode starts with "04010"
products[, has_04010 := {
  parts <- strsplit(BNFChapter, "/", fixed = TRUE) # bnfcode for gold
  vapply(
    parts,
    function(x) any(startsWith(x, "04010")),
    logical(1)
  )
}]

products_04010 <- products[has_04010 == TRUE]

setorder(products_04010, "ProdCodeId")


fwrite(
  products_04010,
  file = output_path,
  sep = "\t",
  na = "",
  quote = FALSE
)
