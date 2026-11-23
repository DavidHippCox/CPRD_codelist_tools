# CPRD codelist tools

R scripts for creating, comparing and mapping clinical and medication codelists for CPRD GOLD and Aurum studies. Covers ICD-10, Read codes, SNOMED CT concept IDs, CPRD medical codes (`medcode` / `MedCodeId`), dm+d IDs (`dmdid`), BNF codes and CPRD product codes (`prodcode` / `ProdCodeId`).

## Tools

| Script | Purpose |
| --- | --- |
| `icd10_tool.R` | Compare or combine ICD-10 codelists. |
| `readcode_tool.R` | Compare or combine Read codelists, with optional CPRD dictionary lookup. |
| `medcode_tool.R` | Compare or combine GOLD or Aurum medical code lists. |
| `dmdcode_tool.R` | Compare or combine dm+d lists, with optional product dictionary lookup. |
| `prodcode_tool.R` | Compare or combine GOLD or Aurum product code lists. |
| `get_medcodeid_from_snomed.R` | Map SNOMED CT concept IDs to Aurum `MedCodeId` values and report unmatched concepts. |
| `get_medcodeid_from_qweb_snomed.R` | Map SNOMED CT concept IDs from a QWeb CSV export to Aurum `MedCodeId` values. |
| `prodcodes_by_bnf.R` | Select Aurum products by BNF prefix and expand to products sharing the selected drug substances. |
| `nhsbsa_dmdid_from_bnf.R` | Extract dm+d IDs from an NHSBSA BNF/SNOMED mapping workbook. |
| `join_categories.R` | Attach study-specific medication categories to a combined codelist. |
| `highlight_for_review.R` | Maintain study-specific lists of codes requiring review. |

The five comparison tools support `intersect`, `union`, `A_not_B` and `B_not_A`, deduplicate codes, and optionally attach lookup metadata, source labels and timestamped outputs/logs. Medical and product code comparisons require both inputs to use the same CPRD database.

## Getting started

1. Install R and the required packages:
   ```r
   install.packages(c("data.table", "readxl"))
   ```
2. Set the working directory to this repository. Supply the relevant CPRD medical/product dictionaries, source codelists and mapping files; update paths and dictionary versions in the selected script.


**Example usage:**

After loading the `medcode_tool()` definition:

```r
dir.create("results", showWarnings = FALSE)
result <- medcode_tool(
  fileA = "codelist_a.txt",
  fileB = "codelist_b.txt",
  mode = "union",
  show_source = TRUE,
  save_dir = "results"
)
```

Comparison inputs are tab-separated files with headers; identifiers are read as text to preserve their values. The Read tool uppercases codes by default; set `normalise = FALSE` when case must be preserved.

## Data layout

`gold/` and `aurum/` hold database-specific codelists and dictionaries; `icd10/` and `snomed/` hold terminology-specific inputs and outputs. Review mappings, unmatched codes and clinical inclusion criteria before using a codelist in a study.

Place your local CPRD dictionaries in the following folders (create them if needed). These folders are excluded from Git by `.gitignore`; dictionaries must be supplied separately.

| Database | Dictionary | Folders |
| --- | --- | --- |
| GOLD | Medical | `gold/medcodes/medical/`, `gold/readcodes/medical/` |
| GOLD | Product | `gold/dmdcodes/product/`, `gold/prodcodes/product/` |
| Aurum | Medical | `aurum/medcodes/medical/`, `aurum/readcodes/medical/` |
| Aurum | Product | `aurum/dmdcodes/product/`, `aurum/prodcodes/product/` |
