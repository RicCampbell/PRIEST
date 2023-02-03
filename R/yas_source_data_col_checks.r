library(data.table)
library(readxl)


## Read in YAS data

  nonepr_cad <- data.table(readRDS("D:/source_data/yas/cad/card36_df_final.rds"))
  epr_cad <- data.table(readRDS("D:/source_data/yas/epr/epr_cad_cases_final.rds"))
  epr <- data.table(readRDS("D:/source_data/yas/epr/epr_final_df.rds"))
  nhs111 <- data.table(readRDS("D:/source_data/yas/nhs111/111_post_optout_final.rds"))


  stopifnot(all(colnames(epr_cad) %in% colnames(nonepr_cad)))
  stopifnot(all(colnames(nonepr_cad) %in% colnames(epr_cad)))

  stopifnot(!any(!(colnames(nonepr_cad) %in% colnames(epr_cad))))
  stopifnot(!any(!(colnames(epr_cad) %in% colnames(nonepr_cad))))

  stopifnot(all(epr_cad$callnumber %in% epr$callnumber))
  stopifnot(!any(!(epr_cad$callnumber %in% epr$callnumber)))


## Read in meta data

  epr_provided_spec <- read_excel("D:/reference_data/YAS metadata for PRIEST - 2020-07-24.xlsx",
                              sheet = "ePR data",
                              col_names = TRUE,
                              col_types = "text",
                              trim_ws = TRUE)

  cad_provided_spec <- read_excel("D:/reference_data/YAS metadata for PRIEST - 2020-07-24.xlsx",
                                  sheet = "CAD data",
                                  col_names = TRUE,
                                  col_types = "text",
                                  trim_ws = TRUE)

  nhs111_provided_spec <- read_excel("D:/reference_data/YAS metadata for PRIEST - 2020-07-24.xlsx",
                                  sheet = "NHS111",
                                  col_names = TRUE,
                                  col_types = "text",
                                  trim_ws = TRUE)


## Get YAS fields names from variable name col

  epr_meta_fields <- epr_provided_spec$`variable name (as supplied)`

  cad_meta_fields <- cad_provided_spec$`variable name (as supplied)`

  nhs111_meta_fields <- nhs111_provided_spec$`variable name (as supplied)`


## Get YAS col names from provided data

  nonepr_cad_cols <- colnames(nonepr_cad)
  epr_cad_cols <- colnames(epr_cad)
  epr_cols <- colnames(epr)
  nhs111_cols <- colnames(nhs111)


## Find any missing or extra fields provided in data compared with meta-data spreadsheet provided

  missing_nonepr_cad_cols <- cad_meta_fields[!cad_meta_fields %in% nonepr_cad_cols]
  extra_nonepr_cad_cols <- nonepr_cad_cols[!nonepr_cad_cols %in% cad_meta_fields]

  missing_eprcad_cols <- cad_meta_fields[!cad_meta_fields %in% epr_cad_cols]
  extra_prcad_cols <- epr_cad_cols[!epr_cad_cols %in% cad_meta_fields]

  missing_epr_cols <- epr_meta_fields[!epr_meta_fields %in% epr_cols]
  extra_epr_cols <- epr_cols[!epr_cols %in% epr_meta_fields]

  missing_nhs111_cols <- nhs111_meta_fields[!nhs111_meta_fields %in% nhs111_cols]
  extra_nhs111_cols <- nhs111_cols[!nhs111_cols %in% nhs111_meta_fields]


## Print out any fields that are missing or extra

  cat(paste0("Missing Non-ePR-CAD: ", missing_nonepr_cad_cols, "\n",
             "Extra Non-ePR-CAD: ", extra_nonepr_cad_cols, "\n",
             "Missing ePR-CAD: ", missing_eprcad_cols, "\n",
             "Extra ePR-CAD: ", extra_prcad_cols, "\n",
             "Missing ePR: ", missing_epr_cols, "\n",
             "Extra ePR: ", extra_epr_cols, "\n",
             "Missing NHS 111: ", missing_nhs111_cols, "\n",
             "Extra NHS 111: ", extra_nhs111_cols, "\n"))

## Tidy up environment

  rm(list = ls())

