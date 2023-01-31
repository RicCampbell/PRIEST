library(data.table)
library(readxl)

## Script for further processing of ECDS data so can be used in all 3 cohort for stats
## Reduction of ECDS data to fields to be used and if a record has a COVID diagnosis


## Load in processed ECDS data

  demo_file_id <- "FILE0115974_2021-03-18-163613"

  ecds_data <- readRDS(file = paste0("data/datasets/cohort_", demo_file_id, "_ecds_data.rds"))


## Load in SNOMED codes used in ED from output spec document that contains a 'full list of the ECDS diagnosis'

  ecds_snomed_codes <- data.table(read_excel("data-raw/reference_data/ED output spec/ecds_tech_output.xlsx",
                                             sheet = "20.1 DIAGNOSIS ",
                                             col_names = TRUE,
                                             col_types = "text",
                                             trim_ws = TRUE))

  setnames(ecds_snomed_codes, make.names(toupper(colnames(ecds_snomed_codes)), unique = TRUE))


## Check snomed codes only exits once in dataset

  stopifnot(ecds_snomed_codes[, .N, by = SNOMED_CODE][N > 1, .N] == 0)


## Load in covid SNOMED codes - taken from small ecds SNOMED codes list (above)

  covid_snomed_codes <- data.table(read_excel("D:/reference_data/Field Mapping and Standardisation.xlsx",
                                             sheet = "snomed_covid_codes",
                                             col_names = TRUE,
                                             col_types = "text",
                                             trim_ws = TRUE))


## Keep only cols that are interested in

  ecds_snomed_codes <- ecds_snomed_codes[, .(SNOMED_CODE, SNOMED_DESCRIPTION)]


# Was there as COVID diagnosis --------------------------------------------

  ## Keep only fields are interested in and melt over record_id

    ecds_diag_fields <- paste("DIAGNOSIS_CODE", 1:12, sep = "_")
    ecds_wanted_cols <- c("ARRIVAL_DATE", "ARRIVAL_TIME", "patient_id", "record_ID", ecds_diag_fields)


  ## Melt data to get one DIAG per row and remove rows with no code

    melted_ecds_data <- melt(ecds_data[, .SD, .SDcols = ecds_wanted_cols],
                             measure.vars = ecds_diag_fields, variable.name = "outcome_numb", value.name = "diag_code")[!is.na(diag_code)]


  ## Merge with snomed codes to get description - no duplicates as using smaller ED snomed list

    melted_ecds_data_snomed_desc <- merge(melted_ecds_data, ecds_snomed_codes, by.x = "diag_code", by.y = "SNOMED_CODE", all.x = TRUE)


  ## Check that every diag code was found in snomed reference data and have same number of records as after melt, and no duplicates

    stopifnot(melted_ecds_data_snomed_desc[is.na(SNOMED_DESCRIPTION), .N] == 0L)


  ## Create field for if there was a COVID diagnosis

    melted_ecds_data_snomed_desc[, melted_covid_diagnosis := (diag_code %chin% covid_snomed_codes$snomed_code)]


  ## Get all record_IDs with positive covid diagnosis

    positive_ed_covid_diagnosis_rows <- melted_ecds_data_snomed_desc[, .(covid_diagnosis = any(melted_covid_diagnosis)), by = record_ID][covid_diagnosis == TRUE, record_ID]


  ## Add field to original ECDS data with is there was a covid diagnosis in that record

    ecds_data[, covid_diagnosis := (record_ID %in% positive_ed_covid_diagnosis_rows)]


  ## Keep only fields interested in for looking in each cohort

    ecds_data_covid_diag <- ecds_data[, .(ARRIVAL_DATE, ARRIVAL_TIME, patient_id, covid_diagnosis, DEPARTMENT_TYPE)]


## Save patients/dates of positive covid diagnosis in ED

  saveRDS(ecds_data_covid_diag, file = paste0("data/datasets/cohort_", demo_file_id, "_ecds_data_covid_diag.rds"))





