## Script for bringing together outcome data from other data source for ePR cohort

library(data.table)
library(readxl)
source("R/outcome_functions.R")

## Read in epr data

  demo_file_id <- "FILE0115974_2021-03-18-163613"

  load(paste("data/datasets/cohort", demo_file_id, "epr_freetext_anonymised_data_ssot_dr_outcomes.rda", sep = "_"))


## Call function that gets all outcome data

  epr_outcome_data <- getOutcomesData(cohort = epr_single_value_fields_tbl,
                                      demo_file_id = demo_file_id,
                                      field_name_patient_id = "patient_id",
                                      field_name_date_field = "incident_datetime",
                                      field_name_contact_id = "epr_id",
                                      duration_days = 365L)


## Save outcomes for cohort

  saveRDS(epr_outcome_data, file = paste("data/datasets/cohort", demo_file_id, "epr_reduced_outcome_data.rds", sep = "_"))
