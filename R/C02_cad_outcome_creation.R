## Script for bringing together outcome data from other data source for CAD cohort

library(data.table)
library(readxl)
source("R/outcome_functions.R")

## Read in CAD data

  demo_file_id <- "FILE0115974_2021-03-18-163613"

  cad_data <- readRDS(file = paste("data/datasets/cohort", demo_file_id, "cad_data_ssot_dr_outcomes.rds", sep = "_"))


## Call function that gets all outcome data

  cad_outcome_data <- getOutcomesData(cohort = cad_data,
                                      demo_file_id = demo_file_id,
                                      field_name_patient_id = "patient_id",
                                      field_name_date_field = "call_datetime",
                                      field_name_contact_id = "amb_incident_id",
                                      duration_days = 365L)


## Save outcomes for cohort

  saveRDS(cad_outcome_data, file = paste("data/datasets/cohort", demo_file_id, "cad_reduced_outcome_data.rds", sep = "_"))
