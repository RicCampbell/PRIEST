## Script for bringing together outcome data from other data source for NHS111 cohort

library(data.table)
library(readxl)
source("R/outcome_functions.R")

## Read in NHS111 data

  demo_file_id <- "FILE0115974_2021-03-18-163613"

  nhs111_data <- readRDS(file = paste("data/datasets/cohort", demo_file_id, "nhs111_data_ssot_dr_outcomes.rds", sep = "_"))


## Call function that gets all outcome data

  nhs111_outcome_data <- getOutcomesData(cohort = nhs111_data,
                                         demo_file_id = demo_file_id,
                                         field_name_patient_id = "patient_id",
                                         field_name_date_field = "call_datetime",
                                         field_name_contact_id = "record_ID",
                                         duration_days = 365L)



## Save outcomes for cohort

  saveRDS(nhs111_outcome_data, file = paste("data/datasets/cohort", demo_file_id, "nhs111_reduced_outcome_data.rds", sep = "_"))


