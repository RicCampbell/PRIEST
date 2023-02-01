## Script for checking all date of death and date of birth in comparison to contact dates with YAS services and core-PRIEST

library(data.table)
source("R/standardise_functions.r")


## Read in all YAS data sets and core-PRIEST data

  demo_file_id <- "FILE0115974_2021-03-18-163613"

  load(paste("data/datasets/cohort", demo_file_id, "epr_freetext_anonymised_data.rda", sep = "_"))

  cad_data <- readRDS(file = paste("data/datasets/cohort", demo_file_id, "cad_data.rds", sep = "_"))

  nhs111_data <- readRDS(file = paste("data/datasets/cohort", demo_file_id, "nhs111_data.rds", sep = "_"))

  core_priest <- readRDS(file = paste("data/datasets/cohort", demo_file_id, "core_priest_data.rds", sep = "_"))


## Change all datetimes to just dates

  epr_single_value_fields_tbl[, incident_datetime := as.Date(incident_datetime)]
  cad_data[, call_datetime := as.Date(call_datetime)]
  nhs111_data[, call_datetime := as.Date(call_datetime)]
  core_priest[, attendance_date := as.Date(attendance_date)]


## Read in death register data and single source of truth

  dr_data <- readRDS(file = paste("data/datasets/cohort", demo_file_id, "dr_data.rds", sep = "_"))

  demo_ssot <- readRDS(file = paste("D:/source_data/ssot/cohort", demo_file_id, "demo_ssot.rds", sep = "_"))


## Bind instead of merge as easier to find earliest/latest dates

  contact_dates <- rbind(epr_single_value_fields_tbl[, .(patient_id, contact_date = incident_datetime)],
                         cad_data[, .(patient_id, contact_date = call_datetime)],
                         nhs111_data[, .(patient_id, contact_date = call_datetime)],
                         core_priest[, .(patient_id, contact_date = attendance_date)])

  minmax_contact_dates <- contact_dates[, .(earliest_contact_date = min(contact_date),
                                            lastest_contact_date = max(contact_date)), by = patient_id]


## Merge in date of death and date of birth

  death_birth_minmax_contact_dates <- merge(merge(minmax_contact_dates,
                                                  dr_data[, .(patient_id, DATE_OF_DEATH)],
                                                  by = "patient_id",
                                                  all.x = TRUE),
                                            demo_ssot[, .(patient_id, dob)],
                                            by = "patient_id",
                                            all.x = TRUE)

  stopifnot(death_birth_minmax_contact_dates[, .N, by = patient_id][N > 1, .N] == 0)

  patients_to_remove <- death_birth_minmax_contact_dates[(dob > earliest_contact_date) | (DATE_OF_DEATH < lastest_contact_date), patient_id]


## Read in previous excluded patient ids

  multi_death_excluded_patients <- readRDS(file = paste("data/datasets/cohort", demo_file_id, "multi_death_excluded_patient_ids.rds", sep = "_"))


## Add patient ids to be removed from cohorts and save

  all_excluded_patients <- c(patients_to_remove, multi_death_excluded_patients)

  saveRDS(all_excluded_patients, file = paste("data/datasets/cohort", demo_file_id, "excluded_patient_ids.rds", sep = "_"))



