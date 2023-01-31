library(data.table)
source("R/cleaning_fns_etl.r")
source("R/outcome_functions.r")

# Read in identity data ---------------------------------------------------

demo_file_id <- "FILE0115974_2021-03-18-163613"
record_id_to_pt_id_lu <- readRDS(paste0("data/linkage/cohort_", demo_file_id, "_record_id_to_pt_id_lu.rds"))

# core-PRIEST Data --------------------------------------------------------

core_priest <- readRDS("data/datasets/core_priest-2020-09-15 151647.rds")

## Run through name mapping again as had some that needed changing
setnames(core_priest,
         c("diastolic_bp", "systolic_bp", "gcs-eyes", "gcs-motor"),
         c("bp_diastolic", "bp_systolic", "gcs_eyes", "gcs_motor"))


core_priest <- merge(core_priest,
                     record_id_to_pt_id_lu,
                     by = "record_ID",
                     all.x = TRUE)

## No standardisation done as no fields being used from core-priest apart from attendance_date


# Remove records with no patient id -----------------------------------------------------

  ## Remove records without patient_id from further analysis
  # # Removed [X] records from analysis
  # core_priest[is.na(patient_id), .N]
  core_priest_excluded_data <- core_priest[is.na(patient_id)]
  core_priest <- core_priest[!is.na(patient_id)]


## Add in calculated age and death outcomes now, so can keep this dataset so removal/deletion of identifiable data does not affect this

# Add in SSoT outcome data ------------------------------------------------

  demo_ssot <- readRDS(file = paste("D:/source_data/ssot/cohort", demo_file_id, "demo_ssot.rds", sep = "_"))

  core_single_source_data <- getSingleSourceData(core_priest,
                                                 demo_ssot = demo_ssot,
                                                 field_name_patient_id = "patient_id",
                                                 field_name_date_field = "attendance_date",
                                                 field_name_contact_id = "record_ID")


  setnames(core_priest, c("sex", "calculated_age", "ethnicity"), c("core_priest_sex", "core_priest_calculated_age", "core_priest_ethnicity"))

  core_priest <- merge(core_priest,
                       core_single_source_data,
                       by = "record_ID",
                       all.x = TRUE)


# Add in DR outcome data --------------------------------------------------

  dr_data <- readRDS(file = paste("data/datasets/cohort", demo_file_id, "dr_data.rds", sep = "_"))

  core_dr_data <- getDeathRecordsOutcomes(core_priest,
                                          death_register = dr_data,
                                          field_name_patient_id = "patient_id",
                                          field_name_date_field = "attendance_date",
                                          field_name_contact_id = "record_ID")

  core_priest <- merge(core_priest,
                       core_dr_data,
                       by = "record_ID",
                       all.x = TRUE)

## This outcomes includes time from incident to death, and is therefore identifiable
## Need to add in CC outcomes and derived outcomes so can remove time to death field at this point

# Add in CC outcome data --------------------------------------------------

  cc_data <- readRDS(file = paste("data/datasets/cohort", demo_file_id, "cc_data.rds", sep = "_"))

  core_cc_data <- getCriticalCareOutcomes(core_priest,
                                          cc_data = cc_data,
                                          field_name_patient_id = "patient_id",
                                          field_name_date_field = "attendance_date",
                                          field_name_contact_id = "record_ID")

  core_priest <- merge(core_priest,
                       core_cc_data,
                       by = "record_ID",
                       all.x = TRUE)


# Add in derived cc-dr outcome data ---------------------------------------

  core_derived_adverse_outcomes <- getDerivedAdverseOutcomes(core_dr_data, core_cc_data, "record_ID")

  core_priest <- merge(core_priest,
                       core_derived_adverse_outcomes,
                       by = "record_ID",
                       all.x = TRUE)

## Remove identifiable fields

  core_priest[, c("dr_time_from_incident_to_death", "crit_care_call_to_cc_time") := NULL]

# Save processed data -----------------------------------------------------

saveRDS(core_priest_excluded_data, file = paste0("data/datasets/cohort_", demo_file_id, "_core_priest_excluded_data.rds"))
saveRDS(core_priest, file = paste0("data/datasets/cohort_", demo_file_id, "_core_priest_data_ssot_dr_outcomes.rds"))

