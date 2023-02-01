library(data.table)
library(lubridate)
library(readxl)
source("R/outcome_functions.R")

## Script to build analysis dataset for table 5.3 in SAP
## Cohort of 'patient Emergency Operations centre assessed over the phone' - this includes when an ambulance was and wasn't dispatched


## ID for demographics file used as single source of truth for attaching patient_ids on datasets

  demo_file_id <- "FILE0115974_2021-03-18-163613"


# Read in CAD data --------------------------------------------------------

  cad_data_all <- readRDS(file = paste0("data/datasets/cohort_", demo_file_id, "_cad_data_ssot_dr_outcomes.rds"))

  cad_outcome_data <- readRDS(file = paste("data/datasets/cohort", demo_file_id, "cad_reduced_outcome_data.rds", sep = "_"))

  stopifnot(cad_data_all[ ,.N] == cad_outcome_data[, .N])



## Only keep card36 records - checked that this is correct

  cad_data <- cad_data_all[substr(despatch_code, 1, 2) == "36"]


  ## Only keep 999 calls  checked that this is also correct

    cad_data <- cad_data[course_of_call == "999"]


## Take count of CAD amb_incident_ids to check counts as we go

  cad_id_count <- uniqueN(cad_data[, amb_incident_id])


#-----------------------------------------------------------
## DO NOT USE ePR DATA IN CAD OUTCOMES AS THESE COHORTS DO NOT MATCH UP

  ## USE SCENE_ARRIVAL_TIME FOR THIS - could add call stop reason as well
#-----------------------------------------------------------

# CAD fields to retain ----------------------------------------------------

  unwanted_cols_single <- c("despatch_code", "gov_tstdtoc", "hospital_attended", "age", "sex",
                            "destination_arrival_time", "time_call_closed", "scene_depart_time", "valid_nhs_number", "record_ID")

  wanted_cols_single <- setdiff(names(cad_data), unwanted_cols_single)

  cad_cohort_desc_data <- cad_data[, ..wanted_cols_single]

  setnames(cad_cohort_desc_data, setdiff(colnames(cad_cohort_desc_data),
                                         c("amb_incident_id", "patient_id", "nhs111_priest_clinical_desc_grouping", "ssot_ethnicity_group",
                                           "ssot_ethnicity_desc", "ssot_iod19_decile", "ssot_gender", "ssot_core_priest_patient",
                                           "ssot_calculated_age", "dr_covid_related_death_code", "dr_death_within_7_days_contact",
                                           "dr_death_within_30_days_contact", "dr_died", "crit_care_cc_within_7_days_contact",
                                           "crit_care_cc_within_30_days_contact", "time_to_deterioration", "time_to_deterioration_lables",
                                           "any_adverse_outcome", "patient_call_record_order", "last_final_dx_desc", "last_priest_clinical_desc_grouping",
                                           "last_patient_call_record_order", "pathway_iteration")),
           paste("cad", setdiff(colnames(cad_cohort_desc_data),
                                c("amb_incident_id", "patient_id", "nhs111_priest_clinical_desc_grouping", "ssot_ethnicity_group",
                                  "ssot_ethnicity_desc", "ssot_iod19_decile", "ssot_gender", "ssot_core_priest_patient",
                                  "ssot_calculated_age", "dr_covid_related_death_code", "dr_death_within_7_days_contact",
                                  "dr_death_within_30_days_contact", "dr_died", "crit_care_cc_within_7_days_contact",
                                  "crit_care_cc_within_30_days_contact", "time_to_deterioration", "time_to_deterioration_lables",
                                  "any_adverse_outcome", "patient_call_record_order", "last_final_dx_desc", "last_priest_clinical_desc_grouping",
                                  "last_patient_call_record_order", "pathway_iteration")), sep = "_"))

  stopifnot(cad_cohort_desc_data[, .N] <= cad_id_count)
  stopifnot(uniqueN(cad_cohort_desc_data[, amb_incident_id]) == cad_id_count)


# Merge tables ------------------------------------------------------------

  cad_analysis_table <- merge(cad_cohort_desc_data,
                              cad_outcome_data,
                              by = "amb_incident_id",
                              all.x = TRUE)


  stopifnot(cad_analysis_table[, .N] == cad_id_count)


  ## Order and add order col, before removing call time & scene arrival (leaving date)
  ## amb_incident_id added as a 'tie-breaker' so sort will always return the same results

    setorder(cad_analysis_table, patient_id, cad_call_datetime, cad_scene_arrival_time, amb_incident_id, na.last = TRUE)
    cad_analysis_table[, patient_call_record_order := 1:.N, by = patient_id]

    cad_analysis_table[, ':=' (cad_call_datetime = as.Date(cad_call_datetime),
                              cad_scene_arrival_time = as.Date(cad_scene_arrival_time))]


  ## Remove excluded patients

    cad_analysis_table_exclude_removed <- removeExcludedPatient(cad_analysis_table)


  ## Save output with date

    save_date <- Sys.Date()

    write.csv(cad_analysis_table_exclude_removed, file = paste0("data/data_for_stats/cohort_", save_date, "_emergency_centre_contact.csv"))





