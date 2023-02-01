library(data.table)
library(lubridate)
library(readxl)
source("private/hash_fn.r")
source("R/outcome_functions.R")


### Script to build analysis dataset for table 5.2 in SAP - cohort of 'NHS111 contact' from outcomes found in previous script

## ID for demographics file used as single source of truth for attaching patient_ids on datasets

  demo_file_id <- "FILE0115974_2021-03-18-163613"


# Read in NHS111 data --------------------------------------------------------

  nhs111_data <- readRDS(file = paste("data/datasets/cohort", demo_file_id, "nhs111_data_ssot_dr_outcomes.rds", sep = "_"))

  nhs111_outcome_data <- readRDS(file = paste("data/datasets/cohort", demo_file_id, "nhs111_reduced_outcome_data.rds", sep = "_"))

  stopifnot(nhs111_data[, .N] == nhs111_outcome_data[, .N])


# NHS111 fields to retain ----------------------------------------------------

  ## Add in NHS Pathways version before removing time as V19.8.3 went live at 10:30 on 2020-06-02

    nhs111_data[call_datetime < as.POSIXct("2020-06-02 10:30:00", tz = "UTC"), pathway_iteration := 1L]
    nhs111_data[call_datetime >= as.POSIXct("2020-06-02 10:30:00", tz = "UTC"), pathway_iteration := 2L]


  unwanted_cols_single <- c("age_years", "called_back", "hospital_attended", "clinical_dx_code", "clinical_dx_Desc",
                            "clinical_assessment_time", "final_symptom_code", "final_symptom_desc", "final_symptom_group",
                            "first_pathway_selected", "non_clinical_dx_code", "non_clinical_dx_desc", "passed_to_clinician", "sex")

  wanted_cols_single <- setdiff(names(nhs111_data), unwanted_cols_single)

  nhs111_cohort_desc_data <- nhs111_data[, ..wanted_cols_single]

  setnames(nhs111_cohort_desc_data, setdiff(colnames(nhs111_cohort_desc_data),
                                            c("record_ID", "patient_id", "nhs111_priest_clinical_desc_grouping", "ssot_ethnicity_group",
                                              "ssot_ethnicity_desc", "ssot_iod19_decile", "ssot_gender", "ssot_core_priest_patient",
                                              "ssot_calculated_age", "dr_covid_related_death_code", "dr_death_within_7_days_contact",
                                              "dr_death_within_30_days_contact", "dr_died", "crit_care_cc_within_7_days_contact",
                                              "crit_care_cc_within_30_days_contact", "time_to_deterioration", "time_to_deterioration_lables",
                                              "any_adverse_outcome", "patient_call_record_order", "last_final_dx_desc", "last_priest_clinical_desc_grouping",
                                              "last_patient_call_record_order", "pathway_iteration")),
           paste("nhs111", setdiff(colnames(nhs111_cohort_desc_data),
                                   c("record_ID", "patient_id", "nhs111_priest_clinical_desc_grouping", "ssot_ethnicity_group",
                                     "ssot_ethnicity_desc", "ssot_iod19_decile", "ssot_gender", "ssot_core_priest_patient",
                                     "ssot_calculated_age", "dr_covid_related_death_code", "dr_death_within_7_days_contact",
                                     "dr_death_within_30_days_contact", "dr_died", "crit_care_cc_within_7_days_contact",
                                     "crit_care_cc_within_30_days_contact", "time_to_deterioration", "time_to_deterioration_lables",
                                     "any_adverse_outcome", "patient_call_record_order", "last_final_dx_desc", "last_priest_clinical_desc_grouping",
                                     "last_patient_call_record_order", "pathway_iteration")), sep = "_"))

  stopifnot(uniqueN(nhs111_cohort_desc_data[, record_ID]) == nhs111_data[, .N])


# Merge tables ------------------------------------------------------------

  nhs111_analysis_table <- merge(nhs111_cohort_desc_data,
                                 nhs111_outcome_data,
                                 by = "record_ID",
                                 all.x = TRUE)


  stopifnot(nhs111_analysis_table[, .N] == nhs111_cohort_desc_data[, .N])


## Pseudo the record number

  stopifnot(nhs111_outcome_data[is.na(record_ID) | record_ID == "", .N] == 0)

  nhs111_analysis_table[, ':=' (record_id = fn_pseudonymiseValues(as.character(record_ID), salt_filename = "private/salt1.txt"))][, record_ID := NULL]


  ## Order and add order col, before removing call time (leaving date only)
  ## record_id added as a 'tie-breaker' so sort will always return the same results

    setorder(nhs111_analysis_table, patient_id, nhs111_call_datetime, record_id, na.last = TRUE)
    nhs111_analysis_table[, patient_call_record_order := 1:.N, by = patient_id]

    nhs111_analysis_table[, nhs111_call_datetime := as.Date(nhs111_call_datetime)]


# Number of contacts per patient ------------------------------------------

    nhs111_analysis_table[, total_records_for_patient := .N, by = patient_id]


  ## Remove excluded patient

    nhs111_analysis_table_excluded_removed <- removeExcludedPatient(nhs111_analysis_table)


  ## Save output with date

    save_date <- Sys.Date()

    write.csv(nhs111_analysis_table_excluded_removed, file = paste0("data/data_for_stats/cohort_", save_date, "_nhs111.csv"))



