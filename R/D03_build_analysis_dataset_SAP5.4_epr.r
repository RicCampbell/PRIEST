library(data.table)
library(lubridate)
library(readxl)
source("R/outcome_functions.r")

## Script to build analysis dataset for table 5.4 in SAP - cohort of 'when an ambulance attended a patient'

## ID for demographics file used as single source of truth for attaching patient_ids on datasets

  demo_file_id <- "FILE0115974_2021-03-18-163613"


# Read in ePR data --------------------------------------------------------

  load(paste("data/datasets/cohort", demo_file_id, "epr_freetext_anonymised_data_ssot_dr_outcomes.rda", sep = "_"))

  epr_outcome_data <- readRDS(file = paste("data/datasets/cohort", demo_file_id, "epr_reduced_outcome_data.rds", sep = "_"))

  stopifnot(epr_single_value_fields_tbl[, .N] == epr_outcome_data[, .N])


## Take number of records/epr_ids to check merges later

  epr_id_count <- epr_single_value_fields_tbl[, .N]


## Find max news score for each epr_id

  max_news_score_data <- epr_news_score_tbl[, .SD[which.max(news_score)], by = epr_id]

  setnames(max_news_score_data, setdiff(colnames(max_news_score_data), "epr_id"),
           paste("epr", setdiff(colnames(max_news_score_data), "epr_id"), sep = "_"))

  stopifnot(max_news_score_data[, .N] <= epr_id_count)
  stopifnot(uniqueN(max_news_score_data[, epr_id]) == max_news_score_data[, .N])


## Take all Primary physical obs (also for calculating NEWS2 score)

  ## Fields to not include

  unwanted_cols_phys <- c("capillary_refill_time", "refill_location", "co_reading_end", "peak_flow_measurement")
  wanted_cols <- setdiff(names(epr_phys_observations_tbl), unwanted_cols_phys)

  primary_phys_obs_data <- epr_phys_observations_tbl[observation_type == "Primary", ..wanted_cols]

  setnames(primary_phys_obs_data, setdiff(colnames(primary_phys_obs_data), "epr_id"),
           paste("epr", setdiff(colnames(primary_phys_obs_data), "epr_id"), sep = "_"))

  stopifnot(primary_phys_obs_data[, .N] <= epr_id_count)
  stopifnot(uniqueN(primary_phys_obs_data[, epr_id]) == primary_phys_obs_data[, .N])


## CPR by crew

    cpr_started_data <- epr_cardiac_respiratory_arrest_tbl[!is.na(cpr_started_time), epr_cpr_by_crew := TRUE][epr_cpr_by_crew == TRUE, .(epr_id, epr_cpr_by_crew)]

    stopifnot(cpr_started_data[, .N] <= epr_id_count)
    stopifnot(uniqueN(cpr_started_data[, epr_id]) == cpr_started_data[, .N])


## Referral type by crew - multiple per epr_id, want to flatten

    erp_referral_type_crew_cast <- dcast(epr_referral_type_crew_tbl, epr_id ~ referral_type_crew, value.var = "referral_type_crew")

    # erp_referral_type_crew_cast <- dcast(epr_referral_type_crew_tbl, epr_id ~ referral_type_crew,
    #                                      fill = FALSE,
    #                                      fun.aggregate = function(x) TRUE,
    #                                      value.var = "referral_type_crew")

    stopifnot(erp_referral_type_crew_cast[, .N] == (epr_referral_type_crew_tbl[, .N, by = epr_id][, .N]))
    stopifnot(erp_referral_type_crew_cast[, .N, by = epr_id][N > 1, .N] == 0)

    setnames(erp_referral_type_crew_cast, setdiff(colnames(erp_referral_type_crew_cast), "epr_id"),
             paste("epr_referral_type_crew", c("community_care", "district_nurse", "gp", "gpooh", "other", "voluntary_sec"), sep = "_"))


# epr_single fields to retain ---------------------------------------------

    unwanted_cols_single <- c("age", "amb_incident_id", "receiving_hospital", "sex", "valid_nhs_number", "ethnicity", "hospital_atmist")
    wanted_cols_single <- setdiff(names(epr_single_value_fields_tbl), unwanted_cols_single)

    epr_single_cohort_desc_data <- epr_single_value_fields_tbl[, ..wanted_cols_single]

    setnames(epr_single_cohort_desc_data, setdiff(colnames(epr_single_cohort_desc_data),
                                                  c("epr_id", "patient_id", "nhs111_priest_clinical_desc_grouping", "ssot_ethnicity_group",
                                                    "ssot_ethnicity_desc", "ssot_iod19_decile", "ssot_gender", "ssot_core_priest_patient",
                                                    "ssot_calculated_age", "dr_covid_related_death_code", "dr_death_within_7_days_contact",
                                                    "dr_death_within_30_days_contact", "dr_died", "crit_care_cc_within_7_days_contact",
                                                    "crit_care_cc_within_30_days_contact", "time_to_deterioration", "time_to_deterioration_lables",
                                                    "any_adverse_outcome", "patient_call_record_order", "last_final_dx_desc", "last_priest_clinical_desc_grouping",
                                                    "last_patient_call_record_order", "pathway_iteration")),
             paste("epr", setdiff(colnames(epr_single_cohort_desc_data),
                                  c("epr_id", "patient_id", "nhs111_priest_clinical_desc_grouping", "ssot_ethnicity_group",
                                    "ssot_ethnicity_desc", "ssot_iod19_decile", "ssot_gender", "ssot_core_priest_patient",
                                    "ssot_calculated_age", "dr_covid_related_death_code", "dr_death_within_7_days_contact",
                                    "dr_death_within_30_days_contact", "dr_died", "crit_care_cc_within_7_days_contact",
                                    "crit_care_cc_within_30_days_contact", "time_to_deterioration", "time_to_deterioration_lables",
                                    "any_adverse_outcome", "patient_call_record_order", "last_final_dx_desc", "last_priest_clinical_desc_grouping",
                                    "last_patient_call_record_order", "pathway_iteration")), sep = "_"))

    stopifnot(epr_single_cohort_desc_data[, .N] <= epr_id_count)
    stopifnot(uniqueN(epr_single_cohort_desc_data[, epr_id]) == epr_id_count)


# Merge tables ------------------------------------------------------------

  ## list of tables to be merged with epr_id (referral_type_crew is a two col table and is needed)

    epr_id_merge_tables <- list(epr_single_cohort_desc_data,
                                epr_outcome_data,
                                max_news_score_data,
                                primary_phys_obs_data,
                                cpr_started_data,
                                erp_referral_type_crew_cast)


    merged_epr_id_tables <- Reduce(function(x, y) merge(x = x, y = y, by = "epr_id", all.x = TRUE), epr_id_merge_tables)

    stopifnot(merged_epr_id_tables[, .N] == epr_id_count)


## Order and add order col, before removing incident time (leaving date) - this does not account for multiple eprs per patient incidents
  ## epr_id added as a 'tie-breaker' so sort will always return the same results

  setorder(merged_epr_id_tables, patient_id, epr_incident_datetime, epr_observations_recorded_time, epr_id, na.last = TRUE)
  merged_epr_id_tables[, patient_record_order := 1:.N, by = patient_id]


  #all_tables_first_contact <- merged_all_tables[patient_incident_order == 1][, order := NULL]

  merged_epr_id_tables[, ':=' (epr_incident_datetime = as.Date(epr_incident_datetime),
                            epr_observations_recorded_time = as.Date(epr_observations_recorded_time))]


  ## Remove excluded patient

    merged_epr_id_tables_excluded_removed <- removeExcludedPatient(merged_epr_id_tables)


  ## Save output with date

  save_date <- Sys.Date()

  write.csv(merged_epr_id_tables_excluded_removed, file = paste0("data/data_for_stats/cohort_", save_date, "_ambulance_attended.csv"))




