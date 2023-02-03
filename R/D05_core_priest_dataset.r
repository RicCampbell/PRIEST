library(data.table)
library(lubridate)
library(readxl)
source("private/hash_fn.r")
source("R/cleaning_fns_etl.r")
source("R/outcome_functions.R")

snomed_ct <- readRDS(file = "data/reference_data/snomed_int_gb_20201125_conceptid_term_lookup.rds")

attachSnomedTerms <- function(data, snomed_data, record_id_fld_name, snomed_fld_names, replace = TRUE, dst_term_suffix = "_term") {
  #
  # data = core_priest_data_v2
  # snomed_data = snomed_ct
  # record_id_fld_name = "record_ID"
  # snomed_fld_names = c("DIAGNOSIS_CODE_1", "DIAGNOSIS_CODE_2", "DIAGNOSIS_CODE_3")
  # replace = FALSE
  # dst_term_suffix = "_term"

  stopifnot(length(record_id_fld_name) == 1)
  stopifnot(length(dst_term_suffix) == 1)
  stopifnot(all(c(record_id_fld_name, snomed_fld_names) %in% colnames(data)))

  data_copy <- copy(data)

  setnames(data_copy, record_id_fld_name, ".record_ID.")
  col_order <- copy(colnames(data_copy))

  other_fields <- colnames(data_copy)[!(colnames(data_copy) %in% c(".record_ID.", snomed_fld_names))]

  nonsnomed_flds <- c(".record_ID.", other_fields)

  data_copy_long <- melt(data_copy, id.vars = nonsnomed_flds, variable.name = "field_names", value.name = "conceptId", variable.factor = FALSE)
  data_copy_long <- merge(data_copy_long,
                          snomed_data,
                          by = "conceptId",
                          all.x = TRUE)

  data_copy_wide <- dcast(data_copy_long, ... ~ field_names, value.var = c("conceptId", "term"))

  if(replace) {
    data_copy_wide[, (paste0("conceptId_", snomed_fld_names)) := NULL]
    setnames(data_copy_wide, paste0("term_", snomed_fld_names), snomed_fld_names)
    setcolorder(data_copy_wide, col_order)
  } else {
    setnames(data_copy_wide,
             paste0(rep(c("conceptId_", "term_"), each = length(snomed_fld_names)), rep(snomed_fld_names, 2)),
             c(snomed_fld_names, paste0(snomed_fld_names, dst_term_suffix)))

    setcolorder(data_copy_wide, c(col_order, paste0(col_order[col_order %in% snomed_fld_names], dst_term_suffix)))
  }

  setnames(data_copy_wide, ".record_ID.", record_id_fld_name)

  return(data_copy_wide)
}

## ID for demographics file used as single source of truth for attaching patient_ids on datasets

  demo_file_id <- "FILE0115974_2021-03-18-163613"


# Read in core-PRIEST data ------------------------------------------------

  core_priest_data <- readRDS(file = paste0("data/datasets/cohort_", demo_file_id, "_core_priest_data_ssot_dr_outcomes.rds"))

  # Covert blanks to NA
  field_types <- sapply(core_priest_data, typeof)
  character_field_names <- names(field_types)[field_types == "character"]
  core_priest_data[, (character_field_names) := lapply(.SD, fn_removeBlanks), .SDcols = character_field_names]

  # Remove empty fields
  empty_fields <- unlist(lapply(core_priest_data, function(fld) {
    return(all(is.na(fld)))
  }))
  empty_field_names <- names(empty_fields)[!is.na(empty_fields) & empty_fields == TRUE]
  core_priest_data[, (empty_field_names) := NULL]

  # Convert binary fields to logical type
  ticked_fields <- unlist(lapply(core_priest_data, function(fld) {
    return(any(as.character(fld) == "Ticked"))
  }))
  ticked_field_names <- names(ticked_fields)[!is.na(ticked_fields) & ticked_fields == TRUE]
  core_priest_data[, (ticked_field_names) := lapply(.SD, function(fld) return(fld %in% "Ticked")), .SDcols = ticked_field_names]

  # Sex mapping
  sex_mapping <- data.table(read_excel("D:/reference_data/Field Mapping and Standardisation.xlsx",
                                       sheet = "sex_standardisation",
                                       col_names = TRUE,
                                       col_types = "text",
                                       trim_ws = TRUE))

  core_priest_data[, core_priest_sex := sex_mapping$destination_code[match(core_priest_sex, sex_mapping$source_code)]]
  core_priest_data[!(core_priest_sex %in% sex_mapping$destination_code), core_priest_sex := NA]

  # Convert date fields
  core_priest_data[, attendance_date := as.Date(attendance_date)]
  core_priest_data[, attendance_datetime := fast_strptime(paste(as.character.Date(attendance_date, format = "%Y-%m-%d"), replace(attendance_time, is.na(attendance_time), "00:00:00")), format = "%Y-%m-%d %H:%M:%S", tz = "Europe/London", lt = FALSE)]

  # Get ODS Trust codes
  core_priest_trust_codes <- readRDS("data/reference_data/core_priest_sites_to_trust_codes.rds")
  core_priest_data <- merge(core_priest_data,
                            core_priest_trust_codes,
                            all.x = TRUE,
                            by = "site")



# Save harmonised core-priest data ----------------------------------------

  save_time <- gsub(" ", "-", gsub(":", "", Sys.time(), fixed = TRUE), fixed = TRUE)

  saveRDS(core_priest_data, paste0("data/datasets/cohort_", demo_file_id, save_time, "_cleaned_core_priest_data_ssot_dr_outcomes.rds"))


# Link 1: Outcomes data ---------------------------------------------------


  core_priest_outcome_data <- getOutcomesData(cohort = core_priest_data,
                                         demo_file_id = demo_file_id,
                                         field_name_patient_id = "patient_id",
                                         field_name_date_field = "attendance_date",
                                         field_name_contact_id = "record_ID",
                                         duration_days = 365L)

  core_priest_data_v1 <- merge(core_priest_data,
                               core_priest_outcome_data,
                               all.x = TRUE,
                               by = "record_ID")


# Link 2: ECDS record -----------------------------------------------------

  ecds_data <- readRDS(paste0("data/datasets/cohort_", demo_file_id, "_ecds_data.rds"))
  setnames(ecds_data, "record_ID", "record_ID_ECDS")

  # core_priest_data_v2 <- merge(core_priest_data,
  #                              ecds_data,
  #                              all.x = TRUE,
  #                              by = "patient_id")
  #
  # core_priest_data_v2[, attend_timediff := abs(difftime(attendance_datetime, ARRIVAL_TIME, unit = "mins"))]
  # setorder(core_priest_data_v2, attend_timediff)
  # core_priest_data_v2[, record_order := 1:.N, by = patient_id]
  # core_priest_data_v2 <- core_priest_data_v2[record_order == 1]
  # core_priest_data_v2[, record_order := NULL]
  #
  # site_lu <- core_priest_data_v2[!is.na(PROVIDER_CODE), .N, by = .(site, trust_code = substr(PROVIDER_CODE, 1, 3))][order(-N)]
  # setorder(site_lu, -N)
  # site_lu[, order := 1:.N, by = site]
  # site_lu <- site_lu[order == 1]
  # site_lu[, c("order", "N") := NULL]
  #
  # saveRDS(site_lu, file = "data/reference_data/core_priest_sites_to_trust_codes.rds")

  core_priest_data_v2_links <- merge(core_priest_data[, .(record_ID, patient_id, trust_code, attendance_datetime)],
                                     ecds_data[, .(record_ID_ECDS, patient_id, trust_code = substr(PROVIDER_CODE, 1, 3), ARRIVAL_TIME)],
                                     all.x = TRUE,
                                     by = c("patient_id", "trust_code"))

  core_priest_data_v2_links[, attend_timediff := abs(difftime(attendance_datetime, ARRIVAL_TIME, unit = "mins"))]
  setorder(core_priest_data_v2_links, attend_timediff)
  core_priest_data_v2_links[, record_order := 1:.N, by = patient_id]
  core_priest_data_v2_links <- core_priest_data_v2_links[record_order == 1]
  core_priest_data_v2_links[, record_order := NULL]
  core_priest_data_v2_links <- core_priest_data_v2_links[!is.na(attend_timediff), .(record_ID, record_ID_ECDS)]

  core_priest_data_v2 <- merge(merge(core_priest_data,
                                           core_priest_data_v2_links,
                                           by = "record_ID",
                                           all.x = TRUE),
                                     ecds_data,
                                     by = c("patient_id", "record_ID_ECDS"),
                                     all.x = TRUE)

  # We do not match all core-PRIEST records to an ECDS record
  core_priest_data_v2[is.na(record_ID_ECDS), .N]
  core_priest_data_v2[, .(pc = round(100 * sum(is.na(record_ID_ECDS)) / .N, 1)), by = site][order(-pc)]

  # # Find all character fields in ECDS
  # fld_types <- sapply(ecds_data, typeof)
  # ch_fields <- names(fld_types)[fld_types == "character"]
  #
  # # For each char type field, find the min (non-empty) char length
  # field_len <- t(ecds_data[, lapply(.SD, function(fld) {min(nchar(fld), na.rm = TRUE)}),.SDcols = (ch_fields)])
  # # SNOMED codes are at least 6chars
  # snomed_cols <- rownames(field_len)[field_len >= 6]

  snomed_cols <- c('ACCOMMODATION_STATUS', 'ACUITY', 'ARRIVAL_MODE', 'ATTENDANCE_SOURCE', 'CHIEF_COMPLAINT', 'CHIEF_COMPLAINT_EXTENDED_CODE', 'COMORBIDITIES_1', 'COMORBIDITIES_10', 'COMORBIDITIES_2', 'COMORBIDITIES_3', 'COMORBIDITIES_4', 'COMORBIDITIES_5', 'COMORBIDITIES_6', 'COMORBIDITIES_7', 'COMORBIDITIES_8', 'COMORBIDITIES_9', 'DIAGNOSIS_CODE_1', 'DIAGNOSIS_CODE_10', 'DIAGNOSIS_CODE_11', 'DIAGNOSIS_CODE_12', 'DIAGNOSIS_CODE_2', 'DIAGNOSIS_CODE_3', 'DIAGNOSIS_CODE_4', 'DIAGNOSIS_CODE_5', 'DIAGNOSIS_CODE_6', 'DIAGNOSIS_CODE_7', 'DIAGNOSIS_CODE_8', 'DIAGNOSIS_CODE_9', 'DIAGNOSIS_QUALIFIER_1', 'DIAGNOSIS_QUALIFIER_10', 'DIAGNOSIS_QUALIFIER_11', 'DIAGNOSIS_QUALIFIER_12', 'DIAGNOSIS_QUALIFIER_2', 'DIAGNOSIS_QUALIFIER_3', 'DIAGNOSIS_QUALIFIER_4', 'DIAGNOSIS_QUALIFIER_5', 'DIAGNOSIS_QUALIFIER_6', 'DIAGNOSIS_QUALIFIER_7', 'DIAGNOSIS_QUALIFIER_8', 'DIAGNOSIS_QUALIFIER_9', 'DISCHARGE_DESTINATION', 'DISCHARGE_STATUS', 'INVESTIGATION_CODE_1', 'INVESTIGATION_CODE_10', 'INVESTIGATION_CODE_11', 'INVESTIGATION_CODE_12', 'INVESTIGATION_CODE_2', 'INVESTIGATION_CODE_3', 'INVESTIGATION_CODE_4', 'INVESTIGATION_CODE_5', 'INVESTIGATION_CODE_6', 'INVESTIGATION_CODE_7', 'INVESTIGATION_CODE_8', 'INVESTIGATION_CODE_9', 'REFERRED_TO_SERVICE_1', 'REFERRED_TO_SERVICE_2', 'REFERRED_TO_SERVICE_3', 'REFERRED_TO_SERVICE_4', 'TREATMENT_CODE_1', 'TREATMENT_CODE_10', 'TREATMENT_CODE_11', 'TREATMENT_CODE_12', 'TREATMENT_CODE_2', 'TREATMENT_CODE_3', 'TREATMENT_CODE_4', 'TREATMENT_CODE_5', 'TREATMENT_CODE_6', 'TREATMENT_CODE_7', 'TREATMENT_CODE_8', 'TREATMENT_CODE_9')

  # Exclude epikey
  snomed_cols <- snomed_cols[!(snomed_cols %in% "EPIKEY")]

  # Convert SNOMED codes to terms
  core_priest_data_v2 <- attachSnomedTerms(core_priest_data_v2, snomed_ct, "record_ID", snomed_cols)
  core_priest_data_v2[, core_to_ECDS_attend_timediff_mins := as.integer(round((difftime(ARRIVAL_TIME, attendance_datetime, unit = "mins"))))]

  # Time differences
  core_priest_data_v2[, .(mn = mean(abs(core_to_ECDS_attend_timediff_mins), na.rm = TRUE), p50 = median(abs(core_to_ECDS_attend_timediff_mins), na.rm = TRUE), p90 = quantile(abs(core_to_ECDS_attend_timediff_mins), probs = 0.9, na.rm = TRUE), p99 = quantile(abs(core_to_ECDS_attend_timediff_mins), probs = 0.99, na.rm = TRUE))]
  core_priest_data_v2[, .(pc = round(100 * sum(abs(core_to_ECDS_attend_timediff_mins) > 60, na.rm = TRUE) / sum(!is.na(core_to_ECDS_attend_timediff_mins)), 1)), by = site][order(-pc)]

  # General comparisons - for those within 1 day
  core_priest_ecds_comp <- core_priest_data_v2[abs(core_to_ECDS_attend_timediff_mins) < 1440, .(site, core_to_ECDS_attend_timediff_mins, most_likely_diagnosis, most_likely_diagnosis_other, DIAGNOSIS_CODE_1, DIAGNOSIS_CODE_2, DIAGNOSIS_CODE_3, INVESTIGATION_CODE_1, INVESTIGATION_CODE_2, INVESTIGATION_CODE_3, disp_cat, referral_source, ATTENDANCE_SOURCE, ARRIVAL_MODE, referral_source_other, previous_attendance, patient_lives_alone_or_no_fixed_abode, ACCOMMODATION_STATUS, core_priest_ethnicity, ETHNIC_CATEGORY, was_patient_admitted_at_initial_assessment, EPIKEY, DECIDED_TO_ADMIT_DATE, DISCHARGE_DESTINATION, DISCHARGE_STATUS, initial_outcome, no_admissions, any_admission, amended_admission, ACUITY, ATTENDANCE_CATEGORY, CHIEF_COMPLAINT, CHIEF_COMPLAINT_EXTENDED_CODE, DECIDED_TO_ADMIT_TIME, DEPARTMENT_TYPE, DEPARTURE_DATE, DEPARTURE_TIME, DIAGNOSIS_CODE_10, DIAGNOSIS_CODE_11, DIAGNOSIS_CODE_12, DIAGNOSIS_CODE_4, DIAGNOSIS_CODE_5, DIAGNOSIS_CODE_6, DIAGNOSIS_CODE_7, DIAGNOSIS_CODE_8, DIAGNOSIS_CODE_9, DIAGNOSIS_QUALIFIER_1, DIAGNOSIS_QUALIFIER_10, DIAGNOSIS_QUALIFIER_11, DIAGNOSIS_QUALIFIER_12, DIAGNOSIS_QUALIFIER_2, DIAGNOSIS_QUALIFIER_3, DIAGNOSIS_QUALIFIER_4, DIAGNOSIS_QUALIFIER_5, DIAGNOSIS_QUALIFIER_6, DIAGNOSIS_QUALIFIER_7, DIAGNOSIS_QUALIFIER_8, DIAGNOSIS_QUALIFIER_9, DISCHARGE_DESTINATION, DISCHARGE_STATUS, EPIKEY, INVESTIGATION_CODE_10, INVESTIGATION_CODE_11, INVESTIGATION_CODE_12, INVESTIGATION_CODE_4, INVESTIGATION_CODE_5, INVESTIGATION_CODE_6, INVESTIGATION_CODE_7, INVESTIGATION_CODE_8, INVESTIGATION_CODE_9, PROVIDER_CODE, REFERRED_TO_SERVICE_1, REFERRED_TO_SERVICE_2, REFERRED_TO_SERVICE_3, REFERRED_TO_SERVICE_4, SITE, TREATMENT_CODE_1, TREATMENT_CODE_10, TREATMENT_CODE_11, TREATMENT_CODE_12, TREATMENT_CODE_2, TREATMENT_CODE_3, TREATMENT_CODE_4, TREATMENT_CODE_5, TREATMENT_CODE_6, TREATMENT_CODE_7, TREATMENT_CODE_8, TREATMENT_CODE_9)]

# core-PRIEST vs Routine Data ---------------------------------------------

  colnames(core_priest_outcome_data)

  #  sex
  core_priest_data_v1[, .N, by = .(core_priest_sex, ssot_gender)]

  # age (likely untraced (by NHS Digital) if DOB incorrect)
  core_priest_data_v1[, .N, by = .(ages_match = core_priest_calculated_age == ssot_calculated_age)]

  # 30 day mortality
  core_priest_data_v1[mortality_status %in% c("Alive", "Dead"), mortality_status_died := (mortality_status == "Dead")]
  ## All records
  core_priest_data_v1[, .N, by = .(day30_vital_status_match = (mortality_status_died == dr_death_within_30_days_contact), mortality_status_died, dr_death_within_30_days_contact)][order(-day30_vital_status_match, -mortality_status_died, -dr_death_within_30_days_contact)]
  ## Records with disagreement
  core_priest_data_v1[mortality_status_died != dr_death_within_30_days_contact, .N, by = .(mortality_status_died, dr_death_within_30_days_contact, dr_died)][order(-mortality_status_died, -dr_death_within_30_days_contact, -dr_died)]

  # ED attendance
  core_priest_data_v1[, .N, by = .(most_likely_diagnosis, ed_covid_diagnosis_30_days)]
  core_priest_data_v1[, .(pc = round(sum(most_likely_diagnosis == "Covid-19" & ed_covid_diagnosis_30_days == FALSE, na.rm = TRUE) / .N * 100, 1)), by = site]

  # Admitted
  core_priest_data_v1[disp_cat %in% c("admitted", "discharged"), disp_cat_admitted := (disp_cat == "admitted")]
  core_priest_data_v1[, .(pc = round(sum(disp_cat_admitted == TRUE & apc_number_of_attendences_in_7_days == 0, na.rm = TRUE) / .N * 100, 1)), by = site]

  # Smoking
  core_priest_data_v1[, .N, by = .(smoking_status_match = tobacco_user == gdppr_comorb_present_smoker)]
  core_priest_data_v1[, .N, by = .(tobacco_user, gdppr_comorb_present_smoker, gdppr_comorb_value_smoker)][order(-tobacco_user, -gdppr_comorb_present_smoker, -N)]

  # Pregnant
  core_priest_data_v1[, .N, by = .(pregnant_status_match = pregnant == gdppr_comorb_present_pregnancy)]
  core_priest_data_v1[, .N, by = .(pregnant, gdppr_comorb_present_pregnancy, gdppr_comorb_value_pregnancy)][order(-pregnant, -gdppr_comorb_present_pregnancy, -N)]

  # hypertension
  core_priest_data_v1[, .N, by = .(hypertension, gdppr_comorb_present_hypertension)][order(-N)]
  core_priest_data_v1[hypertension != gdppr_comorb_present_hypertension, .N, by = .(hypertension, gdppr_comorb_present_hypertension,  gdppr_comorb_value_hypertension)][order(-hypertension, -gdppr_comorb_present_hypertension, -N)]

  # active_malignancy
  core_priest_data_v1[, .N, by = .(active_malignancy, gdppr_comorb_present_malignancy)][order(-N)]
  core_priest_data_v1[active_malignancy != gdppr_comorb_present_malignancy, .N, by = .(active_malignancy, gdppr_comorb_present_malignancy,  gdppr_comorb_value_malignancy)][order(-active_malignancy, -gdppr_comorb_present_malignancy, -N)]

  # diabetes
  core_priest_data_v1[, .N, by = .(diabetes, gdppr_comorb_present_diabetes)][order(-N)]
  core_priest_data_v1[diabetes != gdppr_comorb_present_diabetes, .N, by = .(diabetes, gdppr_comorb_present_diabetes,  gdppr_comorb_value_diabetes)][order(-diabetes, -gdppr_comorb_present_diabetes, -N)]

  # renal impairment
  core_priest_data_v1[, .N, by = .(renal_impairment, gdppr_comorb_present_renal_impairment)][order(-N)]
  core_priest_data_v1[renal_impairment != gdppr_comorb_present_renal_impairment, .N, by = .(diabetes, gdppr_comorb_present_diabetes,  gdppr_comorb_value_diabetes)][order(-diabetes, -gdppr_comorb_present_diabetes, -N)]

  # Obesity
  core_priest_data_v1[, .N, by = .(obesity_status_match = clinically_obese == gdppr_comorb_present_obesity)]
  core_priest_data_v1[, gdppr_comorb_value_obesity_bmi := as.integer(gdppr_comorb_value_obesity)]
  core_priest_data_v1[!is.na(gdppr_comorb_value_obesity_bmi), gdppr_comorb_value_obesity := paste("BMI 30+: ", gdppr_comorb_value_obesity_bmi > 30)]
  core_priest_data_v1[, gdppr_comorb_value_obesity_bmi := NULL]

  core_priest_data_v1[, .N, by = gdppr_comorb_value_obesity]
  core_priest_data_v1[clinically_obese != gdppr_comorb_present_obesity, .N, by = .(clinically_obese, gdppr_comorb_present_obesity, gdppr_comorb_value_obesity)][order(-clinically_obese, -gdppr_comorb_present_obesity, -N)]

  # current meds
  core_priest_data_v1[, .N, by = .(no_current_medication, gdppr_no_drugs = drug_count == 0)][order(-N)]

