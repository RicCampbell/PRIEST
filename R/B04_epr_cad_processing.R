library(data.table)
library(lubridate)
library(readxl)
source("R/cleaning_fns_etl.r")
source("R/standardise_functions.r")
source("R/outcome_functions.r")


# Read in patient_id lookups ----------------------------------------------

demo_file_id <- "FILE0115974_2021-03-18-163613"
record_id_to_pt_id_lu <- readRDS(paste0("data/linkage/cohort_", demo_file_id, "_record_id_to_pt_id_lu.rds"))

# Check no ePR data ID conflicts ------------------------------------------

load("data/datasets/epr_tables_freetext_anonymised-2020-09-15 151647.rda")

epr_pt_id_to_epr_id_lu <- unique(merge(epr_record_ID_tbl,
                                       record_id_to_pt_id_lu,
                                       by = "record_ID",
                                       all.x = TRUE)[, .(patient_id, epr_id)])

# OK within epr_id
stopifnot(epr_pt_id_to_epr_id_lu[, .N, by = epr_id][N > 1, .N] == 0)

# It is valid that there may be >1 patient_id per amb_incident_id in epr data but unusual
epr_id_to_pt_id_to_amb_incident_id_lu <- unique(merge(epr_single_value_fields_tbl[, .(epr_id, amb_incident_id)],
                                                epr_pt_id_to_epr_id_lu,
                                                by = "epr_id",
                                                all.x = TRUE)[, .(amb_incident_id, epr_id, patient_id)])

epr_amb_incident_id_to_pt_id_lu <- epr_id_to_pt_id_to_amb_incident_id_lu[!is.na(amb_incident_id), .(N_eprs_per_patient_incident = .N), by = .(amb_incident_id, patient_id)][, ':=' (N_unidentified_eprs_per_incident = .SD[is.na(patient_id), N_eprs_per_patient_incident],
                                                                                                                                                                                        N_identified_patients_per_incident = .SD[!is.na(patient_id), .N]),
                                                                                                                                                                                by = .(amb_incident_id)]
epr_amb_incident_id_to_pt_id_lu[is.na(N_unidentified_eprs_per_incident), N_unidentified_eprs_per_incident := 0L]

epr_amb_incident_id_to_single_pt_id_lu <- epr_amb_incident_id_to_pt_id_lu[(N_identified_patients_per_incident == 1 & N_unidentified_eprs_per_incident == 0) |
                                                                            (N_identified_patients_per_incident == 0 & N_unidentified_eprs_per_incident == 1), .(amb_incident_id, patient_id)]

epr_amb_incident_id_to_known_multiple_pt_id_lu <- epr_amb_incident_id_to_pt_id_lu[(N_identified_patients_per_incident > 1 & N_unidentified_eprs_per_incident == 0), .(amb_incident_id, patient_id)]



# Check no CAD ID conflicts -----------------------------------------------

cad_data <- readRDS("data/datasets/cad_data-2020-09-15 151647.rds")

cad_amb_incident_id_to_pt_id_lu <- unique(merge(cad_data[, .(record_ID, amb_incident_id)],
                                                record_id_to_pt_id_lu,
                                                by = "record_ID",
                                                all.x = TRUE)[, .(patient_id, amb_incident_id)])

# CAD data contains no internal conflicts
stopifnot(cad_amb_incident_id_to_pt_id_lu[!is.na(patient_id), .N, by = amb_incident_id][N > 1, .N] == 0)


# Check ePR and CAD ePR records for ID conflicts ------------------------

amb_incident_id_to_single_pt_id_lu <- merge(epr_amb_incident_id_to_single_pt_id_lu[, .(amb_incident_id, patient_id_epr = patient_id)],
                                            cad_amb_incident_id_to_pt_id_lu[, .(amb_incident_id, patient_id_cad = patient_id)],
                                            by = "amb_incident_id",
                                            all = FALSE)

#  comparable records
# amb_incident_id_to_single_pt_id_lu[, .N]
#  No IDs in either
# amb_incident_id_to_single_pt_id_lu[is.na(patient_id_cad) & is.na(patient_id_epr), .N]
#  match
# amb_incident_id_to_single_pt_id_lu[patient_id_cad == patient_id_epr, .N]
#  id's from ePR not in CAD
# amb_incident_id_to_single_pt_id_lu[is.na(patient_id_cad) & !is.na(patient_id_epr), .N]
#  id's from CAD not in ePR
# amb_incident_id_to_single_pt_id_lu[!is.na(patient_id_cad) & is.na(patient_id_epr), .N]
#  conflicts
# amb_incident_id_to_single_pt_id_lu[patient_id_cad != patient_id_epr, .N]

# recode appropriately
amb_incident_id_to_single_pt_id_lu[patient_id_cad == patient_id_epr, patient_id := patient_id_cad]
amb_incident_id_to_single_pt_id_lu[is.na(patient_id_cad) & !is.na(patient_id_epr), patient_id := patient_id_epr]
amb_incident_id_to_single_pt_id_lu[!is.na(patient_id_cad) & is.na(patient_id_epr), patient_id := patient_id_cad]
amb_incident_id_to_single_pt_id_lu[patient_id_cad != patient_id_epr, patient_id := NA]
amb_incident_id_to_single_pt_id_lu[, c("patient_id_cad", "patient_id_epr") := NULL]

# Ensure no missing amb_incident_ids and no more than one pt_id per amb_incident
stopifnot(amb_incident_id_to_single_pt_id_lu[is.na(amb_incident_id), .N] == 0 &
            amb_incident_id_to_single_pt_id_lu[, .N, by = amb_incident_id][N > 1, .N] == 0)


# Validation check for epr amb incidents with multiple pt and no unidentified records,
# that one of the identified patients matches the CAD patient (if present)
amb_incident_id_to_known_multiple_pt_id_lu <- merge(epr_amb_incident_id_to_known_multiple_pt_id_lu[, .(amb_incident_id, patient_id_epr = patient_id)],
                                                    cad_amb_incident_id_to_pt_id_lu[, .(amb_incident_id, patient_id_cad = patient_id)],
                                                    by = "amb_incident_id",
                                                    all = FALSE)[!is.na(patient_id_cad)]
stopifnot(all(amb_incident_id_to_known_multiple_pt_id_lu[, .(id_match = any(patient_id_cad == patient_id_epr)), by = amb_incident_id][, id_match]))



# Add/remove patient_id's found from ePR to CAD data ----------------------

cad_amb_incident_id_to_pt_id_final_lu <- merge(cad_amb_incident_id_to_pt_id_lu,
                                         amb_incident_id_to_single_pt_id_lu[, .(amb_incident_id, new_patient_id = patient_id, linked = TRUE)],
                                         by = "amb_incident_id",
                                         all.x = TRUE)

cad_amb_incident_id_to_pt_id_final_lu[!is.na(linked), patient_id := new_patient_id]
cad_amb_incident_id_to_pt_id_final_lu[, c("new_patient_id", "linked") := NULL]

stopifnot(cad_amb_incident_id_to_pt_id_final_lu[, .N, by = amb_incident_id][N > 1, .N] ==0)

# Add/remove patient_id's to ePR data -------------------------------------

epr_id_to_pt_id_final_lu <- merge(epr_id_to_pt_id_to_amb_incident_id_lu,
                                  amb_incident_id_to_single_pt_id_lu[, .(amb_incident_id, new_patient_id = patient_id, linked = TRUE)],
                                  by = "amb_incident_id",
                                  all.x = TRUE)

epr_id_to_pt_id_final_lu[!is.na(linked), patient_id := new_patient_id]
epr_id_to_pt_id_final_lu[, c("new_patient_id", "linked") := NULL]

epr_id_to_pt_id_final_lu[!is.na(amb_incident_id),
                         ':=' (any_unidentified_eprs_per_incident = any(is.na(patient_id)),
                               N_patients_per_incident = uniqueN(patient_id, na.rm = TRUE)),
                         by = amb_incident_id]

epr_id_to_pt_id_final_lu[!is.na(amb_incident_id) &
                           !is.na(patient_id) &
                           any_unidentified_eprs_per_incident == FALSE,
                         multiple_eprs_per_patient_incident := (.N > 1),
                         by = .(patient_id, amb_incident_id)]

epr_id_to_pt_id_final_lu[N_patients_per_incident > 1,
                         multi_patient_incident := TRUE]
epr_id_to_pt_id_final_lu[N_patients_per_incident == 1 &
                           any_unidentified_eprs_per_incident == FALSE,
                         multi_patient_incident := FALSE]

epr_id_to_pt_id_final_lu[, c("N_patients_per_incident", "any_unidentified_eprs_per_incident", "amb_incident_id") := NULL]

stopifnot(epr_id_to_pt_id_final_lu[, .N, by = epr_id][N > 1, .N] ==0)

# Clean up workspace ------------------------------------------------------

data_obj_names <- setdiff(ls(), lsf.str())
rm(list = data_obj_names[!(data_obj_names %in% c("demo_file_id",
                                                 "cad_amb_incident_id_to_pt_id_final_lu",
                                                 "epr_id_to_pt_id_final_lu"))])


# ePR data standardisation ------------------------------------------------

## Read in epr tables and change hash cols to character cols

  epr_data_object_names <- load("data/datasets/epr_tables_freetext_anonymised-2020-09-15 151647.rda")

## Change all hash cols to character cols (epr_id and amb_incident_id)

  epr_freetext_excl_ecg_tbl[, epr_id := as.character(epr_id)]
  epr_ecg_incl_freetext_tbl[, epr_id := as.character(epr_id)]
  epr_airways_intervention_tbl[, epr_id := as.character(epr_id)]
  epr_cardiac_respiratory_arrest_tbl[, epr_id := as.character(epr_id)]
  epr_drugs_tbl[, epr_id := as.character(epr_id)]
  epr_news_score_tbl[, epr_id := as.character(epr_id)]
  epr_phys_observations_tbl[, epr_id := as.character(epr_id)]
  epr_record_ID_tbl[, epr_id := as.character(epr_id)]
  epr_referral_type_crew_tbl[, epr_id := as.character(epr_id)]
  epr_single_value_fields_tbl[, ':=' (epr_id = as.character(epr_id), amb_incident_id = as.character(amb_incident_id))]


## check all have been changed by using epr_data_object_names

  sapply(epr_data_object_names, function(table_name) {
    dt <- get(table_name)
    if(all(class(dt$epr_id) != "character")) stop(paste("dt[, class(epr_id)] == \"character\" are not all TRUE for table", table_name), call. = FALSE)
  })


# Attach patient_id
  epr_single_value_fields_tbl <- merge(epr_single_value_fields_tbl,
                                       epr_id_to_pt_id_final_lu,
                                       by = "epr_id",
                                       all.x = TRUE)


## Read in files for sex standardisation mapping that will be used on all datasets

  sex_mapping <- data.table(read_excel("D:/reference_data/Field Mapping and Standardisation.xlsx",
                                       sheet = "sex_standardisation",
                                       col_names = TRUE,
                                       col_types = "text",
                                       trim_ws = TRUE))


## Read in files for ethnicity standardisation mapping that will be used on all datasets

  ethnicity_mapping <- data.table(read_excel("D:/reference_data/Field Mapping and Standardisation.xlsx",
                                       sheet = "ethnicity_standardisation",
                                       col_names = TRUE,
                                       col_types = "text",
                                       trim_ws = TRUE))


## Read in files for non transport reasons mapping that will be used on epr data

  epr_non_transport_reason_mapping <- data.table(read_excel("D:/reference_data/Field Mapping and Standardisation.xlsx",
                                             sheet = "epr_non_transport_reason",
                                             col_names = TRUE,
                                             col_types = "text",
                                             trim_ws = TRUE))


## Read in file for epr department mapping that will be used on epr data

  epr_receiving_hospital_dept_mapping <- data.table(read_excel("D:/reference_data/Field Mapping and Standardisation.xlsx",
                                             sheet = "epr_receiving_hospital_dept",
                                             col_names = TRUE,
                                             col_types = "text",
                                             trim_ws = TRUE))


## Read in file for hospital mapping that will be used on all dataset

  yas_receiving_hospital_mapping <- data.table(read_excel("D:/reference_data/Field Mapping and Standardisation.xlsx",
                                                               sheet = "yas_receiving_hospital",
                                                               col_names = TRUE,
                                                               col_types = "text",
                                                               trim_ws = TRUE))


## Read in file for epr drug mapping that will be used on epr datasets

  epr_drug_name_mapping <- data.table(read_excel("D:/reference_data/Field Mapping and Standardisation.xlsx",
                                                          sheet = "epr_drug_name_standardisation",
                                                          col_names = TRUE,
                                                          col_types = "text",
                                                          trim_ws = TRUE))


## Read in file for cad clinic attended mapping that will be used on cad dataset

  cad_clinic_mapping <- data.table(read_excel("D:/reference_data/Field Mapping and Standardisation.xlsx",
                                                 sheet = "cad_clinic_attended",
                                                 col_names = TRUE,
                                                 col_types = "text",
                                                 trim_ws = TRUE))




# epr_single_value_fields_tbl standardisation -----------------------------------------------------

## Sex

  epr_single_value_fields_tbl[, sex := sex_mapping$destination_code[match(sex, sex_mapping$source_code)]]
  epr_single_value_fields_tbl[!(sex %in% sex_mapping$destination_code), sex := NA]


## Ethnicity

  epr_ethnicty_na_count <- epr_single_value_fields_tbl[is.na(ethnicity), .N]


## Remove core-priest col

  epr_ethnicty_mapping <- unique(ethnicity_mapping[!is.na(epr_ethnicity), .(ethnicity_group, ethnicity_code, ethnicity_desc, epr_ethnicity)])


## Map ethnicity from current epr field to standardised 2001 census categories, if no mapping, return NA

  epr_single_value_fields_tbl <- epr_single_value_fields_tbl[, ethnicity := epr_ethnicty_mapping$ethnicity_code[match(ethnicity, epr_ethnicty_mapping$epr_ethnicity)]]
  epr_single_value_fields_tbl[!(ethnicity %in% ethnicity_mapping$ethnicity_code), ethnicity := NA]


## Check same number of NAs

  stopifnot(epr_single_value_fields_tbl[is.na(ethnicity), .N] == epr_ethnicty_na_count)


## Age
## Only need to change those whose unit is not (Y)ears

  epr_age_na_count <- epr_single_value_fields_tbl[is.na(age), .N]

  epr_single_value_fields_tbl[age_unit == "M", age := floor(age/12)]
  epr_single_value_fields_tbl[age_unit == "W", age := floor(age/52)]

  epr_single_value_fields_tbl[, age_unit := NULL]


## Check as expected

  stopifnot(epr_single_value_fields_tbl[is.na(age), .N] == epr_age_na_count)


## Check all dates are valid

  ## get date cols

  epr_date_cols <- getPOSIXtFields(epr_single_value_fields_tbl)


  ## Remove all 'not recorded dates'

  epr_single_value_fields_tbl[, (epr_date_cols) := lapply(.SD, function(x) replace(x, as.Date(x) == "1753-01-01", NA)), .SDcols = epr_date_cols]

  ## Check dates if put back in

  # stopifnot(datesWithinRange(table = epr_single_value_fields_tbl, date_cols = epr_date_cols,
  #                              min_date = "2020-03-26", max_date = "2020-06-25"))


## non transport reason mapping -  - see Pre-Hospital PRIEST Validation/Standardisation: field_to_check_grouping

  epr_single_value_fields_tbl[, non_transport_reason := epr_non_transport_reason_mapping$non_transport_reason_mapping[match(non_transport_reason, epr_non_transport_reason_mapping$non_transport_reason)]]
  epr_single_value_fields_tbl[!(non_transport_reason %in% epr_non_transport_reason_mapping$non_transport_reason_mapping), non_transport_reason := NA]


## receiving hospital - add ODS code for each

  epr_single_value_fields_tbl[, receiving_hospital := yas_receiving_hospital_mapping$receiving_hospital_code[match(receiving_hospital, yas_receiving_hospital_mapping$receiving_hospital)]]
  epr_single_value_fields_tbl[!(receiving_hospital %in% yas_receiving_hospital_mapping$receiving_hospital_code), receiving_hospital := NA]


## receiving hospital department -  - see Pre-Hospital PRIEST Validation/Standardisation: field_to_check_grouping

  setnames(epr_single_value_fields_tbl, "receiving_hospital_departement", "receiving_hospital_department")
  epr_single_value_fields_tbl[, receiving_hospital_department := epr_receiving_hospital_dept_mapping$receiving_hospital_department_mapped[match(receiving_hospital_department, epr_receiving_hospital_dept_mapping$receiving_hospital_department)]]
  epr_single_value_fields_tbl[!(receiving_hospital_department %in% epr_receiving_hospital_dept_mapping$receiving_hospital_department_mapped), receiving_hospital_department := NA]


# epr-airways standardisation --------------------------------------------

  ## No airway is size 100, change to NA - see Pre-Hospital PRIEST Validation/Standardisation: Field Questions

  epr_airways_intervention_tbl[airway_intervention_size >= 100L, airway_intervention_size := NA]


# epr-drug standardisation -----------------------------------------------

  ## Drugs mapping - see Pre-Hospital PRIEST Validation/Standardisation: field_to_check_grouping

  epr_drugs_tbl[, drug_name := epr_drug_name_mapping$drug_name_mapped[match(drug_name, epr_drug_name_mapping$drug_name)]]
  epr_drugs_tbl[!(drug_name %in% epr_drug_name_mapping$drug_name_mapped), drug_name := NA]


## Trim white space from drug dose

  epr_drugs_tbl[, drug_dose := trimws(drug_dose)]


## Only X records that have % in drug_dose, and have % as drug_dose_unit

  #epr_drugs_tbl[, drug_dose := gsub("%$", "", drug_dose)]


## NOTE - all dose with a range (1-4), are for oxygen and are litres/min


## Check all dates are valid

  ## get date cols

  drug_date_cols <- getPOSIXtFields(epr_drugs_tbl)


  ## Remove all 'not recorded dates'

  epr_drugs_tbl[, (drug_date_cols) := lapply(.SD, function(x) replace(x, as.Date(x) == "1753-01-01", NA)), .SDcols = drug_date_cols]


  ## Check dates if put back in

  # stopifnot(datesWithinRange(table = epr_drugs_tbl, date_cols = drug_date_cols,
  #                              min_date = "2020-03-26", max_date = "2020-06-25"))


# epr-ecg standardisation ------------------------------------------------

  ## ECG recording order should be primary only, see Pre-Hospital PRIEST Validation/Standardisation: Field Questions
  ## There are [X] id's that have a 'subsequent' recording but no primary

  epr_ecg_incl_freetext_tbl[, ecg_findings := removeFreetextWS(ecg_findings)]


## Check all dates are valid

  ## get date cols
  ecg_date_cols <- getPOSIXtFields(epr_ecg_incl_freetext_tbl)


  ## Remove all 'not recorded dates'

  epr_ecg_incl_freetext_tbl[, (ecg_date_cols) := lapply(.SD, function(x) replace(x, as.Date(x) == "1753-01-01", NA)), .SDcols = ecg_date_cols]


  ## Check dates if put back in

  # stopifnot(datesWithinRange(table = epr_ecg_incl_freetext_tbl, date_cols = ecg_date_cols,
  #                              min_date = "2020-03-26", max_date = "2020-06-25"))



# epr freetext standardisation --------------------------------------------

  free_text_cols <- colnames(epr_freetext_excl_ecg_tbl)[colnames(epr_freetext_excl_ecg_tbl) != "epr_id"]
  epr_freetext_excl_ecg_tbl[, (free_text_cols) := lapply(.SD, removeFreetextWS), .SDcols = free_text_cols]


  ## Not doing anything to free text fields, will be done on ml VM, or meds taken from GDPPR data
  ## Current Medications (**SAP has interested in number of current meds**)

  # ## Change to all uppercase to remove case duplication
  #
  #   epr_single_value_fields_tbl[, current_medications := toupper(current_medications)]
  #
  #
  # ## Remove all return line characters
  #
  #   epr_single_value_fields_tbl[, current_medications := trimws(gsub("[\n]{2,}", "\n", current_medications))]
  #   epr_single_value_fields_tbl[, current_medications := gsub("\n, ", ", ", current_medications)]
  #   epr_single_value_fields_tbl[, current_medications := gsub("\n", ", ", current_medications)]
  #
  #
  # ## NIL == NONE, unknown == NA
  #
  #   epr_single_value_fields_tbl[current_medications == "NIL" | current_medications == "NILL", current_medications := "NONE"]
  #   epr_single_value_fields_tbl[current_medications == "UNKNOWN", current_medications := NA]


# epr-physical observations standardisation ---------------------------------------


## blood sugar reading

  ## Mentioned in Pre-Hospital PRIEST Validation/Standardisation: Standardisation sheet

  epr_phys_observations_tbl[blood_sugar_reading < 0L | blood_sugar_reading > 40L, blood_sugar_reading := NA]


## blood pressure - see Pre-Hospital PRIEST Validation/Standardisation: Field Questions for rule confirmation
  ## apply range

  epr_phys_observations_tbl[bp_diastolic < 0L | bp_diastolic >= 200L, bp_diastolic := NA]

  ## diastolic cannot be higher than systolic

  epr_phys_observations_tbl[bp_diastolic > bp_systolic, bp_diastolic := NA]


## blood pressure - systolic

  epr_phys_observations_tbl[bp_systolic < 30L | bp_systolic > 300L, bp_systolic := NA]


## manually recorded pulse rate - see Pre-Hospital PRIEST Validation/Standardisation: Field Questions for rule confirmation

  stopifnot((epr_phys_observations_tbl[manual_pulse_rate == 0, .N]/epr_phys_observations_tbl[, .N]*100) < 0.1)

  epr_phys_observations_tbl[manual_pulse_rate < 5L | manual_pulse_rate > 220L, manual_pulse_rate := NA]


## oxygen saturation - see Pre-Hospital PRIEST Validation/Standardisation: Field Questions for rule confirmation

  epr_phys_observations_tbl[oxygen_saturations < 11L | oxygen_saturations > 100L, oxygen_saturations := NA]


## supplementary oxygen

  epr_phys_observations_tbl[, obs_supplimental_oxygen := as.logical(obs_supplimental_oxygen)]


## respiratory rate - see Pre-Hospital PRIEST Validation/Standardisation: Field Questions for rule confirmation
  ## Check that percent 0 readings is less that 0.1%

  stopifnot((epr_phys_observations_tbl[respiratory_rate == 0L, .N]/epr_phys_observations_tbl[, .N]*100) < 0.1)

  stopifnot(epr_phys_observations_tbl[respiratory_rate >= 100L, .N] == 0)


## temperature - see Pre-Hospital PRIEST Validation/Standardisation: Field Questions for rule confirmation

  epr_phys_observations_tbl[temperature < 25L, temperature := NA]


  ## Mentioned in Pre-Hospital PRIEST Validation/Standardisation: Standardisation sheet but left as is
  ##epr_phys_observations_tbl[, temperature := round(temperature, digits = 1)]


## Check all dates are valid

  ## get date cols

  phys_obs_date_cols <- getPOSIXtFields(epr_phys_observations_tbl)


  ## Remove all 'not recorded dates'

  epr_phys_observations_tbl[, (phys_obs_date_cols) := lapply(.SD, function(x) replace(x, as.Date(x) == "1753-01-01", NA)), .SDcols = phys_obs_date_cols]


  ## Check dates if put back in

  # stopifnot(datesWithinRange(table = epr_phys_observations_tbl, date_cols = phys_obs_date_cols,
  #                              min_date = "2020-03-06", max_date = "2020-06-25"))



# epr-news standardisation ------------------------------------------------
## NEWS score maybe calculated from first ambulance contact from physical observations, but highest score record per id used for validation
## TS note: DO NOT DO THIS IN UNDERLYING DATA - only select max NEWS score for analysis dataset
# unique(epr_news_score_tbl[, news_score := max(news_score), by = epr_id])


# Remove records without patient_ids -----------------------------------------------------------

  ## Remove epr records without patient_id from further analysis
  # # Removed [X] epr records from analysis
  # epr_single_value_fields_tbl[is.na(patient_id), .N]

  included_epr_ids <- epr_single_value_fields_tbl[!is.na(patient_id), epr_id]

  excluded_epr_data_object_names <- sapply(epr_data_object_names, function(dt_name, eprs_to_retain) {
    excluded_table_name <- paste(dt_name, "excluded", sep ="_")
    dt <- get(dt_name)
    assign(dt_name, dt[epr_id %in% eprs_to_retain], envir = parent.frame(n = 3))
    assign(excluded_table_name, dt[!(epr_id %in% eprs_to_retain)], envir = parent.frame(n = 3))
    return(excluded_table_name)
  }, eprs_to_retain = included_epr_ids)


## Add in calculated age and death outcomes now, so can keep this epr dataset so removal/deletion of identifiable data does not affect this

# Add SSoT data outcomes ---------------------------------------------------

  ## Read in SSoT data

  demo_ssot <- readRDS(file = paste("D:/source_data/ssot/cohort", demo_file_id, "demo_ssot.rds", sep = "_"))


  epr_single_source_data <- getSingleSourceData(epr_single_value_fields_tbl,
                                                demo_ssot = demo_ssot,
                                                field_name_patient_id = "patient_id",
                                                field_name_date_field = "incident_datetime",
                                                field_name_contact_id = "epr_id")


  epr_single_value_fields_tbl[, c("age", "ethnicity", "sex") := NULL]

  epr_single_value_fields_tbl <- merge(epr_single_value_fields_tbl,
                                       epr_single_source_data,
                                       by = "epr_id",
                                       all.x = TRUE)


# Add in DR outcome data ------------------------------------------------------

  ## Read in DR data

  dr_data <- readRDS(file = paste("data/datasets/cohort", demo_file_id, "dr_data.rds", sep = "_"))

  epr_dr_data <- getDeathRecordsOutcomes(epr_single_value_fields_tbl,
                                         death_register = dr_data,
                                         field_name_patient_id = "patient_id",
                                         field_name_date_field = "incident_datetime",
                                         field_name_contact_id = "epr_id")

  epr_single_value_fields_tbl <- merge(epr_single_value_fields_tbl,
                                       epr_dr_data,
                                       by = "epr_id",
                                       all.x = TRUE)

## This outcomes includes time from incident to death, and is therefore identifiable
## Need to add in CC outcomes and derived outcomes so can remove time to death field at this point

# Add in CC outcome data --------------------------------------------------

  ## Read in CC data

  cc_data <- readRDS(file = paste("data/datasets/cohort", demo_file_id, "cc_data.rds", sep = "_"))

  epr_cc_data <- getCriticalCareOutcomes(epr_single_value_fields_tbl,
                                         cc_data = cc_data,
                                         field_name_patient_id = "patient_id",
                                         field_name_date_field = "incident_datetime",
                                         field_name_contact_id = "epr_id")

  epr_single_value_fields_tbl <- merge(epr_single_value_fields_tbl,
                                       epr_cc_data,
                                       by = "epr_id",
                                       all.x = TRUE)


# Add in derived cc-dr outcome data ---------------------------------------

  epr_derived_adverse_outcomes <- getDerivedAdverseOutcomes(epr_dr_data, epr_cc_data, "epr_id")

  epr_single_value_fields_tbl <- merge(epr_single_value_fields_tbl,
                                       epr_derived_adverse_outcomes,
                                       by = "epr_id",
                                       all.x = TRUE)

## Remove identifiable fields

  epr_single_value_fields_tbl[, c("dr_time_from_incident_to_death", "crit_care_call_to_cc_time") := NULL]


# Save ePR data -----------------------------------------------------------

  save(list = epr_data_object_names, file = paste0("data/datasets/cohort_", demo_file_id, "_epr_freetext_anonymised_data_ssot_dr_outcomes.rda"))
  save(list = excluded_epr_data_object_names, file = paste0("data/datasets/cohort_", demo_file_id, "_epr_freetext_anonymised_excluded_data.rda"))


# Clean up workspace ------------------------------------------------------

  data_obj_names <- setdiff(ls(), lsf.str())
  rm(list = data_obj_names[!(data_obj_names %in% c("demo_file_id",
                                                   "sex_mapping",
                                                   "cad_clinic_mapping",
                                                   "yas_receiving_hospital_mapping",
                                                   "cad_amb_incident_id_to_pt_id_final_lu",
                                                   "demo_ssot",
                                                   "dr_data",
                                                   "cc_data"))])

# CAD Data ----------------------------------------------------------------

  cad_data_version <- "2020-09-15 151647"
  cad_data <- readRDS(file = paste0("data/datasets/cad_data-", cad_data_version, ".rds"))


## Change any hash cols to character

  cad_data[, amb_incident_id := as.character(amb_incident_id)]


## Attach patient_id's
  cad_data <- merge(cad_data,
                    cad_amb_incident_id_to_pt_id_final_lu[!is.na(patient_id)],
                    by = "amb_incident_id",
                    all.x = TRUE)


## Sex (using mapping file read in at top of script)

  cad_data[, sex := sex_mapping$destination_code[match(sex, sex_mapping$source_code)]]
  cad_data[!(sex %in% sex_mapping$destination_code), sex := NA]


## age

  cad_data[, age_unit := toupper(trimws((gsub("[0-9 ]+", "", age))))]
  cad_data[, age := as.integer(gsub("[^0-9\\.]+", "", age))]

  cad_data[age_unit %in% "MONTHS", age_unit := "M"]
  cad_data[age_unit %in% "DAYS", age_unit := "D"]

  cad_data[age_unit == "D", age := floor(age/365)]
  cad_data[age_unit == "M", age := floor(age/12)]

  cad_data[, age_unit := NULL]


## clinic attended - To upper case, remove spaces around /, group A&E, group all wards

  ## Apply mapping to clinic - agreed in Pre-Hospital PRIEST Validation/Standardisation: Field Questions

  cad_data[, clinic_attended := cad_clinic_mapping$clinic_attended_mapped[match(clinic_attended, cad_clinic_mapping$clinic_attended)]]
  cad_data[!(clinic_attended %in% cad_clinic_mapping$clinic_attended_mapped), clinic_attended := NA]


## Receiving hospital department mapped to ODS code

  cad_data[, hospital_attended := yas_receiving_hospital_mapping$receiving_hospital_code[match(hospital_attended, yas_receiving_hospital_mapping$receiving_hospital)]]
  cad_data[!(hospital_attended %in% yas_receiving_hospital_mapping$receiving_hospital_code), hospital_attended := NA]


# Remove records with no patient id -----------------------------------------------------

  ## Remove records without patient_id from further analysis
  # # Removed [X] records from analysis
  # cad_data[is.na(patient_id), .N]
  cad_excluded_data <- cad_data[is.na(patient_id)]
  cad_data <- cad_data[!is.na(patient_id)]


# Add in SSoT outcome data ------------------------------------------------

  cad_single_source_data <- getSingleSourceData(cad_data,
                                                demo_ssot = demo_ssot,
                                                field_name_patient_id = "patient_id",
                                                field_name_date_field = "call_datetime",
                                                field_name_contact_id = "amb_incident_id")


  cad_data[, c("age", "sex") := NULL]

  cad_data <- merge(cad_data,
                    cad_single_source_data,
                    by = "amb_incident_id",
                    all.x = TRUE)


# Add in DR outcome data --------------------------------------------------

  cad_dr_data <- getDeathRecordsOutcomes(cad_data,
                                         death_register = dr_data,
                                         field_name_patient_id = "patient_id",
                                         field_name_date_field = "call_datetime",
                                         field_name_contact_id = "amb_incident_id")

  cad_data <- merge(cad_data,
                    cad_dr_data,
                    by = "amb_incident_id",
                    all.x = TRUE)

## This outcomes includes time from incident to death, and is therefore identifiable
## Need to add in CC outcomes and derived outcomes so can remove time to death field at this point

# Add in CC outcome data --------------------------------------------------

  cad_cc_data <- getCriticalCareOutcomes(cad_data,
                                         cc_data = cc_data,
                                         field_name_patient_id = "patient_id",
                                         field_name_date_field = "call_datetime",
                                         field_name_contact_id = "amb_incident_id")

  cad_data <- merge(cad_data,
                    cad_cc_data,
                    by = "amb_incident_id",
                    all.x = TRUE)


  # Add in derived cc-dr outcome data ---------------------------------------

  cad_derived_adverse_outcomes <- getDerivedAdverseOutcomes(cad_dr_data, cad_cc_data, "amb_incident_id")

  cad_data <- merge(cad_data,
                    cad_derived_adverse_outcomes,
                    by = "amb_incident_id",
                    all.x = TRUE)

  ## Remove identifiable fields

  cad_data[, c("dr_time_from_incident_to_death", "crit_care_call_to_cc_time") := NULL]


# Save processed data -----------------------------------------------------

  saveRDS(cad_excluded_data, file = paste0("data/datasets/cohort_", demo_file_id, "_cad_excluded_data.rds"))
  saveRDS(cad_data, file = paste0("data/datasets/cohort_", demo_file_id, "_cad_data_ssot_dr_outcomes.rds"))

