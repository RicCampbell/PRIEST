library(data.table)
library(lubridate)
library(readxl)
source("R/cleaning_fns_etl.r")
source("R/standardise_functions.r")
source("R/outcome_functions.r")


# Read in patient_id lookups ----------------------------------------------

demo_file_id <- "FILE0115974_2021-03-18-163613"
record_id_to_pt_id_lu <- readRDS(paste0("data/linkage/cohort_", demo_file_id, "_record_id_to_pt_id_lu.rds"))


# NHS111 Data -------------------------------------------------------------

nhs111 <- readRDS("data/datasets/nhs111-2020-09-15 151647.rds")

## Attach patient_id
nhs111 <- merge(nhs111,
                record_id_to_pt_id_lu,
                by = "record_ID",
                all.x = TRUE)


## Read in mapping for clinical dispositions

clinical_desc_mapping <- data.table(read_excel("D:/reference_data/Field Mapping and Standardisation.xlsx",
                                               sheet = "nhs111_clinical_disp_standard",
                                               col_names = TRUE,
                                               col_types = "text",
                                               trim_ws = TRUE))

sex_mapping <- data.table(read_excel("D:/reference_data/Field Mapping and Standardisation.xlsx",
                                               sheet = "sex_standardisation",
                                               col_names = TRUE,
                                               col_types = "text",
                                               trim_ws = TRUE))

## Trim white space of clincial desc field

nhs111[, final_dx_desc := trimws(final_dx_desc)]


## Map all clinical outcomes to grouping in SAP

nhs111[, priest_clinical_desc_grouping := clinical_desc_mapping$priest_clinical_desc_grouping[match(final_dx_desc, clinical_desc_mapping$nhs111_desc)]]
nhs111[!(priest_clinical_desc_grouping %in% clinical_desc_mapping$priest_clinical_desc_grouping), priest_clinical_desc_grouping := NA]


## Sex

nhs111[, sex := sex_mapping$destination_code[match(sex, sex_mapping$source_code)]]
nhs111[!(sex %in% sex_mapping$destination_code), sex := NA]


## Age

setnames(nhs111, "age", "age_years")



# Remove records with no patient id ---------------------------------------

## Remove records without patient_id from further analysis
# # Removed [X] records from analysis
# nhs111[is.na(patient_id), .N]
nhs111_excluded <- nhs111[is.na(patient_id)]
nhs111 <- nhs111[!is.na(patient_id)]


# Add in SSoT outcome data ------------------------------------------------

  ## Read in SSoT data

  demo_ssot <- readRDS(file = paste("D:/source_data/ssot/cohort", demo_file_id, "demo_ssot.rds", sep = "_"))

  nhs_111_single_source_data <- getSingleSourceData(nhs111,
                                                    demo_ssot = demo_ssot,
                                                    field_name_patient_id = "patient_id",
                                                    field_name_date_field = "call_datetime",
                                                    field_name_contact_id = "record_ID")

  nhs111[, c("age_years", "sex") := NULL]

  nhs111 <- merge(nhs111,
                  nhs_111_single_source_data,
                  by = "record_ID",
                  all.x = TRUE)


# Add in DR outcome data --------------------------------------------------

  ## Read in DR data

  dr_data <- readRDS(file = paste("data/datasets/cohort", demo_file_id, "dr_data.rds", sep = "_"))

  nhs_111_dr_data <- getDeathRecordsOutcomes(nhs111,
                                             death_register = dr_data,
                                             field_name_patient_id = "patient_id",
                                             field_name_date_field = "call_datetime",
                                             field_name_contact_id = "record_ID")

  nhs111 <- merge(nhs111,
                  nhs_111_dr_data,
                  by = "record_ID",
                  all.x = TRUE)


## This outcomes includes time from incident to death, and is therefore identifiable
## Need to add in CC outcomes and derived outcomes so can remove time to death field at this point

# Add in CC outcome data --------------------------------------------------

  ## Read in CC data

  cc_data <- readRDS(file = paste("data/datasets/cohort", demo_file_id, "cc_data.rds", sep = "_"))

  nhs_111_cc_data <- getCriticalCareOutcomes(nhs111,
                                             cc_data = cc_data,
                                             field_name_patient_id = "patient_id",
                                             field_name_date_field = "call_datetime",
                                             field_name_contact_id = "record_ID")

  nhs111 <- merge(nhs111,
                  nhs_111_cc_data,
                  by = "record_ID",
                  all.x = TRUE)

# Add in derived cc-dr outcome data ---------------------------------------

  nhs_111_derived_adverse_outcomes <- getDerivedAdverseOutcomes(nhs_111_dr_data, nhs_111_cc_data, "record_ID")

  nhs111 <- merge(nhs111,
                  nhs_111_derived_adverse_outcomes,
                  by = "record_ID",
                  all.x = TRUE)


# Find last disposition code prior to adverse event -----------------------

  ## Set order then give records contact number/order by patient

  setorder(nhs111, patient_id, call_datetime, record_ID, na.last = TRUE)

  nhs111[, patient_call_record_order := 1:.N, by = patient_id]


  ## Get records that are first contact with an adverse event

  first_contact_adverse_event <- nhs111[any_adverse_outcome == TRUE & patient_call_record_order == 1]


  ## Create field of date of adverse event

  first_contact_adverse_event[, adverse_event_date := as.Date(call_datetime + pmin(dr_time_from_incident_to_death, crit_care_call_to_cc_time, na.rm = TRUE))]

  stopifnot(first_contact_adverse_event[is.na(adverse_event_date), .N] == 0)


  ## Get all, not first contact, records for people who had an adverse event

  adverse_outcome_all_non_first_contacts <- nhs111[patient_id %chin% first_contact_adverse_event$patient_id,
                                                   .(record_ID, patient_id, call_datetime, final_dx_desc,
                                                     priest_clinical_desc_grouping, patient_call_record_order)][patient_call_record_order > 1]


  ## Merge on patient to add date of adverse event

  adverse_outcome_all_non_first_contacts_adverse_date <- merge(adverse_outcome_all_non_first_contacts,
                                                               first_contact_adverse_event[, .(patient_id, adverse_event_date)],
                                                               by = "patient_id",
                                                               all.x = TRUE)


  ## Create field of call datetime as just date for comparison

  adverse_outcome_all_non_first_contacts_adverse_date[, call_date := as.Date(call_datetime)]

  subsequent_calls_prior_adverse_event <- adverse_outcome_all_non_first_contacts_adverse_date[call_date <= adverse_event_date]

  last_call_prior_adverse_event <- subsequent_calls_prior_adverse_event[, .SD[which.max(patient_call_record_order)], by = patient_id]


  ## Check have one record for each patient required

  stopifnot(uniqueN(last_call_prior_adverse_event[, patient_id]) == uniqueN(subsequent_calls_prior_adverse_event[, patient_id]))


  ## Change col names before merging back into original records so can make sense of which is which, and remove unwanted fields

  setnames(last_call_prior_adverse_event, c("final_dx_desc", "priest_clinical_desc_grouping", "patient_call_record_order"),
           c("last_final_dx_desc", "last_priest_clinical_desc_grouping", "last_patient_call_record_order"))

  last_call_prior_adverse_event[, adverse_event_date := NULL]


  ## Merge back into original record

  first_contact_adverse_event_final_disposition <- merge(first_contact_adverse_event,
                                                         last_call_prior_adverse_event[, .(patient_id, last_final_dx_desc, last_priest_clinical_desc_grouping,
                                                                                           last_patient_call_record_order)],
                                                         by = "patient_id",
                                                         all.x = TRUE)


  ## Remove identifiable fields

  nhs111[, c("dr_time_from_incident_to_death", "crit_care_call_to_cc_time") := NULL]

  first_contact_adverse_event_final_disposition[, c("dr_time_from_incident_to_death", "crit_care_call_to_cc_time", "adverse_event_date") := NULL]


# Save processed data -----------------------------------------------------

  saveRDS(nhs111_excluded, file = paste0("data/datasets/cohort_", demo_file_id, "_nhs111_excluded_data.rds"))
  saveRDS(nhs111, file = paste0("data/datasets/cohort_", demo_file_id, "_nhs111_data_ssot_dr_outcomes.rds"))
  saveRDS(first_contact_adverse_event_final_disposition,file = paste0("data/datasets/cohort_", demo_file_id, "_nhs111_data_ssot_dr_outcomes_multi_call_adverse_event.rds"))

