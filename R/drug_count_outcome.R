getDrugCountByPatientDate <- function(patients_dates,
                                      pt_id_field = "patient_id",
                                      activity_date_field,
                                      duration_days = 365L) {

  # patients_dates:     data table of patient_ids and activity dates for which a
  #                     count of different drugs is sought within the previous
  #                     duration_days
  # pt_id_field:        patient_id field name in patients_dates
  # activity_date_field:activity_date field name in patients_dates
  # duration_days:      Number of days prior to activity_date over which to
  #                     search for drugs.

  patients_dates_unique <- copy(patients_dates)
  setnames(patients_dates_unique, c(pt_id_field, activity_date_field), c("._patient_id_.", "._activity_date_."))
  patients_dates_unique <- unique(patients_dates_unique[, .(._patient_id_., ._activity_date_.)])

  # Read in GDPPR data and med look-up data, and merge
  demo_file_id <- "FILE0115974_2021-03-18-163613"
  gdppr_data <- readRDS(file = paste0("data/datasets/cohort_", demo_file_id, "_gdppr_data.rds"))
  snomed_bnf_mapping_medecines <- readRDS(file = "data/reference_data/snomed_bnf_mapping_medecines.rds")

  gdppr_data_meds <- merge(gdppr_data,
                           snomed_bnf_mapping_medecines,
                           by.x = "CODE",
                           by.y = "snomed_code",
                           all = FALSE)[, .(patient_id, DATE, bnf_purpose_chemical)]

  # Merge in patients and dates of interest
  drugs_by_patient_date <- merge(patients_dates_unique,
                                 gdppr_data_meds,
                                 by.x = "._patient_id_.",
                                 by.y = "patient_id",
                                 all = FALSE)

  # Count distinct drugs within in relevant period for each (patient, activity date)-tuple
  drugs_by_patient_date <- drugs_by_patient_date[DATE < ._activity_date_. & DATE >= ._activity_date_. - duration_days,
                                                 .N,
                                                 by = .(._patient_id_., ._activity_date_., bnf_purpose_chemical)][,
                                                                                                                  .(drug_count = .N),
                                                                                                                  by = .(._patient_id_., ._activity_date_.)]

  # Merge above into all (patient, activity date)-tuples
  drugs_by_all_patient_date <- merge(patients_dates_unique,
                                     drugs_by_patient_date,
                                     by = c("._patient_id_.", "._activity_date_."),
                                     all.x = TRUE)

  # For those tuples with no drug_count from merge above, set drug count to 0.
  drugs_by_all_patient_date[is.na(drug_count), drug_count := 0L]

  # Replace orignal field names
  setnames(drugs_by_all_patient_date, c("._patient_id_.", "._activity_date_."), c(pt_id_field, activity_date_field))

  # Return
  return(drugs_by_all_patient_date)
}
