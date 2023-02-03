library(data.table)
library(lubridate)
library(readxl)


# ED Outcome Function -----------------------------------------------------

## Function that applies ED outcomes to cohort
### cohort - the cohort to have ED outcomes attached to
### ed_data - the ED dataset to use to find the outcomes
### patient_id - the col name that contains the id unique for each patient
### contact_id - the col name that contains the id unique for each contact

getEDOutcomeData <- function(cohort, ed_data, field_name_patient_id, field_name_date_field, field_name_contact_id){

  # cohort <- cad_data
  # ed_data <- ecds_data_covid_diag
  # field_name_date_field <- "call_datetime"
  # field_name_patient_id <- "patient_id"
  # field_name_contact_id <- "amb_incident_id"


  cohort_copy <- copy(cohort)

  setnames(cohort_copy, c(field_name_date_field, field_name_patient_id, field_name_contact_id),
           c("._date_field_.", "._patient_id_.", "._contact_id_."))


  ## Check that the contact id is never empty and that it appears only once each in cohort

    stopifnot(cohort_copy[is.na(._contact_id_.), .N] == 0)
    stopifnot(cohort_copy[, .N, by = ._contact_id_.][N > 1, .N] == 0)


  ## Merge cohort with the ED data on patient_id

    cohort_with_ed_data <- merge(cohort_copy,
                                 ed_data,
                                 by.x = "._patient_id_.",
                                 by.y = "patient_id",
                                 all.x = TRUE)


  ## check datetime is a date(time), and remove time from this

    stopifnot("POSIXct" %in% cohort_copy[, class(._date_field_.)] | "Date" %in% cohort_copy[, class(._date_field_.)])

    cohort_copy[, ._date_field_. := as.Date(._date_field_.)]


  ## Drop records where arrival time is prior to contact time

    cohort_with_ed_data <- cohort_with_ed_data[ARRIVAL_TIME >= ._date_field_.]


  ## Create field of time between cohort call and ED arrival

    cohort_with_ed_data[, call_to_ed_days := lubridate::interval(._date_field_., ARRIVAL_TIME)/lubridate::ddays(1)]


  ## Change department type into an integer and sort on only records of interest

    cohort_with_ed_data[, ed_department_type := as.integer(DEPARTMENT_TYPE)][, DEPARTMENT_TYPE := NULL]

    setorder(cohort_with_ed_data, ed_department_type, call_to_ed_days)


  ## Order by ED department, which is in acuity order 1-5, and take only highest acuity for each contact id
  ## NAs changed to 99 so can use min function in creating outcome table without errors

    cohort_with_ed_data[call_to_ed_days < 7 , highest_acurity_ed_type_7_days := min(ed_department_type), by = ._contact_id_.][is.na(highest_acurity_ed_type_7_days), highest_acurity_ed_type_7_days := 99]
    cohort_with_ed_data[call_to_ed_days < 30 , highest_acurity_ed_type_30_days := min(ed_department_type), by = ._contact_id_.][is.na(highest_acurity_ed_type_30_days), highest_acurity_ed_type_30_days := 99]



  ## Find for each contact_id if there was a covid diagnosis (7 and 30 days), as ECDS data, all have ED admission, and do attendances counts

    ed_outcomes_data <- cohort_with_ed_data[, .(covid_diagnosis_7_days = any(covid_diagnosis & call_to_ed_days < 7),
                                                covid_diagnosis_30_days = any(covid_diagnosis & call_to_ed_days < 30),
                                                number_of_attendences_in_7_days = sum(call_to_ed_days < 7),
                                                number_of_attendences_in_30_days = sum(call_to_ed_days < 30),
                                                highest_acurity_ed_type_7_days = min(highest_acurity_ed_type_7_days),
                                                highest_acurity_ed_type_30_days = min(highest_acurity_ed_type_30_days)),
                                            by = ._contact_id_.]


  ## Merge outcomes back into full cohort

    ed_outcomes_data <- merge(cohort_copy[, .(._contact_id_.)],
                              ed_outcomes_data,
                              by = "._contact_id_.",
                              all.x = TRUE)


    ed_outcomes_data[is.na(covid_diagnosis_7_days), covid_diagnosis_7_days := FALSE]
    ed_outcomes_data[is.na(covid_diagnosis_30_days), covid_diagnosis_30_days := FALSE]

    ed_outcomes_data[is.na(number_of_attendences_in_7_days), number_of_attendences_in_7_days := 0L]
    ed_outcomes_data[is.na(number_of_attendences_in_30_days), number_of_attendences_in_30_days := 0L]

    ed_outcomes_data[highest_acurity_ed_type_7_days == 99, highest_acurity_ed_type_7_days := NA]
    ed_outcomes_data[highest_acurity_ed_type_30_days == 99, highest_acurity_ed_type_30_days := NA]


  ## Check have kept at most one line for each contact_id and number of rows remain the same

    stopifnot(ed_outcomes_data[, .N, by = ._contact_id_.][ N > 1, .N] == 0)
    stopifnot(cohort[, .N] == ed_outcomes_data[, .N])


  ## Add prefix of dataset to the col names

    setnames(ed_outcomes_data, setdiff(colnames(ed_outcomes_data), "._contact_id_."),
             paste("ed", setdiff(colnames(ed_outcomes_data), "._contact_id_."), sep = "_"))


  ## Change names back to original

    setnames(ed_outcomes_data, "._contact_id_.", field_name_contact_id)


    return(ed_outcomes_data)

}


# GDPPR Outcomes Function -------------------------------------------------

## Function for taking comorb assigned GDPPR records and applying them to a cohort
### cohort - the cohort to have ED outcomes attached to
### contact_time - the col names that contains the time contact was made with the service the cohort represents
### demo_file_id - the identifier of the demographics file used to create SSOT
### field_name_patient_id - the col name that contains the id unique for each patient
### field_name_contact_id - the col name that contains the id unique for each contact

getGDPPROutcome <- function(cohort, gdppr_comorb_data, field_name_patient_id, field_name_date_field, field_name_contact_id)  {

  # cohort <- cad_data
  # gdppr_comorb_data <- readRDS(file = paste0("data/datasets/cohort_", demo_file_id, "_gdppr_comorb_records_post_change.rds"))
  # field_name_patient_id <- "patient_id"
  # field_name_date_field <- "call_datetime"
  # field_name_contact_id <- "amb_incident_id"

  cohort_copy <- copy(cohort)

  setnames(cohort_copy, c(field_name_date_field, field_name_patient_id, field_name_contact_id),
           c("._contact_time_.", "._patient_id_.", "._contact_id_."))

## Check that the contact id is never empty and that it appears only once each in cohort

  stopifnot(cohort_copy[is.na(._contact_id_.), .N] == 0)
  stopifnot(cohort_copy[, .N, by = ._contact_id_.][N > 1, .N] == 0)


## Drop time from contact time (if present) field as do not have time for GDPPR record

  stopifnot("POSIXct" %in% cohort_copy[, class(._contact_time_.)] | "Date" %in% cohort_copy[, class(._contact_time_.)])

  cohort_copy[, ._contact_time_. := as.Date(._contact_time_.)]


  ## Change name of record_ID for GDPPR data so as do not end up with both .x and .y versions

    setnames(gdppr_comorb_data, "record_ID", "gdppr_record_ID")


  ##  Merge with the cohort by patient_id, want cohort first so keep all patients

    merged_comorbs_cohort <- merge(cohort_copy,
                                   gdppr_comorb_data,
                                   by.x = "._patient_id_.",
                                   by.y = "patient_id",
                                   all.x = TRUE)


  ## Drop records where arrival time is prior to contact time, NA DATE's are removed here

    merged_comorbs_cohort <- merged_comorbs_cohort[._contact_time_. >= DATE]


  ## Add field of time between GDPPR record and contact of service, and keep only records within the time limit allowed for that CODE

    merged_comorbs_cohort <- merged_comorbs_cohort[, time_comorb_to_contact := ._contact_time_. - DATE][time_comorb_to_contact <= days_applicable_for]


  ## Take count that can be used after cast

    contact_ids_with_comorb <- merged_comorbs_cohort[, .N, by = ._contact_id_.][, .N]


  ## Set the records in order of dates and the time to contact with most recent first

    setorder(merged_comorbs_cohort, -DATE, gdppr_record_ID)

    merged_comorbs_cohort[, comorb_record_order := 1:.N, by = .(._contact_id_., final_comorb_group)]

    merged_comorbs_cohort[final_comorb_group == "immunosuppression" | final_comorb_group == "immunosuppression_meds_steroids",
                          immuno_order := 1:.N, by = .(._contact_id_.)]

 ## Original code commented out to check/change to make
    # merged_comorbs_cohort[final_comorb_group == "smoker" | final_comorb_group == "ex-smoker",
    #                       smoker_order := 1:.N, by = .(._contact_id_.)]


  ## Retain the most recent record per comorb
## Original code commented out to make check/changes
    comorbs_at_contact <- merged_comorbs_cohort[comorb_record_order == 1 & (immuno_order == 1 | is.na(immuno_order))]

    # comorbs_at_contact <- merged_comorbs_cohort[comorb_record_order == 1
    #                                             & (immuno_order == 1 | is.na(immuno_order))
    #                                             & (smoker_order == 1 | is.na(smoker_order))]


  ## Cast per patient to get all the comorbs for that person for that 1st contact

    comorbs_each_contact_id <- dcast(comorbs_at_contact, ._contact_id_. ~ final_comorb_group,
                                     value.var = c("comorb_present", "comorb_value"))


  ## Check have kept to correct number of rows, and one row per contact ID

    stopifnot(comorbs_each_contact_id[, .N] == contact_ids_with_comorb)
    stopifnot(comorbs_each_contact_id[, .N, by = ._contact_id_.][N > 1] == 0)


  ## Create frailty grouping field

    comorbs_each_contact_id[comorb_value_frailty %in% 1:3, frailty_group := "low"]
    comorbs_each_contact_id[comorb_value_frailty %in% 4:6, frailty_group := "medium"]
    comorbs_each_contact_id[comorb_value_frailty %in% 7:9, frailty_group := "high"]


  ## Merge back into full cohort so have outcomes for every row

    gdppr_outcomes_data <- merge(cohort_copy[, .(._contact_id_.)],
                                 comorbs_each_contact_id,
                                 by = "._contact_id_.",
                                 all.x = TRUE)


  ## For all comorb_present cols, change NA to FALSE

    comorb_present_cols <- colnames(gdppr_outcomes_data[, grep("comorb_present", names(gdppr_outcomes_data)), with = FALSE])

    gdppr_outcomes_data[, (comorb_present_cols) := lapply(.SD, function(x) replace(x, is.na(x), FALSE)), .SDcols = comorb_present_cols]


  ## Create col for summarised immunosupression comorb

    gdppr_outcomes_data[comorb_present_immunosuppression == TRUE | comorb_present_immunosuppression_meds_steroids == TRUE,
                        comorb_present_immunosuppression_combined := TRUE][is.na(comorb_present_immunosuppression_combined), comorb_present_immunosuppression_combined := FALSE]


  ## Check have same number of rows as original cohort and each contact id still only appears once

    stopifnot(gdppr_outcomes_data[, .N] == cohort[, .N])
    stopifnot(gdppr_outcomes_data[, .N, by = ._contact_id_.][N > 1, .N] == 0)


  ## Add prefix of dataset to the col names

    setnames(gdppr_outcomes_data, setdiff(colnames(gdppr_outcomes_data), "._contact_id_."),
             paste("gdppr", setdiff(colnames(gdppr_outcomes_data), "._contact_id_."), sep = "_"))


  ## Change col names back to what they were

    setnames(gdppr_outcomes_data, "._contact_id_.", field_name_contact_id)

    setnames(gdppr_comorb_data, "gdppr_record_ID", "record_ID")


  ## Return final dataset of comorbs for each contact at date of contact

    return(gdppr_outcomes_data)
}


# Comorbdidity Value Check Function ---------------------------------------

## Function for check a single value check for SNOMED codes e.g BMI above 30 = obese
### value - The value to be checked against a range
### system - The scoring system used that indicate the range
### lookup_table - table of scoring systems

valueCheckFunction <- function(value, system, lookup_table) {

  fifelse((value >= lookup_table[scoring_system %in% system, low_range]
           & value <= lookup_table[scoring_system %in% system, high_range]),
          (value >= lookup_table[scoring_system %in% system, score]), NA)
}

valueAssignFunction <- function(system, lookup_table) {

  return(lookup_table[scoring_system %in% system, outcome])
}


# Get Single Source of Truth Data Function --------------------------------------

## Get the SSoT data for incident date
### cohort - The cohort wish to find SSoT data for
### demo_ssot - The single source of truth table to be used
### field_name_patient_id - Name of the field that contains the unique patient identifier
### field_name_date_field - Name of the field that contains the contact/incident date
### field_name_contact_id - Name of the field that contains the unique contact identifier

getSingleSourceData <- function(cohort, demo_ssot, field_name_patient_id, field_name_date_field, field_name_contact_id) {

  # cohort <- cad_data
  # demo_ssot <- demo_ssot
  # field_name_patient_id <- "patient_id"
  # field_name_date_field <- "call_datetime"
  # field_name_contact_id <- "amb_incident_id"


  cohort_copy <- copy(cohort)

  setnames(cohort_copy, c(field_name_patient_id, field_name_date_field, field_name_contact_id),
           c("._patient_id_.", "._date_field_.", "._contact_id_."))


## Check that the contact id is never empty and that it appears only once each in cohort

  stopifnot(cohort_copy[is.na(._contact_id_.), .N] == 0)
  stopifnot(cohort_copy[, .N, by = ._contact_id_.][N > 1, .N] == 0)


  ## Merge into data with incident data

    cohort_ssot_merge <- merge(cohort_copy[, .(._patient_id_., ._date_field_., ._contact_id_.)],
                                demo_ssot[, .(dob, patient_id, ethnicity_group, ethnicity_desc, iod19_decile, gender, core_priest_patient)],
                                by.x = "._patient_id_.",
                                by.y = "patient_id")


  ## Check date field is a date and drop time part if present

    stopifnot("POSIXct" %in% cohort_copy[, class(._date_field_.)] | "Date" %in% cohort_copy[, class(._date_field_.)])

    cohort_ssot_merge[, ._date_field_. := as.Date(._date_field_.)]


  ## Should not be any date of birth after contact date

    stopifnot(cohort_ssot_merge[dob > ._date_field_., .N] == 0)


  ## Calculate age from dob - contact_id kept over patient as is age at incident/contact

    cohort_ssot_merge[, calculated_age := as.period(interval(start = dob, end = ._date_field_.))$year][, c("dob", "._patient_id_.", "._date_field_.") := NULL]

    stopifnot((cohort_ssot_merge[, .N] + cohort_copy[is.na(._patient_id_.), .N]) == cohort_copy[, .N])
    stopifnot(cohort_ssot_merge[is.na(calculated_age), .N] == 0)
    stopifnot(cohort_ssot_merge[, .N] == cohort[, .N])


  ## Add a prefix to the colnames to identify dataset came from

    setnames(cohort_ssot_merge, setdiff(colnames(cohort_ssot_merge), "._contact_id_."),
             paste("ssot", setdiff(colnames(cohort_ssot_merge), "._contact_id_."), sep = "_"))


  ## Change names back

    setnames(cohort_ssot_merge, "._contact_id_.", field_name_contact_id)


    return(cohort_ssot_merge)

}


# Death Records Outcomes Function -----------------------------------------

## Function that gets the outcomes from death record data for a cohort
### cohort - The cohort we wish to find death records outcomes for
### death_register - The death register data to be used
### field_name_patient_id - Name of the field that contains the unique patient identifier
### field_name_date_field - Name of the field that contains the contact/incident date
### field_name_contact_id - Name of the field that contains the unique contact identifier

getDeathRecordsOutcomes <- function(cohort, death_register, field_name_patient_id, field_name_date_field, field_name_contact_id){

  # cohort <- cad_data
  # death_register <- dr_data
  # field_name_patient_id <- "patient_id"
  # field_name_date_field <- "call_datetime"
  # field_name_contact_id <- "amb_incident_id"


## Read in ICD-10 COVID codes taken from - https://hscic.kahootz.com/connect.ti/t_c_home/view?objectID=19099248

  icd_10_covid_codes <- data.table(read_excel("D:/reference_data/ICD_10_COVID_codes_2020-11-25.xlsx",
                                              sheet = "ICD_10_COVID_codes",
                                              col_names = TRUE,
                                              col_types = "text",
                                              trim_ws = TRUE))

  ## Create copy of cohort and change names

    cohort_copy <- copy(cohort)

    setnames(cohort_copy, c(field_name_patient_id, field_name_date_field, field_name_contact_id),
             c("._patient_id_.", "._date_field_.", "._contact_id_."))

  ## Check that the contact id is never empty and that it appears only once each in cohort

    stopifnot(cohort_copy[is.na(._contact_id_.), .N] == 0)
    stopifnot(cohort_copy[, .N, by = ._contact_id_.][N > 1, .N] == 0)


  ## Change datetime to date as don't have time in death date

    stopifnot("POSIXct" %in% cohort_copy[, class(._date_field_.)] | "Date" %in% cohort_copy[, class(._date_field_.)])

    cohort_copy[, ._date_field_. := as.Date(._date_field_.)]


  ## Check all death records have a date of death - some have no cause but want to keep as still a death

    stopifnot(death_register[is.na(DATE_OF_DEATH), .N] == 0)


  ## Merge death register data with cohort - remove those without a date of death

    cohort_death_merge <- merge(cohort_copy[, .(._patient_id_., ._date_field_., ._contact_id_.)],
                                 death_register,
                                 by.x = "._patient_id_.",
                                 by.y = "patient_id")


  ## Melt death register so one line per cause of death code - do not remove NAs as this removes deaths

    code_fields_dr <- paste("S_COD_CODE", 1:14, sep = "_")
    wanted_fields_dr <- c("._patient_id_.", "DATE_OF_DEATH", "._date_field_.", "._contact_id_." , code_fields_dr)

    melted_death_reg <- melt(cohort_death_merge[, .SD, .SDcols = wanted_fields_dr],
                             measure.vars = code_fields_dr,
                             variable.name = "outcome_numb",
                             value.name = c("diag_code"))


  ## Cannot get rid of cols where diag_code == "" as there are deaths with no codes at all.

    melted_death_reg[, covid_related_death_code := diag_code %chin% icd_10_covid_codes$ICD_10_code]


  ## Look for each patient_id if there was a covid code

    melted_death_reg[, covid_related_death_code := any(covid_related_death_code), by = ._patient_id_.]


  ## Create field for time between contact and death date

    melted_death_reg[, time_from_incident_to_death := DATE_OF_DEATH - ._date_field_.]


  ## Add field of within 7 and within 30 days

    melted_death_reg[, ':=' (death_within_7_days_contact = time_from_incident_to_death < 7,
                             death_within_30_days_contact = time_from_incident_to_death < 30)]


  ## Want one row per contact id, and they should all be the same for fields we're interested in
    ## Keeping time to death so can create days to deterioration field using ED outcomes

    death_data_for_contact <- unique(melted_death_reg[, .(._contact_id_., covid_related_death_code, time_from_incident_to_death,
                                                          death_within_7_days_contact, death_within_30_days_contact)])


  ## Check one row per contact id

    stopifnot(death_data_for_contact[, .N, by = ._contact_id_.][N > 1, .N] == 0)
    stopifnot(death_data_for_contact[, .N] == uniqueN(cohort_death_merge[, ._contact_id_.]))


  ## Add col to all rows to indicate that patient died

    death_data_for_contact[, died := TRUE]


  ## Merge back into full cohort

    dr_outcomes_data <- merge(cohort_copy[, .(._contact_id_.)],
                              death_data_for_contact,
                              by = "._contact_id_.",
                              all.x = TRUE)


  ## And change all NAs to FALSE

    cols_to_replaces_nas <- c("covid_related_death_code", "death_within_7_days_contact", "death_within_30_days_contact", "died")

    dr_outcomes_data[, (cols_to_replaces_nas) := lapply(.SD, function(x) replace(x, is.na(x), FALSE)), .SDcols = cols_to_replaces_nas]


  ## Add a prefix to the colnames to identify dataset came from

    setnames(dr_outcomes_data, setdiff(colnames(dr_outcomes_data), "._contact_id_."),
             paste("dr", setdiff(colnames(dr_outcomes_data), "._contact_id_."), sep = "_"))


  ## Change col names back

    setnames(dr_outcomes_data, "._contact_id_.", field_name_contact_id)

    return(dr_outcomes_data)

}


# Critical Care Outcomes Function -----------------------------------------

## Function that gets the outcomes from critical data for a cohort
### cohort - The cohort for find critical care outcomes for
### cc_data - The critical care data to be used for finding outcomes from
### field_name_patient_id - Name of the field that contains the unique patient identifier
### field_name_date_field - Name of the field that contains the contact/incident date
### field_name_contact_id - Name of the field that contains the unique contact identifier

getCriticalCareOutcomes <- function(cohort, cc_data, field_name_patient_id, field_name_date_field, field_name_contact_id){

  # cohort <- cad_data
  # cc_data <- cc_data
  # field_name_patient_id <- "patient_id"
  # field_name_date_field <- "call_datetime"
  # field_name_contact_id <- "amb_incident_id"


  ## Create copy of cohort and change names

    cohort_copy <- copy(cohort)

    setnames(cohort_copy, c(field_name_patient_id, field_name_date_field, field_name_contact_id),
             c("._patient_id_.", "._date_field_.", "._contact_id_."))


  ## Check that the contact id is never empty and that it appears only once each in cohort

    stopifnot(cohort_copy[is.na(._contact_id_.), .N] == 0)
    stopifnot(cohort_copy[, .N, by = ._contact_id_.][N > 1, .N] == 0)


  ## Change datetime to date as don't have time in critical care data

    stopifnot("POSIXct" %in% cohort_copy[, class(._date_field_.)] | "Date" %in% cohort_copy[, class(._date_field_.)])

    cohort_copy[, ._date_field_. := as.Date(._date_field_.)]


  ## Renal/cardiac/respiratory from call date (but will only take first call for each patient in analysis)

  ## Only keep records that have a positive number of days in cols interested in

    cc_adverse_outcome <- cc_data[acardsupdays > 0L | aressupdays > 0L
                                  | bcardsupdays > 0L | bressupdays > 0L
                                  | rensupdays > 0L,
                                  .(admidate, acardsupdays, aressupdays, bcardsupdays, bressupdays, rensupdays, ccstartdate, patient_id)]


  ## Check no records are all NAs - all records have at least one > 0, so can't be a combination of 0's and NA's

    stopifnot(cc_adverse_outcome[is.na(acardsupdays) & is.na(aressupdays)
                                 & is.na(bcardsupdays) & is.na(bressupdays)
                                 & is.na(rensupdays), .N] == 0L)


  ## Merge critical care data and cohort

    cohort_cc_merge <- merge(cohort_copy[, .(._patient_id_., ._date_field_., ._contact_id_.)],
                             cc_adverse_outcome,
                             by.x = "._patient_id_.",
                             by.y = "patient_id")


  ## Find time between call date and critical care date

    cohort_cc_merge[, call_to_cc_time := ccstartdate - ._date_field_.]

    cc_attendance_data <- cohort_cc_merge[call_to_cc_time >= 0L & call_to_cc_time < 30L,
                                          .(call_to_cc_time = min(call_to_cc_time)),
                                          by = ._contact_id_.]


  ## Add field of within 7 and within 30 days

    cc_attendance_data[, ':=' (cc_within_7_days_contact = call_to_cc_time < 7,
                               cc_within_30_days_contact = call_to_cc_time < 30)]


  ## Check only one row per contact id

    stopifnot(cc_attendance_data[, .N, by = ._contact_id_.][N > 1, .N] == 0)


  ## Merge back into full cohort

    cc_outcomes_data <- merge(cohort_copy[, .(._contact_id_.)],
                              cc_attendance_data,
                              by = "._contact_id_.",
                              all.x = TRUE)


  ## And change all NAs to FALSE

    cols_to_replaces_nas <- c("cc_within_7_days_contact", "cc_within_30_days_contact")

    cc_outcomes_data[, (cols_to_replaces_nas) := lapply(.SD, function(x) replace(x, is.na(x), FALSE)), .SDcols = cols_to_replaces_nas]


    stopifnot(cc_outcomes_data[, .N] == cohort[, .N])


  ## Add a prefix to the colnames to identify dataset came from

    setnames(cc_outcomes_data, setdiff(colnames(cc_outcomes_data), "._contact_id_."),
             paste("crit_care", setdiff(colnames(cc_outcomes_data), "._contact_id_."), sep = "_"))


  ## Change col name back

    setnames(cc_outcomes_data, "._contact_id_.", field_name_contact_id)


  ## Return critical care data outcomes - have kept time in days for working out deterioration

    return(cc_outcomes_data)

}


# Drug Outcomes Function -------------------------------------------------


getDrugCountByPatientDate <- function(patients_dates,
                                      demo_file_id,
                                      pt_id_field = "patient_id",
                                      activity_date_field,
                                      field_name_contact_id,
                                      duration_days = 365L) {

  # patients_dates:         data table of patient_ids and activity dates for which a
  #                         count of different drugs is sought within the previous
  #                         duration_days
  # demo_file_id :          String of the demo file that was used to create cohorts
  # pt_id_field:            patient_id field name in patients_dates
  # activity_date_field:    activity_date field name in patients_dates
  # field_name_contact_id:  Name of field that contains contact id
  # duration_days:          Number of days prior to activity_date over which to
  #                         search for drugs.

  # patients_dates <- nhs111_data
  # pt_id_field <- "patient_id"
  # activity_date_field <- "call_datetime"
  # field_name_contact_id <- "record_ID"

  patients_dates_unique <- copy(patients_dates)
  setnames(patients_dates_unique, c(pt_id_field, activity_date_field, field_name_contact_id), c("._patient_id_.", "._activity_date_.", "._contact_id_."))
  patients_dates_unique <- unique(patients_dates_unique[, .(._patient_id_., ._activity_date_., ._contact_id_.)])


  ## Check contact id is present for all rows and each one only appears once

  stopifnot(patients_dates_unique[is.na(._contact_id_.), .N] == 0)
  stopifnot(patients_dates_unique[, .N, by = ._contact_id_.][N > 1, .N] == 0)


  ## Change datetime to date otherwise -365 is within ~6 minutes

  stopifnot("POSIXct" %in% patients_dates_unique[, class(._activity_date_.)] | "Date" %in% patients_dates_unique[, class(._activity_date_.)])

  patients_dates_unique[, ._activity_date_. := as.Date(._activity_date_.)]


  # Read in GDPPR data and med look-up data, and merge (GDPPR data is original as oppose to comorb marked up like used in GDPPR outcomes function)

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

  # Count distinct drugs within in relevant period for each (contact, activity date)-tuple
  drugs_by_patient_date <- drugs_by_patient_date[DATE < ._activity_date_. & DATE >= ._activity_date_. - duration_days,
                                                 .N,
                                                 by = .(._contact_id_., ._activity_date_., bnf_purpose_chemical)][,
                                                                                                                  .(drug_count = .N),
                                                                                                                  by = .(._contact_id_., ._activity_date_.)]

  # Merge above into all (contact, activity date)-tuples
  drugs_by_all_patient_date <- merge(patients_dates_unique[, .(._contact_id_., ._activity_date_.)],
                                     drugs_by_patient_date,
                                     by = c("._contact_id_.", "._activity_date_."),
                                     all.x = TRUE)

  # For those tuples with no drug_count from merge above, set drug count to 0.
  drugs_by_all_patient_date[is.na(drug_count), drug_count := 0L]

  # Check have same number of rows as original cohort
  stopifnot(drugs_by_all_patient_date[, .N] == patients_dates[, .N])

  # Remove activity date (will be doubled up in final cohort), as is now done on contact id not patient id
  drugs_by_all_patient_date[, ._activity_date_. := NULL]

  # Replace original field names
  setnames(drugs_by_all_patient_date, "._contact_id_.", field_name_contact_id)

  # Return
  return(drugs_by_all_patient_date)
}


# Inpatient Outcomes Function ---------------------------------------------

# Function that gets the outcome data for a cohort based on incident time

getInpatientOutcomes <- function(cohort, apc_data, field_name_patient_id, field_name_date_field, field_name_contact_id) {

  # cohort <- cad_data
  # #apc_data <- apc_data
  # field_name_patient_id <- "patient_id"
  # field_name_date_field <- "call_datetime"
  # field_name_contact_id <- "amb_incident_id"


  ## Read in ICD-10 COVID codes taken from - https://hscic.kahootz.com/connect.ti/t_c_home/view?objectID=19099248

  icd_10_covid_codes <- data.table(read_excel("D:/reference_data/ICD_10_COVID_codes_2020-11-25.xlsx",
                                              sheet = "ICD_10_COVID_codes",
                                              col_names = TRUE,
                                              col_types = "text",
                                              trim_ws = TRUE))


  ## Create copy of cohort and change names

    cohort_copy <- copy(cohort)

    setnames(cohort_copy, c(field_name_patient_id, field_name_date_field, field_name_contact_id),
             c("._patient_id_.", "._date_field_.", "._contact_id_."))


  ## Check that the contact id is never empty and that it appears only once each in cohort

    stopifnot(cohort_copy[is.na(._contact_id_.), .N] == 0)
    stopifnot(cohort_copy[, .N, by = ._contact_id_.][N > 1, .N] == 0)


  ## Change datetime to date as don't have time in death date

    stopifnot("POSIXct" %in% cohort_copy[, class(._date_field_.)] | "Date" %in% cohort_copy[, class(._date_field_.)])

    cohort_copy[, ._date_field_. := as.Date(._date_field_.)]



  ## Check that no spell id has more than one patient related to it

    stopifnot(apc_data[, .N, by = .(patient_id, SUSSPELLID)][, .N, by = SUSSPELLID][N > 1L] == 0)


  ## Check to see if any SUSSPELLIDs have more than one ADMIDATE

    stopifnot(apc_data[, .N, by = .(SUSSPELLID, ADMIDATE)][, .N, by = SUSSPELLID][N > 1L] == 0)


  ## Keep only cols wanted and melt over spell id

    apc_code_fields <- paste("DIAG_4", formatC(1:20, width = 2, format = "d", flag = "0"), sep = "_")
    apc_wanted_cols <- c("ADMIDATE", "patient_id", "SUSSPELLID", apc_code_fields)


    melted_apc_data <- melt(apc_data[, .SD, .SDcols = apc_wanted_cols],
                            measure.vars = apc_code_fields, variable.name = "outcome_numb", value.name = "diag_code")


    melted_apc_min_date_diagnosed <- melted_apc_data[, ':=' (ADMIDATE_MIN = min(ADMIDATE),
                                                             inpatient_covid_diagnosis = diag_code %in% icd_10_covid_codes$ICD_10_code),
                                                     by = SUSSPELLID][!is.na(diag_code)]


  ## Limit this to patient_id/admission date pairs, with if there was any covid diagnosis on that day for that person

    in_patient_covid_diag <- melted_apc_min_date_diagnosed[, .(inpatient_covid_diagnosis = any(inpatient_covid_diagnosis)), by = .(ADMIDATE_MIN, patient_id)]


  ## Merge in incident call dates based on patient

    merged_apc_diag_call_date <- merge(cohort_copy[, .(._patient_id_., ._date_field_., ._contact_id_.)],
                                       in_patient_covid_diag,
                                       by.x = "._patient_id_.",
                                       by.y = "patient_id",
                                       all.x = TRUE)


  ## Drop records where admission date is prior to contact time

    merged_apc_diag_call_date <- merged_apc_diag_call_date[ADMIDATE_MIN >= ._date_field_.]


    ## Create field of time between cohort call and ED arrival

    merged_apc_diag_call_date[, call_to_admission_days := lubridate::interval(._date_field_., ADMIDATE_MIN)/lubridate::ddays(1)]


    ## Find for each contact_id if there was a covid diagnosis (7 and 30 days), as ECDS data, all have ED admission, and do attendances counts

    inpatient_outcomes_data <- merged_apc_diag_call_date[, .(covid_diagnosis_7_days = any(inpatient_covid_diagnosis & call_to_admission_days < 7),
                                                             covid_diagnosis_30_days = any(inpatient_covid_diagnosis & call_to_admission_days < 30),
                                                             number_of_attendences_in_7_days = sum(call_to_admission_days < 7),
                                                             number_of_attendences_in_30_days = sum(call_to_admission_days < 30)),
                                                         by = ._contact_id_.]


  ## Check have kept one line for each incident

    stopifnot(inpatient_outcomes_data[, .N] == uniqueN(merged_apc_diag_call_date[, ._contact_id_.]))


  ## Merge back into full cohort

    full_cohort_inpatient_outcomes_data <- merge(cohort_copy[, .(._contact_id_.)],
                                                 inpatient_outcomes_data,
                                                 by = "._contact_id_.",
                                                 all.x = TRUE)


  ## And change all NAs to FALSE or to 0s

    cols_to_replaces_nas <- c("covid_diagnosis_7_days", "covid_diagnosis_30_days")

    full_cohort_inpatient_outcomes_data[, (cols_to_replaces_nas) := lapply(.SD, function(x) replace(x, is.na(x), FALSE)), .SDcols = cols_to_replaces_nas]
    full_cohort_inpatient_outcomes_data[is.na(number_of_attendences_in_7_days), number_of_attendences_in_7_days := 0L]
    full_cohort_inpatient_outcomes_data[is.na(number_of_attendences_in_30_days), number_of_attendences_in_30_days := 0L]

    stopifnot(full_cohort_inpatient_outcomes_data[, .N] == cohort[, .N])


  ## Add prefix of dataset to the col names

    setnames(full_cohort_inpatient_outcomes_data, setdiff(colnames(full_cohort_inpatient_outcomes_data), "._contact_id_."),
             paste("apc", setdiff(colnames(full_cohort_inpatient_outcomes_data), "._contact_id_."), sep = "_"))


  ## Change col name back

    setnames(full_cohort_inpatient_outcomes_data, "._contact_id_.", field_name_contact_id)


    return(full_cohort_inpatient_outcomes_data)

}


getDerivedAdverseOutcomes <- function(death_outcomes, cc_outcomes, field_name_contact_id)  {

  death_outcomes_copy <- copy(death_outcomes)
  cc_outcomes_copy <- copy(cc_outcomes)

  setnames(death_outcomes_copy, field_name_contact_id, "._contact_id_.")
  setnames(cc_outcomes_copy, field_name_contact_id, "._contact_id_.")


## Merge both adverse outcomes together by record id

  time_to_adverse_outcome <- merge(death_outcomes_copy[, .(._contact_id_., dr_time_from_incident_to_death )],
                                   cc_outcomes_copy[, .(._contact_id_., crit_care_call_to_cc_time )],
                                   by = "._contact_id_.",
                                   all = TRUE)


## Find the smaller of the times to adverse outcome, and add 1 to match up with core-priest meaning, day '1' = day of contact

  time_to_adverse_outcome[, min_days_to_adverse_event := pmin(dr_time_from_incident_to_death, crit_care_call_to_cc_time, na.rm = TRUE) + 1L]

  time_to_adverse_outcome[, time_to_deterioration := cut(as.numeric(min_days_to_adverse_event),
                                                         breaks = c(1, 3, 7, 12, 17, 22, 27, 30, Inf),
                                                         lables = FALSE,
                                                         include.lowest = TRUE,
                                                         ordered_result = TRUE)]

  time_to_adverse_outcome[, time_to_deterioration_lables := cut(as.numeric(min_days_to_adverse_event),
                                                                breaks = c(1, 3, 7, 12, 17, 22, 27, 30, Inf),
                                                                labels = FALSE,
                                                                include.lowest = TRUE,
                                                                ordered_result = TRUE)]


  time_to_adverse_outcome[time_to_deterioration != "(30,Inf]", any_adverse_outcome := !is.na(time_to_deterioration)]
  time_to_adverse_outcome[is.na(any_adverse_outcome), any_adverse_outcome := FALSE]


  time_to_adverse_outcome <- time_to_adverse_outcome[, .(._contact_id_., time_to_deterioration, time_to_deterioration_lables, any_adverse_outcome)]

  setnames(time_to_adverse_outcome, "._contact_id_.", field_name_contact_id)


  return(time_to_adverse_outcome)

}


removeExcludedPatient <- function(final_cohort) {

  ## Read in file with removed patients due to death register conflicts

    excluded_patients <- readRDS(file = paste("data/datasets/cohort", demo_file_id, "excluded_patient_ids.rds", sep = "_"))


  ## Remove excluded patients

    final_cohort_excluded_removed <- final_cohort[!(patient_id %in% excluded_patients)]

    return(final_cohort_excluded_removed)
}




# Function that uses all outcomes functions and return one table ----------

getOutcomesData <- function(cohort,
                            demo_file_id,
                            field_name_patient_id,
                            field_name_date_field,
                            field_name_contact_id,
                            duration_days) {

# cohort <- nhs111_data
# field_name_patient_id <- "patient_id"
# field_name_date_field <- "call_datetime"
# field_name_contact_id <- "record_ID"
# duration_days = 365L


## Read in all reference data needed

## Read in single source of truth

  #demo_ssot <- readRDS(file = paste("D:/source_data/ssot/cohort", demo_file_id, "demo_ssot.rds", sep = "_"))


## Read in death register data

  #dr_data <- readRDS(file = paste("data/datasets/cohort", demo_file_id, "dr_data.rds", sep = "_"))


## Read in critical care data

  #cc_data <- readRDS(file = paste("data/datasets/cohort", demo_file_id, "cc_data.rds", sep = "_"))


## Read in inpatient data

  apc_data <- readRDS(file = paste("data/datasets/cohort", demo_file_id, "apc_data.rds", sep = "_"))


## Read in pre-processed for outcomes ED data

  ecds_data_covid_diag <- readRDS(file = paste("data/datasets/cohort", demo_file_id, "ecds_data_covid_diag.rds", sep = "_"))


## Read in pre-procesed for outcomes GDPPR data
## Original code commented out to make checks/look at something
  # gdppr_comorb_data <- readRDS(file = paste("data/datasets/cohort", demo_file_id, "gdppr_comorb_records.rds", sep = "_"))

  gdppr_comorb_data <- readRDS(file = paste("data/datasets/cohort", demo_file_id, "gdppr_comorb_records_post_change.rds", sep = "_"))


## Run all functions to get outcome data

  #ssot_outcomes <- getSingleSourceData(cohort, demo_ssot, field_name_patient_id, field_name_date_field, field_name_contact_id)

  #death_outcomes <- getDeathRecordsOutcomes(cohort, dr_data, field_name_patient_id, field_name_date_field, field_name_contact_id)

  #cc_outcomes <- getCriticalCareOutcomes(cohort, cc_data, field_name_patient_id, field_name_date_field, field_name_contact_id)

  #derived_adverse_outcomes <- getDerivedAdverseOutcomes(death_outcomes, cc_outcomes, field_name_contact_id)

  inpatient_outcomes <- getInpatientOutcomes(cohort, apc_data, field_name_patient_id, field_name_date_field, field_name_contact_id)

  ed_outcomes <- getEDOutcomeData(cohort, ecds_data_covid_diag, field_name_patient_id, field_name_date_field, field_name_contact_id)

  gdppr_outcomes <- getGDPPROutcome(cohort, gdppr_comorb_data, field_name_patient_id, field_name_date_field, field_name_contact_id)

  drug_counts <- getDrugCountByPatientDate(cohort,
                                           demo_file_id = demo_file_id,
                                           pt_id_field = field_name_patient_id,
                                           activity_date_field = field_name_date_field,
                                           field_name_contact_id = field_name_contact_id,
                                           duration_days = duration_days)


## Merge together all outcomes data using contact id provided

  list_tables_to_merge <- list(#ssot_outcomes,
                               #death_outcomes,
                               #cc_outcomes,
                               #derived_adverse_outcomes,
                               inpatient_outcomes,
                               ed_outcomes,
                               gdppr_outcomes,
                               drug_counts)


  all_outcomes_table <- Reduce(function(x, y) merge(x = x, y = y, by = field_name_contact_id, all.x = TRUE), list_tables_to_merge)


### Create extra ED or Inpatient fields

  all_outcomes_table[, ed_or_inpatient_covid_diag_7_days := any(ed_covid_diagnosis_7_days, apc_covid_diagnosis_7_days), by = field_name_contact_id]
  all_outcomes_table[, ed_or_inpatient_covid_diag_30_days := any(ed_covid_diagnosis_30_days, apc_covid_diagnosis_30_days), by = field_name_contact_id]

  return(all_outcomes_table)

}


# ## Remove time to death and critical care fields if stated in the function arguments
#
#   if(retain_adverse_times == FALSE)  {
#
#     all_outcomes_table[, c("dr_time_from_incident_to_death", "crit_care_call_to_cc_time") := NULL]
#
#   }
#
#   return(all_outcomes_table)
#
# }



