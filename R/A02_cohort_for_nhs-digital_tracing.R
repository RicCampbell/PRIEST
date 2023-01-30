library(data.table)
library(readxl)
source("r/cleaning_fns_etl.r")


# Cohort for NHS Digital tracing ------------------------------------------


# *****  ENSURE THIS IS CORRECT FILE VERSION  *****
  warning("*******  Is this the correct file?! ****")
  priest_patient_identifiers <- readRDS("D:/source_data/identifiers/priest_patient_identifiers-2020-09-15 151647.rds")


## Remove fields that were used to make as_at_dates and other fields not required for NHS D

  ident_cols_to_keep <- c("record_ID", "data_as_at_date", "nhs_number", "date_of_birth", "sex", "postcode", "first_name", "last_name")

  priest_patient_identifiers_nhsd <- priest_patient_identifiers[, ..ident_cols_to_keep]

  rm(priest_patient_identifiers)


# Identifiable data validation --------------------------------------------

## Check have record number and check unique

  stopifnot(priest_patient_identifiers_nhsd[is.na(record_ID), .N] == 0)
  stopifnot(priest_patient_identifiers_nhsd[, .N, by = record_ID][N > 1, .N] == 0)


## Check have date as at date

  stopifnot(priest_patient_identifiers_nhsd[is.na(data_as_at_date), .N] == 0)


## Only retain data as at dates that are valid

  stopifnot(priest_patient_identifiers_nhsd[data_as_at_date < as.Date("2020-01-31")
                                            | data_as_at_date >= as.Date("2020-07-01"),
                                            .N] == 1)


  priest_patient_identifiers_nhsd <- priest_patient_identifiers_nhsd[!(data_as_at_date < as.Date("2020-01-31")
                                                                       | data_as_at_date >= as.Date("2020-07-01"))]



## Clean date of birth

  priest_patient_identifiers_nhsd[date_of_birth < "1900-01-31", date_of_birth := NA]

  priest_patient_identifiers_nhsd[difftime(data_as_at_date, date_of_birth) < 0, date_of_birth := NA]


## Re-code sex field to fit NHS D wants

  sex_mapping <- data.table(read_excel("D:/reference_data/Field Mapping and Standardisation.xlsx",
                                       sheet = "sex_standardisation",
                                       col_names = TRUE,
                                       col_types = "text",
                                       trim_ws = TRUE))


  priest_patient_identifiers_nhsd[, sex := sex_mapping$destination_code[match(sex, sex_mapping$source_code)]]
  priest_patient_identifiers_nhsd[!(sex %in% sex_mapping$destination_code), sex := NA]


## Clean first names and surnames, removes punctuation, double spacing to single space, leading/trailing whitespace removed

  priest_patient_identifiers_nhsd[is.na(first_name), first_name := ""]

  priest_patient_identifiers_nhsd[is.na(last_name), last_name := ""]

  priest_patient_identifiers_nhsd[, name := paste(first_name, last_name)]

  priest_patient_identifiers_nhsd[, c("first_name", "last_name") := NULL]

  ident_full_names <- fn_splitForenamesSurnames(priest_patient_identifiers_nhsd$name)

  priest_patient_identifiers_nhsd <- cbind(priest_patient_identifiers_nhsd, ident_full_names)

  priest_patient_identifiers_nhsd[, name := NULL]

  priest_patient_identifiers_nhsd[, first_name := fn_cleanNames(forenames)]

  priest_patient_identifiers_nhsd[grepl(" ", first_name, fixed = TRUE), first_name := substr(first_name, 1, regexpr(" ", first_name, fixed = TRUE) - 1)]

  priest_patient_identifiers_nhsd[, last_name := fn_cleanNames(surname)]

  priest_patient_identifiers_nhsd[grepl(" ", last_name, fixed = TRUE), last_name := substr(last_name, regexpr(" [^ ]*$", last_name) + 1, nchar(last_name))]

  priest_patient_identifiers_nhsd[, c("surname", "forenames") := NULL]

  setnames(priest_patient_identifiers_nhsd, c("first_name", "last_name"), c("forename", "surname"))


## NHS Number have all been checked, but need cleaning, and remove original field - should probably remove invalid ones

  priest_patient_identifiers_nhsd[, nhs_number := fn_validateNHSNumber(nhs_number)]


## Clean postcodes

  priest_patient_identifiers_nhsd[, postcode := fn_cleanPostcode(postcode)]


## For records with valid (nhs_number, sex, and date of birth) set data_as_at_date to maximum data_as_at_date

  priest_patient_identifiers_nhsd[!is.na(nhs_number) & !is.na(date_of_birth) & sex %in% as.character(1:2),
                                  data_as_at_date := max(data_as_at_date), by = .(nhs_number, sex, date_of_birth)]


## For records with valid (nhs_number, sex, and date of birth) only keep these fields, for rest keep all other fields

  priest_patient_identifiers_nhsd_minimised <- rbind(priest_patient_identifiers_nhsd[!is.na(nhs_number) & !is.na(sex) & !is.na(date_of_birth) & sex %in% as.character(1:2),
                                                                                     .(record_ID, data_as_at_date, nhs_number, date_of_birth, sex)],
                                                     priest_patient_identifiers_nhsd[!(!is.na(nhs_number) & !is.na(sex) & !is.na(date_of_birth) & sex %in% as.character(1:2))],
                                                     fill = TRUE)

  stopifnot(priest_patient_identifiers_nhsd_minimised[, .N] == priest_patient_identifiers_nhsd[, .N])


## Building table for NHS D with data_as_at_date

# priest_study_ids <- unique(priest_patient_identifiers_nhsd_minimised[, .(data_as_at_date, nhs_number, date_of_birth, sex, postcode, forename, surname)])
#
# priest_study_ids[, study_ID := as.integer(paste0("9",formatC(1:.N, width = nchar(.N), format = "d", flag = "0")))]
#
# priest_study_id_lookup <- merge(priest_patient_identifiers_nhsd_minimised,
#                                 priest_study_ids,
#                                 by = c("data_as_at_date", "nhs_number", "date_of_birth", "sex", "postcode", "forename", "surname"),
#                                 all.x = TRUE)[, c("data_as_at_date", "nhs_number", "date_of_birth", "sex", "postcode", "forename", "surname") := NULL]
#
# stopifnot(priest_study_id_lookup[, .N] == priest_patient_identifiers_nhsd_minimised[, .N])
#
# stopifnot(all(priest_study_id_lookup$record_ID %in% priest_patient_identifiers_nhsd$record_ID))
#
# stopifnot(all(priest_patient_identifiers_nhsd$record_ID %in% priest_study_id_lookup$record_ID))




## Building table for NHS D without data_as_at_date

  priest_study_ids_no_as_at_date <- unique(priest_patient_identifiers_nhsd_minimised[, .(nhs_number, date_of_birth, sex, postcode, forename, surname)])

  priest_study_ids_no_as_at_date[, study_ID := as.integer(paste0("8",formatC(1:.N, width = nchar(.N), format = "d", flag = "0")))]

  priest_study_id_lookup_no_as_at_date <- merge(priest_patient_identifiers_nhsd_minimised,
                                                priest_study_ids_no_as_at_date,
                                                by = c("nhs_number", "date_of_birth", "sex", "postcode", "forename", "surname"),
                                                all.x = TRUE)[, c("data_as_at_date", "nhs_number", "date_of_birth", "sex", "postcode", "forename", "surname") := NULL]

  stopifnot(priest_study_id_lookup_no_as_at_date[, .N] == priest_patient_identifiers_nhsd_minimised[, .N])

  stopifnot(all(priest_study_id_lookup_no_as_at_date$record_ID %in% priest_patient_identifiers_nhsd$record_ID))

  stopifnot(all(priest_patient_identifiers_nhsd$record_ID %in% priest_study_id_lookup_no_as_at_date$record_ID))


## Write out study ID lookup and NHS D table

  save_time <- gsub(":", "", Sys.time(), fixed = TRUE)

  saveRDS(priest_study_id_lookup_no_as_at_date, paste0("data/linkage/priest_study_id_lookup_no_as_at_date-", save_time, ".rds"))

  fwrite(priest_study_ids_no_as_at_date, file =  paste0("D:/source_data/identifiers/priest_study_ids_no_as_at_date-", save_time, ".csv"))


# Formatting for NHS Digital -----------------------------------------------

    priest_study_ids_no_as_at_date_nhs_digital <- copy(priest_study_ids_no_as_at_date)

    stopifnot(priest_study_ids_no_as_at_date_nhs_digital[!is.na(surname), max(nchar(surname))] <= 40)
    stopifnot(priest_study_ids_no_as_at_date_nhs_digital[!is.na(forename), max(nchar(forename))] <= 40)

    priest_study_ids_no_as_at_date_nhs_digital[, ':=' (date_of_birth = format(date_of_birth, "%Y%m%d"))]

    nhs_d_cohort_submission_headers <- unlist(fread("D:/reference_data/CohortSubmission.csv", header = FALSE))

    nhs_d_cohort_submission_header_mapping <- data.table(read_excel("D:/reference_data/nhs_d_cohort_submission_header_mapping.xlsx",
                                                                    sheet = "CohortSubmission",
                                                                    col_names = TRUE,
                                                                    col_types = "text",
                                                                    trim_ws = TRUE))

    setnames(priest_study_ids_no_as_at_date_nhs_digital,
             nhs_d_cohort_submission_header_mapping[!is.na(source), source],
             nhs_d_cohort_submission_header_mapping[!is.na(source), destination])


  priest_study_ids_no_as_at_date_nhs_digital[, (nhs_d_cohort_submission_header_mapping[is.na(source), destination]) := as.character(NA)]

  setcolorder(priest_study_ids_no_as_at_date_nhs_digital, nhs_d_cohort_submission_headers)

  priest_study_ids_no_as_at_date_nhs_digital[, Status := "ADD"]

  stopifnot(colnames(priest_study_ids_no_as_at_date_nhs_digital) == nhs_d_cohort_submission_headers)

  fwrite(priest_study_ids_no_as_at_date_nhs_digital, file =  paste0("D:/source_data/identifiers/NIC-377644-X9J4P_TS.csv"))


# Reporting stats ---------------------------------------------------------

# # Total records
  priest_patient_identifiers_nhsd[, .N]

# Total records with valid NHS Number
  priest_patient_identifiers_nhsd[!is.na(nhs_number), .N]

# Total records with valid NHS Number, DOB, Sex
  priest_patient_identifiers_nhsd[!is.na(nhs_number) & !is.na(sex) & !is.na(date_of_birth), .N]

# Total records with valid NHS Number, DOB, Sex either male/female (traceable)
  priest_patient_identifiers_nhsd[!is.na(nhs_number) & !is.na(sex) & !is.na(date_of_birth) & sex %in% as.character(1:2), .N]

# Total records *without* valid NHS Number, but with valid DOB, Sex either male/female & (postcode or names)  (traceable)
  priest_patient_identifiers_nhsd[is.na(nhs_number) & !is.na(sex) & !is.na(date_of_birth) & ((!is.na(forename) & !is.na(surname))  | !is.na(postcode)) & sex %in% as.character(1:2), .N]

# Total records (probably) not traceable: missing DOB or sex not male/female or (no: names / postcode / NHS Number)
  priest_patient_identifiers_nhsd[is.na(date_of_birth) | !(sex %in% as.character(1:2)) | !((!is.na(forename) & !is.na(surname))  | !is.na(postcode) | !is.na(nhs_number)), .N]

# Data quality issue: Records with valid NHS Number, DOB, Sex either male/female, NHS Numbers with inconsistent recording of DOB and/or sex
  priest_patient_identifiers_nhsd[!is.na(nhs_number) &  sex %in% as.character(1:2) & !is.na(date_of_birth), .N, by = .(nhs_number, sex, date_of_birth)][, .N, by = .(nhs_number)][N > 1, .N]
