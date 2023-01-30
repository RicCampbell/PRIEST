library(data.table)
library(readxl)


## Read in core PRIEST data

  core_priest <- fread("D:/source_data/core_priest/master_labelled.csv")
  core_priest_ids <- fread("D:/source_data/core_priest/identifiable_data_2020_08_07_11_21.csv")
  core_priest_withdrawn <- fread("D:/source_data/core_priest/PRIEST_withdrawals.csv", colClasses = "character")
  core_priest_incorrect_nhs_number <- fread("D:/source_data/core_priest/incorrect_screening.csv", colClasses = "character")

## Change col names to R acceptable names

  setnames(core_priest, make.names(colnames(core_priest), unique = TRUE))
  setnames(core_priest_ids, make.names(colnames(core_priest_ids), unique = TRUE))
  setnames(core_priest_withdrawn, make.names(colnames(core_priest_withdrawn), unique = TRUE))
  setnames(core_priest_incorrect_nhs_number, make.names(colnames(core_priest_incorrect_nhs_number), unique = TRUE))


## Removes incorrect core-PRIEST record (entered with incorrect NHS number).
## A second (more complete) record already exists for the corrected NHS Number so we simply remove the "incorrect" record.

  stopifnot(core_priest[gsub("[^0-9]+", "", screening) %in% gsub("[^0-9]+", "", core_priest_incorrect_nhs_number$incorrect_screening), .N] == 1)
  core_priest <- core_priest[!(gsub("[^0-9]+", "", screening) %in% gsub("[^0-9]+", "", core_priest_incorrect_nhs_number$incorrect_screening))]


## Remove non-English hospitals

  core_priest_english <- core_priest[!(site %in% c("Craigavon", "Edinburgh", "Royal Gwent", "Nevill Hall"))]


## Check that withdrawn participants have been removed

  stopifnot(core_priest_english[trimws(screening) %in% trimws(core_priest_withdrawn$screening), .N] == 0)


## Read in data from files provided by YAS

  epr <- data.table(readRDS("D:/source_data/yas/epr/epr_final_df.rds"))
  epr_cad <- data.table(readRDS("D:/source_data/yas/epr/epr_cad_cases_final.rds"))
  nonepr_cad <- data.table(readRDS("D:/source_data/yas/cad/card36_df_final.rds"))
  nhs111 <- data.table(readRDS("D:/source_data/yas/nhs111/111_post_optout_final.rds"))

  setnames(epr, make.names(colnames(epr), unique = TRUE))
  setnames(epr_cad, make.names(colnames(epr_cad), unique = TRUE))
  setnames(nonepr_cad, make.names(colnames(nonepr_cad), unique = TRUE))
  setnames(nhs111, make.names(colnames(nhs111), unique = TRUE))


## Read in standardised field name lists

  core_priest_mapping <- data.table(read_excel("D:/reference_data/Field Mapping and Standardisation.xlsx",
                                  sheet = "core_PRIEST",
                                  col_names = TRUE,
                                  col_types = "text",
                                  trim_ws = TRUE))

  epr_provided_mapping <- data.table(read_excel("D:/reference_data/Field Mapping and Standardisation.xlsx",
                                  sheet = "ePR",
                                  col_names = TRUE,
                                  col_types = "text",
                                  trim_ws = TRUE))

  epr_cad_provided_mapping <- data.table(read_excel("D:/reference_data/Field Mapping and Standardisation.xlsx",
                                  sheet = "CAD",
                                  col_names = TRUE,
                                  col_types = "text",
                                  trim_ws = TRUE))

  nhs111_provided_mapping <- data.table(read_excel("D:/reference_data/Field Mapping and Standardisation.xlsx",
                                  sheet = "NHS111",
                                  col_names = TRUE,
                                  col_types = "text",
                                  trim_ws = TRUE))

  priest_id_mapping <- data.table(read_excel("D:/reference_data/Field Mapping and Standardisation.xlsx",
                                    sheet = "PRIEST_ids",
                                    col_names = TRUE,
                                    col_types = "text",
                                    trim_ws = TRUE))


## Check that there are no duplicates in the destination field

  stopifnot(all(duplicated(core_priest_mapping$destination_field) == FALSE))
  stopifnot(all(duplicated(epr_provided_mapping$destination_field) == FALSE))
  stopifnot(all(duplicated(nhs111_provided_mapping$destination_field) == FALSE))
  stopifnot(all(duplicated(nhs111_provided_mapping$destination_field) == FALSE))
  stopifnot(all(duplicated(priest_id_mapping$destination_field) == FALSE))


## Re-code all fields to standardise them across datasets

  setnames(core_priest_english, core_priest_mapping$source_field, core_priest_mapping$destination_field)

  setnames(epr, epr_provided_mapping$source_field, epr_provided_mapping$destination_field)

  setnames(epr_cad, epr_cad_provided_mapping$source_field, epr_cad_provided_mapping$destination_field)

  setnames(nonepr_cad, epr_cad_provided_mapping$source_field,  epr_cad_provided_mapping$destination_field)

  setnames(nhs111, nhs111_provided_mapping$source_field, nhs111_provided_mapping$destination_field)

  setnames(core_priest_ids, priest_id_mapping$source_field, priest_id_mapping$destination_field)


## Create Record ID for all rows in each table

  max_width <- nchar(max(nrow(epr), nrow(epr_cad), nrow(nonepr_cad), nrow(nhs111), nrow(core_priest_english)))

  epr[, record_ID := as.integer(paste0("1", formatC(1:.N, width = max_width, format = "d", flag = "0")))]

  epr_cad[, record_ID := as.integer(paste0("2", formatC(1:.N, width = max_width, format = "d", flag = "0")))]

  nonepr_cad[, record_ID := as.integer(paste0("3", formatC(1:.N, width = max_width, format = "d", flag = "0")))]

  nhs111[, record_ID := as.integer(paste0("4", formatC(1:.N, width = max_width, format = "d", flag = "0")))]

  core_priest_english[, record_ID := as.integer(paste0("5", formatC(1:.N, width = max_width, format = "d", flag = "0")))]


## Merge core priest data together from the two data sets

  stopifnot(all(core_priest_english$nhs_number %in% core_priest_ids$nhs_number) &
              core_priest_ids[, .N, by = nhs_number][N > 1, .N] == 0)


  core_priest_english_with_ident <- merge(core_priest_english, core_priest_ids[, .(date_of_birth, individual_id, nhs_number_from_id = nhs_number)],
                                     all.x = TRUE, by = "individual_id")


  stopifnot(core_priest_english_with_ident[nhs_number != nhs_number_from_id, .N] == 0)

  core_priest_english_with_ident[, nhs_number_from_id := NULL]


## Create new table with just identifiable fields for each table

  ident_cols <- data.table(read_excel("D:/reference_data/Field Mapping and Standardisation.xlsx",
                                  sheet = "Ident Fields",
                                  col_names = TRUE,
                                  col_types = "text",
                                  trim_ws = TRUE))


  ident_epr_cols <- c(ident_cols[dataset == "ePR", destination_field], "record_ID", "incident_datetime", "sex")
  ident_cad_cols <- c(ident_cols[dataset == "CAD", destination_field], "record_ID", "call_datetime", "sex")
  ident_nhs111_cols <- c(ident_cols[dataset == "NHS111", destination_field], "record_ID", "call_datetime", "sex")
  ident_core_priest_cols <- c(ident_cols[dataset == "core_priest", destination_field], "record_ID", "attendance_date", "sex")


  ident_epr <- epr[, ..ident_epr_cols]
  ident_epr_cad <- epr_cad[, ..ident_cad_cols]
  ident_nonepr_cad <- nonepr_cad[, ..ident_cad_cols]
  ident_nhs111 <- nhs111[, ..ident_nhs111_cols]
  ident_core_priest <- core_priest_english_with_ident[, ..ident_core_priest_cols]


## Remove identifiable cols from original tables

  epr[, ident_cols[dataset == "ePR", destination_field] := NULL]
  epr_cad[, ident_cols[dataset == "CAD", destination_field] := NULL]
  nonepr_cad[, ident_cols[dataset == "CAD", destination_field] := NULL]
  nhs111[, ident_cols[dataset == "NHS111", destination_field] := NULL]
  core_priest_english_with_ident[, ident_cols[dataset == "core_priest", destination_field] := NULL]


## Cleaning data that will go to NHS Digital (identifiable fields)

  ## epr - date_of_birth, yyyy-mm-dd, UTC, but not time   - dttm
  ##       - pcrdob, yyyy-mm-dd - date
  ## CAD - dateofbirth, yyy-mm-dd - date
  ## NHS111 - dob, yyyy-mm-dd, UTC, but no time - dttm


## Convert any date times to just dates (there are no times in data, so no daylight savings to think about)

    ident_epr[, ':=' (date_of_birth = as.Date(date_of_birth),
                      data_as_at_date = as.Date(incident_datetime))]

    ident_epr_cad[, data_as_at_date := as.Date(call_datetime)]

    ident_nonepr_cad[, data_as_at_date := as.Date(call_datetime)]

    ident_nhs111[, ':=' (date_of_birth = as.Date(date_of_birth),
                         data_as_at_date = as.Date(call_datetime))]

    ident_core_priest[, ':=' (date_of_birth = as.Date(date_of_birth),
                              data_as_at_date = as.Date(attendance_date))]


## Bind all identifiable tables together

    priest_patient_identifiers <- rbind(ident_epr, ident_epr_cad, ident_nonepr_cad, ident_nhs111, ident_core_priest, fill = TRUE)


    stopifnot(all(c(epr$record_ID,
                    epr_cad$record_ID,
                    nonepr_cad$record_ID,
                    nhs111$record_ID,
                    core_priest_english_with_ident$record_ID) %in% priest_patient_identifiers$record_ID))

    stopifnot(all(priest_patient_identifiers$record_ID %in% c(epr$record_ID,
                                                              epr_cad$record_ID,
                                                              nonepr_cad$record_ID,
                                                              nhs111$record_ID,
                                                              core_priest_english_with_ident$record_ID)))

    stopifnot(priest_patient_identifiers[is.na(record_ID), .N] == 0)

    stopifnot(all(duplicated(c(epr$record_ID,
                           epr_cad$record_ID,
                           nonepr_cad$record_ID,
                           nhs111$record_ID,
                           core_priest_english_with_ident$record_ID))) == FALSE)


## Ensure no withdrawn pts in any PRIEST data (core or YAS supplied).

    stopifnot(priest_patient_identifiers[trimws(nhs_number) %in% trimws(core_priest_withdrawn$screening), .N] == 0)


## Save each non-ident table as an R object

    save_time <- gsub(":", "", Sys.time(), fixed = TRUE)

    saveRDS(epr, paste0("data/datasets/epr-", save_time, ".rds"))
    saveRDS(epr_cad, paste0("data/datasets/epr_cad-", save_time, ".rds"))
    saveRDS(nonepr_cad, paste0("data/datasets/nonepr_cad-", save_time, ".rds"))
    saveRDS(nhs111, paste0("data/datasets/nhs111-", save_time, ".rds"))
    saveRDS(core_priest_english_with_ident, paste0("data/datasets/core_priest-", save_time, ".rds"))


    saveRDS(priest_patient_identifiers, paste0("D:/source_data/identifiers/priest_patient_identifiers-", save_time, ".rds"))

