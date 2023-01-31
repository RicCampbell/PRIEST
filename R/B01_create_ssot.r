source("R/cleaning_fns_etl.r")

# Process demographics data -----------------------------------------------

demo_file_id <- "FILE0115974"
demo_data <- fread(paste0("D:/source_data/nhs_digital/demo/", demo_file_id, "_NIC377644_Demographics__APPROVED_53999_01010001_0.txt"), colClasses = "character")

setnames(demo_data, tolower(colnames(demo_data)))

demo_cols_to_retain <- c("study_id", "nhs_no", "gender", "postcode", "dob")
demo_cols_to_delete <- colnames(demo_data)[!(colnames(demo_data) %in% demo_cols_to_retain)]

demo_data[, colnames(demo_data)[!(colnames(demo_data) %in% demo_cols_to_retain)] := NULL]
demo_data[, ':=' (study_id = as.integer(study_id),
                  postcode = toupper(postcode),
                  dob = as.Date(lubridate::fast_strptime(paste(substr(dob, 1, 9), "12:00:00"), format = "%d%b%Y %H:%M:%S", tz = "Europe/London")))]

demo_data[, postcode := fn_removeBlanks(postcode)]

# Ensure NHS Digital Demo data has only one DOB, postcode, gender triplet per NHS Number
stopifnot(demo_data[, .N, by = .(nhs_no, gender, postcode, dob)][, .N, by = nhs_no][N > 1, .N] == 0)
stopifnot(demo_data[is.na(nhs_no), .N] == 0)


# Ensure we respect core-PRIEST withdrawals -------------------------------

core_priest_withdrawn <- unique(fread("D:/source_data/core_priest/PRIEST_withdrawals.csv", colClasses = "character")[, (screening)])
stopifnot(typeof(core_priest_withdrawn) == "character")

demo_data <- demo_data[!(nhs_no %in% core_priest_withdrawn)]


# Create NHS Num to patient_id (1-to-1) lookup ----------------------------
## For GDPPR / ECDS / NHSE data (and HES data)
## Remains identifiable

nhs_num_to_patient_id_lu <- copy(demo_data[, .(nhs_no = unique(nhs_no))])
nhs_num_to_patient_id_lu[, patient_id := as.integer(paste0("7", formatC(sample.int(nrow(nhs_num_to_patient_id_lu)),
                                                        width = nchar(nrow(nhs_num_to_patient_id_lu)),
                                                        format = "d",
                                                        flag = "0")))]

stopifnot(nhs_num_to_patient_id_lu[, .N, by = nhs_no][N > 1, .N] == 0 &
            nhs_num_to_patient_id_lu[, .N, by = patient_id][N > 1, .N] == 0)


# Create study_id to patient_id (many-to-1) lookup ------------------------
## For HES data
demo_data <- merge(demo_data,
                   nhs_num_to_patient_id_lu,
                   by = "nhs_no",
                   all.x = TRUE)

study_id_to_pt_id_lu <- copy(demo_data[, .(study_id, patient_id)])

stopifnot(study_id_to_pt_id_lu[, .N, by = study_id][N > 1, .N] == 0)


# Create record_id to pt_id (many-to-1) lookup ----------------------------
## For YAS / core-PRIEST data

record_id_to_study_id_lu <- readRDS("data/linkage/priest_study_id_lookup_no_as_at_date-2020-09-15 161140.rds")

record_id_to_pt_id_lu <- merge(study_id_to_pt_id_lu,
                               record_id_to_study_id_lu,
                               by.x = "study_id",
                               by.y = "study_ID",
                               all.x = TRUE)[, study_id := NULL]

stopifnot(record_id_to_pt_id_lu[, .N, by = record_ID][N > 1, .N] == 0)


# Create patient SSOT table -----------------------------------------------
## Remains identifiable due to DOBs

demo_data[, study_id := NULL]

demo_ssot <- unique(demo_data)
stopifnot(demo_ssot[, .N, by = patient_id][N > 1, .N] == 0)

# We only require OA
## ensure NHS Digital postcodes always have 1 (and only 1) space between outward and inward portions
stopifnot(demo_ssot[!is.na(postcode) & substr(postcode, nchar(postcode) - 3, nchar(postcode) - 3) == " ", .N] == demo_ssot[!is.na(postcode), .N])

load("data/reference_data/pc_to_oa11_classes.rda")

demo_ssot <- merge(demo_ssot,
                   pc_to_oa11_classes,
                   by.x = "postcode",
                   by.y = "pcds",
                   all.x = TRUE)

demo_ssot[, ':=' (postcode_invalid = (!is.na(postcode) & is.na(usertype)),
                  postcode = NULL)]
demo_ssot[postcode_invalid == FALSE & is.na(usertype), postcode_invalid := NA]


# Ethnicity ---------------------------------------------------------------

snomed_nhs_ethnicity_mappings <- readRDS("data/reference_data/snomed_to_nhs_ethnicity_mappings.rds")
gdppr_data <- fread("D:/source_data/nhs_digital/gdppr/NIC377644_GDPPR_20201204_APPROVED_51666_01010001_0.txt", colClasses = "character")

setnames(gdppr_data, make.names(toupper(colnames(gdppr_data)), unique = TRUE))

gdppr_col_names <- colnames(gdppr_data)
gdppr_data[, (gdppr_col_names) := lapply(.SD, fn_removeBlanks), .SDcols = gdppr_col_names]

gdppr_data <- unique(gdppr_data)
gdppr_data[, record_ID := as.integer(paste0("1", formatC(1:.N, width = nchar(nrow(gdppr_data)), format = "d", flag = "0")))]

# Convert date fields to dates
gdppr_date_cols <- c("REPORTING_PERIOD_END_DATE", "DATE", "RECORD_DATE")
gdppr_data[, (gdppr_date_cols) := lapply(.SD, fn_covertStrToDate), .SDcols = gdppr_date_cols]

# require 1 NHS Number per study_id
stopifnot(gdppr_data[, .N, by = .(NHS_NUMBER, STUDY_ID)][, .N, by = STUDY_ID][N > 1, .N] == 0)

# extract all ethnicity data
gdppr_ethnicity_data <- rbind(merge(gdppr_data[, .(CODE, NHS_NUMBER, REPORTING_PERIOD_END_DATE, RECORD_DATE, DATE, record_ID)],
                                    snomed_nhs_ethnicity_mappings,
                                    by.x = "CODE",
                                    by.y = "conceptid",
                                    all = FALSE)[, CODE := NULL],
                              merge(gdppr_data[, .(ethnicity_code = EHNIC, NHS_NUMBER, REPORTING_PERIOD_END_DATE, RECORD_DATE, DATE, record_ID)],
                                    unique(snomed_nhs_ethnicity_mappings[, .(ethnicity_code, ethnicity_desc, ethnicity_group)]),
                                    by = "ethnicity_code",
                                    all = FALSE))

# Remove unknown and unstated ethnicity
gdppr_ethnicity_data <- gdppr_ethnicity_data[!is.na(ethnicity_code) & ethnicity_code != "Z"]

# order by most recent: date, reporting period, record date (nulls last);
#   all those being equal, order by record ID ascending
setorder(gdppr_ethnicity_data, -DATE, -REPORTING_PERIOD_END_DATE, -RECORD_DATE, record_ID, na.last = TRUE)
gdppr_ethnicity_data[, record_order := 1:.N, by = NHS_NUMBER]

# retain latest stated ethnicity only
latest_gdppr_ethnicity <- gdppr_ethnicity_data[record_order == 1]
latest_gdppr_ethnicity[, c("REPORTING_PERIOD_END_DATE",
                           "RECORD_DATE",
                           "DATE",
                           "record_order",
                           "record_ID") := NULL]

# Merge into SSOT
demo_ssot <- merge(demo_ssot,
                   latest_gdppr_ethnicity,
                   by.x = "nhs_no",
                   by.y = "NHS_NUMBER",
                   all.x = TRUE)[, nhs_no := NULL]



# Core-Priest -------------------------------------------------------------

## Read in core-priest data with identifiable data removed

  ident_partition_date <- "2020-09-15 151647"
  priest_save_date <- "2020-09-15 161140"

  core_priest <- readRDS(paste0("data/datasets/core_priest-", ident_partition_date, ".rds"))

  look_up_study_record_table <- readRDS(paste0("data/linkage/priest_study_id_lookup_no_as_at_date-", priest_save_date, ".rds"))


## Merge core-priest attendance date into SSoT, and therefore marker (although add easy to notice field) that patient was in core-priest

  core_priest <- merge(core_priest[, .(record_ID)],
                       record_id_to_pt_id_lu,
                       by = "record_ID")[, ':=' (core_priest_patient = TRUE,
                                                 record_ID = NULL)]


## Check that patient_id was not linked to 2+ study_IDs

  stopifnot(core_priest[, .N, by = patient_id][N > 1, .N] == 0)


## Merge core priest information into SSoT

  demo_ssot <- merge(demo_ssot,
                     core_priest,
                     by = "patient_id",
                     all.x = TRUE)

  demo_ssot[is.na(core_priest_patient), core_priest_patient := FALSE]

# Save SSOT and lookups ---------------------------------------------------

save_time <- gsub(" ", "-", gsub(":", "", Sys.time(), fixed = TRUE), fixed = TRUE)

## identifiable
saveRDS(nhs_num_to_patient_id_lu, file = paste("D:/source_data/tracing/cohort", demo_file_id, save_time, "nhs_num_to_pt_id_lu.rds", sep = "_"))
saveRDS(demo_ssot, paste("D:/source_data/ssot/cohort", demo_file_id, save_time, "demo_ssot.rds", sep = "_"))

## non-identifiable
saveRDS(study_id_to_pt_id_lu, file = paste("data/linkage/cohort", demo_file_id, save_time, "study_id_to_pt_id_lu.rds", sep = "_"))
saveRDS(record_id_to_pt_id_lu, paste("data/linkage/cohort", demo_file_id, save_time, "record_id_to_pt_id_lu.rds", sep = "_"))
