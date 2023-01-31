source("R/cleaning_fns_etl.r")


# Read in HES data --------------------------------------------------------

ecds1 <- fread("D:/source_data/nhs_digital/ecds/NIC377644_ECDS_201999_APPROVED_51664_01010001_0.txt", colClasses = "character")
ecds2 <- fread("D:/source_data/nhs_digital/ecds/NIC377644_ECDS_202006_APPROVED_51665_01010001_0.txt", colClasses = "character")

ecds <- rbind(ecds1, ecds2)
rm(ecds1, ecds2)

setnames(ecds, make.names(toupper(colnames(ecds)), unique = TRUE))
ecds_col_names <- colnames(ecds)
ecds[, (ecds_col_names) := lapply(.SD, fn_removeBlanks), .SDcols = ecds_col_names]

# remove empty cols: OA_11, TOKEN_PERSON_ID
empty_cols <- c("OA_11", "TOKEN_PERSON_ID")
stopifnot(all(sapply(ecds[, ..empty_cols], function(field) all(is.na(field)))))
ecds[, (empty_cols) := NULL]
# ecds[, .N]
# sapply(ecds, function(col) sum(is.na(col)))

# Read in identity data ---------------------------------------------------

demo_file_id <- "FILE0115974_2021-03-18-163613"
study_id_to_pt_id_lu <- readRDS(paste0("data/linkage/cohort_", demo_file_id, "_study_id_to_pt_id_lu.rds"))
nhs_num_to_pt_id_lu <- readRDS(paste0("D:/source_data/tracing/cohort_", demo_file_id, "_nhs_num_to_pt_id_lu.rds"))

ecds <- merge(ecds,
             nhs_num_to_pt_id_lu[, .(nhs_no, patient_id)],
             by.x = "NHS_NUMBER",
             by.y = "nhs_no",
             all.x = TRUE)

# Retain only records for pts with a patient_id that appears in the demo data
ecds <- ecds[!is.na(patient_id)]


# Remove identifiable/identifying/confusing fields
ecds[, c("NHS_NUMBER",
         "POSTCODE",
         "BIRTH_DATE") := NULL]

# Retain unique records only
ecds_data <- unique(ecds)
ecds_data[, record_ID := as.integer(paste0("1", formatC(1:.N, width = nchar(nrow(ecds_data)), format = "d", flag = "0")))]

# Convert date fields to dates
ecds_date_cols <- c("ARRIVAL_DATE", "CONCLUSION_DATE", "DECIDED_TO_ADMIT_DATE", "DEPARTURE_DATE")
ecds_data[, (ecds_date_cols) := lapply(.SD, fn_covertStrToDate), .SDcols = ecds_date_cols]

# Covert time to datetimes
ecds_data[, ':=' (ARRIVAL_TIME = lubridate::fast_strptime(paste(ARRIVAL_DATE, substr(ARRIVAL_TIME, 1, 8)),
                                                          format = "%Y-%m-%d %H:%M:%S",
                                                          tz = "Europe/London",
                                                          lt = FALSE),
                  CONCLUSION_TIME = lubridate::fast_strptime(paste(CONCLUSION_DATE, substr(CONCLUSION_TIME, 1, 8)),
                                                          format = "%Y-%m-%d %H:%M:%S",
                                                          tz = "Europe/London",
                                                          lt = FALSE),
                  DECIDED_TO_ADMIT_TIME = lubridate::fast_strptime(paste(DECIDED_TO_ADMIT_DATE, substr(DECIDED_TO_ADMIT_TIME, 1, 8)),
                                                          format = "%Y-%m-%d %H:%M:%S",
                                                          tz = "Europe/London",
                                                          lt = FALSE),
                  DEPARTURE_TIME = lubridate::fast_strptime(paste(DEPARTURE_DATE, substr(DEPARTURE_TIME, 1, 8)),
                                                          format = "%Y-%m-%d %H:%M:%S",
                                                          tz = "Europe/London",
                                                          lt = FALSE))]

# Save ECDS data ----------------------------------------------------------

saveRDS(ecds_data, file = paste0("data/datasets/cohort_", demo_file_id, "_ecds_data.rds"))
