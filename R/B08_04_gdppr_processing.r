source("R/cleaning_fns_etl.r")
setDTthreads(threads = 15)

# Read in GDPPR data --------------------------------------------------------

gdppr <- fread("D:/source_data/nhs_digital/gdppr/NIC377644_GDPPR_20201204_APPROVED_51666_01010001_0.txt", colClasses = "character")

setnames(gdppr, make.names(toupper(colnames(gdppr)), unique = TRUE))
gdppr_col_names <- colnames(gdppr)

gdppr[, (gdppr_col_names) := lapply(.SD, fn_removeBlanks), .SDcols = gdppr_col_names]

# require 1 NHS Number per study_id
stopifnot(gdppr[, .N, by = .(NHS_NUMBER, STUDY_ID)][, .N, by = STUDY_ID][N > 1, .N] == 0)

## Study_ids of GDPPR (& APC) data do not relate to study_IDs supplied to NHS Digital for tracing

# Read in identity data ---------------------------------------------------

demo_file_id <- "FILE0115974_2021-03-18-163613"
nhs_num_to_pt_id_lu <- readRDS(paste0("D:/source_data/tracing/cohort_", demo_file_id, "_nhs_num_to_pt_id_lu.rds"))

gdppr <- merge(gdppr,
             nhs_num_to_pt_id_lu[, .(nhs_no, patient_id)],
             by.x = "NHS_NUMBER",
             by.y = "nhs_no",
             all.x = TRUE)

# Retain only records for pts with a patient_id that appears in the demo data
# gdppr[is.na(patient_id), .N]
gdppr <- gdppr[!is.na(patient_id)]

# Remove identifiable/identifying fields
gdppr[, c("NHS_NUMBER",
          "STUDY_ID") := NULL]

# Retain unique records only
gdppr_data <- unique(gdppr)
gdppr_data[, record_ID := as.integer(paste0("1", formatC(1:.N, width = nchar(nrow(gdppr_data)), format = "d", flag = "0")))]

# Convert date fields to dates
gdppr_date_cols <- c("REPORTING_PERIOD_END_DATE", "DATE", "RECORD_DATE")
gdppr_data[, (gdppr_date_cols) := lapply(.SD, fn_covertStrToDate), .SDcols = gdppr_date_cols]

gdppr_data[, PROCESSED_TIMESTAMP := lubridate::fast_strptime(PROCESSED_TIMESTAMP, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/London", lt = FALSE)]


# Save GDPPR data ---------------------------------------------------------

saveRDS(gdppr_data, file = paste0("data/datasets/cohort_", demo_file_id, "_gdppr_data.rds"))
