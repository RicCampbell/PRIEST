source("R/cleaning_fns_etl.r")

dr <- fread("D:/source_data/nhs_digital/dr/FILE0115975_NIC377644_CIVREG_MORT__APPROVED_54000_01010001_0.txt", colClasses = "character")

setnames(dr, make.names(toupper(colnames(dr)), unique = TRUE))
dr_col_names <- colnames(dr)
dr[, (dr_col_names) := lapply(.SD, fn_removeBlanks), .SDcols = dr_col_names]

empty_fields <- sapply(dr, function(field) all(is.na(field)))
dr[, names(empty_fields)[empty_fields] := NULL]
dr[, NIC_NUMBER := NULL]

# Read in identity data ---------------------------------------------------

demo_file_id <- "FILE0115974_2021-03-18-163613"
nhs_num_to_pt_id_lu <- readRDS(paste0("D:/source_data/tracing/cohort_", demo_file_id, "_nhs_num_to_pt_id_lu.rds"))

dr_data <- unique(merge(dr,
             nhs_num_to_pt_id_lu,
             by.x = "DEC_CONF_NHS_NUMBER",
             by.y = "nhs_no",
             all.x = TRUE)[, DEC_CONF_NHS_NUMBER := NULL])

stopifnot(dr_data[is.na(patient_id), .N] == 0)


# Find those in the cohort who appear to have died twice
excluded_patient_ids <- dr_data[, .N, by = patient_id][N > 1, patient_id]
dr_data <- dr_data[!(patient_id %in% excluded_patient_ids)]
stopifnot(dr_data[, .N, by = patient_id][N > 1, .N] == 0)


# Convert date of death to date type
dr_data[, DATE_OF_DEATH := fn_covertStrToDate(REG_DATE_OF_DEATH, dt_format = "%Y%m%d")]
dr_data[, REG_DATE_OF_DEATH := NULL]

saveRDS(dr_data, file = paste0("data/datasets/cohort_", demo_file_id, "_dr_data.rds"))
saveRDS(excluded_patient_ids, file = paste0("data/datasets/cohort_", demo_file_id, "_multi_death_excluded_patient_ids.rds"))
