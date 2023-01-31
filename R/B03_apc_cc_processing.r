source("R/cleaning_fns_etl.r")


# Read in HES data --------------------------------------------------------

apc1 <- fread("D:/source_data/nhs_digital/apc/NIC377644_HES_APC_201999_APPROVED_51667_01010001_0.txt", colClasses = "character")
apc2 <- fread("D:/source_data/nhs_digital/apc/NIC377644_HES_APC_202006_APPROVED_51668_01010001_0.txt", colClasses = "character")
cc1 <- fread("D:/source_data/nhs_digital/cc/NIC377644_HES_CC_201999_APPROVED_51669_01010001_0.txt", colClasses = "character")
cc2<- fread("D:/source_data/nhs_digital/cc/NIC377644_HES_CC_202006_APPROVED_51670_01010001_0.txt", colClasses = "character")

apc <- rbind(apc1, apc2)
cc <- rbind(cc1, cc2)
rm(apc1, apc2, cc1, cc2)

setnames(apc, make.names(toupper(colnames(apc)), unique = TRUE))
apc_col_names <- colnames(apc)
apc[, (apc_col_names) := lapply(.SD, fn_removeBlanks), .SDcols = apc_col_names]

empty_cols <- c(paste(c("DEPDAYS", "INTDAYS"), rep(1:9, each = 2), sep = "_"), "MATCHID", "NUMACP")
stopifnot(all(sapply(apc[, ..empty_cols], function(field) all(is.na(field)))))
apc[, eval(empty_cols) := NULL]

cc_col_names <- colnames(cc)
cc[, (cc_col_names) := lapply(.SD, fn_removeBlanks), .SDcols = cc_col_names]

## Study_ids of APC data do not relate to study_IDs supplied to NHS Digital for tracing
# apc[, STUDY_ID := as.integer(STUDY_ID)]


# Read in identity data ---------------------------------------------------

demo_file_id <- "FILE0115974_2021-03-18-163613"
study_id_to_pt_id_lu <- readRDS(paste0("data/linkage/cohort_", demo_file_id, "_study_id_to_pt_id_lu.rds"))
nhs_num_to_pt_id_lu <- readRDS(paste0("D:/source_data/tracing/cohort_", demo_file_id, "_nhs_num_to_pt_id_lu.rds"))

## Study_ids of APC data do not relate to study_IDs supplied to NHS Digital for tracing
# apc <- merge(apc,
#              study_id_to_pt_id_lu,
#              by.x = "STUDY_ID",
#              by.y = "study_id",
#              all.x = TRUE)

apc <- merge(apc,
             nhs_num_to_pt_id_lu[, .(nhs_no, patient_id)],
             by.x = "NEWNHSNO",
             by.y = "nhs_no",
             all.x = TRUE)

# Retain only records for pts with a patient_id that appears in the demo data
apc <- apc[!is.na(patient_id)]

# # There are now no discrepancies between HES study_id and nhs_num tracing - since not possible to trace by study_id
# stopifnot(apc[, .N, by = .(patient_id, patient_id_from_nhsno)][, .N, by = patient_id][N > 1, .N] == 0)

# Ensure we have only one patient_id per encrypted_hesid
stopifnot(apc[, .N, by = .(patient_id, NEWNHSNO)][!is.na(NEWNHSNO), .N, by = NEWNHSNO][N > 1, .N] == 0 &
            apc[, .N, by = .(patient_id, ENCRYPTED_HESID)][, .N, by = ENCRYPTED_HESID][N > 1, .N] == 0)

# Ensure we have only one patient_id per distinct APC record
stopifnot(apc[, .N, by = .(patient_id, EPIKEY)][, .N, by = EPIKEY][N > 1, .N] == 0)


# Remove identifiable/identifying/confusing fields
apc[, c("ENCRYPTED_HESID",
        "MATCH_RANK",
        "NEWNHSNO",
        "OACODE11",
        "PROVSPNOPS",
        "STUDY_ID") := NULL]

# Retain unique records only
apc_data <- unique(apc)
stopifnot(apc_data[, .N, by = EPIKEY][N > 1, .N] == 0)

# Convert date fields to dates
apc_date_cols <- c("ADMIDATE", "DISDATE", "EPISTART", "EPIEND")
apc_data[, (apc_date_cols) := lapply(.SD, fn_covertStrToDate), .SDcols = apc_date_cols]


# Critical Care data ------------------------------------------------------

susrecid_to_pt_id_lu <- unique(apc_data[, .(SUSRECID, patient_id)])

cc_data <- merge(cc,
                 susrecid_to_pt_id_lu,
                 by = "SUSRECID",
                 all = FALSE)

# Convert date field to ISO
cc_date_cols_convert <- c("ccdisdate", "ccstartdate")
cc_data[, (cc_date_cols_convert) := lapply(.SD, fn_date_to_ISOdate, format_type = "yyyymmdd"), .SDcols = cc_date_cols_convert]

# Convert date fields to dates
cc_date_cols <- c("admidate", "DISDATE", "ccdisdate", "ccstartdate", "EPISTART", "EPIEND")
cc_data[, (cc_date_cols) := lapply(.SD, fn_covertStrToDate), .SDcols = cc_date_cols]

# Convert days to integers
cc_int_cols <- c("acardsupdays", "aressupdays", "bcardsupdays", "bressupdays", "cclev2days", "cclev3days", "rensupdays")
cc_data[, (cc_int_cols) := lapply(.SD, as.integer), .SDcols = cc_int_cols]


# Save HES data -----------------------------------------------------------

saveRDS(apc_data, file = paste0("data/datasets/cohort_", demo_file_id, "_apc_data.rds"))
saveRDS(cc_data, file = paste0("data/datasets/cohort_", demo_file_id, "_cc_data.rds"))
