library(data.table)
library(googlesheets4)
library(tinytex)
library(knitr)

## Read in all data from NHS Digital

apc1 <- fread("D:/source_data/nhs_digital/apc/NIC377644_HES_APC_201999_APPROVED_47032_01010001_0.txt")
apc2 <- fread("D:/source_data/nhs_digital/apc/NIC377644_HES_APC_202005_APPROVED_47033_01010001_0.txt")

apc <- rbind(apc1, apc2)


critical_care1 <- fread("D:/source_data/nhs_digital/cc/NIC377644_HES_CC_201999_APPROVED_47034_01010001_0.txt")
critical_care2<- fread("D:/source_data/nhs_digital/cc/NIC377644_HES_CC_202005_APPROVED_47035_01010001_0.txt")

critical_care <- rbind(critical_care1, critical_care2)


demographic <- fread("D:/source_data/nhs_digital/demo/FILE0113951_NIC377644_Demographics__APPROVED_47886_01010001_0.txt")

death_reg <- fread("D:/source_data/nhs_digital/dr/FILE0113950_NIC377644_CIVREG_MORT__APPROVED_47885_01010001_0.txt")


# ecds1 <- fread("D:/source_data/nhs_digital/ecds/NIC377644_ECDS_201999_APPROVED_47030_01010001_0.txt")
# ecds2 <- fread("D:/source_data/nhs_digital/ecds/NIC377644_ECDS_202005_APPROVED_47031_01010001_0.txt")
# ecds <- rbind(ecds1, ecds2)

rm(apc1, apc2, critical_care1, critical_care2, ecds1, ecds2)


## Read in col names that we asked for from Google Sheet

nhs_digital_requested_data <- as.character(as_sheets_id("https://docs.google.com/spreadsheets/d/1ONAYEbbd_dfwqfy-D0jrOeP31pem-gFytsyojfwrp1o/edit#gid=1397121393"))


## Read in sites sheet from google drive - correctly formatted field name has been done

## Might have to put a delay in as too many read requests in short space of time?

apc_requested <- data.table(range_read(nhs_digital_requested_data,
                                      sheet = "APC"))

cc_requested <- data.table(range_read(nhs_digital_requested_data,
                                       sheet = "CC"))

demo_requested <- data.table(range_read(nhs_digital_requested_data,
                                       sheet = "DEMO"))

dr_requested <- data.table(range_read(nhs_digital_requested_data,
                                       sheet = "DR"))

ecds_requested <- data.table(range_read(nhs_digital_requested_data,
                                       sheet = "ECDS"))



## Change all col names to upper case and take col names and change one to correct data dictionary label

apc_colnames <- colnames(setnames(apc, colnames(apc), toupper(colnames(apc))))
cc_colnames <- colnames(setnames(critical_care, colnames(critical_care), toupper(colnames(critical_care))))
demo_colnames <- colnames(setnames(demographic, colnames(demographic), toupper(colnames(demographic))))
dr_colname <- colnames(setnames(death_reg, colnames(death_reg), toupper(colnames(death_reg))))
ecds_colnames <- colnames(setnames(ecds, colnames(ecds), toupper(colnames(ecds))))

setnames(apc, "PROVSPNOPS", "PROVSPNO")


## Make changes to requested list to add entries that have multiple fields

apc_requested_names <- c(apc_requested[field_name != "DEPDAYS_N" & field_name !="DIAG_4_NN" & field_name != "INTDAYS_N", field_name],
                         paste0("DEPDAYS_", 1:9), paste0("DIAG_4_", formatC(1:20, width = 2, format = "d", flag = "0")), paste0("INTDAYS_", 1:9))




## Check that we have received all the cols asked for, and if have received any more

missing_apc_cols <- apc_requested_names[!apc_requested_names %in% apc_colnames]
extra_apc_cols <- apc_colnames[!apc_colnames %in% apc_requested_names]

missing_cc_cols <- cc_requested[!cc_requested[, toupper(field_name)] %in% cc_colnames][, field_name]
extra_cc_cols <- cc_colnames[!cc_colnames %in% cc_requested[, toupper(field_name)]]

missing_demo_cols <- demo_requested[!demo_requested[, field_name] %in% demo_colnames][, field_name]
extra_demo_cols <- demo_colnames[!demo_colnames %in% demo_requested[, field_name]]

missing_dr_cols <- dr_requested[!dr_requested[, field_name] %in% dr_colname][, field_name]
extra_dr_cols <- dr_colname[!dr_colname %in% dr_requested[, field_name]]

missing_ecds_cols <- ecds_requested[!ecds_requested[, field_name] %in% ecds_colnames][, field_name]
extra_ecds_cols <- ecds_colnames[!ecds_colnames %in% ecds_requested[, field_name]]


## Look if any data is in the extra cols provided for either demographics or death register

demo_non_na_count <- t(demographic[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols = extra_demo_cols])

death_reg_non_na_count <- t(death_reg[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols = extra_dr_cols])


## Limit APC data to just the Study ids we have in demo dataset as this is what will be used as out single source of truth

demo_study_ids <- demographic[, STUDY_ID]

apc_demo_only <- apc[STUDY_ID %in% demo_study_ids]


## Find number of unique records, unique study_ids (or similar) for each dataset before building table

### APC

  apc_records <- length(apc_demo_only[!is.na(EPIKEY), unique(EPIKEY)])
  apc_people_soft <- length(apc_demo_only[!is.na(ENCRYPTED_HESID), unique(ENCRYPTED_HESID)])
  apc_study_ids <- length(apc_demo_only[!is.na(STUDY_ID), unique(STUDY_ID)])


### CC
  ## Find all APC records that have a SUSRECID from CC, to find numbers for critical care

  all_cc_susrecids <- critical_care[, unique(SUSRECID)]

  cc_apc_sub_cohort <- apc_demo_only[SUSRECID %in% all_cc_susrecids]


  ## Check if have an APC record for every Critical Care record

  stopifnot(length(setdiff(all_cc_susrecids, apc[, unique(SUSRECID)])) == 0)


  ## How many do we not have an APC record as have removed APC records not recorded in the demo dataset

  cc_records_not_from_demo_trace <- length(setdiff(all_cc_susrecids, cc_apc_sub_cohort[, unique(SUSRECID)]))


  ## Find numbers for critical care

  cc_records <- length(all_cc_susrecids)
  cc_people_soft <- length(cc_apc_sub_cohort[!is.na(ENCRYPTED_HESID), unique(ENCRYPTED_HESID)])
  cc_study_ids <- length(cc_apc_sub_cohort[!is.na(STUDY_ID), unique(STUDY_ID)])


### Demographics

  demo_records <- unique(demographic)[, .N]
  demo_people_soft <- length(demographic[!is.na(NHS_NO), unique(NHS_NO)])
  demo_study_ids_count <- length(demographic[!is.na(STUDY_ID), unique(STUDY_ID)])


### Death register

  death_records <- unique(death_reg)[, .N]
  death_people_soft <- length(death_reg[!is.na(DEC_CONF_NHS_NUMBER), unique(DEC_CONF_NHS_NUMBER)])
  death_study_ids <- length(death_reg[!is.na(STUDY_ID), unique(STUDY_ID)])


## Create table with numbers of rows, distinct records, unique study_ids, min/max dates

dataset_numbers <- data.table(dataset = "APC", number_of_rows = apc[, .N], number_of_records = apc_records, rough_number_of_people = apc_people_soft, number_study_ids = apc_study_ids,
                              min_date = as.character(apc[!is.na(ADMIDATE), min(ADMIDATE)]), max_date = as.character(apc[!is.na(ADMIDATE), max(ADMIDATE)]))

dataset_numbers <- rbind(dataset_numbers, list("Critical Care", critical_care[, .N], cc_records, cc_people_soft, cc_study_ids,
                                               as.character(critical_care[!is.na(EPISTART), min(EPISTART)]), as.character(critical_care[!is.na(EPISTART), max(EPISTART)])))

dataset_numbers <- rbind(dataset_numbers, list("Demographics", demographic[, .N], demo_records, demo_people_soft, demo_study_ids_count, NA, NA))

dataset_numbers <- rbind(dataset_numbers, list("Death Register", death_reg[, .N], death_records, death_people_soft, death_study_ids, NA, NA))

dataset_numbers <- rbind(dataset_numbers, list("ECDS", ecds[, .N], ecds[!is.na(ARRIVAL_DATE), min(ARRIVAL_DATE)], ecds[!is.na(ARRIVAL_DATE), max(ARRIVAL_DATE)]))

#dataset_numbers <- rbind(dataset_numbers, list("GDPPR", gdppr[!is.na(DATE), min(DATE)], gdppr[!is.na(DATE), max(DATE)]))


## Create table of numbers of rows, distinct records, unique study_ids

dataset_numbers <- data.table(dataset = "APC", )



# Tidy up demographics dataset prior to merges ----------------------------

## Reduce cols of demo data to those that we asked for

demo_reduced <- copy(demographic)
demo_reduced[, (extra_demo_cols) := NULL]


## Change col names so when merged they are different

demo_reduced_col_new <- paste(tolower(colnames(demo_reduced)), "demo", sep = "_")
setnames(demo_reduced, colnames(demo_reduced), demo_reduced_col_new)


## Replace all blanks with NA to allow comparison

demo_reduced_colname <- colnames(demo_reduced)
demo_empty_count <- demo_reduced[postcode_demo == "", .N]

demo_reduced[, (demo_reduced_colname) := lapply(.SD, function(x) replace(x, x == "", NA)), .SDcols = demo_reduced_colname]

stopifnot(demo_empty_count == demo_reduced[is.na(postcode_demo), .N])


## Change format of date of birth to match

demo_reduced[, date_of_birth := gsub("(:).*", "", dob_demo)]
demo_reduced[, date_of_birth := as.Date(date_of_birth, format = "%d%b%Y")]


# APC Section -------------------------------------------------------------

## Merge APC data with demo data

  ## Take row count to check later

  apc_row_count <- apc[, .N]


  ## Merge the two table

  apc_demo <- merge(apc, demo_reduced[, .(study_id_demo, nhs_no_demo)], by.x = "STUDY_ID", by.y = "study_id_demo", all.x = TRUE)

  stopifnot(apc_row_count == apc_demo[, .N])


## How many rows have a nhs number (percentages later)

  apc_records_with_nhs_number <- apc_demo[!is.na(nhs_no_demo), .N]




## Table of records traced, percentage and average records per person

  nhs_digital_breakdown_table <- data.table(data_source = "APC", row_count = apc[, .N], )

