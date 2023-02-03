library(data.table)
source("r/cleaning_fns_etl.r")

# read in data and sort NHSD stupid naming

  nhsd_cohort <- fread("D:/source_data/identifiers/NIC-377644-X9J4P_TS.csv")
  setnames(nhsd_cohort, "UNIQUE REFERENCE", "UNIQUE_REFERENCE")
  demo <- fread("D:/source_data/nhs_digital/demo/FILE0110833_NIC377644_Demographics__APPROVED_45362_01010001_0.txt", sep = "|")
  dr <- fread("D:/source_data/nhs_digital/dr/FILE0110832_NIC377644_CIVREG_MORT__APPROVED_45361_01010001_0.txt", sep = "|")


# DEMO data ---------------------------------------------------------------


# Keep only fields of interest and format

  demo_fields <- c('Study_ID', 'NHS_NO', 'GENDER', 'POSTCODE', 'DOB')
  demo[, colnames(demo)[!(colnames(demo) %in% demo_fields)] := NULL]
  demo[, POSTCODE := fn_cleanPostcode(POSTCODE)]
  demo[, DOB := as.character.Date(as.Date(substr(DOB, 1, 9), format = "%d%b%Y"), format = "%Y%m%d")]
  demo[, NHSD := TRUE]


# Merge data to compare
  compare <- merge(nhsd_cohort,
                   demo,
                   by.x = "UNIQUE_REFERENCE",
                   by.y = "Study_ID",
                   all = TRUE)

# Compare

  compare[, .N, by = NHSD]

  compare[NHSD == TRUE, .N, by = (DOB_match = DATE_OF_BIRTH == DOB)]
  compare[NHSD == TRUE, .N, by = (SEX_match = GENDER.x == GENDER.y)]
  compare[NHSD == TRUE, .N, by = (NHS_NO_match = NHS_NO.x == NHS_NO.y)]
  compare[NHSD == TRUE, .N, by = (POSTCODE_match = POSTCODE.x == POSTCODE.y)]

  compare[NHSD == TRUE, sum(POSTCODE.y == "")/.N]


# DEATH REG data ----------------------------------------------------------

  dr[, .N, by = (IN_DEMO = DEC_CONF_NHS_NUMBER %in% demo$NHS_NO)]
