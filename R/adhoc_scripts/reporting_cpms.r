library(data.table)
library(openxlsx)

# read in relevant datasets
nhs_no_to_pt_id <- readRDS("D:/source_data/tracing/cohort_FILE0115974_2021-03-18-163613_nhs_num_to_pt_id_lu.rds")
core_priest <- fread("D:/source_data/core_priest/master_labelled.csv", colClasses = "character")
core_priest_cpms <- data.table(read.xlsx("D:/source_data/cpms_reporting/cpms_out_20200714.xlsx", detectDates = TRUE))

# Sort existing accruals data format, etc.
cpms_col_names <- copy(colnames(core_priest_cpms))
setnames(core_priest_cpms, make.names(cpms_col_names, unique = TRUE))
core_priest_cpms[, Unique.Participant.Id := as.character(Unique.Participant.Id)]

# Merge in NHS Numbers (from core-PRIEST) to existing accruals
core_priest_cpms_nhs_no <- merge(core_priest_cpms,
                                 core_priest[, .(Unique.Participant.Id = individual_id, nhs_no = screening)],
                                 by = "Unique.Participant.Id",
                                 all = TRUE)

# Check every record in core-PRIEST is in existing CPMS
stopifnot(core_priest_cpms_nhs_no[is.na(Activity.Type), .N] == 0)

# there are two records in CPMS but not in core-PRIEST data
## (not much we can do about these)
core_priest_cpms_nhs_no[is.na(nhs_no), .N]

# before merging prehospital-PRIEST data, check no missing data and all 1-to-1
stopifnot(nhs_no_to_pt_id[is.na(nhs_no) | is.na(patient_id), .N] == 0)
stopifnot(nhs_no_to_pt_id[, .N, by = nhs_no][N > 1, .N] == 0)
stopifnot(nhs_no_to_pt_id[, .N, by = patient_id][N > 1, .N] == 0)

# Merge in prehosital-PRIEST data
all_priest_cpms <- merge(core_priest_cpms_nhs_no,
                             nhs_no_to_pt_id[, .(nhs_no, prehosp_id = patient_id)],
                             by = "nhs_no",
                             all = TRUE)[, nhs_no := NULL]

# For those records which did not exist in the core-PRIEST CPMS, add details
all_priest_cpms[is.na(Unique.Participant.Id), ':=' (Unique.Participant.Id = paste0("PH", prehosp_id),
                                                        Study.Identifier = 12725,
                                                        Study.Acronym = "The PRIEST Study",
                                                        Site.Identifier = "RX8",
                                                        Site.Name = "YORKSHIRE AMBULANCE SERVICE NHS TRUST",
                                                        Activity.Date = as.Date("2020-06-29"),
                                                        Participant.Type = "Other",
                                                        Activity.Type = "Recruitment")]


# Format for output
all_priest_cpms[, prehosp_id := NULL]
setorder(all_priest_cpms, Site.Identifier, Unique.Participant.Id)
setcolorder(all_priest_cpms, make.names(cpms_col_names, unique = TRUE))
setnames(all_priest_cpms, cpms_col_names)

write.xlsx(all_priest_cpms,
           paste0("D:/source_data/cpms_reporting/cpms_out_", format.Date(Sys.Date(), format = "%Y%m%d"), ".xlsx"),
           sheetName = "Table",
           colNames = TRUE,
           rowNames = FALSE,
           overwrite = TRUE)
