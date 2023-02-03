library(data.table)
source("R/outcome_functions.R")

demo_file_id <- "FILE0115974_2021-03-18-163613"
demo_ssot <- readRDS(file = paste("D:/source_data/ssot/cohort", demo_file_id, "demo_ssot.rds", sep = "_"))

drug_counts <- getDrugCountByPatientDate(demo_ssot[, .(patient_id, ac_date = as.Date("2020-06-01"))],
                                      pt_id_field = "patient_id",
                                      activity_date_field = "ac_date",
                                      duration_days = 365L)

drug_counts[, .N, by = drug_count][order(drug_count)]

drug_counts <- merge(drug_counts,
                     demo_ssot,
                     by = "patient_id")
