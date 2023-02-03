## Script to find the epr_ids that are present in the epr dataset but not the freetext dataset

library(data.table)

## Read in both sets of data that was supplied to stats and machine learning virtual machines

  epr <- fread("data/data_for_stats/cohort_2021-05-06_ambulance_attended.csv")

  freetext <- fread("data/data_for_ml/2021-04-06/epr_freetext_excl_ecg_tbl_2021-04-06.csv")


## Find the different between the two lists of epr_ids

  epr_ids_no_freetext <- setdiff(epr[ssot_calculated_age > 15 & patient_record_order == 1, epr_id], freetext[, epr_id])


## Write out these epr_ids to be transferred to machine learning virtual machine

  save_date <- Sys.Date()

  write.csv(epr_ids_no_freetext, file = paste0("data/data_for_ml/2021-04-06/extra_", save_date, "_epr_ids_no_freetext.csv"))
