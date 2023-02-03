library(data.table)
source("private/hash_fn.r")
source("R/outcome_functions.R")


  ## Script to build datasets for ML group
  demo_file_id <- "FILE0115974_2021-03-18-163613"

  ## Read in Single Source of Truth data
  demo_ssot <- readRDS(file = paste("D:/source_data/ssot/cohort", demo_file_id, "demo_ssot.rds", sep = "_"))

  # Read in ePR data --------------------------------------------------------
  epr_object_names <- load(paste("data/datasets/cohort", demo_file_id, "epr_freetext_anonymised_data.rda", sep = "_"))


  ## Find epr_ids to remove from epr single table, only table with patient id in

    epr_single_removed_patient <- removeExcludedPatient(epr_single_value_fields_tbl)

    excluded_epr_ids <- epr_single_value_fields_tbl[!(epr_id %chin% epr_single_removed_patient$epr_id), epr_id]


  ## Remove excluded patients

    removed_records <- sapply(epr_object_names, function(dt_name, excluded_ids) {
      dt <- get(dt_name)
      excluded_records <- dt[epr_id %in% excluded_ids, .N]
      assign(dt_name, dt[!(epr_id %in% excluded_ids)], envir = parent.frame(n = 3))
      return(excluded_records)
    }, excluded_ids = excluded_epr_ids)


  ## Add data from SSOT (and remove any original data fields which may conflict)

    epr_single_source_data <- getSingleSourceData(epr_single_value_fields_tbl,
                                                  demo_ssot = demo_ssot,
                                                  field_name_patient_id = "patient_id",
                                                  field_name_date_field = "incident_datetime",
                                                  field_name_contact_id = "epr_id")


    epr_single_value_fields_tbl[, c("age", "ethnicity", "sex") := NULL]

    epr_single_value_fields_tbl <- merge(epr_single_value_fields_tbl,
                                         epr_single_source_data,
                                         by = "epr_id",
                                         all.x = TRUE)


  ## Remove epr to record_id lookup
    rm(epr_record_ID_tbl)

    epr_object_names <- epr_object_names[epr_object_names != "epr_record_ID_tbl"]

  ## Save output with date

    save_date <- Sys.Date()
    save_path <- "data/data_for_ml"

    record_counts <- sapply(epr_object_names, function(dt_name, save_loc, save_dt) {
      dt <- get(dt_name)
      records <- dt[, .N]
      fwrite(dt, file = paste0(save_loc, "/", dt_name, "_", save_dt, ".csv"))
      return(records)
    },
    save_loc = save_path,
    save_dt = save_date)
