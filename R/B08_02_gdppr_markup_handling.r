## Script for taking in newly created and marked up v21 refset data with all comorbs and notes
## and standardising these to be useable against GDPPR dataset

library(data.table)
library(googlesheets4)
library(readxl)
source("R/cleaning_fns_etl.r")

# Download new refset v21 marked up -----------------------------------------------------

  refset_v21_marked_up <- as.character(as_sheets_id("https://docs.google.com/spreadsheets/d/154Els3dL0cwlQTLuX4P72Jgkh8YfIK7-PKD_5lk4yV0/edit#gid=994368997"))

  gdppr_snomed_codes_comorbs <- data.table(range_read(refset_v21_marked_up,
                                                      sheet = "v18_v21_multiple_cluster_comorbs_notes",
                                                      col_types = "c"))


# Melt into long form -----------------------------------------------------

  conceptid_comorb_and_notes <- melt(gdppr_snomed_codes_comorbs,
                                     id.vars = c("conceptid", "conceptid_description", "v18_all_cluster_descs", "v21_all_cluster_descs"),
                                     measure.vars = patterns("comorbidity_classification_", "notes_"),
                                     variable.name = "number",
                                     value.name = c("comorbidity_classification", "notes"))[!(is.na(comorbidity_classification) & is.na(notes))]



  conceptid_comorb_and_notes_standard <- unique(conceptid_comorb_and_notes[, .(conceptid,
                                                                               comorbidity_classification = fn_removeBlanks(comorbidity_classification),
                                                                               notes = fn_removeBlanks(notes))])

  conceptid_comorb_and_notes_standard <- conceptid_comorb_and_notes_standard[, .(conceptid,
                                                                                 comorbidity_classification = tolower(comorbidity_classification),
                                                                                 notes = tolower(notes))]

  distinct_comorbs <- unique(conceptid_comorb_and_notes_standard[, .(comorbidity_classification)])
  distinct_notes <- unique(conceptid_comorb_and_notes_standard[, .(notes)])

  distinct_comorbs <- distinct_comorbs[comorbidity_classification != "drop" & !is.na(comorbidity_classification) & comorbidity_classification != "na"]
  distinct_notes <- distinct_notes[!is.na(notes) & notes != "na"]

  distinct_comorbs <- unique(setorder(distinct_comorbs, comorbidity_classification))
  distinct_notes <- unique(setorder(distinct_notes, notes))


# Write out back to Google Drive to be checked and categorised ------------
## Changed to range_write as should just target the first col and not overwrite extra cols added later

  range_write(distinct_comorbs,
              ss = "https://docs.google.com/spreadsheets/d/1YgsSr7JSwFzzbESRGI7HJD0D7469Q37ZCBW_QCphv30/edit#gid=0",
              sheet = "distinct_comorbs")

  range_write(distinct_notes,
              ss = "https://docs.google.com/spreadsheets/d/1YgsSr7JSwFzzbESRGI7HJD0D7469Q37ZCBW_QCphv30/edit#gid=0",
              sheet = "distinct_notes")



## When comorb is drop or doesn't exist there should be no notes

  stopifnot(conceptid_comorb_and_notes_standard[((comorbidity_classification == "drop" | is.na(comorbidity_classification) | comorbidity_classification == "na"))
                                       & (!is.na(notes) & notes != "drop" & notes != "na"), .N] == 0)


## Check there are no concepts-comorbiditiy pairs that have different notes

  stopifnot(conceptid_comorb_and_notes_standard[, .N, by = .(conceptid, comorbidity_classification, notes)][, .N, by = .(conceptid, comorbidity_classification)][N > 1, .N] == 0)


## Check there is no concepts with a comorbidity but no notes

  stopifnot(conceptid_comorb_and_notes_standard[!is.na(comorbidity_classification) & comorbidity_classification != "drop" & (is.na(notes) | notes == "na"), .N] == 0)


# Save final static copy of marked up conceptids --------------------------

  save_time <- gsub(":", "", Sys.time(), fixed = TRUE)

  fwrite(gdppr_snomed_codes_comorbs, paste0("D:/reference_data/final_v21_refset_full_markup_", save_time,".csv"))

