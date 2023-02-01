## Script for looking at markup of different comorbidities and notes for these

library(data.table)
library(googlesheets4)
library(readxl)
source("R/cleaning_fns_etl.r")


## Read in final refset v21 conceptids - comorbs - notes

  gdppr_snomed_codes_comorbs <- fread("D:/reference_data/final_v21_refset_full_markup_2021-03-05 154341.csv",
                                      colClasses = "character",
                                      na.strings = "")


## Download final cluster desc/comorbs - final comorbs, and save to D drive

  final_comorbs_address <- as.character(as_sheets_id("https://docs.google.com/spreadsheets/d/1YgsSr7JSwFzzbESRGI7HJD0D7469Q37ZCBW_QCphv30/edit#gid=0"))

  comorb_to_final_comorb_mapping <- data.table(range_read(final_comorbs_address,
                                                 sheet = "distinct_comorbs",
                                                 col_types = "c"))

  fwrite(comorb_to_final_comorb_mapping, file = "D:/reference_data/comorb_to_final_comorb_mapping.csv")


## Melt the refset, so have one comorb a line

  conceptid_comorb_and_notes <- melt(gdppr_snomed_codes_comorbs,
                                     id.vars = c("conceptid", "conceptid_description", "v18_all_cluster_descs", "v21_all_cluster_descs"),
                                     measure.vars = patterns("comorbidity_classification_", "notes_"),
                                     variable.name = "number",
                                     value.name = c("comorbidity_classification", "notes"))[!(is.na(comorbidity_classification) & is.na(notes))]


## Apply standardisation
## Merge this with conceptid markup to get final_comorb - notes pairings

  conceptid_comorb_and_notes[, ':=' (comorbidity_classification = tolower(fn_removeBlanks(comorbidity_classification)),
                                     notes = tolower(fn_removeBlanks(notes)))]

  refset_21_final_comorb <- merge(conceptid_comorb_and_notes,
                                  comorb_to_final_comorb_mapping,
                                  by = "comorbidity_classification",
                                  all.x = TRUE)


# Reduce to all final comorb - notes pairs for each conceptid -------------

  distinct_comorbs_notes_standard <- copy(refset_21_final_comorb[, .(conceptid, comorbidity_classification, final_comorb_group, notes)])


## Check that there are no concept-final_comorb pairs with more than one note

  stopifnot(distinct_comorbs_notes_standard[!is.na(final_comorb_group), .N, by = .(conceptid, final_comorb_group, notes)][, .N, by = .(conceptid, final_comorb_group)][N > 1, .N] == 0)


# Create instinct final_comorb-notes pairs --------------------------------
## The type of code, duration and value can be attached to this

  ## Remove anything that is down as drop or has no final_comorb_group

  final_comorb_notes_pairs <- setorder(unique(distinct_comorbs_notes_standard[!is.na(final_comorb_group),
                                                              .(final_comorb_group, notes)]), final_comorb_group, notes)


## Check that all final comorb groupings appear and therefore have a note

  stopifnot(length(setdiff(comorb_to_final_comorb_mapping[!is.na(final_comorb_group), final_comorb_group],
                           final_comorb_notes_pairs$final_comorb_group)) == 0)


## Save a copy of this for back up, and write to Google Drive

  fwrite(final_comorb_notes_pairs, file = "D:/reference_data/final_comorb_notes_pairs.csv")

  range_write(final_comorb_notes_pairs,
              ss = "https://docs.google.com/spreadsheets/d/1YgsSr7JSwFzzbESRGI7HJD0D7469Q37ZCBW_QCphv30/edit#gid=0",
              sheet = "distinct_final_comorbs_notes")


