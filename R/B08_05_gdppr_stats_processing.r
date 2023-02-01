library(data.table)
library(googlesheets4)
library(readxl)
source("R/outcome_functions.R")
source("R/cleaning_fns_etl.r")


# Read in data needed -----------------------------------------------------

  ## Read in processed GDPPR data

    demo_file_id <- "FILE0115974_2021-03-18-163613"

    gdppr_data <- readRDS(file = paste0("data/datasets/cohort_", demo_file_id, "_gdppr_data.rds"))


  ## Only keep cols that are interested in

    gdppr_data[, c("EHNIC", "GP_SYSTEM_SUPPLIER", "PROCESSED_TIMESTAMP", "EPISODE_CONDITION", "EPISODE_PRESCRIPTION") := NULL]


  ## Change col of measurements to double to keep precision for later

    gdppr_data[, VALUE1_CONDITION := as.double(VALUE1_CONDITION)]


    ## Read in SNOMED codes from D Drive with comorb-notes cols

      gdppr_snomed_codes_comorbs <- fread("D:/reference_data/final_v21_refset_full_markup_2021-03-05 154341.csv",
                                          colClasses = "character",
                                          na.strings = "")


    ## Need to melt this to long form

      conceptid_comorb_and_notes <- melt(gdppr_snomed_codes_comorbs,
                                         id.vars = "conceptid",
                                         measure.vars = patterns("comorbidity_classification_", "notes_"),
                                         variable.name = "number",
                                         value.name = c("comorbidity_classification", "notes"))[!(is.na(comorbidity_classification) & is.na(notes))]


  ## Read in saved v21 refset which has only one description per conceptid - taking concept description from original reference file is better than one uploaded/downloaded

      refset_v21_canonical_desc <- fread("D:/reference_data/refset_v21_canonical_desc.csv", colClasses = "character")


  ## Merge in canonical concept description to conceptids have saved with markup comorb-notes

    canonical_conceptid_comorb_and_notes <- merge(conceptid_comorb_and_notes,
                                                  refset_v21_canonical_desc[, .(conceptid, conceptid_description)],
                                                  by = "conceptid",
                                                  all.x = TRUE)


    stopifnot(canonical_conceptid_comorb_and_notes[is.na(conceptid_description), .N] == 0)


  ## Apply standardisation for merging

    canonical_conceptid_comorb_and_notes[, ':=' (comorbidity_classification = tolower(fn_removeBlanks(comorbidity_classification)),
                                                 notes = tolower(fn_removeBlanks(notes)))]

### THIS IS WHERE CAN CHANGE EX-SMOKER ####

  ## Read in sheet that has final comorb grouping - checked with Carl

    comorb_to_final_comorb_mapping <- fread("D:/reference_data/comorb_to_final_comorb_mapping.csv",
                                            colClasses = "character",
                                            na.strings = "")

    # comorb_to_final_comorb_mapping[comorbidity_classification == "ex-smoker (social history)",
    #                                final_comorb_group := "ex-smoker"]


## Merge marked up snomed codes with file that has groupings of comorbs that we are interested in - drops are removed here

  gdppr_snomed_codes_final_comorbs <- merge(canonical_conceptid_comorb_and_notes[, .(conceptid, conceptid_description, comorbidity_classification, notes)],
                                            comorb_to_final_comorb_mapping,
                                            by = "comorbidity_classification",
                                            all.x = TRUE)[!is.na(final_comorb_group)]


  ## Save this file as want to be able to reference/look at for what is/isn't included in each comorb

  saveRDS(gdppr_snomed_codes_final_comorbs, file = "D:/reference_data/snomed_code_final_comorb_group_reference_file.rds")


## Check to see that all final_comorbs appear at least once

    stopifnot(sum(!comorb_to_final_comorb_mapping[!is.na(final_comorb_group), unique(final_comorb_group)] %chin% gdppr_snomed_codes_final_comorbs[, unique(final_comorb_group)]) == 0)


# Read in reference date and add additional fields --------------------------------------------------

  ## Read in file that has interpretations of each notes field and the scoring system for certain codes
  ## original code commented out to check.make changes

    # snomed_notes_field_interp <- data.table(read_excel("D:/reference_data/final_comorb_notes_pairs_with_coding_scheme.xlsx",
    #                                                    sheet = "final_comorb_notes_pairs",
    #                                                    col_names = TRUE,
    #                                                    col_types = c("text", "text", "text", "numeric", "text", "numeric"),
    #                                                    trim_ws = TRUE))
    #
    # snomed_notes_field_mapping <- data.table(read_excel("D:/reference_data/final_comorb_notes_pairs_with_coding_scheme.xlsx",
    #                                                    sheet = "comorb_value_mapping",
    #                                                    col_names = TRUE,
    #                                                    col_types = c("text", "numeric", "numeric", "numeric", "numeric"),
    #                                                    trim_ws = TRUE))


    snomed_notes_field_interp <- data.table(read_excel("D:/reference_data/final_comorb_notes_pairs_with_coding_scheme_post_change.xlsx",
                                                       sheet = "final_comorb_notes_pairs",
                                                       col_names = TRUE,
                                                       col_types = c("text", "text", "text", "numeric", "text", "numeric"),
                                                       trim_ws = TRUE))

    snomed_notes_field_mapping <- data.table(read_excel("D:/reference_data/final_comorb_notes_pairs_with_coding_scheme_post_change.xlsx",
                                                        sheet = "comorb_value_mapping",
                                                        col_names = TRUE,
                                                        col_types = c("text", "numeric", "numeric", "numeric", "numeric"),
                                                        trim_ws = TRUE))


# Attach reference date to GP records -------------------------------------

  ## Remove any GDPPR records that do not contain a SNOMED code relating to a comorb that we are interested in, take copy

    gdppr_data_reduced <- copy(gdppr_data[CODE %chin% unique(gdppr_snomed_codes_final_comorbs$conceptid)])


    rm(gdppr_data)
    gc()


  ## Merge in comorb cats based on conceptid (allow cartesian as conceptids can have multiple comorbs, so will be duplicate gdppr records but for different comorbs)
  ## The unique around the comorbs dataset is due to codes being in different classifications in snomed, but have the same comorb and notes for PRIEST

    gdppr_data_reduced_comorbs <- merge(gdppr_data_reduced,
                                        unique(gdppr_snomed_codes_final_comorbs[, .(conceptid, final_comorb_group, notes, conceptid_description)]),
                                        by.x = "CODE",
                                        by.y = "conceptid",
                                        all.x = TRUE,
                                        allow.cartesian = TRUE)


  ## Check that no record has been duplicated with the same final comorb grouping

    stopifnot(gdppr_data_reduced_comorbs[, .N, by = .(record_ID, final_comorb_group)][N > 1, .N] == 0)


  ## Merge of gdppr records and fields that help interpret the notes field

    gdppr_data_comorb_interp <- merge(gdppr_data_reduced_comorbs[, .(notes, REPORTING_PERIOD_END_DATE, DATE, RECORD_DATE, patient_id,
                                                                     final_comorb_group, record_ID, VALUE1_CONDITION, conceptid_description)],
                                      snomed_notes_field_interp,
                                      by = c("notes", "final_comorb_group"),
                                      all.x = TRUE)


    stopifnot(gdppr_data_comorb_interp[is.na(type_of_code), .N] == 0)
    stopifnot(gdppr_data_comorb_interp[, .N] == gdppr_data_reduced_comorbs[, .N])


# Tidy GDPPR dataset to records that are useful ---------------------------

  ## Remove any records that are meant to be a value check but do not have a value in VALUE1_CONDITION due to GDPPR export issue
  ## Also remove any ethnicity codes as this has already been processed
  ## e.g BMI test, without value gives no pertinent information

    not_value_codes <- c("positive", "negative", "bmi", "frailty")

    gdppr_data_comorb_interp_reduced <- gdppr_data_comorb_interp[type_of_code %in% not_value_codes
                                                                 | (type_of_code == "value_check" & !is.na(VALUE1_CONDITION))
                                                                 | (type_of_code == "score" & !is.na(VALUE1_CONDITION))]


  ## Remove any records that do not have a DATE, have no way of telling how recent these are as other dates can be much more recent than DATE

    gdppr_data_comorb_interp_reduced <- gdppr_data_comorb_interp_reduced[!is.na(DATE)]


# Label records dependent on SNOMED code ----------------------------------

  ## Create field that indicates if the comorb record relates to is present or not

    gdppr_data_comorb_interp_reduced[type_of_code == "positive", ':=' (comorb_present = TRUE, comorb_value = conceptid_description)]
    gdppr_data_comorb_interp_reduced[type_of_code == "negative", ':=' (comorb_present = FALSE, comorb_value = conceptid_description)]


    gdppr_data_comorb_interp_reduced[type_of_code == "value_check", ':=' (comorb_present = valueCheckFunction(VALUE1_CONDITION, scoring_system, snomed_notes_field_mapping),
                                                                          comorb_value = VALUE1_CONDITION)]


## Range of values means decimal results or those over 9 are ignored

    gdppr_data_comorb_interp_reduced[type_of_code == "score" & final_comorb_group == "frailty" & VALUE1_CONDITION %in% 1:9,
                                     ':=' (comorb_present = TRUE, comorb_value = VALUE1_CONDITION)]

    gdppr_data_comorb_interp_reduced[type_of_code == "frailty",  ':=' (comorb_present = TRUE, comorb_value = single_code_concept_value)]


## Every record has now been attributed to either showing that the comorb is present or not
  ## Remove NA records as do not provide any information, these are obese or frailty with out of range values

    gdppr_data_comorb_interp_reduced <- gdppr_data_comorb_interp_reduced[!is.na(comorb_present)]


# Save records -----------------------------------------------------------

    ## Save whole big file - trim cols
  ## original code commented out to make check/changes - saves to different file name

    # saveRDS(gdppr_data_comorb_interp_reduced[!is.na(comorb_present), .(REPORTING_PERIOD_END_DATE, DATE, RECORD_DATE, patient_id, final_comorb_group,
    #                                                                    record_ID, days_applicable_for, comorb_present, comorb_value)],
    #         file = paste0("data/datasets/cohort_", demo_file_id, "_gdppr_comorb_records.rds"))



    saveRDS(gdppr_data_comorb_interp_reduced[!is.na(comorb_present), .(REPORTING_PERIOD_END_DATE, DATE, RECORD_DATE, patient_id, final_comorb_group,
                                                                       record_ID, days_applicable_for, comorb_present, comorb_value)],
            file = paste0("data/datasets/cohort_", demo_file_id, "_gdppr_comorb_records_post_change.rds"))



