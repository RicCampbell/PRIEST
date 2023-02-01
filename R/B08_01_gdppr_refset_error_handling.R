## Script for processing GDPPR referance data
## V18 release was corrupted, this was used to markup concepts by the project team
## These markup notes and classifications need to be retained

library(data.table)
library(googlesheets4)
library(readxl)
source("R/cleaning_fns_etl.r")


## Read in SNOMED codes from Google Drive that have standardisation of comorbidities in the "notes" col (can take a minute)

  nhs_digital_requested_data <- as.character(as_sheets_id("https://docs.google.com/spreadsheets/d/1d65IPOG_y-wfcsOdqkhK9ZYyBiAVmscw4CUbdsl95Wc/edit?usp=sharing"))

  gdppr_snomed_codes_comorbs <- data.table(range_read(nhs_digital_requested_data, sheet = "priest_gdppr_cluster_refset-2020-11-10 161112",
                                                      col_types = "c"))


# ####~~~
#       ## Back up if download from Google Drive is not available due to quota issues
#       gdppr_snomed_codes_comorbs <- fread(file = "D:/reference_data/google_drive_download_2021-02-08_priest_gdppr_cluster_refset-2020-11-10 161112.csv", colClasses = "character")
#
# ####~~~~

## Rename last col

  setnames(gdppr_snomed_codes_comorbs, "Notes (e.g. date/period validity, order of precedence, calculation)", "notes")


## Read in original, corrupt, V18 file, the corrected v18_v2 that has more conceptids in, and v21 which we will merge into later

  refset_v18 <- fread("D:/reference_data/priest_gdppr_cluster_refset_2021-02-17 135433_V18.csv", colClasses = "character")
  refset_v18.2 <- fread("D:/reference_data/priest_gdppr_cluster_refset_2021-02-17 135433_V18_v2.csv", colClasses = "character")
  refset_v21 <- fread("D:/reference_data/priest_gdppr_cluster_refset_2021-02-17 135433_V21.csv", colClasses = "character")


## Remove special character from cluster description

  refset_v18[, cluster_desc_special_char_removed := gsub("[^A-Za-z0-9\\(\\) ]+", "", cluster_desc)]


# Merge broken refset v18 with original download of clusters interested in so can merge by row number to keep notes and comorbidity ------------

  ## Read in gdppr clusters, and if they were requested for priest from saved file that was pulled from Google Drive, and remove special chars

    gdppr_clusters_selection <- fread("D:/reference_data/gdppr_cluster_names_and_if_requested_for_priest.csv")

    gdppr_clusters_selection[, dars_code_groups := gsub("[^A-Za-z0-9\\(\\) ]+", "", dars_code_groups)]

    stopifnot(gdppr_clusters_selection[, .N, by = dars_code_groups][N > 1, .N] == 0)


  ## Create row number for v18, as the merge will change order, so can reset to enable merge with marked-up version

    refset_v18[, row_number := 1:.N]


  ## Merge reference table v18 with the clusters that were made available for priest
  ## have put all.x back as for v18 so is kept the same as the marked-up file (otherwise 3 clusters not in v18 that were not of interest would create a row)

    refset_v18_cluster_requested_bool <- merge(refset_v18,
                                               gdppr_clusters_selection,
                                               by.x = "cluster_desc_special_char_removed",
                                               by.y = "dars_code_groups",
                                               all.x = TRUE)



# Merge clean v18 and marked-up v18 to get conceptids including scientific notation --------

  ## Need to have both notes and comorbidity classification, and bad scientific conceptids, so merge V18 and marked-up table on row number once created

    refset_v18_priest_clusters <- refset_v18_cluster_requested_bool[for_priest == "Y"]

    stopifnot(refset_v18_priest_clusters[, .N] == gdppr_snomed_codes_comorbs[, .N])


  ## Reset order of v18

    setorder(refset_v18_priest_clusters, row_number)

    refset_v18_priest_clusters[, row_number := 1:.N]
    gdppr_snomed_codes_comorbs[, row_number := 1:.N]


    refset_v18_priest_clusters_notes <- merge(gdppr_snomed_codes_comorbs[, .(row_number, notes, conceptid_description_marked = conceptid_description,
                                                                             comorbidity_classification, cluster_id_marked = cluster_id)],
                                              refset_v18_priest_clusters[, .(row_number, conceptid, conceptid_description, cluster_id)],
                                              by = "row_number",
                                              all = TRUE)[, row_number := NULL]

    stopifnot(refset_v18_priest_clusters_notes[cluster_id != cluster_id_marked, .N] == 0)
    refset_v18_priest_clusters_notes[, cluster_id_marked := NULL]


  ## Strip out special characters

    refset_v18_priest_clusters_notes[, ':=' (conceptid_description_special_chars_removed = gsub("[^A-Za-z0-9\\(\\) ]+", "", conceptid_description),
                                             conceptid_description_marked = gsub("[^A-Za-z0-9\\(\\) ]+", "", conceptid_description_marked))]

    stopifnot(refset_v18_priest_clusters_notes[, .N] == refset_v18_priest_clusters[, .N])
    stopifnot(refset_v18_priest_clusters_notes[conceptid_description_marked != conceptid_description_special_chars_removed
                                               | is.na(conceptid_description_marked)
                                               | is.na(conceptid_description_special_chars_removed), .N] == 0)


# Split V18 into 3 tables dependent on conceptid, conceptid_description --------
## In the following code block, the original, broken, v18 reference table will be split into 3 different tables, which will be named as follows;
# A - Table that contains all conceptids that are not in scientific notation, this will be checked that they are real conceptids
# B - Table that contains conceptids that are in scientific notation, and their description appear in v18.2, these will be linked on description to v18.2
# C - Table that contains conceptids that are in scientific notation, but their description does not appear in v18.2, these will be linked on description to a full SNOMED CT table


# Table A -----------------------------------------------------------------

  ## Get all conceptids and notes from markeup data that do not have scientific notation

    refset_v18_notes_table_a <- refset_v18_priest_clusters_notes[!(grepl("E", conceptid, fixed = TRUE)), .(conceptid, notes,
                                                                                                           comorbidity_classification, cluster_id)]


  ## Check that these conceptids are real ids, read in full SNOMED TRUD CT, includes int, uk and drug conceptids

    snomed_all_concepts <- fread("D:/reference_data/full_trud_snomed_concepts_uk_int_drug_2021-02-12 152010.csv", colClasses = "character")

    stopifnot(refset_v18_notes_table_a[!(conceptid %chin% snomed_all_concepts$conceptid), .N] == 0)


# Table B -----------------------------------------------------------------

  ## Get all scientific notation conceptids we are interested in that have descriptions in both V18 and V18_v2

    marked_refset_scientific_v18.2_label_conceptids <- refset_v18_priest_clusters_notes[grepl("E", conceptid, fixed = TRUE), .N, by = .(conceptid_description, conceptid)][conceptid_description %chin% refset_v18.2$conceptid_description, conceptid]

    marked_refset_scientific_v18.2_label <- refset_v18_priest_clusters_notes[conceptid %chin% marked_refset_scientific_v18.2_label_conceptids, .(conceptid_description, notes, comorbidity_classification, cluster_id)]

    refset_v18_notes_table_b <- unique(merge(marked_refset_scientific_v18.2_label,
                                             refset_v18.2[, .(conceptid, conceptid_description)],
                                             by = "conceptid_description",
                                             all.x = TRUE)[, .(conceptid, notes, comorbidity_classification, cluster_id)])

    stopifnot(refset_v18_notes_table_b[is.na(conceptid), .N] == 0)


# Table C -----------------------------------------------------------------

  ## Get all scientific notation conceptids we are interested in that appear in v18 but do not have a matching description in v18_v2

    marked_refset_scientific_no_v18.2_label_conceptids <- refset_v18_priest_clusters_notes[grepl("E", conceptid, fixed = TRUE), .N, by = .(conceptid_description, conceptid)][!(conceptid_description %chin% refset_v18.2$conceptid_description), conceptid]

    marked_refset_scientific_no_v18.2_label <- refset_v18_priest_clusters_notes[conceptid %chin% marked_refset_scientific_no_v18.2_label_conceptids, .(conceptid_description, notes, comorbidity_classification, cluster_id)]

    refset_v18_notes_table_c <- unique(merge(marked_refset_scientific_no_v18.2_label,
                                             snomed_all_concepts[, .(conceptid, term)],
                                             by.x = "conceptid_description",
                                             by.y = "term",
                                             all.x = TRUE)[, .(conceptid, notes, comorbidity_classification, cluster_id)])

    stopifnot(refset_v18_notes_table_c[is.na(conceptid), .N] == 0)


## Bind together all 3 tables to gain a correct V18

  refset_v18_notes_corrected <- unique(rbind(refset_v18_notes_table_a,
                                             refset_v18_notes_table_b,
                                             refset_v18_notes_table_c))



# Tidy up environment ------------------------------------------------------

  rm(list=setdiff(ls(), c("refset_v18_notes_corrected", "refset_v18", "refset_v21", "gdppr_snomed_codes_comorbs", "gdppr_clusters_selection",
                          "gdppr_v21_with_notes")))


# Read in GDPPR data -----------------------------------------------------

  ## Read in processed GDPPR data

    demo_file_id <- "FILE0115974_2021-03-18-163613"
    gdppr_data <- readRDS(file = paste0("data/datasets/cohort_", demo_file_id, "_gdppr_data.rds"))



# Prepare Refset v21 ------------------------------------------------------

  # Check both v18 and v21 have same cluster_id - cluster_desc pairs, replace v18 with v21 desc if they are not

    refset_v18_cluster_desc_v21_changes <- merge(refset_v18,
                                                 unique(refset_v21[, .(cluster_id, cluster_desc_v21 = cluster_desc)]),
                                                 by = "cluster_id",
                                                 all = FALSE)



    refset_v18_cluster_desc_v21_changes[!is.na(cluster_desc) & !is.na(cluster_desc_v21) & cluster_desc != cluster_desc_v21, cluster_desc := cluster_desc_v21]

    stopifnot(refset_v18_cluster_desc_v21_changes[!is.na(cluster_desc) & !is.na(cluster_desc_v21) & cluster_desc != cluster_desc_v21, .N] == 0)

    refset_v18_cluster_desc_v21_changes[, cluster_desc_v21 := NULL]


## Multiple conceptid - conceptid_desc exist for 16 conceptids, giving each conceptid one description only

    setorder(refset_v21, conceptid_description)

    conceptid_canonical_desc <- copy(refset_v21[, row_order := 1:.N, by = conceptid][row_order == 1, .(conceptid, conceptid_description)])

    refset_v21[, c("row_order", "conceptid_description") := NULL]

    stopifnot(conceptid_canonical_desc[is.na(conceptid), .N] == 0)

    refset_v21_canonical_desc <- merge(refset_v21,
                                       conceptid_canonical_desc,
                                       by = "conceptid",
                                       all.x = TRUE)

## Save this version of refset v21 as needed later when joining with GDPPR data as have limited it down to just one desc per conceptid

    fwrite(conceptid_canonical_desc, file = "D:/reference_data/refset_v21_canonical_desc.csv")


# Merge all concepts from v18 into v21 ------------------------------------

  ## Merge v18 notes into v21 keeping everything

    gdppr_v18_v21_with_notes <- merge(refset_v18_notes_corrected[, from_v18 := TRUE],
                                      refset_v21_canonical_desc[, from_v21 := TRUE],
                                      by = "conceptid",
                                      all = TRUE)


  ## Create summary table that shows concepts that were only in one of either v18 or v21

    gdppr_v18_v21_with_notes_concept_summary <- gdppr_v18_v21_with_notes[, .(concept_v21_only = all(is.na(from_v18)), concept_v18_only = all(is.na(from_v21))), by = conceptid]


  ## Check if these appear in GDPPR data, no codes only in v18 appear (some codes only in v21 do appear in gdppr data)

    stopifnot(gdppr_data[CODE %chin% gdppr_v18_v21_with_notes_concept_summary[concept_v18_only == TRUE, conceptid], .N] == 0)


  ## Remove concepts that appear only in v18, they do not appear in GDPPR data so are not needed

    gdppr_v21_with_notes <- gdppr_v18_v21_with_notes[!(conceptid %chin% gdppr_v18_v21_with_notes_concept_summary[concept_v18_only == TRUE, conceptid])]



# Create final conceptid table for new markup -----------------------------

  ## Use corrected v18 data
  ## Find all unique cluster_id - cluster_desc pairs, and merge into corrected v18, check everything has a cluster_desc

    refset_v18_cluster_id_desc <- unique(refset_v18_cluster_desc_v21_changes[, .(cluster_id, cluster_desc)])

    refset_v18_notes_corrected_cluster_desc <- merge(refset_v18_notes_corrected[, .(conceptid, notes, comorbidity_classification, cluster_id)],
                                                     refset_v18_cluster_id_desc,
                                                     by = "cluster_id",
                                                     all.x = TRUE)

    stopifnot(refset_v18_notes_corrected_cluster_desc[is.na(cluster_desc), .N] == 0)


  ## Keep only conceptids that are in reduced v21

    refset_v18_notes_corrected_cluster_desc <- refset_v18_notes_corrected_cluster_desc[conceptid %chin% unique(gdppr_v21_with_notes[, conceptid])]


  ## Create col of all v18 cluster names for each conceptid

    refset_v18_notes_corrected_cluster_desc[, v18_all_cluster_descs := paste(sort(unique(cluster_desc)), collapse = ";\n"), by = conceptid]

    refset_v18_notes_corrected_cluster_desc_unique <- unique(refset_v18_notes_corrected_cluster_desc[, .(conceptid, notes, comorbidity_classification, v18_all_cluster_descs)])

    setorder(refset_v18_notes_corrected_cluster_desc_unique, comorbidity_classification, notes)

    refset_v18_notes_corrected_cluster_desc_unique[, comorb_notes_order := 1:.N, by = conceptid]


  ## Cast to one line per conceptid with all notes and comorbs in one col

    refset_v18_notes_by_conceptid <- dcast(refset_v18_notes_corrected_cluster_desc_unique,
                                           conceptid + v18_all_cluster_descs ~ comorb_notes_order,
                                           value.var = c("comorbidity_classification", "notes"),
                                           fill = NA)


    full_cluster_list_address <- as.character(as_sheets_id("https://docs.google.com/spreadsheets/d/1p6V2knw2ieGuh13xpKPDl7aoWz39QK0466oWsUrkehk/edit#gid=272979167"))

    full_cluster_list <- data.table(range_read(full_cluster_list_address,
                                               sheet = "full_gdppr_clusters_selection_with_ids",
                                               col_types = "c"))


    ## Merge into clean v21

      refset_v21_cluster_requested_bool <- merge(refset_v21_canonical_desc,
                                                 full_cluster_list[, .(cluster_id, for_priest)],
                                                 by = "cluster_id",
                                                 all = TRUE)


  ## Only want conceptids in gdppr_v21_with_notes, in clusters of interest in v21 (v18+1), and in GDPPR

      gdppr_v21_reduced <- refset_v21_canonical_desc[conceptid %chin% gdppr_v21_with_notes[, conceptid]
                                      & conceptid %chin% refset_v21_cluster_requested_bool[for_priest == "Y", conceptid]
                                      & conceptid %chin% gdppr_data[, CODE],
                                      .(cluster_desc, conceptid, conceptid_description)]


  ## Create col of all v21 cluster names for each conceptid

    gdppr_v21_reduced_clusters_by_conceptid <- gdppr_v21_reduced[, .(v21_all_cluster_descs = paste(sort(unique(cluster_desc)), collapse = ";\n")),
                                                                 by = .(conceptid, conceptid_description)]


  ## Merge v18 and v21 data

    v18_v21_multiple_cluster_comorbs_notes <- merge(gdppr_v21_reduced_clusters_by_conceptid,
                                                    refset_v18_notes_by_conceptid,
                                                    by = "conceptid",
                                                    all.x = TRUE)


  ## Create col to see is cluster descs have changed - highlight new and changes to v21

    v18_v21_multiple_cluster_comorbs_notes[v18_all_cluster_descs == v21_all_cluster_descs, cluster_change_v18_to_v21 := "N"]
    v18_v21_multiple_cluster_comorbs_notes[!is.na(v18_all_cluster_descs) & v18_all_cluster_descs != v21_all_cluster_descs, cluster_change_v18_to_v21 := "Y"]
    v18_v21_multiple_cluster_comorbs_notes[is.na(v18_all_cluster_descs), cluster_change_v18_to_v21 := "Y"]

    stopifnot(v18_v21_multiple_cluster_comorbs_notes[is.na(cluster_change_v18_to_v19), .N] == 0)


  ## Put in row number in case of panic

    v18_v21_multiple_cluster_comorbs_notes[, row_number := 1:.N]


  ## Reorder cols comorb1, notes1, comorb2, notes2

    setcolorder(v18_v21_multiple_cluster_comorbs_notes, c("row_number", "cluster_change_v18_to_v19", "conceptid", "conceptid_description", "v18_all_cluster_descs", "v21_all_cluster_descs",
                                                            "comorbidity_classification_1", "notes_1", "comorbidity_classification_2", "notes_2",
                                                            "comorbidity_classification_3", "notes_3", "comorbidity_classification_4", "notes_4"))

    setorder(v18_v21_multiple_cluster_comorbs_notes, v21_all_cluster_descs, conceptid_description, conceptid)


  ## Check all concepts in v18 are in v21

    stopifnot(v18_v21_multiple_cluster_comorbs_notes[!is.na(v18_all_cluster_descs) & is.na(v21_all_cluster_descs), .N] == 0)


  ## Save file, and then create doc in Google shared drive with NAs replaced with blanks

    save_time <- gsub(":", "", Sys.time(), fixed = TRUE)

    fwrite(v18_v21_multiple_cluster_comorbs_notes, file = paste0("D:/reference_data/v18_v21_multiple_cluster_comorbs_notes_", save_time, ".csv"))


    sheet_write(v18_v21_multiple_cluster_comorbs_notes,
                ss = "https://docs.google.com/spreadsheets/d/154Els3dL0cwlQTLuX4P72Jgkh8YfIK7-PKD_5lk4yV0/edit#gid=0",
                sheet = "v18_v21_multiple_cluster_comorbs_notes")


# Create ref sheets that contain all notes and comorbs used ---------------

## All lower case, remove blanks, drops, and order


  all_comorbs_used <- unique(gdppr_snomed_codes_comorbs[, .(comorbidity_classification)])

  all_notes_used <- unique(gdppr_snomed_codes_comorbs[, .(notes)])

  all_comorbs_used[, comorbidity_classification := fn_removeBlanks(comorbidity_classification)]
  all_notes_used[, notes := fn_removeBlanks(notes)]

  all_comorbs_used[, comorbidity_classification := tolower(comorbidity_classification)]
  all_notes_used[, notes := tolower(notes)]

  all_comorbs_used <- all_comorbs_used[!is.na(comorbidity_classification) & comorbidity_classification != "drop"]
  all_notes_used <- all_notes_used[!is.na(notes) & notes != "drop" & notes != "na"]


  sheet_write(all_comorbs_used,
              ss = "https://docs.google.com/spreadsheets/d/1UbL7Eiz_buSKySWvlOxzjS9xubOEcZKs6P3sCcxmyB4/edit#gid=0",
              sheet = "all_comorbs_used")

  sheet_write(all_notes_used,
              ss = "https://docs.google.com/spreadsheets/d/1UbL7Eiz_buSKySWvlOxzjS9xubOEcZKs6P3sCcxmyB4/edit#gid=0",
              sheet = "all_notes_used")







