## Script to check codes that we recieved in GDPPR data to what was requested

library(data.table)
library(googlesheets4)
library(readxl)

# Read in data needed -----------------------------------------------------

## Read in processed GDPPR data

  demo_file_id <- "FILE0115974_2021-03-18-163613"

  gdppr_data <- readRDS(file = paste0("data/datasets/cohort_", demo_file_id, "_gdppr_data.rds"))

  distinct_gdppr_concepts <- gdppr_data[, .N, by = CODE]


## Read in complete TRUD SNOMED conceptid table

  snomed_all_concepts <- fread("D:/reference_data/full_trud_snomed_concepts_uk_int_drug_2021-02-12 152010.csv", colClasses = "character")
  snomed_uk_concepts <- fread("D:/reference_data/full_trud_snomed_concepts_uk_drug_2021-02-12 152010.csv", colClasses = "character")


## Read in V18.0, V20.0, V 21.0 support data - done processing  (lower case names etc.), and make a one with priest requested codes only

  gdppr_cluster_refset_v18 <- fread("D:/reference_data/priest_gdppr_cluster_refset-2021-02-08 120248V18.csv", colClasses = "character")
  gdppr_cluster_refset_v18_v2 <- fread("D:/reference_data/priest_gdppr_cluster_refset-2021-02-10 101422V18_v2.csv", colClasses = "character")
  gdppr_cluster_refset_v20 <- fread("D:/reference_data/priest_gdppr_cluster_refset-2021-02-08 120248V20.csv", colClasses = "character")
  gdppr_cluster_refset_v21 <- fread("D:/reference_data/priest_gdppr_cluster_refset-2021-02-08 120248V21.csv", colClasses = "character")


## Distinct number of conceptids for each version, including full version

  V18_distinct_concepts <- unique(gdppr_cluster_refset_v18[, .(conceptid, priest_requested_cluster)], by = "conceptid")
  V18_v2_distinct_concepts <- unique(gdppr_cluster_refset_v18_v2[, .(conceptid, priest_requested_cluster)], by = "conceptid")
  V20_distinct_concepts <- unique(gdppr_cluster_refset_v20[, .(conceptid, priest_requested_cluster)], by = "conceptid")
  V21_distinct_concepts <- unique(gdppr_cluster_refset_v21[, .(conceptid, priest_requested_cluster)], by = "conceptid")
  snomed_uk_concepts_unique <- unique(snomed_uk_concepts[, .(conceptid, term)], by = "conceptid")
  snomed_all_concepts_unique <- unique(snomed_all_concepts[, .(conceptid, term)], by = "conceptid")


  V18_distinct_priest_concepts <- V18_distinct_concepts[priest_requested_cluster == TRUE]
  V18_v2_distinct_priest_concepts <- V18_v2_distinct_concepts[priest_requested_cluster == TRUE]
  V20_distinct_priest_concepts <- V20_distinct_concepts[priest_requested_cluster == TRUE]
  V21_distinct_priest_concepts <- V21_distinct_concepts[priest_requested_cluster == TRUE]


## Do all none scientific notation conceptids appear in the full list of conceptids

  v18_in_uk_concept <- V18_distinct_concepts[!grepl("E", conceptid, fixed = TRUE)][, real_concept := conceptid %chin% snomed_uk_concepts_unique$conceptid]
  v18_in_all_concept <- V18_distinct_concepts[!grepl("E", conceptid, fixed = TRUE)][, real_concept := conceptid %chin% snomed_all_concepts_unique$conceptid]

  v18_in_uk_concept[, .N, by = real_concept]
  v18_in_all_concept[, .N, by = real_concept]

  v20_in_all_concept <- V20_distinct_concepts[, real_concept := conceptid %chin% snomed_all_concepts_unique$conceptid]

  v20_in_all_concept[, .N, by = real_concept]


## How many codes in the GDPPR data match those in each refset

  V18_matched_concepts <- distinct_gdppr_concepts[CODE %in% V18_distinct_concepts$conceptid]
  V18_matched_priest_concepts <- distinct_gdppr_concepts[CODE %in% V18_distinct_priest_concepts$conceptid]
  V18_not_matched_concepts <- distinct_gdppr_concepts[!(CODE %in% V18_distinct_concepts$conceptid)]

  V18_v2_matched_concepts <- distinct_gdppr_concepts[CODE %in% V18_v2_distinct_concepts$conceptid]
  V18_v2_matched_priest_concepts <- distinct_gdppr_concepts[CODE %in% V18_v2_distinct_priest_concepts$conceptid]
  V18_v2_not_matched_concepts <- distinct_gdppr_concepts[!(CODE %in% V18_v2_distinct_concepts$conceptid)]

  V20_matched_concepts <- distinct_gdppr_concepts[CODE %in% V20_distinct_concepts$conceptid]
  V20_matched_priest_concepts <- distinct_gdppr_concepts[CODE %in% V20_distinct_priest_concepts$conceptid]
  V20_not_matched_concepts <- distinct_gdppr_concepts[!(CODE %in% V20_distinct_concepts$conceptid)]

  V21_matched_concepts <- distinct_gdppr_concepts[CODE %in% V21_distinct_concepts$conceptid]
  V21_matched_priest_concepts <- distinct_gdppr_concepts[CODE %in% V21_distinct_priest_concepts$conceptid]
  V21_not_matched_concepts <- distinct_gdppr_concepts[!(CODE %in% V21_distinct_concepts$conceptid)]


## Version comparison table

  version_comparison <- data.table(version = "V18", distinct_concepts = V18_distinct_concepts[,.N], matched_concepts = V18_matched_concepts[,.N], unmatached_concepts = V18_not_matched_concepts[,.N],
                                   matched_records = V18_matched_concepts[, sum(N)], unmatched_records = V18_not_matched_concepts[, sum(N)], priest_matched_records = V18_matched_priest_concepts[, sum(N)])

  version_comparison <- rbind(version_comparison, list("V18_2", V18_v2_distinct_concepts[,.N], V18_v2_matched_concepts[,.N], V18_v2_not_matched_concepts[,.N],
                                                       V18_v2_matched_concepts[, sum(N)], V18_v2_not_matched_concepts[, sum(N)], V18_v2_matched_priest_concepts[, sum(N)]))

  version_comparison <- rbind(version_comparison, list("V20", V20_distinct_concepts[,.N], V20_matched_concepts[,.N], V20_not_matched_concepts[,.N],
                                                       V20_matched_concepts[, sum(N)], V20_not_matched_concepts[, sum(N)], V20_matched_priest_concepts[, sum(N)]))

  version_comparison <- rbind(version_comparison, list("V21", V21_distinct_concepts[,.N], V21_matched_concepts[,.N], V21_not_matched_concepts[,.N],
                                                       V21_matched_concepts[, sum(N)], V21_not_matched_concepts[, sum(N)], V21_matched_priest_concepts[, sum(N)]))


## Difference between versions in clusters we are interested in that are found in the GDPPR data

  v18_to_v18_2 <- V18_v2_matched_concepts[!(CODE %chin% V18_matched_concepts$CODE)]
  V18_v2_distinct_concepts[conceptid %chin% v18_to_v18_2$CODE][, .N, by = priest_requested_cluster]


  v18_to_v20 <- V20_matched_concepts[!(CODE %chin% V18_matched_concepts$CODE)]
  V20_distinct_concepts[conceptid %chin% v18_to_v20$CODE][, .N, by = priest_requested_cluster]


## Create list of the new conceptids that we are interested in, that are found in later versions for which there are GDPPR records
##  and isolate from most recent version

  conceptids_from_ref_for_markup <- gdppr_cluster_refset_v20[priest_requested_cluster == TRUE][conceptid %chin% v18_to_v20$CODE][, priest_requested_cluster := NULL]


## Write out this file if needed for markup

  save_date <- Sys.Date()

  write.csv(conceptids_from_ref_for_markup, paste0(file = "D:/reference_data/concepts_of_interest_from_newer_ref_dataset_text_", save_date,".txt"))







## Saved from other file to check what this was looking at #####
  # Merge GDPPR data and snomed ref data ------------------------------------

  # priest_cluster_refset_v18[, .N, by = .(conceptid, conceptid_description)][, .N, by = conceptid][N > 1]
  #
  # test <- priest_cluster_refset_v18[, unique(conceptid, conceptid_description)]
  #
  # gdppr_conceptid_check <- merge(gdppr_data[, .(CODE)],
  #                                priest_cluster_refset_v18[, .N, by = conceptid],
  #                                by.x = "CODE",
  #                                by.y = "conceptid",
  #                                all.x = TRUE)
  #
  # stopifnot(gdppr_conceptid_check[is.na(CODE) & is.na(N), .N] == 0)
  #
  # gdppr_data[CODE %in% gdppr_cluster_refset_v18$conceptid, .N]

