## Script for reading in table of clusters we are interested in for both v18 and v21 addition and binding
## Check all GDPPR codes and that they all appear in refset v21 and how many in clusters we are interested in

library(data.table)
library(googlesheets4)
library(readxl)


## Read in GDPPR data - shouldn't really use this identifier as created in later script, but want GDPPR data with pre-processing done to it

  demo_file_id <- "FILE0115974_2021-03-18-163613"
  gdppr_data <- readRDS(file = paste0("data/datasets/cohort_", demo_file_id, "_gdppr_data.rds"))


## Only keep cols that are interested in

  gdppr_data[, c("EHNIC", "GP_SYSTEM_SUPPLIER", "PROCESSED_TIMESTAMP", "EPISODE_CONDITION", "EPISODE_PRESCRIPTION") := NULL]


## Read in original v18.2 and  v21 of refset data, get unique list of clusters and ids

  refset_v21 <- fread("D:/reference_data/priest_gdppr_cluster_refset_2021-02-17 135433_V21.csv", colClasses = "character")

  refset_v21_clusters <- unique(refset_v21[, .(cluster_id, cluster_desc)])

  refset_v21_clusters[, cluster_desc_special_char_removed := gsub("[^A-Za-z0-9\\(\\) ]+", "", cluster_desc)]


## Read in clusters used for DARS application, and refset used to create clusters to get to Y/N and cluster_id

  nhs_digital_requested_data <- as.character(as_sheets_id("https://docs.google.com/spreadsheets/d/13uCMRrDrzfzh5vgI6gxoD-gFHk_ZwZvDDi5F_HxkHXw/edit#gid=1872012902"))

  dars_app_clusters <- data.table(range_read(nhs_digital_requested_data,
                                             sheet = "NHS Digital DARS",
                                             col_types = "c"))

  dars_app_ref <- data.table(range_read(nhs_digital_requested_data,
                                        sheet = "Expanded Clusters - FOR REF. ONLY",
                                        col_types = "c"))

  dars_app_ref_reduced <- unique(dars_app_ref[, .(Cluster_Desc, Cluster_ID)])

  setnames(dars_app_clusters, gsub(" ", "_", colnames(dars_app_clusters), fixed = TRUE))
  setnames(dars_app_ref_reduced, gsub(" ", "_", colnames(dars_app_ref_reduced), fixed = TRUE))

  setnames(dars_app_clusters, make.names(tolower(colnames(dars_app_clusters)), unique = TRUE))
  setnames(dars_app_ref_reduced, make.names(tolower(colnames(dars_app_ref_reduced)), unique = TRUE))

  dars_app_clusters[, dars_code_groups := gsub("[^A-Za-z0-9\\(\\) ]+", "", dars_code_groups)]
  dars_app_ref_reduced[, cluster_desc := gsub("[^A-Za-z0-9\\(\\) ]+", "", cluster_desc)]


## Read in additional clusters that were available in refset v21, created in A05, and then marked if of interest

  additional_clusters_v21_to_dars <- as.character(as_sheets_id("https://docs.google.com/spreadsheets/d/1Vb6IarEVfGs_hSdNgwjocb8QqOH0F7qdVBhKBMN5lFE/edit#gid=119863370"))

  v21_additional_clusters <- data.table(range_read(additional_clusters_v21_to_dars, sheet = "additional_clusters_v21_to_dars_info_2021-02-17 145154",
                                                   col_types = "c"))


## Merge in refset v18.2 so can have cluster ids as well

  gdppr_clusters_selection_with_ids <- unique(merge(dars_app_clusters[, .(for_priest, dars_code_groups)],
                                                    dars_app_ref_reduced,
                                                    by.x = "dars_code_groups",
                                                    by.y = "cluster_desc",
                                                    all = TRUE))


## Check that all clusters, that have not been sent for re-marking, have a  Y/N for them

  stopifnot(gdppr_clusters_selection_with_ids[!(cluster_id %chin% v21_additional_clusters$cluster_id) & (is.na(for_priest)), .N == 0])


## Check all clusters that have a Y/N now have a cluster_id

  gdppr_clusters_selection_with_ids[!is.na(for_priest) & is.na(cluster_id)]


## Remove original one and bind new ones so we don't have two for_priest cols

  gdppr_clusters_selection_with_ids <- gdppr_clusters_selection_with_ids[!(cluster_id %chin% v21_additional_clusters$cluster_id)]

  full_gdppr_clusters_selection_with_ids <- rbind(gdppr_clusters_selection_with_ids[, .(cluster_id, for_priest)],
                                                  v21_additional_clusters[, .(cluster_id, for_priest)],
                                                  use.names = TRUE)


  stopifnot(full_gdppr_clusters_selection_with_ids[, .N, by = cluster_id][N > 1, .N] == 0)
  stopifnot(full_gdppr_clusters_selection_with_ids[, .N] == (gdppr_clusters_selection_with_ids[, .N] + v21_additional_clusters[, .N]))
  stopifnot(full_gdppr_clusters_selection_with_ids[is.na(for_priest), .N] == 0)


## Merge into v21 to get extra cols

  full_gdppr_clusters_selection_with_ids_v21 <- merge(unique(refset_v21[, .(cluster_id, cluster_category ,cluster_desc)]),
                                                      full_gdppr_clusters_selection_with_ids,
                                                      by = "cluster_id",
                                                      all.x = TRUE)


  stopifnot(full_gdppr_clusters_selection_with_ids_v21[is.na(for_priest), .N] == 0)
  stopifnot(full_gdppr_clusters_selection_with_ids_v21[, .N, by = cluster_id][N > 1, .N] == 0)


## Save table to reference data folder and to google drive

  setorder(full_gdppr_clusters_selection_with_ids_v21, cluster_category, cluster_id, cluster_desc)

  save_time <- gsub(":", "", Sys.time(), fixed = TRUE)

  fwrite(full_gdppr_clusters_selection_with_ids, file = paste0("D:/reference_data/gdppr_cluster_names_and_for_priest_v21_additions_final_", save_time, ".csv"))

## Commented out google write as could overwrite needed information - and changed sheet name just in case

  # sheet_write(full_gdppr_clusters_selection_with_ids_v21,
  #             ss = "https://docs.google.com/spreadsheets/d/1p6V2knw2ieGuh13xpKPDl7aoWz39QK0466oWsUrkehk/edit#gid=0",
  #             sheet = "A different sheet")


# Check the number of conceptids of clusters of interest from v21 that appear in GDPPR data ----------------

  ## Merge in if interested in cluster to whole refset_v21

  refset_v21_with_cluster_interest <- merge(refset_v21,
                                            full_gdppr_clusters_selection_with_ids[, .(cluster_id, for_priest)],
                                            by = "cluster_id",
                                            all.x = TRUE)

  ## Want to look at number for later reference
  gdppr_data[CODE %chin% refset_v21_with_cluster_interest[for_priest == "Y", conceptid], .N, by = CODE]







