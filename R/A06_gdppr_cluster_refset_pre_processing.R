## Script for processing GDPPR reference data
## Looks at the clusters that were selected for PRIEST in the dars application and checks if these are present in v21 of reference data
## Also looks at clusters that are in v21 reference data but were not available for selection in the dars application

library(data.table)
library(googlesheets4)
library(readxl)


## Read in original v18 and v21 of refset data

  refset_v18 <- fread("D:/reference_data/priest_gdppr_cluster_refset_2021-02-17 135433_V18.csv", colClasses = "character")
  refset_v21 <- fread("D:/reference_data/priest_gdppr_cluster_refset_2021-02-17 135433_V21.csv", colClasses = "character")


## Remove special character from cluster description

  refset_v18[, cluster_desc_special_removed := gsub("[^A-Za-z0-9\\(\\) ]+", "", cluster_desc)]
  refset_v21[, cluster_desc_special_removed := gsub("[^A-Za-z0-9\\(\\) ]+", "", cluster_desc)]


# Check that all clusters that we asked for are in the refsets ------------

  ## Read in gdppr cluster selection, and if they were requested for priest - from saved file that was pulled from Google Drive

    gdppr_clusters_selection <- fread("D:/reference_data/gdppr_cluster_names_and_if_requested_for_priest.csv")

    gdppr_clusters_selection[, dars_code_groups := gsub("[^A-Za-z0-9\\(\\) ]+", "", dars_code_groups)]


  ## Merge reference table v21 with the clusters that were made available for priest

    refset_v21_cluster_requested_bool <- merge(refset_v21,
                                               gdppr_clusters_selection,
                                               by.x = "cluster_desc_special_removed",
                                               by.y = "dars_code_groups",
                                               all = TRUE)



    refset_v21_cluster_requested_bool_summary <- refset_v21_cluster_requested_bool[, .(refset_only = all(is.na(for_priest)), cluster_selection_only = all(is.na(conceptid))), by = cluster_desc_special_removed]


  ## What clusters are in refset v21 but were not in DARS options for selection

    additional_clusters_v21_to_dars <- refset_v21_cluster_requested_bool_summary[refset_only == TRUE, cluster_desc_special_removed]

    additional_clusters_v21_to_dars_info <- unique(refset_v21_cluster_requested_bool[cluster_desc_special_removed %chin% additional_clusters_v21_to_dars,
                                                                                     .(cluster_id, cluster_desc, cluster_category)])


  ## Check that v21 has all the clusters that were requested

    additional_clusters_dars_to_v21 <- refset_v21_cluster_requested_bool_summary[cluster_selection_only == TRUE, cluster_desc_special_removed]

    additional_clusters_dars_to_v21_for_priest <- refset_v21_cluster_requested_bool[cluster_desc_special_removed %chin% additional_clusters_dars_to_v21 & for_priest == "Y",
                                                                                   .(for_priest, cluster_desc_special_removed)]

    additional_clusters_dars_to_v21_info <- unique(refset_v18[cluster_desc %chin% additional_clusters_dars_to_v21_for_priest,
                                                              .(cluster_id, cluster_desc, cluster_category)])

  ## Write cluster information for both addition sets

    save_time <- gsub(":", "", Sys.time(), fixed = TRUE)

    fwrite(additional_clusters_v21_to_dars_info, paste0("D:/reference_data/additional_clusters_v21_to_dars_info_", save_time, ".csv"))
    fwrite(additional_clusters_dars_to_v21_info, paste0("D:/reference_data/additional_clusters_dars_to_v21_info_", save_time, ".csv"))




