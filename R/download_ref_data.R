source("R/cleaning_fns_etl.r")

library(data.table)
library(googlesheets4)
library(usethis)
library(readxl)

## Get id of google sheet

  priest_ED_sites_id <- as.character(as_sheets_id("https://docs.google.com/spreadsheets/d/1ZWzuqwi6OeEgozsNdsje8w1BN-YU0eqKQ-hK7aYcjpI/edit#gid=1162259306"))
  priest_gdppr_dars <- as.character(as_sheets_id("https://docs.google.com/spreadsheets/d/13uCMRrDrzfzh5vgI6gxoD-gFHk_ZwZvDDi5F_HxkHXw/edit#gid=1872012902"))


## Read in sites sheet from google drive

  priest_sites <- data.table(range_read(priest_ED_sites_id,
                             sheet = "sites (live)"))[Country == "England"]

  gdppr_clusters_selection <- unique(data.table(range_read(priest_gdppr_dars,
                                                 sheet = "NHS Digital DARS"))[, "DARS Broad Groupings" := NULL])


## Standardise col names of both sheets

  setnames(priest_sites, gsub(".", "_", colnames(priest_sites), fixed = TRUE))

  setnames(priest_sites, make.names(tolower(colnames(priest_sites)), unique = TRUE))

  setnames(gdppr_clusters_selection, gsub(" ", "_", colnames(gdppr_clusters_selection), fixed = TRUE))

  setnames(gdppr_clusters_selection, make.names(tolower(colnames(gdppr_clusters_selection)), unique = TRUE))


## Remove apostrophes in cluster names

  gdppr_clusters_selection[, dars_code_groups := gsub("'", "", dars_code_groups)]


## Get unique table of code cluster for GDPPR - Body mass index (BMI) codes appears twice

  gdppr_clusters_selection <- unique(gdppr_clusters_selection)


## Write out cluster names for use later

  fwrite(gdppr_clusters_selection, file = "D:/reference_data/gdppr_cluster_names_and_if_requested_for_priest.csv")


## Download GDPPR cluster refset from TRUD (v18.0 - one used for mark up notes for comorbidities)

  ## V18.0 as that was one that was marked up for comorbs

  cluster_reference_address_trud_18 <- "https://isd.digital.nhs.uk/trud3/api/v1/keys/4149673aafc1a67390597b8f681b4d6a3660968b/files/SCT_PC/18.0.0/UKPC_SCT2/ukpc_sct2_18.0.0_20200909000001.zip"

  temp <- tempfile(fileext = ".zip")
  download.file(cluster_reference_address_trud_18, temp, mode = "wb")
  files <- unzip(temp, exdir = "D:/reference_data/temp/primary_care_clinical_reference_sets")
  #files2 <- unzip("D:/reference_data/temp/primary_care_clinical_reference_sets/ukpc_sct2_18.0.0_20200909.zip",
  #                exdir = "D:/reference_data/temp/primary_care_clinical_reference_sets")

  cluster_reference_table_18 <- fread("D:/reference_data/temp/primary_care_clinical_reference_sets/ukpc_sct2_18.0.0_20200909/SnomedCT_UKPrimaryCareRF2_PRODUCTION_20200909T000000Z/SupportingProducts/GDDPR_Cluster_refset_1000230_20200909.csv",
                                      colClasses = "character")

  cluster_reference_table_18_v2 <- fread("D:/reference_data/GDPPR_Cluster_refset_1000230_20201016_v2.csv", colClasses = "character")


  ## Also download V20.0 (October), and 21.0 (February 2021) to see if we have these codes (V19.0 had an error that meant this file was not present)

  cluster_reference_address_trud_20 <- "https://isd.digital.nhs.uk/trud3/api/v1/keys/4149673aafc1a67390597b8f681b4d6a3660968b/files/SCT_PC/20.0.0/UKPC_SCT2/ukpc_sct2_20.0.0_20201223.zip"
  temp <- tempfile(fileext = ".zip")
  download.file(cluster_reference_address_trud_20, temp, mode = "wb")
  files <- unzip(temp, exdir = "D:/reference_data/temp/primary_care_clinical_reference_sets")

  cluster_reference_table_20 <- fread("D:/reference_data/temp/primary_care_clinical_reference_sets/ukpc_sct2_20.0.0_20201223/SnomedCT_UKPrimaryCareRF2_PRODUCTION_20201223T000000Z/SupportingProducts/GDPPR_Cluster_refset_1000230_20201223.csv",
                                      colClasses = "character")


  cluster_reference_address_trud_21 <- "https://isd.digital.nhs.uk/trud3/api/v1/keys/4149673aafc1a67390597b8f681b4d6a3660968b/files/SCT_PC/21.0.0/UKPC_SCT2/ukpc_sct2_21.0.0_20210127.zip"
  temp <- tempfile(fileext = ".zip")
  download.file(cluster_reference_address_trud_21, temp, mode = "wb")
  files <- unzip(temp, exdir = "D:/reference_data/temp/primary_care_clinical_reference_sets")

  cluster_reference_table_21 <- fread("D:/reference_data/temp/primary_care_clinical_reference_sets/ukpc_sct2_21.0.0_20210127/SnomedCT_UKPrimaryCareRF2_PRODUCTION_20210127T000000Z/SupportingProducts/GDPPR_Cluster_refset_1000230_20210127.csv",
                                      colClasses = "character")


  unlink(c(temp, files))

  setnames(cluster_reference_table_18, make.names(tolower(colnames(cluster_reference_table_18)), unique = TRUE))
  setnames(cluster_reference_table_18_v2, make.names(tolower(colnames(cluster_reference_table_18_v2)), unique = TRUE))
  setnames(cluster_reference_table_20, make.names(tolower(colnames(cluster_reference_table_20)), unique = TRUE))
  setnames(cluster_reference_table_21, make.names(tolower(colnames(cluster_reference_table_21)), unique = TRUE))


## Remove apostrophes from reference cluster names - apostrophe is curly, not straight style

  cluster_reference_table_18[, cluster_desc := gsub("’", "", cluster_desc)]
  cluster_reference_table_18_v2[, cluster_desc := gsub("’", "", cluster_desc)]
  cluster_reference_table_20[, cluster_desc := gsub("’", "", cluster_desc)]
  cluster_reference_table_21[, cluster_desc := gsub("’", "", cluster_desc)]


## Save reference data

  save_time <- gsub(":", "", Sys.time(), fixed = TRUE)

  fwrite(cluster_reference_table_18, paste0("D:/reference_data/priest_gdppr_cluster_refset_", save_time, "_V18.csv"))
  fwrite(cluster_reference_table_18_v2, paste0("D:/reference_data/priest_gdppr_cluster_refset_", save_time, "_V18_v2.csv"))
  fwrite(cluster_reference_table_20, paste0("D:/reference_data/priest_gdppr_cluster_refset_", save_time, "_V20.csv"))
  fwrite(cluster_reference_table_21, paste0("D:/reference_data/priest_gdppr_cluster_refset_", save_time, "_V21.csv"))


  cluster_reference_table_18[, priest_requested_cluster := cluster_desc %in% cluster_names]
  cluster_reference_table_18_v2[, priest_requested_cluster := cluster_desc %in% cluster_names]
  cluster_reference_table_20[, priest_requested_cluster := cluster_desc %in% cluster_names]
  cluster_reference_table_21[, priest_requested_cluster := cluster_desc %in% cluster_names]


## See if any of the clusters have asked for are not in the reference file - only for V18

  stopifnot(length(setdiff(cluster_names, unique(cluster_refs_for_priest_18$cluster_desc))) == 0)


## Download SNOMED CT from TRUD (V31.3.0, 2021-01-27, ~930MB)

  trud_address <- "https://isd.digital.nhs.uk/trud3/api/v1/keys/4149673aafc1a67390597b8f681b4d6a3660968b/files/SNOMEDCT2/31.3.0/UK_SCT2CL/uk_sct2cl_31.3.0_20210120000001.zip"


  temp <- tempfile(fileext = ".zip")
  download.file(trud_address, temp, mode = "wb")
  files <- unzip(temp, exdir = "D:/reference_data/temp/snomed")

  snomed_uk_desc <- fread("D:/reference_data/temp/snomed/SnomedCT_UKClinicalRF2_PRODUCTION_20210120T000001Z/Full/Terminology/sct2_Description_Full-en_GB1000000_20210120.txt",
                          colClasses = "character")
  snomed_int_desc <- fread("D:/reference_data/temp/snomed/SnomedCT_InternationalRF2_PRODUCTION_20200731T120000Z/Full/Terminology/sct2_Description_Full-en_INT_20200731.txt",
                           colClasses = "character")

  setnames(snomed_uk_desc, make.names(tolower(colnames(snomed_uk_desc)), unique = TRUE))
  setnames(snomed_int_desc, make.names(tolower(colnames(snomed_int_desc)), unique = TRUE))

  snomed_desc_full <- rbind(snomed_uk_desc, snomed_int_desc)


## Remove temp file and link to unzipped file

  unlink(c(temp, files))


# Download Additional Drug Tables from TRUD -------------------------------

  trud_address <- "https://isd.digital.nhs.uk/trud3/api/v1/keys/4149673aafc1a67390597b8f681b4d6a3660968b/files/SNOMEDCT2/31.3.0/UK_SCT2DR/uk_sct2dr_31.3.0_20210120000001.zip"


  temp <- tempfile(fileext = ".zip")
  download.file(trud_address, temp, mode = "wb")
  files <- unzip(temp, exdir = "D:/reference_data/temp/snomed")

  snomed_drug_desc <- fread("D:/reference_data/temp/snomed/SnomedCT_UKDrugRF2_PRODUCTION_20210120T000001Z/Full/Terminology/sct2_Description_Full-en_GB1000001_20210120.txt",
                            colClasses = "character")

  setnames(snomed_drug_desc, make.names(tolower(colnames(snomed_drug_desc)), unique = TRUE))

  snomed_uk_drugs <- rbind(snomed_uk_desc, snomed_drug_desc)
  snomed_full_drugs <- rbind(snomed_desc_full, snomed_drug_desc)

  save_time <- gsub(":", "", Sys.time(), fixed = TRUE)

  write.csv(snomed_uk_drugs, paste0("D:/reference_data/full_trud_snomed_concepts_uk_drug_", save_time, ".csv"))
  write.csv(snomed_full_drugs, paste0("D:/reference_data/full_trud_snomed_concepts_uk_int_drug_", save_time, ".csv"))

  rm(snomed_uk_desc, snomed_int_desc, snomed_uk_drugs, snomed_full_drugs)


## Remove temp file and link to unzipped file

  unlink(c(temp, files))



# Download and read in ONS file for postcode to LSOA11, RUC11, OAC --------

  # ONSPD Aug 2020

  temp <- tempfile(fileext = ".zip")
  download.file("https://www.arcgis.com/sharing/rest/content/items/a644dd04d18f4592b7d36705f93270d8/data", temp, mode = "wb")

  temp_location <- "D:/reference_data/temp/onspd_2020-08"
  files <- unzip(temp, exdir = temp_location)
  pc_to_oa11_classes <- fread(paste0(temp_location, "/Data/ONSPD_AUG_2020_UK.csv"))

  # retain only fields of interest
  pc_to_oa11_classes_fields_to_retain <- c("pcds", "usertype", "lsoa11", "ru11ind", "oac11", "imd")
  pc_to_oa11_classes[, colnames(pc_to_oa11_classes)[!(colnames(pc_to_oa11_classes) %in% pc_to_oa11_classes_fields_to_retain)] := NULL]

  pc_to_oa11_classes_char_cols <- colnames(pc_to_oa11_classes)[sapply(pc_to_oa11_classes, typeof) == "character"]
  pc_to_oa11_classes[, (pc_to_oa11_classes_char_cols) := lapply(.SD, fn_removeBlanks), .SDcols = pc_to_oa11_classes_char_cols]

  # Retain Output Area Classifications for England/Wales/Scotland/NI only
  pc_to_oa11_classes[!(substr(lsoa11, 1, 1) %in% c("E", "W", "S", "9")), ru11ind := NA]

  # Retain Rural-Urban classifications for England/Wales only - other home countries use other measures
  pc_to_oa11_classes[!(substr(lsoa11, 1, 1) %in% c("E", "W")), ru11ind := NA]

  # Retain IoD/IMD for England only - other home countries use other measures
  pc_to_oa11_classes[substr(lsoa11, 1, 1) != "E", imd := NA]

  # It is necessary to calculate quantiles using Type 4 to match the UK Gov published deciles.
  pc_to_oa11_classes[, iod19_decile := cut(imd,
                                   breaks = quantile(pc_to_oa11_classes[!is.na(imd), .N, by = .(lsoa11, imd)][, imd], probs = 0:10/10, type = 4),
                                   labels = FALSE,
                                   include.lowest = TRUE)]

  pc_to_oa11_classes[, imd := NULL]

  ## Remove temp file and link to unzipped file
  unlink(c(temp, files))

  # Check only one entry per postcode
  stopifnot(pc_to_oa11_classes[, .N, by = pcds][N > 1, .N] == 0)
  stopifnot(pc_to_oa11_classes[is.na(pcds), .N] == 0)

  # Check postcode format is variable length, one space between outward and inward parts)
  stopifnot(pc_to_oa11_classes[, max(nchar(pcds), na.rm = TRUE)] == 8 & pc_to_oa11_classes[, min(nchar(pcds), na.rm = TRUE)] == 6 &
            pc_to_oa11_classes[substr(pcds, nchar(pcds) - 3, nchar(pcds) - 3) == " ", .N] == nrow(pc_to_oa11_classes))

  save(pc_to_oa11_classes, file = "data/reference_data/pc_to_oa11_classes.rda")


# ## Download look up for ODS codes for use with Study Sites
#
#   ods_address <- "https://files.digital.nhs.uk/assets/ods/current/ets.zip"
#   temp <- tempfile(fileext = ".zip")
#   download.file(ods_address, temp, mode = "wb")
#   files <- unzip(temp, exdir = "D:/reference_data/temp")
#
#   ods_code_files <- lapply(files[grepl(".csv$", files)], function(file){
#     return(fread(file))
#   })
#
#   file_names <- list.files(dirname(files[1]))[grepl(".csv$", list.files(dirname(files[1])))]


## Download ED output spec that has reduced list of of SNOMED codes used for ECDS data

  ecds_tech_output_address <- "https://digital.nhs.uk/binaries/content/assets/website-assets/data-and-information/data-sets/ecds/ecds_data_set_etos_v3.0.0-draft.xlsx"

  temp <- tempfile(fileext = ".xlsx")
  save_location <- "data-raw/reference_data/ED output spec/ecds_tech_output.xlsx"
  download.file(url = ecds_tech_output_address, destfile = save_location, mode = "wb")



# GDPPR ethnicity codes ---------------------------------------------------

snomed_gdppr_ethnicity_mappings <- fread("D:/reference_data/dss_gdppr_ethnicity_mappings_latest_2021-01-17.csv", colClasses = "character")
gdppr_nhs_ethnicity_mappings <- unique(data.table(read_excel("D:/reference_data/Field Mapping and Standardisation.xlsx",
                                                             sheet = "ethnicity_standardisation",
                                                             col_names = TRUE,
                                                             col_types = "text",
                                                             trim_ws = TRUE))[, .(ethnicity_code, ethnicity_desc, ethnicity_group, gdppr_ethnicity)])


setnames(snomed_gdppr_ethnicity_mappings, make.names(tolower(colnames(snomed_gdppr_ethnicity_mappings)), unique = TRUE))
setnames(snomed_gdppr_ethnicity_mappings, "primarycode", "gdppr_ethnicity")

snomed_gdppr_ethnicity_mappings[, c("dss_key", "dss_system_created_date") := NULL]
snomed_nhs_ethnicity_mappings <- merge(snomed_gdppr_ethnicity_mappings,
                                         gdppr_nhs_ethnicity_mappings,
                                         by = "gdppr_ethnicity",
                                         all = TRUE)[, gdppr_ethnicity := NULL]

stopifnot(snomed_nhs_ethnicity_mappings[is.na(ethnicity_code), .N] == 0)
stopifnot(snomed_nhs_ethnicity_mappings[, .N, by = conceptid][N > 1, .N] == 0)

# Check all relevant ethnicity records recorded.
gdppr_ref_data <- fread("D:/reference_data/GDPPR_Cluster_refset_1000230_20201016_v2/GDPPR_Cluster_refset_1000230_20201016_v2.csv", colClasses = "character")
setnames(gdppr_ref_data, make.names(tolower(colnames(gdppr_ref_data)), unique = TRUE))
gdppr_ethnicity_ref_data <- unique(gdppr_ref_data[cluster_category == "Patient factors" & grepl(".* ethnicity .*", cluster_desc, ignore.case = TRUE), .(conceptid, conceptid_description)])
stopifnot(all(gdppr_ethnicity_ref_data[!(conceptid_description %chin% c("Caucasian (racial group)",
                                                                        "Mixed racial group (racial group)",
                                                                        "Race not stated (racial group)",
                                                                        "Unknown racial group (racial group)")), conceptid] %in% snomed_nhs_ethnicity_mappings$conceptid))

saveRDS(snomed_nhs_ethnicity_mappings, file = "data/reference_data/snomed_to_nhs_ethnicity_mappings.rds")


## Download BNF-SNOMED mapping

snomed_bnf_address <- "https://www.nhsbsa.nhs.uk/sites/default/files/2021-01/BNF%20Snomed%20Mapping%20data%2020210121.zip"
temp <- tempfile(fileext = ".zip")
download.file(snomed_bnf_address, temp, mode = "wb")
mapping_file <- unzip(temp, exdir = "D:/reference_data/temp")

snomed_bnf_mapping <- data.table(read_excel(mapping_file, col_types = "text"))

# set valid colnames
setnames(snomed_bnf_mapping, make.names(gsub("[.]+", "_", make.names(tolower(colnames(snomed_bnf_mapping)))), unique = TRUE))
cols_to_retain <- c("bnf_code", "snomed_code")

snomed_bnf_mapping[, (colnames(snomed_bnf_mapping)[!(colnames(snomed_bnf_mapping) %in% cols_to_retain)]) := NULL]

# See https://digital.nhs.uk/data-and-information/areas-of-interest/prescribing/practice-level-prescribing-in-england-a-summary/practice-level-prescribing-glossary-of-terms
snomed_bnf_mapping_medecines <- unique(snomed_bnf_mapping[substring(bnf_code, 1, 2) %in% formatC(1:15, digits = 0, width = 2, format = "d", flag = "0")][, ':=' (bnf_purpose_chemical = substring(bnf_code, 1, 9),
                                                                                                                                                          bnf_code = NULL)])
stopifnot(snomed_bnf_mapping_medecines[, .N, by = snomed_code][N > 1, .N] == 0)

saveRDS(snomed_bnf_mapping_medecines, file = "data/reference_data/snomed_bnf_mapping_medecines.rds")

# delete downloads
unlink(c(temp, mapping_file))



## Download SNOMED CT from TRUD (V31.1.0, 2020-11-25)

trud_address <- "https://isd.digital.nhs.uk/trud3/api/v1/keys/a3d462ddec9d10700a030b62c538622920702a43/files/SNOMEDCT2/31.1.0/UK_SCT2CL/uk_sct2cl_31.1.0_20201125000001.zip"

temp <- tempfile(fileext = ".zip")
download.file(trud_address, temp, mode = "wb")
files <- unzip(temp, exdir = "D:/reference_data/snomed_CT")

snomed_int <- fread("D:/reference_data/snomed_CT/SnomedCT_InternationalRF2_PRODUCTION_20200731T120000Z/Full/Terminology/sct2_Description_Full-en_INT_20200731.txt", sep = "\t", colClasses = "character")[, src := "INT"]
snomed_gb <- fread("D:/reference_data/snomed_CT/SnomedCT_UKClinicalRF2_PRODUCTION_20201125T000001Z/Full/Terminology/sct2_Description_Full-en_GB1000000_20201125.txt", sep = "\t", colClasses = "character")[, src := "GB"]

# Retain active Fully Specified Names (FSNs) only
snomed_ct <- rbind(snomed_int, snomed_gb)[typeId == "900000000000003001" & active == "1"]
rm(snomed_int, snomed_gb)

setnames(snomed_ct, make.names(colnames(snomed_ct), unique = TRUE))

fnames <- colnames(snomed_ct)[!(colnames(snomed_ct) %in% c("id", "conceptId", "term"))]
snomed_ct[, (fnames) := NULL]
snomed_ct <- unique(snomed_ct, by = c("conceptId", "term"))

setorder(snomed_ct, id)
snomed_ct[, order := 1:.N, by = conceptId]
snomed_ct <- snomed_ct[order == 1]
snomed_ct[, c("order", "id") := NULL]
stopifnot(snomed_ct[, .N, by = conceptId][N > 1, .N] == 0)

saveRDS(snomed_ct, file = "data/reference_data/snomed_int_gb_20201125_conceptid_term_lookup.rds")


## Download NHS Trust data

temp <- tempfile(fileext = ".zip")
download.file("https://files.digital.nhs.uk/assets/ods/current/etr.zip", temp, mode = "wb")

temp_location <- "D:/reference_data/temp/etr_2021_08"
files <- unzip(temp, exdir = temp_location)
trust_data <- fread(paste0(temp_location, "/etr.csv"), colClasses = "character")

setnames(trust_data, c("org_code", "name", "national_group", "high_level_health_geog", "address_1", "address_2", "address_3", "address_4", "address_5",
                       "postcode", "open_date", "close_date", "null_1", "null_2", "null_3", "null_4", "null_5", "telephone_number", "null_6", "null_7", "null_8",
                       "amended_record", "null_9", "gor_code", "null_10", "null_11", "null_12"))


saveRDS(trust_data, file = "D:/reference_data/trust_data.rds")

unlink(c(temp, files))
