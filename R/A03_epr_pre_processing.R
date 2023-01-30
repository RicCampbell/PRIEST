library(data.table)
library(lubridate)
library(readxl)
source("R/cleaning_fns_etl.r")
source("private/hash_fn.r")


## Read in ePR data and mapping of YAS fields for groupings

  epr_data_version <- "2020-09-15 151647"

  epr <- readRDS(paste0("data/datasets/epr-", epr_data_version, ".rds"))
  epr_groups <- data.table(read_excel("D:/reference_data/Field Mapping and Standardisation.xlsx",
                                     sheet = "ePR",
                                     col_names = TRUE,
                                     col_types = "text",
                                     trim_ws = TRUE))[, .(destination_field, field_group)]



##Pseudonymise the GUID and call number

  stopifnot(epr[is.na(epr_GUID) | epr_GUID == "", .N] == 0)

  epr[, ':=' (amb_incident_id = fn_pseudonymiseValues(as.character(call_number), salt_filename = "private/salt1.txt"),
              epr_id = fn_pseudonymiseValues(epr_GUID, salt_filename = "private/salt1.txt"))]


## Remove call_number and guid from dataset

  epr[, c("call_number", "epr_GUID") := NULL]


## Replace all empty strings with NA for character cols (also want to replace all "n/a" with N/A)
  ## Trying to figure out getting just cols that are char so can check for empty strings

  epr_col_names <- colnames(epr)

  epr_char_cols <- colnames(epr)[sapply(epr, typeof) == "character"]

  epr[, (epr_char_cols) := lapply(.SD, fn_removeBlanks), .SDcols = epr_char_cols]


## Replace all 'n/a' in character cols with actual <N/A>

  epr[, (epr_char_cols) := lapply(.SD, fn_removeValues, "n/a"), .SDcols = epr_char_cols]


# Create guid 'one-to-one' table -------------------------------------------------


## Create table of col names and the groupings they belong to (e.g. observations)

## Identify number of distinct values per epr_id for each field
## - indicates groups of fields which are likely to be 1-to-1 with epr_id
  col_names <- colnames(epr)[colnames(epr) != "epr_id"]
  distinct_vals_by_field_epr_id_wide <- epr[, lapply(.SD, uniqueN), by = epr_id, .SDcols = col_names][, lapply(.SD, max), .SDcols = col_names]

  distinct_vals_by_field_epr_id <- melt(distinct_vals_by_field_epr_id_wide,
                                            measure.vars = colnames(distinct_vals_by_field_epr_id_wide),
                                            variable.name = "field",
                                            variable.factor = FALSE,
                                            value.name = "max_distinct_vals")

  distinct_vals_by_field_epr_id <- merge(distinct_vals_by_field_epr_id,
                                             epr_groups,
                                             by.x = "field",
                                             by.y = "destination_field",
                                             all.x= TRUE)

## Group all cols that are 'one-to-one', and are not part of a grouping

  epr_id_single_value_cols <- distinct_vals_by_field_epr_id[is.na(field_group) & max_distinct_vals == 1, field]

  epr_id_single_value_cols <- c("epr_id", epr_id_single_value_cols)

  ## Create table with all values but just these cols and only distinct values

  epr_single_value_fields_tbl <- unique(epr[, ..epr_id_single_value_cols])


## Check number of rows is the same as unique guids in all epr

  stopifnot(epr_single_value_fields_tbl[, .N] == epr[, uniqueN(epr_id)])



# Create epr_id one-to-multiple tables --------------------------------------

## Get list of cols, not in groups, that are not one-to-one relationships to epr_id

  epr_multi_value_ungrouped_fields <- distinct_vals_by_field_epr_id[is.na(field_group) & max_distinct_vals > 1, field]

  epr_multi_value_ungrouped_field_tables <- sapply(epr_multi_value_ungrouped_fields, function(field, data) {
    fields <- c("epr_id", field)
    table_name <- paste("epr", field, "tbl", sep ="_")
    assign(table_name, unique(data[!is.na(get(field)), ..fields]), envir = parent.frame(n = 3))
    return(table_name)
  }, data = epr)

## Get list of groups that are not a one-to-one relationship to epr_id

  epr_multi_value_field_groups <- distinct_vals_by_field_epr_id[!is.na(field_group), unique(field_group)]

  epr_multi_value_grouped_field_tables <- sapply(epr_multi_value_field_groups, function(field_group_name, data, groupings) {
    fields <- c("epr_id", groupings[field_group == field_group_name, field])
    table_name <- paste("epr", field_group_name, "tbl", sep ="_")
    dt <- data[, ..fields][, .non_missing_obs. := Reduce(`+`, lapply(.SD, function(x) !is.na(x)))][.non_missing_obs. != 1][, .non_missing_obs. := NULL]
    assign(table_name, unique(dt), envir = parent.frame(n = 3))
    return(table_name)
  }, data = epr, groupings = distinct_vals_by_field_epr_id)


## Save all tables

   save(list = c("epr_single_value_fields_tbl",
                 epr_multi_value_ungrouped_field_tables,
                 epr_multi_value_grouped_field_tables),
        file = paste0("data/datasets/epr_tables-", epr_data_version, ".rda"))


# Process freetext --------------------------------------------------------



   # fn to remove white space
   standardiseFreetext <- function(x) {
     x <- gsub("\\n", " ", x, fixed = TRUE)
     x <- trimws(gsub("[\\s]{1,}", " ", x, perl = TRUE))
     replace(x, x == "", NA)
   }


   # load data
   load("data/datasets/epr_tables-2020-09-15 151647.rda")

   epr_freetext_excl_ecg_tbl <- melt(epr_freetext_excl_ecg_tbl, id.vars = "epr_id", variable.factor = FALSE)
   epr_freetext_excl_ecg_tbl[, ft_record_id := 1:.N]
   epr_ecg_incl_freetext_tbl[, ft_record_id := nrow(epr_freetext_excl_ecg_tbl) + 1:.N]

   freetext_tbl <- copy(rbind(epr_freetext_excl_ecg_tbl[, .(ft_record_id, value)],
                              epr_ecg_incl_freetext_tbl[, .(ft_record_id, value = ecg_findings)]))

   stopifnot(freetext_tbl[, .N, by = ft_record_id][N > 1, .N] == 0)
   stopifnot(nrow(freetext_tbl) == nrow(epr_freetext_excl_ecg_tbl) + nrow(epr_ecg_incl_freetext_tbl))

   epr_freetext_excl_ecg_tbl[, value := NULL]
   epr_ecg_incl_freetext_tbl[, ecg_findings := NULL]

   save(epr_freetext_excl_ecg_tbl,
        epr_ecg_incl_freetext_tbl,
        epr_airways_intervention_tbl,
        epr_cardiac_respiratory_arrest_tbl,
        epr_drugs_tbl,
        epr_news_score_tbl,
        epr_phys_observations_tbl,
        epr_record_ID_tbl,
        epr_referral_type_crew_tbl,
        epr_single_value_fields_tbl,
        file = "data/datasets/epr_tables_freetext_removed-2020-09-15 151647.rda")

   rm(epr_freetext_excl_ecg_tbl,
      epr_ecg_incl_freetext_tbl,
      epr_airways_intervention_tbl,
      epr_cardiac_respiratory_arrest_tbl,
      epr_drugs_tbl,
      epr_news_score_tbl,
      epr_phys_observations_tbl,
      epr_record_ID_tbl,
      epr_referral_type_crew_tbl,
      epr_single_value_fields_tbl)

   freetext_tbl[, freetext := standardiseFreetext(value)]
   freetext_tbl[!is.na(freetext), ft_sample_id := .GRP, by = freetext]


   saveRDS(freetext_tbl[, .(ft_record_id,
                            ft_sample_id,
                            value)], file = "data/datasets/epr_freetext_values-lookup-2020-09-15 151647.rds")

   saveRDS(unique(freetext_tbl[!is.na(ft_sample_id), .(
     ft_sample_id,
     freetext)]), file = "data/datasets/epr_freetext_distinct_values-2020-09-15 151647.rds")


   # # Take sample: for test purposes only --------------------------------------
   #
   # ft <- unique(freetext_tbl[!is.na(ft_sample_id), .(
   #   ft_sample_id,
   #   freetext)])
   #
   # # sample_indices <- sample.int(nrow(ft), size = 20)
   # # Confirmed non-identifying
   # sample_indices <- c(1897, 33333, 53821, 11243, 12554, 10666, 8674, 33407, 6194, 32350, 15447, 52665, 43328, 39579, 48268, 31308, 41088, 49240, 21475, 50183)
   # ft_sample_vals <- ft[sample_indices, freetext]
   #
   # # Resample and add random digits to avoid caching.
   # ft_sample <- data.table(freetext = rep(ft_sample_vals, 50))
   # ft_sample[, ':=' (ft_sample_id = 1:.N,
   #                   freetext = paste(freetext, sample.int(nrow(ft_sample))))]
   #
   # saveRDS(ft_sample, file = "data/datasets/epr_freetext_distinct_values-sample.rds")

