library(data.table)

epr_data_version <- "2020-09-15 151647"

freetext_data <- readRDS(paste0("data/datasets/epr_freetext_distinct_values-", epr_data_version, ".rds"))

freetext_anon_data <- rbind(readRDS(paste0("data/datasets/freetext_anon-", sub(" ", "_", epr_data_version), "_part1.rds"))[status == "OK"],
                       readRDS(paste0("data/datasets/freetext_anon-", sub(" ", "_", epr_data_version), "_part2.rds"))[status == "OK"])

stopifnot(freetext_data[, .N] == freetext_anon_data[, .N])
stopifnot(freetext_anon_data[is.na(anon_freetext), .N] == 0)

setnames(freetext_anon_data, "id", "ft_sample_id")
freetext_anon_data[, status := NULL]

freetext_anon_eval <- merge(freetext_data,
                            freetext_anon_data,
                            by = "ft_sample_id",
                            all = TRUE)

stopifnot(freetext_anon_eval[is.na(freetext) | is.na(anon_freetext), .N] == 0)

saveRDS(freetext_anon_eval, file = "data/datasets/epr_freetext_anonimisation_eval.rds")

rm(freetext_data, freetext_anon_eval)

# Make substitutions in epr data
epr_tbl_names <- load(paste0("data/datasets/epr_tables_freetext_removed-", epr_data_version, ".rda"))
freetext_values_lookup <- readRDS(paste0("data/datasets/epr_freetext_values-lookup-", epr_data_version, ".rds"))
freetext_values_lookup[, value := NULL]

# ecg table
epr_ecg_incl_freetext_tbl <- merge(merge(epr_ecg_incl_freetext_tbl,
                                         freetext_values_lookup,
                                         by = "ft_record_id",
                                         all.x = TRUE),
                                   freetext_anon_data,
                                   by = "ft_sample_id",
                                   all.x = TRUE)[, c("ft_record_id", "ft_sample_id") := NULL]

setnames(epr_ecg_incl_freetext_tbl, "anon_freetext", "ecg_findings")

# freetext table
epr_freetext_excl_ecg_tbl <- merge(merge(epr_freetext_excl_ecg_tbl,
                                         freetext_values_lookup,
                                         by = "ft_record_id",
                                         all.x = TRUE),
                                   freetext_anon_data,
                                   by = "ft_sample_id",
                                   all.x = TRUE)[, c("ft_record_id", "ft_sample_id") := NULL]

epr_freetext_excl_ecg_tbl <- dcast(epr_freetext_excl_ecg_tbl,
                                   epr_id ~ variable,
                                   fill = NA,
                                   value.var = "anon_freetext")

save(list = epr_tbl_names, file = paste0("data/datasets/epr_tables_freetext_anonymised-", epr_data_version, ".rda"))
