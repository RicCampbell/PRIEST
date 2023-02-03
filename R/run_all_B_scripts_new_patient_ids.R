## Script for running all scripts up to cohort creation if a new Single Source of Truth has been created
## A new Single Source of Truth (if the whole file is run) creates new random patient_ids

# source("R/B01_create_ssot.R")
#
# rm(list = ls())

source("R/B02_dr_processing.R")

rm(list = ls())

source("R/B03_apc_cc_processing.R")

rm(list = ls())

source("R/B04_epr_cad_processing.R")

rm(list = ls())

source("R/B05_nhs111_processing.R")

rm(list = ls())

source("R/B06_corepriest_processing.R")

rm(list = ls())

source("R/B07_01_ecds_processing.R")

rm(list = ls())

source("R/B07_02_ecds_stats_processing.R")

rm(list = ls())

source("R/B08_01_gdppr_refset_error_handling.R")

rm(list = ls())

source("R/B08_02_gdppr_markup_handling.R")

rm(list = ls())

source("R/B08_03_gdppr_markup_checking.R")

rm(list = ls())

source("R/B08_04_gdppr_processing.R")

rm(list = ls())

source("R/B08_05_gdppr_stats_processing.R")

rm(list = ls())
