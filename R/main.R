## Central Script for creation of datasets for Pre-Hospital PRIEST

# ## Run script for checking completeness of  the data that we received from YAS
#  ## Reads in raw data file delivered by YAS
#  ## Tidies up environment and leaves nothing read in after
# This should be an Rmarkdown file - not part of main processing
#   source("R/source_check.r")


## Run script for partitioning the data
  ## Reads in raw data from YAS and core PRIEST, removes identifiable data for NHS Digital tracing, col names changed, record-id created
  ## Saves all datasets separately in the project folder - data/datasets
  ## Saves identifiable data on D drive under folder - source_data/identifiers
  ## Last run 2020-09-15 151647 - Subsequent scripts will need updating if re-run as file names will change

  source("R/A01_partition_identifiable_priest_data.R")


## Run script for creating cohort for NHS Digital tracing
  ## Creates cohort, formats for sending to NHSD
  ## Takes input from - partition_identifiable_priest_data.r
  ## Outputs to - D:/source_data/identifiers
  ## Last run on 2020-09-15 - file was transferred to NHS Digital, no need to run again

  source("R/A02_cohort_for_nhs-digital_tracing.R")


## Run script for splitting ePR data on relationship to GUID
  ## Takes input from - partition_identifiable_priest_data.R
  ## Outputs as a grouped RData file to - data/datasets
  ## Last run on 2020/10/28 123709 - Subsequent scripts will need updating if re-run as file names will change

  source("R/A03_epr_pre_processing.R")


## Run script for standardising and validating YAS and core-priest data
  ## Takes ePR data created by - epr_pre_processing.R
  ## Takes CAD, NHS111, and core-PRIEST data created by - partition_identifiable_priest_data.R
  ## Outputs data to
  ## Still in creation,

