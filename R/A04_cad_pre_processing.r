library(data.table)
source("R/cleaning_fns_etl.r")
source("R/standardise_functions.r")
source("private/hash_fn.r")

## Read in data

cad_data_version <- "2020-09-15 151647"

epr_cad <- readRDS(paste0("data/datasets/epr_cad-", cad_data_version, ".rds"))
nonepr_cad <- readRDS(paste0("data/datasets/nonepr_cad-", cad_data_version, ".rds"))

load("data/datasets/epr_tables_freetext_anonymised-2020-09-15 151647.rda")


## Check that have same cols both ways

stopifnot(all(colnames(epr_cad) %in% colnames(nonepr_cad)) == TRUE)
stopifnot(all(colnames(nonepr_cad) %in% colnames(epr_cad)) == TRUE)

## Bind together (record_id has different prefix if ever need to know where record came from)

cad_data <- rbind(epr_cad, nonepr_cad)

## Remove dash from col name
setnames(cad_data,
         c("call_stop-reason", "hospital_arrival_time"),
         c("call_stop_reason", "destination_arrival_time"))


##Pseudonymise  call number (same salt so can link with ePR still)
cad_data[, amb_incident_id := fn_pseudonymiseValues(as.character(call_number), salt_filename = "private/salt1.txt")]
cad_data[, call_number := NULL]

## We know there are duplicates (only twins) across but not within CAD ePR and CAD non-ePR data
cnames <- colnames(cad_data)[colnames(cad_data) != "record_ID"]

stopifnot(cad_data[substr(as.character(record_ID), 1, 1) == "2", .N, by = cnames][N > 1, .N] == 0)
stopifnot(cad_data[substr(as.character(record_ID), 1, 1) == "3", .N, by = cnames][N > 1, .N] == 0)
stopifnot(cad_data[, .N, by = cnames][N > 2, .N] == 0)

# Check all call_number's appearing in ePR records also appear in CAD records
stopifnot(epr_single_value_fields_tbl[!(amb_incident_id %in% cad_data$amb_incident_id) & !is.na(amb_incident_id), .N] == 0)

## Remove duplicate records - unique on all cols apart from record_ID
cad_data <- unique(cad_data, by = cnames)
stopifnot(cad_data[, .N, by = cnames][N > 1, .N] == 0)

## Check all removed records have been nonepr_cad table records
##  [record_id's beginning '3']

stopifnot(cad_data[substr(record_ID, 1, 1) == "2", .N] == epr_cad[, .N])


## Limit CAD data to only one record per call_number


cad_data[, .hospital_was_destination. := (!is.na(destination_arrival_time) &
                                            hospital_attended %in% c('NORTHERN GENERAL HOSPITAL', 'BRADFORD ROYAL INFIRMARY',
                                                                     'ST JAMES UNIVERSITY HOSPITAL', 'PINDERFIELDS GENERAL HOSPITAL',
                                                                     'BARNSLEY DISTRICT GENERAL', 'CALDERDALE ROYAL HOSPITAL',
                                                                     'LEEDS GENERAL INFIRMARY', 'SCARBOROUGH DISTRICT GENERAL HOSPITAL',
                                                                     'DONCASTER ROYAL INFIRMARY', 'ROTHERHAM DISTRICT GENERAL HOS',
                                                                     'HUDDERSFIELD ROYAL INFIRMARY', 'HULL ROYAL INFIRMARY',
                                                                     'AIREDALE GENERAL HOSPITAL', 'YORK DISTRICT HOSPITAL',
                                                                     'HARROGATE DISTRICT HOSPITAL', 'CASTLE HILL HOSPITAL',
                                                                     'SCUNTHORPE GENERAL HOSPITAL', 'JAMES COOK UNIVERSITY HOSPITAL',
                                                                     'FRIARAGE HOSPITAL', 'EAST RIDING COMMUNITY HOSPITAL',
                                                                     'SHEFFIELD CHILDRENS HOSPITAL', 'DARLINGTON MEMORIAL HOSPITAL',
                                                                     'ROYAL HALLAMSHIRE', 'DEWSBURY DISTRICT HOSPITAL',
                                                                     'BASSETLAW DISTRICT GENERAL', 'PONTEFRACT GENERAL INFIRMARY',
                                                                     'THE MOUNT', 'CHESTERFIELD ROYAL HOSPITAL', 'WESTON PARK HOSPITAL',
                                                                     'TICKHILL ROAD HOSPITAL', 'TICKHILL ROAD SITE HOSPITAL',
                                                                     'OTHER HOSPITAL', 'LEEDS GENERAL INFIRMARY (PPCI)',
                                                                     'WHITBY HOSPITAL', 'BRIDLINGTON AND DISTRICT HOSPITAL',
                                                                     'LANCASTER ROYAL INFIRMARY', 'ROYAL OLDHAM HOSPITAL',
                                                                     'BRANSHOLME MIU', 'RIPON DISTRICT HOSPITAL',
                                                                     'MALTON HOSPITAL'))]


# For incidents with any conveyance to hospital, choose record with earliest destination_arrival_time (at a hospital)
setorder(cad_data, destination_arrival_time, record_ID, na.last = TRUE)
cad_data[.hospital_was_destination. == TRUE, .cad_record_order. := 1:.N, by = amb_incident_id]
cad_data[, .any_order. := any(!is.na(.cad_record_order.)), by = amb_incident_id]

# For incidents with conveyance but none to hospital, choose record with earliest destination_arrival_time (not at a hospital)
cad_data[.any_order. == FALSE & !is.na(destination_arrival_time), .cad_record_order. := 1:.N, by = amb_incident_id]
cad_data[, .any_order. := any(!is.na(.cad_record_order.)), by = amb_incident_id]

# For incidents with no conveyance, choose record with earliest scene_arrival_time
setorder(cad_data, scene_arrival_time, record_ID, na.last = TRUE)
cad_data[.any_order. == FALSE & !is.na(scene_arrival_time), .cad_record_order. := 1:.N, by = amb_incident_id]
cad_data[, .any_order. := any(!is.na(.cad_record_order.)), by = amb_incident_id]

# For incidents with no scene_arrival_time, pick earliest record_ID
setorder(cad_data, record_ID, na.last = TRUE)
cad_data[.any_order. == FALSE, .cad_record_order. := 1:.N, by = amb_incident_id]

cad_data <- cad_data[.cad_record_order. == 1][, c(".hospital_was_destination.",
                                                  ".cad_record_order.",
                                                  ".any_order.") := NULL]

saveRDS(cad_data,
        file = paste0("data/datasets/cad_data-", cad_data_version, ".rds"))
