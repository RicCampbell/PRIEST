library(data.table)

fn_date_to_ISOdate <- function(in_date, format_type) {
  # Converts to YYYY-MM-DD
  if(format_type == "dd/mm/yyyy") {
    out_date <- ifelse((is.na(in_date) | nchar(in_date) < 10),
                       as.character(NA),
                       paste(substr(in_date, 7, 10),
                             substr(in_date, 4, 5),
                             substr(in_date, 1, 2), sep = "-"))
  } else if(format_type == "yyyymmdd") {
    out_date <- ifelse((is.na(in_date) | nchar(in_date) < 8),
                       as.character(NA),
                       paste(substr(in_date, 1, 4),
                             substr(in_date, 5, 6),
                             substr(in_date, 7, 8), sep = "-"))
  } else if(format_type == "yyyy-mm-dd etc") {
    out_date <- ifelse((is.na(in_date) | nchar(in_date) < 10),
                       as.character(NA),
                       substr(in_date, 1, 10))
  }

  return(out_date)
}

fn_time_to_ISOtime <- function(in_times, format_type) {
  # Converts to HH:MM:SS
  if(format_type == "HHMMSS") {
    out_times <- stringr::str_pad(in_times, 6, side = "left", pad = "0")
    out_times <- ifelse(is.na(out_times),
                        as.character(NA),
                        paste(substr(out_times, 1, 2),
                              substr(out_times, 3, 4),
                              substr(out_times, 5, 6), sep = ":"))
  } else if(format_type == "yyyy-mm-dd HH:MM:SS") {
    out_times <- ifelse((is.na(in_times) | nchar(in_times) != 19),
                        as.character(NA),
                        substr(in_times, 12, 19))
  }

  return(out_times)
}

fn_datetime_to_ISOdatetime <- function(in_times, format_type) {
  # Converts to YYYY-MM-DD HH:MM:SS
  if(format_type == "YYYY-MM-DD HH:MM:SS") {
    out_times <- ifelse(is.na(in_times) | nchar(in_times) != 19,
                        as.character(NA),
                        paste(paste(substr(in_times, 1, 4),
                                    substr(in_times, 5, 6),
                                    substr(in_times, 7, 8), sep = "-"),
                              substr(in_times, 10, 17), sep = " "))
  } else if(format_type == "yyyy-mm-dd HH:MM:SS") {

  }

  return(out_times)
}


fn_time_to_AEtime <- function(in_times, format_type) {
  # Converts to HHMM
  if(format_type == "HM") {
    out_times <- stringr::str_pad(in_times, 4, side = "left", pad = "0")
  } else if(format_type == "yyyy-mm-dd HH:MM:SS") {
    out_times <- ifelse((is.na(in_times) | nchar(in_times) != 19),
                        as.character(NA),
                        paste0(substr(in_times, 12, 13), substr(in_times, 15, 16)))
  } else if(format_type == "HH:MMetc") {
    out_times <- ifelse((is.na(in_times) | nchar(in_times) < 5),
                        as.character(NA),
                        paste0(substr(in_times, 1, 2), substr(in_times, 4, 5)))
  } else if(format_type == "HHMMetc") {
    out_times <- ifelse((is.na(in_times) | nchar(in_times) < 4),
                        as.character(NA),
                        substr(in_times, 1, 4))
  }

  return(out_times)
}


fn_removeBlanks <- function(field) {
  field <- trimws(field)
  return(replace(field, field == "", NA))
}

fn_removeValues <- function(field, values_to_remove, case_sensitive = FALSE) {
  stopifnot(length(values_to_remove) > 0)

  if(case_sensitive) {
    field <- replace(field, field %in% values_to_remove, NA)
  } else {
    field <- replace(field, toupper(field) %in% toupper(values_to_remove), NA)
  }

  return(field)
}

fn_splitForenamesSurnames <- function(names) {
  max_name_len <- max(nchar(names), na.rm = TRUE)

  out_names <- data.table(names)
  out_names[, name_last_blank := regexpr("\\s[^\\s]+\\s*$", names, perl = TRUE)]
  out_names[name_last_blank == -1, name_last_blank := 1]
  out_names[, ':=' (surname = trimws(substr(names, name_last_blank, max_name_len)),
                    forenames = trimws(substr(names, 1, name_last_blank - 1)))]
  out_names[name_last_blank == 1, forenames := NA]

  out_names[, c("names", "name_last_blank") := NULL]

  return(out_names)
}

fn_cleanNames <- function(name_in) {
  # 1: remove non-whitespace, non-letter characters
  # 2: replace mulitple whitespace characters with a single space
  # 3: remove leading/trailing whitespace
  text_temp <- trimws(gsub("\\s+", " ", gsub("[^A-Z\\s\\-\\']+", "", toupper(name_in), perl = TRUE), perl = TRUE))

  # Set blanks to NA
  return(replace(text_temp, text_temp == "" | text_temp == "UNKNOWN" , as.character(NA)))
}

fn_validateNHSNumber <- function(NHSnumber_in) {

  expand_nhsno <- data.table(nhsno = NHSnumber_in)

  # remove non-numeric characters
  expand_nhsno[, nhsno := gsub("[^0-9]+", "", NHSnumber_in)]

  # Set NHS number of invalid length or dummy value to NA
  expand_nhsno[nchar(nhsno) != 10L | nhsno == "2333455667", nhsno := NA]

  # Split NHS number into characters
  expand_nhsno[, paste0("dig", 1:10) := tstrsplit(nhsno, "", fixed = TRUE)]

  # convert digits to to integers
  col_names <- paste0("dig", 1:10)
  expand_nhsno[, (col_names) := lapply(.SD, as.integer), .SDcols = col_names]

  # calculate checksum
  expand_nhsno[, checksum := 11L - (10L * dig1 + 9L * dig2 + 8L * dig3 + 7L * dig4 + 6L * dig5 + 5L * dig6 + 4L * dig7 + 3L * dig8 + 2L * dig9) %% 11L]
  expand_nhsno[checksum == 11L, checksum := 0L]

  # compare checksum (this accounts for case when checksum 10 [as a single digit cannot be 10!])
  expand_nhsno[dig10 != checksum, nhsno := NA]

  # Check for other invalid formats: 1st and last equal all else 0 OR all equal
  expand_nhsno[(dig1 == dig10 &
                  dig2 == 0 &
                  dig3 == 0 &
                  dig4 == 0 &
                  dig5 == 0 &
                  dig6 == 0 &
                  dig7 == 0 &
                  dig8 == 0 &
                  dig9 == 0) |
                 (dig1 == dig2 &
                    dig1 == dig3 &
                    dig1 == dig4 &
                    dig1 == dig5 &
                    dig1 == dig6 &
                    dig1 == dig7 &
                    dig1 == dig8 &
                    dig1 == dig9 &
                    dig1 == dig10), nhsno := NA]

  return(expand_nhsno[, nhsno])
}

fn_cleanPostcode <- function(postcode_in) {
  postcode_temp <- gsub("[^A-Z0-9]+", "", toupper(postcode_in))
  postcode_temp[!grepl("[A-Z][A-Z0-9]{1,3}[0-9][A-Z]{2}", postcode_temp)] <- NA
  postcode_out <- paste0(substring(postcode_temp, 1, nchar(postcode_temp) - 3),
                         " ",
                         substring(postcode_temp, nchar(postcode_temp) - 2))

  postcode_out <- replace(postcode_out, is.na(postcode_temp), NA)
  postcode_out <- replace(postcode_out, is.na(postcode_in), NA)
  return(postcode_out)
}

fn_validateCodes <- function(field_in, validCodes, invalidCode) {
  return(replace(field_in, (!is.na(field_in) & !(field_in %in% validCodes)), invalidCode))
}


fn_validateDates <- function(field_in) {
  date_out <- replace(field_in, field_in %in% "1900-01-01", NA)
  date_converted <- lubridate::fast_strptime(date_out, format = "%Y-%m-%d", tz = "Europe/London", lt = FALSE)
  return(replace(date_out, !is.na(field_in) &
                   (is.na(date_converted) |
                      date_converted < as.POSIXct("1888-01-01", tz = "Europe/London") |
                      date_converted > as.POSIXct("2017-03-31", tz = "Europe/London")), NA))
}

fn_validateDatetimes <- function(datetime_in, invalidCode) {
  datetime_out <- replace(datetime_in, datetime_in %in% "1900-01-01 00:00:00", NA)
  datetime_converted <- lubridate::fast_strptime(datetime_out, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/London", lt = FALSE)
  return(replace(datetime_out, !is.na(datetime_in) &
                   (is.na(datetime_converted) | datetime_converted < as.POSIXct("1888-01-01 00:00:00", tz = "Europe/London")), invalidCode))

}


fn_validateAETimes <- function(time_in, invalidCode) {
  time_out <- replace(time_in, time_in == "2400", "0000")
  return(replace(time_out, !is.na(time_out) & !grepl("^(?:[01][0-9]|2[0-3])(?:[0-5][0-9])$", time_out), invalidCode))
}


fn_validateDigits <- function(field_in, len, validCodes, invalidCode) {
  field_in <- stringr::str_pad(field_in, width = len, side = "left", "0")
  return(replace(field_in, (!is.na(field_in) & !(field_in %in% validCodes)), invalidCode))
}

fn_validateNumeric <- function(field_in, len) {
  field_in <- stringr::str_pad(trimws(field_in), width = len, side = "left", "0")
  return(replace(field_in, (!is.na(field_in) & !grepl(paste0("^[$[0-9]{", len, "}$"), field_in)), paste0(rep(9, len), collapse = "")))
}


fn_validateProcodet <- function(procodet_in, procodet_sender) {
  stopifnot(length(unique(procodet_sender)) == 1)
  procodet_out <- toupper(procodet_in)
  procodet_out <- replace(procodet_out, is.na(procodet_out) | (nchar(procodet_out) != 3 & nchar(procodet_out) != 5), procodet_sender[1])
  return(substr(procodet_out, 1, 3))
}

fn_validateSitetret <- function(sitetret_in, procodet) {
  sitetret_out <- toupper(sitetret_in)
  sitetret_out <- replace(sitetret_out,
                          !is.na(sitetret_out) & (substr(sitetret_out, 1, 3) != procodet | (nchar(sitetret_out) != 3 & nchar(sitetret_out) != 5)),
                          procodet[!is.na(sitetret_out) & (substr(sitetret_out, 1, 3) != procodet | (nchar(sitetret_out) != 3 & nchar(sitetret_out) != 5))])
  return(replace(sitetret_out,
                 !is.na(sitetret_out) & nchar(sitetret_out) == 3,
                 paste0(sitetret_out[!is.na(sitetret_out) & nchar(sitetret_out) == 3], "00")))
}

fn_validateGpprac <- function(field_in, invalidCode) {
  field_out <- toupper(field_in)
  return(replace(field_out, (!is.na(field_in) & !grepl("^[A-HJ-NP-WY][0-9]{5}$", field_out)), invalidCode))
}

fn_validateReferorg <- function(field_in, invalidCode) {
  field_out <- toupper(field_in)
  return(replace(field_out, (!is.na(field_in) & !grepl("^[A-Z0-9]{2,6}$", field_out)), invalidCode))
}

fn_validateICD10Diagnoses <- function(diags_wide, APCdata = TRUE) {
  icd10_x_codes <- readRDS("data/reference/apc_reference_data.rds")[["diag_icd10_x"]][, code]

  diags <- melt(diags_wide, id.vars = "urid", variable.name = "ordinal", value.name = "val_diag", variable.factor = FALSE)

  diags[, ':=' (ordinal = as.numeric(substr(ordinal, 6, 7)),
                val_diag = toupper(val_diag))]

  # Remove morphology data
  diags[grepl("^M?[0-9]{4}\\/[0-9]{0,1}$", val_diag), ':=' (val_diag = NA,
                                                            ordinal = NA)]

  ## Reorder
  setorder(diags, ordinal, na.last = TRUE)
  diags[, ordinal := as.numeric(1:.N), by = urid]

  ## Strip dashes from codes
  diags[, val_diag := sub("-", " ", val_diag, fixed = TRUE)]

  ## General ICD10 form
  ##  (approximates HES ACv3 Rule0510)
  if(APCdata) {
    diags[!is.na(val_diag) & !grepl("^[A-Z][0-9]{2}\\.?(?:[ X0-9][ A-Z0-9]{0,3})?$", val_diag), val_diag := "R69X8"]
  } else {
    diags[!is.na(val_diag) & !grepl("^[A-Z][0-9]{2}\\.?(?:[ X0-9][ A-Z0-9]{0,3})?$", val_diag), val_diag := NA]
  }

  ## Strip dot from codes
  ##  (approximates HES ACv3 Rule0460)
  diags[, val_diag := sub(".", "", val_diag, fixed = TRUE)]

  ## Replace space (if present)- or if only 3 char diag right pad- with X for valid X codes
  ##   (HES ACv3 Rule0470)
  diags[(substr(val_diag, 4, 4) == " " | nchar(val_diag) == 3) & substr(val_diag, 1, 3) %in% icd10_x_codes,
        val_diag := paste0(substr(val_diag, 1, 3), "X", substring(val_diag, 5))]

  ## Replace space (if present)- or if only 3 char diag right pad- with 9 for non X codes
  ##   (HES ACv3 Rule0470)
  diags[(substr(val_diag, 4, 4) == " " | nchar(val_diag) == 3) & !(substr(val_diag, 1, 3) %in% icd10_x_codes),
        val_diag := paste0(substr(val_diag, 1, 3), "9", substring(val_diag, 5))]

  if(APCdata) {
    ## Null primary diagnosis
    ##   (HES ACv3 Rule0500a,b)
    diags[ordinal == 1 & is.na(val_diag), val_diag := "R69X6"]

    ## Cause codes
    ##   (HES ACv3 Rule0580 [Part 1])
    if(diags[grepl("^[VWXY][0-9]{2}", val_diag), .N] > 0) {
      cause_diags <- merge(diags[grepl("^[VWXY][0-9]{2}", val_diag), .(ordinal = min(ordinal)), by = urid],
                           diags,
                           by = c("urid", "ordinal"))


      diags <- rbind(diags, cause_diags[, .(urid, ordinal = 21, val_diag)])


      if(cause_diags[ordinal == 1, .N] > 0) {
        cause_primary_diags <- merge(cause_diags[ordinal == 1, .(urid, cause = val_diag)],
                                     diags[is.na(val_diag), .(ordinal = min(ordinal)), by = urid],
                                     by = "urid")

        ## cause code in primary diag
        ##   (HES ACv3 Rule0530)
        diags[ordinal == 1 & grepl("^[VWXY][0-9]{2}", val_diag), val_diag := "R69X3"]

        diags <- merge(diags, cause_primary_diags, by = c("urid", "ordinal"), all.x = TRUE)
        diags[!is.na(cause), val_diag := cause]
        diags[, cause := NULL]
      }
    }
  }

  # Reshape wide
  diags[, ordinal := paste0("val_diag_", stringr::str_pad(ordinal, 2, side = "left", pad = "0"))]
  diags_wide <- dcast(diags, urid ~ ordinal, fill = NA, drop = FALSE, value.var = "val_diag")
  ##   (HES ACv3 Rule0580 [Part 2])
  if("val_diag_21" %in% colnames(diags_wide)) {
    setnames(diags_wide, "val_diag_21", "cause")
  } else if(APCdata) {
    diags_wide[, cause := as.character(NA)]
  }

  # Return diagnoses
  return(diags_wide)
}

fn_validateOPCSOps <- function(ops_wide, invalidDateCode) {

  ## reshape long
  ops_data <- melt(ops_wide, id.vars = "urid", measure = patterns("opertn_", "opdate_"),
                   variable.name = "ordinal", value.name = c("val_opertn", "val_opdate"),
                   variable.factor = FALSE)

  ops_data[, ':=' (ordinal = as.numeric(ordinal),
                   val_opertn = toupper(val_opertn))]

  ## General OPCS form
  ##   (approximates HES ACv3 Rule0550 [Part1] & Rule0560)
  ops_data[!is.na(val_opertn) & !grepl("^[A-HJ-Z][0-9]{2}\\.?[0-9\\-]?$", val_opertn), val_opertn := "&"]

  ## Strip dot from codes
  ops_data[, val_opertn := sub(".", "", val_opertn, fixed = TRUE)]

  ## Replace dash (if present)- or if only 3 char right pad- with 9
  ##   (approximates HES ACv3 Rule0450)
  ops_data[(substr(val_opertn, 4, 4) == "-" | nchar(val_opertn) == 3),
           val_opertn := paste0(substr(val_opertn, 1, 3), 9)]

  ## Invalid primary operation
  ##   (approximates HES ACv3 Rule0550 [Part2])
  ops_data[ordinal == 1 &
             (substring(val_opertn, 1, 1) %in% c("Y", "Z") | substring(val_opertn, 1, 3) %in% c("O11", "O13", "O14")),
           val_opertn := "&"]

  ## Invalid primary operation
  ##   (HES ACv3 Rule0540)
  ops_data[ordinal == 1 & substring(val_opertn, 1, 3) == "X99",
           val_opertn := NA]

  ## Validate operation dates
  ##   (approximates HES ACv3 Rule0480 & Rule0485)
  ops_data[is.na(val_opertn) & !is.na(val_opdate), val_opdate := invalidDateCode]
  ops_data[, val_opdate := fn_validateDates(val_opdate, invalidDateCode)]
  ops_data[val_opdate < "1930-01-01", val_opdate := invalidDateCode]

  ## Reshape wide
  ops_data[, ordinal := stringr::str_pad(ordinal, 2, side = "left", pad = "0")]
  ops_wide <- dcast(ops_data, urid ~ ordinal, fill = NA, drop = FALSE, value.var = c("val_opertn", "val_opdate"))

  return(ops_wide)
}


fn_validateAEDiagnoses <- function(diags) {
  diags_out <- toupper(diags)
  return(replace(diags_out, !is.na(diags_out) & !grepl("^[0-9 ]{1,5}[ LRB8]$", diags_out), NA))
}


fn_validateAEInvestigations <- function(invests) {
  invests_out <- stringr::str_pad(substr(invests, 1, 2), 2, side = "left", pad = "0")
  return(replace(invests_out, !is.na(invests_out) & !grepl("^[0-9]{2}$", invests_out), NA))
}


fn_validateAETreatments <- function(treats) {
  treats_out <- stringr::str_pad(substr(treats, 1, 3), 2, side = "left", pad = "0")
  return(replace(treats_out, !is.na(treats_out) & !grepl("^[0-9]{2,3}$", treats_out), NA))
}


fn_calcAge <- function(point_time, ref_time) {
  age <- floor(lubridate::time_length(difftime(as.Date(point_time, format = "%Y-%m-%d"), as.Date(ref_time, format = "%Y-%m-%d")), "years"))
  age <- replace(age, age > 120, 120)
  age <- replace(age, age < 0, NA)
  return(stringr::str_pad(as.character(age), width = 3, side = "left", pad = "0"))
}

removeFreetextWS <- function(text_vector) {
  text_vector <- gsub("\\n", " ", text_vector, fixed = TRUE)
  text_vector <- trimws(gsub("[\\s]{1,}", " ", text_vector, perl = TRUE))
  text_vector <- replace(text_vector, text_vector == "", NA)
  return(text_vector)
}

fn_covertStrToDate <- function(date_in, dt_format = "%Y-%m-%d") {
  date_out <- as.Date(lubridate::fast_strptime(paste(date_in, "12:00:00"), format = paste(dt_format, "%H:%M:%S"), tz = "Europe/London", lt = FALSE))
  date_out <- replace(date_out, date_out %in% as.Date("1900-01-01"), NA)
  return(date_out)
}
