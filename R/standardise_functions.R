library(data.table)
library(lubridate)


## function for finding the number of non NA fields for each col in a table

nonNaRecordsTable <- function(table, over_id_col = NA) {
  stopifnot(length(over_id_col) == 1)
  stopifnot(is.na(over_id_col) | is.character(over_id_col))
  stopifnot(is.na(over_id_col) | over_id_col %in% colnames(table))

  if(!is.na(over_id_col)) {
    record_count <- table[, .N, by = eval(over_id_col)][, .N]
  } else {
    record_count <- table[, .N]
  }

  cols <- colnames(table)[!(colnames(table) %in% over_id_col)]

  if(!is.na(over_id_col)) {
    non_na_table_patient <- table[, lapply(.SD, function (y) any(!is.na(y))), by = eval(over_id_col), .SDcols = cols]
    non_na_table_count <- non_na_table_patient[, lapply(.SD, function(z) sum(z) / record_count * 100), .SDcols = cols]
  } else {
    non_na_table_count <- table[, lapply(.SD, function(z) sum(!is.na(z)) / record_count * 100), .SDcols = cols]
  }

  return(melt(non_na_table_count,
              measure.vars = colnames(non_na_table_count),
              variable.name = "field",
              variable.factor = FALSE,
              value.name = "non_missing_pc"))
}

## function that returns all date col names in table

getPOSIXtFields <- function(table) {
  return(colnames(table)[sapply(table, is.POSIXt)])
}


## function for checking min and max of a date col

datesWithinRange <- function(table, date_cols, min_date, max_date) {

  return(table[, .(out_of_range = Reduce(`+`, lapply(.SD, function(x) {
    return(as.Date(max(x, na.rm = TRUE), "%Y-%m-%d", tz = "Europe/London") > max_date |
      as.Date(min(x, na.rm = TRUE), "%Y-%m-%d", tz = "Europe/London") < min_date)
    }))), .SDcols = date_cols][1, out_of_range] == 0)
}






