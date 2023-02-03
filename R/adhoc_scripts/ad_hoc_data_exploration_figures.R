## Script for ad-hoc data exploration/figure finding

library(data.table)
source("private/hash_fn.r")


## How many guid have more than one news scores that are not N/A

  news_score_table <- copy(guid_news_score_table)


  ## Remove all NA news score to get clearer number

  news_score_table <- news_score_table[!is.na(news_score)]

  news_score_table[, order := 1:.N, by = pseudo_guid]


  ## Number of unique guids with a news score (just under the number of unique guid)

  news_score_table[order == 1, .N]


  ## Number of guids that have more than one news score

  news_score_table[order == 2, .N]


  ## Percentage of guids with a news score that have more than one

  news_score_table[order == 2, .N]/news_score_table[order == 1, .N]*100

  ## Tidy up

  rm(news_score_table)


## Variation of physical obvs example

  phys_obs <- copy(guid_phys_obs_table)

  phys_obs[, order := 1:.N, by = pseudo_guid]

  high_order_guid <- phys_obs[order == max(order), pseudo_guid]

  phys_obs[pseudo_guid %in% high_order_guid][order(pseudo_guid, observations_recorded_time)]

  rm(phys_obs, high_order_guid)


## One person, multiple contacts check

  check_nhs111 <- data.table(readRDS("D:/source_data/yas/nhs111/111_post_optout_final.rds"))

  setorder(check_nhs111, nhsnumber, call_commenced_date_time)

  check_nhs111[!is.na(nhsnumber), order := 1:.N, by = nhsnumber]

  check_nhs111[, time_diff := call_commenced_date_time - shift(call_commenced_date_time), by = nhsnumber]

  longest_time_between <- check_nhs111[!is.na(time_diff)][time_diff == max(time_diff), nhsnumber]

  check_nhs111[nhsnumber == (longest_time_between)]


  ## Percentage people who have a second contact over 1 week later

  check_nhs111[time_diff > 604800, .N]/length(unique(check_nhs111[, nhsnumber]))*100


  ## Final clinical assessment of second contact a week or more after first

  check_nhs111[time_diff > 604800, .N, by = finaldxdesc][order(-N)]


  ## What was first assessment of people with second contact

  multi_contact_number <- check_nhs111[time_diff > 604800, nhsnumber]

  check_nhs111[nhsnumber %in% multi_contact_number & order == 1, .N, by = finaldxdesc][order(-N)]

  ## Tidy up

  rm(check_nhs111, longest_time_between, multi_contact_number)


## Do duplicate records in CAD data have ePR data or not

  erp_cad_copy <- copy(epr_cad)
  nonepr_cad_copy <- copy(nonepr_cad)

  erp_cad_copy[, record_ID := NULL]
  nonepr_cad_copy[, record_ID := NULL]

  cad_duplicated <- fintersect(erp_cad_copy, nonepr_cad_copy)


  ## Percentage of duplicates removed from full total of records

  cad_duplicated[, .N]/(erp_cad_copy[, .N] + nonepr_cad_copy[, .N]) * 100


  ## Percentage of duplicates that have a scene arrival time

  cad_duplicated[!is.na(scene_arrival_time), .N]/cad_duplicated[, .N] * 100


  ##Pseudonymise  call number (same salt so can link with ePR still)

  cad_duplicated[, call_number := as.character(call_number)]
  cad_duplicated[, pseudo_call_number := fn_pseudonymiseValues(call_number, salt_filename = "private/salt1.txt")][, call_number := NULL]


  ## Work out the percentage of duplicate cad call numbers that have an ePR record

  dup_with_epr_percentage <- length(cad_duplicated[, pseudo_call_number] %in% guid_single_value_table[, pseudo_call_number])/cad_duplicated[, .N] * 100


