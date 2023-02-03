library(data.table)
library(ggplot2)


desc_stats_func <- function(data_table, col_name) {

  desc_stat <- data.table(variable = gsub("_", " ", col_name))

  desc_stat[, count := length(data_table[, get(col_name)])]
  desc_stat[, missing_values := data_table[is.na(get(col_name)), .N]]
  desc_stat[, missing_percent := round((missing_values/count)*100, digits = 2)]

  if(typeof(data_table[, get(col_name)]) == "integer" | typeof(data_table[, get(col_name)]) == "double")  {

     desc_stat[, range := data_table[, max(get(col_name), na.rm = TRUE)] - data_table[, min(get(col_name), na.rm = TRUE)]]
     desc_stat[, mean := data_table[, round(mean(get(col_name), na.rm = TRUE), digits = 2)]]
     desc_stat[, median := data_table[, median(get(col_name), na.rm = TRUE)]]

  }

  if(col_name == "age_years") {

    desc_stat[, under_sixteen := data_table[get(col_name) < 16, .N]]
  }

  return(desc_stat)

}


#variable_plot <- ggplot(data_table[!is.na(get(col_name)), .N, by = col_name][order(get(col_name))],
#                          aes(x = get(col_name), y = N)) + geom_line() + geom_point() + geom_smooth() + ggtitle("Distribution of ")


## Bind together CAD datasets and remove duplicates

  cad <- rbind(epr_cad, nonepr_cad)

  setorder(cad, call_number, age, sex, na.last = TRUE)

  cad[, order := 1:.N, by = call_number]

  cad_dup_calls_removed <- cad[order == 1][, order := NULL]


## Create age groups for each of the datasets

  guid_single_value_table[, age_group := cut(age_years, c(seq(0, 110, 10), Inf), include.lowest = TRUE)]

  cad_dup_calls_removed[, age_group := cut(age_years, c(seq(0, 110, 10), Inf), include.lowest = TRUE)]

  nhs111[, age_group := cut(age_years, c(seq(0, 110, 10), Inf), include.lowest = TRUE)]



# Descriptive stats -------------------------------------------------------


## ePR data

  epr_news_score_table <- desc_stats_func(guid_news_score_table, "news_score")

  epr_age_table <- desc_stats_func(guid_single_value_table, "age_years")

  epr_ethnicity_percentages <- guid_single_value_table[, .N, by = ethnicity][, percentage := round((N/sum(N))*100, digits = 2)][order(-N)]

  epr_gender_percentages <- guid_single_value_table[, .N, by = sex][, percentage := round((N/sum(N))*100, digits = 2)][order(-N)]


## Physical Observation stats

  ## Take only first set of observations per person (don't have to worry about daylight savings [yet!], as data does not cover clocks going back in October)

  setorder(guid_phys_obs_table, pseudo_guid, observations_recorded_time, capillary_refill_time, na.last = TRUE)

  guid_phys_obs_table[!is.na(observations_recorded_time), order := 1:.N, by = c("pseudo_guid")]

  first_physical_observations <- guid_phys_obs_table[order == 1][, order := NULL]


#### Can/should we be using "observation_type", as this is meant to state if it was a primary or secondary observation, but times do not match up ###

## should all of these be done on the first for that field that we have available?? ###

  epr_avpu_score_percentages <- first_physical_observations[, .N, by = avpu_score][, percentage := round((N/sum(N))*100, digits = 2)][order(-N)]

  epr_eye_component_score_percentages <- first_physical_observations[, .N, by = eye_component_score][, percentage := round((N/sum(N))*100, digits = 2)][order(eye_component_score)]

  epr_motor_component_score_percentages <- first_physical_observations[, .N, by = motor_component_score][, percentage := round((N/sum(N))*100, digits = 2)][order(motor_component_score)]

  epr_total_component_score_percentages <- first_physical_observations[, .N, by = total_component_score][, percentage := round((N/sum(N))*100, digits = 2)][order(total_component_score)]

  epr_verbal_component_score_percentages <- first_physical_observations[, .N, by = verbal_component_score][, percentage := round((N/sum(N))*100, digits = 2)][order(verbal_component_score)]

  epr_observation_type_percentages <- first_physical_observations[, .N, by = observation_type][, percentage := round((N/sum(N))*100, digits = 2)][order(-N)]


  epr_physical_obvs_table <- desc_stats_func(first_physical_observations, "blood_sugar_reading")

  epr_physical_obvs_table <- rbind(epr_physical_obvs_table, desc_stats_func(first_physical_observations, "bp_diastolic"))

  epr_physical_obvs_table <- rbind(epr_physical_obvs_table, desc_stats_func(first_physical_observations, "bp_systolic"))

  epr_physical_obvs_table <- rbind(epr_physical_obvs_table, desc_stats_func(first_physical_observations, "manual_pulse_rate"))

  epr_physical_obvs_table <- rbind(epr_physical_obvs_table, desc_stats_func(first_physical_observations, "respiratory_rate"))

  epr_physical_obvs_table <- rbind(epr_physical_obvs_table, desc_stats_func(first_physical_observations, "oxygen_saturations"))

  epr_physical_obvs_table <- rbind(epr_physical_obvs_table, desc_stats_func(first_physical_observations[obs_supplimental_oxygen == TRUE], "oxygen_saturations")[, variable := "oxygen saturates with supplimental oxygen"])

  epr_physical_obvs_table <- rbind(epr_physical_obvs_table, desc_stats_func(first_physical_observations[obs_supplimental_oxygen == FALSE], "oxygen_saturations")[, variable := "oxygen saturates without supplimental oxygen"])

  epr_physical_obvs_table <- rbind(epr_physical_obvs_table, desc_stats_func(first_physical_observations, "temperature"))

  setnames(epr_physical_obvs_table, "variable", "Observation Field")


## CAD Data

  cad_desc_stat_table <- desc_stats_func(cad_dup_calls_removed, "age_years")

  cad_gender_percentages <- cad_dup_calls_removed[, .N, by = sex][, percentage := round((N/sum(N))*100, digits = 2)][order(-N)]


## NHS 111 Data

  nhs111_desc_stat_table <- desc_stats_func(nhs111, "age_years")

  nhs111_gender_percentages <- nhs111[, .N, by = sex][, percentage := round((N/sum(N))*100, digits = 2)][order(-N)]



# Plots -------------------------------------------------------------------

## ePR Plots

  new_score_plot <- ggplot(guid_news_score_table[!is.na(news_score), .N, by = news_score][order(news_score)],
                          aes(x = news_score, y = N)) + geom_line() + geom_point() + geom_smooth() + ggtitle("Distribution of News Scores - ePR data")


  epr_age_plot <- ggplot(guid_single_value_table[!is.na(age_years), .N, by = age_years][order(age_years)],
                           aes(x = age_years, y = N)) + geom_line() + geom_point() + geom_smooth() + ggtitle("Distribution of Age - ePR data")

  epr_age_group_plot <- ggplot(guid_single_value_table[!is.na(age_group), .N, by = age_group][order(age_group)],
                               aes(x = age_group, y = N)) + geom_bar(stat = "identity")+ geom_smooth() + ggtitle("Distribution of Age - ePR data")


## ePR physical obvs Plots

  blood_sugar_plot <- ggplot(first_physical_observations[!is.na(blood_sugar_reading), .N, by = blood_sugar_reading][order(blood_sugar_reading)],
                           aes(x = blood_sugar_reading, y = N)) + geom_line() + geom_point() + geom_smooth() + ggtitle("Distribution of Blood Sugar Reading - ePR data")

  diastolic_plot <- ggplot(first_physical_observations[!is.na(bp_diastolic), .N, by = bp_diastolic][order(bp_diastolic)],
                             aes(x = bp_diastolic, y = N)) + geom_line() + geom_point() + geom_smooth() + ggtitle("Distribution of Diastolic Blood Pressure - ePR data")

  systolic_plot <- ggplot(first_physical_observations[!is.na(bp_systolic), .N, by = bp_systolic][order(bp_systolic)],
                             aes(x = bp_systolic, y = N)) + geom_line() + geom_point() + geom_smooth() + ggtitle("Distribution of Systolic Blood Pressure - ePR data")

  manual_pulse_rate_plot <- ggplot(first_physical_observations[!is.na(manual_pulse_rate), .N, by = manual_pulse_rate][order(manual_pulse_rate)],
                          aes(x = manual_pulse_rate, y = N)) + geom_line() + geom_point() + geom_smooth() + ggtitle("Distribution of Manual Pulse Rate - ePR data")

  oxygen_saturation_all_plot <- ggplot(first_physical_observations[!is.na(oxygen_saturations), .N, by = oxygen_saturations][order(oxygen_saturations)],
                                       aes(x = oxygen_saturations, y = N)) + geom_line() + geom_point() + geom_smooth() + ggtitle("Distribution of Oxygen Saturation - ePR data")

  oxygen_saturation_plot <- ggplot(first_physical_observations[!is.na(oxygen_saturations), .N, by = .(oxygen_saturations, obs_supplimental_oxygen)][order(oxygen_saturations)],
                                   mapping = aes(x = oxygen_saturations, y = N, color = obs_supplimental_oxygen)) + geom_line() + geom_point() + geom_smooth() + ggtitle("Distribution of Oxygen Saturation - ePR data")


  oxygen_saturation_with_suppliment_plot <- ggplot(first_physical_observations[!is.na(oxygen_saturations) & obs_supplimental_oxygen == TRUE, .N, by = oxygen_saturations][order(oxygen_saturations)],
                                                   aes(x = oxygen_saturations, y = N)) + geom_line() + geom_point() + geom_smooth() + ggtitle("Distribution of oxygen saturation with supplimental oxygen  - ePR data")

  oxygen_saturation_no_supplimental_plot <- ggplot(first_physical_observations[!is.na(oxygen_saturations) & obs_supplimental_oxygen == FALSE, .N, by = oxygen_saturations][order(oxygen_saturations)],
                                                   aes(x = oxygen_saturations, y = N)) + geom_line() + geom_point() + geom_smooth() + ggtitle("Distribution of oxygen saturation without supplimental oxygen- ePR data")

  temperature_plot <- ggplot(first_physical_observations[!is.na(temperature), .N, by = temperature][order(temperature)],
                                   aes(x = temperature, y = N)) + geom_line() + geom_point() + geom_smooth() + ggtitle("Distribution of Temperature - ePR data")


## CAD Plots

  cad_age_plot <- ggplot(cad_dup_calls_removed[!is.na(age_years), .N, by = age_years][order(age_years)],
                         aes(x = age_years, y = N)) + geom_line() + geom_point() + geom_smooth() + ggtitle("Distribution of Age - CAD data")

  cad_age_group_plot <- ggplot(cad_dup_calls_removed[!is.na(age_group), .N, by = age_group][order(age_group)],
                                  aes(x = age_group, y = N)) + geom_bar(stat = "identity")+ geom_smooth() + ggtitle("Distribution of Age - CAD data")



## NHS 111 Plots

  nhs111_age_plot <- ggplot(nhs111[!is.na(age_years), .N, by = age_years][order(age_years)],
                             aes(x = age_years, y = N)) + geom_line() + geom_point() + geom_smooth() + ggtitle("Distribution of Age - NHS 111 data")

  nhs111_age_group_plot <- ggplot(nhs111[!is.na(age_group), .N, by = age_group][order(age_group)],
                            aes(x = age_group, y = N)) + geom_bar(stat = "identity")+ geom_smooth() + ggtitle("Distribution of Age - NHS 111 data")





