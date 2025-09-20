# Copyright 2025 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

# ==============================================================================
# Script name:      03d_Forecast_Performance.R
# ------------------------------------------------------------------------------
# Script version:
# 2025-09-19:       v1 - JG
#
# ------------------------------------------------------------------------------
# Notes/Objectives:
# script for tracking recent model performance, plots, and putting on
#     objectstore
#
# ==============================================================================


library(dplyr)
library(ggplot2)
library(readr)
library(showtext)

# create folder
figure_location <- "output/performance_tracking/"
file_loc <- normalizePath(figure_location)
dir.create(file_loc, showWarnings = FALSE)


# Get well info
pgown_well_info_all <- read_csv("user_inputs/Forecasting_Model_Data.csv")

# read timeseries
timeseries <- read_rds("https://nrs.objectstore.gov.bc.ca/rfc-conditions/timeseries_data/PGOWN_Well_Level_Daily_Data.rds")

# read archive
archive <- read_rds("https://nrs.objectstore.gov.bc.ca/rfc-conditions/groundwater_forecast/outputs/previous_forecasts/daily_csv/predictive_forecast_results_archive.rds")

font_add("BCSans", normalizePath("docs/fonts/BCSans-Regular.ttf"))
showtext_auto()

# loop through each well and save plot
for (well_id in sort(unique(archive$Well))) {
  #well_id <-  "OW502"

  print(well_id)

  well_info <- pgown_well_info_all %>%
    filter(Well == well_id)

  archive_well <- archive %>%
    filter(Well == well_id) %>%
    mutate(lag_day = paste0(lag_day, "-Day Forecast"))

  if (!all(is.na(archive_well$Date_predicted))) {

    end_date <- max(archive_well$Date_predicted, na.rm = TRUE) + 7

    timeseries_well <- timeseries %>%
      filter(Well == well_id) %>%
      filter(!is.na(Value))

    timeseries_well_recent <- timeseries_well %>%
      filter(Date >= Sys.Date() - 365)

    statistics_well <- timeseries_well %>%
      filter(Date >= "2004-01-01") %>%
      filter(Date < end_date - 365) %>%
      filter(!Month == 2 | !Day == 29) %>%
      group_by(Month, Day) %>%
      summarise(Min = min(Value),
                P10 = quantile(Value, 0.1),
                P25 = quantile(Value, 0.25),
                P50 = quantile(Value, 0.5),
                P75 = quantile(Value, 0.75),
                P90 = quantile(Value, 0.9),
                Max = max(Value),
                .groups = "keep")

    stats_well_two_years <- bind_rows(
      statistics_well %>%
        mutate(Date = as.Date(paste0(lubridate::year(Sys.Date())-1, "-", Month, "-", Day))),
      statistics_well %>%
        mutate(Date = as.Date(paste0(lubridate::year(Sys.Date()), "-", Month, "-", Day))),
      statistics_well %>%
        mutate(Date = as.Date(paste0(lubridate::year(Sys.Date())+1, "-", Month, "-", Day)))) %>%
      filter(Date < end_date) %>%
      filter(Date >= end_date-365) %>%
      left_join(timeseries_well_recent %>%
                  select(Date, Value),
                by = "Date")


    perf_plot <- ggplot(data = archive_well, aes(x = Date_predicted)) +
      geom_ribbon(data = stats_well_two_years, aes(x = Date, ymin = Min, ymax = Max), fill = "gray80") +
      geom_ribbon(data = stats_well_two_years, aes(x = Date, ymin = P10, ymax = P90), fill = "gray60") +
      geom_ribbon(data = stats_well_two_years, aes(x = Date, ymin = P25, ymax = P75), fill = "gray50") +
      geom_line(data = stats_well_two_years, aes(x = Date, y = P50), size = 0.2, colour = "black") +
      geom_ribbon(aes(ymin = predicted_value_10th, ymax = predicted_value_90th), fill = "blue", alpha = 0.3) +
      geom_ribbon(aes(ymin = predicted_value_25th, ymax = predicted_value_75th), fill = "blue", alpha = 0.6) +
      geom_line(aes(y = predicted_value_50th), colour = "darkblue")+
      facet_wrap(~lag_day)+
      geom_line(data = stats_well_two_years, aes(x = Date, y = Value), size = 1, colour = "red", alpha = 0.7) +
      scale_y_reverse()+
      scale_x_date(expand = c(0,0), date_breaks = "1 month", date_labels = "%b")+
      labs(x = NULL, y = "Water Depth Below Ground (m)",
           title = paste0("Groundwater Level Forecast Performance - ", well_id, " - ", well_info$Location),
           subtitle = "Gray: historic percentiles | Light blue: forecast 10-90th percentiles | Dark blue: forecast 25-75th percentiles | Red: observed water level",
           caption = paste0("As of ", format(Sys.Date(), "%b %d, %Y")))+
      theme_bw() +
      theme(text = element_text(family = "BCSans", size = 33),
            strip.background = element_rect(fill = "white", color = NA),
            strip.text = element_text(face = "bold", hjust = 0))
    perf_plot

    ggsave(filename = paste0(file_loc, "\\", well_id, "_performance.png"),
           plot = perf_plot,
           height = 6, width = 10)
  }
}
showtext_auto(FALSE)


# load figures onto objecstore

bucket <- "rfc-conditions/groundwater_forecast/outputs/performance_tracking"
region <- ""

# Recursively list all files in the directory and subdirectories
all_files <- list.files(file_loc, recursive = TRUE, full.names = TRUE)

for (file_path in all_files) {
  # file_path <- all_files[1]
  aws.s3::put_object(file = file_path,
                     object = basename(file_path),
                     bucket = bucket,
                     region = region,
                     headers = list(
                       `Content-Type` = "image/png",
                       `Content-Disposition` = "inline"
                     ),
                     acl = "public-read")
}


### Determine if model correctly forecast each conditions

# results <- archive %>%
#   left_join(timeseries %>%
#               select(Well, Date_predicted = Date, Actual_Level = Value, Actual_Percentile = Percentile),
#             by = join_by(Well, Date_predicted)) %>%
#   mutate(conditions = case_when(grepl("3) Below", conditions) ~ "Below Normal",
#                                 grepl("2) Normal", conditions) ~ "Normal",
#                                 grepl("1) Above", conditions) ~ "Above Normal"),
#          Actual_Conditions = case_when(Actual_Percentile < 25 ~ "Below Normal",
#                                        Actual_Percentile <= 75 ~ "Normal",
#                                        Actual_Percentile > 75 ~ "Above Normal"),
#          Likelihood_Number = as.numeric(gsub("<", "", gsub(">", "", likelihood))),
#          Likely_Condition = case_when(Likelihood_Number > 50 ~ conditions,
#                                       TRUE ~ NA),
#          Forecast_Match = ifelse(Actual_Conditions == Likely_Condition, TRUE, FALSE)) %>%
#   filter(!is.na(Likelihood_Number))
#
#
#
#
#

# forecast_analysis <- forecast %>%
#   group_by(Well, Model, Date_predicted, lag_day) %>%
#   mutate(actual_value,
#          predicted_value_50th,
#          RMSE = sqrt(mean((actual_value - predicted_value_50th)^2)),
#          MSE = mean((actual_value - predicted_value_50th)^2),
#          residual = abs(actual_value - predicted_value_50th),
#          ssr = sum((actual_value - predicted_value_50th)^2),
#          # sst = sum((actual_value - mean_data_day)^2),
#          min_data = min(actual_value),
#          max_data = max(actual_value)) %>%
#   select(Well, Model, Date_predicted, lag_day,
#          actual_value, predicted_value_50th, predicted_value_10th, predicted_value_90th,predicted_value_5th, predicted_value_95th,
#          predicted_value_min, predicted_value_max,
#          RMSE, MSE, residual, ssr, min_data, max_data)





# dates <- list.dirs("output/previous_forecasts/")
# dates <- dates[2:length(dates)]
# dates
# combined <- bind_rows(
#   lapply(dates, function(x){
#     readr::read_csv(paste0(x, "/predictive_forecast_results.csv")) %>%
#       mutate(likelihood = as.character(likelihood)) %>%
#       mutate(MODEL_DATE = basename(x)) %>%
#       select(MODEL_DATE, everything())
#
#   })
# )
#
# for_file_location <- "G:/R/projects/rfc-groundwater-forecast/outputs_objectstore/previous_forecasts/daily_csv"
# for_files <- list.files(path = for_file_location,
#                         pattern = "predictive_forecast_results")
#
# combined <- bind_rows(
#   lapply(for_files, function(x){
#     readr::read_csv(paste0(for_file_location, "/", x)) %>%
#       mutate(likelihood = as.character(likelihood)) %>%
#       mutate(MODEL_DATE = as.Date(gsub("_", "-", gsub("predictive_forecast_results_", "", gsub(".csv","", basename(x)))))) %>%
#       select(MODEL_DATE, everything())
#   })
# )
#
# combined2 <- combined %>%
#   select(MODEL_DATE,Well,Forecast_Date, Date_predicted,lag_day,Model,likelihood,Latest_Conditions,
#          conditions,Date,groundwater,predicted_value_mean,predicted_value_min,predicted_value_max,
#          predicted_value_25th,predicted_value_75th,predicted_value_50th,predicted_value_90th,predicted_value_10th,predicted_value_5th,
#          predicted_value_95th,performance)
# saveRDS(combined2, "output/previous_forecasts/daily_csv/predictive_forecast_results_archive.rds")
