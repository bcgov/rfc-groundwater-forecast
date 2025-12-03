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
# Script name:      03_Model_forecasting.R
# ------------------------------------------------------------------------------
# Script version:
# 2025-09-04:       v2 - JG
#
# ------------------------------------------------------------------------------
# Notes/Objectives:
# main script for generating forecasts
#
# ==============================================================================
#

## Load inputs and functions ---------------------------------------------------

source("01_ConfigInputs.R")
source("functions/dl_climate_data.R")
source("functions/dl_pgown_wl_data.R")
source("functions/dl_snow_data.R")
source("functions/dl_deterministic_forecast.R")
source("functions/dl_ensemble_forecast.R")

## Create directories  ---------------------------------------------------------

output_path <- file.path(figure_location, "previous_forecasts/", as.character(Sys.Date()))
dir.create(file.path(figure_location, "/"), showWarnings = FALSE, recursive = TRUE)
dir.create(output_path, showWarnings = FALSE, recursive = TRUE)
dir.create(paste0(figure_location, "/previous_forecasts/daily_pdf/"), showWarnings = FALSE)


## Loop through each region and run forecasting scripts  -----------------------


# Filter for wells to include in the model

pgown_well_info_all <- pgown_well_info_all %>%
  filter(Include_Well)

Regional_group_list <- pgown_well_info_all %>%
  dplyr::select(Regional_group) %>%
  distinct(Regional_group) %>%
  dplyr::pull(Regional_group)
Regional_group_list <- as.list(Regional_group_list)

# Regional_group_list <- Regional_group_list[6]

for (i in Regional_group_list) {

  # i <- Regional_group_list[6]
  # i <- Regional_group_list[4]

  message(paste0("Starting model group: ", i))

  pgown_well_info <- pgown_well_info_all %>%
    filter(Regional_group == i) #%>%
  #  filter(Well == "OW002")


  ## Downloads -----------------------------------------------------------------

  climate_data <- dl_climate_data(pgown_well_info, data_location)
  pgown_data <- dl_pgown_wl_data(pgown_well_info, data_location)
  snow_data <- dl_snow_data(pgown_well_info, data_location)
  ensemble_forecast_data <- dl_ensemble_forecast(pgown_well_info, data_location)
  deterministic_forecast_data <- dl_deterministic_forecast(pgown_well_info, data_location)

  # Merge timeseries data

  message("Joining timeseries data...")

  Time_series_data <- full_join(pgown_data, climate_data, by = c("Date", "Well")) %>%
    full_join(snow_data, by = c("Date", "Well")) %>%
    filter(Date >= as.Date("2004-01-01")) %>%
    pad(by = "Date", group = c("Well"), interval = "day") %>%
    group_by(Well) %>%
    fill(Snow_influenced, .direction = "downup") %>%
    mutate(SWE = ifelse(month(Date) >= 8 & month(Date) <= 10, ifelse(is.na(SWE), 0, SWE), SWE)) %>%
    fill(SWE, .direction = "downup") %>%
    mutate(groundwater_up_to_last_year = if_else(year(Date) <= year(Sys.Date()) - 1, groundwater, NA_real_)) %>%
    mutate(groundwater_period_average = mean(groundwater_up_to_last_year, na.rm = TRUE)) %>%
    mutate(groundwater = groundwater_period_average - groundwater) %>%
    dplyr::select(-groundwater_up_to_last_year) %>%
    ungroup() %>%
    distinct()

  #### MODEL -------------------------------------------------------------------

  message("Running forecast model...")

  source("functions/Forecasting.R")

  Model_Forecasting_data <- forecast_model(Time_series_data, forecast_days,
                                           num_cores, figure_location, output_path,
                                           model_path, data_location, pgown_well_info,
                                           rfc_forecast_include,
                                           ensemble_forecast_data, deterministic_forecast_data,
                                           Missing_date_window, rfc_forecast_date_window,
                                           generate_well_pdf)

  # Save the csv output
  write_csv(Model_Forecasting_data, paste0(output_path, "/predictive_forecast_results_", i, ".csv"))
  # write_csv(Model_Forecasting_data, paste0("Output//predictive_forecast_results_", i, ".csv"))

}


message("Combining forecasts...")

####  Combine pdf outputs

list_of_pdfs <- list.files(path = output_path,
                           pattern = "\\.pdf$",
                           full.names = TRUE)

qpdf::pdf_combine(input = list_of_pdfs, output = normalizePath(paste0(output_path, "/Model_Forecasts.pdf")))
# qpdf::pdf_combine(input = list_of_pdfs, output = normalizePath(paste0(figure_location, "previous_forecasts/daily_pdf/Model_Forecasts_", Sys.Date(), ".pdf")))
qpdf::pdf_combine(input = list_of_pdfs, output = normalizePath(paste0(figure_location, "Model_Forecasts.pdf")))


####  Combine forecast files into single outputs

list_of_results <- list.files(path = output_path,
                              pattern = "predictive_forecast_results_",
                              full.names = TRUE)

combined <- bind_rows(
  lapply(list_of_results, function(forecast_file){
    read_csv(file = forecast_file) %>%
      mutate(Date_predicted = as.Date(Date_predicted))
  }))# %>%
# filter(!is.na(lag_day))

data_out <- combined %>%  # format and add some links ready for mapping/ArcGIS online
  left_join(pgown_well_info_all)

write.csv(data_out, paste0(output_path, "/predictive_forecast_results.csv"), row.names = FALSE)
write.csv(data_out, paste0("output/predictive_forecast_results.csv"), row.names = FALSE)


## Save to archive

# prep the data for appending
new_data <- data_out %>%
  mutate(likelihood = as.character(likelihood)) %>%
  mutate(MODEL_DATE = Sys.Date()) %>%
  select(MODEL_DATE, everything()) %>%
  select(MODEL_DATE,Well,Forecast_Date, Date_predicted,lag_day,Model,likelihood,Latest_Conditions,
         conditions,Date,groundwater,predicted_value_mean,predicted_value_min,predicted_value_max,
         predicted_value_25th,predicted_value_75th,predicted_value_50th,predicted_value_90th,predicted_value_10th,predicted_value_5th,
         predicted_value_95th,performance)

# read in the archive and append the latest forecast
archive <- read_rds("https://nrs.objectstore.gov.bc.ca/rfc-conditions/groundwater_forecast/outputs/previous_forecasts/daily_csv/predictive_forecast_results_archive.rds") %>%
  filter(MODEL_DATE < Sys.Date()) %>%
  bind_rows(new_data)

# save the rds file
file_path <- "data/predictive_forecast_results_archive.rds"
saveRDS(archive, file_path)

# Save the rds file onto objectstore
bucket <- "rfc-conditions/groundwater_forecast/outputs"
region <- ""
aws.s3::put_object(file = file_path,
                   object = paste0("predictive_forecast_results_archive.rds"),
                   bucket = paste0(bucket, "/previous_forecasts/daily_csv"),
                   region = region,
                   acl = "public-read")


# Create table for csv output for ArcGIS online and leaflet mapping

# merge and format data for output
data_table_out <- data_out %>%
  mutate(Likelihood_Category = case_when(likelihood > 90 ~ ">90% - Extremely Likely",
                                         likelihood > 75 ~ "75-90% - Very Likely",
                                         likelihood > 50 ~ "50-75% - Likely",
                                         likelihood > 25 ~ "25-50% - Somewhat Likely",
                                         likelihood >= 10 ~ "10-25% - Unlikely",
                                         likelihood >= 0  ~ "<10% - Very Unlikely",
                                         is.na(likelihood) ~ "Not Available"),
         Likelihood_Category = factor(Likelihood_Category, levels = c(">90% - Extremely Likely",
                                                                      "75-90% - Very Likely",
                                                                      "50-75% - Likely",
                                                                      "25-50% - Somewhat Likely",
                                                                      "10-25% - Unlikely",
                                                                      "<10% - Very Unlikely",
                                                                      "Not Available"))) %>%
  select(Well, Location, Region, Latitude = latitude, Longitude = longitude,
         Aquifer_ID = aquifer_id, Aquifer_Subtype = subtype_sym,
         Forecast_Date, Latest_Date = Date, Latest_Conditions, Predicted_Date = Date_predicted,
         Forecast_Days = lag_day, Conditions = conditions,
         Likelihood = likelihood, Likelihood_Category,
         Forecast_Performance = performance, Snow_Influenced = Snow_influenced,
         Predicted_Min = predicted_value_min, Predicted_10th = predicted_value_10th,
         Predicted_25th = predicted_value_25th,
         Predicted_Mean = predicted_value_mean, Predicted_Median = predicted_value_50th,
         Predicted_75th = predicted_value_75th, Predicted_90th = predicted_value_90th,
         Predicted_Max = predicted_value_max,
         Well_Tag_number) %>%
  mutate(Forecast_URL = paste0("https://nrs.objectstore.gov.bc.ca/rfc-conditions/groundwater_forecast/outputs/", Well, "_Model_Forecast.pdf"),
         Technical_Forecast_URL = paste0("https://nrs.objectstore.gov.bc.ca/rfc-conditions/groundwater_forecast/outputs/", Well, "_Model_Forecast.jpeg"),
         Realtime_URL = paste0("https://bcmoe-prod.aquaticinformatics.net/Data/Location/Summary/Location/", Well, "/Interval/Latest"),
         Aquifer_URL = ifelse(is.na(Aquifer_ID), "", paste0("https://apps.nrs.gov.bc.ca/gwells/aquifers/", Aquifer_ID)),
         Well_URL = ifelse(is.na(Well_Tag_number), "", paste0("https://apps.nrs.gov.bc.ca/gwells/well/", Well_Tag_number)),
         Interactive_Hydrograph_URL = paste0("https://bcrfc.env.gov.bc.ca/Real-time_Data/Interactive_Q_process/InteractivePlots/groundwater_levels/", Well, "_dailyWL.html"),
         Static_Hydrograph_URL = paste0("https://bcrfc.env.gov.bc.ca/Real-time_Data/Interactive_Q_process/InteractivePlots/groundwater_levels/", Well, "_dailyWL.png"),
         Issued_At = format(Sys.time(), "%Y-%m-%d %H:%M")) %>%
  select(-Well_Tag_number)

# Save outputs
write.csv(data_table_out, paste0(output_path, "/RFC_GW_Forecast.csv"), row.names = FALSE)
write.csv(data_table_out, paste0("output/RFC_GW_Forecast.csv"), row.names = FALSE)

#### Make table of most likely conditions for each well for mapping and other

well_likely_conditions <- data_out %>%
  mutate(conditions = case_when(conditions == "1) Above Normal - above 75th percentile" ~ "Above Normal",
                                conditions == "2) Normal - 25th to 75th percentile" ~ "Normal",
                                conditions == "3) Below Normal - below 25th percentile" ~ "Below Normal",
                                conditions == "No forecast available (no recent data)" ~ "Not Available")) %>%
  group_by(Well, lag_day) %>%
  summarise(
    Likely_Conditions = {

      # Replace NA conditions

      # Which conditions meet the thresholds
      high <- conditions[likelihood > 50]
      med  <- conditions[likelihood >= 40]

      if (length(high) > 0) {
        str_c(high, collapse = " to ")
      } else if (length(med) >= 2) {
        str_c(med, collapse = " to ")
      } else {
        "Uncertain"
      }
    },
    .groups = "drop"
  ) %>%
  mutate(Likely_Conditions = ifelse(is.na(Likely_Conditions), "Not Available", Likely_Conditions)) %>%
  ungroup() %>%
  dplyr::rename(Forecast_Days = lag_day) %>%
  left_join(data_table_out %>%
              select(Well, Location, Region, Latitude, Longitude,
                     Aquifer_ID, Aquifer_Subtype,
                     Forecast_Date, Latest_Date, Latest_Conditions, Predicted_Date,
                     Forecast_Days,
                     Forecast_Performance, Snow_Influenced,
                     Forecast_URL, Technical_Forecast_URL, Realtime_URL,
                     Aquifer_URL, Well_URL, Interactive_Hydrograph_URL,
                     Static_Hydrograph_URL, Issued_At) %>%
              unique(),
            by = join_by(Well, Forecast_Days))

# Save outputs
write.csv(well_likely_conditions, paste0(output_path, "/RFC_GW_Conditions_Forecast.csv"), row.names = FALSE)
write.csv(well_likely_conditions, paste0("output/RFC_GW_Conditions_Forecast.csv"), row.names = FALSE)
