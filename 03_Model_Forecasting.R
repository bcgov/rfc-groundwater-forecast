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
# 2025-04-01:       v1
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

#create a new directory to save the figures too based on the date.

figure_location <- "Output/"

# Specify the path where you want to create the new folder
output_path <- paste0(figure_location, "Model_results/", as.character(Sys.Date()))

# Create new folder
dir.create(output_path)



for(i in Regional_group_list){

  # i <- Regional_group_list[6]

  pgown_well_info <- pgown_well_info_all %>%
    filter(Regional_group == i)


  ## Downloads -------------------------------------------------------------------

  climate_data <- dl_climate_data(pgown_well_info, data_location)

  pgown_data <- dl_pgown_wl_data(pgown_well_info, data_location)

  snow_data <- dl_snow_data(pgown_well_info, data_location)

  ensemble_forecast_data <- dl_ensemble_forecast(pgown_well_info, data_location)

  deterministic_forecast_data <- dl_deterministic_forecast(pgown_well_info, data_location)



  # Combine data

  Time_series_data <- full_join(pgown_data, climate_data) %>%
    full_join(snow_data, by = c("Date", "Well")) %>%
    filter(Date >= as.Date("2004-01-01")) %>%
    pad(by = "Date", group = c("Well")) %>%
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





  #### MODEL ####

  source("functions/Forecasting.R")

  Model_Forecasting_data <- forecast_model(Time_series_data, forecast_days,
                                           num_cores, figure_location, output_path,
                                           model_path, data_location, pgown_well_info,
                                           ensemble_forecast_data, deterministic_forecast_data,
                                           Missing_date_window, rfc_forecast_date_window)


  write_csv(Model_Forecasting_data,paste0(output_path, "/predictive_forecast_results_", i, ".csv"))

}





