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

## Create directories  ---------------------------------------------------------

output_path <- paste0(figure_location, "previous_forecasts/", as.character(Sys.Date()))
dir.create(paste0(figure_location, "/"), showWarnings = FALSE)
dir.create(output_path, showWarnings = FALSE)


## Loop through each region and run forecasting scripts  -----------------------

for (i in Regional_group_list) {

  # i <- Regional_group_list[6]

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

  Time_series_data <- full_join(pgown_data, climate_data, by = c("Date", "Well")) %>%
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

  #### MODEL -------------------------------------------------------------------

  source("functions/Forecasting.R")

  Model_Forecasting_data <- forecast_model(Time_series_data, forecast_days,
                                           num_cores, figure_location, output_path,
                                           model_path, data_location, pgown_well_info,
                                           rfc_forecast_include,
                                           ensemble_forecast_data, deterministic_forecast_data,
                                           Missing_date_window, rfc_forecast_date_window)


  # Save the csv output
  write_csv(Model_Forecasting_data, paste0(output_path, "/predictive_forecast_results_", i, ".csv"))
  # write_csv(Model_Forecasting_data, paste0("Output//predictive_forecast_results_", i, ".csv"))

}


# Merge forecast files into one file

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

# data_out_simple <- data_out %>%
#   select(Well, )

write.csv(data_out, paste0("output/predictive_forecast_results.csv"), row.names = FALSE)




