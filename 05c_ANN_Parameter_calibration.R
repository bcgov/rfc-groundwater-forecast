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
# Script name:      05c_ANN_Parameter_calibration.R
# ------------------------------------------------------------------------------
# Script version:
# 2025-04-01:       v1
#
# ------------------------------------------------------------------------------
# Notes/Objectives:
# main script for determining ANN hyperparameters to use for wells being forecasted, generates nessasary inputs.
#
# ==============================================================================
#

## Load inputs and functions ---------------------------------------------------

source("01_ConfigInputs.R")
source("functions/dl_climate_data.R")
source("functions/dl_pgown_wl_data.R")
source("functions/dl_snow_data.R")


## Create directories  ---------------------------------------------------------
figure_location <- "Testing and Calibration/"

output_path <- paste0(figure_location, "model calibration/ANN_hyperparameters/", as.character(Sys.Date()))
dir.create(paste0(figure_location, "model calibration/"), showWarnings = FALSE)
dir.create(paste0(figure_location, "model calibration/ANN_hyperparameters/"), showWarnings = FALSE)
dir.create(output_path, showWarnings = FALSE)

## Get data --------------------------------------------------------------------

pgown_well_info_all <- read_csv(paste0(user_input_location, "ANN_hyperparameter_inputs.csv")) %>%
  filter(!is.na(Climate_station_Id) & !is.na(Lag_time)) %>%
  mutate(Climate_Infilled_id = ifelse(is.na(Climate_Infilled_id), 0, Climate_Infilled_id),
         Climate_secondary_Infilled = ifelse(is.na(Climate_secondary_Infilled), 0, Climate_secondary_Infilled),
         Climate_tertiary_Infilled = ifelse(is.na(Climate_tertiary_Infilled), 0, Climate_tertiary_Infilled),
         Climate_quaternary_Infilled = ifelse(is.na(Climate_quaternary_Infilled), 0, Climate_quaternary_Infilled))

Regional_group_list <- pgown_well_info_all %>%
  #  filter(Well == "OW473") %>%
  dplyr::select(Regional_group) %>%
  distinct(Regional_group) %>%
  dplyr::pull(Regional_group)

Regional_group_list <- as.list(Regional_group_list)


## Loop through each region and run calibration scripts  -----------------------

forecast_test_length <- 90

# Number of cores (for parallel computing)
num_cores <- 4

for (i in Regional_group_list){

  pgown_well_info <- pgown_well_info_all %>%
    filter(Regional_group == i)


  ## Downloads -------------------------------------------------------------------

  climate_data <- dl_climate_data(pgown_well_info, data_location)

  pgown_data <- dl_pgown_wl_data(pgown_well_info, data_location)

  snow_data <- dl_snow_data(pgown_well_info, data_location)



  Time_series_data <- full_join(pgown_data,climate_data) %>%
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





  #### Validation testing ####
  source("functions/ANN_hyperparameter_calibration.R")


  ANN_hyper_parameters <- ANN_hyper_parameters_calibration(Time_series_data, pgown_well_info,
                                                           forecast_test_length, num_cores,
                                                           output_path)

  #summarise Statistics
  location <- paste0(output_path, "/ANN_hypercalibration_results_", i, ".csv")
  write.csv(ANN_hyper_parameters, file = location, row.names = FALSE)


}


# Merge input file with output to create prep file for model testing

list_of_results <- list.files(path = output_path, pattern = "ANN_hypercalibration_results",
                              full.names = TRUE)

combined <- bind_rows(
  lapply(list_of_results, function(file){

    read.csv(file = file, stringsAsFactors = FALSE)
  })) %>%
  select(Well, ann_size, ann_decay,	ann_maxit)

prep_file <- read_csv(paste0(user_input_location, "ANN_hyperparameter_inputs.csv")) %>%
  mutate(station_name_RF_forecast = "")

data_out <- left_join(prep_file, combined)

write.csv(data_out,
          paste0(output_path,
                 "/Model_testing_inputs_prep.csv"), row.names = FALSE)

