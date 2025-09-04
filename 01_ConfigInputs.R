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
# Script name:      01_ConfigInputs.R
# ------------------------------------------------------------------------------
# Script version:
# 2025-09-04:       v4 - JG
#
# ------------------------------------------------------------------------------
# Notes/Objectives:
# loads common libraries, sets default switches, colours, ggplot formatting
#
# ==============================================================================

# Test if running on Github Actions
is_github_actions <- Sys.getenv("GITHUB_ACTIONS") == "true"

# Set library path for cache
if (is_github_actions) {
  message("Running inside GitHub Actions")
  Sys.setenv(R_LIBS_USER = "~/.local/share/R/library")
  dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE, showWarnings = FALSE)
  .libPaths(Sys.getenv("R_LIBS_USER"))
} else {
  message("Running locally")
}


## LOAD CRAN PACKAGES --------------------------------------------------
pkgs <- c('dplyr',
          'ggplot2',
          'tidyr',
          'lubridate',
          'readr',
          'stringr',
          'tibble',
          'forcats',
          'purrr',
          'padr',
          'zoo',
          'ggplot2',
          'lubridate',
          'padr',
          'zoo',
          'ggplot2',
          'patchwork',
          'readxl',
          'RSQLite',
          'randomForest',
          'ggnewscale',
          'forecast',
          'mgcv',
          'stats',
          'xts',
          'powerjoin',
          'foreach',
          'doParallel',
          'reshape',
          'dplyr',
          "janitor",
          "ggpubr",
          "sp",
          "sf",
          "nnet",
          "cowplot",
          "magick",
          "grid",
          "xgboost",
          'gt',
          'leaflet',
          'htmlwidgets',
          'pak')

#Queries and installs missing packages
new.packages <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Non-cran packages
pkgs_github <- c('weathercan',
                 'bcsnowdata')
new.packages_github <- pkgs_github[!(pkgs_github %in% installed.packages()[,"Package"])]

if ('weathercan' %in% new.packages_github) {
  pak::pak("ropensci/weathercan", lib = Sys.getenv("R_LIBS_USER"))
}
if ('bcsnowdata' %in% new.packages_github) {
  pak::pak("bcgov/bcsnowdata", lib = Sys.getenv("R_LIBS_USER"))
}

# load packages
pkgs <- c(pkgs, pkgs_github)
lapply(pkgs, library, character.only = TRUE)



# MANUAL INPUTS ----------------------------------------------------------------

# Choose location to save files
figure_location <- "output/"
data_location <- "data/"
model_path <- "models/"
user_input_location <- "user_inputs/"

# Create the folders if they don't exist
dir.create(figure_location, showWarnings = FALSE)
dir.create(data_location, showWarnings = FALSE)
dir.create(model_path, showWarnings = FALSE)
# dir.create(paste0(figure_location, "Model_results"), showWarnings = FALSE)


# Fill in well information based on location
pgown_well_info_all <- read_csv(paste0(user_input_location, "Forecasting_Model_Data.csv")) %>%
  filter(!is.na(Climate_station_Id) & !is.na(Lag_time)) %>%
  mutate(Climate_Infilled_id = ifelse(is.na(Climate_Infilled_id), 0, Climate_Infilled_id),
         Climate_secondary_Infilled = ifelse(is.na(Climate_secondary_Infilled), 0, Climate_secondary_Infilled),
         Climate_tertiary_Infilled = ifelse(is.na(Climate_tertiary_Infilled), 0, Climate_tertiary_Infilled),
         Climate_quaternary_Infilled = ifelse(is.na(Climate_quaternary_Infilled), 0, Climate_quaternary_Infilled))

Regional_group_list <- pgown_well_info_all %>%
  dplyr::select(Regional_group) %>%
  distinct(Regional_group) %>%
  dplyr::pull(Regional_group)

Regional_group_list <- as.list(Regional_group_list)


# User set number of days to forecast
forecast_days <- c(14, 30, 60, 90)

# Number of cores (for parallel computing)
num_cores <- ifelse(is_github_actions, 2, 4)

Missing_date_window <- 3

rfc_forecast_date_window <- 3

# rfc_forecast_include <- c("deterministic", "ensemble")
rfc_forecast_include <- c("deterministic")
# rfc_forecast_include <- NA

generate_well_pdf <- TRUE

# OPTIONS ----------------------------------------------------------------

options(digits = 3,
        scipen = 5,
        warn = 0,
        timeout = 1200)

Sys.setenv(TZ = "America/Vancouver")

