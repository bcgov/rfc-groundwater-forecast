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
# Script name:      03_CCF_lag_analysis.R
# ------------------------------------------------------------------------------
# Script version:
# 2025-04-01:       v3
#
# ------------------------------------------------------------------------------
# Notes/Objectives:
# main script for determining recharge lag to use for wells being forecasted, generates nessasary inputs.
#
# ==============================================================================
#

## Load inputs and functions ---------------------------------------------------

source("01_ConfigInputs.R")
source("functions/dl_climate_data.R")
source("functions/dl_pgown_wl_data.R")
source("functions/dl_snow_data.R")

## Get data --------------------------------------------------------------------

pgown_well_info <- read_csv(paste0(user_input_location, "CCF_lag_analysis_inputs.csv")) %>%
  filter(!is.na(Climate_station_Id)) %>%
  mutate(Climate_Infilled_id = ifelse(is.na(Climate_Infilled_id), 0, Climate_Infilled_id),
         Climate_secondary_Infilled = ifelse(is.na(Climate_secondary_Infilled), 0, Climate_secondary_Infilled),
         Climate_tertiary_Infilled = ifelse(is.na(Climate_tertiary_Infilled), 0, Climate_tertiary_Infilled),
         Climate_quaternary_Infilled = ifelse(is.na(Climate_quaternary_Infilled), 0, Climate_quaternary_Infilled))


## Downloads -------------------------------------------------------------------

climate_data <- dl_climate_data(pgown_well_info, data_location)
pgown_data <- dl_pgown_wl_data(pgown_well_info, data_location)
snow_data <- dl_snow_data(pgown_well_info, data_location)


## All Time Series Data --------------------------------------------------------

Time_series_data <- full_join(pgown_data, climate_data, by = c("Date", "Well")) %>%
  full_join(snow_data, by = c("Date", "Well")) %>%
  filter(Date >= as.Date("2004-01-01")) %>%
  # pad(by = "Date", group = c("Well")) %>%
  group_by(Well) %>%
  complete(Date = seq.Date(min(Date), max(Date), by = "day")) %>% # replace pad()
  fill(Snow_influenced, .direction = "downup") %>%
  mutate(SWE = ifelse(month(Date) >= 8 & month(Date) <= 10, ifelse(is.na(SWE), 0, SWE), SWE)) %>%
  fill(SWE, .direction = "downup") %>%
  mutate(groundwater_up_to_last_year = if_else(year(Date) <= year(Sys.Date()) - 1, groundwater, NA_real_)) %>%
  mutate(groundwater_period_average = mean(groundwater_up_to_last_year, na.rm = TRUE)) %>%
  mutate(groundwater = groundwater_period_average - groundwater) %>%
  dplyr::select(-groundwater_up_to_last_year) %>%
  ungroup() %>%
  distinct() %>%
  mutate(precipitation_30_day = rollmean(total_precip, 30, fill = NA, align = 'right', na.rm = TRUE)) %>%
  mutate(precip_average = mean(precipitation_30_day, na.rm = TRUE)) %>%
  mutate(precipitation_30_day = precipitation_30_day-precip_average) %>%
  mutate(SWE = - SWE) %>%
  mutate(SWE_diff = SWE - lag(SWE, 1))



## Loop through each well and do lag analysis  ---------------------------------

Well_list <- as.list(unique(unlist(Time_series_data$Well)))

ccf_data <- bind_rows(
  lapply(Well_list, function(x){

    # Convert date to Date object
    temp <- Time_series_data %>%
      filter(Well == x)

    print(unique(temp$Snow_influenced))

    print(x)

    if (unique(temp$Snow_influenced == 0)) {

      temp <- temp %>%
        select(-c(SWE, SWE_diff)) %>%
        drop_na(precipitation_30_day, groundwater)

      # Create time series objects
      precipitation_ts <- xts(temp$precipitation_30_day, order.by = temp$Date)
      # precipitation_ts
      water_level_ts <- xts(temp$groundwater, order.by = temp$Date)

      precipitation_ts <- as.ts(precipitation_ts)
      water_level_ts <- as.ts(water_level_ts)

      # Use the cross-correlation function
      ccf_result <- ccf(precipitation_ts, water_level_ts, lag.max = 180, plot = TRUE)

      ccf_df <- as.data.frame(ccf_result$acf)
      ccf_df$Lag <- ccf_result$lag
      ccf_df <- ccf_df %>%
        mutate(Well = x)

      # ccf_data <- rbind(ccf_data, ccf_df)

    } else {

      temp <- temp %>%
        drop_na(SWE_diff, groundwater) %>%
        distinct(Date, .keep_all = T)

      SWE_ts <- xts(temp$SWE_diff, order.by = temp$Date)
      # precipitation_ts
      water_level_ts <- xts(temp$groundwater, order.by = temp$Date)

      SWE_ts <- as.ts(SWE_ts)
      water_level_ts <- as.ts(water_level_ts)

      # Use the cross-correlation function
      ccf_result <- ccf(SWE_ts, water_level_ts, lag.max = 180, plot = TRUE)

      ccf_df <- as.data.frame(ccf_result$acf)
      ccf_df$Lag <- ccf_result$lag
      ccf_df <- ccf_df %>%
        mutate(Well = x)

    }

    ccf_df


  }))

## Wrangle data for completion  ------------------------------------------------

# Find the lag with the highest absolute correlation
ccf_data_stats <- ccf_data %>%
  mutate(Lag = -1 * Lag,
         Lag = ifelse(Lag < 0, 1, Lag)) %>%
  group_by(Well) %>%
  mutate(max_acf = max(V1)) %>%
  filter(V1 == max_acf)

Lag_times <- ccf_data_stats %>%
  ungroup() %>%
  select(Well, Lag) %>%
  mutate(Lag = round(Lag)) %>%
  dplyr::rename(Lag_time = Lag,
                myLocation = Well)

## Save outputs  ---------------------------------------------------------------

output_path <- paste0(figure_location, "model calibration/CCF_analysis/", as.character(Sys.Date()))
dir.create(paste0(figure_location, "model calibration/"), showWarnings = FALSE)
dir.create(paste0(figure_location, "model calibration/CCF_analysis"), showWarnings = FALSE)
dir.create(output_path, showWarnings = FALSE)

write.csv(Lag_times,
          paste0(output_path,
                 "/CCF_lag_analysis_results.csv"), row.names = FALSE)


# Merge input file with output to create prep file for Dynamic Weighting analysis

Lag_times_next_step <- read_csv(paste0(user_input_location, "CCF_lag_analysis_inputs.csv")) %>%
  left_join(Lag_times, by = join_by(Well == myLocation))

write.csv(Lag_times_next_step,
          paste0(output_path,
                 "/Calibration_Well_inputs_prep.csv"),
          row.names = FALSE)

