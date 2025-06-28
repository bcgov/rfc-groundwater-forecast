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

source("Groundwater Level Forecasting Toolkit Phase 2 V2.0/01_ConfigInputs.R")
source("Groundwater Level Forecasting Toolkit Phase 2 V2.0/functions/dl_climate_data.R")
source("Groundwater Level Forecasting Toolkit Phase 2 V2.0/functions/dl_pgown_wl_data.R")
source("Groundwater Level Forecasting Toolkit Phase 2 V2.0/functions/dl_snow_data.R")



pgown_well_info <- read_csv(paste0(user_input_location,"CCF_lag_analysis_inputs.csv")) %>%
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
  dplyr::select(-groundwater_up_to_last_year)%>%
  ungroup()%>%  
  distinct()%>%
  mutate(precipitation_30_day = rollmean(total_precip, 30, fill = NA, align='right', na.rm = T)) %>%
  mutate(precip_average = mean(precipitation_30_day, na.rm = TRUE)) %>%
  mutate(precipitation_30_day = precipitation_30_day-precip_average) %>%
  mutate(SWE = - SWE) %>%
  mutate(SWE_diff = SWE - lag(SWE, 1))

Well_list <- as.list(unique(unlist(Time_series_data$Well)))
ccf_data <- data.frame()


for(x in Well_list){
  # Convert date to Date object
  temp <- Time_series_data %>%
    filter(Well == x)
  
  print(unique(temp$Snow_influenced))
  
  if (unique(temp$Snow_influenced == 0)) {
    print(x)
    
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
    
    # Find the lag with the highest absolute correlation
    ccf_data <- rbind(ccf_data, ccf_df)
    
  } else {
    print(x)
    
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
    
    # Find the lag with the highest absolute correlation
    ccf_data <- rbind(ccf_data, ccf_df)
    
  }
  
}

ccf_data_stats <- ccf_data %>%
  mutate(Lag = -1 *Lag,
         Lag = ifelse(Lag <0, 1, Lag)) %>%
  group_by(Well) %>%
  mutate(max_acf = max(V1)) %>%
  filter(V1 == max_acf)

Lag_times <- ccf_data_stats %>%
  ungroup() %>%
  select(Well, Lag) %>%
  mutate(Lag = round(Lag))%>%
  dplyr::rename(Lag_time = Lag,
                myLocation = Well)


write.csv(Lag_times, paste0(figure_location, "model calibration/CCF_analysis/CCF_lag_analysis_results.csv"))









