# ==============================================================================
# Script name:      05b_Dynamic_Weighting_Coeffiencent_Calibration.R
# ------------------------------------------------------------------------------
# Script version:   
# 2025-04-01:       v1
#
# ------------------------------------------------------------------------------
# Notes/Objectives:
# main script for determining Dynamic Weighting Coefficient to use for wells being forecasted, generates nessasary inputs. 
# 
# ==============================================================================
#

## Load inputs and functions ---------------------------------------------------


source("Groundwater Level Forecasting Toolkit Phase 2 V2.0/01_ConfigInputs.R")
source("Groundwater Level Forecasting Toolkit Phase 2 V2.0/functions/dl_climate_data.R")
source("Groundwater Level Forecasting Toolkit Phase 2 V2.0/functions/dl_pgown_wl_data.R")
source("Groundwater Level Forecasting Toolkit Phase 2 V2.0/functions/dl_snow_data.R")



pgown_well_info_all <- read_csv(paste0(user_input_location,"Calibration_Well_inputs.csv")) %>%
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



# Specify the path where you want to create the new folder
output_path <- paste0(figure_location,"model calibration/Dynamic Weighting/",as.character(Sys.Date()))

# Create new folder
dir.create(output_path)

forecast_test_length <- 90

# Number of cores (for parallel computing)
num_cores <- 2


for(i in Regional_group_list){


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
    dplyr::select(-groundwater_up_to_last_year)%>%
    ungroup()%>%  
    distinct()
  
  
  
#### Validation testing ####

source("Groundwater Level Forecasting Toolkit Phase 2 V2.0/functions/Dynamic_Weight_calibration.R")

  Dynamic_weighting_results<- Dynamic_weighting_calibration(Time_series_data,pgown_well_info, forecast_days, num_cores, output_path,forecast_test_length)

  
  Dynamic_weighting_results <- Dynamic_weighting_results %>%
    group_by(Well, Model, lag_day, Rtype)%>%
    mutate(max_R2 = max(R2))%>%
    filter(R2 == max_R2)%>%
    ungroup()
  

  
  
#summarise Statistics
location <- paste0(output_path,"/Dynamic_weighting_calibration_results_",i,".csv") 
write.csv(Dynamic_weighting_results, file = location)


}


