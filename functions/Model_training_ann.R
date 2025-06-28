forecast_model_training <- function(Time_series_data, forecast_days, num_cores, figure_location,output_path, model_path,pgown_well_info){
  
  
  
  # Creates historical groundwater levels for figures
  
  temp_WL_states<- Time_series_data %>%
    mutate(days_in_year = yday(Date)) %>%
    group_by(days_in_year, Well)%>%
    drop_na(groundwater) %>%
    summarise(Min = min(groundwater), 
              per5th = quantile(groundwater, 0.05),
              per10th = quantile(groundwater, 0.1),
              per25th = quantile(groundwater, 0.25),
              per50th = quantile(groundwater, 0.5),
              per75th = quantile(groundwater, 0.75),
              per90th = quantile(groundwater, 0.9),
              per95th = quantile(groundwater, 0.95),
              Max = max(groundwater)) %>%
    mutate(fake_date = as.Date("2020-01-01")+days_in_year)
  
  
  Waterlevel_adjustments <- Time_series_data %>%
    group_by(Well) %>%
    summarise(groundwater_period_average = mean(groundwater_period_average, na.rm = TRUE)) %>%
    ungroup() 
  
  # register number of Parallel cores to utilize
  
  registerDoParallel(cores = num_cores) 
  
  # Creates a list of Wells from the available data
  pgown_well_info_temp <- pgown_well_info %>%
    dplyr::select(Well, Snow_influenced, Lag_time, Training_start_date, DWC_Precip, DWC_Snow,
                  ann_size, ann_decay, ann_maxit, 
                  R2_14,R2_30,R2_60, R2_90,
                  NRMSE_14,NRMSE_30,NRMSE_60,NRMSE_90,
                  RSR_14,RSR_30,RSR_60,RSR_90)
    
    
  
  Well_list <- as.list(unique(pgown_well_info_temp$Well))
  
  # Pre-allocate a list to store the results
  simulated_data <- vector("list", length(Well_list))
  
  
  #run calculations for each well in parellel
  
  simulated_data <- foreach(y = Well_list, .combine = rbind, 
                            .packages = c("ggpubr", "dplyr", "tidyverse", "mgcv", "randomForest","zoo","ggnewscale", "cowplot","nnet")) %dopar% {
                               # filter data by well                          
    temp <- Time_series_data %>%
      filter(Well == y) 
    
    
    temp_WL_states_temp <- temp_WL_states %>%
      filter(Well == y)
    
    pgown_well_info_Well_info <- pgown_well_info_temp%>%
      filter(Well == y)
   
    Training_start <- pgown_well_info_Well_info%>%
      pull(Training_start_date)
    
    
    #groundwater level adjustment to actual DTW
    
    Waterlevel_adjustments_temp <- Waterlevel_adjustments %>%
      filter(Well == y)
    
    Waterlevel_adjustments_temp<- Waterlevel_adjustments_temp$groundwater_period_average
    
    # extract well leg time 
    lag_period <- pgown_well_info_Well_info %>%
      pull(Lag_time)
    
    a_coeff <- pgown_well_info_Well_info %>%
      pull(DWC_Precip)
    
    
    
    a_coeff_snow <- pgown_well_info_Well_info %>%
      pull(DWC_Snow)
    
    ann_size <- pgown_well_info_Well_info %>%
      pull(ann_size)  
    
    ann_decay<- pgown_well_info_Well_info %>%
      pull(ann_decay)
    
    ann_maxit <- pgown_well_info_Well_info %>%
      pull(ann_maxit)
    
    # Function to classify performance based on R² and NRMSE
    classify_performance <- function(r2, nrmse) {
      if (r2 >= 0.8 & nrmse <= 0.1) {
        return("Good")
      } else if ((r2 >= 0.6 & nrmse <= 0.2) | (r2 >= 0.8 & nrmse <= 0.2)) {
        return("Fair")
      } else {
        return("Poor")
      }
    }
    
    # Function to classify forecast results based on RSR
    classify_forecast_results <- function(rsr) {
      if (rsr < 1) {
        return("Results of Value")
      } else {
        return("Results of Limited Value")
      }
    }
    
    # Assign categories for each forecast period
    well_lag_time_2 <- pgown_well_info_Well_info %>%
      rowwise() %>%
      mutate(
        Performance_14 = classify_performance(R2_14, NRMSE_14),
        Forecast_14 = classify_forecast_results(RSR_14),
        Performance_30 = classify_performance(R2_30, NRMSE_30),
        Forecast_30 = classify_forecast_results(RSR_30),
        Performance_60 = classify_performance(R2_60, NRMSE_60),
        Forecast_60 = classify_forecast_results(RSR_60),
        Performance_90 = classify_performance(R2_90, NRMSE_90),
        Forecast_90 = classify_forecast_results(RSR_90)
      ) %>%
      mutate(
        comb_perf_value_14 = paste0(Performance_14, ", ", Forecast_14),
        comb_perf_value_30 = paste0(Performance_30, ", ", Forecast_30),
        comb_perf_value_60 = paste0(Performance_60, ", ", Forecast_60),
        comb_perf_value_90 = paste0(Performance_90, ", ", Forecast_90)
      ) %>%
      dplyr::select(Well, comb_perf_value_14, comb_perf_value_30, comb_perf_value_60, comb_perf_value_90) %>%
      gather(key = "per_name", value = "performance", comb_perf_value_14, comb_perf_value_30, comb_perf_value_60, comb_perf_value_90) %>%
      mutate(lag_day = ifelse(per_name == "comb_perf_value_14", 14, 
                              ifelse(per_name == "comb_perf_value_30", 30,
                                     ifelse(per_name == "comb_perf_value_60", 60,
                                            ifelse(per_name == "comb_perf_value_90", 90,NA))))) %>%
      dplyr::select(Well, lag_day,performance)
    
    
    
    
    
    
    
    # RAIN INFLUENCED ----------------------------------------------------------
    if (unique(pgown_well_info_Well_info$Snow_influenced == 0)) {
      plot_type <- "rain"
        for(x in forecast_days){
        #summarise leading and lagging variables

      
      # Preprocess temp
      temp2 <- temp %>%
         mutate(lag_day = x, #forcast interval
               Date_predicted = lead(Date,x),
               precipitation_lag = rollsum(total_precip, lag_period, fill = NA, align='right', na.rm = TRUE), #recharge lagging precipitation
               mean_temp_lag = rollmean(mean_temp, lag_period, fill = NA,align='right', na.rm = TRUE), # recharge lagging temperature
               groundwater_predict = lead(groundwater, x), # actual groundwater level we are predicting
               mean_temp_lead = rollmean(mean_temp, x, fill = NA,align='left', na.rm = TRUE), #forecasted temperature
               precip_lead = rollsum(total_precip, x, fill = NA,align='left', na.rm = TRUE) #forecasted precipitation
        ) %>%
        mutate( SWE = NA,
                SWE_lag_diff = NA,
                SWE_lead_diff = NA,
                groundwater_diff = groundwater_predict - groundwater,
                actual_predicted_groundwater = groundwater_predict,
                year = year(Date_predicted),
                days_in_year = yday(Date_predicted)
        ) %>%
        dplyr::select( # only select variables of interest remove others
          Date,
          Date_predicted,
          Well,
          lag_day,
          days_in_year,
          year,
          groundwater, 
          groundwater_predict, 
          actual_predicted_groundwater,
          mean_temp_lag, 
          mean_temp_lead,  
          groundwater_diff,
          precipitation_lag, 
          precip_lead, 
          SWE,
          SWE_lag_diff,
          SWE_lead_diff#,
          #max_groundwater,
          #min_groundwater
        )
      
      
      
      
      calculate_weighted_lead <- function(i, x, lag_period, a_coeff) {
        
        temp %>%
         mutate(lag_day = x, #forcast interval
                 Date_predicted = lead(Date,x)
          ) %>%
          mutate(lag_day_adjusted = (x-lag_period))%>%
          mutate(weight_max = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period*a_coeff))%>%
          mutate(normal_weight = dnorm(i, mean = lag_day_adjusted, sd = lag_period*a_coeff ))%>%
          mutate(normal_weight = (normal_weight - 0)/(weight_max-0))%>%
          mutate(weighted_lead = lead(total_precip,i)*normal_weight)%>%
          dplyr::select( # only select variables of interest remove others
            Date,
            Date_predicted,
            Well,
            lag_day,
            lag_day_adjusted,
            weighted_lead
          )
        

      }
      
      lead_data <- map_dfr(1:x, calculate_weighted_lead, x = x, lag_period = lag_period, a_coeff = a_coeff)
      
      
      lead_data <-   lead_data %>%
        group_by(# only select variables of interest remove others
          Date,
          Date_predicted,
          Well,
          lag_day
        ) %>%
        summarise(weighted_lead = mean(weighted_lead, na.rm = TRUE))%>%
        ungroup() 
      
      
      
      lag_data <- data.frame()
      
      lag_period_length = lag_period*3
      
      calculate_weighted_lag <- function(i, lag_period, a_coeff) {

        temp %>%
          mutate(lag_day = x, #forcast interval
                 Date_predicted = lead(Date,x)
                      ) %>%
          mutate(lag_day_adjusted = lag_period -x)%>%
          mutate(weight_max = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period*a_coeff))%>%
          mutate(normal_weight = dnorm(i, mean = lag_day_adjusted, sd = lag_period*a_coeff ))%>%
          mutate(normal_weight = (normal_weight - 0)/(weight_max-0))%>%
          mutate(weighted_lag = lag(total_precip,i)*normal_weight)%>%
          dplyr::select( # only select variables of interest remove others
            Date,
            Date_predicted,
            Well,
            lag_day,
            lag_day_adjusted,
            weighted_lag
          )
        

      }
      
      
      lag_data <- map_dfr(1:(lag_period * 3), calculate_weighted_lag, lag_period = lag_period, a_coeff = a_coeff)
      
      
      
      lag_data <-   lag_data %>%
        group_by(# only select variables of interest remove others
          Date,
          Date_predicted,
          Well,
          lag_day
        ) %>%
        summarise(weighted_lag = mean(weighted_lag, na.rm = TRUE))%>%
        ungroup() 
      
      lead_data_lag <- full_join(lead_data, lag_data)
      
      temp2 <- full_join(temp2,lead_data_lag)
      
      temp2 <- temp2 %>%
        mutate(precip_lead_org = precip_lead,
               precipitation_lag_org = precipitation_lag,
               precip_lead = weighted_lead,
               precipitation_lag = weighted_lag)%>%
        mutate(max_precip_lead = max(precip_lead, na.rm = TRUE),
               min_precip_lead = min(precip_lead, na.rm = TRUE),
               max_precipitation_lag = max(precipitation_lag, na.rm = TRUE),
               min_precipitation_lag = min(precipitation_lag, na.rm = TRUE),
               max_mean_temp_lead = max(mean_temp_lead, na.rm = TRUE),
               min_mean_temp_lead = min(mean_temp_lead, na.rm = TRUE),
               max_mean_temp_lag = max(mean_temp_lag, na.rm = TRUE),
               min_mean_temp_lag = min(mean_temp_lag, na.rm = TRUE),
               max_groundwater = max(groundwater, na.rm = TRUE),
               min_groundwater = min(groundwater, na.rm = TRUE),
               max_groundwater_diff = max(groundwater_diff, na.rm = TRUE),
               min_groundwater_diff = min(groundwater_diff, na.rm = TRUE)
        ) %>%
        mutate(precip_lead = (precip_lead - min_precip_lead)/(max_precip_lead-min_precip_lead),
               precipitation_lag = (precipitation_lag - min_precipitation_lag)/(max_precipitation_lag-min_precipitation_lag),
               mean_temp_lag = (mean_temp_lag - min_mean_temp_lag)/(max_mean_temp_lag-min_mean_temp_lag),
               mean_temp_lead = (mean_temp_lead - min_mean_temp_lead)/(max_mean_temp_lead-min_mean_temp_lead),
               groundwater = (groundwater - min_groundwater)/(max_groundwater-min_groundwater),
               actual_predicted_groundwater = (actual_predicted_groundwater - min_groundwater)/(max_groundwater-min_groundwater), 
               groundwater_predict =  (groundwater_predict - min_groundwater)/(max_groundwater-min_groundwater),
               groundwater_diff =  (groundwater_diff - min_groundwater_diff)/(max_groundwater_diff-min_groundwater_diff)
        ) 
     
      temp2_original <- temp2
      
          
        
        
        # create a dataframe to train model with no missing values
        
        temp2_2 <- temp2 %>%
          mutate(Date >= Training_start) %>%
          drop_na(groundwater,groundwater_diff, groundwater_predict, mean_temp_lag, mean_temp_lead,  precipitation_lag ,precip_lead)%>%
          filter(year(Date) != year(Sys.Date()))
        
        
        
        fit_ann <- nnet(groundwater_diff ~ groundwater + precip_lead + mean_temp_lead + precipitation_lag + mean_temp_lag, data = temp2_2, size = ann_size, decay = ann_decay, maxit = ann_maxit)

        model_file <- paste0(model_path,"ANN_Model_",y,"_",x,".Rdata")
        save(fit_ann, file = model_file)
        


          
        }
        

    }else if (unique(pgown_well_info_Well_info$Snow_influenced == 1)) {      # SNOW INFLUENCED ----------------------------------------------------------
      plot_type <- "snow"
      
      #run for predicted forcast intervals (i.e 14,30,60 and 90 days)
        for(x in forecast_days){

     
          temp2 <- temp %>%
            mutate(lag_day = x, #forcast interval
                   Date_predicted = lead(Date,x),
                   precipitation_lag = rollsum(total_precip, lag_period, fill = NA,align='right', na.rm = T), #recharge lagging precipitation
                   mean_temp_lag = rollmean(mean_temp, lag_period, fill = NA,align='right', na.rm = T), #recharge lagging temperature
                   groundwater_predict = lead(groundwater, x), #actual groundwater level we are predicting
                   mean_temp_lead = rollmean(mean_temp, x, fill = NA,align='left', na.rm = T), #forecasted precipitation
                   precip_lead = rollsum(total_precip, x, fill = NA,align='left', na.rm = T), #forecasted temperature
                   SWE_lag = lag(SWE, lag_period), #Snow level at rechrage lag period
                   SWE_lead = lead(SWE,x) #snow level at prediction date
            ) %>%
            mutate(
              SWE_lag_diff =  SWE - SWE_lag, #snowmelt that occured during lag period
              SWE_lead_diff =  SWE_lead - SWE,
              groundwater_diff = groundwater_predict - groundwater,
              actual_predicted_groundwater = groundwater_predict,
              year = year(Date_predicted),
              days_in_year = yday(Date_predicted)) %>%#snowmelt that occured during forecasted period
            dplyr::select( # only select variables of interest remove others
              Date,
              Date_predicted,
              Well,
              lag_day,
              days_in_year,
              year,
              groundwater, 
              groundwater_predict, 
              groundwater_diff,
              actual_predicted_groundwater,
              mean_temp_lag, 
              mean_temp_lead,  
              precipitation_lag, 
              precip_lead, 
              SWE,
              SWE_lag_diff,
              SWE_lead_diff
            )
          
          
          
          
          calculate_weighted_lead <- function(i, x, lag_period, a_coeff) {

            temp %>%
             mutate(lag_day = x, #forcast interval
                     Date_predicted = lead(Date,x),
                     SWE_lag = lag(SWE, 1), #Snow level at rechrage lag period
                     SWE_lead = lead(SWE,1)
              ) %>%
              mutate(
                SWE_lag_diff =  SWE - SWE_lag, #snowmelt that occured during lag period
                SWE_lead_diff =  SWE_lead - SWE)%>%
              mutate(lag_day_adjusted = (x- lag_period))%>%
              mutate(weight_max = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period*a_coeff))%>%
              mutate(normal_weight = dnorm(i, mean = lag_day_adjusted, sd = lag_period*a_coeff ))%>%
              mutate(normal_weight = (normal_weight - 0)/(weight_max-0))%>%
              mutate(weight_max2 = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period*a_coeff_snow))%>%
              mutate(normal_weight2 = dnorm(i, mean = lag_day_adjusted, sd = lag_period*a_coeff_snow ))%>%
              mutate(normal_weight2 = (normal_weight2 - 0)/(weight_max2-0))%>%
              mutate(weighted_lead = lead(total_precip,i)*normal_weight,
                     weighted_lead_temp = lead(mean_temp,i)*normal_weight2,
                     weighted_lead_SWE = lead(SWE_lead_diff,i)*normal_weight2)%>%
              dplyr::select( # only select variables of interest remove others
                Date,
                Date_predicted,
                Well,
                lag_day,
                lag_day_adjusted,
                weighted_lead,
                weighted_lead_SWE,
                weighted_lead_temp)
            
            
          }
          
          lead_data <- map_dfr(1:x, calculate_weighted_lead, x = x, lag_period = lag_period, a_coeff = a_coeff)
          
          lead_data <-   lead_data %>%
            group_by(# only select variables of interest remove others
              Date,
              Date_predicted,
              Well,
              lag_day
            ) %>%
            summarise(weighted_lead = mean(weighted_lead, na.rm = TRUE),
                      weighted_lead_SWE = mean(weighted_lead_SWE, na.rm = TRUE),
                      weighted_lead_temp = mean(weighted_lead_temp, na.rm = TRUE))%>%
            ungroup() 
          
          
          
          
          lag_period_length = lag_period*3
          
          calculate_weighted_lag <- function(i, lag_period, a_coeff) {

            temp %>%
              mutate(lag_day = x, #forcast interval
                     Date_predicted = lead(Date,x),
                     SWE_lag = lag(SWE, 1), #Snow level at rechrage lag period
                     SWE_lead = lead(SWE,1)
                                  ) %>%
              mutate(
                SWE_lag_diff =  SWE - SWE_lag, #snowmelt that occured during lag period
                SWE_lead_diff =  SWE_lead - SWE)%>%
              mutate(lag_day_adjusted = lag_period -x)%>%
              mutate(weight_max = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period*a_coeff))%>%
              mutate(weight_max2 = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period*a_coeff_snow))%>%
              mutate(normal_weight = dnorm(i, mean = lag_day_adjusted, sd = lag_period*a_coeff ))%>%
              mutate(normal_weight2 = dnorm(i, mean = lag_day_adjusted, sd = lag_period*a_coeff_snow ))%>%
              mutate(normal_weight = (normal_weight - 0)/(weight_max-0))%>%
              mutate(normal_weight2 = (normal_weight2 - 0)/(weight_max2-0))%>%
              mutate(weighted_lag = lag(total_precip,i)*normal_weight,
                     weighted_lag_temp = lag(mean_temp,i)*normal_weight2,
                     weighted_lag_SWE = lag(SWE_lead_diff,i)*normal_weight2)%>%
              dplyr::select( # only select variables of interest remove others
                Date,
                Date_predicted,
                Well,
                lag_day,
                lag_day_adjusted,
                weighted_lag,
                weighted_lag_SWE,
                weighted_lag_temp
              )
            
            
          }
          
          
          lag_data <- map_dfr(1:(lag_period * 3), calculate_weighted_lag, lag_period = lag_period, a_coeff = a_coeff)
          
          lag_data <-   lag_data %>%
            group_by(# only select variables of interest remove others
              Date,
              Date_predicted,
              Well,
              lag_day
            ) %>%
            summarise(weighted_lag = mean(weighted_lag, na.rm = TRUE),
                      weighted_lag_SWE = mean(weighted_lag_SWE, na.rm = TRUE),
                      weighted_lag_temp = mean(weighted_lag_temp, na.rm = TRUE)
            )%>%
            ungroup() 
          
          lead_data_lag <- full_join(lead_data, lag_data)
          
          temp2 <- full_join(temp2,lead_data_lag)
          
          temp2 <- temp2 %>%
            mutate(precip_lead_org = precip_lead,
                   precipitation_lag_org = precipitation_lag,
                   precip_lead = weighted_lead,
                   precipitation_lag = weighted_lag,
                   SWE_lead_diff_org = SWE_lead_diff,
                   SWE_lag_diff_org = SWE_lag_diff,
                   SWE_lead_diff = weighted_lead_SWE,
                   SWE_lag_diff = weighted_lag_SWE,
                   mean_temp_lead = weighted_lead_temp,
                   mean_temp_lag = weighted_lag_temp
            ) %>%
            mutate(max_precip_lead = max(precip_lead, na.rm = TRUE),
                   min_precip_lead = min(precip_lead, na.rm = TRUE),
                   max_precipitation_lag = max(precipitation_lag, na.rm = TRUE),
                   min_precipitation_lag = min(precipitation_lag, na.rm = TRUE),
                   max_SWE_lead_diff = max(SWE_lead_diff, na.rm = TRUE),
                   min_SWE_lead_diff = min(SWE_lead_diff, na.rm = TRUE),
                   max_SWE_lag_diff = max(SWE_lag_diff, na.rm = TRUE),
                   min_SWE_lag_diff = min(SWE_lag_diff, na.rm = TRUE),
                   max_mean_temp_lead = max(mean_temp_lead, na.rm = TRUE),
                   min_mean_temp_lead = min(mean_temp_lead, na.rm = TRUE),
                   max_mean_temp_lag = max(mean_temp_lag, na.rm = TRUE),
                   min_mean_temp_lag = min(mean_temp_lag, na.rm = TRUE),
                   max_groundwater = max(groundwater, na.rm = TRUE),
                   min_groundwater = min(groundwater, na.rm = TRUE),
                   max_groundwater_diff = max(groundwater_diff, na.rm = TRUE),
                   min_groundwater_diff = min(groundwater_diff, na.rm = TRUE)
            ) %>%
            mutate(precip_lead = (precip_lead - min_precip_lead)/(max_precip_lead-min_precip_lead),
                   precipitation_lag = (precipitation_lag - min_precipitation_lag)/(max_precipitation_lag-min_precipitation_lag),
                   SWE_lead_diff = (SWE_lead_diff - min_SWE_lead_diff)/(max_SWE_lead_diff-min_SWE_lead_diff),
                   SWE_lag_diff = (SWE_lag_diff - min_SWE_lag_diff)/(max_SWE_lag_diff-min_SWE_lag_diff),
                   mean_temp_lag = (mean_temp_lag - min_mean_temp_lag)/(max_mean_temp_lag-min_mean_temp_lag),
                   mean_temp_lead = (mean_temp_lead - min_mean_temp_lead)/(max_mean_temp_lead-min_mean_temp_lead),
                   groundwater = (groundwater - min_groundwater)/(max_groundwater-min_groundwater),
                   actual_predicted_groundwater = (actual_predicted_groundwater - min_groundwater)/(max_groundwater-min_groundwater), 
                   groundwater_predict =  (groundwater_predict - min_groundwater)/(max_groundwater-min_groundwater),
                   groundwater_diff =  (groundwater_diff - min_groundwater_diff)/(max_groundwater_diff-min_groundwater_diff))
          
          
          
          
          temp2_original <- temp2
          
          
          
        # create a dataframe to train model with no missing values
        
        temp2_2 <- temp2 %>%
          mutate(Date >= Training_start) %>%
          drop_na(groundwater,groundwater_diff, groundwater_predict, mean_temp_lag, mean_temp_lead,  precipitation_lag ,precip_lead ,SWE,SWE_lag_diff,SWE_lead_diff)%>%
          filter(year(Date) != year(Sys.Date()))
     
        
        fit_ann <- nnet(groundwater_diff ~ groundwater + precip_lead + mean_temp_lead + precipitation_lag + mean_temp_lag, data = temp2_2, size = ann_size, decay = ann_decay, maxit = ann_maxit)
        
        

        model_file <- paste0(model_path,"ANN_Model_",y,"_",x,".Rdata")
        save(fit_ann, file = model_file)
        

      }
      
    }
    

     
      
  }
  
}

