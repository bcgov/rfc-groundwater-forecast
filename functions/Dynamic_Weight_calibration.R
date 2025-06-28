Dynamic_weighting_calibration <- function(Time_series_data,pgown_well_info, forecast_days, num_cores, output_path,forecast_test_length){
  
 

  Waterlevel_adjustments <- Time_series_data %>%
    group_by(Well) %>%
    summarise(groundwater_period_average = mean(groundwater_period_average, na.rm = TRUE)) %>%
    ungroup() 
  
  # register number of Parallel cores to utilize
  
  registerDoParallel(cores = num_cores) 
  
  # Creates a list of Wells from the available data
  
  Well_list <- as.list(unique(Time_series_data$Well))
  
  #Well_list <- list("OW002")
  # Pre-allocate a list to store the results
  simulated_data <- vector("list", length(Well_list))
  
  
  #run calculations for each well in parellel
  
  simulated_data <- foreach(y = Well_list, .combine = rbind, 
                            .packages = c("ggpubr", "dplyr", "tidyverse", "mgcv","nnet", "randomForest","zoo","ggnewscale", "cowplot")) %dopar% {
     # filter data by well                          
  temp <- Time_series_data %>%
      filter(Well == y) 


    #groundwater level adjustment to actual DTW
    
    Waterlevel_adjustments_temp <- Waterlevel_adjustments %>%
      filter(Well == y)
    
    Waterlevel_adjustments_temp<- Waterlevel_adjustments_temp$groundwater_period_average
    
    # extract well leg time 
    
    lag_period <- pgown_well_info %>%
      filter(Well == y) %>%
      pull(Lag_time)
    
    
    x = forecast_test_length
    
    
    
    #create empty data frame to put data    
    data_statistics <- data.frame()
    # RAIN INFLUENCED ----------------------------------------------------------
    if (unique(temp$Snow_influenced == 0)) {
      plot_type <- "rain"
      
        temp_years <- temp %>%
        filter(Well == y) %>%
        drop_na(groundwater,total_precip,mean_temp)%>%
        mutate(year = year(Date))  %>%
        mutate(count = 1)%>%
        group_by(Well, year)%>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        filter(count >= 100)
      

        # t = 2015
      #run for predicted forcast intervals (i.e 14,30,60 and 90 days)
coeff_list <- list(0.5,1,2,3,5,10,100) 
for(a_coeff in coeff_list){

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
      SWE_lead_diff
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
  

  
  temp2_2 <- temp2 %>%
    drop_na(groundwater,groundwater_diff, groundwater_predict, mean_temp_lag, mean_temp_lead,  precipitation_lag ,precip_lead )


fit_ann <- nnet(groundwater_diff ~ groundwater + precip_lead + mean_temp_lead + precipitation_lag + mean_temp_lag, data = temp2_2, size = 5, decay = 0.01, maxit = 500)



temp2_pred <- temp2 %>%
  mutate(groundwater_diff = NA) 

#predict groundwater using GAM model

# Predict groundwater using ANN model
predicted_response_ANN <- predict(fit_ann, newdata = temp2_pred)
temp2_pred <- cbind(temp2_pred, predicted_response_ANN)

# Predict groundwater using Wavelet ANN model


temp2_pred <- temp2_pred %>%
  dplyr::rename(
                ANN = predicted_response_ANN) %>%
  gather(c(ANN), key = "Model", value = predicted_value)


temp2_pred <- temp2_pred %>%
  mutate(predicted_value = predicted_value*(max_groundwater_diff - min_groundwater_diff) + min_groundwater_diff,
         groundwater = groundwater*(max_groundwater - min_groundwater) + min_groundwater,
         actual_predicted_groundwater = actual_predicted_groundwater*(max_groundwater - min_groundwater)+ min_groundwater) %>%
  mutate(predicted_value = groundwater + predicted_value) %>%
  dplyr::select(Date,Date_predicted,Well,lag_day,days_in_year,year,actual_predicted_groundwater,Model,predicted_value)


        
        simulated_data_compiled_stats <- temp2_pred %>%
          mutate(Simulation_lag = paste0("Days ",lag_day)) %>%
          group_by(Well,Model,lag_day) %>%
          mutate( mean_data_day = mean(actual_predicted_groundwater, na.rm = TRUE))%>% 
          ungroup()%>%
          group_by(Well,Model,days_in_year,lag_day) %>%
          mutate(min_data_day = min(actual_predicted_groundwater, na.rm = TRUE),
                 max_data_day = max(actual_predicted_groundwater, na.rm = TRUE))%>% 
          ungroup()%>%
          mutate(count = 1)%>%
          drop_na(actual_predicted_groundwater, predicted_value)%>%
          group_by(Well,Model,Simulation_lag,lag_day,days_in_year,min_data_day,max_data_day,year) %>%
          summarise(RMSE = sqrt(mean((actual_predicted_groundwater - predicted_value)^2)),
                    residual = abs(actual_predicted_groundwater - predicted_value),
                    ssr = sum((actual_predicted_groundwater - predicted_value)^2),
                    sst = sum((actual_predicted_groundwater - mean_data_day)^2),
                    min_data = min(actual_predicted_groundwater),
                    max_data = max(actual_predicted_groundwater),
                    count = sum(count)) %>%
          mutate(range = max_data_day - min_data_day) %>%
          ungroup()%>%
          mutate(NRMSE = RMSE/range)
        
        simulated_data_compiled_stats_year <- simulated_data_compiled_stats %>%
          group_by(Well,Model,Simulation_lag,lag_day,year) %>%
          summarise(RMSE = mean(RMSE),
                    #RSE = sqrt(sum((groundwater - predicted_value)^2) / (n() - 2)),
                    SD = sd(residual),# mae = mean(abs(groundwater - predicted_value)),
                    # R2 = mean(R2),
                    ssr = sum(ssr),
                    sst = sum(sst),
                    NRMSE = mean(NRMSE),
                    min_data = min(min_data),
                    max_data = max(max_data),
                    count = sum(count)) %>%
          mutate(R2 = 1 - (ssr/sst))%>%
          ungroup()%>%
          mutate(perc_year = count/365)
        
        
        
        
        simulated_data_compiled_stats_total <- simulated_data_compiled_stats %>%
          group_by(Well,Model,Simulation_lag,lag_day) %>%
          summarise(RMSE = mean(RMSE),
                    #RSE = sqrt(sum((groundwater - predicted_value)^2) / (n() - 2)),
                    SD = sd(residual),# mae = mean(abs(groundwater - predicted_value)),
                    # R2 = mean(R2),
                    ssr = sum(ssr),
                    sst = sum(sst),
                    NRMSE = mean(NRMSE),
                    min_data = min(min_data),
                    max_data = max(max_data)) %>%
          mutate(R2 = 1 - (ssr/sst))%>%
          ungroup()
        
        
        
        simulated_data_compiled_stats_R2 <- simulated_data_compiled_stats_total %>% 
         # mutate(R2 = ifelse(perc_year >= 0.6, R2, NA)) %>%
          dplyr::select(Well, Model, Simulation_lag, lag_day, R2 ) %>%
          mutate(Rtype = "Rain", DWC_Precip = a_coeff, DWC_Snow = NA)
        
        
        data_statistics <- rbind(data_statistics, simulated_data_compiled_stats_R2)

        
        
}
        
        
        

    }else if (unique(temp$Snow_influenced == 1)) {      # SNOW INFLUENCED ----------------------------------------------------------
      plot_type <- "snow"
      
      #run excluding validation years
      temp_years <- temp %>%
        filter(Well == y) %>%
        drop_na(groundwater,total_precip,mean_temp,SWE)%>%
        mutate(year = year(Date))  %>%
        mutate(count = 1)%>%
        group_by(Well, year)%>%
        summarise(count = sum(count)) %>%
        ungroup() %>%
        filter(count >= 100)
        

      coeff_list <- list(0.5,1,2,3,5,10,100) 
      coeff_list_snow <- list(0.5,1,2,3,5,10,100) 
      
      
      for(a_coeff in coeff_list){
for(a_coeff_snow in coeff_list_snow){

  
  
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
  
  
  
        
        
        
        
        # create a dataframe to train model with no missing values
  temp2_2 <- temp2 %>%
    drop_na(groundwater,groundwater_diff, groundwater_predict, mean_temp_lag, mean_temp_lead,  precipitation_lag ,precip_lead ,SWE,SWE_lag_diff,SWE_lead_diff)
   
  
        fit_ann <- nnet(groundwater_diff ~  groundwater + precip_lead + precipitation_lag + mean_temp_lead + mean_temp_lag + SWE_lag_diff + SWE + SWE_lead_diff, data = temp2_2, size = 5, decay = 0.01, maxit = 500)
        
        
        
        temp2_pred <- temp2 %>%
          drop_na(groundwater, precipitation_lag,mean_temp_lag, SWE_lag_diff, SWE) %>%
          mutate(groundwater_diff = NA) 
        
        
        
        
        # Predict groundwater using ANN model
        predicted_response_ANN <- predict(fit_ann, newdata = temp2_pred)
        temp2_pred <- cbind(temp2_pred, predicted_response_ANN)
        
        # Predict groundwater using Wavelet ANN model
        
        
        temp2_pred <- temp2_pred %>%
          dplyr::rename(ANN = predicted_response_ANN) %>%
          gather(c( ANN), key = "Model", value = predicted_value)
        
        
        temp2_pred <- temp2_pred %>%
          mutate(predicted_value = predicted_value*(max_groundwater_diff - min_groundwater_diff) + min_groundwater_diff,
                 groundwater = groundwater*(max_groundwater - min_groundwater) + min_groundwater,
                 actual_predicted_groundwater = actual_predicted_groundwater*(max_groundwater - min_groundwater)+ min_groundwater) %>%
          mutate(predicted_value = groundwater + predicted_value) %>%
          dplyr::select(Date,Date_predicted,Well,lag_day,days_in_year,year,actual_predicted_groundwater,Model,predicted_value)
        
        
       # }
      
      
      
      
      simulated_data_compiled_stats <- temp2_pred %>%
        mutate(Simulation_lag = paste0("Days ",lag_day)) %>%
        group_by(Well,Model,lag_day) %>%
        mutate( mean_data_day = mean(actual_predicted_groundwater, na.rm = TRUE))%>% 
        ungroup()%>%
        group_by(Well,Model,days_in_year,lag_day) %>%
        mutate(min_data_day = min(actual_predicted_groundwater, na.rm = TRUE),
               max_data_day = max(actual_predicted_groundwater, na.rm = TRUE))%>% 
        ungroup()%>%
        mutate(count = 1)%>%
        drop_na(actual_predicted_groundwater, predicted_value)%>%
        group_by(Well,Model,Simulation_lag,lag_day,days_in_year,min_data_day,max_data_day,year) %>%
        summarise(RMSE = sqrt(mean((actual_predicted_groundwater - predicted_value)^2)),
                  residual = abs(actual_predicted_groundwater - predicted_value),
                  ssr = sum((actual_predicted_groundwater - predicted_value)^2),
                  sst = sum((actual_predicted_groundwater - mean_data_day)^2),
                  min_data = min(actual_predicted_groundwater),
                  max_data = max(actual_predicted_groundwater),
                  count = sum(count)) %>%
        mutate(range = max_data_day - min_data_day) %>%
        ungroup()%>%
        mutate(NRMSE = RMSE/range)
      
      simulated_data_compiled_stats_year <- simulated_data_compiled_stats %>%
        group_by(Well,Model,Simulation_lag,lag_day,year) %>%
        summarise(RMSE = mean(RMSE),
                  #RSE = sqrt(sum((groundwater - predicted_value)^2) / (n() - 2)),
                  SD = sd(residual),# mae = mean(abs(groundwater - predicted_value)),
                  # R2 = mean(R2),
                  ssr = sum(ssr),
                  sst = sum(sst),
                  NRMSE = mean(NRMSE),
                  min_data = min(min_data),
                  max_data = max(max_data),
                  count = sum(count)) %>%
        mutate(R2 = 1 - (ssr/sst))%>%
        ungroup()%>%
        mutate(perc_year = count/365)
      
      
      
      
      simulated_data_compiled_stats_total <- simulated_data_compiled_stats %>%
        group_by(Well,Model,Simulation_lag,lag_day) %>%
        summarise(RMSE = mean(RMSE),
                  #RSE = sqrt(sum((groundwater - predicted_value)^2) / (n() - 2)),
                  SD = sd(residual),# mae = mean(abs(groundwater - predicted_value)),
                  # R2 = mean(R2),
                  ssr = sum(ssr),
                  sst = sum(sst),
                  NRMSE = mean(NRMSE),
                  min_data = min(min_data),
                  max_data = max(max_data)) %>%
        mutate(R2 = 1 - (ssr/sst))%>%
        ungroup()
      
      
      
      simulated_data_compiled_stats_R2 <- simulated_data_compiled_stats_total %>% 
        dplyr::select(Well, Model, Simulation_lag, lag_day, R2 ) %>%
      mutate(Rtype = "Snow", DWC_Precip = a_coeff, DWC_Snow = a_coeff_snow)
      
      
      data_statistics <- rbind(data_statistics, simulated_data_compiled_stats_R2)
      
      

      }

      
      }
      
            
      
      }
  return(data_statistics)
  
    
    }
}
  



