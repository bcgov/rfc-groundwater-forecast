Model_testing <- function(Time_series_data, pgown_well_info, forecast_days, num_cores, output_path){


  # Creates historical groundwater levels for figures

  temp_WL_states <- Time_series_data %>%
    mutate(days_in_year = yday(Date),
           Month = month(Date),
           Day = day(Date)) %>%
    #filter(Date <= as.Date("2023-12-31")) %>%
    # group_by(days_in_year, Well) %>%
    filter(Month != 2 | Day != 29) %>%
    group_by(Well, Month, Day) %>%
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
    ungroup() %>%
    # mutate(fake_date = as.Date("2020-01-01") + days_in_year - 1,
    mutate(fake_date = as.Date(paste0("2020-", Month, "-", Day)),
           days_in_year = yday(fake_date))




  Waterlevel_adjustments <- Time_series_data %>%
    group_by(Well) %>%
    summarise(groundwater_period_average = mean(groundwater_period_average, na.rm = TRUE)) %>%
    ungroup()

  # register number of Parallel cores to utilize

  registerDoParallel(cores = num_cores)

  # Creates a list of Wells from the available data
  pgown_well_info_temp <- pgown_well_info #%>%
  #  filter(Snow_influenced == 0)


  Well_list <- as.list(unique(pgown_well_info_temp$Well))


  # Pre-allocate a list to store the results
  simulated_data <- vector("list", length(Well_list))



  #run calculations for each well in parellel

  simulated_data <- foreach(y = Well_list, .combine = rbind,
                            .packages = c("ggpubr", "dplyr", "tidyverse", "mgcv",
                                          "randomForest","zoo","ggnewscale", "cowplot","nnet")
                            ) %dopar% {
                             # filter data by well
    temp <- Time_series_data %>%
      filter(Well == y)


    pgown_well_info_Well_info <- pgown_well_info %>%
      filter(Well == y)

    #groundwater level adjustment to actual DTW

    Waterlevel_adjustments_temp <- Waterlevel_adjustments %>%
      filter(Well == y)

    Waterlevel_adjustments_temp<- Waterlevel_adjustments_temp$groundwater_period_average

    # extract well leg time
    pgown_well_info_Well_info <- pgown_well_info %>%
      filter(Well == y)


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
    #create empty data frame to put data

    Time_series_data_3_validation<- data.frame()

    Time_series_data_3_operational<- data.frame()


    # RAIN INFLUENCED ----------------------------------------------------------
    if (unique(pgown_well_info_Well_info$Snow_influenced == 0)) {
      plot_type <- "rain"

      temp_years <- Time_series_data %>%
        filter(Well == y) %>%
        mutate(groundwater_lag = lag(groundwater,lag_period),groundwater_lead_1 = lead(groundwater, 14),groundwater_lead_2 = lead(groundwater, 30),groundwater_lead_3 = lead(groundwater, 60),groundwater_lead_4 = lead(groundwater,90)) %>%
        drop_na(groundwater,total_precip,mean_temp,groundwater_lag,groundwater_lead_1,groundwater_lead_2,groundwater_lead_3,groundwater_lead_4) %>%
        mutate(year = year(Date)) %>%
        filter(year!= year(Sys.Date()))


     year_list <- as.list(unique(temp_years$year))
      #run excluding validation years

      for (t in year_list) {
        Time_series_data_2_validation <- data.frame()
        Time_series_data_2 <- data.frame()

        for (x in forecast_days){
    #x = 90


          # Preprocess temp
          temp2 <- temp %>%
            #mutate(#max_groundwater = max(groundwater, na.rm = TRUE),
                   #min_groundwater = min(groundwater, na.rm = TRUE)#,
                  # max_total_precip = max(total_precip, na.rm = TRUE),
                   #min_total_precip = min(total_precip, na.rm = TRUE),
                   #max_temp = max(mean_temp, na.rm = TRUE),
                   #min_temp = min(mean_temp, na.rm = TRUE)
            #      ) %>%
            #mutate(#total_precip = (total_precip - min_total_precip)/(max_total_precip-min_total_precip),
             #      groundwater = (groundwater - min_groundwater)/(max_groundwater-min_groundwater),
                   #mean_temp = (mean_temp - min_temp)/(max_temp-min_temp)
            #) %>%
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
              #mutate(#max_groundwater = max(groundwater, na.rm = TRUE),
                     #min_groundwater = min(groundwater, na.rm = TRUE)#,
                     #max_total_precip = max(total_precip, na.rm = TRUE),
                     #min_total_precip = min(total_precip, na.rm = TRUE),
                     #max_temp = max(mean_temp, na.rm = TRUE),
                     #min_temp = min(mean_temp, na.rm = TRUE)
               #      ) %>%
              #mutate(#total_precip = (total_precip - min_total_precip)/(max_total_precip-min_total_precip),
               #      groundwater = (groundwater - min_groundwater)/(max_groundwater-min_groundwater),
                     #mean_temp = (mean_temp - min_temp)/(max_temp-min_temp)
              #) %>%
              mutate(lag_day = x, #forcast interval
                     Date_predicted = lead(Date,x)
                     # precipitation_lag = rollsum(total_precip, lag_period, fill = NA, align='right', na.rm = TRUE), #recharge lagging precipitation
                     #mean_temp_lag = rollmean(mean_temp, lag_period, fill = NA,align='right', na.rm = TRUE), # recharge lagging temperature
                     #groundwater_predict = lead(groundwater, x), # actual groundwater level we are predicting
                     # mean_temp_lead = rollmean(mean_temp, x, fill = NA,align='left', na.rm = TRUE), #forecasted temperature
                     # precip_lead = rollsum(total_precip, x, fill = NA,align='left', na.rm = TRUE) #forecasted precipitation
              ) %>%
              mutate(lag_day_adjusted = (x-lag_period)) %>%
              mutate(weight_max = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period*a_coeff)) %>%
              mutate(normal_weight = dnorm(i, mean = lag_day_adjusted, sd = lag_period*a_coeff )) %>%
              mutate(normal_weight = (normal_weight - 0)/(weight_max-0)) %>%
              mutate(weighted_lead = lead(total_precip,i)*normal_weight) %>%
              dplyr::select( # only select variables of interest remove others
                Date,
                Date_predicted,
                Well,
                lag_day,
                lag_day_adjusted,
                weighted_lead#,
                #max_groundwater,
                #min_groundwater
                )

         #   lead_data <- rbind(lead_data, temp3)

          }

          lead_data <- map_dfr(1:x, calculate_weighted_lead, x = x, lag_period = lag_period, a_coeff = a_coeff)


          lead_data <-   lead_data %>%
            group_by(# only select variables of interest remove others
              Date,
              Date_predicted,
              Well,
              lag_day
            ) %>%
            summarise(weighted_lead = mean(weighted_lead, na.rm = TRUE)) %>%
            ungroup()



          lag_data <- data.frame()

          lag_period_length = lag_period*3

          calculate_weighted_lag <- function(i, lag_period, a_coeff) {
            #i = 5

           temp %>%
             # mutate(max_groundwater = max(groundwater, na.rm = TRUE),
              #       min_groundwater = min(groundwater, na.rm = TRUE),
                     #max_total_precip = max(total_precip, na.rm = TRUE),
                     #min_total_precip = min(total_precip, na.rm = TRUE),
                     #max_temp = max(mean_temp, na.rm = TRUE),
                     #min_temp = min(mean_temp, na.rm = TRUE)
               #      ) %>%
              #mutate(#total_precip = (total_precip - min_total_precip)/(max_total_precip-min_total_precip),
               #      groundwater = (groundwater - min_groundwater)/(max_groundwater-min_groundwater),
                     #mean_temp = (mean_temp - min_temp)/(max_temp-min_temp)
              #) %>%
              mutate(lag_day = x, #forcast interval
                     Date_predicted = lead(Date,x)
                     # precipitation_lag = rollsum(total_precip, lag_period, fill = NA, align='right', na.rm = TRUE), #recharge lagging precipitation
                     #mean_temp_lag = rollmean(mean_temp, lag_period, fill = NA,align='right', na.rm = TRUE), # recharge lagging temperature
                     #groundwater_predict = lead(groundwater, x), # actual groundwater level we are predicting
                     # mean_temp_lead = rollmean(mean_temp, x, fill = NA,align='left', na.rm = TRUE), #forecasted temperature
                     # precip_lead = rollsum(total_precip, x, fill = NA,align='left', na.rm = TRUE) #forecasted precipitation
              ) %>%
              mutate(lag_day_adjusted = lag_period -x) %>%
              mutate(weight_max = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period*a_coeff)) %>%
              mutate(normal_weight = dnorm(i, mean = lag_day_adjusted, sd = lag_period*a_coeff )) %>%
              mutate(normal_weight = (normal_weight - 0)/(weight_max-0)) %>%
              mutate(weighted_lag = lag(total_precip,i)*normal_weight) %>%
              dplyr::select( # only select variables of interest remove others
                Date,
                Date_predicted,
                Well,
                lag_day,
                lag_day_adjusted,
                weighted_lag#,
                #max_groundwater,
                #min_groundwater
                )

           # lag_data <- rbind(lag_data, temp3)

          }


          lag_data <- map_dfr(1:(lag_period * 3), calculate_weighted_lag, lag_period = lag_period, a_coeff = a_coeff)



          lag_data <-   lag_data %>%
            group_by(# only select variables of interest remove others
              Date,
              Date_predicted,
              Well,
              lag_day
            ) %>%
            summarise(weighted_lag = mean(weighted_lag, na.rm = TRUE)) %>%
            ungroup()

          lead_data_lag <- full_join(lead_data, lag_data)

          temp2 <- full_join(temp2,lead_data_lag)

          temp2 <- temp2 %>%
            mutate(precip_lead_org = precip_lead,
                   precipitation_lag_org = precipitation_lag,
                   precip_lead = weighted_lead,
                   precipitation_lag = weighted_lag) %>%
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
          # mutate(max_groundwater = max(groundwater, na.rm = TRUE),
          #       min_groundwater = min(groundwater, na.rm = TRUE),
          #max_total_precip = max(total_precip, na.rm = TRUE),
          #min_total_precip = min(total_precip, na.rm = TRUE),
          #max_temp = max(mean_temp, na.rm = TRUE),
          #min_temp = min(mean_temp, na.rm = TRUE)
          #      ) %>%
          #mutate(#total_precip = (total_precip - min_total_precip)/(max_total_precip-min_total_precip),
          #      groundwater = (groundwater - min_groundwater)/(max_groundwater-min_groundwater),
          #mean_temp = (mean_temp - min_temp)/(max_temp-min_temp)
          #) %>%

temp2_original <- temp2



          # create a dataframe to train model with no missing values

          temp2_2 <- temp2 %>%
            drop_na(groundwater,groundwater_diff, groundwater_predict, mean_temp_lag, mean_temp_lead,  precipitation_lag ,precip_lead ) %>%
            filter(year != t)


           #TRAIN and FIT RF model
          fit_ann <- nnet(groundwater_diff ~ groundwater + precip_lead + mean_temp_lead + precipitation_lag + mean_temp_lag, data = temp2_2, size = ann_size, decay = ann_decay, maxit = ann_maxit)




          temp2_pred <- temp2 %>%
            filter(year == t) %>%
            mutate(groundwater_diff = NA)

          #predict groundwater using GAM model


          # Predict groundwater using ANN model
           predicted_response_ANN <- predict(fit_ann, newdata = temp2_pred)
          temp2_pred <- cbind(temp2_pred, predicted_response_ANN)

          # Predict groundwater using Wavelet ANN model


          temp2_pred <- temp2_pred %>%
            dplyr::rename(#GAM = predicted_response_GAM,
                          #GLM = predicted_response_GLM,
           #                     RF = predicted_response_RF,
                          ANN = predicted_response_ANN
            ) %>%
            gather(c(#GAM, GLM,#RF
                     ANN), key = "Model", value = predicted_value)


          temp2_pred <- temp2_pred %>%
            mutate(predicted_value = predicted_value*(max_groundwater_diff - min_groundwater_diff) + min_groundwater_diff,
                   groundwater = groundwater*(max_groundwater - min_groundwater) + min_groundwater,
                   actual_predicted_groundwater = actual_predicted_groundwater*(max_groundwater - min_groundwater)+ min_groundwater) %>%
            mutate(predicted_value = groundwater + predicted_value)

          Time_series_data_2_validation <-  rbind(Time_series_data_2_validation,temp2_pred) #%>%
            #mutate(predicted_value = groundwater + predicted_value)





          temp2_pred2_forplots_stats <- temp2 %>%
            dplyr::select(Date, Date_predicted, lag_day, days_in_year, year, precip_lead,precipitation_lag,
                          mean_temp_lag, mean_temp_lead,
                          precip_lead_org,precipitation_lag_org) %>%
            dplyr::rename(mean_temp_lead = mean_temp_lead,
                          precip_lead_weighted = precip_lead,
                          precipitation_lag_weighted = precipitation_lag) %>%
            gather(c(precip_lead_weighted,precipitation_lag_weighted,
                     mean_temp_lag, mean_temp_lead,
                     precip_lead_org,precipitation_lag_org), key = "variable", value = "Value") %>%
            mutate(month = month(Date_predicted), day = day(Date_predicted)) %>%
            group_by(month,day,lag_day, variable) %>%
         summarise(predicted_value_mean = mean(Value, na.rm = TRUE),
                     predicted_value_min = min(Value, na.rm = TRUE),
                    predicted_value_max = max(Value, na.rm = TRUE),
                   predicted_value_25th =  quantile(Value, 0.25, na.rm = TRUE),
                  predicted_value_75th = quantile(Value, 0.75, na.rm = TRUE),
                 predicted_value_50th = quantile(Value, 0.50, na.rm = TRUE),
                predicted_value_90th = quantile(Value, 0.90, na.rm = TRUE),
               predicted_value_10th = quantile(Value, 0.10, na.rm = TRUE),
              predicted_value_5th = quantile(Value, 0.05, na.rm = TRUE),
              predicted_value_95th = quantile(Value, 0.95, na.rm = TRUE)
            ) %>%
            ungroup()







          # its normalized... is that the problem????
        # make a fake data set tripling the dataset

        daily_stats<- temp2_original %>%
          #mutate(precip_lead = precip_lead*(max_precip_lead - min_precip_lead) + min_precip_lead,
           #  mean_temp_lead = mean_temp_lead*(max_mean_temp_lead - min_mean_temp_lead) + min_mean_temp_lead) %>%
          group_by(days_in_year) %>%
          summarise(
            precip_lead_mean = mean(precip_lead, na.rm = TRUE),
            precip_lead_sd = sd(precip_lead, na.rm = TRUE),
            mean_temp_lead_mean = mean(mean_temp_lead, na.rm = TRUE),
            mean_temp_lead_sd = sd(mean_temp_lead, na.rm = TRUE)
          )


        # Generate Monte Carlo values for each day
        monte_carlo_df <- daily_stats %>%
          tidyr::uncount(200) %>%
          mutate(
            precip_lead = rnorm(n(), mean = precip_lead_mean, sd = precip_lead_sd),
            mean_temp_lead = rnorm(n(), mean = mean_temp_lead_mean, sd = mean_temp_lead_sd)
          )

        monte_carlo_df <- monte_carlo_df %>%
          dplyr::select(days_in_year, precip_lead, mean_temp_lead)
        # Join the new data frame with the original one



        temp2_pred <- temp2 %>%
          drop_na(groundwater, precipitation_lag,mean_temp_lag)


        # Filter the DataFrame by the last date -- and combine with percentiles
        temp2_pred <- temp2_pred %>%
          filter(year == t) %>%
          dplyr::select(-mean_temp_lead,
                        -precip_lead) %>%
          left_join(monte_carlo_df) %>%
          mutate(groundwater_diff = NA)




                # Predict groundwater using ANN model
        predicted_response_ANN <- predict(fit_ann, newdata = temp2_pred)
        temp2_pred <- cbind(temp2_pred, predicted_response_ANN)




        #Combine data
        temp2_pred <- temp2_pred %>%
          dplyr::rename(
                        ANN = predicted_response_ANN
          ) %>%
          gather(c(
            #GAM,GLM,#RF,
                   ANN), key = "Model", value = predicted_value) %>%
         # mutate(actual_predicted_groundwater = actual_predicted_groundwater*(max_groundwater - min_groundwater) + min_groundwater,
          #       predicted_value = predicted_value*(max_groundwater - min_groundwater) + min_groundwater) %>%
          mutate(
            predicted_value = predicted_value*(max_groundwater_diff - min_groundwater_diff) + min_groundwater_diff,
            groundwater = groundwater*(max_groundwater - min_groundwater) + min_groundwater,
            predicted_value = groundwater + predicted_value,
            actual_predicted_groundwater = actual_predicted_groundwater*(max_groundwater - min_groundwater) + min_groundwater,
            # predicted_value = predicted_value*(max_groundwater - min_groundwater) + min_groundwater
          ) %>%
          group_by(Well, Date,Date_predicted, days_in_year,year,lag_day,Model, groundwater,actual_predicted_groundwater) %>%
          summarise(predicted_value_mean = mean(predicted_value, na.rm = TRUE),
                    predicted_value_min = min(predicted_value, na.rm = TRUE),
                    predicted_value_max = max(predicted_value, na.rm = TRUE),
                    predicted_value_25th =  quantile(predicted_value, 0.25, na.rm = TRUE),
                    predicted_value_75th = quantile(predicted_value, 0.75, na.rm = TRUE),
                    predicted_value_50th = quantile(predicted_value, 0.50, na.rm = TRUE),
                    predicted_value_90th = quantile(predicted_value, 0.90, na.rm = TRUE),
                    predicted_value_10th = quantile(predicted_value, 0.10, na.rm = TRUE),
                    predicted_value_5th = quantile(predicted_value, 0.05, na.rm = TRUE),
                    predicted_value_95th = quantile(predicted_value, 0.95, na.rm = TRUE)
          ) %>%
          ungroup() %>%
          mutate(
            quantile_range = case_when(
              actual_predicted_groundwater <= predicted_value_5th ~ "Below 5th",
              actual_predicted_groundwater <= predicted_value_10th ~ "5th-10th",
              actual_predicted_groundwater <= predicted_value_25th ~ "10th-25th",
              actual_predicted_groundwater <= predicted_value_50th ~ "25th-50th",
              actual_predicted_groundwater <= predicted_value_75th ~ "50th-75th",
              actual_predicted_groundwater <= predicted_value_90th ~ "75th-90th",
              actual_predicted_groundwater <= predicted_value_95th ~ "90th-95th",
              TRUE ~ "Above 95th"
            )
            )

        Time_series_data_2 <- rbind(Time_series_data_2,temp2_pred)




        }

        Time_series_data_3_validation <- rbind(Time_series_data_3_validation,Time_series_data_2_validation)

        Time_series_data_3_operational <- rbind(Time_series_data_3_operational,Time_series_data_2)


        }


    } else if (unique(pgown_well_info_Well_info$Snow_influenced == 1)) {      # SNOW INFLUENCED ----------------------------------------------------------
      plot_type <- "snow"

      temp_years <- Time_series_data %>%
        filter(Well == y) %>%
        mutate(groundwater_lag = lag(groundwater,lag_period),groundwater_lead_1 = lead(groundwater, 14),groundwater_lead_2 = lead(groundwater, 30),groundwater_lead_3 = lead(groundwater, 60),groundwater_lead_4 = lead(groundwater,90)) %>%
        drop_na(groundwater,total_precip,mean_temp,SWE,groundwater_lag,groundwater_lead_1,groundwater_lead_2,groundwater_lead_3,groundwater_lead_4) %>%
        mutate(year = year(Date)) %>%
        filter(year!= year(Sys.Date()))
   # year_list <- list("2022")
     year_list <- as.list(unique(temp_years$year))

       for (t in year_list) {
         Time_series_data_2_validation <- data.frame()
         Time_series_data_2 <- data.frame()

     # t = 2022
      #run for predicted forcast intervals (i.e 14,30,60 and 90 days)
        for (x in forecast_days) {

        #summarise leading and lagging variables


          temp2 <- temp %>%
          #mutate(max_groundwater = max(groundwater, na.rm = TRUE),
          #         min_groundwater = min(groundwater, na.rm = TRUE),
           #        max_total_precip = max(total_precip, na.rm = TRUE),
            #       min_total_precip = min(total_precip, na.rm = TRUE),
             #      max_temp = max(mean_temp, na.rm = TRUE),
            #       min_temp = min(mean_temp, na.rm = TRUE)) %>%
          #  mutate(total_precip = (total_precip - min_total_precip)/(max_total_precip-min_total_precip),
           #        groundwater = (groundwater - min_groundwater)/(max_groundwater-min_groundwater),
            #       mean_temp = (mean_temp - min_temp)/(max_temp-min_temp)
            #) %>%
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
              SWE_lead_diff#,
              #max_groundwater,
              #min_groundwater
              )




          calculate_weighted_lead <- function(i, x, lag_period, a_coeff) {
            #i = 5

            temp %>%
          #    mutate(max_groundwater = max(groundwater, na.rm = TRUE),
           #          min_groundwater = min(groundwater, na.rm = TRUE),
            #         max_total_precip = max(total_precip, na.rm = TRUE),
             #        min_total_precip = min(total_precip, na.rm = TRUE),
            #         max_temp = max(mean_temp, na.rm = TRUE),
             #        min_temp = min(mean_temp, na.rm = TRUE)) %>%
            #  mutate(total_precip = (total_precip - min_total_precip)/(max_total_precip-min_total_precip),
             #        groundwater = (groundwater - min_groundwater)/(max_groundwater-min_groundwater),
              #       mean_temp = (mean_temp - min_temp)/(max_temp-min_temp)
              #) %>%
              mutate(lag_day = x, #forcast interval
                     Date_predicted = lead(Date,x),
                     # precipitation_lag = rollsum(total_precip, lag_period, fill = NA, align='right', na.rm = TRUE), #recharge lagging precipitation
                     #mean_temp_lag = rollmean(mean_temp, lag_period, fill = NA,align='right', na.rm = TRUE), # recharge lagging temperature
                     #groundwater_predict = lead(groundwater, x), # actual groundwater level we are predicting
                     # mean_temp_lead = rollmean(mean_temp, x, fill = NA,align='left', na.rm = TRUE), #forecasted temperature
                     # precip_lead = rollsum(total_precip, x, fill = NA,align='left', na.rm = TRUE) #forecasted precipitation
                     SWE_lag = lag(SWE, 1), #Snow level at rechrage lag period
                     SWE_lead = lead(SWE,1)
              ) %>%
              mutate(
                SWE_lag_diff =  SWE - SWE_lag, #snowmelt that occured during lag period
                SWE_lead_diff =  SWE_lead - SWE) %>%
              mutate(lag_day_adjusted = (x- lag_period)) %>%
              mutate(weight_max = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period*a_coeff)) %>%
              mutate(normal_weight = dnorm(i, mean = lag_day_adjusted, sd = lag_period*a_coeff )) %>%
              mutate(normal_weight = (normal_weight - 0)/(weight_max-0)) %>%
              mutate(weight_max2 = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period*a_coeff_snow)) %>%
              mutate(normal_weight2 = dnorm(i, mean = lag_day_adjusted, sd = lag_period*a_coeff_snow )) %>%
              mutate(normal_weight2 = (normal_weight2 - 0)/(weight_max2-0)) %>%
              mutate(weighted_lead = lead(total_precip,i)*normal_weight,
                     weighted_lead_temp = lead(mean_temp,i)*normal_weight2,
                     weighted_lead_SWE = lead(SWE_lead_diff,i)*normal_weight2) %>%
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
                      weighted_lead_temp = mean(weighted_lead_temp, na.rm = TRUE)) %>%
            ungroup()




          lag_period_length = lag_period*3

          calculate_weighted_lag <- function(i, lag_period, a_coeff) {
            #i = 5

            temp %>%
              #mutate(#max_groundwater = max(groundwater, na.rm = TRUE),
                     #min_groundwater = min(groundwater, na.rm = TRUE),
                     #max_total_precip = max(total_precip, na.rm = TRUE),
                     #min_total_precip = min(total_precip, na.rm = TRUE),
                     #max_temp = max(mean_temp, na.rm = TRUE),
                     #min_temp = min(mean_temp, na.rm = TRUE),
                     #max_SWE = max(SWE, na.rm = TRUE),
                     #min_SWE = min(SWE, na.rm = TRUE)) %>%
              #mutate(total_precip = (total_precip - min_total_precip)/(max_total_precip-min_total_precip),
               #      groundwater = (groundwater - min_groundwater)/(max_groundwater-min_groundwater),
                #     SWE = (SWE - min_SWE)/(max_SWE-min_SWE),
                 #    mean_temp = (mean_temp - min_temp)/(max_temp-min_temp)
              #) %>%
              mutate(lag_day = x, #forcast interval
                     Date_predicted = lead(Date,x),
                     SWE_lag = lag(SWE, 1), #Snow level at rechrage lag period
                     SWE_lead = lead(SWE,1)
                     # precipitation_lag = rollsum(total_precip, lag_period, fill = NA, align='right', na.rm = TRUE), #recharge lagging precipitation
                     #mean_temp_lag = rollmean(mean_temp, lag_period, fill = NA,align='right', na.rm = TRUE), # recharge lagging temperature
                     #groundwater_predict = lead(groundwater, x), # actual groundwater level we are predicting
                     # mean_temp_lead = rollmean(mean_temp, x, fill = NA,align='left', na.rm = TRUE), #forecasted temperature
                     # precip_lead = rollsum(total_precip, x, fill = NA,align='left', na.rm = TRUE) #forecasted precipitation
              ) %>%
              mutate(
                SWE_lag_diff =  SWE - SWE_lag, #snowmelt that occured during lag period
                SWE_lead_diff =  SWE_lead - SWE) %>%
              mutate(lag_day_adjusted = lag_period -x) %>%
              mutate(weight_max = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period*a_coeff)) %>%
              mutate(weight_max2 = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period*a_coeff_snow)) %>%
              mutate(normal_weight = dnorm(i, mean = lag_day_adjusted, sd = lag_period*a_coeff )) %>%
              mutate(normal_weight2 = dnorm(i, mean = lag_day_adjusted, sd = lag_period*a_coeff_snow )) %>%
              mutate(normal_weight = (normal_weight - 0)/(weight_max-0)) %>%
              mutate(normal_weight2 = (normal_weight2 - 0)/(weight_max2-0)) %>%
              mutate(weighted_lag = lag(total_precip,i)*normal_weight,
                     weighted_lag_temp = lag(mean_temp,i)*normal_weight2,
                     weighted_lag_SWE = lag(SWE_lead_diff,i)*normal_weight2) %>%
              dplyr::select( # only select variables of interest remove others
                Date,
                Date_predicted,
                Well,
                lag_day,
                lag_day_adjusted,
                weighted_lag,
                weighted_lag_SWE,
                weighted_lag_temp#,
                #max_groundwater,
                #min_groundwater
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
                      ) %>%
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




          temp2_original <- temp2




        # create a dataframe to train model with no missing values

        temp2_2 <- temp2 %>%
          drop_na(groundwater,groundwater_diff, groundwater_predict, mean_temp_lag, mean_temp_lead,  precipitation_lag ,precip_lead ,SWE,SWE_lag_diff,SWE_lead_diff) %>%
          filter(year != t)


       # fit_rf <- randomForest(groundwater_diff ~ groundwater + precip_lead + precipitation_lag + mean_temp_lead + mean_temp_lag + SWE_lag_diff + SWE + SWE_lead_diff , data = temp2_2, ntree = 2000, importance = TRUE)

        fit_ann <- nnet(groundwater_diff ~  groundwater + precip_lead + precipitation_lag + mean_temp_lead + mean_temp_lag + SWE_lag_diff + SWE + SWE_lead_diff, data = temp2_2, size = ann_size, decay = ann_decay, maxit = ann_maxit)


        # snowmelt model

        temp2_pred <- temp2 %>%
          filter(year == t) %>%
          drop_na(groundwater, precipitation_lag,mean_temp_lag, SWE_lag_diff, SWE) %>%
          mutate(groundwater_diff = NA)




        #predict groundwater using RF model

         # predicted_response_RF <- predict(fit_rf, newdata = temp2_pred)

          # temp2_pred <- cbind(temp2_pred, predicted_response_RF)


        # Predict groundwater using ANN model
        predicted_response_ANN <- predict(fit_ann, newdata = temp2_pred)
        temp2_pred <- cbind(temp2_pred, predicted_response_ANN)

        # Predict groundwater using Wavelet ANN model


        temp2_pred <- temp2_pred %>%
          dplyr::rename(#GAM = predicted_response_GAM,
                        #GLM = predicted_response_GLM,
                                  #     RF = predicted_response_RF,
                        ANN = predicted_response_ANN
                        ) %>%
          gather(c(#GAM, GLM,#RF,
                   ANN
                   ), key = "Model", value = predicted_value)


        temp2_pred <- temp2_pred %>%
          mutate(predicted_value = predicted_value*(max_groundwater_diff - min_groundwater_diff) + min_groundwater_diff,
                 groundwater = groundwater*(max_groundwater - min_groundwater) + min_groundwater,
                 actual_predicted_groundwater = actual_predicted_groundwater*(max_groundwater - min_groundwater)+ min_groundwater) %>%
          mutate(predicted_value = groundwater + predicted_value)


       Time_series_data_2_validation <-  rbind(Time_series_data_2_validation,temp2_pred) #%>%
       # Time_series_data_2_validation <- temp2_pred #%>%


        # Fit linear model to each year's data
# Plot data and fitted lines for each year




     # print(combined_plot)

     # ggsave(temp_plot2, filename = paste0("R/For Client/Groundwater Drought Forecasting R Toolkit version 2/Output/model testing/operational/Snowmelt_model_Comparison_",y,"_",t,"_",x,".jpeg") ,height = 8, width = 5, units = "in")


          # Calculate the mean and standard deviation of the variables by day
          daily_stats<- temp2_original %>%
            group_by(days_in_year) %>%
            summarise(
              precip_lead_mean = mean(precip_lead, na.rm = TRUE),
              precip_lead_sd = sd(precip_lead, na.rm = TRUE),
              mean_temp_lead_mean = mean(mean_temp_lead, na.rm = TRUE),
              mean_temp_lead_sd = sd(mean_temp_lead, na.rm = TRUE),
              SWE_lead_diff_mean = mean(SWE_lead_diff, na.rm = TRUE),
              SWE_lead_diff_sd = sd(SWE_lead_diff, na.rm = TRUE)
            )


          # Generate Monte Carlo values for each day
          monte_carlo_df <- daily_stats %>%
            tidyr::uncount(200) %>%
            mutate(
              precip_lead_MC = rnorm(n(), mean = precip_lead_mean, sd = precip_lead_sd),
              mean_temp_lead_MC = rnorm(n(), mean = mean_temp_lead_mean, sd = mean_temp_lead_sd),
              SWE_lead_diff_MC = rnorm(n(), mean = SWE_lead_diff_mean, sd = SWE_lead_diff_sd)

            )

          monte_carlo_df <- monte_carlo_df %>%
            dplyr::select(days_in_year, precip_lead_MC, mean_temp_lead_MC,SWE_lead_diff_MC)
          # Join the new data frame with the original one



          temp2_pred <- temp2

          #last_date <- max(temp2_pred$Date, na.rm = TRUE)

          # Filter the DataFrame by the last date -- and combine with percentiles
          temp2_pred <- temp2_pred %>%
            filter(year == t) %>%
            left_join(monte_carlo_df) %>%
            mutate(SWE_lead_diff_MC = SWE_lead_diff_MC*(max_SWE_lead_diff - min_SWE_lead_diff) + min_SWE_lead_diff) %>%
            mutate(mean_temp_lead =  mean_temp_lead_MC,
                   precip_lead = precip_lead_MC,
                   SWE_lead_diff = ifelse(SWE_lead_diff_MC < 0,ifelse(SWE_lead_diff_MC <= -SWE, -SWE, SWE_lead_diff_MC),SWE_lead_diff_MC)) %>%
            mutate(SWE_lead_diff = (SWE_lead_diff - min_SWE_lead_diff)/(max_SWE_lead_diff-min_SWE_lead_diff)) %>%
              dplyr::select(-mean_temp_lead_MC,
                          -precip_lead_MC,
                          -SWE_lead_diff_MC) %>%
            mutate(groundwater_diff = NA)




          # Predict groundwater using ANN model
          predicted_response_ANN <- predict(fit_ann, newdata = temp2_pred)
          temp2_pred <- cbind(temp2_pred, predicted_response_ANN)


           temp2_pred <- temp2_pred %>%
            dplyr::rename(#GAM = predicted_response_GAM,
                   #GLM = predicted_response_GLM,
                   #RF = predicted_response_RF,
               ANN = predicted_response_ANN
                   ) %>%
            gather(c(#GAM,GLM,#RF,
                     ANN), key = "Model", value = predicted_value) %>%
             mutate(
               predicted_value = predicted_value*(max_groundwater_diff - min_groundwater_diff) + min_groundwater_diff,
               groundwater = groundwater*(max_groundwater - min_groundwater) + min_groundwater,
               predicted_value = groundwater + predicted_value,
               actual_predicted_groundwater = actual_predicted_groundwater*(max_groundwater - min_groundwater) + min_groundwater,
               # predicted_value = predicted_value*(max_groundwater - min_groundwater) + min_groundwater
             ) %>%
             group_by(Well, Date,Date_predicted, days_in_year,year,lag_day,Model, groundwater,actual_predicted_groundwater) %>%
             summarise(predicted_value_mean = mean(predicted_value, na.rm = TRUE),
                       predicted_value_min = min(predicted_value, na.rm = TRUE),
                       predicted_value_max = max(predicted_value, na.rm = TRUE),
                       predicted_value_25th =  quantile(predicted_value, 0.25, na.rm = TRUE),
                       predicted_value_75th = quantile(predicted_value, 0.75, na.rm = TRUE),
                       predicted_value_50th = quantile(predicted_value, 0.50, na.rm = TRUE),
                       predicted_value_90th = quantile(predicted_value, 0.90, na.rm = TRUE),
                       predicted_value_10th = quantile(predicted_value, 0.10, na.rm = TRUE),
                       predicted_value_5th = quantile(predicted_value, 0.05, na.rm = TRUE),
                       predicted_value_95th = quantile(predicted_value, 0.95, na.rm = TRUE)
             ) %>%
             ungroup() %>%
             mutate(
               quantile_range = case_when(
                 actual_predicted_groundwater <= predicted_value_5th ~ "Below 5th",
                 actual_predicted_groundwater <= predicted_value_10th ~ "5th-10th",
                 actual_predicted_groundwater <= predicted_value_25th ~ "10th-25th",
                 actual_predicted_groundwater <= predicted_value_50th ~ "25th-50th",
                 actual_predicted_groundwater <= predicted_value_75th ~ "50th-75th",
                 actual_predicted_groundwater <= predicted_value_90th ~ "75th-90th",
                 actual_predicted_groundwater <= predicted_value_95th ~ "90th-95th",
                 TRUE ~ "Above 95th"
               )
             )


         Time_series_data_2 <- rbind(Time_series_data_2, temp2_pred)

        #  Time_series_data_2 <-  temp2_pred



        }




         Time_series_data_3_validation <- rbind(Time_series_data_3_validation,Time_series_data_2_validation)

         Time_series_data_3_operational <- rbind(Time_series_data_3_operational,Time_series_data_2)


      }}

    #save data for internal figures if needed
    filename <- paste0(output_path, "/Model_well_raw_results_validaction_testing_", y, ".Rdata")
    save(Time_series_data_3_validation, file = filename )

    filename <- paste0(output_path, "/Model_well_raw_results_operational_testing_", y, ".Rdata")
    save(Time_series_data_3_operational, file = filename )


    temp_WL_states <- temp %>%
      filter(Well == y) %>%
      drop_na(groundwater) %>%
      group_by(Well) %>%
      mutate(Mean_total = mean(groundwater)) %>%
      mutate(days_in_year = yday(Date),
             Month = month(Date),
             Day = day(Date)) %>%
      #filter(Date <= as.Date("2023-12-31")) %>%
      filter(Month != 2 | Day != 29) %>%
      group_by(Well, Month, Day) %>%
      summarise(Min = min(groundwater),
                per5th = quantile(groundwater, 0.05),
                per10th = quantile(groundwater, 0.1),
                per25th = quantile(groundwater, 0.25),
                per50th = quantile(groundwater, 0.5),
                per75th = quantile(groundwater, 0.75),
                per90th = quantile(groundwater, 0.9),
                per95th = quantile(groundwater, 0.95),
                Max = max(groundwater),
                Mean = mean(groundwater),
                Mean_total = mean(Mean_total)) %>%
      ungroup() %>%
      mutate(fake_date = as.Date(paste0("2020-", Month, "-", Day)),
             days_in_year = yday(fake_date))


    summer_months <- c(5, 6, 7, 8, 9, 10)


    # Assuming your data is in 'temp'
    simulated_data_compiled_stats <- Time_series_data_3_validation %>%
      mutate(month = month(Date_predicted)) %>%
      mutate(Simulation_lag = paste0("Days ", lag_day)) %>%
      group_by(Well, Model, lag_day) %>%
      mutate(actual_predicted_groundwater_RM = rollmean(actual_predicted_groundwater,
                                                        k = 30, fill = NA, align = "center")) %>%
      mutate(predicted_value_RM = rollmean(predicted_value, k = 30, fill = NA, align = "center")) %>%
      ungroup() %>%
      left_join(temp_WL_states) %>%
      mutate(count = 1) %>%
      drop_na(actual_predicted_groundwater, predicted_value) %>%
      group_by(Well, Model, Simulation_lag, lag_day, days_in_year, Min, Max, year) %>%
      mutate(RMSE = sqrt(mean((actual_predicted_groundwater - predicted_value)^2)),
             MSE = mean((actual_predicted_groundwater - predicted_value)^2),
             residual = abs(actual_predicted_groundwater - predicted_value),
             ssr = sum((actual_predicted_groundwater - predicted_value)^2),
             sst = sum((actual_predicted_groundwater - Mean_total)^2),
             ssr_RM = sum((actual_predicted_groundwater_RM - predicted_value_RM)^2),
             sst_RM = sum((actual_predicted_groundwater_RM - Mean_total)^2),
             min_data = min(actual_predicted_groundwater),
             max_data = max(actual_predicted_groundwater),
             count = sum(count)) %>%
      mutate(range = Max - Min) %>%
      ungroup() %>%
      mutate(NRMSE = ifelse(range == 0, NA, RMSE / range))



    simulated_data_compiled_stats_summer <- simulated_data_compiled_stats %>%
      filter(month %in% summer_months) %>%
      group_by(Well, Model, Simulation_lag, lag_day, year) %>%
      summarise(RMSE_summer = mean(RMSE),
                NRMSE_summer = mean(NRMSE),
                count_summer = sum(count)) %>%
      ungroup()


    simulated_data_compiled_stats_year <- simulated_data_compiled_stats %>%
      group_by(Well, Model, Simulation_lag, lag_day, year) %>%
      summarise(RMSE = mean(RMSE),
                SD = sd(residual), # mae = mean(abs(groundwater - predicted_value)),
                ssr = sum(ssr),
                sst = sum(sst),
                ssr_RM = sum(ssr_RM, na.rm = TRUE),
                sst_RM = sum(sst_RM, na.rm = TRUE),
                MSE = mean(MSE),
                NRMSE = mean(NRMSE),
                min_data = min(min_data),
                max_data = max(max_data),
                count = sum(count)) %>%
      mutate(R2 = 1 - (ssr / sst)) %>%
      mutate(R2_RM = 1 - (ssr_RM / sst_RM)) %>%
      mutate(range = max_data - min_data) %>%
      ungroup() %>%
      mutate(perc_year = count / 365) %>%
      left_join(simulated_data_compiled_stats_summer, by = c("Well", "Model", "Simulation_lag", "lag_day", "year"))




    simulated_data_compiled_stats_total <- simulated_data_compiled_stats %>%
      group_by(Well,Model,Simulation_lag,lag_day) %>%
      summarise(RMSE = mean(RMSE),
                MSE = mean(MSE),
                NRMSE = mean(NRMSE, na.rm = TRUE),
                SD = sd(residual),# mae = mean(abs(groundwater - predicted_value)),
                ssr = sum(ssr),
                sst = sum(sst),
                ssr_RM = sum(ssr_RM, na.rm = TRUE),
                sst_RM = sum(sst_RM, na.rm = TRUE),
                min_data = min(min_data),
                max_data = max(max_data)) %>%
      mutate(range = max_data - min_data) %>%
      mutate(R2 = 1 - (ssr/sst)) %>%
      mutate(R2_RM = 1 - (ssr_RM/sst_RM)) %>%
      ungroup()



    simulated_data_compiled_stats_year <- simulated_data_compiled_stats_year %>%
      mutate_at(vars(R2, R2_RM,MSE, RMSE, NRMSE,NRMSE_summer,  SD), ~ifelse(perc_year >= 0.6, ., NA)) %>%
      dplyr::select(Well, Model, Simulation_lag, lag_day, year, R2,R2_RM, MSE, RMSE, NRMSE, NRMSE_summer, SD)

    # simulated_data_compiled_stats_year <- left_join(simulated_data_compiled_stats_year,pgown_well_info_all_2 )

    #write_csv(simulated_data_compiled_stats_year, paste0(data_location_files,"Model_stats_well_individual_years.csv"))

    simulated_data_compiled_stats_total <- simulated_data_compiled_stats_total %>%
      dplyr::select(Well, Model, Simulation_lag, lag_day, R2,R2_RM, MSE, RMSE, NRMSE,  SD)

    #simulated_data_compiled_stats_total <- left_join(simulated_data_compiled_stats_total,pgown_well_info_all_2 )


    #write_csv(simulated_data_compiled_stats_total, paste0(data_location_files,"Model_stats_well_totals.csv"))

    #temp <- read_csv(paste0(data_location_files,"Model_stats_well_totals",x,".csv"))




    temp_stats2 <- simulated_data_compiled_stats_total %>%
      mutate(#R2 = signif(R2, 2),
        R2 = signif(R2, 2)) %>%
      mutate(Stats = paste0("R2 = ",R2)) %>%
      #mutate(NRMSE = NRMSE*100) %>%
      mutate(NRMSE = signif(NRMSE, 2)) %>%
      mutate(Stats2 = paste0("NRMSE = ",NRMSE)) %>%
      mutate(LabelVJust = case_when(
        Model == "ANN" ~ 1.5
      ))


    #simulated_data_compiled_stats_total



    temp2 <- Time_series_data_3_validation %>%
      mutate(Simulation_lag = paste0("Days ", lag_day))


    gglayers <- list(
      #scale_y_continuous(sec.axis = sec_axis( trans=~.-temp2_toc, name="Depth below ground surface (m)")),
      theme_bw(),
      ylab("Water Level Range Around Long-Term Mean (m)"),
      xlab(""),
      #scale_x_date(date_labels = ("%b"), date_breaks = "1 month"),
      theme(#legend.title = element_blank(),
        legend.position = "right",
        legend.background = element_blank(),
        axis.text.x = element_text(angle = 30, hjust =1),
        legend.spacing.x = unit(0.001, 'mm')))

    temp_plot2<- ggplot()+
      geom_line(data = temp2, aes( x = Date_predicted, y =predicted_value, colour = Model),  linetype = 1,size = 1)+
      # geom_text(data = temp_stats, aes(label = Stats, colour = Model, vjust = LabelVJust), x = Inf, y = Inf, hjust = 1.5,  size = 3)+
      #  geom_text(data = temp_stats2, aes(label = Stats, colour = Model, vjust = LabelVJust), x = -Inf, y = Inf, hjust = -0.5, size = 3) +
      geom_text(data = temp_stats2, aes(label = Stats, colour = Model, vjust = LabelVJust), x = Inf, y = Inf, hjust = 1.5,  size = 3)+
      geom_text(data = temp_stats2, aes(label = Stats2, colour = Model, vjust = LabelVJust), x = -Inf, y = Inf, hjust = -0.5, size = 3) +
      scale_colour_manual(name = "",values = c( "orange", "#0000FF","green","pink","dark green"))+
      new_scale_colour()+
      # geom_line(data = Time_series_data_2_plot, aes(x = fake_date2, y = groundwater, colour = "Actual Water Level2"),  linewidth = 1)+
      geom_line(data = temp2, aes(x = Date_predicted, y = actual_predicted_groundwater, colour = "Actual Water Level"),  linewidth = 1)+
      # geom_path(data = pgown_data4, aes(x = fake_date, y = percentile, colour = "Average (daily)",linetype ="Average (daily)"),size = 1.2)+
      scale_colour_manual(name = "",values = c(  "red","grey", "#0000FF","green","orange","yellow"))+
      #labs(title = paste("Well = ",y,"; year =", t))+
      facet_grid(Simulation_lag~., scale = "free_y")+
      gglayers

    temp_plot2



    ggsave(temp_plot2, filename = paste0(output_path, "/validation_results_", y, ".jpeg"), height = 8, width = 11.5, units = "in")


    # Calculate skill by comparing quantile ranges
    hit_rate_stat <- Time_series_data_3_operational %>%
      rowwise() %>%
      mutate(
        hit_25_75th = ifelse(actual_predicted_groundwater >= predicted_value_25th & actual_predicted_groundwater <= predicted_value_75th, 1, 0),
        hit_10_90th = ifelse(actual_predicted_groundwater >= predicted_value_10th & actual_predicted_groundwater <= predicted_value_90th, 1, 0),
        hit_min_max = ifelse(actual_predicted_groundwater >= predicted_value_min & actual_predicted_groundwater <= predicted_value_max, 1, 0),
        false_alarm = ifelse(actual_predicted_groundwater < predicted_value_min | actual_predicted_groundwater > predicted_value_max, 1, 0)
      ) %>%
      mutate(range = predicted_value_95th-predicted_value_5th) %>%
      group_by(Well, Model, lag_day, days_in_year) %>%
      mutate(range_actual = max(actual_predicted_groundwater, na.rm = TRUE)- min(actual_predicted_groundwater, na.rm = TRUE)) %>%
      mutate(ratio =range/range_actual,
             mean_groundwater = mean(actual_predicted_groundwater,na.rm = TRUE),
             max_groundwater = max(actual_predicted_groundwater, na.rm = TRUE),
             min_groundwater = min(actual_predicted_groundwater, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(  mean_rmse  = (predicted_value_50th - actual_predicted_groundwater)^2,
               mean_rmse_ref = ( mean_groundwater - actual_predicted_groundwater)^2,
               min_rmse  = (predicted_value_5th - actual_predicted_groundwater)^2,
               min_rmse_ref = ( min_groundwater - actual_predicted_groundwater)^2,
               max_rmse  = (predicted_value_95th - actual_predicted_groundwater)^2,
               max_rmse_ref = ( max_groundwater - actual_predicted_groundwater)^2) %>%
      group_by(Well, Model, lag_day) %>%
      summarise(
        hit_25_75th = mean(hit_25_75th, na.rm = TRUE),
        hit_10_90th = mean(hit_10_90th, na.rm = TRUE),
        hit_min_max = mean(hit_min_max, na.rm = TRUE),
        false_alarm_rate = mean(false_alarm, na.rm = TRUE),
        ratio = mean(ratio, na.rm = TRUE),
        mean_rmse = mean(mean_rmse, na.rm = TRUE),
        mean_rmse_ref = mean(mean_rmse_ref, na.rm = TRUE),
        min_rmse = mean(min_rmse, na.rm = TRUE),
        min_rmse_ref = mean(min_rmse_ref, na.rm = TRUE),
        max_rmse = mean(max_rmse, na.rm = TRUE),
        max_rmse_ref = mean(max_rmse_ref, na.rm = TRUE)
      ) %>%
      mutate(RMSE_ratio = mean_rmse/mean_rmse_ref,
             min_RMSE_ratio = min_rmse/min_rmse_ref,
             max_RMSE_ratio = max_rmse/max_rmse_ref)

    hit_rate_stat_sum <- hit_rate_stat %>%
      dplyr::select(Well, Model, lag_day, RMSE_ratio, ratio) %>%
      dplyr::rename("ROSR" = ratio,
             "RSR" = RMSE_ratio)



    Stat_summary <- full_join(simulated_data_compiled_stats_total,hit_rate_stat_sum )




    temp_stats_operation2 <- hit_rate_stat %>%
      mutate(RMSE_ratio = signif(RMSE_ratio, 2)) %>%
      mutate(ratio = signif(ratio, 2)) %>%
      mutate(Stats4 = paste0("RSR = ",RMSE_ratio)) %>%
      mutate(Stats5 = paste0("Ratio Obs to Sim range = ",ratio)) %>%
      mutate(Simulation_lag = paste0("Days ", lag_day))



    gglayers <- list(
      #scale_y_continuous(sec.axis = sec_axis( trans=~.-temp2_toc, name="Depth below ground surface (m)")),
      theme_bw(),
      ylab("Water Level Range Around Long-Term Mean (m)"),
      xlab(""),
      #scale_x_date(date_labels = ("%b"), date_breaks = "1 month"),
      theme(#legend.title = element_blank(),
        legend.position = "right",
        legend.background = element_blank(),
        axis.text.x = element_text(angle = 30, hjust =1),
        legend.spacing.x = unit(0.001, 'mm')))

    temp_plot2<- ggplot()+
      new_scale_fill()+
      #   geom_ribbon(data = temp_operation2, aes(x = Date_predicted,ymax = predicted_value_5th, ymin = predicted_value_min,  fill = "1 Min - Q5"), size = 1)+
      geom_ribbon(data = Time_series_data_3_operational, aes(x = Date_predicted,ymax = predicted_value_10th, ymin = predicted_value_5th,  fill = "2 Q5 - Q10"), size = 1)+
      geom_ribbon(data = Time_series_data_3_operational, aes(x = Date_predicted,ymax = predicted_value_25th, ymin = predicted_value_10th, fill = "3 Q10 - Q25"), size = 1)+
      geom_ribbon(data = Time_series_data_3_operational, aes(x = Date_predicted, ymax = predicted_value_75th, ymin = predicted_value_25th, fill = "4 Q25 - Q75"), size = 1)+
      geom_ribbon(data = Time_series_data_3_operational, aes(x = Date_predicted, ymax = predicted_value_90th, ymin = predicted_value_75th, fill = "5 Q75 - Q90"), size = 1)+
      geom_ribbon(data = Time_series_data_3_operational, aes(x = Date_predicted, ymax = predicted_value_95th, ymin = predicted_value_90th, fill = "6 Q90 - Q95"), size = 1)+
      #  geom_ribbon(data = temp_operation2, aes(x = Date_predicted, ymax = predicted_value_max, ymin = predicted_value_95th, fill = "7 Q95 - Max"), size = 1)+
      scale_fill_brewer(name = "Simulated Data Range \n (Operational)",palette = "Blues", direction = -1)+
      new_scale_fill()+
      geom_line(data = temp2, aes(x = Date_predicted, y = predicted_value, colour = "Validation"),  linetype = 1,size = 1)+
      # geom_text(data = temp_stats, aes(label = Stats, colour = Model, vjust = LabelVJust), x = Inf, y = Inf, hjust = 1.5,  size = 3)+
      #  geom_text(data = temp_stats2, aes(label = Stats, colour = Model, vjust = LabelVJust), x = -Inf, y = Inf, hjust = -0.5, size = 3) +
      geom_text(data = temp_stats2, aes(label = Stats, vjust = 1.5), colour = "black", x = Inf, y = Inf, hjust = 1.5,  size = 3)+
      geom_text(data = temp_stats2, aes(label = Stats2, vjust = 3), colour = "black", x = Inf, y = Inf, hjust = 1.5, size = 3) +
      geom_text(data = temp_stats_operation2, aes(label = Stats4, vjust = 4.5), colour = "black", x = Inf, y = Inf, hjust =  1.5, size = 3) +

       scale_colour_manual(name = "",values = c( "orange", "#0000FF","green","pink","dark green"))+
      new_scale_colour()+
      # geom_line(data = Time_series_data_2_plot, aes(x = fake_date2, y = groundwater, colour = "Actual Water Level2"),  linewidth = 1)+
      geom_line(data = temp2, aes(x = Date_predicted, y = actual_predicted_groundwater, colour = "Actual Water Level"),  linewidth = 1)+
      # geom_path(data = pgown_data4, aes(x = fake_date, y = percentile, colour = "Average (daily)",linetype ="Average (daily)"),size = 1.2)+
      scale_colour_manual(name = "",values = c(  "red","grey", "#0000FF","green","orange","yellow"))+
      #labs(title = paste("Well = ",y,"; year =", t))+
      facet_grid(Simulation_lag~., scale = "free_y")+
      gglayers
    temp_plot2

    ggsave(temp_plot2, filename = paste0(output_path, "/operational_results_", y, ".jpeg"), height = 8, width = 11.5, units = "in")





  return(Stat_summary)

                            }


}







