ANN_hyper_parameters_calibration <- function(Time_series_data, pgown_well_info, forecast_test_length, num_cores, output_path){
  
 
  
  # Creates historical groundwater levels for figures
  
  
  
  Waterlevel_adjustments <- Time_series_data %>%
    group_by(Well) %>%
    summarise(groundwater_period_average = mean(groundwater_period_average, na.rm = TRUE)) %>%
    ungroup() 
  
  # register number of Parallel cores to utilize
  
  registerDoParallel(cores = num_cores) 
  
  # Creates a list of Wells from the available data
  
  Well_list <- as.list(unique(Time_series_data$Well))
  
  # Pre-allocate a list to store the results
  simulated_data <- vector("list", length(Well_list))
  
 
  #run calculations for each well in parellel
  
  simulated_data <- foreach(y = Well_list, .combine = rbind, 
 .packages = c("ggpubr", "dplyr", "tidyverse", "mgcv","nnet","xgboost", "randomForest","zoo","ggnewscale", "cowplot")) %dopar% {
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
    
    a_coeff <- pgown_well_info %>%
      filter(Well == y) %>%
      pull(DWC_Precip)

    a_coeff_snow <- pgown_well_info %>%
      filter(Well == y) %>%
      pull(DWC_Snow)
    
    x = forecast_test_length
    
    #create empty data frame to put data    
    Time_series_data_3 <- data.frame()
    # RAIN INFLUENCED ----------------------------------------------------------
    if (unique(temp$Snow_influenced == 0)) {
      plot_type <- "rain"
      
   
      
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
          mutate(lag_day_adjusted = lag_period-x)%>%
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

 

# Define a grid of hyperparameters for tuning
param_grid_ann <- expand.grid(
size = c(5, 10, 15, 20),  # Number of neurons
  decay = c(0.001, 0.01, 0.1),  # Learning rate
  maxit = c(100, 200, 500)  # Maximum iterations
)



# Function to perform k-fold cross-validation for ANN
k_fold_cv_nnet <- function(data, k, size, decay, maxit) {
  set.seed(123)  # For reproducibility
  folds <- cut(seq(1, nrow(data)), breaks = k, labels = FALSE)
  cv_errors <- c()
  
  for (i in 1:k) {
    # Split data into training and validation sets
    validation_indices <- which(folds == i, arr.ind = TRUE)
    validation_data <- data[validation_indices, ]
    training_data <- data[-validation_indices, ]
    
    # Train the model
    fit_ann <- nnet(
      groundwater_diff ~ groundwater + precip_lead + mean_temp_lead + precipitation_lag + mean_temp_lag,
      data = training_data,
      size = size,
      decay = decay,
      maxit = maxit,
      linout = TRUE,
      trace = FALSE
    )
    
    # Predict on validation set
    predictions <- predict(fit_ann, newdata = validation_data)
    validation_error <- mean((validation_data$groundwater_diff - predictions)^2)
    cv_errors <- c(cv_errors, validation_error)
  }
  
  return(mean(cv_errors))  # Return the average cross-validation error
}

# Apply the function to the grid of hyperparameters
cv_results_ann <- apply(param_grid_ann, 1, function(params) {
  print(params)  # Debugging print
  k_fold_cv_nnet(temp2_2, k = 5, size = params[1], decay = params[2], maxit = params[3])
})

# Find the best hyperparameters
best_params_ann <- param_grid_ann[which.min(cv_results_ann), ]

ann_size <-  best_params_ann$size
ann_decay <- best_params_ann$decay
ann_maxit <- best_params_ann$maxit



params <- data.frame(
  Well = y,
  ann_size = ann_size,
  ann_decay = ann_decay,
  ann_maxit = ann_maxit
)


        
    }else if (unique(temp$Snow_influenced == 1)) {      # SNOW INFLUENCED ----------------------------------------------------------
      plot_type <- "snow"
      
      
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
        #i = 5
        
        temp %>%
            mutate(lag_day = x, #forcast interval
                 Date_predicted = lead(Date,x),
                 SWE_lag = lag(SWE, 1), #Snow level at rechrage lag period
                 SWE_lead = lead(SWE,1)
                        ) %>%
          mutate(
            SWE_lag_diff =  SWE - SWE_lag, #snowmelt that occured during lag period
            SWE_lead_diff =  SWE_lead - SWE)%>%
          mutate(lag_day_adjusted = lag_period-x)%>%
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
      

        param_grid_ann <- expand.grid(
          size = c(5, 10, 15),  # Number of neurons
          decay = c(0.001, 0.01, 0.1),  # Learning rate
          maxit = c(200, 500)  # Maximum iterations
        )

        
        # Function to perform k-fold cross-validation for ANN
        k_fold_cv_nnet <- function(data, k, size, decay, maxit) {
          set.seed(123)  # For reproducibility
          folds <- cut(seq(1, nrow(data)), breaks = k, labels = FALSE)
          cv_errors <- c()
          
          for (i in 1:k) {
            # Split data into training and validation sets
            validation_indices <- which(folds == i, arr.ind = TRUE)
            validation_data <- data[validation_indices, ]
            training_data <- data[-validation_indices, ]
            
            # Train the model
            fit_ann <- nnet(
              groundwater_diff ~ groundwater + precip_lead + precipitation_lag + mean_temp_lead + mean_temp_lag + SWE_lag_diff + SWE + SWE_lead_diff,
              data = training_data,
              size = size,
              decay = decay,
              maxit = maxit,
              linout = TRUE,
              trace = FALSE
            )
            
            # Predict on validation set
            predictions <- predict(fit_ann, newdata = validation_data)
            validation_error <- mean((validation_data$groundwater_diff - predictions)^2)
            cv_errors <- c(cv_errors, validation_error)
          }
          
          return(mean(cv_errors))  # Return the average cross-validation error
        }
        
        # Apply the function to the grid of hyperparameters
        cv_results_ann <- apply(param_grid_ann, 1, function(params) {
          print(params)  # Debugging print
          k_fold_cv_nnet(temp2_2, k = 5, size = params[1], decay = params[2], maxit = params[3])
        })
        
        # Find the best hyperparameters
        best_params_ann <- param_grid_ann[which.min(cv_results_ann), ]
        
        ann_size <-  best_params_ann$size
        ann_decay <- best_params_ann$decay
        ann_maxit <- best_params_ann$maxit
        
        
        
        
        params <- data.frame(
          Well = y,
          ann_size = ann_size,
          ann_decay = ann_decay,
          ann_maxit = ann_maxit
        )
        
        


    }
          
          
  return(params)

                            }
  
  

 

}

