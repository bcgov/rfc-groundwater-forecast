forecast_model <- function(Time_series_data, forecast_days, num_cores,
                           figure_location,output_path, model_path,
                           data_location, pgown_well_info,
                           rfc_forecast_include,
                           ensemble_forecast_data,
                           deterministic_forecast_data, Missing_date_window,
                           rfc_forecast_date_window,
                           generate_well_pdf) {


  # Creates historical groundwater levels for figures

  temp_WL_states <- Time_series_data %>%
    mutate(days_in_year = yday(Date),
           Month = month(Date),
           Day = day(Date),
           year = year(Date)) %>%
    filter(Date < (Sys.Date() + max(forecast_days) + 7) - years(1)) %>% # remove current year from plot
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
              Max = max(groundwater),
              .groups = "keep") %>%
    ungroup() %>%
    # mutate(fake_date = as.Date("2020-01-01") + days_in_year - 1,
    mutate(fake_date = as.Date(paste0("2020-", Month, "-", Day)),
           days_in_year = yday(fake_date))


  Waterlevel_adjustments <- Time_series_data %>%
    filter(year(Date) != year(Sys.Date())) %>%
    group_by(Well) %>%
    summarise(groundwater_period_average = mean(groundwater_period_average, na.rm = TRUE)) %>%
    ungroup()

  # register number of Parallel cores to utilize

  registerDoParallel(cores = num_cores)

  # Creates a list of Wells from the available data
  pgown_well_info_temp <- pgown_well_info


  #  filter(Snow_influenced == 0)


  pgown_well_info_temp2 <- pgown_well_info %>%
    dplyr::select(Well,Regional_group)

  last_measurements <- Time_series_data %>%
    group_by(Well) %>%
    drop_na(groundwater) %>%
    filter(Date == max(Date)) %>%
    mutate(last_measurements = Date,
           last_measurements_value = groundwater ) %>%
    ungroup() %>%
    dplyr::select(Well, last_measurements, last_measurements_value)

  last_measurements <- left_join(pgown_well_info_temp2, last_measurements, by = join_by(Well))



  Well_list <- as.list(sort(unique(pgown_well_info_temp$Well)))

  # Pre-allocate a list to store the results
  simulated_data <- vector("list", length(Well_list))


  #run calculations for each well in parallel

  simulated_data <- foreach(
    y = Well_list, .combine = rbind,
    .packages = c("ggpubr", "dplyr", "tidyr", "lubridate", "ggplot2", "purrr", "forcats", "mgcv",
                  "randomForest", "zoo", "ggnewscale","showtext", #"webshot2",
                  "grid", "cowplot", "nnet", "magick")) %dopar% {

                    # filter data by well
                    #y= "OW492"
                    #y= "OW002"
                    #y= "OW406"
                    #y= "OW459"
                    #y= "OW275"
                    #y= "OW255"
                    # y = Well_list[1]


                    last_measurements_well <- last_measurements %>%
                      filter(Well == y) %>%
                      pull(last_measurements)

                    if (between(last_measurements_well, Sys.Date() - Missing_date_window, Sys.Date())) {

                      temp <- Time_series_data %>%
                        filter(Well == y)

                      temp_WL_states_temp <- temp_WL_states %>%
                        filter(Well == y)

                      pgown_well_info_Well_info <- pgown_well_info_temp %>%
                        filter(Well == y)

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

                      ann_decay <- pgown_well_info_Well_info %>%
                        pull(ann_decay)

                      ann_maxit <- pgown_well_info_Well_info %>%
                        pull(ann_maxit)

                      # Function to classify performance based on R² and NRMSE
                      classify_performance <- function(r2, nrmse) {
                        if (r2 >= 0.8 & nrmse <= 0.15) {
                          return("Good")
                        } else if ((r2 >= 0.6 & nrmse <= 0.25) | (r2 >= 0.8 & nrmse <= 0.25)) {
                          return("Fair")
                        } else {
                          return("Poor")
                        }
                      }

                      # Function to classify forecast results based on RSR
                      classify_forecast_results <- function(rsr) {
                        if (rsr < 1) {
                          return(", forecast less sensitive to future weather")
                        } else {
                          return(", forecast more sensitive to future weather")
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
                          comb_perf_value_14 = paste0(Performance_14, "", Forecast_14),
                          comb_perf_value_30 = paste0(Performance_30, "", Forecast_30),
                          comb_perf_value_60 = paste0(Performance_60, "", Forecast_60),
                          comb_perf_value_90 = paste0(Performance_90, "", Forecast_90)
                        ) %>%
                        dplyr::select(Well, comb_perf_value_14, comb_perf_value_30, comb_perf_value_60, comb_perf_value_90) %>%
                        gather(key = "per_name", value = "performance", comb_perf_value_14, comb_perf_value_30, comb_perf_value_60, comb_perf_value_90) %>%
                        mutate(lag_day = ifelse(per_name == "comb_perf_value_14", 14,
                                                ifelse(per_name == "comb_perf_value_30", 30,
                                                       ifelse(per_name == "comb_perf_value_60", 60,
                                                              ifelse(per_name == "comb_perf_value_90", 90, NA))))) %>%
                        dplyr::select(Well, lag_day, performance)



                      # remove from data
                      # remove from legend
                      # remove from plot




                      deterministic_forecast_data_y <- deterministic_forecast_data %>%
                        filter(Well == y) %>%
                        dplyr::rename(total_precip = PP, mean_temp = TA) %>%
                        mutate(FC_type = 1) %>%
                        mutate(Date = as.Date(Date)) %>%
                        dplyr::select(Well, Date, total_precip, mean_temp, FC_type )

                      deterministic_forecast_data_y_prediction_date <- deterministic_forecast_data_y %>%
                        ungroup() %>%
                        summarise(last_measurements = min(Date, na.rm = TRUE)) %>%
                        ungroup() %>%
                        pull(last_measurements)


                      ensemble_forecast_data_y <- ensemble_forecast_data %>%
                        filter(Well == y) %>%
                        dplyr::rename(total_precip = PP, mean_temp = TA, en_sim = simulation) %>%
                        mutate(FC_type = 2) %>%
                        dplyr::select(Well, Date, total_precip, mean_temp, en_sim,FC_type )

                      max_ensemble <- max(ensemble_forecast_data_y$en_sim)

                      ensemble_forecast_data_y_prediction_date <- ensemble_forecast_data_y %>%
                        ungroup() %>%
                        summarise(last_measurements = min(Date, na.rm = TRUE)) %>%
                        ungroup() %>%
                        pull(last_measurements)


                      # TEST THIS -- MAKING VALUE FOR INCLUDING DETERM/ENSEMBLE
                      if ((Sys.Date() - deterministic_forecast_data_y_prediction_date <= rfc_forecast_date_window) &&
                          ("deterministic" %in% rfc_forecast_include)) {
                        include_deterministic_forecast <- TRUE
                      }

                      if ((Sys.Date() - ensemble_forecast_data_y_prediction_date <= rfc_forecast_date_window) &&
                          ("ensemble" %in% rfc_forecast_include)) {
                        include_ensemble_forecast <- TRUE
                      }


                      #create empty data frame to put data
                      Time_series_data_2 <- data.frame()
                      # RAIN INFLUENCED ----------------------------------------------------------
                      if (unique(pgown_well_info_Well_info$Snow_influenced == 0)) {

                        plot_type <- "rain"
                        #run for predicted forcast intervals (i.e 14,30,60 and 90 days)
                        for (x in forecast_days) {
                          #  x = 90
                          #summarise leading and lagging variables

                          # Preprocess temp
                          temp2 <- temp %>%
                            mutate(lag_day = x, #forcast interval
                                   Date_predicted = lead(Date,x),
                                   precipitation_lag = rollsum(total_precip, lag_period, fill = NA, align = 'right', na.rm = TRUE), #recharge lagging precipitation
                                   mean_temp_lag = rollmean(mean_temp, lag_period, fill = NA, align = 'right', na.rm = TRUE), # recharge lagging temperature
                                   groundwater_predict = lead(groundwater, x), # actual groundwater level we are predicting
                                   mean_temp_lead = rollmean(mean_temp, x, fill = NA, align = 'left', na.rm = TRUE), #forecasted temperature
                                   precip_lead = rollsum(total_precip, x, fill = NA, align = 'left', na.rm = TRUE) #forecasted precipitation
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
                                     Date_predicted = lead(Date, x)
                              ) %>%
                              mutate(lag_day_adjusted = (x - lag_period)) %>%
                              mutate(weight_max = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period * a_coeff)) %>%
                              mutate(normal_weight = dnorm(i, mean = lag_day_adjusted, sd = lag_period * a_coeff )) %>%
                              mutate(normal_weight = (normal_weight - 0) / (weight_max - 0)) %>%
                              mutate(weighted_lead = lead(total_precip, i) *normal_weight) %>%
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
                            summarise(weighted_lead = mean(weighted_lead, na.rm = TRUE),
                                      .groups = "keep") %>%
                            ungroup()


                          lag_data <- data.frame()

                          lag_period_length = lag_period * 3

                          calculate_weighted_lag <- function(i, lag_period, a_coeff) {

                            temp %>%

                              mutate(lag_day = x, #forcast interval
                                     Date_predicted = lead(Date, x)
                              ) %>%
                              mutate(lag_day_adjusted = lag_period - x) %>%
                              mutate(weight_max = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period * a_coeff)) %>%
                              mutate(normal_weight = dnorm(i, mean = lag_day_adjusted, sd = lag_period * a_coeff)) %>%
                              mutate(normal_weight = (normal_weight - 0) / (weight_max - 0)) %>%
                              mutate(weighted_lag = lag(total_precip, i) * normal_weight) %>%
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
                            summarise(weighted_lag = mean(weighted_lag, na.rm = TRUE),
                                      .groups = "keep") %>%
                            ungroup()

                          lead_data_lag <- full_join(lead_data, lag_data, by = join_by(Date, Date_predicted, Well, lag_day))

                          temp2 <- full_join(temp2, lead_data_lag, by = join_by(Date, Date_predicted, Well, lag_day))

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
                            mutate(precip_lead = (precip_lead - min_precip_lead) / (max_precip_lead - min_precip_lead),
                                   precipitation_lag = (precipitation_lag - min_precipitation_lag) / (max_precipitation_lag - min_precipitation_lag),
                                   mean_temp_lag = (mean_temp_lag - min_mean_temp_lag) / (max_mean_temp_lag - min_mean_temp_lag),
                                   mean_temp_lead = (mean_temp_lead - min_mean_temp_lead) / (max_mean_temp_lead - min_mean_temp_lead),
                                   groundwater = (groundwater - min_groundwater) / (max_groundwater - min_groundwater),
                                   actual_predicted_groundwater = (actual_predicted_groundwater - min_groundwater) / (max_groundwater - min_groundwater),
                                   groundwater_predict =  (groundwater_predict - min_groundwater) / (max_groundwater - min_groundwater),
                                   groundwater_diff =  (groundwater_diff - min_groundwater_diff) / (max_groundwater_diff - min_groundwater_diff)
                            )


                          temp2_original <- temp2





                          # model_file <- paste0(model_path, "ANN_Model_", y, "_", x, ".Rdata")
                          # load(model_file)

                          model_file <- paste0("https://nrs.objectstore.gov.bc.ca/rfc-conditions/groundwater_forecast/models/", "ANN_Model_", y[[1]], "_", x, ".Rdata") #AWS
                          tmp_rdata <- tempfile(fileext = ".RData") # Create unique temp file per task
                          download.file(model_file, destfile = tmp_rdata, mode = "wb") # Safely download the file and load it
                          load(tmp_rdata)


                          # Calculate the mean and standard deviation of the variables by day
                          # need to use raw precip data



                          daily_stats <- temp2_original %>%
                            mutate(
                              mean_temp_lead = mean_temp_lead*(max_mean_temp_lead - min_mean_temp_lead) + min_mean_temp_lead#,
                            ) %>%
                            group_by(days_in_year) %>%
                            summarise(
                              precip_lead_mean = mean(precip_lead_org, na.rm = TRUE),
                              precip_lead_sd = sd(precip_lead_org, na.rm = TRUE),
                              mean_temp_lead_mean = mean(mean_temp_lead, na.rm = TRUE),
                              mean_temp_lead_sd = sd(mean_temp_lead, na.rm = TRUE)
                            )


                          # Generate Monte Carlo values for each day
                          monte_carlo_df <- daily_stats %>%
                            tidyr::uncount(500) %>%
                            mutate(
                              precip_lead = rnorm(n(), mean = precip_lead_mean, sd = precip_lead_sd),
                              mean_temp_lead = rnorm(n(), mean = mean_temp_lead_mean, sd = mean_temp_lead_sd)
                            )

                          monte_carlo_df <- monte_carlo_df %>%
                            dplyr::select(days_in_year, precip_lead, mean_temp_lead) %>%
                            mutate(precip_lead = precip_lead / x, mean_temp_lead = mean_temp_lead)


                          last_date <- temp2_original %>%
                            drop_na(groundwater) %>%
                            summarise(last_measurements = max(Date, na.rm = TRUE)) %>%
                            pull(last_measurements)


                          last_date_days_in_year  <- yday(last_date + x)

                          Prediction_date <- last_date + x
                          monte_carlo_df <- monte_carlo_df %>%
                            filter(days_in_year == last_date_days_in_year) %>%
                            mutate(sim_num = row_number()) %>%
                            mutate(joiner = 1,
                                   FC_type = NA) %>%
                            dplyr::select(sim_num, joiner, precip_lead, mean_temp_lead, FC_type)
                          # Join the new data frame with the original one


                          #forcasted_data

                          temp_forcast <- data.frame()
                          for (p in 1:x) {
                            # p = 1
                            temp_fc <- data.frame(
                              Well = unlist(y),
                              Date = as.Date(last_date + p),
                              Snow_influenced =  unique(temp$Snow_influenced),
                              groundwater_period_average = unique(temp$groundwater_period_average)
                            )

                            temp_fc <- temp_fc %>%
                              mutate(days_in_year = yday(Date))
                            temp_forcast <- rbind(temp_forcast, temp_fc)
                          }

                          temp_forcast4 <- data.frame()
                          #Sys.Date() - ensemble_forecast_data_y_prediction_date <= rfc_forecast_date_window
                          if ((Sys.Date() - ensemble_forecast_data_y_prediction_date <= rfc_forecast_date_window) &&
                              ("ensemble" %in% rfc_forecast_include)) {# ensemble

                            for (g in 1:max_ensemble) {
                              ensemble_forecast_data_z <- ensemble_forecast_data_y %>%
                                filter(en_sim == g)

                              if ((Sys.Date() - deterministic_forecast_data_y_prediction_date <= rfc_forecast_date_window) &&
                                  ("deterministic" %in% rfc_forecast_include)) {# no deterministic but ensemble


                                temp_forcast2 <- full_join(deterministic_forecast_data_y, ensemble_forecast_data_z)
                              } else {

                                temp_forcast2 <- ensemble_forecast_data_z
                              }

                              temp_forcast2_2 <- temp_forcast2 %>%
                                dplyr::select(-en_sim) %>%
                                mutate(days_in_year = yday(Date + x))
                              temp_forcast3 <- full_join(temp_forcast, temp_forcast2_2)


                              temp_forcast3_2  <- temp_forcast3 %>%
                                mutate(joiner = 1) %>%
                                full_join(monte_carlo_df) %>%
                                mutate(total_precip = ifelse(is.na(FC_type), precip_lead, total_precip),
                                       mean_temp = ifelse(is.na(FC_type), mean_temp_lead, mean_temp)) %>%
                                mutate(FC_type = ifelse(is.na(FC_type), 3, FC_type)) %>%
                                group_by(Well, Date) %>%
                                mutate(min_FC = min(FC_type)) %>%
                                filter(min_FC == FC_type) %>%
                                ungroup() %>%
                                mutate(lag_day_adjusted = (lag_period)) %>%
                                group_by(sim_num) %>%
                                arrange(Date) %>%
                                mutate(lead_num = row_number()) %>%
                                ungroup() %>%
                                mutate(weight_max = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period * a_coeff)) %>%
                                mutate(normal_weight = dnorm(lead_num, mean = lag_day_adjusted, sd = lag_period * a_coeff )) %>%
                                mutate(normal_weight = (normal_weight - 0) / (weight_max - 0)) %>%
                                mutate(total_precip = total_precip * normal_weight) %>%
                                filter(Date >= last_date & Date <= Prediction_date) %>%
                                group_by(sim_num, Well) %>%
                                summarise(precip_lead = mean(total_precip, na.rm = TRUE),
                                          mean_temp_lead = mean(mean_temp, na.rm = TRUE)) %>%
                                mutate(en_sim = g) %>%
                                ungroup()

                              temp_forcast4 <- rbind(temp_forcast4, temp_forcast3_2)
                            }
                          } else {

                            # Sys.Date() - deterministic_forecast_data_y_prediction_date <= rfc_forecast_date_window
                            if ((Sys.Date() - deterministic_forecast_data_y_prediction_date <= rfc_forecast_date_window) &&
                                ("deterministic" %in% rfc_forecast_include)) {# deterministic and no ensemble

                              temp_forcast2_2 <- deterministic_forecast_data_y %>%
                                mutate(days_in_year = yday(Date + x))
                              temp_forcast3 <- full_join(temp_forcast, temp_forcast2_2, by = join_by(Well, Date, days_in_year))

                              temp_forcast3_2 <- temp_forcast3 %>%
                                mutate(joiner = 1) %>%
                                full_join(monte_carlo_df, by = join_by(FC_type, joiner)) %>%
                                mutate(total_precip = ifelse(is.na(FC_type), precip_lead, total_precip),
                                       mean_temp = ifelse(is.na(FC_type), mean_temp_lead, mean_temp)) %>%
                                mutate(FC_type = ifelse(is.na(FC_type), 3, FC_type)) %>%
                                group_by(Well, Date) %>%
                                mutate(min_FC = min(FC_type)) %>%
                                filter(min_FC == FC_type) %>%
                                ungroup() %>%
                                mutate(lag_day_adjusted = (lag_period)) %>%
                                group_by(sim_num) %>%
                                arrange(Date) %>%
                                mutate(lead_num = row_number()) %>%
                                ungroup() %>%
                                mutate(weight_max = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period * a_coeff)) %>%
                                mutate(normal_weight = dnorm(lead_num, mean = lag_day_adjusted, sd = lag_period * a_coeff )) %>%
                                mutate(normal_weight = (normal_weight - 0) / (weight_max - 0)) %>%
                                mutate(total_precip = total_precip * normal_weight) %>%
                                filter(Date >= last_date & Date <= Prediction_date) %>%
                                group_by(sim_num, Well) %>%
                                summarise(precip_lead = mean(total_precip, na.rm = TRUE),
                                          mean_temp_lead = mean(mean_temp, na.rm = TRUE),
                                          .groups = "keep") %>%
                                mutate(en_sim = 1) %>%
                                ungroup()

                              temp_forcast4 <- rbind(temp_forcast4, temp_forcast3_2)

                            } else {  # no deterministic and no ensemble

                              temp_forcast3_2 <- temp_forcast %>%
                                mutate(joiner = 1) %>%
                                full_join(monte_carlo_df, by = join_by(joiner)) %>%
                                mutate(total_precip = ifelse(is.na(FC_type), precip_lead, total_precip),
                                       mean_temp = ifelse(is.na(FC_type), mean_temp_lead, mean_temp)) %>%
                                mutate(FC_type = ifelse(is.na(FC_type), 3, FC_type)) %>%
                                group_by(Well, Date) %>%
                                mutate(min_FC = min(FC_type)) %>%
                                filter(min_FC == FC_type) %>%
                                ungroup() %>%
                                mutate(lag_day_adjusted = (lag_period)) %>%
                                group_by(sim_num) %>%
                                arrange(Date) %>%
                                mutate(lead_num = row_number()) %>%
                                ungroup() %>%
                                mutate(weight_max = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period * a_coeff)) %>%
                                mutate(normal_weight = dnorm(lead_num, mean = lag_day_adjusted, sd = lag_period * a_coeff )) %>%
                                mutate(normal_weight = (normal_weight - 0) / (weight_max - 0)) %>%
                                mutate(total_precip = total_precip * normal_weight) %>%
                                filter(Date >= last_date & Date <= Prediction_date) %>%
                                group_by(sim_num, Well) %>%
                                summarise(precip_lead = mean(total_precip, na.rm = TRUE),
                                          mean_temp_lead = mean(mean_temp, na.rm = TRUE),
                                          .groups = "keep") %>%
                                mutate(en_sim = 1) %>%
                                ungroup()

                              temp_forcast4 <- rbind(temp_forcast4, temp_forcast3_2)


                            }
                          }

                          temp_forcast4 <- temp_forcast4 %>%
                            mutate(joiner = 1)


                          temp2_pred <- temp2 %>%
                            filter(Date == last_date) %>%
                            dplyr::select(Date, Well, groundwater, groundwater_diff, precipitation_lag,
                                          mean_temp_lag, max_groundwater, min_groundwater,
                                          max_groundwater_diff, min_groundwater_diff, min_mean_temp_lead,
                                          max_mean_temp_lead, max_precip_lead, min_precip_lead) %>%
                            mutate(joiner = 1) %>%
                            mutate(Date_predicted = Date + x) %>%
                            full_join(temp_forcast4, by = join_by(Well, joiner)) %>%
                            mutate(precip_lead = (precip_lead - min_precip_lead) / (max_precip_lead - min_precip_lead),
                                   mean_temp_lead = (mean_temp_lead - min_mean_temp_lead) / (max_mean_temp_lead - min_mean_temp_lead))


                          # Filter the DataFrame by the last date -- and combine with percentiles
                          temp2_pred2 <- temp2_pred


                          predicted_response_ANN <- predict(fit_ann, newdata = temp2_pred2)
                          temp2_pred2 <- cbind(temp2_pred2, predicted_response_ANN)



                          #Combine data
                          temp2_pred2 <- temp2_pred2 %>%
                            dplyr::rename(
                              ANN = predicted_response_ANN
                            ) %>%
                            gather(c(
                              ANN), key = "Model", value = predicted_value)

                          temp2_pred2 <- temp2_pred2 %>%
                            mutate(predicted_value = predicted_value * (max_groundwater_diff - min_groundwater_diff) + min_groundwater_diff,
                                   groundwater = groundwater * (max_groundwater - min_groundwater) + min_groundwater) %>%
                            mutate(predicted_value = groundwater + predicted_value) %>%
                            mutate(lag_day = x)


                          Time_series_data_2 <- rbind(Time_series_data_2, temp2_pred2)


                        }


                      } else if (unique(pgown_well_info_Well_info$Snow_influenced == 1)) {

                        # SNOW INFLUENCED ----------------------------------------------------------
                        plot_type <- "snow"

                        #run for predicted forcast intervals (i.e 14,30,60 and 90 days)
                        for (x in forecast_days) {


                          #x = 90

                          temp2 <- temp %>%
                            mutate(lag_day = x, #forcast interval
                                   Date_predicted = lead(Date, x),
                                   precipitation_lag = rollsum(total_precip, lag_period, fill = NA, align = 'right', na.rm = TRUE), #recharge lagging precipitation
                                   mean_temp_lag = rollmean(mean_temp, lag_period, fill = NA, align = 'right', na.rm = TRUE), #recharge lagging temperature
                                   groundwater_predict = lead(groundwater, x), #actual groundwater level we are predicting
                                   mean_temp_lead = rollmean(mean_temp, x, fill = NA, align = 'left', na.rm = TRUE), #forecasted precipitation
                                   precip_lead = rollsum(total_precip, x, fill = NA, align = 'left', na.rm = TRUE), #forecasted temperature
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

                            temp %>%
                              mutate(lag_day = x, #forcast interval
                                     Date_predicted = lead(Date, x),
                                     SWE_lag = lag(SWE, 1), #Snow level at rechrage lag period
                                     SWE_lead = lead(SWE, 1)
                              ) %>%
                              mutate(
                                SWE_lag_diff = SWE - SWE_lag, #snowmelt that occured during lag period
                                SWE_lead_diff = SWE_lead - SWE) %>%
                              mutate(lag_day_adjusted = (x - lag_period)) %>%
                              mutate(weight_max = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period * a_coeff)) %>%
                              mutate(normal_weight = dnorm(i, mean = lag_day_adjusted, sd = lag_period * a_coeff)) %>%
                              mutate(normal_weight = (normal_weight - 0) / (weight_max - 0)) %>%
                              mutate(weight_max2 = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period * a_coeff_snow)) %>%
                              mutate(normal_weight2 = dnorm(i, mean = lag_day_adjusted, sd = lag_period * a_coeff_snow)) %>%
                              mutate(normal_weight2 = (normal_weight2 - 0) / (weight_max2 - 0)) %>%
                              mutate(weighted_lead = lead(total_precip, i) * normal_weight,
                                     weighted_lead_temp = lead(mean_temp, i) * normal_weight2,
                                     weighted_lead_SWE = lead(SWE_lead_diff, i) * normal_weight2) %>%
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

                          lead_data <- lead_data %>%
                            group_by(# only select variables of interest remove others
                              Date,
                              Date_predicted,
                              Well,
                              lag_day
                            ) %>%
                            summarise(weighted_lead = mean(weighted_lead, na.rm = TRUE),
                                      weighted_lead_SWE = mean(weighted_lead_SWE, na.rm = TRUE),
                                      weighted_lead_temp = mean(weighted_lead_temp, na.rm = TRUE),
                                      .groups = "keep") %>%
                            ungroup()


                          lag_period_length = lag_period * 3

                          calculate_weighted_lag <- function(i, lag_period, a_coeff) {
                            #i = 5

                            temp %>%
                              mutate(lag_day = x, #forcast interval
                                     Date_predicted = lead(Date, x),
                                     SWE_lag = lag(SWE, 1), #Snow level at rechrage lag period
                                     SWE_lead = lead(SWE, 1)
                              ) %>%
                              mutate(
                                SWE_lag_diff = SWE - SWE_lag, #snowmelt that occured during lag period
                                SWE_lead_diff = SWE_lead - SWE) %>%
                              mutate(lag_day_adjusted = lag_period - x) %>%
                              mutate(weight_max = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period * a_coeff)) %>%
                              mutate(weight_max2 = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period * a_coeff_snow)) %>%
                              mutate(normal_weight = dnorm(i, mean = lag_day_adjusted, sd = lag_period * a_coeff )) %>%
                              mutate(normal_weight2 = dnorm(i, mean = lag_day_adjusted, sd = lag_period * a_coeff_snow )) %>%
                              mutate(normal_weight = (normal_weight - 0) / (weight_max - 0)) %>%
                              mutate(normal_weight2 = (normal_weight2 - 0) / (weight_max2 - 0)) %>%
                              mutate(weighted_lag = lag(total_precip, i) * normal_weight,
                                     weighted_lag_temp = lag(mean_temp, i) * normal_weight2,
                                     weighted_lag_SWE = lag(SWE_lead_diff, i) * normal_weight2) %>%
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

                          lag_data <- lag_data %>%
                            group_by(# only select variables of interest remove others
                              Date,
                              Date_predicted,
                              Well,
                              lag_day
                            ) %>%
                            summarise(weighted_lag = mean(weighted_lag, na.rm = TRUE),
                                      weighted_lag_SWE = mean(weighted_lag_SWE, na.rm = TRUE),
                                      weighted_lag_temp = mean(weighted_lag_temp, na.rm = TRUE),
                                      .groups = "keep"
                            ) %>%
                            ungroup()

                          lead_data_lag <- full_join(lead_data, lag_data, by = join_by(Date, Date_predicted, Well, lag_day))

                          temp2 <- full_join(temp2, lead_data_lag, by = join_by(Date, Date_predicted, Well, lag_day))

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
                            mutate(precip_lead = (precip_lead - min_precip_lead) / (max_precip_lead - min_precip_lead),
                                   precipitation_lag = (precipitation_lag - min_precipitation_lag) / (max_precipitation_lag - min_precipitation_lag),
                                   SWE_lead_diff = (SWE_lead_diff - min_SWE_lead_diff) / (max_SWE_lead_diff - min_SWE_lead_diff),
                                   SWE_lag_diff = (SWE_lag_diff - min_SWE_lag_diff) / (max_SWE_lag_diff - min_SWE_lag_diff),
                                   mean_temp_lag = (mean_temp_lag - min_mean_temp_lag) / (max_mean_temp_lag - min_mean_temp_lag),
                                   mean_temp_lead = (mean_temp_lead - min_mean_temp_lead) / (max_mean_temp_lead - min_mean_temp_lead),
                                   groundwater = (groundwater - min_groundwater) / (max_groundwater - min_groundwater),
                                   actual_predicted_groundwater = (actual_predicted_groundwater - min_groundwater) / (max_groundwater - min_groundwater),
                                   groundwater_predict =  (groundwater_predict - min_groundwater) / (max_groundwater - min_groundwater),
                                   groundwater_diff =  (groundwater_diff - min_groundwater_diff) / (max_groundwater_diff - min_groundwater_diff))

                          temp2_original <- temp2


                          # model_file <- paste0(model_path, "ANN_Model_", y, "_", x, ".Rdata")
                          # load(model_file)

                          model_file <- paste0("https://nrs.objectstore.gov.bc.ca/rfc-conditions/groundwater_forecast/models/", "ANN_Model_", y[[1]], "_", x, ".Rdata") #AWS
                          tmp_rdata <- tempfile(fileext = ".RData") # Create unique temp file per task
                          download.file(model_file, destfile = tmp_rdata, mode = "wb") # Safely download the file and load it
                          load(tmp_rdata)


                          # Calculate the mean and standard deviation of the variables by day

                          daily_stats <- temp2_original %>%
                            mutate(
                              mean_temp_lead = mean_temp_lead * (max_mean_temp_lead - min_mean_temp_lead) + min_mean_temp_lead#,
                            ) %>%
                            group_by(days_in_year) %>%
                            summarise(
                              precip_lead_mean = mean(precip_lead_org, na.rm = TRUE),
                              precip_lead_sd = sd(precip_lead_org, na.rm = TRUE),
                              mean_temp_lead_mean = mean(mean_temp_lead, na.rm = TRUE),
                              mean_temp_lead_sd = sd(mean_temp_lead, na.rm = TRUE),
                              SWE_lead_diff_mean = mean(SWE_lead_diff_org, na.rm = TRUE),
                              SWE_lead_diff_sd = sd(SWE_lead_diff_org, na.rm = TRUE)
                            )


                          # Generate Monte Carlo values for each day
                          monte_carlo_df <- daily_stats %>%
                            tidyr::uncount(500) %>%
                            mutate(
                              precip_lead = rnorm(n(), mean = precip_lead_mean, sd = precip_lead_sd),
                              mean_temp_lead = rnorm(n(), mean = mean_temp_lead_mean, sd = mean_temp_lead_sd),
                              SWE_lead_diff = rnorm(n(), mean = SWE_lead_diff_mean, sd = SWE_lead_diff_sd)
                            )

                          monte_carlo_df <- monte_carlo_df %>%
                            dplyr::select(days_in_year, precip_lead, mean_temp_lead, SWE_lead_diff) %>%
                            mutate(precip_lead = precip_lead / x,
                                   mean_temp_lead = mean_temp_lead,
                                   SWE_lead_diff = SWE_lead_diff / x)


                          last_date <- temp2_original%>%
                            drop_na(groundwater) %>%
                            summarise(last_measurements = max(Date, na.rm = TRUE)) %>%
                            pull(last_measurements)


                          last_date_days_in_year <- yday(last_date + x)

                          Prediction_date <- last_date + x

                          monte_carlo_df <- monte_carlo_df %>%
                            filter(days_in_year == last_date_days_in_year) %>%
                            mutate(sim_num = row_number()) %>%
                            mutate(joiner = 1,
                                   FC_type = NA) %>%
                            dplyr::select(sim_num, joiner, precip_lead, mean_temp_lead, SWE_lead_diff, FC_type)
                          # Join the new data frame with the original one


                          temp_forcast <- data.frame()

                          for (p in 1:x) {

                            temp_fc <- data.frame(
                              Well = y,
                              Date = as.Date(last_date+p),
                              Snow_influenced =  unique(temp$Snow_influenced),
                              groundwater_period_average = unique(temp$groundwater_period_average)
                            )

                            temp_fc <- temp_fc %>%
                              mutate(days_in_year = yday(Date + x))

                            temp_forcast <- rbind(temp_forcast, temp_fc)

                          }

                          temp_forcast4 <- data.frame()
                          if ((Sys.Date() - ensemble_forecast_data_y_prediction_date <= rfc_forecast_date_window) &&
                              ("ensemble" %in% rfc_forecast_include)) {# ensemble

                            for (g in 1:max_ensemble) {
                              # g <- 1
                              ensemble_forecast_data_z <- ensemble_forecast_data_y %>%
                                filter(en_sim == g)

                              if ((Sys.Date() - deterministic_forecast_data_y_prediction_date <= rfc_forecast_date_window) &&
                                  ("deterministic" %in% rfc_forecast_include)) {# no deterministic but ensemble

                                temp_forcast2 <- full_join(deterministic_forecast_data_y, ensemble_forecast_data_z,
                                                           by = join_by(Well, Date, total_precip, mean_temp, FC_type))
                              } else {
                                temp_forcast2 <- ensemble_forecast_data_z
                              }

                              temp_forcast2_2 <- temp_forcast2 %>%
                                dplyr::select(-en_sim) %>%
                                mutate(days_in_year = yday(Date + x))

                              temp_forcast3 <- full_join(temp_forcast, temp_forcast2_2, by = join_by(Well, Date, days_in_year))

                              temp_forcast3_2  <- temp_forcast3 %>%
                                mutate(joiner = 1) %>%
                                full_join(monte_carlo_df, by = join_by(FC_type, joiner)) %>%
                                mutate(total_precip = ifelse(is.na(FC_type), precip_lead, total_precip),
                                       mean_temp = ifelse(is.na(FC_type), mean_temp_lead, mean_temp)) %>%
                                mutate(FC_type = ifelse(is.na(FC_type), 3, FC_type)) %>%
                                group_by(Well, Date) %>%
                                mutate(min_FC = min(FC_type)) %>%
                                filter(min_FC == FC_type) %>%
                                ungroup() %>%
                                mutate(lag_day_adjusted = (lag_period)) %>%
                                group_by(sim_num) %>%
                                arrange(Date) %>%
                                mutate(lead_num = row_number()) %>%
                                ungroup() %>%
                                mutate(lag_day_adjusted = (x - lag_period)) %>%
                                mutate(weight_max = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period * a_coeff)) %>%
                                mutate(normal_weight = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period * a_coeff )) %>%
                                mutate(normal_weight = (normal_weight - 0) / (weight_max - 0)) %>%
                                mutate(weight_max2 = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period * a_coeff_snow)) %>%
                                mutate(normal_weight2 = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period * a_coeff_snow )) %>%
                                mutate(normal_weight2 = (normal_weight2 - 0) / (weight_max2 - 0)) %>%
                                mutate(total_precip = total_precip * normal_weight) %>%
                                mutate(mean_temp = mean_temp * normal_weight2) %>%
                                mutate(SWE_lead_diff = SWE_lead_diff * normal_weight2) %>%
                                filter(Date >= last_date & Date <= Prediction_date) %>%
                                group_by(sim_num, Well) %>%
                                summarise(precip_lead = mean(total_precip, na.rm = TRUE),
                                          mean_temp_lead = mean(mean_temp, na.rm = TRUE),
                                          SWE_lead_diff = mean(SWE_lead_diff, na.rm = TRUE)) %>%
                                mutate(en_sim = g) %>%
                                ungroup()

                              temp_forcast4 <- rbind(temp_forcast4, temp_forcast3_2)
                            }
                          } else {
                            if ((Sys.Date() - deterministic_forecast_data_y_prediction_date <= rfc_forecast_date_window) &&
                                ("deterministic" %in% rfc_forecast_include)) {# deterministic and no ensemble

                              temp_forcast2_2 <- deterministic_forecast_data_y %>%
                                mutate(days_in_year = yday(Date + x))
                              temp_forcast3 <- full_join(temp_forcast, temp_forcast2_2, by = join_by(Well, Date, days_in_year))

                              temp_forcast3_2 <- temp_forcast3 %>%
                                mutate(joiner = 1) %>%
                                full_join(monte_carlo_df, by = join_by(FC_type, joiner)) %>%
                                mutate(total_precip = ifelse(is.na(FC_type), precip_lead, total_precip),
                                       mean_temp = ifelse(is.na(FC_type), mean_temp_lead, mean_temp)) %>%
                                mutate(FC_type = ifelse(is.na(FC_type), 3, FC_type)) %>%
                                group_by(Well, Date) %>%
                                mutate(min_FC = min(FC_type)) %>%
                                filter(min_FC == FC_type) %>%
                                ungroup() %>%
                                mutate(lag_day_adjusted = (lag_period)) %>%
                                group_by(sim_num) %>%
                                arrange(Date) %>%
                                mutate(lead_num = row_number()) %>%
                                ungroup() %>%
                                mutate(lag_day_adjusted = (x - lag_period)) %>%
                                mutate(weight_max = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period * a_coeff)) %>%
                                mutate(normal_weight = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period * a_coeff )) %>%
                                mutate(normal_weight = (normal_weight - 0) / (weight_max - 0)) %>%
                                mutate(weight_max2 = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period * a_coeff_snow)) %>%
                                mutate(normal_weight2 = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period * a_coeff_snow )) %>%
                                mutate(normal_weight2 = (normal_weight2 - 0) / (weight_max2 - 0)) %>%
                                mutate(total_precip = total_precip * normal_weight) %>%
                                mutate(mean_temp = mean_temp * normal_weight2) %>%
                                mutate(SWE_lead_diff = SWE_lead_diff * normal_weight2) %>%
                                filter(Date >= last_date & Date <= Prediction_date) %>%
                                group_by(sim_num, Well) %>%
                                summarise(precip_lead = mean(total_precip, na.rm = TRUE),
                                          mean_temp_lead = mean(mean_temp, na.rm = TRUE),
                                          SWE_lead_diff = mean(SWE_lead_diff, na.rm = TRUE),
                                          .groups = "keep") %>%
                                mutate(en_sim = 1) %>%
                                ungroup()

                              temp_forcast4 <- rbind(temp_forcast4, temp_forcast3_2)

                            } else {

                              temp_forcast3_2  <- temp_forcast %>%
                                mutate(joiner = 1) %>%
                                full_join(monte_carlo_df, by = join_by(joiner)) %>%
                                mutate(total_precip = ifelse(is.na(FC_type), precip_lead, total_precip),
                                       mean_temp = ifelse(is.na(FC_type), mean_temp_lead, mean_temp)) %>%
                                mutate(FC_type = ifelse(is.na(FC_type), 3, FC_type)) %>%
                                group_by(Well, Date) %>%
                                mutate(min_FC = min(FC_type)) %>%
                                filter(min_FC == FC_type) %>%
                                ungroup() %>%
                                mutate(lag_day_adjusted = (lag_period)) %>%
                                group_by(sim_num) %>%
                                arrange(Date) %>%
                                mutate(lead_num = row_number()) %>%
                                ungroup() %>%
                                mutate(lag_day_adjusted = (x - lag_period)) %>%
                                mutate(weight_max = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period * a_coeff)) %>%
                                mutate(normal_weight = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period * a_coeff )) %>%
                                mutate(normal_weight = (normal_weight - 0) / (weight_max - 0)) %>%
                                mutate(weight_max2 = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period * a_coeff_snow)) %>%
                                mutate(normal_weight2 = dnorm(lag_day_adjusted, mean = lag_day_adjusted, sd = lag_period * a_coeff_snow )) %>%
                                mutate(normal_weight2 = (normal_weight2 - 0) / (weight_max2 - 0)) %>%
                                mutate(total_precip = total_precip * normal_weight) %>%
                                mutate(mean_temp = mean_temp * normal_weight2) %>%
                                mutate(SWE_lead_diff = SWE_lead_diff * normal_weight2) %>%
                                filter(Date >= last_date & Date <= Prediction_date) %>%
                                group_by(sim_num, Well) %>%
                                summarise(precip_lead = mean(total_precip, na.rm = TRUE),
                                          mean_temp_lead = mean(mean_temp, na.rm = TRUE),
                                          SWE_lead_diff = mean(SWE_lead_diff, na.rm = TRUE),
                                          .groups = "keep") %>%
                                mutate(en_sim = 1) %>%
                                ungroup()
                              temp_forcast4 <- rbind(temp_forcast4, temp_forcast3_2)
                            }
                          }

                          temp_forcast4 <- temp_forcast4 %>%
                            mutate(joiner = 1)

                          temp2_pred <- temp2 %>%
                            filter(Date == last_date) %>%
                            dplyr::select(Date, Well,groundwater,groundwater_diff, precipitation_lag, mean_temp_lag,
                                          max_groundwater, min_groundwater, max_groundwater_diff, min_groundwater_diff,
                                          min_mean_temp_lead, max_mean_temp_lead, max_precip_lead, min_precip_lead,
                                          SWE, SWE_lag_diff, max_SWE_lead_diff, min_SWE_lead_diff) %>%
                            mutate(joiner = 1) %>%
                            mutate(Date_predicted = Date+x) %>%
                            full_join(temp_forcast4, by = join_by(Well, joiner)) %>%
                            mutate(SWE_lead_diff = ifelse(SWE_lead_diff < 0,ifelse(SWE_lead_diff <= -SWE, -SWE, SWE_lead_diff), SWE_lead_diff)) %>%
                            mutate(precip_lead = (precip_lead - min_precip_lead) / (max_precip_lead - min_precip_lead),
                                   mean_temp_lead = (mean_temp_lead - min_mean_temp_lead) / (max_mean_temp_lead - min_mean_temp_lead),
                                   SWE_lead_diff = (SWE_lead_diff - min_SWE_lead_diff) / (max_SWE_lead_diff - min_SWE_lead_diff)
                            )

                          temp2_test <- temp2 %>%
                            filter(days_in_year == last_date_days_in_year )

                          predicted_response_ANN <- predict(fit_ann, newdata = temp2_pred)
                          temp2_pred <- cbind(temp2_pred, predicted_response_ANN)

                          #Combine data
                          temp2_pred <- temp2_pred %>%
                            dplyr::rename(ANN = predicted_response_ANN
                            ) %>%
                            gather(c(
                              ANN), key = "Model", value = predicted_value)

                          temp2_pred <- temp2_pred %>%
                            mutate(predicted_value = predicted_value * (max_groundwater_diff - min_groundwater_diff) + min_groundwater_diff,
                                   groundwater = groundwater * (max_groundwater - min_groundwater) + min_groundwater) %>%
                            mutate(predicted_value = groundwater + predicted_value) %>%
                            mutate(lag_day = x)

                          Time_series_data_2 <- rbind(Time_series_data_2, temp2_pred)

                        }

                      }

                      # PLOTTING -----------------------------------------------------------------

                      font_add("BCSans", normalizePath("docs/fonts/BCSans-Regular.ttf"))
                      # showtext_auto()

                      #gather model predictions for plot

                      Time_series_data_2_plot <- Time_series_data_2 %>%
                        group_by(Well, Date, Date_predicted, lag_day,Model, groundwater) %>%
                        summarise(predicted_value_mean = mean(predicted_value, na.rm = TRUE),
                                  predicted_value_min = min(predicted_value, na.rm = TRUE),
                                  predicted_value_max = max(predicted_value, na.rm = TRUE),
                                  predicted_value_25th =  quantile(predicted_value, 0.25, na.rm = TRUE),
                                  predicted_value_75th = quantile(predicted_value, 0.75, na.rm = TRUE),
                                  predicted_value_50th = quantile(predicted_value, 0.50, na.rm = TRUE),
                                  predicted_value_90th = quantile(predicted_value, 0.90, na.rm = TRUE),
                                  predicted_value_10th = quantile(predicted_value, 0.10, na.rm = TRUE),
                                  predicted_value_5th = quantile(predicted_value, 0.05, na.rm = TRUE),
                                  predicted_value_95th = quantile(predicted_value, 0.95, na.rm = TRUE),
                                  .groups = "keep"
                        ) %>%
                        ungroup()


                      #Entire data set to plot actual levels
                      temp <- temp %>%
                        mutate(days_in_year = yday(Date))

                      start_day = Sys.Date() - 365 + max(forecast_days) + 7
                      end_day = Sys.Date() + max(forecast_days) + 7

                      start_year = year(start_day)
                      end_year = year(end_day)

                      # filter historical data percentiles to just past forecast date

                      if (start_year == end_year) {

                        temp_WL_states_temp_plot <- temp_WL_states_temp %>%
                          mutate(month = month(fake_date), day = day(fake_date)) %>%
                          mutate(Date = make_date(year = start_year, month = month, day = day))

                      } else {

                        temp_WL_states_temp1 <- temp_WL_states_temp %>%
                          mutate(month = month(fake_date), day = day(fake_date)) %>%
                          mutate(Date = make_date(year = start_year, month = month, day = day))

                        temp_WL_states_temp2 <- temp_WL_states_temp %>%
                          mutate(month = month(fake_date), day = day(fake_date)) %>%
                          mutate(Date = make_date(year = end_year, month = month, day = day))

                        temp_WL_states_temp_plot <- rbind(temp_WL_states_temp1, temp_WL_states_temp2)

                      }

                      temp_WL_states_temp_plot <- temp_WL_states_temp_plot %>%
                        drop_na(Date)


                      snow_influenced  <- if (unique(pgown_well_info_Well_info$Snow_influenced == 0)) {


                        #climate graphs.
                        temp_historical_conditions <- temp %>%
                          mutate(precipitation_lag = rollsum(total_precip, 30, fill = NA, align = 'right', na.rm = TRUE), #recharge lagging precipitation
                                 mean_temp_lag = rollmean(mean_temp, 30, fill = NA, align = 'right', na.rm = TRUE)
                          ) %>%
                          mutate(days_in_year = yday(Date)) %>%
                          dplyr::select(Date, days_in_year, precipitation_lag, mean_temp_lag) %>%
                          gather(c(precipitation_lag, mean_temp_lag), key = "variable", value = "Value")

                        #histoircal stats



                        deterministic_forecast_data_temp <- full_join(temp, deterministic_forecast_data_y, by = join_by(Well, Date, mean_temp, total_precip))


                        deterministic_forecast_data_temp <- deterministic_forecast_data_temp %>%
                          mutate(precipitation_lag = rollsum(total_precip, 30, fill = NA, align = 'right', na.rm = TRUE), #recharge lagging precipitation
                                 mean_temp_lag = rollmean(mean_temp, 30, fill = NA, align = 'right', na.rm = TRUE)
                          ) %>%
                          mutate(days_in_year = yday(Date)) %>%
                          dplyr::select(Date, days_in_year, precipitation_lag, mean_temp_lag, FC_type) %>%
                          gather(c(precipitation_lag, mean_temp_lag), key = "variable", value = "Value") %>%
                          drop_na(FC_type) %>%
                          mutate(variable = ifelse(variable == "mean_temp_lag", "Temp (30 day)",
                                                   ifelse(variable == "precipitation_lag", "Precip (30 day)", NA)))

                        deterministic_forecast_data_temp_max <- max(deterministic_forecast_data_temp$Date)

                        ensemble_forecast_data_y_temp <- data.frame()



                        for (g in 1:max_ensemble) {
                          #g = 1

                          ensemble_forecast_data_z <- ensemble_forecast_data_y %>%
                            filter(en_sim == g)

                          ensemble_forecast_data_z<- full_join(temp, ensemble_forecast_data_z, by = join_by(Well, Date, mean_temp, total_precip))


                          ensemble_forecast_data_z <- ensemble_forecast_data_z %>%
                            mutate(precipitation_lag = rollsum(total_precip, 30, fill = NA, align = 'right', na.rm = TRUE), #recharge lagging precipitation
                                   mean_temp_lag = rollmean(mean_temp, 30, fill = NA, align = 'right', na.rm = TRUE)
                            ) %>%
                            mutate(days_in_year = yday(Date)) %>%
                            dplyr::select(Date, days_in_year, precipitation_lag, mean_temp_lag, FC_type, en_sim) %>%
                            gather(c(precipitation_lag, mean_temp_lag), key = "variable", value = "Value") %>%
                            drop_na(FC_type) %>%
                            mutate(variable = ifelse(variable == "mean_temp_lag", "Temp (30 day)",
                                                     ifelse(variable == "precipitation_lag", "Precip (30 day)", NA)))



                          ensemble_forecast_data_y_temp <- rbind(ensemble_forecast_data_y_temp, ensemble_forecast_data_z)
                        }


                        ensemble_forecast_data_y_temp <- ensemble_forecast_data_y_temp %>%
                          filter(Date >= deterministic_forecast_data_temp_max )



                        temp_historical_conditions_stats <- temp_historical_conditions %>%
                          mutate(month = month(Date), year = year(Date), day = day(Date)) %>%
                          # filter(year < max(year)) %>% # remove current year from plot
                          filter(Date < end_day - years(1)) %>% # remove current year from plot
                          group_by(month, day, variable) %>%
                          summarise(predicted_value_mean = mean(Value, na.rm = TRUE),
                                    predicted_value_min = min(Value, na.rm = TRUE),
                                    predicted_value_max = max(Value, na.rm = TRUE),
                                    predicted_value_25th =  quantile(Value, 0.25, na.rm = TRUE),
                                    predicted_value_75th = quantile(Value, 0.75, na.rm = TRUE),
                                    predicted_value_50th = quantile(Value, 0.50, na.rm = TRUE),
                                    predicted_value_90th = quantile(Value, 0.90, na.rm = TRUE),
                                    predicted_value_10th = quantile(Value, 0.10, na.rm = TRUE),
                                    predicted_value_5th = quantile(Value, 0.05, na.rm = TRUE),
                                    predicted_value_95th = quantile(Value, 0.95, na.rm = TRUE),
                                    .groups = "keep"
                          ) %>%
                          ungroup()


                        if (start_year == end_year) {

                          temp_historical_conditions_stats_plot <- temp_historical_conditions_stats %>%
                            mutate(Date = make_date(year = start_year, month = month, day = day))

                        } else {

                          temp_historical_conditions_stats_temp1 <- temp_historical_conditions_stats %>%
                            mutate(Date = make_date(year = start_year, month = month, day = day))

                          temp_historical_conditions_stats_temp2 <- temp_historical_conditions_stats %>%
                            mutate(Date = make_date(year = end_year, month = month, day = day))

                          temp_historical_conditions_stats_plot <- rbind(temp_historical_conditions_stats_temp1,
                                                                         temp_historical_conditions_stats_temp2)

                        }

                        temp_historical_conditions_stats_plot <- temp_historical_conditions_stats_plot %>%
                          drop_na(Date) %>%
                          mutate(variable = ifelse(variable == "mean_temp_lag", "Temp (30 day)",
                                                   ifelse(variable == "precipitation_lag", "Precip (30 day)", NA)))



                        temp_historical_conditions <- temp_historical_conditions %>%
                          mutate(variable = ifelse(variable == "mean_temp_lag", "Temp (30 day)",
                                                   ifelse(variable == "precipitation_lag", "Precip (30 day)", NA)))

                        recharge_lag <- pgown_well_info_Well_info$Lag_time


                        dummy_data2 <- data.frame(
                          peak_recharge = as.Date(c(Sys.Date() + 14 - recharge_lag , Sys.Date() + 30 - recharge_lag, Sys.Date() + 60 - recharge_lag, Sys.Date() + 90 - recharge_lag)),
                          prediction_period = c("14 Days", "30 Days", "60 Days", "90 Days"),
                          lower_range = as.Date(c(Sys.Date() + 14 - recharge_lag - recharge_lag,
                                                  Sys.Date() + 30 - recharge_lag - recharge_lag,
                                                  Sys.Date() + 60 - recharge_lag - recharge_lag,
                                                  Sys.Date() + 90 - recharge_lag - recharge_lag)),
                          upper_range = as.Date(c(Sys.Date() + 14 - recharge_lag + recharge_lag,
                                                  Sys.Date() + 30 - recharge_lag + recharge_lag,
                                                  Sys.Date() + 60 - recharge_lag + recharge_lag,
                                                  Sys.Date() + 90 - recharge_lag + recharge_lag)),
                          variable = c("Precip (30 day)", "Precip (30 day)", "Precip (30 day)", "Precip (30 day)"),
                          location = c(0.8, 0.87, 0.95, 1.03))


                        gglayers <- list(
                          #scale_y_continuous(sec.axis = sec_axis( trans=~.-temp2_toc, name="Depth below ground surface (m)")),
                          theme_bw(),
                          ylab("  Degrees (°C) Precipitation (mm)"),
                          xlab(""),
                          scale_x_date(date_labels = ("%b"), date_breaks = "1 month", limits = c(start_day, end_day), expand = c(0,0)),
                          theme(#legend.title = element_blank(),
                            legend.position = "right",
                            legend.background = element_blank(),
                            axis.text.x = element_text(angle = 30, hjust =1),
                            legend.spacing.y = unit(0.1, 'mm'),
                            legend.margin = ggplot2::margin(t = 0.25, b = 0.25)))

                        Precip_historical <- temp_historical_conditions_stats_plot %>%
                          filter(variable == "Precip (30 day)" )
                        Range = max(Precip_historical$predicted_value_max)




                        temp_graph2 <- ggplot() +
                          geom_ribbon(data = temp_historical_conditions_stats_plot, alpha = 0.5, aes(x = Date, ymax = predicted_value_5th, ymin = predicted_value_min, fill = "1 Min - Q5"), size = 1) +
                          geom_ribbon(data = temp_historical_conditions_stats_plot, alpha = 0.5, aes(x = Date, ymax = predicted_value_10th, ymin = predicted_value_5th, fill = "2 Q5 - Q10"), size = 1) +
                          geom_ribbon(data = temp_historical_conditions_stats_plot, alpha = 0.5, aes(x = Date, ymax = predicted_value_25th, ymin = predicted_value_10th, fill = "3 Q10 - Q25"), size = 1) +
                          geom_ribbon(data = temp_historical_conditions_stats_plot, alpha = 0.5, aes(x = Date, ymax = predicted_value_75th, ymin = predicted_value_25th, fill = "4 Q25 - Q75"), size = 1) +
                          geom_ribbon(data = temp_historical_conditions_stats_plot, alpha = 0.5, aes(x = Date, ymax = predicted_value_90th, ymin = predicted_value_75th, fill = "5 Q75 - Q90"), size = 1) +
                          geom_ribbon(data = temp_historical_conditions_stats_plot, alpha = 0.5, aes(x = Date, ymax = predicted_value_95th, ymin = predicted_value_90th, fill = "6 Q90 - Q95"), size = 1) +
                          geom_ribbon(data = temp_historical_conditions_stats_plot, alpha = 0.5, aes(x = Date, ymax = predicted_value_max, ymin = predicted_value_95th, fill = "7 Q95 - Max"), size = 1) +
                          scale_fill_brewer(name = "Historical Data (2004-2023)", palette = "Spectral", direction = 1) +
                          scale_alpha_manual(name = "Historical Data (2004-2023)", values = c(1, 1, 1, 1, 1, 1, 1, 1)) +
                          gglayers +
                          new_scale_colour() +
                          new_scale_fill() +
                          geom_line(data = temp_historical_conditions, aes(x = Date, y = Value, colour = "1"), linewidth = 0.8) +
                          { if ("deterministic" %in% rfc_forecast_include) geom_line(data = deterministic_forecast_data_temp, aes(x = Date, y = Value, colour = "2"), linetype = 1, linewidth = 0.8) } +
                          # geom_line(data = deterministic_forecast_data_temp, aes(x = Date, y = Value, colour = "2"), linetype = 1, linewidth = 0.8) +
                          geom_line(data = ensemble_forecast_data_y_temp, aes(x = Date, y = Value, colour = "3", group = en_sim), linetype = 1, linewidth = 0.8) +
                          scale_colour_manual(name = "", values = c("black", "red", "orange")) +
                          geom_segment(data = dummy_data2,
                                       aes(x = lower_range,
                                           xend = upper_range,
                                           y = Range * location,
                                           yend = Range * location),
                                       size = 1, alpha = 0.5,
                                       color = "darkblue")+
                          geom_point(data = dummy_data2,
                                     aes(x = peak_recharge,
                                         y = Range * location),
                                     shape = 21, size = 1, alpha = 0.5, fill = "darkblue") +

                          geom_text(data = dummy_data2,
                                    aes(x = lower_range,
                                        y = Range * location,
                                        label = prediction_period),
                                    color = "darkblue",
                                    size = 2,
                                    vjust = 0.25,
                                    hjust = 1)+
                          theme(legend.position = "none") +  # Remove legend
                          facet_grid(variable ~ ., scale = "free_y")
                        temp_graph2





                      } else if (unique(pgown_well_info_Well_info$Snow_influenced == 1)) {
                        # SNOW INFLUENCED ----------------------------------------------------------

                        #climate graphs.
                        temp_historical_conditions <- temp %>%
                          mutate(precipitation_lag = rollsum(total_precip, 30, fill = NA, align = 'right', na.rm = TRUE), #recharge lagging precipitation
                                 mean_temp_lag = rollmean(mean_temp, 30, fill = NA, align = 'right', na.rm = TRUE)
                          ) %>%
                          mutate(days_in_year = yday(Date)) %>%
                          dplyr::select(Date,days_in_year, precipitation_lag, mean_temp_lag, SWE) %>%
                          gather(c(precipitation_lag, mean_temp_lag, SWE), key = "variable", value = "Value")

                        #histoircal stats


                        deterministic_forecast_data_temp <- full_join(temp, deterministic_forecast_data_y,
                                                                      by = join_by(Well, Date, mean_temp, total_precip))


                        deterministic_forecast_data_temp <- deterministic_forecast_data_temp %>%
                          mutate(precipitation_lag = rollsum(total_precip, 30, fill = NA, align = 'right', na.rm = TRUE), #recharge lagging precipitation
                                 mean_temp_lag = rollmean(mean_temp, 30, fill = NA, align = 'right', na.rm = TRUE)
                          ) %>%
                          mutate(days_in_year = yday(Date)) %>%
                          dplyr::select(Date, days_in_year, precipitation_lag, mean_temp_lag, SWE, FC_type) %>%
                          gather(c(precipitation_lag, mean_temp_lag, SWE), key = "variable", value = "Value") %>%
                          drop_na(FC_type) %>%
                          mutate(variable = ifelse(variable == "mean_temp_lag", "Temp (30 day)",
                                                   ifelse(variable == "precipitation_lag", "Precip (30 day)",
                                                          ifelse(variable == "SWE", "SWE", NA))))
                        deterministic_forecast_data_temp_max <- max(deterministic_forecast_data_temp$Date)

                        ensemble_forecast_data_y_temp <- data.frame()



                        for (g in 1:max_ensemble) {
                          # g = 2


                          ensemble_forecast_data_z <- ensemble_forecast_data_y %>%
                            filter(en_sim == g)

                          ensemble_forecast_data_z <- full_join(temp, ensemble_forecast_data_z,
                                                                by = join_by(Well, Date, mean_temp, total_precip))


                          ensemble_forecast_data_z <- ensemble_forecast_data_z %>%
                            mutate(precipitation_lag = rollsum(total_precip, 30, fill = NA, align = 'right', na.rm = TRUE), #recharge lagging precipitation
                                   mean_temp_lag = rollmean(mean_temp, 30, fill = NA, align = 'right', na.rm = TRUE)
                            ) %>%
                            mutate(days_in_year = yday(Date)) %>%
                            dplyr::select(Date, days_in_year, precipitation_lag, mean_temp_lag, SWE, FC_type, en_sim) %>%
                            gather(c(precipitation_lag, mean_temp_lag, SWE), key = "variable", value = "Value") %>%
                            drop_na(FC_type) %>%
                            mutate(variable = ifelse(variable == "mean_temp_lag", "Temp (30 day)",
                                                     ifelse(variable == "precipitation_lag", "Precip (30 day)",
                                                            ifelse(variable == "SWE", "SWE", NA))))



                          ensemble_forecast_data_y_temp <- rbind(ensemble_forecast_data_y_temp, ensemble_forecast_data_z)
                        }


                        ensemble_forecast_data_y_temp <- ensemble_forecast_data_y_temp %>%
                          filter(Date >= deterministic_forecast_data_temp_max)


                        temp_historical_conditions_stats <- temp_historical_conditions %>%
                          mutate(month = month(Date), year = year(Date), day = day(Date)) %>%
                          filter(Date < end_day - years(1)) %>% # remove current year from plot
                          group_by(month, day, variable) %>%
                          summarise(predicted_value_mean = mean(Value, na.rm = TRUE),
                                    predicted_value_min = min(Value, na.rm = TRUE),
                                    predicted_value_max = max(Value, na.rm = TRUE),
                                    predicted_value_25th =  quantile(Value, 0.25, na.rm = TRUE),
                                    predicted_value_75th = quantile(Value, 0.75, na.rm = TRUE),
                                    predicted_value_50th = quantile(Value, 0.50, na.rm = TRUE),
                                    predicted_value_90th = quantile(Value, 0.90, na.rm = TRUE),
                                    predicted_value_10th = quantile(Value, 0.10, na.rm = TRUE),
                                    predicted_value_5th = quantile(Value, 0.05, na.rm = TRUE),
                                    predicted_value_95th = quantile(Value, 0.95, na.rm = TRUE),
                                    .groups = "keep"
                          ) %>%
                          ungroup()


                        if (start_year == end_year) {

                          temp_historical_conditions_stats_plot <- temp_historical_conditions_stats %>%
                            mutate(Date = make_date(year = start_year, month = month, day = day))

                        } else {

                          temp_historical_conditions_stats_temp1 <- temp_historical_conditions_stats %>%
                            mutate(Date = make_date(year = start_year, month = month, day = day))

                          temp_historical_conditions_stats_temp2 <- temp_historical_conditions_stats %>%
                            mutate(Date = make_date(year = end_year, month = month, day = day))

                          temp_historical_conditions_stats_plot <- rbind(temp_historical_conditions_stats_temp1,
                                                                         temp_historical_conditions_stats_temp2)

                        }

                        temp_historical_conditions_stats_plot <- temp_historical_conditions_stats_plot %>%
                          drop_na(Date) %>%
                          mutate(variable = ifelse(variable == "mean_temp_lag", "Temp (30 day)",
                                                   ifelse(variable == "precipitation_lag", "Precip (30 day)",
                                                          ifelse(variable == "SWE", "SWE", NA))))



                        temp_historical_conditions <- temp_historical_conditions %>%
                          mutate(variable = ifelse(variable == "mean_temp_lag", "Temp (30 day)",
                                                   ifelse(variable == "precipitation_lag", "Precip (30 day)",
                                                          ifelse(variable == "SWE", "SWE", NA))))




                        # model error
                        recharge_lag <- pgown_well_info_Well_info$Lag_time



                        dummy_data2 <- data.frame(
                          peak_recharge = as.Date(c(Sys.Date() + 14 - recharge_lag , Sys.Date() + 30 - recharge_lag, Sys.Date() + 60 - recharge_lag, Sys.Date() + 90 - recharge_lag)),
                          prediction_period = c("14 Days", "30 Days", "60 Days", "90 Days"),
                          lower_range = as.Date(c(Sys.Date() + 14 - recharge_lag - recharge_lag,
                                                  Sys.Date() + 30 - recharge_lag - recharge_lag,
                                                  Sys.Date() + 60 - recharge_lag - recharge_lag,
                                                  Sys.Date() + 90 - recharge_lag - recharge_lag)),
                          upper_range = as.Date(c(Sys.Date() + 14 - recharge_lag + recharge_lag,
                                                  Sys.Date() + 30 - recharge_lag + recharge_lag,
                                                  Sys.Date() + 60 - recharge_lag + recharge_lag,
                                                  Sys.Date() + 90 - recharge_lag + recharge_lag)),
                          variable = c("Precip (30 day)", "Precip (30 day)", "Precip (30 day)", "Precip (30 day)"),
                          location = c(0.8, 0.87, 0.95, 1.03))


                        Precip_historical <- temp_historical_conditions_stats_plot %>%
                          filter(variable == "Precip (30 day)" )
                        Range = max(Precip_historical$predicted_value_max, na.rm = TRUE)

                        #plot(dummy_data2)


                        gglayers <- list(
                          #scale_y_continuous(sec.axis = sec_axis( trans=~.-temp2_toc, name="Depth below ground surface (m)")),
                          theme_bw(),
                          ylab(" Degrees (°C) SWE (mm) Precipitation (mm)"),
                          xlab(""),
                          scale_x_date(date_labels = ("%b"), date_breaks = "1 month", limits = c(start_day, end_day), expand = c(0,0)),
                          theme(#legend.title = element_blank(),
                            legend.position = "right",
                            legend.background = element_blank(),
                            axis.text.x = element_text(angle = 30, hjust =1),
                            legend.spacing.y = unit(0.1, 'mm'),
                            legend.margin = ggplot2::margin(t = 0.25, b = 0.25)))


                        #deterministic_forecast_data_temp
                        #ensemble_forecast_data_y_temp

                        temp_graph2 <- ggplot() +
                          geom_ribbon(data = temp_historical_conditions_stats_plot, alpha = 0.5, aes(x = Date, ymax = predicted_value_5th, ymin = predicted_value_min, fill = "1 Min - Q5"), size = 1) +
                          geom_ribbon(data = temp_historical_conditions_stats_plot, alpha = 0.5, aes(x = Date, ymax = predicted_value_10th, ymin = predicted_value_5th, fill = "2 Q5 - Q10"), size = 1) +
                          geom_ribbon(data = temp_historical_conditions_stats_plot, alpha = 0.5, aes(x = Date, ymax = predicted_value_25th, ymin = predicted_value_10th, fill = "3 Q10 - Q25"), size = 1) +
                          geom_ribbon(data = temp_historical_conditions_stats_plot, alpha = 0.5, aes(x = Date, ymax = predicted_value_75th, ymin = predicted_value_25th, fill = "4 Q25 - Q75"), size = 1) +
                          geom_ribbon(data = temp_historical_conditions_stats_plot, alpha = 0.5, aes(x = Date, ymax = predicted_value_90th, ymin = predicted_value_75th, fill = "5 Q75 - Q90"), size = 1) +
                          geom_ribbon(data = temp_historical_conditions_stats_plot, alpha = 0.5, aes(x = Date, ymax = predicted_value_95th, ymin = predicted_value_90th, fill = "6 Q90 - Q95"), size = 1) +
                          geom_ribbon(data = temp_historical_conditions_stats_plot, alpha = 0.5, aes(x = Date, ymax = predicted_value_max, ymin = predicted_value_95th, fill = "7 Q95 - Max"), size = 1) +
                          scale_fill_brewer(name = "Historical Data (2004-2023)", palette = "Spectral", direction = 1) +
                          scale_alpha_manual(name = "Historical Data (2004-2023)", values = c(1, 1, 1, 1, 1, 1, 1, 1)) +
                          gglayers +
                          new_scale_colour() +
                          new_scale_fill() +
                          geom_line(data = temp_historical_conditions, aes(x = Date, y = Value, colour = "1"), linewidth = 0.8) +
                          { if ("deterministic" %in% rfc_forecast_include) geom_line(data = deterministic_forecast_data_temp, aes(x = Date, y = Value, colour = "2"), linetype = 1, linewidth = 0.8) } +
                          # geom_line(data = deterministic_forecast_data_temp, aes(x = Date, y = Value, colour = "2"), linetype = 1, linewidth = 0.8) +
                          geom_line(data = ensemble_forecast_data_y_temp, aes(x = Date, y = Value, colour = "3", group = en_sim), linetype = 1, linewidth = 0.8) +
                          scale_colour_manual(name = "", values = c("black", "red", "orange")) +
                          geom_segment(data = dummy_data2,
                                       aes(x = lower_range,
                                           xend = upper_range,
                                           y = Range * location,
                                           yend = Range * location),
                                       size = 1, alpha = 0.5,
                                       color = "darkblue")+
                          geom_point(data = dummy_data2,
                                     aes(x = peak_recharge,
                                         y = Range * location),
                                     shape = 21, size = 1, alpha = 0.5, fill = "darkblue") +

                          geom_text(data = dummy_data2,
                                    aes(x = lower_range,
                                        y = Range * location,
                                        label = prediction_period),
                                    color = "darkblue",
                                    size = 2,
                                    vjust = 0.25,
                                    hjust = 1)+
                          theme(legend.position = "none") +  # Remove legend
                          facet_grid(variable ~ ., scale = "free_y")
                        temp_graph2



                      }


                      if ("ensemble" %in% rfc_forecast_include) {
                        ensemble_forecast_data_y_last_date <- max(ensemble_forecast_data_y$Date)
                      }

                      if ("deterministic" %in% rfc_forecast_include) {
                        deterministic_forecast_data_y_last_date <- max(deterministic_forecast_data_y$Date)
                      }


                      Time_series_data_2_plot <- left_join(Time_series_data_2_plot, well_lag_time_2, by = join_by(Well, lag_day))

                      Time_series_data_2_plot$Model <- factor(Time_series_data_2_plot$Model, levels = c("Good, forecast less sensitive to future weather",
                                                                                                        "Good, forecast more sensitive to future weather",
                                                                                                        "Fair, forecast less sensitive to future weather",
                                                                                                        "Fair, forecast more sensitive to future weather"))


                      # Define custom color scale for performance categories
                      performance_colors <- c('Good, forecast less sensitive to future weather' = '#00CC00',
                                              'Good, forecast more sensitive to future weather' = "#99FF00",
                                              'Fair, forecast less sensitive to future weather' = '#FF6600',
                                              'Fair, forecast more sensitive to future weather' = '#FFCC33')

                      gglayers <- list(
                        theme_bw(),
                        ylab("Water Level Below Ground (m)"),
                        xlab(""),
                        scale_x_date(date_labels = ("%b"), date_breaks = "1 month", limits = c(start_day, end_day), expand = c(0,0)),
                        theme(
                          legend.position = "right",
                          legend.background = element_blank(),
                          axis.text.x = element_text(angle = 30, hjust = 1),
                          legend.spacing.y = unit(0.1, 'mm'),
                          legend.margin = ggplot2::margin(t = 0.25, b = 0.25)
                        )
                      )

                      # Create the first plot (temp_graph) without the legend
                      temp_graph <- ggplot() +
                        geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per10th, ymin = Waterlevel_adjustments_temp - Min, fill = "1) Much Below Normal (0 - 10th percentile)"), size = 1) +
                        #   geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per10th, ymin = Waterlevel_adjustments_temp - per5th, fill =  ""), size = 1) +
                        geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per25th, ymin = Waterlevel_adjustments_temp - per10th, fill = "2) Below Normal (10 - 25th percentile)"), size = 1) +
                        geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per75th, ymin = Waterlevel_adjustments_temp - per25th, fill = "3) Normal (25 - 75th percentile)"), size = 1) +
                        geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per90th, ymin = Waterlevel_adjustments_temp - per75th, fill = "4) Above Normal (75 - 90th percentile)"), size = 1) +
                        #  geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per95th, ymin = Waterlevel_adjustments_temp - per90th, fill = "Above Normal (75 - 90th percentile)"), size = 1) +
                        geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - Max, ymin = Waterlevel_adjustments_temp - per90th, fill = "5) Much Above Normal (90 - 100th percentile)"), size = 1) +
                        geom_line(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, y = Waterlevel_adjustments_temp - per50th), linewidth = 0.1, colour = "#808080", alpha = 0.5) +
                        scale_fill_brewer(name = "Historical Data (2004-2023)", palette = "Spectral", direction = 1) +
                        scale_alpha_manual(name = "Historical Data (2004-2023)", values = c(1, 1, 1, 1, 1, 1, 1, 1)) +
                        gglayers +
                        new_scale_colour() +
                        geom_line(data = temp, aes(x = Date, y = Waterlevel_adjustments_temp - groundwater, colour = "Recorded GW Level"), linewidth = 1) +
                        scale_colour_manual(name = "", values = c("black")) +
                        new_scale_colour() +
                        geom_crossbar(data = Time_series_data_2_plot, aes(x = Date_predicted, y = Waterlevel_adjustments_temp - predicted_value_50th, ymin = Waterlevel_adjustments_temp - predicted_value_25th, ymax = Waterlevel_adjustments_temp - predicted_value_75th, colour = performance), alpha = 0.5, linetype = 1, size = 0.5, width = 5, position = position_dodge(width = 10)) +
                        geom_errorbar(data = Time_series_data_2_plot, aes(x = Date_predicted, ymin = Waterlevel_adjustments_temp - predicted_value_5th, ymax = Waterlevel_adjustments_temp - predicted_value_95th, colour = performance), alpha = 0.5, linetype = 1, size = 0.5, width = 5, position = position_dodge(width = 10)) +
                        scale_colour_manual(name = "Performance", values = performance_colors) +
                        # new_scale_colour() +
                        #   geom_errorbar(data = Time_series_data_2_plot, aes(x = Date_predicted, ymin = Waterlevel_adjustments_temp - predicted_value_50th + ME, ymax = Waterlevel_adjustments_temp - predicted_value_50th - ME, colour = "Model Error"), alpha = 0.5, linetype = 1, size = 0.5, width = 5, position = position_dodge(width = 10)) +
                        #  scale_colour_manual(name = "", values = c("dark grey")) +
                        # geom_vline(aes(xintercept = deterministic_forecast_data_y_last_date, colour = "Deterministic Forecast"), linetype = 2) +
                        #geom_vline(aes(xintercept = ensemble_forecast_data_y_last_date, colour = "Ensemble Forecast"), linetype = 3) +
                        #scale_colour_manual(name = "Climate Forecast Range", values = c("dark green", "dark blue", "steelblue2", "yellow")) +
                        guides(
                          color = guide_legend(reverse = TRUE)
                        ) +
                        theme(legend.position = "none") +  # Remove legend
                        facet_grid(Well ~ ., scale = "free_y")+
                        scale_y_reverse()
                      temp_graph

                      # Create the second plot (temp_graph2) without the legend
                      # JG remove for now
                      # ggsave(temp_graph,
                      #        filename = paste0(output_path, "/", "Well_", y, "_Model_Prediction_rawplot_", plot_type, ".jpeg"),
                      #        height = 6.5, width = 11, units = "in")


                      temp_graph_pdf <- ggplot() +
                        geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per10th, ymin = Waterlevel_adjustments_temp - Min, fill = "1) Much Below Normal (0 - 10th percentile)"), size = 1) +
                        #   geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per10th, ymin = Waterlevel_adjustments_temp - per5th, fill =  ""), size = 1) +
                        geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per25th, ymin = Waterlevel_adjustments_temp - per10th, fill = "2) Below Normal (10 - 25th percentile)"), size = 1) +
                        geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per75th, ymin = Waterlevel_adjustments_temp - per25th, fill = "3) Normal (25 - 75th percentile)"), size = 1) +
                        geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per90th, ymin = Waterlevel_adjustments_temp - per75th, fill = "4) Above Normal (75 - 90th percentile)"), size = 1) +
                        #  geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per95th, ymin = Waterlevel_adjustments_temp - per90th, fill = "Above Normal (75 - 90th percentile)"), size = 1) +
                        geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - Max, ymin = Waterlevel_adjustments_temp - per90th, fill = "5) Much Above Normal (90 - 100th percentile)"), size = 1) +
                        geom_line(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, y = Waterlevel_adjustments_temp - per50th), linewidth = 0.1, colour = "#808080", alpha = 0.5) +
                        scale_fill_brewer(name = "Historical Data (2004-2023)", palette = "Spectral", direction = 1) +
                        scale_alpha_manual(name = "Historical Data (2004-2023)", values = c(1, 1, 1, 1, 1, 1, 1, 1)) +
                        gglayers +
                        new_scale_colour() +
                        geom_line(data = temp, aes(x = Date, y = Waterlevel_adjustments_temp - groundwater, colour = "Recorded GW Level"), linewidth = 1) +
                        scale_colour_manual(name = "", values = c("black")) +
                        new_scale_colour() +
                        geom_crossbar(data = Time_series_data_2_plot, aes(x = Date_predicted, y = Waterlevel_adjustments_temp - predicted_value_50th, ymin = Waterlevel_adjustments_temp - predicted_value_25th, ymax = Waterlevel_adjustments_temp - predicted_value_75th, colour = performance), alpha = 0.5, linetype = 1, size = 0.5, width = 5, position = position_dodge(width = 10)) +
                        geom_errorbar(data = Time_series_data_2_plot, aes(x = Date_predicted, ymin = Waterlevel_adjustments_temp - predicted_value_5th, ymax = Waterlevel_adjustments_temp - predicted_value_95th, colour = performance), alpha = 0.5, linetype = 1, size = 0.5, width = 5, position = position_dodge(width = 10)) +
                        scale_colour_manual(name = "Performance", values = performance_colors) +
                        # new_scale_colour() +
                        #   geom_errorbar(data = Time_series_data_2_plot, aes(x = Date_predicted, ymin = Waterlevel_adjustments_temp - predicted_value_50th + ME, ymax = Waterlevel_adjustments_temp - predicted_value_50th - ME, colour = "Model Error"), alpha = 0.5, linetype = 1, size = 0.5, width = 5, position = position_dodge(width = 10)) +
                        #  scale_colour_manual(name = "", values = c("dark grey")) +
                        # geom_vline(aes(xintercept = deterministic_forecast_data_y_last_date, colour = "Deterministic Forecast"), linetype = 2) +
                        #geom_vline(aes(xintercept = ensemble_forecast_data_y_last_date, colour = "Ensemble Forecast"), linetype = 3) +
                        #scale_colour_manual(name = "Climate Forecast Range", values = c("dark green", "dark blue", "steelblue2", "yellow")) +
                        guides(
                          color = guide_legend(reverse = TRUE)
                        ) +
                        theme(legend.position = "none",
                              axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11)) +  # Remove legend
                        scale_y_reverse()




                      dummy_data <- data.frame(
                        Date_predicted = as.Date(c('2025-01-01', '2025-01-02', '2025-01-03', '2025-01-04')),
                        fake_value = c(1, 2, 3, 4),
                        performance = c('Good, forecast less sensitive to future weather',
                                        'Good, forecast more sensitive to future weather',
                                        'Fair, forecast less sensitive to future weather',
                                        'Fair, forecast more sensitive to future weather')
                      )


                      # Legend for png
                      legend_plot <- cowplot::get_legend(
                        ggplot() +
                          geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per10th, ymin = Waterlevel_adjustments_temp - Min, fill = "0 - 10th percentile (Much Below Normal)"), size = 1) +
                          #   geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per10th, ymin = Waterlevel_adjustments_temp - per5th, fill =  ""), size = 1) +
                          geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per25th, ymin = Waterlevel_adjustments_temp - per10th, fill = "10 - 25th percentile (Below Normal)"), size = 1) +
                          geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per75th, ymin = Waterlevel_adjustments_temp - per25th, fill = "25 - 75th percentile (Normal)"), size = 1) +
                          geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per90th, ymin = Waterlevel_adjustments_temp - per75th, fill = "75 - 90th percentile (Above Normal)"), size = 1) +
                          #  geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - per95th, ymin = Waterlevel_adjustments_temp - per90th, fill = "Above Normal (75 - 90th percentile)"), size = 1) +
                          geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5, aes(x = Date, ymax = Waterlevel_adjustments_temp - Max, ymin = Waterlevel_adjustments_temp - per90th, fill = "90 - 100th percentile (Much Above Normal)"), size = 1) +
                          scale_fill_brewer(name = "Historical Data", palette = "Spectral", direction = 1) +
                          geom_line(data = temp, aes(x = Date, y = Waterlevel_adjustments_temp - groundwater, colour = "Recorded GW Level/ Recorded Climate"), linewidth = 1) +
                          { if ("deterministic" %in% rfc_forecast_include) geom_line(data = deterministic_forecast_data_temp, aes(x = Date, y = Value, colour = "Deterministic Forecast"), linetype = 1, linewidth = 1) } +
                          # geom_line(data = deterministic_forecast_data_temp, aes(x = Date, y = Value, colour = "Deterministic Forecast"), linetype = 1, linewidth = 1) +
                          geom_line(data = ensemble_forecast_data_y_temp, aes(x = Date, y = Value, colour = "Ensemble Forecast", group = en_sim), linetype = 1, linewidth = 1) +
                          geom_line(
                            data = temp_WL_states_temp_plot,
                            aes(x = Date, y = Waterlevel_adjustments_temp - per50th),
                            linewidth = 0.5,
                            alpha = 0.5,
                            colour = "#808080"
                          ) +
                          geom_ribbon(
                            data = temp_WL_states_temp_plot[1,],    # a single-row dummy
                            aes(
                              x = Date,
                              ymin = 0,
                              ymax = 0,
                              fill = "Median line"
                            ),
                            alpha = 0,    # invisible layer
                            linewidth = 0
                          ) +
                          scale_colour_manual(
                            name = "Recorded Data",
                            values = c(
                              "Recorded GW Level/ Recorded Climate" = "black",
                              "Deterministic Forecast" = "red",
                              "Ensemble Forecast" = "orange",
                              "Median line" = "#808080"
                            )
                          ) +

                          new_scale_color() +
                          geom_crossbar(data = dummy_data,
                                        aes(x = Date_predicted, y = fake_value, ymin = fake_value, ymax = fake_value, colour = performance),
                                        alpha = 0.5, linetype = 1, size = 0.5, width = 5, position = position_dodge(width = 10)) +
                          geom_errorbar(data = dummy_data,
                                        aes(x = Date_predicted, ymin = fake_value, ymax = fake_value, colour = performance),
                                        alpha = 0.5, linetype = 1, size = 0.5, width = 5, position = position_dodge(width = 10)) +
                          scale_colour_manual(name = "Model Performance (forecast period)", values = performance_colors) +
                          new_scale_colour() +
                          #  geom_errorbar(data = Time_series_data_2_plot, aes(x = Date_predicted, ymin = Waterlevel_adjustments_temp - predicted_value_50th - ME, ymax = Waterlevel_adjustments_temp - predicted_value_50th + ME, colour = "Expected model error from 'typical conditions'"), alpha = 0.5, linetype = 1, size = 0.5, width = 5, position = position_dodge(width = 10)) +
                          geom_segment(data = dummy_data2,
                                       aes(x = lower_range,
                                           xend = upper_range,
                                           y = Range * location,
                                           yend = Range * location, color = "Peak Recharge Period"),
                                       size = 1, alpha = 0.5)+
                          geom_point(data = dummy_data2,
                                     aes(x = peak_recharge,
                                         y = Range * location, color = "Peak Recharge Period"),
                                     size = 1, alpha = 0.5 ) +

                          geom_text(data = dummy_data2,
                                    aes(x = lower_range,
                                        y = Range * location,
                                        label = prediction_period, color = "Peak Recharge Period"),
                                    size = 2,
                                    vjust = 0.25,
                                    hjust = 1)+
                          # geom_vline(aes(xintercept = deterministic_forecast_data_y_last_date, colour = "Deterministic Forecast"), linetype = 2) +
                          #  geom_vline(aes(xintercept = ensemble_forecast_data_y_last_date, colour = "Ensemble Forecast"), linetype = 3) +
                          scale_colour_manual(name = "", values = c("darkblue")) +
                          theme(
                            legend.position = "right",
                            legend.key.size = unit(0.5, "lines"),
                            legend.box = "vertical",
                            legend.title = element_text(size = 9)  # Adjust the size as needed
                          )
                      )
                      # plot(legend_plot)


                      showtext_auto(TRUE)

                      # Legend components for pdf plot
                      legend_plot_pdf_1 <- cowplot::get_legend(
                        ggplot() +
                          geom_line(data = temp, aes(x = Date, y = Waterlevel_adjustments_temp - groundwater, colour = "Groundwater Levels"), linewidth = 1) +
                          geom_line(data = ensemble_forecast_data_y_temp, aes(x = Date, y = Value, colour = "Ensemble Forecast", group = en_sim), linetype = 1, linewidth = 1) +
                          scale_colour_manual(name = "Recent Observed Data",
                                              values = c("Groundwater Levels" = "black")) +
                          theme(
                            legend.position = "right",
                            legend.key.size = unit(0.5, "lines"),
                            legend.box = "vertical",
                            legend.title = element_text(size = 9)#,
                            #text = element_text(family = "BCSans")
                          )
                      )
                      # plot(legend_plot_pdf_1)

                      legend_plot_pdf_2 <- cowplot::get_legend(
                        ggplot() +
                          geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5,
                                      aes(x = Date,
                                          ymax = Waterlevel_adjustments_temp - per10th,
                                          ymin = Waterlevel_adjustments_temp - Min,
                                          fill = "0 - 10th  (Much Below Normal)"),
                                      size = 1) +
                          geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5,
                                      aes(x = Date,
                                          ymax = Waterlevel_adjustments_temp - per25th,
                                          ymin = Waterlevel_adjustments_temp - per10th,
                                          fill = "10 - 25th  (Below Normal)"),
                                      size = 1) +
                          geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5,
                                      aes(x = Date,
                                          ymax = Waterlevel_adjustments_temp - per75th,
                                          ymin = Waterlevel_adjustments_temp - per25th,
                                          fill = "25 - 75th  (Normal)"),
                                      size = 1) +
                          geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5,
                                      aes(x = Date,
                                          ymax = Waterlevel_adjustments_temp - per90th,
                                          ymin = Waterlevel_adjustments_temp - per75th,
                                          fill = "75 - 90th  (Above Normal)"),
                                      size = 1) +
                          geom_ribbon(data = temp_WL_states_temp_plot, alpha = 0.5,
                                      aes(x = Date,
                                          ymax = Waterlevel_adjustments_temp - Max,
                                          ymin = Waterlevel_adjustments_temp - per90th,
                                          fill = "90 - 100th  (Much Above Normal)"),
                                      size = 1) +

                          # ---- MEDIAN LINE ----
                        geom_line(
                          data = temp_WL_states_temp_plot,
                          aes(x = Date,
                              y = Waterlevel_adjustments_temp - per50th,
                              colour = "50th (Median)"),
                          linewidth = 0.6
                        ) +

                          # ---- SAME LEGEND TITLE, DIFFERENT SCALES ----
                        # Fill scale (first in ordering)
                        scale_fill_brewer(
                          name = "Historic Percentiles",
                          palette = "Spectral",
                          direction = 1,
                          guide = guide_legend(order = 1)
                        ) +

                          # Colour scale (second in ordering)
                          scale_colour_manual(
                            name = NULL,
                            values = c("50th (Median)" = "#808080"),
                            guide = guide_legend(order = 2)     # <-- puts Median BELOW fills
                          ) +

                          theme(
                            legend.position = "right",
                            legend.key.size = unit(0.5, "lines"),
                            legend.box = "vertical",
                            legend.title = element_text(size = 9),
                            legend.key = element_rect(fill = NA, colour = NA),
                            legend.spacing.y = unit(-0.5, "lines"),
                            legend.background = element_rect(fill = NA, colour = NA)  # remove legend box background
                          )
                      )


                      # plot(legend_plot_pdf_2)

                      legend_plot_pdf_3 <- cowplot::get_legend(
                        ggplot() +
                          geom_crossbar(data = dummy_data,
                                        aes(x = Date_predicted, y = fake_value, ymin = fake_value, ymax = fake_value, colour = performance),
                                        alpha = 0.5, linetype = 1, size = 0.5, width = 5, position = position_dodge(width = 10)) +
                          geom_errorbar(data = dummy_data,
                                        aes(x = Date_predicted, ymin = fake_value, ymax = fake_value, colour = performance),
                                        alpha = 0.5, linetype = 1, size = 0.5, width = 5, position = position_dodge(width = 10)) +
                          scale_colour_manual(name = "Forecast Model Performance", values = performance_colors) +
                          theme(
                            legend.position = "right",
                            legend.key.size = unit(0.5, "lines"),
                            legend.box = "vertical",
                            legend.title = element_text(size = 9)#,
                            #text = element_text(family = "BCSans")
                          )
                      )
                      # plot(legend_plot_pdf_3)

                      showtext_auto(FALSE)



                      # aquifer info, aquifer number, aquifer subtype.

                      #disclaimer.
                      boxplot_description <- paste("Recorded & \n Predicted Levels \n 95th, 75th, 50th,\n 25th, 5th")
                      Well_name <- paste("Well =", pgown_well_info_Well_info$Well)
                      Location <- paste("Location =", pgown_well_info_Well_info$Location)

                      Aquifer_id <- paste("Aquifer ID =", pgown_well_info_Well_info$aquifer_id)
                      subtype <- paste("Aquifer subtype = ", pgown_well_info_Well_info$subtype_sym)
                      Climate_station_id <- paste("Climate Station =", pgown_well_info_Well_info$Climate_station_Id)
                      Climate_station_name <- paste("Climate Station = \n", pgown_well_info_Well_info$Climate_station_me)
                      snow_station <- paste("Snow Station =", pgown_well_info_Well_info$snow_stn)

                      above_normal_definition <- "1) Above Normal - above 75th percentile"
                      normal_definition <- "2) Normal - 25th to 75th percentile"
                      below_normal_definition <- "3) Below Normal - below 25th percentile"


                      note_1 <-  "    Explanation"
                      note_2 <-  "        - 'Range of Predictions' displays results of simulations using deterministic and ensemble climate forecast data,"
                      note_3 <-  "           and historical climate scenarios (for periods longer then available climate forecasts). Boxplots show percentiles"
                      note_4 <-  "           of simulations and general likelihood."
                      note_5 <- ""

                      note_6 <-  "        - 'Subject to climate uncertainty' indicates that the prediction at a given forecast period becomes more reliant on future"
                      note_7 <-  "           climate variability and is subject to more future climate uncertainty without accurate climate forcasts."
                      note_8 <- ""

                      note_9 <-  "        - 'Good' Model performance indicates for a given forecast period the model performs well, while 'Fair' indicates a acceptable"
                      note_10 <- "           model performance for a given forecast period."
                      note_11 <- ""
                      note_12 <- "        - 'Expected model error from typical conditions' indicates a probable range of model error from 'typical conditions' or 50th"
                      note_13 <- "           percentile for each forecast period based on Root Mean Square Error statistics generated during model testing."
                      note_14 <- "        - 'Peak Recharge Period' indicates the timeframe that climate variables are expected to most impact groundwater levels."



                      disclaimer_title <- "    Disclaimer"
                      disclaimer <-      "        Groundwater level forecasts use statistical models and third-party data. These models have two types of errors:"
                      disclaimer1 <-     "        systematic (model limitations) and random (input data). Forecasts may differ from actual observations, and"
                      disclaimer2 <-     "        water levels could exceed forecast bounds. Users must accept responsibility for their use and interpretation"



                      Note_comb <- ggpubr::text_grob(
                        paste(
                          note_1,
                          note_2,
                          note_3,
                          note_4,
                          #   note_5,
                          note_6,
                          note_7,
                          #  note_8,#
                          note_9,
                          note_10,
                          #note_11,
                          note_12,
                          note_13,
                          note_14,
                          "",
                          disclaimer_title,
                          disclaimer,
                          disclaimer1,
                          disclaimer2,
                          sep = "\n"
                        ),
                        x = 0, hjust = 0, face = "italic", size = 6.5
                      )



                      Note_comb2 <- ggpubr::text_grob(
                        paste(
                          note_8,
                          note_9,
                          note_10,
                          note_11,#
                          note_12,#
                          sep = "\n"
                        ),
                        x = 0, hjust = 0, face = "italic", size = 7
                      )




                      text_annotation2 <- ggpubr::text_grob(
                        paste(
                          "Disclaimer",
                          disclaimer,
                          disclaimer1,
                          disclaimer2,
                          sep = "\n"
                        ),
                        x = 0, hjust = 0, face = "italic", size = 6.5
                      )


                      snow_influenced  <- if (unique(pgown_well_info_Well_info$Snow_influenced == 0)) {
                        text_annotation <- ggpubr::text_grob(
                          paste(
                            #  Well_name,
                            # Location,
                            # "",
                            Aquifer_id,
                            subtype,
                            Climate_station_name,
                            sep = "\n"
                          ),
                          x = 0, hjust = 0,  size = 9
                        )
                      } else if (unique(pgown_well_info_Well_info$Snow_influenced == 1)) {      # SNOW INFLUENCED ----------------------------------------------------------

                        text_annotation <- ggpubr::text_grob(
                          paste(
                            Well_name,
                            Location,
                            "",
                            Aquifer_id,
                            subtype,
                            Climate_station_id,
                            snow_station,
                            sep = "\n"
                          ),
                          x = 0, hjust = 0,  size = 9
                        )
                      }



                      # Add the specified text below the table


                      Probabilities <- Time_series_data_2 %>%
                        mutate(days_in_year = yday(Date_predicted))




                      Probabilities <- left_join(Probabilities, temp_WL_states_temp, by = join_by(Well, days_in_year))

                      calculate_likelihood <- function(data, condition) {
                        data %>%
                          mutate(count = 1) %>%
                          group_by(Well, Date_predicted, lag_day ,Model) %>%
                          mutate(total = sum(count)) %>%
                          summarise(count = sum(if_else({{condition}}, count, 0)), total = mean(total),
                                    .groups = "keep") %>%
                          ungroup() %>%
                          mutate(likelihood = if_else(total == 0, NA_real_, count/total))
                      }

                      # Use the function to calculate likelihoods
                      Probabilities_above_normal <- calculate_likelihood(Probabilities, predicted_value >= per75th)
                      Probabilities_normal <- calculate_likelihood(Probabilities, predicted_value <= per75th & predicted_value >= per25th)
                      Probabilities_below_normal <- calculate_likelihood(Probabilities, predicted_value <= per25th)

                      # Combine the data frames
                      Probabilities_combined <- bind_rows(
                        mutate(Probabilities_above_normal, category = "1) Above Normal - above 75th percentile"),
                        mutate(Probabilities_normal, category = "2) Normal - 25th to 75th percentile"),
                        mutate(Probabilities_below_normal, category = "3) Below Normal - below 25th percentile")
                      )


                      # Calculate ensemble likelihoods
                      Probabilities_combined <- Probabilities_combined %>%
                        mutate(likelihood = likelihood * 100)

                      # JG removed rounding from data
                      # Probabilities_combined <- Probabilities_combined %>%
                      #   mutate(likelihood = pmin(round(likelihood / 5) * 5, 95))
                      Probabilities_combined <- Probabilities_combined %>%
                        filter(Model == "ANN")

                      Probabilities_combined_liklihood <- Probabilities_combined %>%
                        arrange(Date_predicted) %>%
                        mutate(table_name = paste0(Date_predicted, " (", lag_day, " days; ", Model, ")")) %>%
                        dplyr::select(category, table_name, likelihood) %>%
                        mutate(likelihood = case_when(likelihood < 10 ~ "<10",
                                                      likelihood > 90 ~ ">90",
                                                      TRUE ~ as.character(round(likelihood, 0)))) %>%
                        # mutate(likelihood = ifelse(likelihood > 90, ">90", round(likelihood, 0))) %>%
                        spread(table_name, likelihood)

                      Probabilities_combined_lag_day <- Probabilities_combined %>%
                        arrange(Date_predicted) %>%
                        mutate(table_name = paste0(Date_predicted, " (", lag_day, " days; ", Model, ")")) %>%
                        mutate(lag_day = paste0(lag_day, " days")) %>%
                        dplyr::select(category, table_name, lag_day) %>%
                        mutate(category = "") %>%
                        distinct(category, table_name, lag_day) %>%
                        spread(table_name, lag_day)

                      Probabilities_combined_Date_predicted <- Probabilities_combined %>%
                        arrange(Date_predicted) %>%
                        mutate(table_name = paste0(Date_predicted, " (", lag_day, " days; ", Model, ")")) %>%
                        dplyr::select(category, table_name, Date_predicted) %>%
                        mutate(category = "Likelihood of Groundwater Conditions (%)",
                               Date_predicted = as.character(format(Date_predicted, "%b-%d"))) %>%
                        distinct(category, table_name, Date_predicted) %>%
                        spread(table_name, Date_predicted)

                      Probabilities_combined_model <- Probabilities_combined %>%
                        arrange(Date_predicted) %>%
                        mutate(table_name = paste0(Date_predicted, " (", lag_day, " days; ", Model, ")")) %>%
                        dplyr::select(category, table_name, Model) %>%
                        mutate(category = "Conditions") %>%
                        distinct(category, table_name, Model) %>%
                        spread(table_name, Model)


                      # calculate latest percentile

                      latest_wl <- last_measurements %>%
                        filter(Well == y) %>%
                        dplyr::select(Well,
                                      Date = last_measurements,
                                      Latest = last_measurements_value)

                      temp_WL_states_latest <- temp_WL_states %>%
                        filter(Well == y) %>%
                        filter(days_in_year == yday(latest_wl$Date)) %>%
                        left_join(latest_wl, by = join_by(Well)) %>%
                        mutate(Latest_Percentile = case_when(Latest < per25th ~ "Below Normal",
                                                             Latest <= per75th  ~ "Normal",
                                                             Latest > per75th ~ "Above Normal"),
                               Date = as.character(format(Date, "%b-%d"))) %>%
                        dplyr::select(Date, Latest_Percentile)


                      latest_value_table <- tibble(latest = c("Latest",
                                                              temp_WL_states_latest$Date,
                                                              ifelse(temp_WL_states_latest$Latest_Percentile == "Above Normal", ">99", ""),
                                                              ifelse(temp_WL_states_latest$Latest_Percentile == "Normal", ">99", ""),
                                                              ifelse(temp_WL_states_latest$Latest_Percentile == "Below Normal", ">99", "")))


                      Probabilities_combined2 <- rbind(Probabilities_combined_lag_day,
                                                       Probabilities_combined_Date_predicted, #,Probabilities_combined_model,
                                                       Probabilities_combined_liklihood) %>%
                        bind_cols(latest_value_table) %>%
                        dplyr::select(category, latest, everything())


                      #         Probabilities_combined2 <- rbind(Probabilities_combined_lag_day,
                      #                                  Probabilities_combined_Date_predicted, #,Probabilities_combined_model,
                      #                                  Probabilities_combined_liklihood)


                      #Customize the table theme


                      mytheme <- gridExtra::ttheme_default(
                        core = list(
                          fg_params = list(
                            cex = 0.7
                          )
                        ),
                        colhead = list(fg_params = list(cex = 0.7)),
                        rowhead = list(fg_params = list(cex = 0.7))
                      )


                      temp_chart <- gridExtra::tableGrob(Probabilities_combined2, theme = mytheme, rows = NULL, cols = NULL)



                      #Anno#Anno#Annotate the ggplot table

                      temp_chart_gg <- temp_chart

                      # ggpubr::annotate_figure(temp_chart,
                      #                               top = ggpubr::text_grob(" "),
                      #                              fig.lab = "Table 1: Likelihood of Groundwater Conditions (%)",
                      #                             fig.lab.pos = "top.left",
                      #                            fig.lab.size = 9)


                      box_plot_explanation <- paste0(data_location, "Boxplot_Explanation.png")
                      box_plot_explanation <- image_read(box_plot_explanation)

                      box_plot_explanation <- rasterGrob(box_plot_explanation, interpolate = TRUE)



                      RF_logo <- paste0(data_location, "WLRS/English/BC_WLRS_H_RGB_pos.png")
                      RF_logo <- image_read(RF_logo)
                      RF_logo <- rasterGrob(RF_logo, interpolate = TRUE)

                      Hatfield_logo <- paste0(data_location, "Hatfield_Logo_Hor_Blue_RGB.png")
                      Hatfield_logo <- image_read(Hatfield_logo)
                      Hatfield_logo <- rasterGrob(Hatfield_logo, interpolate = TRUE)


                      # Arrange the plots and the table in a grid with cowplot
                      temp_figure <- cowplot::plot_grid(
                        cowplot::plot_grid(temp_graph, temp_graph2, ncol = 1, align = "v", axis = "lr", rel_heights = c(1, 0.75)),
                        cowplot::plot_grid( temp_chart_gg,
                                            cowplot::plot_grid(legend_plot,
                                                               cowplot::plot_grid(text_annotation,box_plot_explanation, ncol = 1, align = "v", axis = "lr", rel_heights = c(0.3, 0.7)),
                                                               ncol = 2, rel_widths = c(1, 0.8)),
                                            Note_comb,
                                            cowplot::plot_grid(Hatfield_logo, RF_logo, ncol = 2, align = "h", rel_widths = c(0.25, 0.6)),
                                            ncol = 1,
                                            rel_heights = c(0.18,0.5, 0.3,0.08)
                        ),
                        ncol = 2,
                        rel_widths = c(1.5, 1)
                      )


                      # Print the final figure
                      print(temp_figure)




                      #Arrange the plot and the table in a grid
                      #temp_figure <- ggpubr::ggarrange(temp_graph, temp_chart_gg, ncol = 2, widths = c(3, 1))



                      location <- pgown_well_info %>%
                        filter(Well == y) %>%
                        pull(Location)

                      recharge_type <- ifelse(plot_type == "snow", "snowmelt dominated environment", "rainfall dominated environment")



                      #Annotate the final figure
                      final_figure <- ggpubr::annotate_figure(temp_figure,
                                                              top = ggpubr::text_grob(" "),
                                                              fig.lab = paste0(y, " - ", location, ", ", recharge_type, " (Prediction Date: ", last_date, ")"),
                                                              fig.lab.pos = "top.left"
                      )

                      #Save the final figure

                      ggsave(final_figure, filename = paste0(output_path, "/", y, "_Model_Forecast.jpeg"), height = 8, width = 13.5, units = "in", bg = "white")
                      ggsave(final_figure, filename = paste0("output/", y, ".jpeg"), height = 8, width = 13.5, units = "in", bg = "white")
                      #  ggsave(final_figure, filename = paste0(figure_location, "Well_", y, "_Model_Forecast_", plot_type, ".jpeg"), height = 8, width = 11.5, units = "in")


                      #simplistic image



                      # Arrange the plots and the table in a grid with cowplot
                      temp_figure_simple <- cowplot::plot_grid(
                        cowplot::plot_grid(temp_graph, temp_graph2, ncol = 1, align = "v", axis = "lr", rel_heights = c(1, 0.75)),
                        cowplot::plot_grid( #temp_chart_gg,
                          legend_plot,
                          box_plot_explanation,
                          # Note_comb,
                          cowplot::plot_grid(Hatfield_logo, RF_logo, ncol = 2, align = "h", rel_widths = c(0.4, 0.6)),
                          ncol = 1,
                          rel_heights = c(0.6, 0.25, 0.15)
                        ),
                        ncol = 2,
                        rel_widths = c(2, 1)
                      )


                      # Print the final figure
                      print(temp_figure_simple)




                      #Arrange the plot and the table in a grid
                      #temp_figure <- ggpubr::ggarrange(temp_graph, temp_chart_gg, ncol = 2, widths = c(3, 1))



                      location <- pgown_well_info %>%
                        filter(Well == y) %>%
                        pull(Location)

                      recharge_type <- ifelse(plot_type == "snow", "snowmelt dominated environment", "rainfall dominated environment")



                      #Annotate the final figure
                      final_figure_simple <- ggpubr::annotate_figure(temp_figure_simple,
                                                                     top = ggpubr::text_grob(" "),
                                                                     fig.lab = paste0(y, " - ", location, ", ", recharge_type, " (Prediction Date: ", last_date, ")"),
                                                                     fig.lab.pos = "top.left"
                      )

                      #Save the final figure
                      # JG remove for now
                      # ggsave(final_figure_simple, filename = paste0(output_path, "/", y, "_Model_Forecast_simple_", plot_type, ".jpeg"), height = 6, width = 10.1, units = "in")
                      #  ggsave(final_figure, filename = paste0(figure_location, y, "_Model_Forecast_", plot_type, ".jpeg"), height = 8, width = 11.5, units = "in")




                      # Markdown Report

                      if (generate_well_pdf) {

                        recharge_type_2 <- ifelse(plot_type == "snow", "Snow", "Rain")

                        start_year <- temp %>%
                          filter(!is.na(groundwater)) %>%
                          pull(Date) %>%
                          min() %>%
                          year()

                        # Make the gt table

                        library(gt)

                        prob_cats_orig <- c("1) Above Normal - above 75th percentile",
                                            "2) Normal - 25th to 75th percentile",
                                            "3) Below Normal - below 25th percentile")

                        prob_cats <- c("Above Normal",
                                       "Normal",
                                       "Below Normal")

                        latest_column <- latest_value_table[c(3:5),]

                        table_for_gt <- Probabilities_combined %>%
                          arrange(Date_predicted) %>%
                          mutate(table_name = paste0(lag_day, "-days ", format(Date_predicted, "%b-%d"))) %>%
                          dplyr::select(category, table_name, likelihood) %>%
                          mutate(likelihood = case_when(likelihood < 10 ~ "<10",
                                                        likelihood > 90 ~ ">90",
                                                        TRUE ~ as.character(round(likelihood, 0)))) %>%
                          spread(table_name, likelihood) %>%
                          bind_cols(latest_column) %>%
                          dplyr::select(category, latest, everything()) %>%
                          mutate(category = case_when(category == prob_cats_orig[1] ~ prob_cats[1],
                                                      category == prob_cats_orig[2] ~ prob_cats[2],
                                                      category == prob_cats_orig[3] ~ prob_cats[3]),
                                 Percentiles = case_when(category == prob_cats[1] ~ "Above 75th",
                                                         category == prob_cats[2] ~ "25th to 75th",
                                                         category == prob_cats[3] ~ "Below 25th")) %>%
                          select("Groundwater Level Conditions" = category, "Percentile Range" = Percentiles, latest, everything())
                        names(table_for_gt)[names(table_for_gt) == "latest"] <- paste0("Latest ", latest_value_table[2,])


                        flag_tbl_above <- table_for_gt %>%
                          dplyr::slice(1) %>% # only first row
                          select(-(1:2)) %>%
                          mutate(across(everything(), ~ {
                            num <- suppressWarnings(as.numeric(gsub("[^0-9]", "", .x)))
                            res <- .x == ">95" | num > 50
                            tidyr::replace_na(res, FALSE)  # convert NA → FALSE
                          }))

                        flag_tbl_normal <- table_for_gt %>%
                          dplyr::slice(2) %>% # only first row
                          select(-(1:2)) %>%
                          mutate(across(everything(), ~ {
                            num <- suppressWarnings(as.numeric(gsub("[^0-9]", "", .x)))
                            res <- .x == ">95" | num > 50
                            tidyr::replace_na(res, FALSE)  # convert NA → FALSE
                          }))

                        flag_tbl_below <- table_for_gt %>%
                          dplyr::slice(3) %>% # only first row
                          select(-(1:2)) %>%
                          mutate(across(everything(), ~ {
                            num <- suppressWarnings(as.numeric(gsub("[^0-9]", "", .x)))
                            res <- .x == ">95" | num > 50
                            tidyr::replace_na(res, FALSE)  # convert NA → FALSE
                          }))

                        table_gt <- table_for_gt %>%
                          gt() %>%
                          tab_style(
                            style = cell_fill(color = "lightblue"),
                            locations = cells_body(
                              rows = 1,
                              columns = all_of(names(flag_tbl_above)[unlist(flag_tbl_above)])
                            )
                          ) %>%
                          tab_style(
                            style = cell_fill(color = "lightgreen"),
                            locations = cells_body(
                              rows = 2,
                              columns = all_of(names(flag_tbl_normal)[unlist(flag_tbl_normal)])
                            )
                          ) %>%
                          tab_style(
                            style = cell_fill(color = "lightpink"),
                            locations = cells_body(
                              rows = 3,
                              columns = all_of(names(flag_tbl_below)[unlist(flag_tbl_below)])
                            )
                          )  %>%
                          cols_width(
                            starts_with("Groundwater") ~ pct(20),
                            starts_with("Percentile") ~ pct(17),
                            everything() ~ pct(12)
                          )  %>%
                          cols_align(
                            align = "center",
                            columns = -(1:2)  # this means: all columns *except* the first
                          )  %>%
                          cols_align(
                            align = "left",
                            columns = 1:2  # or use tidyselect helpers like everything(), starts_with(), etc.
                          )%>%
                          tab_style(
                            style = cell_text(weight = "bold"),
                            locations = list(cells_column_labels(everything()))
                          ) %>%
                          tab_style(
                            style = list(cell_fill(color = "gray96"),
                                         cell_text(color = "black")),
                            locations = cells_column_labels(everything())
                          )  %>%
                          tab_style(
                            style = cell_fill(color = "gray96"),
                            locations = cells_body(columns = 1:2)
                          )

                        # Make the pdf plot and merge in legends

                        showtext_auto(TRUE)

                        main_plot <- temp_graph_pdf +
                          theme(text = element_text(family = "BCSans"))


                        legend_pdf <- plot_grid(plot_grid(legend_plot_pdf_1,
                                                          legend_plot_pdf_2,
                                                          legend_plot_pdf_3,
                                                          ncol = 1, align = "v",
                                                          rel_heights = c(0.1, 0.3, 0.3)),
                                                ggdraw() +
                                                  draw_image("docs/range_legend.png", x = -0.1, scale = 1),
                                                ncol = 1, align = "v",
                                                rel_heights = c(0.7, 0.3))

                        plot_pdf <- plot_grid(plot_grid(main_plot, ncol = 1),
                                              # spacer,
                                              legend_pdf,
                                              rel_widths = c(0.7,0.3),
                                              nrow = 1)


                        # Temporary directory for rendering
                        tmp_dir <- file.path(tempdir(), paste0("tmp_", y))
                        dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

                        int_dir <- file.path(tempdir(), paste0("int_", y))
                        dir.create(int_dir, recursive = TRUE, showWarnings = FALSE)

                        temp_font_dir <- file.path(tmp_dir, "fonts")
                        dir.create(temp_font_dir, recursive = TRUE, showWarnings = FALSE)

                        # Copy files into temp dir
                        rmd_copy <- file.copy("docs/well_report.Rmd", tmp_dir, overwrite = TRUE)
                        tex_copy <- file.copy("docs/well_report.tex", tmp_dir, overwrite = TRUE)
                        png_copy <- file.copy("docs/BCID_V_RGB_rev.png", tmp_dir, overwrite = TRUE)
                        png2_copy <- file.copy("docs/range_legend.png", tmp_dir, overwrite = TRUE)
                        fonts_copy <- file.copy(from = list.files("docs/fonts/", full.names = TRUE),
                                                to = temp_font_dir, recursive = FALSE)

                        # Save the plot to read back in.
                        ggsave(
                          filename = file.path(tmp_dir, paste0("Well_", y, "_plot.pdf")),
                          plot = plot_pdf,
                          device = cairo_pdf,
                          width = 10,
                          height = 4
                        )

                        # options(browser = "/usr/bin/chromium-browser")
                        # gtsave(
                        #   file.path(tmp_dir, paste0("Well_", y, "_table.png")),
                        #   data = table_gt,
                        #   vwidth = 750,
                        #   vheight = 1000,
                        #   expand = 10
                        # )

                        showtext_auto(FALSE)


                        # Path to copied Rmd
                        tmp_rmd <- file.path(tmp_dir, "well_report.Rmd")

                        # Render the document
                        tryCatch({
                          rmarkdown::render(input = tmp_rmd,
                                            output_file = paste0(y, "_Model_Forecast.pdf"),
                                            output_dir = output_path,
                                            knit_root_dir = tmp_dir,
                                            intermediates_dir = int_dir,
                                            envir = new.env(),
                                            params = list("well_id" = y[[1]],
                                                          "well_location" = location,
                                                          "well_tag_number" = pgown_well_info_Well_info$Well_Tag_number,
                                                          "aquifer_num" = pgown_well_info_Well_info$aquifer_id,
                                                          "aquifer_type" = pgown_well_info_Well_info$aquifer_material,
                                                          "model_type" = paste0(recharge_type_2, "-Dominated Model"),
                                                          "latest_date" = last_date,
                                                          "start_year" = start_year,
                                                          "table" = table_gt))
                        }, error = function(e) {
                        })


                        # ### Create the png
                        #
                        # # read in the table
                        # table_grob <- image_read(file.path(tmp_dir, paste0("Well_", y, "_table.png")))
                        # table_grob <- rasterGrob(table_grob, interpolate = TRUE)
                        #
                        # logo_grob <- image_read("docs/BCID_V_RGB_rev.png")
                        # logo_grob <- rasterGrob(logo_grob, interpolate = TRUE)
                        #
                        # table_title <- ggdraw() +
                        #   draw_label(paste0("Observation Well ", y, " - ", location),
                        #              x = 0.5, y = 0.85, hjust = 0.5, fontface = 'bold', size = 13, color = "black") +
                        #   draw_label(paste0(pgown_well_info_Well_info$aquifer_material, " Aquifer | Data since ",
                        #                     start_year, " | Latest on ", format(last_date, "%b-%d"),
                        #                     " | Forecast issued: ", format(Sys.Date(), "%b-%d")),
                        #              x = 0.5, y = 0.4, hjust = 0.5,  size = 12, color = "black")
                        #
                        #
                        # disclaimer <- ggdraw() +
                        #   draw_label(paste0("Disclaimer: Groundwater level forecasts use statistical models and third-\n",
                        #                     "party data. These models have two types of errors: systematic (model\n",
                        #                     "limitations) and random (input data). Forecasts may differ from actual\n",
                        #                     "observations, and water levels could exceed forecast bounds. Users must\n",
                        #                     "accept responsibility for their use and interpretation."),
                        #              x = 0.5, y = 0.1, hjust = 0.5, vjust = 0,
                        #              fontface = 'italic', size = 9, color = "black")
                        #
                        #
                        # banner <- ggdraw() +
                        #   draw_grob(logo_grob, x = 0.9, y = 0.09, width = 0.1, height = 0.8) +
                        #   draw_label(paste0("Groundwater Level Forecast"), x = 0.03, y = 0.65, hjust = 0,
                        #              fontface = "bold", size = 18, color = "white") +
                        #   draw_label(paste0("Forecast of likely groundwater levels for the next 14–90 days compared to historical conditions"),
                        #              x = 0.03, y = 0.3, hjust = 0,
                        #              fontface = "bold", size = 10, color = "white") +
                        #   theme(plot.background = element_rect(fill = "#234075b3"))+
                        #   draw_line(
                        #     x = c(0, 1), y = c(0, 0),
                        #     size = 1, color = "#e3a82b"
                        #   )
                        #
                        # footer_text <- ggdraw() +
                        #   draw_label(paste0("Issued by BC River Forecast Centre on ",format(Sys.time(), "%d %b %Y %H:%M")),
                        #              x = 0.5, y = 0.6, hjust = 0.5, vjust = 0,
                        #              , size = 10, color = "black")
                        #
                        # blank_space <- ggplot() + theme_void()
                        #
                        # plot_png <- plot_grid(plot_grid(main_plot, ncol = 1),
                        #                       legend_pdf,
                        #                       rel_widths = c(0.6,0.3),
                        #                       nrow = 1)
                        #
                        # note_text <- ggdraw() +
                        #   draw_label("Note: Forecasts provide a range of possible conditions and are presented as likelihoods of groundwater being above, below, or near normal.\nThis analysis uses the “normal” term solely in reference to water levels between the 25th to 75th percentiles of historical data, not to\nimply a steady state baseline for a well. Data should be interpreted with the context of long-term records, patterns, and trends.",
                        #              # fontface = "bold",
                        #              size = 8,
                        #              x = 0.5, y = 0.6,
                        #              hjust = 0.5, vjust = 0.5)
                        #
                        # disclaimer_text <- ggdraw() +
                        #   draw_label("Disclaimer: Groundwater level forecasts use statistical models and third-party data. These models have two types of errors: systematic\n(model limitations) and random (input data). Forecasts may differ from actual observations, and water levels could exceed forecast bounds.\nUsers must accept responsibility for their use and interpretation.",
                        #              # fontface = "bold",
                        #              size = 8,
                        #              x = 0.5, y = 0.9,
                        #              hjust = 0.5, vjust = 0.5)
                        #
                        # final_output <- plot_grid(
                        #   banner,
                        #   blank_space,
                        #   table_title,
                        #   plot_png,
                        #   ggdraw() +
                        #     draw_label(paste0(y, "- Likelihood (%) of Groundwater Level Conditions"),
                        #                x = 0.5, y = 0.4, vjust = 1, hjust = 0.5,  size = 12, color = "black"),
                        #   plot_grid(blank_space, table_grob, blank_space, ncol = 3, rel_widths = c(0.05,0.9,0.05)),
                        #   note_text,
                        #   disclaimer_text,
                        #   # plot_grid(note_text, note_text, ncol = 2, rel_widths = c(0.5, 0.5)),
                        #   footer_text,
                        #   ncol = 1,
                        #   rel_heights = c(0.08,
                        #                   0.02,
                        #                   0.07,
                        #                   0.42,
                        #                   0.06,
                        #                   0.26,
                        #                   0.10,
                        #                   0.08,
                        #                   0.02)
                        # )
                        #
                        # save_plot(plot = final_output, filename = paste0(output_path, "/", y, "_Model_Forecast.png"),
                        #           base_width = 8.5, base_height = 8, bg = "white")

                      }



                      # Summarize data for output


                      Probabilities_combined_output <- Probabilities_combined %>%
                        mutate(Forecast_Date = Sys.Date()) %>%
                        # mutate(likelihood = ifelse(likelihood < 5, "<5", round(likelihood, 0))) %>%
                        mutate(conditions = category) %>%
                        mutate(Latest_Conditions = temp_WL_states_latest$Latest_Percentile) %>%
                        dplyr::select(Well, Forecast_Date, Date_predicted, lag_day, Model, likelihood, conditions, Latest_Conditions)
                      Probabilities_combined_output

                      Waterlevel_adjustments

                      Time_series_data_2_plot <- Time_series_data_2_plot %>%
                        dplyr::select(-Model) %>%
                        mutate(across(where(is.numeric), ~ na_if(.x, -Inf))) %>%
                        mutate(across(where(is.numeric), ~ na_if(.x, Inf))) %>%
                        mutate(across(where(is.numeric), ~ na_if(.x, NaN))) %>%
                        mutate(across(where(is.numeric), ~ replace_na(.x, NA))) %>%
                        mutate(
                          groundwater = Waterlevel_adjustments_temp - as.numeric(groundwater),
                          predicted_value_mean = Waterlevel_adjustments_temp - as.numeric(predicted_value_mean),
                          predicted_value_min = Waterlevel_adjustments_temp - as.numeric(predicted_value_min),
                          predicted_value_max = Waterlevel_adjustments_temp - as.numeric(predicted_value_max),
                          predicted_value_25th = Waterlevel_adjustments_temp - as.numeric(predicted_value_25th),
                          predicted_value_75th = Waterlevel_adjustments_temp - as.numeric(predicted_value_75th),
                          predicted_value_50th = Waterlevel_adjustments_temp - as.numeric(predicted_value_50th),
                          predicted_value_90th = Waterlevel_adjustments_temp -as.numeric(predicted_value_90th),
                          predicted_value_10th = Waterlevel_adjustments_temp - as.numeric(predicted_value_10th),
                          predicted_value_5th = Waterlevel_adjustments_temp - as.numeric(predicted_value_5th),
                          predicted_value_95th = Waterlevel_adjustments_temp - as.numeric(predicted_value_95th)
                        ) %>%
                        mutate(missing_data = ifelse(is.na(groundwater), "Missing", NA))

                      Probabilities_combined_output <- full_join(Probabilities_combined_output, Time_series_data_2_plot,
                                                                 by = join_by(Well, Date_predicted, lag_day))

                    } else {

                      Probabilities_combined_output <- data.frame(Well = y,
                                                                  Forecast_Date = Sys.Date(),
                                                                  Date_predicted = NA,
                                                                  lag_day = NA,
                                                                  Model = "ANN",
                                                                  likelihood = NA,
                                                                  conditions = "No forecast available (no recent data)",
                                                                  Date = last_measurements_well,
                                                                  Latest_Conditions = NA,
                                                                  groundwater = NA,
                                                                  predicted_value_mean = NA,
                                                                  predicted_value_min = NA,
                                                                  predicted_value_max = NA,
                                                                  predicted_value_25th = NA,
                                                                  predicted_value_75th = NA,
                                                                  predicted_value_50th = NA,
                                                                  predicted_value_90th = NA,
                                                                  predicted_value_10th = NA,
                                                                  predicted_value_5th = NA,
                                                                  predicted_value_95th = NA,
                                                                  performance = NA,
                                                                  # ME = NA,
                                                                  missing_data = NA)


                    }


                    return(Probabilities_combined_output)

                  }

}
