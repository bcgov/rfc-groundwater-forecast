dl_climate_data <- function(pgown_well_info, data_location) {
  
  #stations_dl()
  
  #### Create list of climate stations that need data download ####
  weathercan_station_list <- as.list(
    unique(c(unique(pgown_well_info$climate_infilled_1), 
             unique(pgown_well_info$climate_infilled_2),
             unique(pgown_well_info$climate_infilled_3),
                    unique(pgown_well_info$climate_infilled_4),
             unique(pgown_well_info$climate_infilled_5))))
  
  #### Download climate data ####
  climate_dl <- lapply(weathercan_station_list, function(Climate_station_Id) {
    weather_dl(station_ids = Climate_station_Id, interval = "day", start = "2004-01-01")
  }) %>%
    bind_rows() 

  
  climate_record <- climate_dl %>%
    dplyr::select(station_name, station_id, date, total_precip,mean_temp)
  
  climate_record_padded <- climate_record %>%
    group_by(station_name, station_id) %>%
    pad(interval = "day") %>%
    ungroup()
  climate_record_padded <- climate_record_padded %>%
    mutate(count = 1, 
           temp_count = ifelse(is.na(mean_temp), 0, 1),
           precip_count = ifelse(is.na(total_precip), 0, 1)
            ) %>%
    group_by(station_name, station_id) %>%
    summarise(count = sum(count), temp_count = sum(temp_count), precip_count = sum(precip_count),
              start_date = min(date), end_date = max(date)) %>%
    ungroup()%>%
    mutate(temp_precentcomplete = temp_count/count, 
           precip_precentcomplete = precip_count/count)
  
  
  
  
  
  
  
 # save(climate_record_padded,file = paste0(data_location, "climate_data_record.RData"))
  
  
  #### Set order of stations for infilling data (primary, secondary, tertiary station) #### 
  station_order <- pgown_well_info %>%
    dplyr::select(c("climate_infilled_1", "climate_infilled_2", "climate_infilled_3","climate_infilled_4","climate_infilled_5")) %>%
    distinct()
  
  #### Infill climate data from secondary (and tertiary stations) ####
  climate_infilled <- lapply(1:nrow(station_order), function(i) {
  #i = 3
    order <- as.character(unlist(station_order[i, ]))
    #print(order)
    
    climate_filled <- climate_dl %>%
      filter(station_id %in% order) %>%
      arrange(match(.$station_id, order)) %>%
      filter(!is.na(mean_temp) & !is.na(total_precip)) %>%
      distinct(date, .keep_all = TRUE)
    
    return(climate_filled)
  }) %>%
    bind_rows()
  
  #### Join infilled data with PGOWN station data ####
  climate_data <- power_left_join(climate_infilled, pgown_well_info, 
                                  by = ~ .x$station_id == .y$climate_infilled_1 | .x$station_id == .y$climate_infilled_2 | .x$station_id == .y$climate_infilled_3 |.x$station_id == .y$climate_infilled_4 | .x$station_id == .y$climate_infilled_5) %>%
    dplyr::select(-contains(".x")) %>%
    rename_with(~ gsub("\\.y", "", .), contains(".y")) %>%
    dplyr::rename(Date = date) %>%
    dplyr::select(Well, Date, mean_temp, total_precip, station_id)
   

  #### Saving/returning climate data ####

  return(climate_data)
  
}


