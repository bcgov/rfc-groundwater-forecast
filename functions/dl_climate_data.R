dl_climate_data <- function(pgown_well_info, data_location) {
  
  #stations_dl()
  
  #### Create list of climate stations that need data download ####
  temp <- pgown_well_info %>%
    #dplyr::select(Climate_station_Id,Climate_Infilled_id,Climate_secondary_Infilled,Climate_tertiary_Infilled,Climate_quaternary_Infilled) %>%
    tidyr::gather(c(Climate_station_Id,Climate_Infilled_id,Climate_secondary_Infilled,Climate_tertiary_Infilled,Climate_quaternary_Infilled), key = "Infilled", value = "Station")%>%
    separate_rows(Station, Well, sep = ". ")
  
  
  
  weathercan_station_list <- as.list(
    unique(c(unique(temp$Station))))
  

  #### Download climate data ####
  climate_dl <- lapply(weathercan_station_list, function(Climate_station_Id) {
    weather_dl(station_ids = Climate_station_Id, interval = "day", start = "2004-01-01")
  }) %>%
    bind_rows() 

  #### Set order of stations for infilling data (primary, secondary, tertiary station) #### 
  station_order <- pgown_well_info %>%
    dplyr::select(c("Well","Climate_station_Id", "Climate_Infilled_id", "Climate_secondary_Infilled","Climate_tertiary_Infilled","Climate_quaternary_Infilled")) %>%
    distinct()
  
  
  climate_dl <- climate_dl %>%
    mutate(station_id2 = station_id)%>%
    group_by(station_name)%>%
    mutate(station_id = paste(unique(station_id), collapse = ". ")) %>%
    group_by(station_id, station_name, date) %>%
    summarise(mean_temp = mean(mean_temp), 
              total_precip = mean(total_precip)) %>%
    ungroup()
  
  
  

   Well_list <- as.list(unique(pgown_well_info$Well))
   
   climate_infilled <- data.frame()
   
   for(i in Well_list){
     
    #i = "OW124"
     
    ranks <- station_order %>%
      filter(Well == i) %>%
     gather(Climate_station_Id:Climate_quaternary_Infilled, key = "Key", value = "station_id") %>%
      mutate(climate_station_rank = row_number()) %>%
      dplyr::select(-Key)
    
    order <-station_order %>%
      filter(Well == i) %>%
      dplyr::select(-Well)
    
    order <- as.character(unlist(order))
    #print(order)
    
    climate_filled <- climate_dl %>%
      filter(station_id %in% order) %>%
      arrange(match(.$station_id, order)) %>%
    filter(!is.na(mean_temp) & !is.na(total_precip)) %>%
      distinct(date, .keep_all = TRUE)
    
    climate_filled <- left_join(climate_filled, ranks) 
    
    climate_infilled <- rbind(climate_infilled,climate_filled)
     
   }
   
   
   
    
    climate_data <- climate_infilled%>%
      # Remove columns with ".x" suffix
      dplyr::rename(Date = date) %>%
      # Select relevant columns
      dplyr::select(Well, Date, mean_temp, total_precip, climate_station_rank)
    
    
    
  #climate_data_temp <- climate_data %>%
   # filter(Well == "OW378")
  #### Saving/returning climate data ####
  #save(climate_data,file = paste0(data_location, "climate_data.RData"))
  
  return(climate_data)
  
}

#load(paste0(data_location, "climate_data.RData"))

