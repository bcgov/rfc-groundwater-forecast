dl_deterministic_forecast <- function(pgown_well_info, data_location) {
  
  url <- "https://nrs.objectstore.gov.bc.ca/rfc-conditions/gw_forecasting/FORECAST_CMC.csv"
  
  deterministic_forecast<- read.csv(paste0(url))
  
  deterministic_forecast <- deterministic_forecast %>%
    gather(c(2:1450), key = "Station", value = Value)%>%
    mutate(station_name_RF_forecast = str_sub(Station, start = 1L, end = 3L)) %>%
    mutate(Variable = str_sub(Station, start = -2L, end = -1L)) %>%
    dplyr::select(-Station)%>%
    spread(key = Variable, value = Value) %>%
    mutate(TA = (TN+TX)/2)
    

  
  deterministic_forecast <- full_join(deterministic_forecast, pgown_well_info)
  
  deterministic_forecast <- deterministic_forecast %>%
    drop_na(Well)%>%
    dplyr::rename(Date = DATE)%>%
    dplyr::select(Well,station_name_RF_forecast,Date,PP,TN,TX,TA)
  
  return(deterministic_forecast)
}



