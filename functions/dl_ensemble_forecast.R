
dl_ensemble_forecast <- function(pgown_well_info, data_location) {
  
  
  
  url <- "https://nrs.objectstore.gov.bc.ca/rfc-conditions/gw_forecasting/MSC_ENS.csv"
  
  
  ensemble_forecast <- read.csv(paste0(url))
  
  ensemble_forecast <- ensemble_forecast %>% 
    filter(DATE != "DATE")%>%
    mutate(DATE = as.Date(DATE)) %>%
    group_by(DATE) %>%
    mutate(simulation = row_number())%>%
    gather(c(2:1450), key = "Station", value = Value)%>%
    mutate(station_name_RF_forecast = str_sub(Station, start = 1L, end = 3L)) %>%
    mutate(Variable = str_sub(Station, start = -2L, end = -1L)) %>%
    dplyr::select(-Station)%>%
    mutate(Value = as.numeric(Value))%>%
    spread(key = Variable, value = Value) %>%
    mutate(TA = (TN+TX)/2) 
  
  ensemble_forecast <- full_join(ensemble_forecast, pgown_well_info)
  
  ensemble_forecast <- ensemble_forecast %>%
    drop_na(Well)%>%
    dplyr::rename(Date = DATE)%>%
    dplyr::select(Well,station_name_RF_forecast,simulation,Date,PP,TN,TX,TA)
  
  
  return(ensemble_forecast)
}

