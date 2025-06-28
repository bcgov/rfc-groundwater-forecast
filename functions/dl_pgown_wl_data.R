dl_pgown_wl_data <- function(pgown_well_info, data_location) {

url <- "http://www.env.gov.bc.ca/wsd/data_searches/obswell/map/data/"

well_list <- as.list(unique(pgown_well_info$Well))

#### Download groundwater level data ####
pgown_data <- lapply(well_list, function(wellname) {
  welltype <- "data"  # or "recent" (one year); "average" (daily, date format is different)
  read.csv(paste0(url, wellname, "-", welltype, ".csv"))
}) %>%
  bind_rows() %>%
  mutate(Date = as.Date(Time)) %>%
  dplyr::rename(Well = myLocation) %>%
  full_join(pgown_well_info) %>%
  mutate(Date = as.Date(Time)) %>%
  group_by(Well, Date,Snow_influenced) %>%
  summarise(groundwater = mean(Value, na.rm = TRUE), groundwater_min = max(Value, na.rm = TRUE)) %>%
  ungroup()

save(pgown_data,file = paste0(data_location, "pgown_data.RData"))

return(pgown_data)
}
