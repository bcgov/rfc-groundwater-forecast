# ==============================================================================
# Script name:      04c_RFC_forecast_assigning.R
# ------------------------------------------------------------------------------
# Script version:   
# 2024-04-01:       v1
#
# ------------------------------------------------------------------------------
# Notes/Objectives:
# main script for identify which RFC forecast_stations to use for model inputs for individual wells. 
# 
# ==============================================================================
#

## Load inputs and functions ---------------------------------------------------


source("Groundwater Level Forecasting Toolkit Phase 2 V2.0/01_ConfigInputs.R")



pgown_well_info_all <- read_csv(paste0(user_input_location,"Climate_station_testing.csv")) 

Regional_group_list <- pgown_well_info_all %>%
  dplyr::select(Regional_group) %>%
  distinct(Regional_group) %>%
  dplyr::pull(Regional_group)

Regional_group_list <- as.list(Regional_group_list)


# Specify the path where you want to create the new folder
output_path <- paste0(figure_location,"Station_testing/",as.character(Sys.Date()))

# Create new folder
dir.create(output_path)


for(i in Regional_group_list){
  
  
  # Filter well information for the current regional group
  
  pgown_locations <- pgown_well_info_all %>%
    filter(Regional_group == i)
  
  
  
coordinates(pgown_locations) <- ~longitude+latitude
proj4string(pgown_locations) <- CRS("+proj=longlat +datum=WGS84")

pgown_well_info_sf <- st_as_sf(pgown_locations)

# Transform to EPSG:3857 (Web Mercator)
pgown_well_info_proj <- st_transform(pgown_well_info_sf, crs = CRS("+proj=merc +datum=WGS84"))



#snow_stations <- bcsnowdata::snow_auto_location() %>%
#  filter(STATUS == "Active")

filename = paste0(data_location,"/RF_forcast/forecast_station_coords.csv" )

forcast_stations <- read.csv(filename) %>%
  dplyr::rename(station_name = MD_ID)

coordinates(forcast_stations) <- ~LONGITUDE+LATITUDE
proj4string(forcast_stations) <- CRS("+proj=longlat +datum=WGS84")

forcast_stations <- st_as_sf(forcast_stations)


forcast_stations <- st_transform(forcast_stations, crs = CRS("+proj=merc +datum=WGS84"))

# Define the buffer distance in meters (100 km)

# Define the buffer distance in meters (100 km)
buffer_distance <- 100000

# Create a function to find stations within the buffer and calculate distances
find_stations_within_buffer <- function(well, stations, buffer_distance) {
  # Create a buffer around the well
  well_buffer <- st_buffer(well, dist = buffer_distance)
  
  # Find stations within the buffer
  stations_within_buffer <- st_intersection(stations, well_buffer)
  
  # Calculate distances between the well and the stations
  distances <- st_distance(well, stations_within_buffer)
  
  # Add the well ID and distances to the stations
  stations_within_buffer$Well <- well$Well
  stations_within_buffer$distance_m <- as.numeric(distances)
  
  return(stations_within_buffer)
}

# Apply the function to each well
results <- lapply(1:nrow(pgown_well_info_proj), function(i) {
  well <- pgown_well_info_proj[i, ]
  stations_within_buffer <- find_stations_within_buffer(well, forcast_stations, buffer_distance)
  return(stations_within_buffer)
})

# Combine results into a single data frame
combined_results <- do.call(rbind, results)
# Combine re
combined_results <- combined_results %>%
  group_by(Well) %>%
  arrange(distance_m) %>%
  slice_head(n = 1) %>%
  ungroup()

combined_results <- st_drop_geometry(combined_results)

combined_results <- combined_results %>%
  dplyr::rename(station_name_RF_forecast = station_name) %>%
  dplyr::select(station_name_RF_forecast, Well)


filename = paste0(output_path,"/List_RFC_station_",i,".csv")
write_csv(combined_results, filename )

}
  
  
  
  