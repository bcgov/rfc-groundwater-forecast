# ==============================================================================
# Script name:      04a_Climate_station_testing.R
# ------------------------------------------------------------------------------
# Script version:   
# 2024-04-01:       v1
#
# ------------------------------------------------------------------------------
# Notes/Objectives:
# main script for identify potential climate stations to use for model inputs for individual wells. 
# 
# ==============================================================================
#

## Load inputs and functions ---------------------------------------------------

source("Groundwater Level Forecasting Toolkit Phase 2 V2.0/01_ConfigInputs.R")
source("Groundwater Level Forecasting Toolkit Phase 2 V2.0/functions/dl_climate_data_testing.R")
source("Groundwater Level Forecasting Toolkit Phase 2 V2.0/functions/dl_pgown_wl_data.R")




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

  # Filter well information for the specific regional group
  
    pgown_well_info <- pgown_well_info_all %>%
     filter(Regional_group == i)
  
  # Download well data for the filtered well information
  
  pgown_data <- dl_pgown_wl_data(pgown_well_info, data_location)
  
  
  # Calculate statistics for the well data
pgown_data_stats <- pgown_data %>%
  mutate(Date = as.Date(Date))%>%
  group_by(Well) %>%
  summarise(Start_day = as_date(min(Date, na.rm = TRUE)), End_day = as_date(max(Date, na.rm = TRUE))) %>%
  ungroup()

# Adjust start day and calculate start and end years

pgown_data_stats <- pgown_data_stats %>%
  mutate(Start_day = if_else(Start_day <= as.Date("2004-01-01"), as.Date("2004-01-01"), Start_day)) %>%
  mutate(Start_year = year(Start_day), End_year = year(End_day))

# Join well information with the calculated statistics

pgown_well_info <- full_join(pgown_well_info,pgown_data_stats)
pgown_well_info_df <- pgown_well_info


# Filter and clean station list

station_list <- stations()
station_list <- station_list %>%
  filter(interval == "day") %>%
  filter(end >= 2004)%>%
  drop_na(lat,lon)

station_list_coordinates <- station_list


# Add climate coordinates to the station list

station_list <- station_list %>%
  mutate(climate_lon = lon, climate_lat = lat)

# Convert station list and well information to spatial objects

coordinates(station_list) <- ~lon+lat
proj4string(station_list) <- CRS("+proj=longlat +datum=WGS84")

coordinates(pgown_well_info) <- ~longitude+latitude
proj4string(pgown_well_info) <- CRS("+proj=longlat +datum=WGS84")


# Convert to spatial objects
pgown_well_info_sf <- st_as_sf(pgown_well_info)
station_list_sf <- st_as_sf(station_list)

# Transform to EPSG:3857 (Web Mercator)
pgown_well_info_proj <- st_transform(pgown_well_info_sf, crs = CRS("+proj=merc +datum=WGS84"))
station_list_proj <- st_transform(station_list_sf, crs = CRS("+proj=merc +datum=WGS84"))


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
  stations_within_buffer <- find_stations_within_buffer(well, station_list_proj, buffer_distance)
  return(stations_within_buffer)
})

# Combine results into a single dataframe
final_results <- do.call(rbind, results)

# Select and group data with ranges
final_results <- final_results %>%
  dplyr::select(station_name, station_id, start,end, climate_lon, climate_lat, Well, Regional_group, Region, Location, aquifer_id, Start_day, End_year, distance_m )


# Drop geometry and group by specific columns

final_results <- st_drop_geometry(final_results)


# groups stations by the same names

final_results2 <- final_results %>%
  group_by(station_name, climate_lon, climate_lat,Well, distance_m)%>%
  summarise(    station_id = paste(unique(station_id), collapse = ". "),
                start = min(start),
                end = max(end)
  )%>%
  ungroup()


# Filter and rank the results

final_results2 <- final_results2 %>%
  group_by(Well) %>%
  arrange(distance_m) %>%
  filter(end >= year(Sys.Date())-3)%>%
  slice_head(n = 10) %>%
  mutate(rank = row_number()) %>%
  ungroup() 

# Separate rows by station_id and Well

final_results3 <- final_results2 %>%
  separate_rows(station_id, Well, sep = ". ")

# Create a list of unique climate station IDs

climate_stations_id <- final_results3 %>%
dplyr::select(station_id) %>%
  distinct(station_id) %>%
  dplyr::pull(station_id)

climate_stations_id <- as.list(climate_stations_id)



# Download climate data for the selected stations
climate_dl <- lapply(climate_stations_id, function(Climate_station_Id) {
  weather_dl(station_ids = Climate_station_Id, interval = "day", start = "2004-01-01")
}) %>%
  bind_rows() 

# Select and pad climate records

climate_record <- climate_dl %>%
  dplyr::select(station_name, station_id, date, total_precip,mean_temp)

climate_record_padded <- climate_record %>%
  group_by(station_name, station_id) %>%
  pad(interval = "day") %>%
  ungroup()


# Summarize climate records

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



# Join climate records with station list

  final_results4 <- left_join(final_results3, climate_record_padded)

  final_results4 <- final_results4 %>%
    mutate(Region = i ) 
  
  # Save complete list of station options to CSV for PGOWN
  final_results4_2 <- final_results4 %>%
    dplyr::select(station_name,climate_lon,climate_lat, Well, station_id, start_date, end_date, temp_precentcomplete,precip_precentcomplete )
  
  
filename = paste0(output_path,"/Complete_climate_station_list_",i,".csv")
write_csv(final_results4_2, filename )

# Compile and summarize climate data


climate_data_testing_compiled_DT <-final_results4 %>%
  group_by(station_name, Well,Region, rank, climate_lon, climate_lat,distance_m)%>%
  summarise(station_id = paste(unique(station_id), collapse = ". "),
                start = min(start),
                end = max(end),
            start_date = min(start_date),
            end_date = max(end_date),
            count = sum(count), 
            temp_count = sum(temp_count), 
            precip_count = sum(precip_count)) %>%
  ungroup()%>%
  mutate(temp_precentcomplete = temp_count/count,
         precip_precentcomplete = precip_count/count)%>%
  mutate(number_years = year(end_date)- year(start_date)) %>%
  mutate(months = month(end_date), year_end = year(end_date)) 
  
            

# Identify primary station to be utilized. requirements includes completion of dataset > 80% and active station data. 

climate_data_testing_compiled_DT_count1 <- climate_data_testing_compiled_DT %>%
  group_by(Well,Region)%>%
  filter(temp_precentcomplete >= 0.80 & precip_precentcomplete >= 0.8)%>%
  filter(year_end == year(Sys.Date()))%>%
  #filter(months == month(Sys.Date()))%>%
  arrange(Well, distance_m, number_years) %>%
  slice_head(n = 1) %>%
  mutate(rank = row_number()) %>%
  ungroup() 


#remove primary station and rank infilling, filtering out stations with less the 70% completeless and then distance to primary

climate_data_testing_compiled_DT_count2 <- anti_join(climate_data_testing_compiled_DT, climate_data_testing_compiled_DT_count1, by = c("station_name", "Well", "Region"))

climate_data_testing_compiled_DT_count2 <- climate_data_testing_compiled_DT_count2%>%
  group_by(Well,Region)%>%
  filter(temp_precentcomplete >= 0.70 & precip_precentcomplete >= 0.7)%>%
  arrange(Well, distance_m, number_years) %>%
  slice_head(n = 4) %>%
  mutate(rank = 1+row_number()) %>%
  ungroup() 


# Combine and summarize the climate data

climate_data_testing_compiled_DT_comb <- bind_rows(climate_data_testing_compiled_DT_count1, climate_data_testing_compiled_DT_count2)


climate_data_testing_compiled_DT_comb2 <- climate_data_testing_compiled_DT_comb %>%
 mutate(rank_name = paste0("climate_infilled_",rank)) %>%
  dplyr::select(Well, Region, rank_name,station_id)%>%
  spread(rank_name, station_id)

# Save the suggested climate stations to CSV

filename = paste0(output_path,"/Suggested_climate_station_",i,".csv")
write_csv(climate_data_testing_compiled_DT_comb2, filename )


}





