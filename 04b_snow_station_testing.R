# Copyright 2025 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.


# ==============================================================================
# Script name:      04b_snow_station_testing.R
# ------------------------------------------------------------------------------
# Script version:
# 2024-04-01:       v1
#
# ------------------------------------------------------------------------------
# Notes/Objectives:
# main script for identify potential snow stations to use for model inputs for individual wells.
#
# ==============================================================================
#

## Load inputs and functions ---------------------------------------------------

source("01_ConfigInputs.R")
source("functions/dl_pgown_wl_data.R")
source("functions/dl_snow_data_testing.R")


## Create directories  ---------------------------------------------------------
figure_location <- "testing and calibration/"

output_path <- paste0(figure_location, "station testing/", as.character(Sys.Date()))
dir.create(paste0(figure_location, "station testing/"), showWarnings = FALSE)
dir.create(output_path, showWarnings = FALSE)


## Get data for testing---------------------------------------------------------

# Read the well information data
pgown_well_info_all <- read_csv(paste0(user_input_location, "testing and calibration/Climate_station_testing.csv"))

# Extract distinct regional groups from the well information data
Regional_group_list <- pgown_well_info_all %>%
  dplyr::select(Regional_group) %>%
  distinct(Regional_group) %>%
  dplyr::pull(Regional_group)

Regional_group_list <- as.list(Regional_group_list)


## Loop through each region and run testing scripts  ---------------------------

for (i in Regional_group_list) {


  # Filter well information for the current regional group

  pgown_well_info <- pgown_well_info_all %>%
    filter(Regional_group == i)

  # Download well data for the filtered wells

  pgown_data <- dl_pgown_wl_data(pgown_well_info, data_location)


  # Calculate start and end dates for each well

  pgown_data_stats <- pgown_data %>%
    mutate(Date = as.Date(Date)) %>%
    group_by(Well) %>%
    summarise(Start_day = as_date(min(Date, na.rm = TRUE)),
              End_day = as_date(max(Date, na.rm = TRUE))) %>%
    ungroup()

  # Adjust start dates and add year columns

  pgown_data_stats <- pgown_data_stats %>%
    mutate(Start_day = if_else(Start_day <= as.Date("2004-01-01"),
                               as.Date("2004-01-01"),
                               Start_day)) %>%
    mutate(Start_year = year(Start_day), End_year = year(End_day))


  # Join well information with calculated statistics

  pgown_well_info <- full_join(pgown_well_info,pgown_data_stats)
  pgown_well_info_df <- pgown_well_info

  # Convert well information to spatial objects

  coordinates(pgown_well_info) <- ~longitude+latitude
  proj4string(pgown_well_info) <- CRS("+proj=longlat +datum=WGS84")


  pgown_well_info_sf <- st_as_sf(pgown_well_info)

  # Transform to EPSG:3857 (Web Mercator)
  pgown_well_info_proj <- st_transform(pgown_well_info_sf,
                                       crs = CRS("+proj=merc +datum=WGS84"))

  # Download snow station data and filter for active stations


  snow_stations <- bcsnowdata::snow_auto_location() %>%
    filter(STATUS == "Active")


  snow_stations <- st_drop_geometry(snow_stations)

  snow_stations <- as.data.frame(snow_stations)

  coordinates(snow_stations) <- ~LONGITUDE+LATITUDE
  proj4string(snow_stations) <- CRS("+proj=longlat +datum=WGS84")



  snow_stations <- st_as_sf(snow_stations)


  snow_stations <- st_transform(snow_stations, crs = CRS("+proj=merc +datum=WGS84"))

  # Define the buffer distance in meters (500 km)
  buffer_distance <- 500000

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
    stations_within_buffer <- find_stations_within_buffer(well, snow_stations,
                                                          buffer_distance)
    return(stations_within_buffer)
  })


  # Combine results into a single dataframe
  final_results <- do.call(rbind, results)

  # Extract unique snow station IDs

  final_results_2 <-final_results %>%
    dplyr::select(LOCATION_ID,distance_m,Well)


  snow_station_list <- final_results_2 %>%
    dplyr::select(LOCATION_ID) %>%
    distinct(LOCATION_ID) %>%
    dplyr::pull(LOCATION_ID)


  snow_station_list <- as.list(snow_station_list)


  # Download snow data for the identified stations
  snow_data_raw <- lapply(snow_station_list, function(stn) {
    get_aswe_databc(station_id = stn, get_year = "All",
                    parameter = "swe", timestep = "daily")
  }) %>%
    bind_rows() %>%
    mutate(date = as.Date(date_utc))

  # Calculate date ranges and data completeness for snow data

  snow_date_ranges_dates <- snow_data_raw %>%
    dplyr::select(-date_utc)%>%
    mutate(date = as.Date(date)) %>%  # Ensure date is Date class
    group_by(id) %>%
    mutate(count = 1) %>%
    pad( interval = "day") %>%
    mutate(count2 = 1) %>%
    filter(date >= as.Date("2004-01-01")) %>%
    summarise(start_date_SWE = min(date, na.rm = TRUE),
              end_date_SWE = max(date, na.rm = TRUE),
              count = sum(count, na.rm = TRUE),
              count2 = sum(count2, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(per_data_complete = count/count2)%>%
    mutate(start_year_SWE = year(start_date_SWE),
           end_year_SWE = year(end_date_SWE)) %>%
    dplyr::rename("LOCATION_ID" = id) %>%
    dplyr::select(-count, -count2)

  # Join snow data ranges with well information

  snow_date_ranges_dates <- left_join(snow_date_ranges_dates, final_results_2)

  snow_date_ranges_dates <- st_drop_geometry(snow_date_ranges_dates)

  pgown_well_info_df2 <- pgown_well_info_df %>%
    dplyr::select(Well,Start_year, End_year)

  snow_date_ranges_dates <- left_join(snow_date_ranges_dates,
                                      pgown_well_info_df2)


  # Filter and rank snow stations based on data completeness and distance


  snow_date_ranges_dates <- snow_date_ranges_dates %>%
    filter(start_year_SWE <= Start_year) %>%
    group_by(Well) %>%
    arrange(distance_m) %>%
    filter(per_data_complete >= 0.8)%>%
    slice_head(n = 5) %>%
    mutate(rank = row_number()) %>%
    ungroup()


  # Extract well and snow station pairs


  snow_date_ranges_dates2 <- snow_date_ranges_dates %>%
    dplyr::select(Well, LOCATION_ID)

  Well_list <- snow_date_ranges_dates2 %>%
    dplyr::select(Well) %>%
    distinct(Well) %>%
    dplyr::pull(Well)


  Well_list <- as.list(Well_list)

  ccf_data <- data.frame()
  # Loop through each well and calculate cross-correlation with snow data


  for(x in Well_list){

    temp_list <- snow_date_ranges_dates2 %>%
      filter(Well == x)%>%
      dplyr::select(LOCATION_ID) %>%
      distinct(LOCATION_ID) %>%
      dplyr::pull(LOCATION_ID)

    temp_list <- as.list(temp_list)

    #pgown_data

    temp_pgown <- pgown_data %>%
      filter(Well == x)

    for(y in temp_list){
      #y = "4A02P"
      temp_snow <- snow_data_raw %>%
        filter(id == y) %>%
        dplyr::rename("Date" = "date", "SWE" = "value")%>%
        dplyr::select(Date, SWE) %>%
        mutate(year = year(Date))%>%
        group_by(year)%>%
        mutate(max_SWE = max(SWE))%>%
        ungroup()%>%
        mutate(max_SWE = mean(max_SWE)) %>%
        mutate(norm_SWE = SWE/max_SWE) %>%
        mutate(SWE_diff = norm_SWE - lag(norm_SWE, 1))%>%
        dplyr::select(Date, SWE_diff)




      temp_comb <- left_join(temp_pgown, temp_snow)

      temp_comb <- temp_comb %>%
        filter(Date >= "2004-01-01") %>%
        drop_na(groundwater, SWE_diff)






      SWE_ts <- xts(temp_comb$SWE_diff, order.by = temp_comb$Date)
      # precipitation_ts
      water_level_ts <- xts(temp_comb$groundwater, order.by = temp_comb$Date)

      SWE_ts <- as.ts(SWE_ts)
      water_level_ts <- as.ts(water_level_ts)

      # Use the cross-correlation function
      ccf_result <- ccf(SWE_ts, water_level_ts, lag.max = 180, plot = TRUE)

      ccf_df <- as.data.frame(ccf_result$acf)
      ccf_df$Lag <- ccf_result$lag
      ccf_df <- ccf_df %>%
        mutate(Well = x,snow_station = y)

      ccf_data_stats <- ccf_df %>%
        mutate(Lag = -1 *Lag,
               Lag = ifelse(Lag <0, 1, Lag)) %>%
        group_by(Well,snow_station) %>%
        mutate(max_acf = max(V1)) %>%
        filter(V1 == max_acf) %>%
        dplyr::select(-V1)



      # Find the lag with the highest absolute correlation
      ccf_data <- rbind(ccf_data, ccf_data_stats)

    }
  }


  # Select the best cross-correlation data for each well

  ccf_data_to_use <- ccf_data %>%
    group_by(Well)%>%
    arrange(Well,-max_acf)%>%
    ungroup() %>%
    dplyr::rename("CCF_value" = max_acf,
                  "LOCATION_ID" = snow_station) %>%
    dplyr::select(Well, LOCATION_ID, CCF_value)


  # Join the cross-correlation data with snow date ranges

  snow_date_ranges_dates2 <- full_join(snow_date_ranges_dates, ccf_data_to_use)

  # Select relevant columns for the final output


  snow_date_ranges_dates2 <- snow_date_ranges_dates2 %>%
    dplyr::select(Well, LOCATION_ID, start_date_SWE, end_date_SWE,
                  distance_m, per_data_complete, rank)
  # Write the final data to a CSV file

  filename = paste0(output_path, "/List_Snow_station_", i, ".csv")
  write_csv(snow_date_ranges_dates2, filename)

}





