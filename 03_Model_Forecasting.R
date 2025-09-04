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
# Script name:      03_Model_forecasting.R
# ------------------------------------------------------------------------------
# Script version:
# 2025-04-01:       v1
#
# ------------------------------------------------------------------------------
# Notes/Objectives:
# main script for generating forecasts
#
# ==============================================================================
#

## Load inputs and functions ---------------------------------------------------

source("01_ConfigInputs.R")
source("functions/dl_climate_data.R")
source("functions/dl_pgown_wl_data.R")
source("functions/dl_snow_data.R")
source("functions/dl_deterministic_forecast.R")
source("functions/dl_ensemble_forecast.R")

## Create directories  ---------------------------------------------------------

output_path <- paste0(figure_location, "previous_forecasts/", as.character(Sys.Date()))
dir.create(paste0(figure_location, "/"), showWarnings = FALSE)
dir.create(output_path, showWarnings = FALSE)


## Loop through each region and run forecasting scripts  -----------------------

for (i in Regional_group_list) {

  # i <- Regional_group_list[6]

  pgown_well_info <- pgown_well_info_all %>%
    filter(Regional_group == i) #%>%
  #  filter(Well == "OW002")


  ## Downloads -----------------------------------------------------------------

  climate_data <- dl_climate_data(pgown_well_info, data_location)
  pgown_data <- dl_pgown_wl_data(pgown_well_info, data_location)
  snow_data <- dl_snow_data(pgown_well_info, data_location)
  ensemble_forecast_data <- dl_ensemble_forecast(pgown_well_info, data_location)
  deterministic_forecast_data <- dl_deterministic_forecast(pgown_well_info, data_location)

  # Merge timeseries data

  Time_series_data <- full_join(pgown_data, climate_data, by = c("Date", "Well")) %>%
    full_join(snow_data, by = c("Date", "Well")) %>%
    filter(Date >= as.Date("2004-01-01")) %>%
    pad(by = "Date", group = c("Well")) %>%
    group_by(Well) %>%
    fill(Snow_influenced, .direction = "downup") %>%
    mutate(SWE = ifelse(month(Date) >= 8 & month(Date) <= 10, ifelse(is.na(SWE), 0, SWE), SWE)) %>%
    fill(SWE, .direction = "downup") %>%
    mutate(groundwater_up_to_last_year = if_else(year(Date) <= year(Sys.Date()) - 1, groundwater, NA_real_)) %>%
    mutate(groundwater_period_average = mean(groundwater_up_to_last_year, na.rm = TRUE)) %>%
    mutate(groundwater = groundwater_period_average - groundwater) %>%
    dplyr::select(-groundwater_up_to_last_year) %>%
    ungroup() %>%
    distinct()

  #### MODEL -------------------------------------------------------------------

  source("functions/Forecasting.R")

  Model_Forecasting_data <- forecast_model(Time_series_data, forecast_days,
                                           num_cores, figure_location, output_path,
                                           model_path, data_location, pgown_well_info,
                                           rfc_forecast_include,
                                           ensemble_forecast_data, deterministic_forecast_data,
                                           Missing_date_window, rfc_forecast_date_window,
                                           generate_well_pdf)

  # Save the csv output
  write_csv(Model_Forecasting_data, paste0(output_path, "/predictive_forecast_results_", i, ".csv"))
  # write_csv(Model_Forecasting_data, paste0("Output//predictive_forecast_results_", i, ".csv"))

}



#### MAKE THIS INTO ANOTHER SCRIPT AND LINK TO THIS AT THE END
# Merge forecast files into one file

list_of_results <- list.files(path = output_path,
                              pattern = "predictive_forecast_results_",
                              full.names = TRUE)

combined <- bind_rows(
  lapply(list_of_results, function(forecast_file){
    read_csv(file = forecast_file) %>%
      mutate(Date_predicted = as.Date(Date_predicted))
  }))# %>%
# filter(!is.na(lag_day))

data_out <- combined %>%  # format and add some links ready for mapping/ArcGIS online
  left_join(pgown_well_info_all)

write.csv(data_out, paste0(output_path, "/predictive_forecast_results.csv"), row.names = FALSE)
write.csv(data_out, paste0("output/predictive_forecast_results.csv"), row.names = FALSE)


# Create table for csv output for ArcGIS online and leaflet mapping

# Get well tag information
wells_sf <- bcdata::bcdc_query_geodata("e4731a85-ffca-4112-8caf-cb0a96905778") %>%
  dplyr::filter(!is.na(.data$OBSERVATION_WELL_NUMBER)) %>%
  dplyr::collect() %>%
  dplyr::select(Well = OBSERVATION_WELL_NUMBER,
                Well_Tag_number = WELL_TAG_NUMBER) %>%
  dplyr::mutate(Well = paste0("OW", Well))

# merge and format data for output
data_table_out <- data_out %>%
  left_join(wells_sf, by = join_by(Well)) %>%
  mutate(Likelihood_Category = case_when(likelihood > 90 ~ ">90% - Extremely Likely",
                                         likelihood > 75 ~ "75-90% - Very Likely",
                                         likelihood > 50 ~ "50-75% - Likely",
                                         likelihood > 25 ~ "25-50% - Somewhat Likely",
                                         likelihood >= 10 ~ "10-25% - Unlikely",
                                         likelihood >= 0  ~ "<10% - Very Unlikely",
                                         is.na(likelihood) ~ "Not Available"),
         Likelihood_Category = factor(Likelihood_Category, levels = c(">90% - Extremely Likely",
                                                                      "75-90% - Very Likely",
                                                                      "50-75% - Likely",
                                                                      "25-50% - Somewhat Likely",
                                                                      "10-25% - Unlikely",
                                                                      "<10% - Very Unlikely",
                                                                      "Not Available"))) %>%
  select(Well, Location, Region, Latitude = latitude, Longitude = longitude,
         Aquifer_ID = aquifer_id, Aquifer_Subtype = subtype_sym,
         Forecast_Date, Latest_Date = Date, Latest_Conditions, Predicted_Date = Date_predicted,
         Forecast_Days = lag_day, Conditions = conditions,
         Likelihood = likelihood, Likelihood_Category,
         Forecast_Performance = performance, Snow_Influenced = Snow_influenced,
         Predicted_Min = predicted_value_min, Predicted_10th = predicted_value_10th,
         Predicted_25th = predicted_value_25th,
         Predicted_Mean = predicted_value_mean, Predicted_Median = predicted_value_50th,
         Predicted_75th = predicted_value_75th, Predicted_90th = predicted_value_90th,
         Predicted_Max = predicted_value_max,
         Well_Tag_number) %>%
  mutate(Hydrograph_URL = paste0("https://nrs.objectstore.gov.bc.ca/rfc-conditions/gw_forecasting/outputs/Well_",Well,"_Model_Predictions.pdf"),
         Realtime_URL = paste0("https://bcmoe-prod.aquaticinformatics.net/Data/Location/Summary/Location/",Well,"/Interval/Latest"),
         Aquifer_URL = ifelse(is.na(Aquifer_ID), "", paste0("https://apps.nrs.gov.bc.ca/gwells/aquifers/", Aquifer_ID)),
         Well_URL = ifelse(is.na(Well_Tag_number), "", paste0("https://apps.nrs.gov.bc.ca/gwells/well/", Well_Tag_number)),
         Issued_At = format(Sys.time(), "%Y-%m-%d %H:%M")) %>%
  select(-Well_Tag_number)

# Save outputs
write.csv(data_table_out, paste0(output_path, "/RFC_GW_Forecast.csv"), row.names = FALSE)
write.csv(data_table_out, paste0("output/RFC_GW_Forecast.csv"), row.names = FALSE)
#### MAYBE MAKE THE OUTPUT THE LIKELYHOOD OF NORMAL/BELOW/ABOVE IN ONE ROW RATHER THAN ALL SEPERATE

# THEN MAKE MAP

library(leaflet)
library(sf)

# format table for mapping
data_table_map <- data_table_out %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  mutate(Hydrograph_URL = paste0("<a href = '", Hydrograph_URL, "' target='_blank' > Hydrograph and Forecast </a>"),
         Realtime_URL = paste0("<a href = '", Realtime_URL, "' target='_blank' > Real-time Data </a>"),
         Aquifer_URL = paste0("<a href = '", Aquifer_URL, "' target='_blank' > Aquifer Summary </a>"),
         Well_URL = paste0("<a href = '", Well_URL, "' target='_blank' > Well Summary </a>")) %>%
  mutate(Likelihood_Label = case_when(Likelihood < 10 ~ "<10",
                                      Likelihood > 90 ~ ">90",
                                      TRUE ~ as.character(round(Likelihood)))) %>%
  mutate(Conditions = case_when(Conditions == "1) Above Normal - above 75th percentile" ~ "Above Normal",
                                Conditions == "2) Normal - 25th to 75th percentile" ~ "Normal",
                                Conditions == "3) Below Normal - below 25th percentile" ~ "Below Normal",
                                Conditions == "No forecast available (no recent data)" ~ "Not Available")) %>%
  # REMOVE ME
  mutate(Location = case_when(Well == "OW008" ~ "Abbotsford (Vye Rd E Of Mccallum Rd)",
                              TRUE ~ Location))


# table for each forecast
forecast_14d <- data_table_map %>%
  filter(Forecast_Days == 14 | is.na(Forecast_Days))
date_14d <- as.Date(max(unique(forecast_14d$Predicted_Date), na.rm = TRUE))
date_14d_range <- seq(min(forecast_14d$Predicted_Date, na.rm = TRUE), max(forecast_14d$Predicted_Date, na.rm = TRUE), by = "1 day")

forecast_30d <- data_table_map %>%
  filter(Forecast_Days == 30 | is.na(Forecast_Days))
date_30d <- as.Date(max(unique(forecast_30d$Predicted_Date), na.rm = TRUE))
date_30d_range <- seq(min(forecast_30d$Predicted_Date, na.rm = TRUE), max(forecast_30d$Predicted_Date, na.rm = TRUE), by = "1 day")

forecast_60d <- data_table_map %>%
  filter(Forecast_Days == 60 | is.na(Forecast_Days))
date_60d <- as.Date(max(unique(forecast_60d$Predicted_Date), na.rm = TRUE))
date_60d_range <- seq(min(forecast_60d$Predicted_Date, na.rm = TRUE), max(forecast_60d$Predicted_Date, na.rm = TRUE), by = "1 day")

forecast_90d <- data_table_map %>%
  filter(Forecast_Days == 90 | is.na(Forecast_Days))
date_90d <- as.Date(max(unique(forecast_90d$Predicted_Date), na.rm = TRUE))
date_90d_range <- seq(min(forecast_90d$Predicted_Date, na.rm = TRUE), max(forecast_90d$Predicted_Date, na.rm = TRUE), by = "1 day")

# mapping prep
cols_likelihood <- c("#F95D06", "#FFA500", "#FFEA00",
                     "#B5E48C", "#75C9B7", "#A7D8F0",
                     "gray80")
levs_likelihood <- c(">90% - Extremely Likely",
                     "75-90% - Very Likely",
                     "50-75% - Likely",
                     "25-50% - Somewhat Likely",
                     "10-25% - Unlikely",
                     "<10% - Very Unlikely",
                     "Not Available")
vals_likelihood <- factor(levs_likelihood, levels = levs_likelihood)
pal_likelihood <- colorFactor(palette = cols_likelihood,
                              levels = levs_likelihood)

bc_bound_line <- readRDS(file = "data/spatial/bcmaps_bcbound_line.rds")

# Map the data

gw_map <- leaflet::leaflet(options = leaflet::leafletOptions(attributionControl = FALSE,
                                                             zoomSnap = 0.5)) %>%
  leaflet::addTiles(group = "OpenStreetMap") %>%
  leaflet::addProviderTiles(leaflet::providers$Esri.NatGeoWorldMap, group = "NatGeoWorldMap (ESRI)") %>%
  leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "WorldImagery (ESRI)") %>%
  leaflet::addProviderTiles(leaflet::providers$Esri.WorldTopoMap, group = "Topographic (ESRI)") %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, group = "Positron (CartoDB)") %>%
  leaflet::addMapPane("points", zIndex = 430) %>%
  leaflet::addMapPane("polygons", zIndex = 420) %>%
  leaflet::addMapPane("bc", zIndex = 400)  %>%
  addPolylines(data = bc_bound_line,
               color = "black", weight = 1,
               group = "Province",
               options = pathOptions(pane = "bc")) %>%
  leaflet::addCircleMarkers(data = forecast_14d %>% filter(Conditions == "Below Normal"),
                            fillOpacity = 100, color = "black", radius = 6, weight = 1,
                            fillColor = ~pal_likelihood(Likelihood_Category),
                            group = "Below Normal - 14 days",
                            options = leaflet::pathOptions(pane = "points"),
                            label = ~paste0(Well, " (", Location, ") - ", "Likelihood Below Normal: ", Likelihood_Label, "%"),
                            popup = ~paste0(
                              "<b>", Well, " - ", Location ,"</b>",
                              "<br><b>Aquifer ID:</b> ", Aquifer_ID,
                              "<br><b>Aquifer Subtype:</b> ", Aquifer_Subtype,
                              "<br><br><b><u>Latest</b></u>",
                              "<br><b>Latest Date</b>: ", format(Latest_Date, "%b-%d"),
                              "<br><b>Latest Conditions</b>: ", Latest_Conditions,
                              "<br><br><b><u>14-Day Forecast</b></u>",
                              "<br><b>Predicted Date</b>: ", format(Predicted_Date, "%b-%d"),
                              "<br><b>Likelihood of Below Normal Conditions</b>: ", Likelihood_Label, "%",
                              "<br><b>Forecast Performance</b>: ", Forecast_Performance,
                              "<br>",
                              "<br>", Hydrograph_URL,
                              "<br>", Realtime_URL,
                              "<br>", Well_URL,
                              "<br>", Aquifer_URL)) %>%
  leaflet::addCircleMarkers(data = forecast_30d %>% filter(Conditions == "Below Normal"),
                            fillOpacity = 100, color = "black", radius = 6, weight = 1,
                            fillColor = ~pal_likelihood(Likelihood_Category),
                            group = "Below Normal - 30 days",
                            options = leaflet::pathOptions(pane = "points"),
                            label = ~paste0(Well, " (", Location, ") - ", "Likelihood Below Normal: ", Likelihood_Label, "%"),
                            popup = ~paste0(
                              "<b>", Well, " - ", Location ,"</b>",
                              "<br><b>Aquifer ID:</b> ", Aquifer_ID,
                              "<br><b>Aquifer Subtype:</b> ", Aquifer_Subtype,
                              "<br><br><b><u>Latest</b></u>",
                              "<br><b>Latest Date</b>: ", format(Latest_Date, "%b-%d"),
                              "<br><b>Latest Conditions</b>: ", Latest_Conditions,
                              "<br><br><b><u>30-Day Forecast</b></u>",
                              "<br><b>Predicted Date</b>: ", format(Predicted_Date, "%b-%d"),
                              "<br><b>Likelihood of Below Normal Conditions</b>: ", Likelihood_Label, "%",
                              "<br><b>Forecast Performance</b>: ", Forecast_Performance,
                              "<br>",
                              "<br>", Hydrograph_URL,
                              "<br>", Realtime_URL,
                              "<br>", Well_URL,
                              "<br>", Aquifer_URL)) %>%
  leaflet::addCircleMarkers(data = forecast_60d %>% filter(Conditions == "Below Normal"),
                            fillOpacity = 100, color = "black", radius = 6, weight = 1,
                            fillColor = ~pal_likelihood(Likelihood_Category),
                            group = "Below Normal - 60 days",
                            options = leaflet::pathOptions(pane = "points"),
                            label = ~paste0(Well, " (", Location, ") - ", "Likelihood Below Normal: ", Likelihood_Label, "%"),
                            popup = ~paste0(
                              "<b>", Well, " - ", Location ,"</b>",
                              "<br><b>Aquifer ID:</b> ", Aquifer_ID,
                              "<br><b>Aquifer Subtype:</b> ", Aquifer_Subtype,
                              "<br><br><b><u>Latest</b></u>",
                              "<br><b>Latest Date</b>: ", format(Latest_Date, "%b-%d"),
                              "<br><b>Latest Conditions</b>: ", Latest_Conditions,
                              "<br><br><b><u>60-Day Forecast</b></u>",
                              "<br><b>Predicted Date</b>: ", format(Predicted_Date, "%b-%d"),
                              "<br><b>Likelihood of Below Normal Conditions</b>: ", Likelihood_Label, "%",
                              "<br><b>Forecast Performance</b>: ", Forecast_Performance,
                              "<br>",
                              "<br>", Hydrograph_URL,
                              "<br>", Realtime_URL,
                              "<br>", Well_URL,
                              "<br>", Aquifer_URL)) %>%
  leaflet::addCircleMarkers(data = forecast_90d %>% filter(Conditions == "Below Normal"),
                            fillOpacity = 100, color = "black", radius = 6, weight = 1,
                            fillColor = ~pal_likelihood(Likelihood_Category),
                            group = "Below Normal - 90 days",
                            options = leaflet::pathOptions(pane = "points"),
                            label = ~paste0(Well, " (", Location, ") - ", "Likelihood Below Normal: ", Likelihood_Label, "%"),
                            popup = ~paste0(
                              "<b>", Well, " - ", Location ,"</b>",
                              "<br><b>Aquifer ID:</b> ", Aquifer_ID,
                              "<br><b>Aquifer Subtype:</b> ", Aquifer_Subtype,
                              "<br><br><b><u>Latest</b></u>",
                              "<br><b>Latest Date</b>: ", format(Latest_Date, "%b-%d"),
                              "<br><b>Latest Conditions</b>: ", Latest_Conditions,
                              "<br><br><b><u>90-Day Forecast</b></u>",
                              "<br><b>Predicted Date</b>: ", format(Predicted_Date, "%b-%d"),
                              "<br><b>Likelihood of Below Normal Conditions</b>: ", Likelihood_Label, "%",
                              "<br><b>Forecast Performance</b>: ", Forecast_Performance,
                              "<br>",
                              "<br>", Hydrograph_URL,
                              "<br>", Realtime_URL,
                              "<br>", Well_URL,
                              "<br>", Aquifer_URL)) %>%
  leaflet::addLegend("bottomleft",
                     pal = pal_likelihood,
                     values = vals_likelihood,
                     opacity = 1,
                     title = paste0("Likelihood of ",
                                    "<br>Below Normal",
                                    "<br>Groundwater Levels",
                                    "<br>in 14 days (", format(as.Date(date_14d), "%b %d"),")",
                                    "<br><br>Likelihood") ,
                     group = "Below Normal - 14 days") %>%
  leaflet::addLegend("bottomleft",
                     pal = pal_likelihood,
                     values = vals_likelihood,
                     opacity = 1,
                     title = paste0("Likelihood of ",
                                    "<br>Below Normal",
                                    "<br>Groundwater Levels",
                                    "<br>in 30 days (", format(as.Date(date_30d), "%b %d"),")",
                                    "<br><br>Likelihood") ,
                     group = "Below Normal - 30 days") %>%
  leaflet::addLegend("bottomleft",
                     pal = pal_likelihood,
                     values = vals_likelihood,
                     opacity = 1,
                     title = paste0("Likelihood of ",
                                    "<br>Below Normal",
                                    "<br>Groundwater Levels",
                                    "<br>in 60 days (", format(as.Date(date_60d), "%b %d"),")",
                                    "<br><br>Likelihood") ,
                     group = "Below Normal - 60 days") %>%
  leaflet::addLegend("bottomleft",
                     pal = pal_likelihood,
                     values = vals_likelihood,
                     opacity = 1,
                     title = paste0("Likelihood of ",
                                    "<br>Below Normal",
                                    "<br>Groundwater Levels",
                                    "<br>in 90 days (", format(as.Date(date_90d), "%b %d"),")",
                                    "<br><br>Likelihood") ,
                     group = "Below Normal - 90 days") %>%
  leaflet::addLayersControl(baseGroups = c("Topographic (ESRI)", "OpenStreetMap",
                                           "NatGeoWorldMap (ESRI)", "WorldImagery (ESRI)",
                                           "Positron (CartoDB)"),
                            overlayGroups = c("Below Normal - 14 days","Below Normal - 30 days",
                                              "Below Normal - 60 days","Below Normal - 90 days"),
                            options = leaflet::layersControlOptions(collapsed = TRUE))  %>%
  leaflet::hideGroup(group = c("Below Normal - 30 days", "Below Normal - 60 days","Below Normal - 90 days"))
gw_map



map_title_text <- paste0("<b><u>Groundwater Drought Forecasting - DEMONSTRATION PURPOSES ONLY</u></b>")

map_title_text_body <- paste0("This project is under development and should not be used for any decision making.
                              This map is for demonstration and internal purposes at this time.
                              Updated: <b>", format(Sys.time(), format = "%H:%M %a. %b. %d, %Y"), "</b>.")


headerCSS <- "
<style>
  .leaflet-container {
    position: relative;
  }
  #header {
    position: absolute;
    top: 0;
    width: 100%;
    background-color: rgba(35, 64, 117, 0.7); /* Change background color and transparency here */
    color: white;
    text-align: center;
    padding: 5px 50px 5px 50px; /* 10px top/bottom and 20px left/right */
    font-size: 14px;
    z-index: 1000; /* Ensure the header stays above other map elements */
    max-width: calc(100% - 0px); /* Limit width to 100% minus padding (20px on each side) */
    margin: 0 auto; /* Center the header horizontally */
    font-family: 'Calibri', sans-serif; /* Change font type here */
    border-bottom: 4px solid #e3a82b; /* Add thin line at the bottom with hex color */
  }
</style>
"

# HTML for the header
headerHTML <- paste0("
<div id='header'>
  <font size='4'>",map_title_text,"</font><br>
  ",map_title_text_body,"
</div>
")
gw_map <- gw_map  %>%
  htmlwidgets::prependContent(htmltools::HTML(headerCSS)) %>%
  htmlwidgets::prependContent(htmltools::HTML(headerHTML))


#gw_map

htmlwidgets::saveWidget(widget = gw_map,
                        file = paste0(output_path, "/Groundwater_Drought_Forecast_Map.html"),
                        selfcontained = TRUE,
                        title = "B.C. Groundwater Drought Forecast")
htmlwidgets::saveWidget(widget = gw_map,
                        file = paste0("output/Groundwater_Drought_Forecast_Map.html"),
                        selfcontained = TRUE,
                        title = "B.C. Groundwater Drought Forecast")

# remove building files
unlink(paste0(output_path, "/Groundwater_Drought_Forecast_Map_files"), recursive = TRUE)
unlink(paste0("output/Groundwater_Drought_Forecast_Map_files"), recursive = TRUE)


