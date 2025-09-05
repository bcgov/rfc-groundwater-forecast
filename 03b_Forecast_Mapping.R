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
# Script name:      03b_Forecast_Mapping.R
# ------------------------------------------------------------------------------
# Script version:
# 2025-09-04:       v1 - JG
#
# ------------------------------------------------------------------------------
# Notes/Objectives:
# generate leaflet maps
#
# ==============================================================================
#


library(leaflet)
library(sf)

# format table for mapping
data_table_map <- data_table_out %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  mutate(Hydrograph_URL = paste0("<a href = '", Hydrograph_URL, "' target='_blank' > Hydrograph and Forecast </a>"),
         Technical_Hydrograph_URL = paste0("<a href = '", Technical_Hydrograph_URL, "' target='_blank' > Technical Hydrograph and Forecast </a>"),
         Realtime_URL = paste0("<a href = '", Realtime_URL, "' target='_blank' > Real-time Data </a>"),
         Aquifer_URL = paste0("<a href = '", Aquifer_URL, "' target='_blank' > Aquifer Summary </a>"),
         Well_URL = paste0("<a href = '", Well_URL, "' target='_blank' > Well Summary </a>")) %>%
  mutate(Likelihood_Label = case_when(Likelihood < 10 ~ "<10",
                                      Likelihood > 90 ~ ">90",
                                      TRUE ~ as.character(round(Likelihood)))) %>%
  mutate(Conditions = case_when(Conditions == "1) Above Normal - above 75th percentile" ~ "Above Normal",
                                Conditions == "2) Normal - 25th to 75th percentile" ~ "Normal",
                                Conditions == "3) Below Normal - below 25th percentile" ~ "Below Normal",
                                Conditions == "No forecast available (no recent data)" ~ "Not Available"))

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
                              "<br>", Technical_Hydrograph_URL,
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
                              "<br>", Technical_Hydrograph_URL,
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
                              "<br>", Technical_Hydrograph_URL,
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
                              "<br>", Technical_Hydrograph_URL,
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



map_title_text <- paste0("<b><u>Groundwater Level Drought Forecast</u></b>")

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


