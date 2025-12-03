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


message("Mapping forecasts...")

library(leaflet)
library(sf)
library(htmltools)
library(gt)


# format table for mapping
data_table_map <- data_table_out %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  mutate(Forecast_URL = paste0("<a href = '", Forecast_URL, "' target='_blank' > Forecast Hydrograph </a>"),
         Technical_Forecast_URL = paste0("<a href = '", Technical_Forecast_URL, "' target='_blank' > Technical Forecast Hydrograph </a>"),
         Realtime_URL = paste0("<a href = '", Realtime_URL, "' target='_blank' > Real-time Data </a>"),
         Aquifer_URL = paste0("<a href = '", Aquifer_URL, "' target='_blank' > Aquifer Summary </a>"),
         Well_URL = paste0("<a href = '", Well_URL, "' target='_blank' > Well Summary </a>"),
         Interactive_Hydrograph_URL = paste0("<a href = '", Interactive_Hydrograph_URL, "' target='_blank' > Interactive Hydrograph </a>"),
         Static_Hydrograph_URL = paste0("<a href = '", Static_Hydrograph_URL, "' target='_blank' > Hydrograph and Historic Record</a>")) %>%
  mutate(Likelihood_Label = case_when(Likelihood < 10 ~ "<10",
                                      Likelihood > 90 ~ ">90",
                                      TRUE ~ as.character(round(Likelihood)))) %>%
  mutate(Conditions = case_when(Conditions == "1) Above Normal - above 75th percentile" ~ "Above Normal",
                                Conditions == "2) Normal - 25th to 75th percentile" ~ "Normal",
                                Conditions == "3) Below Normal - below 25th percentile" ~ "Below Normal",
                                Conditions == "No forecast available (no recent data)" ~ "Not Available"))


## Map the data


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

well_likely_conditions_map <- well_likely_conditions %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  mutate(Forecast_URL = paste0("<a href = '", Forecast_URL, "' target='_blank' > Forecast Hydrograph </a>"),
         Technical_Forecast_URL = paste0("<a href = '", Technical_Forecast_URL, "' target='_blank' > Technical Forecast Hydrograph </a>"),
         Realtime_URL = paste0("<a href = '", Realtime_URL, "' target='_blank' > Real-time Data </a>"),
         Aquifer_URL = paste0("<a href = '", Aquifer_URL, "' target='_blank' > Aquifer Summary </a>"),
         Well_URL = paste0("<a href = '", Well_URL, "' target='_blank' > Well Summary </a>"),
         Interactive_Hydrograph_URL = paste0("<a href = '", Interactive_Hydrograph_URL, "' target='_blank' > Interactive Hydrograph </a>"),
         Static_Hydrograph_URL = paste0("<a href = '", Static_Hydrograph_URL, "' target='_blank' > Hydrograph and Historic Record</a>"))

# Single conditions

forecast_conditions_14d <- well_likely_conditions_map %>%
  filter(Forecast_Days == 14 | is.na(Forecast_Days)) %>%
  mutate(Point_size = case_when(is.na(Forecast_Days) ~ 3,
                                TRUE ~ 6))

forecast_conditions_30d <- well_likely_conditions_map %>%
  filter(Forecast_Days == 30 | is.na(Forecast_Days)) %>%
  mutate(Point_size = case_when(is.na(Forecast_Days) ~ 3,
                                TRUE ~ 6))

forecast_conditions_60d <- well_likely_conditions_map %>%
  filter(Forecast_Days == 60 | is.na(Forecast_Days)) %>%
  mutate(Point_size = case_when(is.na(Forecast_Days) ~ 3,
                                TRUE ~ 6))

forecast_conditions_90d <- well_likely_conditions_map %>%
  filter(Forecast_Days == 90 | is.na(Forecast_Days)) %>%
  mutate(Point_size = case_when(is.na(Forecast_Days) ~ 3,
                                TRUE ~ 6))


# mapping prep
cols_likelihood_conditions <- c("#0072B2", "#56B4E9", "#009E73",
                                "#F0E442", "#D55E00", "tan",
                                "gray80")
levs_likelihood_conditions <- c("Above Normal",
                                "Above Normal to Normal",
                                "Normal",
                                "Normal to Below Normal",
                                "Below Normal",
                                "Uncertain",
                                "Not Available")
vals_likelihood_conditions <- factor(levs_likelihood_conditions, levels = levs_likelihood_conditions)
pal_likelihood_conditions <- colorFactor(palette = cols_likelihood_conditions,
                                         levels = levs_likelihood_conditions)



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
                              "<br>", Forecast_URL,
                              #"<br>", Technical_Forecast_URL,
                              "<br>", Realtime_URL,
                              "<br>", Well_URL,
                              "<br>", Aquifer_URL,
                              "<br>", Interactive_Hydrograph_URL)) %>%
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
                              "<br>", Forecast_URL,
                              # "<br>", Technical_Forecast_URL,
                              "<br>", Realtime_URL,
                              "<br>", Well_URL,
                              "<br>", Aquifer_URL,
                              "<br>", Interactive_Hydrograph_URL,
                              "<br>", Static_Hydrograph_URL)) %>%
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
                              "<br>", Forecast_URL,
                              # "<br>", Technical_Forecast_URL,
                              "<br>", Realtime_URL,
                              "<br>", Well_URL,
                              "<br>", Aquifer_URL,
                              "<br>", Interactive_Hydrograph_URL,
                              "<br>", Static_Hydrograph_URL)) %>%
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
                              "<br>", Forecast_URL,
                              # "<br>", Technical_Forecast_URL,
                              "<br>", Realtime_URL,
                              "<br>", Well_URL,
                              "<br>", Aquifer_URL,
                              "<br>", Interactive_Hydrograph_URL,
                              "<br>", Static_Hydrograph_URL)) %>%
  leaflet::addLegend("bottomleft",
                     pal = pal_likelihood,
                     values = vals_likelihood,
                     opacity = 1,
                     title = paste0("Likelihood of Groundwater",
                                    "<br>Levels Below the 25th",
                                    "<br>Percentile (Below Normal)",
                                    "<br>in 14 days (", format(as.Date(date_14d), "%b %d"),")",
                                    "<br><br>Likelihood") ,
                     group = "Below Normal - 14 days") %>%
  leaflet::addLegend("bottomleft",
                     pal = pal_likelihood,
                     values = vals_likelihood,
                     opacity = 1,
                     title = paste0("Likelihood of Groundwater",
                                    "<br>Levels Below the 25th",
                                    "<br>Percentile (Below Normal)",
                                    "<br>in 30 days (", format(as.Date(date_30d), "%b %d"),")",
                                    "<br><br>Likelihood") ,
                     group = "Below Normal - 30 days") %>%
  leaflet::addLegend("bottomleft",
                     pal = pal_likelihood,
                     values = vals_likelihood,
                     opacity = 1,
                     title = paste0("Likelihood of Groundwater",
                                    "<br>Levels Below the 25th",
                                    "<br>Percentile (Below Normal)",
                                    "<br>in 60 days (", format(as.Date(date_60d), "%b %d"),")",
                                    "<br><br>Likelihood") ,
                     group = "Below Normal - 60 days") %>%
  leaflet::addLegend("bottomleft",
                     pal = pal_likelihood,
                     values = vals_likelihood,
                     opacity = 1,
                     title = paste0("Likelihood of Groundwater",
                                    "<br>Levels Below the 25th",
                                    "<br>Percentile (Below Normal)",
                                    "<br>in 90 days (", format(as.Date(date_90d), "%b %d"),")",
                                    "<br><br>Likelihood") ,
                     group = "Below Normal - 90 days") %>%
  leaflet::addLayersControl(baseGroups = c("Topographic (ESRI)", "OpenStreetMap",
                                           "NatGeoWorldMap (ESRI)", "WorldImagery (ESRI)",
                                           "Positron (CartoDB)"),
                            overlayGroups = c("Below Normal - 14 days","Below Normal - 30 days",
                                              "Below Normal - 60 days","Below Normal - 90 days"),
                            options = leaflet::layersControlOptions(collapsed = TRUE)) %>%
  # single conditions
  leaflet::addCircleMarkers(data = forecast_conditions_14d,
                            fillOpacity = 100, color = "black", radius = ~Point_size, weight = 1,
                            fillColor = ~pal_likelihood_conditions(Likely_Conditions),
                            group = "Likely Conditions - 14 days",
                            options = leaflet::pathOptions(pane = "points"),
                            label = ~paste0(Well, " (", Location, ") - ", "Likely Conditions: ", Likely_Conditions),
                            popup = ~paste0(
                              "<b>", Well, " - ", Location ,"</b>",
                              "<br><b>Aquifer ID:</b> ", Aquifer_ID,
                              "<br><b>Aquifer Subtype:</b> ", Aquifer_Subtype,
                              "<br><br><b><u>Latest</b></u>",
                              "<br><b>Latest Date</b>: ", format(Latest_Date, "%b-%d"),
                              "<br><b>Latest Conditions</b>: ", Latest_Conditions,
                              "<br><br><b><u>14-Day Forecast</b></u>",
                              "<br><b>Predicted Date</b>: ", format(Predicted_Date, "%b-%d"),
                              "<br><b>Likely Conditions</b>: ", Likely_Conditions,
                              "<br><b>Forecast Performance</b>: ", Forecast_Performance,
                              "<br>",
                              "<br>", Forecast_URL,
                              "<br>", Technical_Forecast_URL,
                              "<br>", Realtime_URL,
                              "<br>", Well_URL,
                              "<br>", Aquifer_URL,
                              "<br>", Interactive_Hydrograph_URL,
                              "<br>", Static_Hydrograph_URL)) %>%
  leaflet::addLegend("bottomleft",
                     pal = pal_likelihood_conditions,
                     values = vals_likelihood_conditions,
                     opacity = 1,
                     title = paste0("Likely Groundwater",
                                    "<br>Level Conditions",
                                    "<br>in 14 days (", format(as.Date(date_14d), "%b %d"),")",
                                    "<br><br>Likely Conditions") ,
                     group = "Likely Conditions - 14 days") %>%
  leaflet::addCircleMarkers(data = forecast_conditions_30d,
                            fillOpacity = 100, color = "black", radius = ~Point_size, weight = 1,
                            fillColor = ~pal_likelihood_conditions(Likely_Conditions),
                            group = "Likely Conditions - 30 days",
                            options = leaflet::pathOptions(pane = "points"),
                            label = ~paste0(Well, " (", Location, ") - ", "Likely Conditions: ", Likely_Conditions),
                            popup = ~paste0(
                              "<b>", Well, " - ", Location ,"</b>",
                              "<br><b>Aquifer ID:</b> ", Aquifer_ID,
                              "<br><b>Aquifer Subtype:</b> ", Aquifer_Subtype,
                              "<br><br><b><u>Latest</b></u>",
                              "<br><b>Latest Date</b>: ", format(Latest_Date, "%b-%d"),
                              "<br><b>Latest Conditions</b>: ", Latest_Conditions,
                              "<br><br><b><u>30-Day Forecast</b></u>",
                              "<br><b>Predicted Date</b>: ", format(Predicted_Date, "%b-%d"),
                              "<br><b>Likely Conditions</b>: ", Likely_Conditions,
                              "<br><b>Forecast Performance</b>: ", Forecast_Performance,
                              "<br>",
                              "<br>", Forecast_URL,
                              "<br>", Technical_Forecast_URL,
                              "<br>", Realtime_URL,
                              "<br>", Well_URL,
                              "<br>", Aquifer_URL,
                              "<br>", Interactive_Hydrograph_URL,
                              "<br>", Static_Hydrograph_URL)) %>%
  leaflet::addLegend("bottomleft",
                     pal = pal_likelihood_conditions,
                     values = vals_likelihood_conditions,
                     opacity = 1,
                     title = paste0("Likely Groundwater",
                                    "<br>Level Conditions",
                                    "<br>in 30 days (", format(as.Date(date_30d), "%b %d"),")",
                                    "<br><br>Likely Conditions") ,
                     group = "Likely Conditions - 30 days") %>%
  leaflet::addCircleMarkers(data = forecast_conditions_60d,
                            fillOpacity = 100, color = "black", radius = ~Point_size, weight = 1,
                            fillColor = ~pal_likelihood_conditions(Likely_Conditions),
                            group = "Likely Conditions - 60 days",
                            options = leaflet::pathOptions(pane = "points"),
                            label = ~paste0(Well, " (", Location, ") - ", "Likely Conditions: ", Likely_Conditions),
                            popup = ~paste0(
                              "<b>", Well, " - ", Location ,"</b>",
                              "<br><b>Aquifer ID:</b> ", Aquifer_ID,
                              "<br><b>Aquifer Subtype:</b> ", Aquifer_Subtype,
                              "<br><br><b><u>Latest</b></u>",
                              "<br><b>Latest Date</b>: ", format(Latest_Date, "%b-%d"),
                              "<br><b>Latest Conditions</b>: ", Latest_Conditions,
                              "<br><br><b><u>60-Day Forecast</b></u>",
                              "<br><b>Predicted Date</b>: ", format(Predicted_Date, "%b-%d"),
                              "<br><b>Likely Conditions</b>: ", Likely_Conditions,
                              "<br><b>Forecast Performance</b>: ", Forecast_Performance,
                              "<br>",
                              "<br>", Forecast_URL,
                              "<br>", Technical_Forecast_URL,
                              "<br>", Realtime_URL,
                              "<br>", Well_URL,
                              "<br>", Aquifer_URL,
                              "<br>", Interactive_Hydrograph_URL,
                              "<br>", Static_Hydrograph_URL)) %>%
  leaflet::addLegend("bottomleft",
                     pal = pal_likelihood_conditions,
                     values = vals_likelihood_conditions,
                     opacity = 1,
                     title = paste0("Likely Groundwater",
                                    "<br>Level Conditions",
                                    "<br>in 60 days (", format(as.Date(date_60d), "%b %d"),")",
                                    "<br><br>Likely Conditions") ,
                     group = "Likely Conditions - 60 days") %>%
  leaflet::addCircleMarkers(data = forecast_conditions_90d,
                            fillOpacity = 100, color = "black", radius = ~Point_size, weight = 1,
                            fillColor = ~pal_likelihood_conditions(Likely_Conditions),
                            group = "Likely Conditions - 90 days",
                            options = leaflet::pathOptions(pane = "points"),
                            label = ~paste0(Well, " (", Location, ") - ", "Likely Conditions: ", Likely_Conditions),
                            popup = ~paste0(
                              "<b>", Well, " - ", Location ,"</b>",
                              "<br><b>Aquifer ID:</b> ", Aquifer_ID,
                              "<br><b>Aquifer Subtype:</b> ", Aquifer_Subtype,
                              "<br><br><b><u>Latest</b></u>",
                              "<br><b>Latest Date</b>: ", format(Latest_Date, "%b-%d"),
                              "<br><b>Latest Conditions</b>: ", Latest_Conditions,
                              "<br><br><b><u>90-Day Forecast</b></u>",
                              "<br><b>Predicted Date</b>: ", format(Predicted_Date, "%b-%d"),
                              "<br><b>Likely Conditions</b>: ", Likely_Conditions,
                              "<br><b>Forecast Performance</b>: ", Forecast_Performance,
                              "<br>",
                              "<br>", Forecast_URL,
                              "<br>", Technical_Forecast_URL,
                              "<br>", Realtime_URL,
                              "<br>", Well_URL,
                              "<br>", Aquifer_URL,
                              "<br>", Interactive_Hydrograph_URL,
                              "<br>", Static_Hydrograph_URL)) %>%
  leaflet::addLegend("bottomleft",
                     pal = pal_likelihood_conditions,
                     values = vals_likelihood_conditions,
                     opacity = 1,
                     title = paste0("Likely Groundwater",
                                    "<br>Level Conditions",
                                    "<br>in 90 days (", format(as.Date(date_90d), "%b %d"),")",
                                    "<br><br>Likely Conditions") ,
                     group = "Likely Conditions - 90 days") %>%
  leaflet::addLayersControl(baseGroups = c("Topographic (ESRI)", "OpenStreetMap",
                                           "NatGeoWorldMap (ESRI)", "WorldImagery (ESRI)",
                                           "Positron (CartoDB)"),
                            overlayGroups = c("Below Normal - 14 days","Below Normal - 30 days",
                                              "Below Normal - 60 days","Below Normal - 90 days",
                                              "Likely Conditions - 14 days",
                                              "Likely Conditions - 30 days",
                                              "Likely Conditions - 60 days",
                                              "Likely Conditions - 90 days"),
                            options = leaflet::layersControlOptions(collapsed = TRUE))  %>%
  leaflet::hideGroup(group = c("Below Normal - 30 days", "Below Normal - 60 days","Below Normal - 90 days",
                               "Likely Conditions - 14 days",
                               "Likely Conditions - 30 days",
                               "Likely Conditions - 60 days",
                               "Likely Conditions - 90 days"))
gw_map



# Make the GT table

forecast_date <- max(data_table_map$Forecast_Date, na.rm = TRUE)

data_table_table <- data_table_map %>%
  st_drop_geometry() %>%
  filter(Conditions == "Below Normal") %>%
  group_by(Forecast_Days) %>%
  mutate(Forecast_Date = paste0(Forecast_Days, " days (", format(max(Predicted_Date),"%b-%d"), ")")) %>%
  ungroup() %>%
  mutate(Aquifer = paste0("<a href = https://apps.nrs.gov.bc.ca/gwells/aquifers/", Aquifer_ID," target='_blank'>", Aquifer_ID," </a>"),
         Aquifer = purrr::map(Aquifer, gt::html),
         Location = paste0("<a href = https://nrs.objectstore.gov.bc.ca/rfc-conditions/groundwater_forecast/outputs/", Well, "_Model_Forecast.pdf target='_blank' >", Location," </a>"),
         Location = purrr::map(Location, gt::html),
         Well = paste0("<a href = https://nrs.objectstore.gov.bc.ca/rfc-conditions/groundwater_forecast/outputs/", Well, "_Model_Forecast.pdf target='_blank' >", Well," </a>"),
         Well = purrr::map(Well, gt::html)) %>%
  ungroup() %>%
  select(Well, Region, Location, Latest = Latest_Conditions,
         Forecast_Date, Likelihood_Category) %>%
  pivot_wider(names_from = Forecast_Date, values_from = Likelihood_Category)
names(data_table_table)[names(data_table_table)=="Latest"] <- paste0("Latest (", format(max(data_table_map$Latest_Date, na.rm = TRUE),"%b-%d"), ")")


gw_table <- gt(data_table_table %>%
  group_by(Region)) %>%
  cols_align(align = "left",
    columns = 3) %>%
  cols_align(align = "center",
             columns = 4:7) %>%
  tab_style(style = cell_fill(color = "lightgreen"),
    locations = cells_body(
      columns = 4,
      rows = data_table_table[[4]] == "Normal")) %>%
  tab_style(style = cell_fill(color = "lightblue"),
            locations = cells_body(
              columns = 4,
              rows = data_table_table[[4]] == "Above Normal")) %>%
  tab_style(style = cell_fill(color = "lightpink"),
            locations = cells_body(
              columns = 4,
              rows = data_table_table[[4]] == "Below Normal")) %>%
  gt::tab_style(style = gt::cell_text(weight = "bold", size = px(16)),
                locations = gt::cells_column_labels()) %>%
  gt::tab_style(style = gt::cell_text(weight = "bold", size = px(15)),
                locations = gt::cells_column_spanners()) %>%
  gt::tab_style(style = list(gt::cell_text(weight = "bold", size = px(15), align = "center"),
                             cell_fill(color = "gray95")),
                locations = gt::cells_row_groups()) %>%
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "#767676",
      weight = px(2),
      style = "solid"
    ),
    locations = cells_body(columns = 3:4)
  ) %>%
  tab_style(
    style = cell_text(size = px(14)),  # adjust size as needed
    locations = cells_body()
  )


# Add colours to the table
for (i in 5:8) {
  for (j in seq_along(levs_likelihood)) {
    gw_table <- gw_table %>%
      tab_style(
        style = cell_fill(color = cols_likelihood[j]),
        locations = cells_body(
          columns = i,
          rows = data_table_table[[i]] == levs_likelihood[j]
        )
      )
  }
}
# gw_table



# Make the GT table

forecast_date <- max(data_table_map$Forecast_Date, na.rm = TRUE)

conditions_table_table <- well_likely_conditions_map %>%
  filter(Likely_Conditions != "Not Available") %>%
  st_drop_geometry() %>%
  group_by(Forecast_Days) %>%
  mutate(Forecast_Date = paste0(Forecast_Days, " days (", format(max(Predicted_Date),"%b-%d"), ")")) %>%
  ungroup() %>%
  mutate(Aquifer = paste0("<a href = https://apps.nrs.gov.bc.ca/gwells/aquifers/", Aquifer_ID," target='_blank'>", Aquifer_ID," </a>"),
         Aquifer = purrr::map(Aquifer, gt::html),
         Location = paste0("<a href = https://nrs.objectstore.gov.bc.ca/rfc-conditions/groundwater_forecast/outputs/", Well, "_Model_Forecast.pdf target='_blank' >", Location," </a>"),
         Location = purrr::map(Location, gt::html),
         Well = paste0("<a href = https://nrs.objectstore.gov.bc.ca/rfc-conditions/groundwater_forecast/outputs/", Well, "_Model_Forecast.pdf target='_blank' >", Well," </a>"),
         Well = purrr::map(Well, gt::html)) %>%
  ungroup() %>%
  select(Well, Region, Location, Latest = Latest_Conditions,
         Forecast_Date, Likely_Conditions) %>%
  pivot_wider(names_from = Forecast_Date, values_from = Likely_Conditions)
names(conditions_table_table)[names(conditions_table_table)=="Latest"] <- paste0("Latest (", format(max(data_table_map$Latest_Date, na.rm = TRUE),"%b-%d"), ")")

# mapping prep

gw_conditions_table <- gt(conditions_table_table %>%
                 group_by(Region)) %>%
  cols_align(align = "left",
             columns = 3) %>%
  cols_align(align = "center",
             columns = 4:8) %>%
  tab_style(style = cell_fill(color = "#009E73"),
            locations = cells_body(
              columns = 4,
              rows = conditions_table_table[[4]] == "Normal")) %>%
  tab_style(style = cell_fill(color = "#0072B2"),
            locations = cells_body(
              columns = 4,
              rows = conditions_table_table[[4]] == "Above Normal")) %>%
  tab_style(style = cell_fill(color = "#D55E00"),
            locations = cells_body(
              columns = 4,
              rows = conditions_table_table[[4]] == "Below Normal")) %>%
  gt::tab_style(style = gt::cell_text(weight = "bold", size = px(16)),
                locations = gt::cells_column_labels()) %>%
  gt::tab_style(style = gt::cell_text(weight = "bold", size = px(15)),
                locations = gt::cells_column_spanners()) %>%
  gt::tab_style(style = list(gt::cell_text(weight = "bold", size = px(15), align = "center"),
                             cell_fill(color = "gray95")),
                locations = gt::cells_row_groups()) %>%
  tab_style(
    style = cell_borders(
      sides = "right",
      color = "#767676",
      weight = px(2),
      style = "solid"
    ),
    locations = cells_body(columns = 3:4)
  ) %>%
  tab_style(
    style = cell_text(size = px(14)),  # adjust size as needed
    locations = cells_body()
  )


# Add colours to the table
for (i in 5:8) {
  for (j in seq_along(levs_likelihood_conditions)) {
    gw_conditions_table <- gw_conditions_table %>%
      tab_style(
        style = cell_fill(color = cols_likelihood_conditions[j]),
        locations = cells_body(
          columns = i,
          rows = conditions_table_table[[i]] == levs_likelihood_conditions[j]
        )
      )
  }
}

# render report
rmarkdown::render(input = normalizePath("docs/province_report.Rmd"),
                  output_file = "Groundwater_Drought_Forecast_Report.html",
                  output_dir = normalizePath("output/"),
                  params = list("forecast_date" = forecast_date,
                                "map" = gw_map,
                                "table" = gw_table,
                                "table2" = gw_conditions_table))

# render archived report
rmarkdown::render(input = normalizePath("docs/province_report.Rmd"),
                  output_file = "Groundwater_Drought_Forecast_Report.html",
                  output_dir = normalizePath(paste0(output_path, "/")),
                  params = list("forecast_date" = forecast_date,
                                "map" = gw_map,
                                "table" = gw_table,
                                "table2" = gw_conditions_table))





