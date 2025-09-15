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
                              "<br>", Forecast_URL,
                              "<br>", Technical_Forecast_URL,
                              "<br>", Realtime_URL,
                              "<br>", Well_URL,
                              "<br>", Aquifer_URL,
                              "<br>", Interactive_Hydrograph_URL,
                              "<br>", Static_Hydrograph_URL)) %>%
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
                              "<br>", Technical_Forecast_URL,
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
                              "<br>", Technical_Forecast_URL,
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
                              "<br>", Technical_Forecast_URL,
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
                            options = leaflet::layersControlOptions(collapsed = TRUE))  %>%
  leaflet::hideGroup(group = c("Below Normal - 30 days", "Below Normal - 60 days","Below Normal - 90 days"))
gw_map



map_title_text <- paste0("<b><u>RFC: Groundwater Level Drought Forecast</u></b>")

map_title_text_body <- paste0("**This project is under development.** Forecast of groundwater levels being below normal
for the next 14, 30, 60, and 90 days compared to historical conditions. Click the layer
box on the top right for more forecast days and the info box on the top left for more information.
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

info.box <- HTML(paste0(
  HTML(
    '<div class="modal fade" id="infobox" role="dialog"><div class="modal-dialog"><!-- Modal content--><div class="modal-content"><div class="modal-header"><button type="button" class="close" data-dismiss="modal">&times;</button>'
  ),

  # Header / Title
  HTML(
    paste0("<b>RFC: Groundwater Level Conditions Forecast</b>")
  ),
  HTML(
    '</div><div class="modal-body">'
  ),

  # Body
  HTML(
    paste0("This groundwater level model provides forecasts for 14, 30, 60, and 90 days in advance. It uses artificial neural networks, a type of machine learning,
    to relate groundwater levels to historic precipitation, temperature, and, if applicable, snowpack data. The model also accounts for recharge lag times,
    capturing how groundwater responds to hydroclimate data. Forecasts provide a range of likely conditions and are presented as likelihoods of
    groundwater being above, below, or near normal. This analysis uses the “normal” term solely in reference to water levels between the 25th to 75th
    percentiles of historical data, not to imply a steady state baseline for comparison. Data should be interpreted with the context of long-term
    records, patterns, and trends. The forecast will be updated daily throughout the year, unless issues with data availability
    or other issues arise.
                   <br><br>
               Model Links:<br>
              <u><a href=https://www2.gov.bc.ca/gov/content/environment/air-land-water/water/drought-flooding-dikes-dams/river-forecast-centre target='_blank' >Model explanation</a></u>
               | <u><a href=https://www2.gov.bc.ca/gov/content/environment/air-land-water/water/drought-flooding-dikes-dams/river-forecast-centre target='_blank' >Technical reference </a></u>
               | <u><a href=https://nrs.objectstore.gov.bc.ca/rfc-conditions/groundwater_forecast/outputs/Groundwater_Drought_Forecast_Report.html target='_blank' >Forecast Report</a></u>
               | <u><a href=https://github.com/bcgov/rfc-groundwater-forecast target='_blank' >Code (GitHub)</a></u>
               <br><br>
               Related Links:<br>
              <u><a href=https://www2.gov.bc.ca/gov/content/environment/air-land-water/water/drought-flooding-dikes-dams/river-forecast-centre target='_blank' >RFC Homepage</a></u>
               | <u><a href=https://www2.gov.bc.ca/gov/content/environment/air-land-water/water/groundwater-wells-aquifers/groundwater-observation-well-network target='_blank' > Provincial Groundwater Observation Well Network (PGOWN) </a></u>
               | <u><a href=https://www2.gov.bc.ca/gov/content/environment/air-land-water/water/groundwater-wells-aquifers/groundwater-observation-well-network/active-wells target='_blank' >List of active PGOWN wells</a></u>
               | <u><a href=https://bcmoe-prod.aquaticinformatics.net/ target='_blank' >Real-time water data tool</a></u>
               | <u><a href=https://climate.weather.gc.ca/ target='_blank' >ECCC Climate Data</a></u>
               | <u><a href=https://www2.gov.bc.ca/gov/content/environment/air-land-water/water/water-science-data/water-data-tools/snow-survey-data target='_blank' >BC Snow Program</a></u>
               | <u><a href=https://droughtportal.gov.bc.ca/ target='_blank' >B.C. Drought Information Portal</a></u>

               <br><br>
               <h4>Disclaimer</h4>
               Groundwater level forecasts use statistical models and third-party data. These models have two types of errors: systematic (model limitations) and random (input data).
               Forecasts may differ from actual observations, and water levels could exceed forecast bounds. Users must accept responsibility for their use and interpretation.
               Use the information provided with caution and at your own risk.
               <br><br>
               Although every effort has been made to provide accurate information and locations, the Government of British
               Columbia makes no representation or warranties regarding the accuracy of information on or linked from this map, nor will
               it accept responsibility for errors or omissions. Access to and/or content of this map may be suspended,
               discontinued, or altered, in part or in whole, at any time, for any reason, with or without prior notice,
               at the discretion of the Government of British Columbia.
               <br><br>
              <u><a href=https://www2.gov.bc.ca/gov/content/home/copyright target='_blank' >Copyright</a></u>
              | <u><a href=https://www2.gov.bc.ca/gov/content/home/disclaimer target='_blank' >Disclaimer</a></u>
              | <u><a href=http://www2.gov.bc.ca/gov/admin/privacy.page target='_blank' >Privacy</a></u>
              | <u><a href=https://www2.gov.bc.ca/gov/content/home/accessible-government target='_blank' >Accessibility</a></u>
          <br><u><a href=https://www2.gov.bc.ca/gov/content/home/copyright target='_blank' > Copyright Province of British Columbia </a></u>")
  ),
  # Closing divs
  HTML('</div><div class="modal-footer"><button type="button" class="btn btn-default" data-dismiss="modal">Close</button></div></div>')
))


gw_map <- gw_map  %>%
  htmlwidgets::prependContent(htmltools::HTML(headerCSS)) %>%
  htmlwidgets::prependContent(htmltools::HTML(headerHTML))%>%
  ##### Add Info box button ----
leaflet.extras::addBootstrapDependency() %>% # Add Bootstrap to be able to use a modal
  addEasyButton(easyButton(icon = "fa-info-circle", title = "Map Information and Disclaimer",
                           onClick = JS("function(btn, map){ $('#infobox').modal('show'); }")
  )) %>% # Trigger the infobox
  htmlwidgets::appendContent(info.box)
gw_map

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


