[![img](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)


Groundwater Level Forecasting - River Forecast Centre
============================

R scripts for forecasting groundwater levels in BC's
[Provincial Groundwater Observation Well Network (PGOWN)](https://www2.gov.bc.ca/gov/content/environment/air-land-water/water/groundwater-wells-aquifers/groundwater-observation-well-network) 
wells using artificial neural network computational models.

This groundwater level model provides forecasts for 14, 30, 60, and 90 days in advance. It uses artificial neural networks, a type of machine learning, 
to relate groundwater levels to historic precipitation, temperature, and snowpack data. The model also accounts for recharge lag times,
capturing how groundwater responds to hydroclimate data. Forecasts provide a range of possible conditions, derived from different combinations of past weather patterns,
and are presented as likelihoods of groundwater being above, below, or near normal. More information can be found in the [Forecast Model Overview](https://nrs.objectstore.gov.bc.ca/rfc-conditions/groundwater_forecast/documentation/gw_forecast_overview.pdf).

Results of the forecast model can be found on the Groundwater Level Drought Forecast [Map](https://nrs.objectstore.gov.bc.ca/rfc-conditions/groundwater_forecast/outputs/Groundwater_Drought_Forecast_Map.html) 
and [Report](https://nrs.objectstore.gov.bc.ca/rfc-conditions/groundwater_forecast/outputs/Groundwater_Drought_Forecast_Report.html). 


### Project Status

This project is under development and will be subject to change. 

This project is maintained by the River Forecast Centre in the Water Management Branch of the BC Ministry of Water, Land and Resource Stewardship.

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an [issue](https://github.com/bcgov/rfc-groundwater-forecast/issues/).

### How to Contribute

If you would like to contribute, please see our [CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

### License

```
Copyright 2025 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.
```
