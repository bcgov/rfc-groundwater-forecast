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
# Script name:      00_Run_Model.R
# ------------------------------------------------------------------------------
# Script version:
# 2025-09-04:       v1
#
# ------------------------------------------------------------------------------
# Notes/Objectives:
# run scripts for model forecasting, creating maps and putting onto object storage
#
# ==============================================================================
#


# Run config setup
source("01_ConfigInputs.R")

# Run the model
source("03_Model_Forecasting.R")

# Save maps and reports
source("03b_Forecast_Mapping.R")
source("03c_Forecast_Report.R")

# Put results on objectstore
source("99_objectstore.R")

# Save performance tracking plots
source("03d_Forecast_Performance.R")


