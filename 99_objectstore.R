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
# Script name:      99_objectstore.R
# ------------------------------------------------------------------------------
# Script version:
# 2025-09-04:       v1 - JG
#
# ------------------------------------------------------------------------------
# Notes/Objectives:
# script for putting forecasts onto objectstore
#
# ==============================================================================

source("01_ConfigInputs.R")

#create a new directory to save the figures too based on the date.


library(aws.s3)

figure_location <- "output/"
output_path <- file.path(figure_location, "previous_forecasts", as.character(Sys.Date()))
file_loc <- normalizePath(output_path)

bucket <- "rfc-conditions/groundwater_forecast/outputs"
region <- ""

# Recursively list all files in the directory and subdirectories
all_files <- list.files(file_loc, recursive = TRUE, full.names = TRUE)

for (file_path in all_files) {

  # Determine S3 object key (relative path from output_path)
  relative_key <- sub(paste0(normalizePath(output_path), .Platform$file.sep), "", file_path, fixed = TRUE)

  file_ext <- sub(".*\\.", "", relative_key)


  message("Uploading: ", relative_key)

  if (file_ext == "html") {
    put_object(file = file_path,
               object = relative_key,
               bucket = bucket,
               region = region,
               headers = list(`Content-Type` = "text/html"),
               acl = "public-read")

  } else if (file_ext == "pdf") {
    put_object(file = file_path,
               object = relative_key,
               bucket = bucket,
               region = region,
               headers = list(
                 `Content-Type` = "application/pdf",
                 `Content-Disposition` = "inline"
               ),               acl = "public-read")

  } else if (file_ext == "jpeg") {
    put_object(file = file_path,
               object = relative_key,
               bucket = bucket,
               region = region,
               headers = list(
                 `Content-Type` = "image/jpeg",
                 `Content-Disposition` = "inline"
               ),
               acl = "public-read")

  } else {
    put_object(file = file_path,
               object = relative_key,
               bucket = bucket,
               region = region,
               acl = "public-read")

  }
}

# save in a date folder
folder_name <- paste0("previous_forecasts/", Sys.Date())  # include trailing slash

# Create an empty object to simulate a folder
put_object(file = rawConnection(raw(0)),
           object = paste0(folder_name, "/"),
           bucket = bucket,
           region = region,
           acl = "public-read")

# new bucket to put files
bucket_date <- paste0(bucket, "/", folder_name)

for (file_path in all_files) {

  # file_path <- all_files[195]

  # Determine S3 object key (relative path from output_path)
  relative_key <- sub(paste0(normalizePath(output_path), .Platform$file.sep), "", file_path, fixed = TRUE)

  file_ext <- sub(".*\\.", "", relative_key)


  message("Uploading: ", relative_key)

  if (file_ext == "html") {
    put_object(file = file_path,
               object = relative_key,
               bucket = bucket_date,
               region = region,
               headers = list(`Content-Type` = "text/html"),
               acl = "public-read")

  } else if (file_ext == "pdf") {
    put_object(file = file_path,
               object = relative_key,
               bucket = bucket_date,
               region = region,
               headers = list(
                 `Content-Type` = "application/pdf",
                 `Content-Disposition` = "inline"
               ),               acl = "public-read")

  } else if (file_ext == "jpeg") {
    put_object(file = file_path,
               object = relative_key,
               bucket = bucket_date,
               region = region,
               headers = list(
                 `Content-Type` = "image/jpeg",
                 `Content-Disposition` = "inline"
               ),
               acl = "public-read")

  } else {
    put_object(file = file_path,
               object = relative_key,
               bucket = bucket_date,
               region = region,
               acl = "public-read")

  }


  # put the csv files in the dail_csv folder
  if (relative_key %in% c("RFC_GW_Forecast.csv", "predictive_forecast_results.csv")) {

    file_name <- sub("\\.[^.]*$", "", relative_key)

    put_object(file = file_path,
               object = paste0(file_name, "_", Sys.Date(), ".csv"),
               bucket = paste0(bucket, "/previous_forecasts/daily_csv"),
               region = region,
               acl = "public-read")
  }

}




