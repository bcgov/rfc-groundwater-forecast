dl_snow_data <- function(pgown_well_info, data_location) {

  message("Starting snow data download...")

  station_list <- as.list(unique(pgown_well_info$snow_stn)) %>%
    map(discard, is.na) %>%
    compact()

  snow_data_raw <- lapply(station_list, function(stn) {
    get_aswe_databc(station_id = stn, get_year = "All", parameter = "swe",
                    timestep = "daily")
  }) %>%
    bind_rows() %>%
    mutate(date = date(date_utc)) %>%
    pad(by = "date", group = c("id")) %>%
    ungroup() %>%
    dplyr::rename(snow_stn = id,
                  Date = date,
                  SWE = value) %>%
    full_join(pgown_well_info, by = "snow_stn") %>%
    dplyr::select(Well, Date, SWE)

  save(snow_data_raw, file = paste0(data_location, "snow_data_raw.RData"))

  message("...snow data download done!")

  return(snow_data_raw)

}

