`%>%` <- magrittr::`%>%`

# shapefile setup
crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# equal area crs
new_crs <- "+proj=utm +zone=12 +datum=NAD83 +no_defs +ellps=GRS80"

# different geom fix
sf::sf_use_s2(FALSE)

cod <- sf::st_read(here::here("data-raw/CodShapefiles/codstox.shp"))

cod <- cod %>%
  sf::st_transform(proj4string = new_crs)

years <- 1982:2021
total_data <- tibble::tibble()

for (j in years) {
  message(paste("starting", j))
  # download data ----
  dir.create(here::here("data-raw", "gridded", "sst_data"), recursive = TRUE)
  url <- paste0("https://downloads.psl.noaa.gov/Datasets/noaa.oisst.v2.highres/sst.day.mean.", j, ".v2.nc")
  download.file(url, destfile = "test.nc")

  # R can't open the file (will have to do this in a gh action...)
  # download file manually for testing on desktop

  # name <- paste0(j, ".nc")
  name <- "test.nc"

  data <- ecopull::nc_to_raster(nc = name, varname = "sst") # converts to NAD83
  data <- raster::rotate(data)
  message("converted to raster...")

  # make sure all days are there ----

  if (raster::nlayers(data) < 365) {
    message(j, " does not have a full year of data! skipping!")
  } else {

    # filter to just july
    july <- c(
      paste0("X", j, ".07.0", 1:9),
      paste0("X", j, ".07.", 10:31)
    )

    # crop to regions & calculate ----

    # loop over months ----

    for (i in c(paste0("0", 1:9), paste0("1", 0:2))) {
      this_month <- stringr::str_subset(
        names(data),
        paste0("X", j, ".", i)
      )

      month_data <- data[[this_month]]

      message(paste("filtered to", i, "..."))

      # loop over regions ----

      for (k in c("EGOM", "GBK", "WGOM", "SNE")) {
        region_data <- raster::mask(
          x = month_data,
          mask = cod %>%
            dplyr::filter(STOCK == k)
        )

        # calculate total area ----

        area <- raster::area(region_data, na.rm = TRUE) %>%
          raster::as.data.frame(xy = TRUE) %>%
          dplyr::select(-c(x, y))

        total_area <- area %>%
          colSums(na.rm = TRUE)

        weighted_temp <- (region_data@data@values * area) %>%
          colSums(na.rm = TRUE)

        final_temp <- weighted_temp / total_area

        out_data <- tibble::tibble(
          Year = j,
          Month = i,
          Region = k,
          mean_temp = final_temp
        )

        if(nrow(total_data) == 0) {
          total_data <- out_data
        } else { 
          total_data <- dplyr::full_join(total_data, out_data)
        }
        message("calculated regional mean monthly temp...")
      }
    }
  }
  message(paste("done with", j))
}

total_data

write.csv(total_data, here::here("data-raw/cod_temperature.csv"))
