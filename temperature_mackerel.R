`%>%` <- magrittr::`%>%`

# shapefile setup
crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# equal area crs
new_crs <- "+proj=utm +zone=12 +datum=NAD83 +no_defs +ellps=GRS80"

# different geom fix
sf::sf_use_s2(FALSE)

mab <- NEesp::shape %>%
  dplyr::select(STRATA, geometry) %>%
  sf::st_transform(proj4string = new_crs) %>% 
  # try geom fix?
  # dplyr::mutate(geometry = geometry %>% 
  #                 s2::s2_rebuild() %>%
  #                 sf::st_as_sfc()) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%  
  sf::st_crop(y = c(xmin = -80, xmax = -69, 
                    ymax = 41.5, ymin =  35.8327))

gosl <- sf::st_read(here::here("data-raw/MackerelShapefiles/iho.shp"))

ggplot2::ggplot() +
  ggplot2::geom_sf(data = gosl) +
  ggplot2::theme_minimal()

years <- 1982:2021
prop_spring <- c()
area_days_spring <- c()

for(j in years) {
  message(paste("starting", j))
  # download data ----
  dir.create(here::here("data-raw","gridded", "sst_data"), recursive = TRUE)
  url <- paste0("https://downloads.psl.noaa.gov/Datasets/noaa.oisst.v2.highres/sst.day.mean.", j, ".v2.nc")
  download.file(url, destfile = "test.nc")
  # 
  # R can't open the file (will have to do this in a gh action...)
  # download file manually for testing on desktop
  
   # name <- paste0(j, ".nc")
  name <- "test.nc"
  
  data <- ecopull::nc_to_raster(nc = name, varname = 'sst') # converts to NAD83
  data <- raster::rotate(data)
  message("converted to raster...")
  
  # make sure all days are there ----
  
  if(raster::nlayers(data) < 365) {
    message(j, " does not have a full year of data! skipping!")
  } else {
  
  # crop to MAB ----
    
    # filter to just may and june
    months <- c(paste0("X", j, ".05.0", 1:9),
                paste0("X", j, ".05.", 10:31),
                paste0("X", j, ".06.0", 1:9),
                paste0("X", j, ".06.", 10:30))
    
  # mab_temp <- raster::mask(x = data[[months]], 
  #                          mask = mab)
  # message("cropped to MAB...")
  
  mab_temp <- raster::mask(x = data[[months]], 
                           mask = gosl)
  message("cropped to Gulf of St Lawrence...")
  
  # create dataframes ----
  rast_mab_df <- raster::as.data.frame(mab_temp, xy = TRUE)
  message("created data frames...")
  
  # calculate proportion between 12-16C by day ----
  
  # calculate total area ----
  total_area <- raster::area(mab_temp, na.rm = TRUE) %>%
    raster::as.data.frame(xy = TRUE) %>%
    dplyr::select(-c(x, y)) %>%
    colSums(na.rm = TRUE)
  
  # calculate area between 12-16C ----
  opt_mab_temp <- mab_temp
  opt_mab_temp@data@values[which(mab_temp@data@values > 16)] <- NA
  opt_mab_temp@data@values[which(mab_temp@data@values < 12)] <- NA

 optimal_area <- raster::area(opt_mab_temp, na.rm = TRUE) %>%
    raster::as.data.frame(xy = TRUE) %>%
    dplyr::select(-c(x, y)) %>%
    colSums(na.rm = TRUE)
 
 this_dat <- tibble::tibble(Year = j,
                            mean_total_area = mean(total_area),
                            mean_optimal_area = mean(optimal_area),
                            sum_optimal_area = sum(optimal_area),
                            proportion_optimal = mean_optimal_area/mean_total_area)

  message("calculated indicators...")
  
  area_days_spring <- rbind(area_days_spring, this_dat)
  }
  message(paste("done with", j))
}

print(area_days_spring)

out_data <- tibble::tibble(area_days_spring)
write.csv(out_data, here::here("data-raw/temperature_mackerel_gosl.csv"))