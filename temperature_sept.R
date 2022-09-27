`%>%` <- magrittr::`%>%`

# shapefile setup
crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# equal area crs
new_crs <- "+proj=utm +zone=12 +datum=NAD83 +no_defs +ellps=GRS80"

# different geom fix
sf::sf_use_s2(FALSE)

north_atlantic <- NEesp::shape %>%
  dplyr::select(STRATA, geometry) %>%
  sf::st_transform(proj4string = new_crs) %>% 
  # try geom fix?
  # dplyr::mutate(geometry = geometry %>% 
  #                 s2::s2_rebuild() %>%
  #                 sf::st_as_sfc()) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%  
  sf::st_crop(y = c(xmin = -90, xmax = -50, 
                    ymax = 50, ymin =  30))

years <- 1982:2021
temp_out <- NULL

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
  
  # crop to north atlantic ----
    
    # filter to just september
    months <- c(paste0("X", j, ".09.0", 1:9),
                paste0("X", j, ".09.", 10:30))
    
  na_temp <- raster::mask(x = data[[months]], 
                           mask = north_atlantic)
  message("cropped to north atlantic...")
  
  # calculate mean temp for each location
  
  mean_temp <- raster::calc(na_temp, mean)
  names(mean_temp) <- j
  
  if(is.null(temp_out)) {
    temp_out <- mean_temp
  } else {
        temp_out <- raster::stack(mean_temp, temp_out)
  }
  }
  message(paste("done with", j))
}

raster::writeRaster(temp_out, "data-raw/september_mean_temperature.grd")