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

bf_strata <- NEesp::shape %>%
  dplyr::select(STRATA, geometry) %>%
  dplyr::filter(STRATA %in% strata) %>%
  sf::st_transform(proj4string = new_crs) %>% 
  dplyr::summarise(geometry = sf::st_union(geometry))

years <- 1982:2021
prop_july <- c()

for(j in years) {
  message(paste("starting", j))
  # download data ----
  dir.create(here::here("data-raw","gridded", "sst_data"), recursive = TRUE)
  url <- paste0("https://downloads.psl.noaa.gov/Datasets/noaa.oisst.v2.highres/sst.day.mean.", j, ".v2.nc")
  download.file(url, destfile = "test.nc")
  
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
  
    # filter to just july
    july <- c(paste0("X", j, ".07.0", 1:9),
              paste0("X", j, ".07.", 10:31))
    
  # crop to MAB ----
  # ndays <- raster::nlayers(data) # account for leap years
  
  mab_temp <- raster::mask(x = data[[july]], 
                           mask = bf_strata # mab
                           )
  message("cropped to July MAB...")

  # calculate total area ----
  total_area <- raster::area(mab_temp, na.rm = TRUE) %>%
    raster::as.data.frame(xy = TRUE) %>%
    dplyr::select(-c(x, y)) %>%
    colSums(na.rm = TRUE)
  
  # calculate area above 22-25.6C ----
  mab_temp@data@values[which(mab_temp@data@values < 22)] <- NA
  # also remove areas that are too warm
  # mab_temp@data@values[which(mab_temp@data@values > 25.6)] <- NA
  
  july_area <- raster::area(mab_temp, na.rm = TRUE) %>%
    raster::as.data.frame(xy = TRUE) %>%
    dplyr::select(-c(x, y)) %>%
    colSums(na.rm = TRUE)
  
  # calculate proportion ----
  
  props <- july_area/total_area
  this_prop <- mean(props)
  
  prop_july <- c(prop_july, this_prop)
  message("calculated proportion...")
  }
  message(paste("done with", j))
}

prop_july

out_data <- tibble::tibble(years, prop_july)
write.csv(out_data, here::here("data-raw/temperature_july_bfstrata.csv"))