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

years <- 1982:2021
prop_spring <- c()

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
    
  ndays <- raster::nlayers(data) # account for leap years
  
  mab_temp <- raster::mask(x = data[[months]], 
                           mask = mab)
  message("cropped to MAB...")
  
  # create dataframes ----
  rast_mab_df <- raster::as.data.frame(mab_temp, xy = TRUE)
  message("created data frames...")
  
  # calculate proportion between 12-16C by day ----
  names <- c()
  value <- c()
  for(i in 3:ncol(rast_mab_df)){
    new_dat <- rast_mab_df[,i] %>%
      tibble::as_tibble() %>%
      tidyr::drop_na()
    names[i-2] <- colnames(rast_mab_df[i])
    value[i-2] <- nrow(new_dat %>% dplyr::filter(value > 12 & value < 16)) / nrow(new_dat)
    }
  mab_prop <- tibble::tibble(names, value)

  print(mab_prop)
  
  # maybe use dplyr::bind_rows?
  mab_prop <- mab_prop %>%
    dplyr::mutate(Year = stringr::str_extract(names, pattern = "\\d{4}"), 
                  names = stringr::str_remove(names, pattern = "X"), 
                  DOY = lubridate::as_date(names),
                  week = lubridate::week(DOY),
                  month = lubridate::month(DOY),
                  value = as.numeric(value))
  
  print(mab_prop)
  
  this_prop <- mab_prop
  
  print(this_prop)
  
  prop_spring <- c(prop_spring, mean(this_prop$value))
  message("calculated proportion...")
  }
  message(paste("done with", j))
}

print(prop_spring)

out_data <- tibble::tibble(years, prop_spring)
write.csv(out_data, here::here("data-raw/temperature_mackerel.csv"))