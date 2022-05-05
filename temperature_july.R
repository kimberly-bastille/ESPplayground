`%>%` <- magrittr::`%>%`

# shapefile setup
crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# different geom fix
sf::sf_use_s2(FALSE)

mab <- NEesp::shape %>%
  dplyr::select(STRATA, geometry) %>%
  sf::st_transform(proj4string = crs) %>% 
  # try geom fix?
  # dplyr::mutate(geometry = geometry %>% 
  #                 s2::s2_rebuild() %>%
  #                 sf::st_as_sfc()) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%  
  sf::st_crop(y = c(xmin = -80, xmax = -69, 
                    ymax = 41.5, ymin =  35.8327))

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
  
  data <- ecopull::nc_to_raster(nc = name, varname = 'sst')
  data <- raster::rotate(data)
  message("converted to raster...")
  
  # make sure all days are there ----
  
  if(raster::nlayers(data) < 365) {
    message(j, " does not have a full year of data! skipping!")
  } else {
  
  # crop to MAB ----
  ndays <- raster::nlayers(data) # account for leap years
  
  mab_temp <- raster::mask(x = data[[1:180]], 
                           mask = mab)
  mab_temp2 <- raster::mask(x = data[[181:ndays]], 
                            mask = mab)
  message("cropped to MAB...")
  
  # create dataframes ----
  rast_mab_df <- raster::as.data.frame(mab_temp, xy = TRUE)
  rast_mab_df2 <- raster::as.data.frame(mab_temp2, xy = TRUE)
  message("created data frames...")
  
  # calculate proportion above 22C by day ----
  names <- c()
  value <- c()
  for(i in 3:ncol(rast_mab_df)){
    new_dat <- rast_mab_df[,i] %>%
      tibble::as_tibble() %>%
      tidyr::drop_na()
    names[i-2] <- colnames(rast_mab_df[i])
    value[i-2] <- nrow(new_dat %>% dplyr::filter(value > 22)) / nrow(new_dat)
  }
  mab_prop <- tibble::tibble(names, value, region = "MAB")
  
  names <- c()
  value <- c()
  for(i in 3:ncol(rast_mab_df2)){
    new_dat <- rast_mab_df2[,i] %>%
      tibble::as_tibble() %>%
      tidyr::drop_na()
    names[i-2] <- colnames(rast_mab_df2[i])
    value[i-2] <- nrow(new_dat %>% dplyr::filter(value > 22)) / nrow(new_dat)
  }
  mab_prop2 <- tibble::tibble(names, value, region = "MAB")
  
  mab_prop <- rbind(mab_prop, mab_prop2) %>%
    dplyr::mutate(Year = stringr::str_extract(names, pattern = "\\d{4}"), 
                  names = stringr::str_remove(names, pattern = "X"), 
                  DOY = lubridate::as_date(names),
                  week = lubridate::week(DOY),
                  month = lubridate::month(DOY),
                  value = as.numeric(value))
  
  this_prop <- mab_prop %>%
    dplyr::filter(region == "MAB",
                  month == 7) %>%
    dplyr::arrange(DOY)
  
  prop_july <- c(prop_july, mean(this_prop$value))
  message("calculated proportion...")
  }
  message(paste("done with", j))
}

prop_july

out_data <- tibble::tibble(years, prop_july)
write.csv(out_data, here::here("data-raw/temperature_july.csv"))