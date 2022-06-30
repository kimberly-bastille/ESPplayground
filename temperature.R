`%>%` <- magrittr::`%>%`

# shapefile setup
crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# equal area crs
new_crs <- "+proj=utm +zone=12 +datum=NAD83 +no_defs +ellps=GRS80"

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
first <- c()
last <- c()
n_days <- c()
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
  
  # reproject to equal area crs ----
  
  mab_temp <- raster::projectRaster(mab_temp, crs = new_crs)
  mab_temp2 <- raster::projectRaster(mab_temp2, crs = new_crs)
  
  # create dataframes ----
  rast_mab_df <- raster::as.data.frame(mab_temp, xy = TRUE)
  rast_mab_df2 <- raster::as.data.frame(mab_temp2, xy = TRUE)
  message("created data frames...")

  # calculate mean and proportion above 18C by day ----
  names <- c()
  value <- c()
  mean <- c()
  for(i in 3:ncol(rast_mab_df)){
    new_dat <- rast_mab_df[,i] %>%
      tibble::as_tibble() %>%
      tidyr::drop_na()
    names[i-2] <- colnames(rast_mab_df[i])
    value[i-2] <- nrow(new_dat %>% dplyr::filter(value > 18)) / nrow(new_dat)
    mean[i-2] <- mean(new_dat$value)
  }
  mab_prop <- tibble::tibble(names, value, mean, region = "MAB")
  
  names <- c()
  value <- c()
  mean <- c()
  for(i in 3:ncol(rast_mab_df2)){
    new_dat <- rast_mab_df2[,i] %>%
      tibble::as_tibble() %>%
      tidyr::drop_na()
    names[i-2] <- colnames(rast_mab_df2[i])
    value[i-2] <- nrow(new_dat %>% dplyr::filter(value > 18)) / nrow(new_dat)
    mean[i-2] <- mean(new_dat$value)
  }
  mab_prop2 <- tibble::tibble(names, value, mean, region = "MAB")
  
  mab_prop <- dplyr::full_join(mab_prop, mab_prop2) %>%
    dplyr::mutate(Year = stringr::str_extract(names, pattern = "\\d{4}"), 
                  names = stringr::str_remove(names, pattern = "X"), 
                  DOY = lubridate::as_date(names),
                  week = lubridate::week(DOY),
                  value = as.numeric(value))
  
  this_first <- mab_prop %>%
    dplyr::filter(mean >= 18) %>%
    dplyr::arrange(DOY)

  first <- c(first, lubridate::yday(this_first$DOY[1]))
  last <- c(last, lubridate::yday(this_first$DOY[nrow(this_first)]))
  message("calculated mean temp...")
  
  this_n_days <- mab_prop %>%
    dplyr::filter(region == "MAB",
                  value >= 0.75) %>%
    dplyr::arrange(DOY)
  
  n_days <- c(n_days, length(this_n_days$DOY))
  message("calculated proportion...")
  }
  message(paste("done with", j))
}

first
last
n_days

out_data <- tibble::tibble(years, first, last, n_days)
write.csv(out_data, here::here("data-raw/temperature_indicators.csv"))