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

strata <- readxl::read_excel(here::here("data-raw/BLF_STRATA.xlsx"),
                             skip = 3)
colnames(strata) <- c("alb", "big")
strata <- c(strata$alb, strata$big) %>%
  unique() 
print(strata)

bf_strata <- NEesp::shape %>%
  dplyr::select(STRATA, geometry) %>%
  dplyr::filter(STRATA %in% strata) %>%
  sf::st_transform(proj4string = new_crs) %>% 
  dplyr::summarise(geometry = sf::st_union(geometry))

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
  
  data <- ecopull::nc_to_raster(nc = name, varname = 'sst') # converts to NAD83
  data <- raster::rotate(data)
  message("converted to raster...")
  
  # make sure all days are there ----
  
  if(raster::nlayers(data) < 365) {
    message(j, " does not have a full year of data! skipping!")
  } else {
  
  # crop to MAB ----
  ndays <- raster::nlayers(data) # account for leap years
  
  mab_temp <- raster::mask(x = data[[1:180]], 
                           mask = bf_strata # mab
                           )
  mab_temp2 <- raster::mask(x = data[[181:ndays]], 
                            mask = bf_strata # mab
                            )
  message("cropped to MAB...")
  # 
  # # reproject to equal area crs ----
  # 
  # mab_temp <- raster::projectRaster(mab_temp, crs = new_crs)
  # mab_temp2 <- raster::projectRaster(mab_temp2, crs = new_crs)
  
  # calculate total area ----
  raster_areas <- raster::area(mab_temp, na.rm = TRUE) %>%
    raster::as.data.frame(xy = TRUE) %>%
    dplyr::select(-c(x, y))
  raster_areas2 <- raster::area(mab_temp2, na.rm = TRUE) %>%
    raster::as.data.frame(xy = TRUE) %>%
    dplyr::select(-c(x, y))
  
  total_area <- raster_areas %>%
    colSums(na.rm = TRUE) %>%
    unique() # always the same (should do earlier/simpler)
  
  temps_df <- mab_temp %>%
    raster::as.data.frame(xy = TRUE) %>%
    dplyr::select(-c(x, y))
  temps_df2 <- mab_temp2 %>%
    raster::as.data.frame(xy = TRUE) %>%
    dplyr::select(-c(x, y))
  
  # calculate weighted mean temp
  weighted_mean_temp <- (temps_df * raster_areas / total_area) %>%
    colSums(na.rm = TRUE)
  weighted_mean_temp2 <- (temps_df2 * raster_areas2 / total_area) %>%
    colSums(na.rm = TRUE)
  
  
  # calculate area 18-25.6C ----
  mab_temp@data@values[which(mab_temp@data@values < 18)] <- NA
  # also remove areas that are too warm
  mab_temp@data@values[which(mab_temp@data@values > 25.6)] <- NA
  
  mab_temp2@data@values[which(mab_temp2@data@values < 18)] <- NA
  # also remove areas that are too warm
  mab_temp2@data@values[which(mab_temp2@data@values > 25.6)] <- NA
  
  warm_area <- raster::area(mab_temp, na.rm = TRUE) %>%
    raster::as.data.frame(xy = TRUE) %>%
    dplyr::select(-c(x, y)) %>%
    colSums(na.rm = TRUE)
  warm_area2 <- raster::area(mab_temp2, na.rm = TRUE) %>%
    raster::as.data.frame(xy = TRUE) %>%
    dplyr::select(-c(x, y)) %>%
    colSums(na.rm = TRUE)
  
  area_data <- tibble::tibble(names = c(names(weighted_mean_temp), names(weighted_mean_temp2)),
                              warm_prop = c(warm_area, warm_area2)/total_area,
                              mean_temp = c(weighted_mean_temp, weighted_mean_temp2)
                              )
  
  # create dataframes ----
  # rast_mab_df <- raster::as.data.frame(mab_temp, xy = TRUE)
  # rast_mab_df2 <- raster::as.data.frame(mab_temp2, xy = TRUE)
  # message("created data frames...")
  # 
  # # calculate mean and proportion above 18C by day ----
  # names <- c()
  # prop <- c()
  # mean <- c()
  # for(i in 3:ncol(rast_mab_df)){
  #   new_dat <- rast_mab_df[,i] %>%
  #     tibble::as_tibble() %>%
  #     tidyr::drop_na()
  #   names[i-2] <- colnames(rast_mab_df[i])
  #   prop[i-2] <- nrow(new_dat %>% dplyr::filter(value > 18)) / nrow(new_dat)
  #   mean[i-2] <- mean(new_dat$value)
  # }
  # mab_prop <- tibble::tibble(names, prop, mean)
  # 
  # names <- c()
  # prop <- c()
  # mean <- c()
  # for(i in 3:ncol(rast_mab_df2)){
  #   new_dat <- rast_mab_df2[,i] %>%
  #     tibble::as_tibble() %>%
  #     tidyr::drop_na()
  #   names[i-2] <- colnames(rast_mab_df2[i])
  #   prop[i-2] <- nrow(new_dat %>% dplyr::filter(value > 18)) / nrow(new_dat)
  #   mean[i-2] <- mean(new_dat$value)
  # }
  # mab_prop2 <- tibble::tibble(names, prop, mean)
  # 
  # mab_prop <- dplyr::full_join(mab_prop, mab_prop2) %>%
  
  mab_prop <- area_data %>%
    dplyr::mutate(Year = stringr::str_extract(names, pattern = "\\d{4}"), 
                  names = stringr::str_remove(names, pattern = "X"), 
                  DOY = lubridate::as_date(names))
  
  # first and last days with mean temp > 18
  this_first <- mab_prop %>%
    dplyr::filter(mean_temp >= 18) %>%
    dplyr::arrange(DOY)
  
  first <- c(first, lubridate::yday(this_first$DOY[1]))
  last <- c(last, lubridate::yday(this_first$DOY[nrow(this_first)]))
  message("calculated mean temp...")
  
  # number of days with >=75% of area >18
  this_n_days <- mab_prop %>%
    dplyr::filter(warm_prop >= 0.75) %>%
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
write.csv(out_data, here::here("data-raw/temperature_indicators_bfstrata.csv"))