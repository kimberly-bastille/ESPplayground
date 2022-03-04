library(dplyr)
library(raster)
library(sf)
library(ggplot2)
library(ncdf4)
library(reshape2)
library(ecodata)
library(stringr)


raw.dir <- here::here("data-raw")

esp <- ecodata::ESP_sf
#
# masking function
 get_masked_mean <- function(w1, esp_name){
   w <- mask(w1, esp[esp$ID == esp_name,])
   wind_out <- mean(w1@data@values, na.rm = T)
   return(wind_out)
 }
#
wind_nc <-"vwnd.10m.2021.nc"
#
#Get anomaly calculation
w1 <- raster::stack(file.path(raw.dir,wind_nc))
w1 <- raster::crop(w1, extent(280,300,30,50))
w1 <- raster::rotate(w1)
crs(w1) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
#
"names" <- NULL
"values" <- NULL
df<- data.frame()
for(i in 1:nlayers(w1)){
  names <- w1[[i]]@data@names
  values<- w1[[i]]@data@max
  dat<-cbind(names,values)
  df<-rbind(df, dat)
  }

wind_out<- df %>% dplyr::mutate(Var = "vwind",
                                Year = stringr::str_extract(names, pattern = "\\d{4}"), 
                                names = stringr::str_remove(names, pattern = "X"), 
                                DOY = lubridate::as_date(names), 
                                values = as.numeric(values))

wind_out %>% 
  ggplot2::ggplot(aes(x= DOY, y = values))+
  ggplot2::geom_point()+
  ggplot2::ggtitle("V wind")+
  ggplot2::ylab("south to north wind")




##### Monthly 
raw.dir <- here::here("data-raw")
wind_nc <-"vwnd.10m.mon.mean.nc"
esp <- ecodata::ESP_sf

w1 <- raster::stack(file.path(raw.dir,wind_nc))
w1 <- raster::crop(w1, extent(280,300,30,50))
w1 <- raster::rotate(w1)
crs(w1) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
#
"names" <- NULL
"values" <- NULL
df<- data.frame()
for(i in 1:nlayers(w1)){
  names <- w1[[i]]@data@names
  values<- w1[[i]]@data@max
  dat<-cbind(names,values)
  df<-rbind(df, dat)
}

wind_out<- df %>% dplyr::mutate(Var = "vwind",
                                Year = stringr::str_extract(names, pattern = "\\d{4}"), 
                                names = stringr::str_remove(names, pattern = "X"), 
                                DOY = lubridate::as_date(names), 
                                values = as.numeric(values), 
                                month = lubridate::month(DOY))

wind_out %>% 
  ggplot2::ggplot(aes(x= DOY, y = values))+
  ggplot2::geom_point()+
  ggplot2::ggtitle("U wind")+
  ggplot2::ylab("west to east wind")+
  ggplot2::facet_wrap(~month)+
  ecodata::geom_gls()


### Seasonal
season_wind_out<- wind_out %>% 
  dplyr::mutate(season = dplyr::recode(month, 
                              "1" = "Winter",  "2" = "Winter",  "3" = "Winter", 
                              "4" = "Spring",  "5" = "Spring",  "6" = "Spring", 
                              "7" = "Summer",  "8" = "Summer",  "9" = "Summer", 
                              "10" = "Fall",  "11" = "Fall",   "12" = "Fall")) %>% 
  group_by(season, Year) %>% 
  summarise(values = mean(values)) %>% 
  mutate(Year = as.numeric(Year))


season_wind_out %>% 
  ggplot2::ggplot(aes(x= Year, y = values))+
  ggplot2::geom_point()+
  ggplot2::ggtitle("V wind")+
  ggplot2::ylab("south to north wind")+
  ggplot2::facet_wrap(~season)+
  ecodata::geom_gls()


