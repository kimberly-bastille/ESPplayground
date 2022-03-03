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
#
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


