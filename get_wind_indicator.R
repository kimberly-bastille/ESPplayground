#Get data frame for wind indicator

## Year
## Month
## U
## V 
## Hypotenus = Pythagrean Therom = U2 + V2 = C2
## Angle = tan(opp/adj) == tan(U/V)

######################################################################## 

library(tidyverse)
library(raster)


raw.dir <- here::here("data-raw")

vwind_nc <-"vwnd.10m.mon.mean.nc"
uwind_nc <-"uwnd.10m.mon.mean.nc"

esp <- ecodata::ESP_sf

v1 <- raster::stack(file.path(raw.dir,vwind_nc))
v1 <- raster::crop(v1, extent(280,300,30,50))
v1 <- raster::rotate(v1)
crs(v1) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
#
"names" <- NULL
"values" <- NULL
df<- data.frame()
for(i in 1:nlayers(v1)){
  names <- v1[[i]]@data@names
  values<- v1[[i]]@data@max
  dat<-cbind(names,values)
  df<-rbind(df, dat)
}

vwind_out<- df %>% dplyr::mutate(Var = "vwind",
                                Year = stringr::str_extract(names, pattern = "\\d{4}"), 
                                names = stringr::str_remove(names, pattern = "X"), 
                                DOY = lubridate::as_date(names), 
                                values = as.numeric(values), 
                                month = lubridate::month(DOY))


u1 <- raster::stack(file.path(raw.dir,uwind_nc))
u1 <- raster::crop(u1, extent(280,300,30,50))
u1 <- raster::rotate(u1)
crs(u1) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
#
"names" <- NULL
"values" <- NULL
df<- data.frame()
for(i in 1:nlayers(u1)){
  names <- u1[[i]]@data@names
  values<- u1[[i]]@data@max
  dat<-cbind(names,values)
  df<-rbind(df, dat)
}

uwind_out<- df %>% dplyr::mutate(Var = "uwind",
                                 Year = stringr::str_extract(names, pattern = "\\d{4}"), 
                                 names = stringr::str_remove(names, pattern = "X"), 
                                 DOY = lubridate::as_date(names), 
                                 values = as.numeric(values), 
                                 month = lubridate::month(DOY))

wind_direction<- vwind_out %>% rbind(uwind_out) %>% 
  dplyr::select(c("values", "month", "Year", "Var")) %>% 
  tidyr::pivot_wider(names_from = "Var", values_from = "values") %>% 
  dplyr::mutate(hypot = sqrt(vwind^2 + uwind^2), 
                angle = tan(vwind/uwind))


write.csv(wind_direction, file.path(here::here("data-raw", "ESP_wind.csv")))

# wind_out %>% 
#   ggplot2::ggplot(aes(x= DOY, y = values))+
#   ggplot2::geom_point()+
#   ggplot2::ggtitle("U wind")+
#   ggplot2::ylab("west to east wind")+
#   ggplot2::facet_wrap(~month)+
#   ecodata::geom_gls()