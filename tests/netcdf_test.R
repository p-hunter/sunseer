library(tidyverse)
library(magrittr)
library(ncdf4)


test_nc <- ncdf4::nc_open("patmosx_v05r03-preliminary_NOAA-15_asc_d20220101_c20220105.nc")


Long <- test_nc %>% ncdf4::ncvar_get(., "longitude")
Lat <- test_nc %>% ncdf4::ncvar_get(., "latitude")


Coord_System <- expand.grid(Long = as.vector(Long), Lat =  as.vector(Lat))


Cloud_F <- ncdf4::ncvar_get(test_nc, "cloud_fraction")
Cloud_F2 <- as.vector(Cloud_F )


nc2df_test_1 <- data.frame(Coord_System, Cloud_F2) %>% na.omit()
