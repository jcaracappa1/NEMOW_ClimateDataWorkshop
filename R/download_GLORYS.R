system("pip install motuclient==1.8.4")
# install.packages("CopernicusMarine")
library(CopernicusMarine)
library(tidyverse)
library(tidync)

home_dir = getwd()
out_dir = paste0(home_dir, "/GLORYS/")

REGION = c(-78,34,-62,47)
VERT_RANGE =  c(0, 500) 

copernicus_product_details(product       = "GLOBAL_MULTIYEAR_PHY_001_030",
                           layer         = "cmems_mod_glo_phy_my_0.083_P1M-m",
                           variable = "thetao")

copernicus_download_motu(
  username = "USERNAME",
  password = "PASSWORD",
  destination   = paste0(out_dir,'GLORYS-Monthly-1993-2000.nc'),
  product       = "GLOBAL_MULTIYEAR_PHY_001_030",
  layer         = "cmems_mod_glo_phy_my_0.083_P1M-m",
  variable      = "sea_water_velocity",
  output        = "netcdf",
  region        = REGION,
  timerange     = c("1993-01-01", "2000-10-31"),
  sub_variables = 'thetao',
  verticalrange = VERT_RANGE,
  overwrite=TRUE
)
