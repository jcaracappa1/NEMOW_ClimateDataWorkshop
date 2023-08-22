system("pip install motuclient==1.8.4")
install.packages("CopernicusMarine")
library(CopernicusMarine)
library(tidyverse)
library(tidync)

home_dir = getwd()
out_dir = paste0(home_dir, "/Data/")

REGION = c(-78,34,-62,47)
VERT_RANGE =  c(0, 500) 

copernicus_download_motu(
  username = "jcaracappa",
  password = "Montaigne$1",
  destination   = paste0(out_dir,'GLORYS-Monthly-1993-2000.nc'),
  product       = "GLOBAL_MULTIYEAR_PHY_001_030",
  layer         = "cmems_mod_glo_phy_my_0.083_P1M-m",
  variable      = "Sea water velocity",
  output        = "netcdf",
  region        = REGION,
  timerange     = c("1993-01-01", "2019-12-31"),
  verticalrange = VERT_RANGE,
  sub_variables = c("thetao"),
  overwrite=TRUE
)