# GLORYS climatology using CDO
# organize files
library(tidyverse)
library(tidync)
library(here)

# setwd("~/GLORYS_ESM_data")
all_fl <- list.files(here("GLORYS"),'.nc',full.names = T)
glorys_fl <- all_fl[grepl("GLORYS",all_fl)]

calc_glorys_clim <- function(hfile){
  vn <- tidync(hfile) %>% hyper_vars() %>% pull('name')
  if(!dir.exists(here('GLORYS_climatology'))){
    dir.create(here('GLORYS_climatology'))
  }
  ofile <- paste0(here(),'/GLORYS_climatology/glorys_',vn,"_clim.nc")
  cmd <- paste0('sudo cdo -ymonmean ',hfile,' ',ofile)
  system(cmd)
}

# apply to everything
purrr::walk(glorys_fl,calc_glorys_clim)

## Step 2: Add timesteps for future!
# all GLORYS outputs from the previous step are for year 2018
# yrs_to_expand <- 2006:2100-2018
# yrs_to_expand <- yrs_to_expand[which(yrs_to_expand!=0)]
# 
# cat_glorys_clim <- function(gfile){
#   shift1=paste0(yrs_to_expand[1],'year')
#   system(paste0('sudo cdo -shifttime,',shift1,' ',gfile,' glorys_climatology/yrx.nc'))
#   for(i in 2:length(yrs_to_expand)){
#     shift=paste0(yrs_to_expand[i],'year')
#     system(paste0('sudo cdo -shifttime,',shift,' ',gfile,' glorys_climatology/tmp1.nc'))
#     system(paste0('sudo cdo -cat glorys_climatology/yrx.nc glorys_climatology/tmp1.nc glorys_climatology/tmp2.nc'))
#     system(paste0('sudo mv glorys_climatology/tmp2.nc glorys_climatology/yrx.nc'))
#   }
#   system(paste0('sudo mv glorys_climatology/yrx.nc ',paste0(str_replace(gfile,"_clim\\.","_clim_exp."))))
# }
# 
# gfile <- "glorys_climatology/glorys_no3_clim.nc"
# cat_glorys_clim(gfile)
