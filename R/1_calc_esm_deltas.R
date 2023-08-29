# Creates ESM delta climatology using CDO

# organize files
library(tidyverse)
library(tidync)
library(here)

#Specify data directories

esm.dir = here('GFDL_CM4')
all_fl <- list.files(esm.dir,'*.nc',full.names = F)
all_fl_full <- list.files(esm.dir,'*.nc',full.names = T)

gfdl_historical_fl <- all_fl[which(grepl("GFDL",all_fl)&grepl("historical",all_fl))]
gfdl_historical_fl_full <- all_fl_full[which(grepl("GFDL",all_fl)&grepl("historical",all_fl_full))]

gfdl_future_fl <- all_fl[which(grepl("GFDL",all_fl)&grepl("ssp245",all_fl))]
gfdl_future_fl_full <- all_fl_full[which(grepl("GFDL",all_fl)&grepl("ssp245",all_fl_full))]

#Specify time variables

gfdl_historical_time = sapply(gfdl_historical_fl,function(x) strsplit(x,'_|.nc')[[1]][7],USE.NAMES = F)
gfdl_future_time = sapply(gfdl_future_fl,function(x) strsplit(x,'_|.nc')[[1]][7],USE.NAMES = F)

merged.time = c('1990-2054')

## Pre-process step: stitch historical and future ESMs so we end up matching time periods with GLORYS
system(paste0('sudo cdo -sinfon ',gfdl_historical_fl_full[1]))
system(paste0('sudo cdo  -sinfon ',gfdl_future_fl_full[1]))

merge_esm_past_future <- function(esm.dir,hfile,ffile,merged.time){
  if(!dir.exists(paste0(esm.dir,'/merged_ESM'))){
    dir.create(paste0(esm.dir,'/merged_ESM'))
  }
  time.str = strsplit(hfile[1],'_|.nc')[[1]][7]
  ofile <- hfile[1] %>% str_replace("historical","merged") %>% str_replace(time.str,merged.time)
  ofile <- paste0(esm.dir,"/merged_ESM/",ofile)
  hfile.str = paste0(esm.dir,'/',hfile, collapse = ' ')
  ffile.str = paste0(esm.dir,'/',ffile, collapse = ' ')
  cmd1 <- paste('sudo cdo  -mergetime,',hfile.str,ffile.str,ofile)
  system(cmd1)
}

# Function to calculate the ESM delta (future minus climatology)
# The function calculates both the mean climatology (by month) and the delta (by subtracting the monthly means from each future projection year)
calc_esm_delta <- function(mergefile,esm.dir){
  vn <- tidync(mergefile) %>% hyper_vars() %>% pull('name')
  if(grepl('zos',mergefile)) vn <- 'zos'
  newvn <- paste0('delta_',vn)
  esm <- str_split(mergefile,"_")[[1]][5]
  if(!dir.exists(here('ESM_climatology'))){
    dir.create(here('ESM_climatology'))
  }
  ofile <- paste0(here(),'/ESM_climatology/',esm,"_",newvn,".nc")
  cmd <- paste0('sudo cdo  -O -chname,',vn,',',newvn,' -ymonsub ',mergefile,' -ymonmean -selyear,1993/2018 ',mergefile,' ',ofile)
  # if the variable is si, no3, or o2, need to convert from mol/m3 to mmol/m3 to match GLORYS
  if(grepl('no3',mergefile)|grepl('o2',mergefile)|grepl('si',mergefile)){
    cmd <- paste0('sudo cdo  -O -setunit,"mmol m-3" -mulc,1000 -selname,',newvn,' -chname,',vn,',',newvn,' -ymonsub ',mergefile,' -ymonmean -selyear,1993/2018 ',mergefile,' ',ofile)
  }
  # if the variable is chl, need to convert from kg/m3 to mg/m3 to match GLORYS
  if(grepl('chl',mergefile)){
    cmd <- paste0('sudo cdo  -O -setunit,"mg m-3" -mulc,1000000 -selname,',newvn,' -chname,',vn,',',newvn,' -ymonsub ',mergefile,' -ymonmean ',mergefile,' ',ofile)
  }
  system(cmd)
}

# for a multiplicative delta (OPTIONAL)
# UPDATE 05.25.2022- UPDATING CHLOROPHYLL TRANSLATION
# REPLACING NEGATIVE VALUES IN THE ESMs WITH SMALL POSITIVE
calc_esm_proportional_delta <- function(mergefile){
  vn <- tidync(mergefile) %>% hyper_vars() %>% pull('name')
  if(grepl('zos',mergefile)) vn <- 'zos'
  newvn <- paste0('delta_',vn)
  esm <- str_split(mergefile,"_")[[1]][5]
  ofile <- paste0('esm_climatology/',esm,"_",newvn,"_proportional.nc")
  cmd <- paste0('sudo cdo  -O -chname,',vn,',',newvn,' -ymonsub ',mergefile,' -ymonmean -selyear,1993/2018 ',mergefile,' ',ofile)
  # if the variable is si, no3, or o2, need to convert from mol/m3 to mmol/m3 to match GLORYS
  if(grepl('no3',mergefile)|grepl('o2',mergefile)|grepl('si',mergefile)){
    cmd <- paste0('sudo cdo  -O -setunit,"mmol m-3" -mulc,1000 -selname,',newvn,' -chname,',vn,',',newvn,' -ymondiv ',mergefile,' -ymonmean -selyear,1993/2018 ',mergefile,' ',ofile)
  }
  # if the variable is chl, need to convert from kg/m3 to mg/m3 to match GLORYS
  if(grepl('chl',mergefile)){
    cmd <- paste0('sudo cdo  -O -setunit,"mg m-3" -selname,',newvn,' -chname,',vn,',',newvn,' -ymondiv -setrtoc,-10,0,1e-15 ',mergefile,' -ymonmean -setrtoc,-10,0,1e-15 -selyear,1993/2018 ',mergefile,' ',ofile)
  }
  system(cmd)
}

#Creates a table to run through parameters
fl_tbl <- tibble(h=c(gfdl_historical_fl),f=c(gfdl_future_fl))

# apply to everything

merge_esm_past_future(esm.dir = esm.dir,
                      hfile = fl_tbl$h,
                      ffile = fl_tbl$f,
                      merged.time)
# purrr::pwalk(list(esm.dir = esm.dir,hfile = fl_tbl$h,ffile = fl_tbl$f, merged.time),merge_esm_past_future)

# we do additive deltas for everything except chl
mergefile <- list.files(paste0(esm.dir,"/merged_ESM"),full.names = T)
calc_esm_delta(mergefile = mergefile,
               esm.dir = esm.dir)
