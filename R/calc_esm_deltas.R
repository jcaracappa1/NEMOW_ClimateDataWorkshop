# ESM delta climatology using CDO
# organize files
library(tidyverse)
library(tidync)

### NEW 06/09/2022: MAKE THIS 1993-2018 FOR BOTH SETS OF MODELS
### EXPLICITLY PLOT THE DELTAS

setwd("~/GLORYS_ESM_data")
all_fl <- list.files('raw_model_output',full.names = T)
ipsl_historical_fl <- all_fl[which(grepl("IPSL",all_fl)&grepl("historical",all_fl))]
gfdl_historical_fl <- all_fl[which(grepl("GFDL",all_fl)&grepl("historical",all_fl))]
had_historical_fl <- all_fl[which(grepl("HadGEM2",all_fl)&grepl("historical",all_fl))]
ipsl_future_fl <- all_fl[which(grepl("IPSL",all_fl)&grepl("rcp85",all_fl))]
gfdl_future_fl <- all_fl[which(grepl("GFDL",all_fl)&grepl("rcp85",all_fl))]
had_future_fl <- all_fl[which(grepl("HadGEM2",all_fl)&grepl("rcp85",all_fl))]

## Pre-process step: stitch historical and future ESMs so we end up matching time periods with GLORYS
system(paste0('sudo cdo -sinfon ',gfdl_historical_fl[1]))
system(paste0('sudo cdo -sinfon ',gfdl_future_fl[1]))

merge_esm_past_future <- function(hfile,ffile){
  ofile <- hfile %>% str_replace("historical","merged") %>% str_replace("1976-2005","1976-2100")
  ofile <- paste0("merged_ESMs/",ofile)
  cmd1 <- paste('sudo cdo -mergetime,',hfile,ffile,ofile)
  system(cmd1)
}

# Function to calculate the ESM delta (future minus climatology)
# The function calculates both the mean climatology (by month) and the delta (by subtracting the monthly means from each future projection year)
calc_esm_delta <- function(mergefile){
  vn <- tidync(mergefile) %>% hyper_vars() %>% pull('name')
  if(grepl('zos',mergefile)) vn <- 'zos'
  newvn <- paste0('delta_',vn)
  esm <- str_split(mergefile,"_")[[1]][5]
  ofile <- paste0('esm_climatology/',esm,"_",newvn,".nc")
  cmd <- paste0('sudo cdo -O -chname,',vn,',',newvn,' -ymonsub ',mergefile,' -ymonmean -selyear,1993/2018 ',mergefile,' ',ofile)
  # if the variable is si, no3, or o2, need to convert from mol/m3 to mmol/m3 to match GLORYS
  if(grepl('no3',mergefile)|grepl('o2',mergefile)|grepl('si',mergefile)){
    cmd <- paste0('sudo cdo -O -setunit,"mmol m-3" -mulc,1000 -selname,',newvn,' -chname,',vn,',',newvn,' -ymonsub ',mergefile,' -ymonmean -selyear,1993/2018 ',mergefile,' ',ofile)
  }
  # if the variable is chl, need to convert from kg/m3 to mg/m3 to match GLORYS
  if(grepl('chl',mergefile)){
    cmd <- paste0('sudo cdo -O -setunit,"mg m-3" -mulc,1000000 -selname,',newvn,' -chname,',vn,',',newvn,' -ymonsub ',mergefile,' -ymonmean ',mergefile,' ',ofile)
  }
  system(cmd)
}
# for a multiplicative delta
# UPDATE 05.25.2022- UPDATING CHLOROPHYLL TRANSLATION
# REPLACING NEGATIVE VALUES IN THE ESMs WITH SMALL POSITIVE
calc_esm_proportional_delta <- function(mergefile){
  vn <- tidync(mergefile) %>% hyper_vars() %>% pull('name')
  if(grepl('zos',mergefile)) vn <- 'zos'
  newvn <- paste0('delta_',vn)
  esm <- str_split(mergefile,"_")[[1]][5]
  ofile <- paste0('esm_climatology/',esm,"_",newvn,"_proportional.nc")
  cmd <- paste0('sudo cdo -O -chname,',vn,',',newvn,' -ymonsub ',mergefile,' -ymonmean -selyear,1993/2018 ',mergefile,' ',ofile)
  # if the variable is si, no3, or o2, need to convert from mol/m3 to mmol/m3 to match GLORYS
  if(grepl('no3',mergefile)|grepl('o2',mergefile)|grepl('si',mergefile)){
    cmd <- paste0('sudo cdo -O -setunit,"mmol m-3" -mulc,1000 -selname,',newvn,' -chname,',vn,',',newvn,' -ymondiv ',mergefile,' -ymonmean -selyear,1993/2018 ',mergefile,' ',ofile)
  }
  # if the variable is chl, need to convert from kg/m3 to mg/m3 to match GLORYS
  if(grepl('chl',mergefile)){
    cmd <- paste0('sudo cdo -O -setunit,"mg m-3" -selname,',newvn,' -chname,',vn,',',newvn,' -ymondiv -setrtoc,-10,0,1e-15 ',mergefile,' -ymonmean -setrtoc,-10,0,1e-15 -selyear,1993/2018 ',mergefile,' ',ofile)
  }
  system(cmd)
}

fl_tbl <- tibble(h=c(gfdl_historical_fl,ipsl_historical_fl,had_historical_fl),f=c(gfdl_future_fl,ipsl_future_fl,had_future_fl))

# apply to everything

purrr::pwalk(list(fl_tbl$h,fl_tbl$f),merge_esm_past_future)

# we do additive deltas for everything except chl
mergedfls <- list.files("merged_ESMs",full.names = T) %>% str_subset("chl",negate=TRUE)
chlfls <- list.files("merged_ESMs",full.names = T) %>% str_subset("chl")
purrr::walk(mergedfls,calc_esm_delta)
purrr::walk(chlfls,calc_esm_proportional_delta)
