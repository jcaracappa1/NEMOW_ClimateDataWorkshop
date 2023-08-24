library(tidync)
library(tidyverse)
library(tictoc)
library(here)


# Convert to GLORYS grid horizontally and vertically

# GLORYS grid file
system(paste0('sudo cdo griddes ',here('GLORYS','GLORYS-Monthly-1993-2000_thetao.nc'),' > glorys3d.grd')) # just need to run this once
system(paste0('sudo cdo griddes ',here('GLORYS','GLORYS-Monthly-1993-2000_zos.nc'),' > glorys2d.grd')) # just need to run this once

# setwd("~/GLORYS_ESM_data")

all_clim_fl <- list.files(here("ESM_climatology"))
all_delta_fl  <- all_clim_fl[grepl(".nc",all_clim_fl)]

# vertical levels as a character string
zlevs <- tidync(here("GLORYS_climatology","glorys_thetao_clim.nc")) %>% activate("D4") %>% hyper_tibble() %>% pull('depth') %>% 
  paste(collapse=",")

system('sudo chmod -R 777 /home/azureuser/NEMOW/*')

# Warp delta ESM to GLORYS grid
deltanc_to_glorys <- function(deltancfn,levs=zlevs){
  is_3d <- ifelse(grepl('zos',deltancfn),F,T)
  infile <- paste0(here('ESM_climatology',deltancfn))
  # vn <- paste0('delta_',str_split(infile,"_")[[1]][2])
  # outfile1 <- paste0(str_sub(infile,start=1,end=-4),"_glorys_horiz.nc")
  if(!dir.exists(here('ESM_climatology_warp_glorys'))){
    dir.create(here('ESM_climatology_warp_glorys'))
  }
  outfile <- str_replace(infile,"ESM_climatology/","ESM_climatology_warp_glorys/")
  
  glorys <- ifelse(is_3d,"glorys3d.grd","glorys2d.grd")
  
  # create piped CDO command to remap in horizontal (distance-weighted using 4 nearest neighbors) and depth (linear) dimensions
  if(!is_3d) {
    cmd <- paste0("sudo cdo -remapdis,",glorys," ",infile," ",outfile)
  } else {
    cmd <- paste0("sudo cdo -intlevel,",zlevs," -remapdis,",glorys," ",infile," ",outfile)
  }
  
  # cmd1 <- paste0("sudo cdo -remapbil,",glorys," -selname,",vn," ",infile," ",outfile1)
  # cmd2 <- paste0("sudo cdo -intlevel,",zlevs," ",outfile1," ",outfile2)
  
  system(cmd)
  
}
# apply to all delta ESM files

deltanc_to_glorys(
  deltancfn = all_delta_fl,
  levs = zlevs
)

# SCRATCH WORK #

#5.25.2022- running proportional chl again
# deltanc_to_glorys('HadGEM2-ES_delta_chl_proportional.nc')
# deltanc_to_glorys('GFDL-ESM2M_delta_chl_proportional.nc')
