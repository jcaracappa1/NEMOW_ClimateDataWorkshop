## Purpose: Add the ESM delta to the GLORYS climatology
## NEW idea: split ESM deltas into annual files
## then, change time index for glorys climatology into temporary file
## finally, add the climatology to each ESM delta annual file

#load dependencies
library(tidync)
library(tidyverse)
library(tictoc)
library(here)
library(ncdf4)

calc_glorys_final <- function(esmfl,glorysfl,timestep, yrs=1990:2054,type='add'){
  tic(paste("Calculating final output from",esmfl,"and",glorysfl))
  # first, split esm file by year into separate files
  newflext <- str_replace(esmfl,".nc","_")
  if(!dir.exists(here('delta_method_final_outputs'))){
    dir.create(here('delta_method_final_outputs'))
  }
  if(!dir.exists(here('GLORYS_ESM_final'))){
    dir.create(here('GLORYS_ESM_final'))
  }
  #separates singe file into multiple years
  cmd1 <- paste0('sudo cdo splityear ',here(),'/ESM_climatology_warp_glorys/',esmfl,' ',here(),'/delta_method_final_outputs/',newflext)
  
  system(cmd1)
  
  system(paste0('sudo chmod 775 ',here(),'/delta_method_final_outputs/*'))
  
  # then, for each year, calculate and write a final converted file for that variable and year
  for(i in 1:length(yrs)){
    
    yr=yrs[i]
    
    #Reset reference time for files
    cmd2 <- paste0("sudo cdo setreftime,1850-01-01,00:00:00,1day ",here(),'/GLORYS_climatology/',glorysfl,' ',here(),'/delta_method_final_outputs/tmp1.nc')
    system(cmd2)
    
    ofile <-  str_replace(newflext,"delta","glorys")
    
    # fix degenerate variable created when climatology was averaged, because it messes up adding
    cmd3 <- paste0('sudo ncks -O -x -v average_DT ',here(),'/delta_method_final_outputs/',newflext,yr,'.nc ',here(),'/delta_method_final_outputs/',newflext,yr,'.nc')
    system(cmd3)
    
    # if 'type' specified is 'add' (the default) and if monthly timestep, use ymonadd
    # if 'type' specified is 'add' (the default) and if timestep is yearly use add
    # if 'type' specified is NOT 'add' and if monthly timestep, use ymonmul (multiplicative delta)
    # if 'type' specified is 'add' (the default) and if timestep is yearly use mul
    
    #change file permissions for temp files
    system(paste0('sudo chmod 775 ',here(),'/delta_method_final_outputs/tmp1.nc'))
    system(paste0('sudo chmod 775 ',here(),'/delta_method_final_outputs/',newflext,yr,'.nc'))

    if(timestep=='month'){
      fxn <- ifelse(type=="add",'ymonadd','ymonmul')
      cmd4 <- paste0('sudo cdo ',fxn,' ',here(),'/delta_method_final_outputs/',newflext,yr,'.nc ',here(),'/GLORYS_climatology/',glorysfl,' ',here(),'/GLORYS_ESM_final/',ofile,yr,'.nc')
    }else{
      fxn <- ifelse(type=="add",'add','mul')
      cmd4 <- paste0('sudo cdo ',fxn,' ',here(),'/delta_method_final_outputs/tmp1.nc ',here(),'/delta_method_final_outputs/tmp2.nc ',here(),'/delta_method_final_outputs/',ofile,yr,'.nc')
    }
    system(cmd4)
  }
  toc()
}

# Table of files to apply this to (only do the files that line up right now, which are no3,o2,si,so,thetao,uo,vo,and zos)
esmfls <- list.files(here('ESM_climatology_warp_glorys'))[grepl('_delta_',list.files(here('ESM_climatology_warp_glorys')))]
esmfls <- esmfls[str_detect(esmfls,"\\d.nc$",negate=T)|str_detect(esmfls,"o2.nc$",negate=F)|str_detect(esmfls,"no3.nc$",negate=F)]
glorysfls <- list.files(here('GLORYS_climatology'))[grepl('.nc',list.files(here('GLORYS_climatology')))]

tbl_to_calc <- tibble(esmfl=esmfls) %>% 
  mutate(glorysfl=glorysfls[1]) %>% 
  mutate(timestep='month') %>% 
  mutate(type='add')

## apply!
calc_glorys_final(esmfl = tbl_to_calc$esmfl[1],
                  glorysfl = tbl_to_calc$glorysfl[1],
                  timestep = tbl_to_calc$timestep[1],
                  yrs=1990:2054,
                  type='add'
                  )

# purrr::pwalk(tbl_to_calc %>% slice(-(1:10)),calc_glorys_final)