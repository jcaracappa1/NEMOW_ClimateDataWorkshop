library(tidync)
library(tidyverse)
library(tictoc)
library(here)
library(ncdf4)


## Purpose: Add the ESM delta to the GLORYS climatology

## NEW idea: split ESM deltas into annual files
## then, change time index for glorys climatology into temporary file
## finally, add the climatology to each ESM delta annual file


# seems to work if we just use cmd 'add' for the annual files

# function to do this
calc_glorys_final <- function(esmfl,glorysfl,timestep, yrs=1990:2054,type='add'){
  tic(paste("Calculating final output from",esmfl,"and",glorysfl))
  # first, split esm file by year into separate files
  newflext <- str_replace(esmfl,".nc","_")
  if(!dir.exists(here('delta_method_final_outputs'))){
    dir.create(here('delta_method_final_outputs'))
  }
  cmd1 <- paste0('sudo cdo splityear ',here(),'/ESM_climatology_warp_glorys/',esmfl,' ',here(),'/delta_method_final_outputs/',newflext)
  # system(paste0('sudo chmod 775 ',here(),'/delta_method_final_outputs/*'))
  system(cmd1)
  system(paste0('sudo chmod 775 ',here(),'/delta_method_final_outputs/*'))
  
  # then, for each year, calculate and write a final converted file for that variable and year
  for(i in 1:length(yrs)){
    
    yr=yrs[i]
    
    # cmd2 <- paste0('sudo cdo setyear,',yr,' ',here(),'/GLORYS_climatology/',glorysfl,' ',here(),'/delta_method_final_outputs/tmp1.nc')
    cmd2 <- paste0("sudo cdo setreftime,1850-01-01,00:00:00,1day ",here(),'/GLORYS_climatology/',glorysfl,' ',here(),'/delta_method_final_outputs/tmp1.nc')
    system(cmd2)
    # cmd2b <- paste0("sudo cdo -f nc -settaxis,",yr,"-12-13,12:00:00,1month ",here(),'/delta_method_final_outputs/tmp1.nc ',here(),'/delta_method_final_outputs/tmp1b.nc')
    # cm2b <- paste0("sudo cdo chname,depth,lev ",here(),'/delta_method_final_outputs/tmp1.nc ',here(),'/delta_method_final_outputs/tmp1b.nc')
    # system(cmd2b)
    
    # t1 = tidync(here('delta_method_final_outputs','tmp1b.nc'))
    # t1.df = t1 %>% hyper_tibble()
    # sort(unique(t1.df$time))
    # nc_open(here('delta_method_final_outputs','tmp1b.nc'))%>%ncatt_get('time')
    ofile <-  str_replace(newflext,"delta","glorys")
    
    # fix degenerate variable created when climatology was averaged, because it messes up adding
    cmd3 <- paste0('sudo ncks -O -x -v average_DT ',here(),'/delta_method_final_outputs/',newflext,yr,'.nc ',here(),'/delta_method_final_outputs/tmp2.nc')
    system(cmd3)
    
    # t2 =tidync(here('delta_method_final_outputs',paste0(newflext,yr,'.nc')))
    # t2.df = t2%>%hyper_tibble()
    # sort(unique(t2.df$time))
    # t2 = nc_open(here('delta_method_final_outputs',paste0(newflext,yr,'.nc')))
    # esm.time = t2$dim$time$vals
    # as.POSIXct(esm.time*86400,origin = '1991-01-01 00:00:00',tz = 'UTC')

    
    # if 'type' specified is 'add' (the default) and if monthly timestep, use ymonadd
    # if 'type' specified is 'add' (the default) and if timestep is yearly use add
    # if 'type' specified is NOT 'add' and if monthly timestep, use ymonmul (multiplicative delta)
    # if 'type' specified is 'add' (the default) and if timestep is yearly use mul
    
    system(paste0('sudo chmod 775 ',here(),'/delta_method_final_outputs/tmp1.nc'))
    system(paste0('sudo chmod 775 ',here(),'/delta_method_final_outputs/tmp2.nc'))

    if(timestep=='month'){
      fxn <- ifelse(type=="add",'ymonadd','ymonmul')
      cmd4 <- paste0('sudo cdo ',fxn,' ',here(),'/delta_method_final_outputs/tmp2.nc ',here(),'/GLORYS_climatology/',glorysfl,' ',here(),'/delta_method_final_outputs/',ofile,yr,'.nc')
      # cmd4 <- paste0('sudo cdo ',fxn,' ',here(),'/delta_method_final_outputs/tmp1b.nc ',here(),'/delta_method_final_outputs/tmp2.nc ',here(),'/delta_method_final_outputs/',ofile,"_",yr,'.nc')
    }else{
      fxn <- ifelse(type=="add",'add','mul')
      cmd4 <- paste0('sudo cdo ',fxn,' ',here(),'/delta_method_final_outputs/tmp1.nc ',here(),'/delta_method_final_outputs/tmp2.nc ',here(),'/delta_method_final_outputs/',ofile,yr,'.nc')
    }
    system(cmd4)
  }
  toc()
}

# x = tidync('/home/azureuser/NEMOW/delta_method_final_outputs/tmp1.nc') %>% hyper_tibble() %>% filter( depth <1 & time < 352000)
  
# Table of files to apply this to (only do the files that line up right now, which are no3,o2,si,so,thetao,uo,vo,and zos)
esmfls <- list.files(here('ESM_climatology_warp_glorys'))[grepl('_delta_',list.files(here('ESM_climatology_warp_glorys')))]

# esmfls <- esmfls %>% str_subset("chl.nc"|"no3.nc")
esmfls <- esmfls[str_detect(esmfls,"\\d.nc$",negate=T)|str_detect(esmfls,"o2.nc$",negate=F)|str_detect(esmfls,"no3.nc$",negate=F)]
glorysfls <- list.files(here('GLORYS_climatology'))[grepl('.nc',list.files(here('GLORYS_climatology')))]
tbl_to_calc <- tibble(esmfl=esmfls) %>% 
  mutate(glorysfl=glorysfls[1]) %>% 
  mutate(timestep='month') %>% 
  mutate(type='add')

#View(tbl_to_calc) #check this to make sure it looks right!

## apply!
calc_glorys_final(esmfl = tbl_to_calc$esmfl[1],
                  glorysfl = tbl_to_calc$glorysfl[1],
                  timestep = tbl_to_calc$timestep[1],
                  yrs=1990:2054,
                  type='add'
                  )

# purrr::pwalk(tbl_to_calc %>% slice(-(1:10)),calc_glorys_final)

#### BELOW: Notes and Scratch work ####

#UPDATE 05.25.2022: Fixing proportional chlorophyll
# calc_glorys_final(esmfl = "HadGEM2-ES_delta_chl_proportional.nc",glorysfl = "glorys_chl_clim.nc",timestep = 'year',type = 'mul')
# calc_glorys_final(esmfl = "GFDL-ESM2M_delta_chl_proportional.nc",glorysfl = "glorys_chl_clim.nc",timestep = 'year',type='mul')

# what's going on with no3
# x <- tidync("delta_method/GFDL-ESM2M_delta_no3.nc") %>% 
#   hyper_filter(lev=index==28) %>% 
#   hyper_tibble() %>% 
#   group_by(time) %>% 
#   summarise(meandelta=mean(delta_no3,na.rm=T))
# y <- tidync("esm_climatology/GFDL-ESM2M_delta_no3.nc") %>% 
#   hyper_filter(lev=index==28) %>% 
#   hyper_tibble() %>% 
#   group_by(time) %>% 
#   summarise(meandelta=mean(delta_no3,na.rm=T))
# 
# testfile<-"esm_climatology/GFDL-ESM2M_delta_no3.nc"
# ncdf4::nc_open(testfile)
# testfile2<-"glorys_climatology/glorys_uo_glor_clim.nc"
# ncdf4::nc_open(testfile2)
# 
# testfile3<-"esm_climatology/GFDL-ESM2M_delta_o2.nc"
# ncdf4::nc_open(testfile3)
# testfile4<-"glorys_climatology/glorys_o2_clim.nc"
# ncdf4::nc_open(testfile4)


# NO3, O2, and SI are IN MMOL IN GLORYS AND MOL IN ESM ARRRAGGHHH

# Try using Climate Data Operators
# system("sudo apt-get update")
# system("sudo apt-get install cdo")

# quick CDO help reference
# help
# system("sudo cdo -h")
# operators
# system("sudo cdo --operators")
# system("sudo cdo -h ymonadd")

# get file info
# system("sudo cdo -info <infile>")

# cmd <- paste0("sudo cdo -remapbil,",glorys," ",infile," ",outfile)
# system(cmd)

# test <- (tidync(outfile))

# now try with a delta .rds to ncdf
# library(RNetCDF)
# library(ncdf4)

# GLORYS climatology
# system('sudo cdo -ymonmean NEP_vo_GLORYS2v4_1993-2018.nc glorys_climatology/vo_mean.nc')


# this function converts the dataframe climatology deltas to netCDF files for later use in CDO grid conversion

# df_to_ncdf <- function(fn,templatefn,rootfolder="ESM climatology",newfolder="delta nc"){
#   # read the delta dataset
#   df <- read_rds(paste0(rootfolder,"/",fn))
#   
#   # name of the primary variable
#   vn <- names(df)[1]
#   
#   fullpath <- paste0(rootfolder,"/",newfolder)
#   
#   # copy a template .nc file (with desired grid attributes) to new file
#   file.copy(templatefn,fullpath)
#   newfile <- str_replace(fn,".rds",".nc")
#   file.rename(paste0(fullpath,"/",templatefn),paste0(fullpath,"/",newfile))
#   
#   # join the delta data to the original .nc data
#   newdf <- tidync(paste0(fullpath,"/",newfile)) %>% activate(vn) %>% hyper_tibble(na.rm=F) %>% 
#     left_join(df %>% dplyr::select(any_of(c('i','j','lon','lat','rlon','rlat','time','lev','deltaESM'))))
#   
#   # make the new variable and insert it into the ncdf
#   templatenc <- paste0(fullpath,"/",newfile) %>% nc_open(write=TRUE)
#   poss_dimnames <- c('rlon','rlat',"lon","lat","i","j","lev","time")
#   dimn <- names(newdf)[which(names(newdf) %in% poss_dimnames)]
#   
#   xdim <- templatenc$dim[[dimn[which(dimn %in% c('i','lon','rlon'))]]]
#   ydim <- templatenc$dim[[dimn[which(dimn %in% c('j','lat','rlat'))]]]
#   if("lev" %in% dimn) zdim <- templatenc$dim[['lev']]
#   tdim <- templatenc$dim[['time']]
#   
#   if("lev" %in% dimn){dimlist <- list(xdim,ydim,zdim,tdim)} else{dimlist <- list(xdim,ydim,tdim)}
#   mv <- templatenc$var[[vn]]$missval
#   u <- templatenc$var[[vn]]$units
#   
#   newvn <- paste0('delta_',vn)
#   # define the delta variable
#   var_q <- ncvar_def(newvn, u, dimlist, mv)
#   templatenc <- ncvar_add(templatenc, var_q)
#   # add the values
#   ncvar_put(templatenc,newvn,newdf$deltaESM)
#   ncvar_
#   # write
#   nc_close(templatenc)
# }
# 
# all_clim_fl <- list.files("ESM climatology")
# all_delta_fl  <- all_clim_fl[grepl(".rds",all_clim_fl)&grepl("delta",all_clim_fl)& !(grepl('glorys_grd',all_clim_fl))]
# all_template_fl <- c(gfdl_future_fl,had_future_fl,ipsl_future_fl)
# all_delta_fl <- tibble(fn=all_delta_fl,template=all_template_fl)
# 
# # convert delta dfs to .nc files
# for(i in 1:nrow(all_delta_fl)){
#   df_to_ncdf(all_delta_fl$fn[i],templatefn =all_delta_fl$template[i])
# }

# calc_glorys_final <- function(esmfl,glorysfl){
#   vn <- tidync(esmfl) %>% hyper_vars() %>% pull('name')
#   vng <- tidync(glorysfl) %>% hyper_vars() %>% pull('name')
#   if(grepl('zos',esmfile)) vn <- 'delta_zos'
#   newvn <- str_replace(vn,'delta_','')
#   ofile <- str_replace(esmfl,"_delta_","_glorys_")
#   # cmd1 <- paste0('sudo cdo -chname,',vng,',',newvn,' ',glorysfl,' delta_method/tmp1.nc')
#   # cmd2 <- paste0('sudo cdo -chname,',vn,',',newvn,' ',esmfl,' delta_method/tmp2.nc')
#   cmd3 <- paste0('sudo cdo -chname,',vn,',',newvn,' -ymonadd ',esmfl,' ',glorysfl,' ',ofile)
#   # cmd3 <- paste0('sudo cdo -ymonadd delta_method/tmp2.nc delta_method/tmp1.nc ',ofile)
#   # system(cmd1)
#   # system(cmd2)
#   system(cmd3)
# }
# 
# system(paste0('sudo cdo -splitmon ',glorysfl,' delta_method/glorys_',vng,'_mon'))
# system(paste0('sudo cdo -splitmon ',esmfl,' delta_method/esm_',vn,'_mon'))
# 
# m1 <- 'delta_method/esm_delta_no3_mon07.nc'
# m2 <- 'delta_method/glorys_no3_mon01.nc'
# 
# system(paste0('sudo cdo -add ',m1,' ',m2, ' delta_method/esm_glorys_',vng,'_mon01.nc'))

#cdo ymonadd (Abort): Input streams have different number of variables per timestep!
# system('sudo cdo diffn -select,name=vo delta_method/tmp1.nc -select,name=vo delta_method/tmp2.nc')


# library(cubeview)
# 
# library(stars)
# fi <- here("delta_method_final_outputs","GFDL-CM4_glorys_thetao_2050.nc")
# x = tidync(fi) %>% hyper_tibble()
# summary(x$delta_thetao)
# ggplot(x, aes(x = longitude, y= latitude, color = delta_thetao))+geom_point()
# x <- read_stars(ofile)
# y <- x %>% slice(time,1) %>% st_as_stars()
# cubeview(y)
# 
# t1 = tidync(here("delta_method_final_outputs","tmp1.nc"))
#   # hyper_tibble(select_var = 'thetao')%>%
#   # filter(time == min(time) & depth == min(depth))
# ggplot(t1,aes(x=longitude,y =latitude,color = thetao))+geom_point()
# 
# t2 = tidync(here("delta_method_final_outputs","tmp2.nc"))
#   # hyper_tibble(select_var = 'delta_thetao')%>%
#   # filter(time == min(time) & lev == min(lev))
# ggplot(t2,aes(x=longitude,y =latitude,color = delta_thetao))+geom_point()

# deltanc_to_glorys(all_delta_fl[1])
# try with one year of one file
# first split .nc by year
# system('sudo cdo splityear delta_method/IPSL-CM5A-MR_delta_thetao.nc delta_method/IPSL_glorys_delta_thetao')
# 
# # for a particular year
# yr <- 2096
# system(paste0('sudo cdo setyear,',yr,' glorys_climatology/glorys_vo_glor_clim.nc delta_method/tmp1.nc'))
# system(paste0('sudo cdo ymonadd delta_method/tmp1.nc delta_method/IPSL_glorys_delta_thetao2096.nc delta_method/IPSL_glorys_thetao2096.nc'))
# read_stars("delta_method/IPSL_glorys_thetao2096.nc") %>% slice(time,1) %>% cubeview()
# 
# # what about one that is annual only
# system('sudo cdo splityear delta_method/IPSL-CM5A-MR_delta_no3.nc delta_method/IPSL_glorys_delta_no3')
# yr <- 2006
# system(paste0('sudo cdo setyear,',yr,' glorys_climatology/glorys_no3_clim.nc delta_method/tmp1.nc'))
# system(paste0('sudo cdo add delta_method/tmp1.nc delta_method/IPSL_glorys_delta_no32096.nc delta_method/IPSL_glorys_no32096.nc'))
# read_stars("delta_method/IPSL_glorys_thetao2096.nc") %>% slice(time,1) %>% cubeview()
