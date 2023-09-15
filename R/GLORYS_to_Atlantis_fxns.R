# Functions to use CDO to interpolate from GLORYS to daily Atlantis
library(here)
library(tictoc)
library(ncdf4)
library(RNetCDF)
# data and join keys
source(here('GLORYS_ESM_data','scripts','GLORYS_to_Atlantis_preprocessing.R'))

#### Function that uses CDO to interpolate GLORYS to 12h time steps and across Atlantis depth layers ####
### Can be used for state variable files or flux files
### Note: the downscaled GLORYS files are in monthly resolution, so we want to pull in the
### last timestep of the previous year's data and the first timestep of the next year's data
### so that we can output a full year of data

interpolate_ncdf <- function(infile){
  
  #temporary files for CDO work
  tmpfile1 <- "/home/atlantis/GLORYS_ESM_data/scratch_files/atlantis_cat_tmp.nc"
  tmpfile2 <- "/home/atlantis/GLORYS_ESM_data/scratch_files/atlantis_retime_tmp.nc"
  tmpfile3 <- "/home/atlantis/GLORYS_ESM_data/scratch_files/atlantis_interp_tmp.nc"
  # Remove any old scratch files
  if(file.exists(tmpfile1))file.remove(tmpfile1)
  if(file.exists(tmpfile2))file.remove(tmpfile2)
  if(file.exists(tmpfile3))file.remove(tmpfile3)
  
  # year of file
  yr <- str_sub(infile,start=str_length(infile)-6,end=str_length(infile)-3)
  
  # can only concatenate years that exist
  if(yr<2007 | yr>2099) stop("This GLORYS translation is designed for input files between years 2014 and 2099.")
  
  prevyr <- as.character(as.numeric(yr)-1)
  prevyrfn <- str_replace(infile,yr,prevyr)
  nextyr <- as.character(as.numeric(yr)+1)
  nextyrfn <- str_replace(infile,yr,nextyr)
  
  #concatenate last year, this year, and next year's files
  # tic("Concatenating netCDFs")
  cmd1 <- paste0('sudo cdo -cat ',prevyrfn,' ',infile,' ',nextyrfn,' ',tmpfile1)
  system(cmd1)
  # toc()
  
  # change time axis to seconds
  # and set reference time to 2013-01-01 00:00:00
  # tic("Re-dimensioning time")
  # taxis_start <- paste0(as.integer(yr)-1,"-01-01,00:00:00")
  taxis_start <- "2013-01-01,00:00:00"
  tshift <- as.integer(yr)-2013-1
  cmd2 <- paste0('sudo cdo -shifttime,',tshift,'years -setreftime,2013-01-01,00:00:00,seconds -settaxis,',taxis_start,',1mon ',tmpfile1,' ',tmpfile2)
  system(cmd2)
  # toc()
  
  # interpolate values at Atlantis box depth layers (linear interpolation, cdo cmd -intlevel)
  cmd3 <- paste0('sudo cdo -intlevel,',paste(-atlantis_z_mid$midz,collapse=','),' ',tmpfile2,' ',tmpfile3)
  system(cmd3)
  # toc()
  
  # tic("Extracting data from ncdf")
  out <- tidync(tmpfile3) %>% hyper_tibble
  # toc()
  
  return(out)
}

# x <- interpolate_ncdf(infile)

#### Function to join interpolated data to appropriate Atlantis polygon boxes and faces ####
### Calls the previous function to interpolate ncdfs first
### For state variables (what="boxes") (e.g., salinity, temperature, etc.), outputs a
### summary by box, time, and depth layer
### For fluxes (u or v), outputs a data frame of flux by face, time, and depth layer

join_to_atlantis_geometry <- function(infile,what="boxes"){
  
  # tic("Processing input ncdfs")
  vn <- tidync(infile) %>% hyper_vars() %>% pull('name')
   # depth should be negative, longitude should be -180 to 180
  x <- interpolate_ncdf(infile) %>% 
    mutate(depth_interp=-depth)
  
  # lat/lon convention
  if(all(x$longitude>0)){
    x <- x %>% mutate(longitude=-(360-longitude))
  }
  
  x <- x %>% 
    left_join(atlantis_z_mid,by=c('depth_interp'='midz'))
  # toc()
  
  # join the GLORYS grid index
  # tic("Adding Atlantis grid reference")
  y <- x %>% left_join(latlon_grd,by=c("longitude","latitude"))
  # toc()
  
  # join to boxes
  if(!(what%in%c("boxes","faces"))) stop("param `what` should be in c('boxes','faces')")
  if(what=="boxes"){
    # tic("Summarizing by box, depth, and day")
    out <- boxes_glorys_join_with_depth %>% 
      # if boxes have dz==0, they are not part of the model (too deep)
      filter(dz!=0) %>% 
      # join interpolated data
      left_join(y,by=c('grididx','atlantis_layer')) %>% 
      # summarise variables of interest ('vn' from above) by box, depth layer, and time
      group_by(.bx0,time,atlantis_layer) %>%
      summarise({{vn}}:=mean(.data[[vn]],na.rm=T)) %>% 
      ungroup()
    # toc()
  }
  if(what=="faces"){
    # join to faces. For GLORYS, u and v points are at the same coordinates, unlike ROMS
    # tic("Joining to faces by grid index and depth layer")
    out<- faces_u_join_with_depth %>% 
      left_join(y,by=c('grididx','atlantis_layer')) %>% 
      # summarise by face, depth layer, and time
      group_by(.fx0,time,atlantis_layer) %>%
      summarise({{vn}}:=mean(.data[[vn]],na.rm=T)) %>% 
      ungroup()
    # toc()
  }
  
  out
}

# y <- infile %>% join_to_atlantis_geometry(what="faces")

#### Calculate orthogonal fluxes across faces ####
### Using u and v extracted with the function above, join u and v data, 
### then calculate the flux of water orthogonal to each face using trigonometry
### Sign convention: for now, we calculate such that a positive flux is R to L 
### (i.e., box to right of face flowing to box to left of face)
### Function then multiplies orthogonal flux by the area of the face to calculate 
### total flux per time step across each face, in m^3/s
### OPTION: correct for hyperdiffusion using distances from faces to adjoining boxes
### NOTE: this does not yet include vertical flows, those are balanced in the next function
### Output: data frame with 4 variables: face, time, atlantis layer, gross flux in m3/s

GLORYS_to_Atlantis_fluxes <- function(this_year,esm,hyperdiffusion=T){
  # this was written to work for particular ESMs for CalCurrent- could be more generalized
  esmfn <- switch(esm,"GFDL"="GFDL","Hadley"="HadGEM2","IPSL"="IPSL-CM5A")
  # get file names to process
  ifiles <- list.files('~/GLORYS_ESM_data/delta_method_final_outputs',full.names = T) %>% 
    str_subset('_delta_',negate = T) %>% 
    str_subset('zos',negate = T) %>% 
    str_subset("proportional",negate = T) %>% 
    str_subset(paste0(this_year,".nc")) %>% 
    str_subset(esmfn)
  
  # tic("Extracting u and v")
  #fluxes
  u <- ifiles %>% str_subset("uo") %>% join_to_atlantis_geometry(what='faces')
  v <- ifiles %>% str_subset("vo") %>% join_to_atlantis_geometry(what='faces')
  # toc()
  
  # tic("Joining u and v")
  uv <- u %>% left_join(v,by = c(".fx0", "atlantis_layer","time")) %>% 
    left_join(faces_angles,by=c(".fx0"))
  # toc()
  
  tic("Calculating orthogonal flux")
  # get the component of the flux that is perpendicular to the face. Because of the angle transformation (+/- 2*pi), 
  # multiplying by the sine of the angle between uv and the face returns the correct sign for the flux: negative for L to R, positive for R to L.
  
  uv_interp <- uv %>% 
    dplyr::select(.fx0,time,atlantis_layer,face_angle,uo_glor,vo_glor) %>%
    mutate(uv_angle=atan2(vo_glor, uo_glor),uv_mag=sqrt(uo_glor^2+vo_glor^2)) %>%
    mutate(diff_angle = uv_angle - face_angle, # beta - alpha
           angle_new = ifelse((diff_angle>=0 & diff_angle<=pi) | (diff_angle<0 & diff_angle>=-pi), diff_angle, 
                              ifelse(diff_angle < -pi, diff_angle+2*pi, diff_angle-2*pi)),# add or subtract 2*pi as needed)
           orthogonal_flux = uv_mag * sin(angle_new)) %>% 
    ungroup()
  
  fluxes_interp <- uv_interp %>% 
    left_join(faces_u_join_with_depth,by=c(".fx0","atlantis_layer")) %>% 
    mutate(gross_flux_m3_s = orthogonal_flux * face_area) %>%
    select(.fx0,time,atlantis_layer, gross_flux_m3_s)
  
  # BIG CHOICE HERE: correct for hyperdiffusion or not?
  if(hyperdiffusion){
    fluxes_interp <- fluxes_interp %>% 
      left_join(hd_corrections,by=".fx0") %>% 
      # if flux is positive, it is going R to L, so we use the "left side" correction factor, and vice versa
      mutate(flux_corrected=ifelse(gross_flux_m3_s>0,gross_flux_m3_s/left_distance,gross_flux_m3_s/right_distance)) %>% 
      dplyr::select(.fx0,time,atlantis_layer,gross_flux_m3_s=flux_corrected)
  }
  toc()
  
  return(fluxes_interp)
}

test_no_hd <- GLORYS_to_Atlantis_fluxes(2013,"IPSL",hyperdiffusion = F)
test_hd <- GLORYS_to_Atlantis_fluxes(2013,"IPSL")

#### Calculate source and destination cells and flows ####
### Atlantis needs fluxes organized by destination box and
### destination depth layer, so we need to sum up the net
### flows into and out of each box/depth cell
### Function takes the output of the previous function above

### For vertical flux (between depth layers) we will have to
### calculate it ourselves as a difference between horizontal fluxes

flux_to_exchanges <- function(interpolated_fluxes){

  # organize fluxes by source and destination boxes
  source_dest_org <- interpolated_fluxes %>% 
    left_join(faces %>% dplyr::select(.fx0,left,right),by=".fx0") %>%
    # because flux is positive R to L, we call right boxes "sources" and left boxes "destinations"
    # flux should be positive when it is LEAVING a source cell
    rename(source_box=right,dest_box=left)
  
  # calc summarized fluxes from source boxes/layers
  source_fluxes <- source_dest_org %>%
    group_by(time,source_box,atlantis_layer) %>%
    # sign convention means that positive fluxes will indicate water leaving a source cell
    summarise(net_flux_as_source=sum(gross_flux_m3_s,na.rm=T)) %>%
    ungroup() %>%
    rename(box=source_box)
  
  # calc summarized fluxes to destination boxes/layers
  dest_fluxes <- source_dest_org %>%
    group_by(time,dest_box,atlantis_layer) %>%
    # sign convention means that positive fluxes here will indicate water entering a dest cell
    summarise(net_flux_as_dest=sum(gross_flux_m3_s,na.rm=T)) %>%
    ungroup() %>%
    rename(box=dest_box)
  
  # do vertical flux balancing.
  # Calculating w (vertical flux) is tricky, and we use a workaround
  # vertical flux is used to balance horizontal fluxes, and we use a box-specific algorithm:
  
  # For each box, flux will accumulate downwards. In other words, if there
  # is an excess of water in a cell horizontally, it will cause downwelling into the next-deepest layer
  # If there is excess water in the bottom layer, it gets assigned to box 0 (the boundary box)
  # This is a hack, but is consistent with what others have done for Atlantis
  # sign convention: positive vertical flux is downwelling
  
  # first, calculate net difference between source and sink
  flux_in_out <- source_fluxes %>%
    left_join(dest_fluxes,by = c("time", "box", "atlantis_layer")) %>%
    # the combined flux entering the cell is the flux into the box (as a destination)
    # minus the flux out of the box (as a source)
    mutate(net_flux_into_cell=net_flux_as_dest-net_flux_as_source)
  
  # calculate w (vertical flux)
  calc_w <- flux_in_out %>% 
    group_by(time,box) %>% 
    arrange(atlantis_layer) %>% 
    # add names to keep track of box and depth layer destinations
    mutate(dest_b=box,dest_k=lead(atlantis_layer)) %>% 
    # calculate w as a cumulative sum of horizontal flux diffs, accumulating downwards
    mutate(w_down=cumsum(net_flux_into_cell)) %>% 
    ungroup() %>% 
    arrange(time,box,atlantis_layer) %>% 
    # rename for later joining
    rename(t=time,b=box,z=atlantis_layer) %>% 
    dplyr::select(t,b,z,dest_b,dest_k,exchange=w_down)
  
  # flux out of the deepest layer for each box goes to or comes from box0
  # Calculate what flux we need to send to (or source from) the boundary box
  box0_fluxes <- calc_w %>% 
    group_by(t,b) %>% 
    # find the deepest Atlantis layer for each box
    slice_max(z) %>% 
    ungroup() %>% 
    dplyr::select(t,b,z,exchange) %>% 
    # rename for joining
    mutate(dest_b=0,dest_k=0)
  
  # Now we have all the pieces of flux, we just need to clean up
  # Atlantis expects this in the "destination cell" form.
  # We already have source_dest_org for horizontal fluxes,
  # just need to add in the info about vertical fluxes,
  # as well as fluxes out of the bottom of each box to box0
  
  final_exchanges <- source_dest_org %>%
    rename(t=time,z=atlantis_layer,b=source_box,dest_b=dest_box) %>% 
    mutate(dest_k=z) %>% 
    group_by(t,z,b,dest_b,dest_k) %>% 
    # horizontal source-dest fluxes
    summarise(exchange=sum(gross_flux_m3_s)) %>% 
    ungroup() %>% 
    # attach vertical fluxes
    bind_rows(calc_w %>% drop_na()) %>% 
    # attach fluxes to and from box 0
    bind_rows(box0_fluxes)
  
  # number of destinations, to add the "dest" dimension for Atlantis
  dests <- final_exchanges %>% 
    distinct(b,z,dest_k,dest_b) %>% 
    group_by(b,z) %>% 
    mutate(dest=row_number()) %>% 
    ungroup()

  ### a few big final calculations here: 
  ### 1. convert to total exchange (not flow per time) by multiplying by time step
  ### 3. swap ordering of depth layers for Atlantis input compatibility (highest number is shallowest layer for each box)
  
  # find the timestep.
  # 43200 SECONDS IN 12 HOURS (60*60*12)
  
  # timestep <- interpolated_fluxes %>% distinct(time) %>%
  #   slice(1:2) %>%
  #   mutate(td=lead(time)-time) %>%
  #   drop_na() %>%
  #   pull(td) %>% duration("seconds") %>%
  #   time_length() %>% unique()

  timestep <- 43200
  
  out <- final_exchanges %>% 
    # attach dest dimension
    left_join(dests,by = c("z", "b", "dest_b", "dest_k")) %>% 
    arrange(t,b,z,dest) %>% 
    
  # hyperdiffusion correction here; divide all fluxes by box area
    # EDIT 04/08/2022 MOVED THE HYPERDIFFUSION OPTION UP TO THE PREVIOUS FUNCTION
    # left_join(box_areas,by=('dest_b'='b')) %>%
    # mutate(exchange=exchange/area) %>%
    
  # multiply by the timestep length to get total exchange instead of rate
    mutate(exchange_m3=exchange*timestep) %>% 
  
  # fix depth layer naming to fit Atlantis (for both sources and dests)
    # first, join to source variables (b and z)
    left_join(depth_swap,by=c('b'='.bx0','z'='atlantis_layer')) %>% 
    mutate(z=atlantis_input_depth_layer) %>%
    dplyr::select(-atlantis_input_depth_layer) %>% 
    # then, join to destination variables(dest_b,dest_k)
    # and reconfigure depth layer naming
    left_join(depth_swap,by=c('dest_b'='.bx0','dest_k'='atlantis_layer')) %>% 
    mutate(dest_k=atlantis_input_depth_layer) %>% 
    mutate(dest_k=replace_na(dest_k,0)) %>% 
    
    dplyr::select(dest_b,dest_k,exchange_m3,dest,z,b,t)
  
  
  out
  
}

# flux2013 <- GLORYS_to_Atlantis_fluxes(2013,"GFDL") %>% flux_to_exchanges()

# Comparison
# oldhydro <- here("Oceanography2013","hydro2013","CalCurrent89_hydro2013_729steps1.nc")
# test <- tidync(oldhydro) %>% 
#   hyper_filter(t=index==1) %>% 
#   hyper_tibble()

#### Pack Hydro NetCDF ####
### All in one function to apply the functions above and then pack into a netCDF for Atlantis hydro ###
pack_hydro_ncdf<-function(this_year,esm,hyperdiffusion=T){
  
  tic("Calculating box to box exchanges")
  exchanges <- GLORYS_to_Atlantis_fluxes(this_year,esm,hyperdiffusion = hyperdiffusion) %>% flux_to_exchanges()
  toc()
  
  # find the timestep
  tic("Making exchange arrays")
  exchanges <- exchanges %>% filter(!is.na(t))
  timestep <- exchanges %>% distinct(t) %>% slice(1:2) %>%  
    mutate(td=lead(t)-t) %>% drop_na() %>% 
    pull(td) %>% duration("seconds") %>% 
    time_length() %>% unique()
  
  nz <- length(unique(atlantis_depths$atlantis_layer))
  nt <- length(unique(exchanges$t))
  nb <- length(unique(atlantis_sf$.bx0))
  ndest <- max(exchanges$dest,na.rm=T)
  
  # complete empty exchanges to make a full array
  full_exch <- exchanges %>% 
    filter(!is.na(t)) %>% 
    complete(t,b=full_seq(c(0,nb-1),1),z=full_seq(c(0,nz-1),1),dest=full_seq(c(1,ndest),1)) %>%    
  # sensitive point- have to arrange this way so the stuff goes into the array in the right way
  # RIGHT-MOST ARRANGE HERE SHOULD BE FIRST dim ARGUMENT OF ARRAY PACKING (because it varies fastest)
  # in other words, I think we need to reverse the order of indexing variables from tibble to array
  # here, we arrange by time, then box, then depth
  # but then pack the array with dimensions (nz,nb,nt) [depth,box,time]
    arrange(t,b,z,dest)
  
  dest_b_arr <- array(full_exch$dest_b,dim=c(ndest,nz,nb,nt))
  dest_k_arr <- array(full_exch$dest_k,dim=c(ndest,nz,nb,nt))
  exchange_arr <- array(full_exch$exchange_m3,dim=c(ndest,nz,nb,nt))
  time_arr <- array(unique(full_exch$t))
  
  toc()
  
  # Here we need to write out the variables and the flows to a NetCDF file.
  
  # info on the run
  this_geometry <- "CalCurrentV3_utm.bgm"
  this_title <- "Advection between boxes"
  
  # d_units <- "depth layers"
  
  #options exchanges
  ex_units <- "m3"
  
  #options time dimension
  timestep.nc <- as.duration(timestep) %>% as.numeric('seconds')
  t_units <- "seconds since 2013-01-01 00:00:00" # should it be absolute or relative to the file?
  # time.unit.length <- 2 # years
  # seconds_timestep <- 60*60*12
  # time_array <- array((0:(nt-1))*timestep.nc)
  
  # find the year again
  yrchr <- this_year %>% as.character()
  nc_name <- paste0(here("GLORYS_ESM_data","GLORYS_Atlantis_translation","CalCurrent_GLORYS_"),esm,'_hydro_',yrchr,".nc")
  tmpfile4 <- "/home/atlantis/GLORYS_ESM_data/scratch_files/atlantis_interp_time_tmp.nc"
  # temporary .nc for monthly, then convert to 12h using CDO (below)
  
  tic(paste("Packing netCDF",nc_name))
  
  nc_file <- create.nc(tmpfile4,clobber=T)
  
  dim.def.nc(nc_file, "t", unlim=TRUE)
  dim.def.nc(nc_file, "b", nb) 
  dim.def.nc(nc_file, "z", nz) 
  dim.def.nc(nc_file, "dest", ndest)
  
  var.def.nc(nc_file, "t", "NC_DOUBLE", "t")
  var.def.nc(nc_file, "exchange", "NC_DOUBLE", c("dest","z","b","t"))
  var.def.nc(nc_file, "dest_b", "NC_INT", c("dest","z","b","t"))
  var.def.nc(nc_file, "dest_k", "NC_INT", c("dest","z","b","t"))
  
  att.put.nc(nc_file, "exchange", "_FillValue", "NC_DOUBLE", 0)
  att.put.nc(nc_file, "dest_b", "_FillValue", "NC_INT", -1)
  att.put.nc(nc_file, "dest_k", "_FillValue", "NC_INT", -1)
  att.put.nc(nc_file, "exchange", "units", "NC_CHAR", ex_units)
  att.put.nc(nc_file, "t", "units", "NC_CHAR", t_units)
  # att.put.nc(nc_file, "t", "dt", "NC_DOUBLE", timestep.nc)
  att.put.nc(nc_file, "NC_GLOBAL", "title", "NC_CHAR", this_title)
  att.put.nc(nc_file, "NC_GLOBAL", "geometry", "NC_CHAR", this_geometry)
  att.put.nc(nc_file, "NC_GLOBAL", "parameters", "NC_CHAR", "")
  
  var.put.nc(nc_file, "t", time_arr)
  var.put.nc(nc_file, "dest_b", dest_b_arr)
  var.put.nc(nc_file, "dest_k", dest_k_arr)
  var.put.nc(nc_file, "exchange", exchange_arr)
  
  close.nc(nc_file)
  toc()
  
  tic("Interpolating to 12 hour")
  # interpolate to 12h time, then select just this year's dates
  strt <- paste0(yrchr,"-01-01,00:00:00")
  cmd <- paste0('sudo cdo -selyear,',yrchr,' -inttime,',strt,',','12hour ',tmpfile4,' ',nc_name)
  system(cmd)
  
  # also have to set dt attribute of t, and parameters global attribute because of interpolation
  # do this with nco
  cmd2 <- paste0('sudo ncatted -a dt,t,c,d,43200 ',nc_name)
  system(cmd2)
  # 
  cmd3 <- paste0('sudo ncatted -a parameters,global,c,c,"" ',nc_name)
  system(cmd3)
  
  toc()
  
}

#### Pack NetCDF for State Variables ####
## also creates a sediment layer for each box that has the same statevar value as the deepest water layer
# inputs: year you'd like
# esm ("GFDL","IPSL",or "Hadley")
# statevar (one of: chl,so,si,thetao,no3,o2)

pack_statevars_ncdf <- function(this_year,esm,statevar){
  esmfn <- switch(esm,"GFDL"="GFDL","Hadley"="HadGEM2","IPSL"="IPSL-CM5A")
  ifiles <- list.files('~/GLORYS_ESM_data/delta_method_final_outputs',full.names = T) %>% 
    str_subset('_delta_',negate = T) %>% 
    str_subset('zos',negate = T) %>% 
    str_subset("proportional",negate = T) %>% 
    str_subset(paste0(this_year,".nc")) %>% 
    str_subset(esmfn)
  
  # concatenate ncdfs, redimension, and interpolate variable
  tic("Extracting state variable")
  vardat <- ifiles %>% str_subset(statevar) %>% join_to_atlantis_geometry(what='boxes')
  
  # rename and prepare arrays for netCDF
  vardat2 <- vardat %>%
    set_names(c("b","t","atlantis_layer",statevar)) %>% 
    left_join(depth_swap,by=c('b'='.bx0','atlantis_layer')) %>% 
    mutate(z=atlantis_input_depth_layer) %>%
    dplyr::select(-atlantis_input_depth_layer,-atlantis_layer) %>% 
    filter(!is.na(t))
  toc()
  
  # find the timestep
  
  tic("Making variable arrays")
  timestep <- vardat2 %>% distinct(t) %>% slice(1:2) %>%  
    mutate(td=lead(t)-t) %>% drop_na() %>% 
    pull(td) %>% duration("seconds") %>% 
    time_length() %>% unique()
  
  nz <- length(unique(atlantis_depths$atlantis_layer))+1 # need an extra layer for sediment
  nt <- length(unique(vardat2$t))
  nb <- length(unique(atlantis_sf$.bx0))
  
  # Create sediment layer by copying the bottom depth layer (where z==0)
  sediment <- vardat2 %>%
    filter(z==0) %>%
    mutate(z=nz-1) # our layers start at 0, not 1, so the max layer will be nz-1
  
  # complete empty exchanges to make a full array
  full_vardat <- vardat2 %>% 
    bind_rows(sediment) %>% 
    complete(t,b=full_seq(c(0,nb-1),1),z=full_seq(c(0,nz-1),1)) %>%
    # sensitive point- have to arrange this way so the stuff goes into the array in the right way
    # RIGHT-MOST ARRANGE HERE SHOULD BE FIRST dim ARGUMENT OF ARRAY PACKING (because it varies fastest)
    arrange(t,b,z)
  # in other words, I think we need to reverse the order of indexing variables from tibble to array
  # here, we arrange by time, then box, then depth
  # but then pack the array with dimensions (nz,nb,nt)
  var_arr <- full_vardat %>% 
    pull(statevar) %>% 
    array(dim=c(nz,nb,nt))
  
  time_arr <- array(unique(full_vardat$t))
  
  toc()
  
  # Here we need to write out the state variables to a NetCDF file.
  
  #nicer names for state variables
  varname <- switch(statevar,
                    "chl"="chlorophyll",
                    "so"="salinity",
                    "si"="silicate",
                    "thetao"="temperature",
                    "no3"="nitrate",
                    "o2"="oxygen")
  # units definition
  varunits <- switch(statevar,
                      "chl"="mg m-3",
                      "so"="1e-3",
                      "si"="mmol m-3",
                      "thetao"="degrees_C",
                      "no3"="mmol m-3",
                      "o2"="mmol m-3")
  # info on the run
  this_geometry <- "CalCurrentV3_utm.bgm"
  this_title <- paste(varname,"in boxes")
  
  # d_units <- "depth layers"
  
  
  #options time dimension
  # timestep.nc <- as.duration(timestep) %>% as.numeric('seconds')
  t_units <- "seconds since 2013-01-01 00:00:00" # should it be absolute or relative to the file?
  # time.unit.length <- 2 # years
  # seconds_timestep <- 60*60*12
  # time_array <- array((0:(nt-1))*timestep.nc)
  
  
  # find the year again
  yrchr <- this_year %>% as.character(.)
  nc_name <- paste0(here("GLORYS_ESM_data","GLORYS_Atlantis_translation","CalCurrent_GLORYS_"),esm,'_',varname,'_',yrchr,".nc")
  tmpfile4 <- "/home/atlantis/GLORYS_ESM_data/scratch_files/atlantis_interp_time_tmp.nc"
  # temporary .nc for monthly, then convert to 12h using CDO (below)
  
  tic(paste("Packing netCDF",nc_name))
  nc_file <- create.nc(tmpfile4,clobber=T)
  
  dim.def.nc(nc_file, "t", unlim=TRUE)
  dim.def.nc(nc_file, "b", nb) 
  dim.def.nc(nc_file, "z", nz)
  
  var.def.nc(nc_file, "t", "NC_DOUBLE", "t")
  var.def.nc(nc_file, varname, "NC_DOUBLE", c("z","b","t"))
  
  att.put.nc(nc_file, varname, "_FillValue", "NC_DOUBLE", 0) # WHAT FILL VALUE TO USE?
  att.put.nc(nc_file, varname, "units", "NC_CHAR", varunits)
  att.put.nc(nc_file, "t", "units", "NC_CHAR", t_units)
  # att.put.nc(nc_file, "t", "dt", "NC_DOUBLE", timestep.nc) # re-adding this later because of later interpolation
  att.put.nc(nc_file, "NC_GLOBAL", "title", "NC_CHAR", this_title)
  att.put.nc(nc_file, "NC_GLOBAL", "geometry", "NC_CHAR", this_geometry)
  # att.put.nc(nc_file, "NC_GLOBAL", "parameters", "NC_CHAR", "") #re-adding later
  
  var.put.nc(nc_file, "t", time_arr)
  var.put.nc(nc_file, varname, var_arr)
  
  close.nc(nc_file)
  
  toc()
  
  tic("Interpolating to 12 hour")
  # interpolate to 12h time, then select just this year's dates
  strt <- paste0(yrchr,"-01-01,00:00:00")
  cmd <- paste0('sudo cdo -selyear,',yrchr,' -inttime,',strt,',','12hour ',tmpfile4,' ',nc_name)
  system(cmd)
  
  # also have to set dt attribute because of interpolation
  # do this with nco
  cmd2 <- paste0('sudo ncatted -a dt,t,c,d,43200 ',nc_name)
  system(cmd2)
  cmd3 <- paste0('sudo ncatted -a parameters,global,c,c,"" ',nc_name)
  system(cmd3)
  toc()
}

# pack_statevars_ncdf(2013,"IPSL","thetao")
# pack_statevars_ncdf(2013,"IPSL","so")
# pack_hydro_ncdf(2013,"IPSL",hyperdiffusion = T)

# z <- tidync(here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_IPSL_salinity_2013.nc'))
# compsalt <- here("Oceanography2063","CalCurrent89_salt2063_729steps1.nc") %>% tidync()
# test <- compsalt %>% hyper_filter(b=b==5) %>% hyper_filter(t=index==3) %>% hyper_tibble()
# gsalt <- here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_IPSL_salinity_2013.nc') %>% tidync()
# test2 <- gsalt %>% hyper_filter(b=b==5) %>% hyper_filter(t=index==3) %>% hyper_tibble()
