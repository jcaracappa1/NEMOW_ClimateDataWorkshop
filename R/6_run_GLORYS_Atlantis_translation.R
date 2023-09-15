# Run GLORYS to Atlantis translation
library(here)
# library(future)
# library(furrr)
source(here('GLORYS_ESM_data','scripts','GLORYS_to_Atlantis_fxns.R'))
# future::plan(multisession)

# job table
ipsl_salinity <-crossing(this_year=c(2013:2099),esm="IPSL",statevar="so")
pwalk(ipsl_salinity,pack_statevars_ncdf)

ipsl_temperature <-crossing(this_year=2013:2099,esm="IPSL",statevar="thetao")
pwalk(ipsl_temperature,pack_statevars_ncdf)

ipsl_hydro <- crossing(this_year=2013:2099,esm="IPSL",hyperdiffusion=T)
pwalk(ipsl_hydro,pack_hydro_ncdf)

gfdl_salinity <-crossing(this_year=c(2013:2099),esm="GFDL",statevar="so")
pwalk(gfdl_salinity,pack_statevars_ncdf)

gfdl_temperature <-crossing(this_year=2013:2099,esm="GFDL",statevar="thetao")
pwalk(gfdl_temperature,pack_statevars_ncdf)

gfdl_hydro <- crossing(this_year=2013:2099,esm="GFDL",hyperdiffusion=T)
pwalk(gfdl_hydro,pack_hydro_ncdf)

had_salinity <-crossing(this_year=c(2013:2099),esm="Hadley",statevar="so")
pwalk(had_salinity,pack_statevars_ncdf)

had_temperature <-crossing(this_year=2013:2099,esm="Hadley",statevar="thetao")
pwalk(had_temperature,pack_statevars_ncdf)

had_hydro <- crossing(this_year=2013:2099,esm="Hadley",hyperdiffusion=T)
pwalk(had_hydro,pack_hydro_ncdf)


# Concatenate
ipsl_salinity_files <- list.files(here("GLORYS_ESM_data","GLORYS_Atlantis_translation"),full.names = T) %>% 
  str_subset("IPSL") %>% str_subset("salinity")%>% str_subset("2013-2099",negate=T) %>% paste(collapse=" ")

system(paste0('sudo cdo -cat ',ipsl_salinity_files,' ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_IPSL_salinity_2013-2099.nc')))

ipsl_temperature_files <- list.files(here("GLORYS_ESM_data","GLORYS_Atlantis_translation"),full.names = T) %>% 
  str_subset("IPSL") %>% str_subset("temperature")%>% str_subset("2013-2099",negate=T) %>% paste(collapse=" ")

system(paste0('sudo cdo -cat ',ipsl_temperature_files,' ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_IPSL_temperature_2013-2099.nc')))

ipsl_hydro_files <- list.files(here("GLORYS_ESM_data","GLORYS_Atlantis_translation"),full.names = T) %>% 
  str_subset("IPSL") %>% str_subset("hydro")%>% str_subset("2013-2099",negate=T) %>% paste(collapse=" ")

system(paste0('sudo cdo -cat ',ipsl_hydro_files,' ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_IPSL_hydro_2013-2099.nc')))

gfdl_salinity_files <- list.files(here("GLORYS_ESM_data","GLORYS_Atlantis_translation"),full.names = T) %>% 
  str_subset("GFDL") %>% str_subset("salinity")%>% str_subset("2013-2099",negate=T) %>% paste(collapse=" ")

system(paste0('sudo cdo -cat ',gfdl_salinity_files,' ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_GFDL_salinity_2013-2099.nc')))

gfdl_temperature_files <- list.files(here("GLORYS_ESM_data","GLORYS_Atlantis_translation"),full.names = T) %>% 
  str_subset("GFDL") %>% str_subset("temperature")%>% str_subset("2013-2099",negate=T) %>% paste(collapse=" ")

system(paste0('sudo cdo -cat ',gfdl_temperature_files,' ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_GFDL_temperature_2013-2099.nc')))

gfdl_hydro_files <- list.files(here("GLORYS_ESM_data","GLORYS_Atlantis_translation"),full.names = T) %>% 
  str_subset("GFDL") %>% str_subset("hydro")%>% str_subset("2013-2099",negate=T) %>% paste(collapse=" ")

system(paste0('sudo cdo -cat ',gfdl_hydro_files,' ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_GFDL_hydro_2013-2099.nc')))

had_salinity_files <- list.files(here("GLORYS_ESM_data","GLORYS_Atlantis_translation"),full.names = T) %>% 
  str_subset("Hadley") %>% str_subset("salinity")%>% str_subset("2013-2099",negate=T) %>% paste(collapse=" ")

system(paste0('sudo cdo -cat ',had_salinity_files,' ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_Hadley_salinity_2013-2099.nc')))

had_temperature_files <- list.files(here("GLORYS_ESM_data","GLORYS_Atlantis_translation"),full.names = T) %>% 
  str_subset("Hadley") %>% str_subset("temperature")%>% str_subset("2013-2099",negate=T) %>% paste(collapse=" ")

system(paste0('sudo cdo -cat ',had_temperature_files,' ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_Hadley_temperature_2013-2099.nc')))

had_hydro_files <- list.files(here("GLORYS_ESM_data","GLORYS_Atlantis_translation"),full.names = T) %>% 
  str_subset("Hadley") %>% str_subset("hydro")%>% str_subset("2013-2099",negate=T) %>% paste(collapse=" ")

system(paste0('sudo cdo -cat ',had_hydro_files,' ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_Hadley_hydro_2013-2099.nc')))

# 04.12.2022 Fixing the missing dt issue
# as well as the missing "parameters" global attribute?
cmd <- paste0('sudo ncatted -a parameters,global,c,c,"" ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_IPSL_hydro_2013-2099.nc'))
system(cmd)
cmd <- paste0('sudo ncatted -a dt,t,c,d,43200 ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_IPSL_hydro_2013-2099.nc'))
system(cmd)

cmd <- paste0('sudo ncatted -a parameters,global,c,c,"" ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_IPSL_salinity_2013-2099.nc'))
system(cmd)
cmd <- paste0('sudo ncatted -a dt,t,c,d,43200 ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_IPSL_salinity_2013-2099.nc'))
system(cmd)

cmd <- paste0('sudo ncatted -a parameters,global,c,c,"" ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_IPSL_temperature_2013-2099.nc'))
system(cmd)
cmd <- paste0('sudo ncatted -a dt,t,c,d,43200 ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_IPSL_temperature_2013-2099.nc'))
system(cmd)

cmd <- paste0('sudo ncatted -a parameters,global,c,c,"" ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_GFDL_hydro_2013-2099.nc'))
system(cmd)
cmd <- paste0('sudo ncatted -a dt,t,c,d,43200 ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_GFDL_hydro_2013-2099.nc'))
system(cmd)

cmd <- paste0('sudo ncatted -a parameters,global,c,c,"" ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_GFDL_salinity_2013-2099.nc'))
system(cmd)
cmd <- paste0('sudo ncatted -a dt,t,c,d,43200 ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_GFDL_salinity_2013-2099.nc'))
system(cmd)

cmd <- paste0('sudo ncatted -a parameters,global,c,c,"" ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_GFDL_temperature_2013-2099.nc'))
system(cmd)
cmd <- paste0('sudo ncatted -a dt,t,c,d,43200 ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_GFDL_temperature_2013-2099.nc'))
system(cmd)

cmd <- paste0('sudo ncatted -a parameters,global,c,c,"" ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_Hadley_hydro_2013-2099.nc'))
system(cmd)
cmd <- paste0('sudo ncatted -a dt,t,c,d,43200 ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_Hadley_hydro_2013-2099.nc'))
system(cmd)

cmd <- paste0('sudo ncatted -a parameters,global,c,c,"" ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_Hadley_salinity_2013-2099.nc'))
system(cmd)
cmd <- paste0('sudo ncatted -a dt,t,c,d,43200 ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_Hadley_salinity_2013-2099.nc'))
system(cmd)

cmd <- paste0('sudo ncatted -a parameters,global,c,c,"" ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_Hadley_temperature_2013-2099.nc'))
system(cmd)
cmd <- paste0('sudo ncatted -a dt,t,c,d,43200 ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_Hadley_temperature_2013-2099.nc'))
system(cmd)

#check
paste0('sudo ncdump -h ',here('GLORYS_ESM_data','GLORYS_Atlantis_translation','CalCurrent_GLORYS_IPSL_salinity_2013-2099.nc')) %>% system()
