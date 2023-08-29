library(here)
library(ggplot2)
library(tidync)
library(dplyr)
library(mapdata)

glorys.file <- here("GLORYS_climatology","glorys_thetao_clim.nc")
esm.file  <- here("delta_method_final_outputs","GFDL-CM4_delta_thetao_2050.nc")
glorys.esm.file  <- here("GLORYS_ESM_final","GFDL-CM4_glorys_thetao_2050.nc")

glorys.nc = tidync(glorys.file)%>%
  hyper_tibble()%>%
  filter(depth == min(depth))

esm.nc = tidync(esm.file)%>%
  hyper_tibble()%>%
  filter(lev == min(lev))

glorys.esm.nc = tidync(glorys.esm.file)%>%
  hyper_tibble()%>%
  filter(lev == min(lev))

ggplot(data = glorys.nc, aes(x= longitude, y = latitude, color = thetao))+
  geom_point()+
  annotation_map(map_data('worldHires'),fill = 'grey70',alpha=0.3)
  
ggplot(data = esm.nc, aes(x= longitude, y = latitude, color = delta_thetao))+
  geom_point()+
  annotation_map(map_data('worldHires'),fill = 'grey70',alpha=0.3)

ggplot(data = glorys.esm.nc, aes(x= longitude, y = latitude, color = delta_thetao))+
  geom_point()+
  annotation_map(map_data('worldHires'),fill = 'grey70',alpha=0.3)

