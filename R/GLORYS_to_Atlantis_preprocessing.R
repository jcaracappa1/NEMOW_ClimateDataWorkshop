#################################
# Step 0: Calculate geometry and spatial join keys for GLORYS to Atlantis translation
#################################

# Packages and settings
library(raster)
library(tidyverse)
library(sf)
library(here)
library(tidync)
library(rbgm)
library(viridis)
library(angstroms)
library(tabularaster)
library(lubridate)

select <- dplyr::select
here <- here::here
map <- purrr::map
options(dplyr.summarise.inform=FALSE)

# Get GLORYS file names from projected delta downscaled product
ifiles <- list.files('~/GLORYS_ESM_data/delta_method_final_outputs',full.names = T) %>% 
  str_subset('_delta_',negate = T) %>% 
  str_subset('zos',negate = T) %>% 
  str_subset('2006_2100',negate=T) %>% 
  str_subset('tmp',negate=T)

# read first GLORYS data for depth and grid info - assumes this does not change between time steps and GLORYS files
gfile <- ifiles[1]
glorys_grid <- tidync(gfile)

# read Atlantis BGM file
bgm <- read_bgm(here('GLORYS_ESM_data','CalCurrentV3_utm.bgm'))
# convert bgm to spatial data frame (sf) and convert to lat/lon
atlantis_sf <- bgm %>% box_sf() %>% 
  st_transform(4326) %>% 
  select(label:area,insideX,insideY,.bx0,box_id)
# Atlantis box areas key
box_areas <- atlantis_sf %>% st_set_geometry(NULL) %>% 
  dplyr::select(b=.bx0,area)

# list of GLORYS variables to translate
# note: not using sea surface height (zos) for now (can add back in later if needed)
glorys_vars <- c('si','o2','no3','chl','thetao','so','uo','vo')

# GLORYS grids
# Horizontal coordinates: longitude/latitude of points
latlon_grd <- glorys_grid %>% 
  hyper_filter(depth=index==5) %>% 
  hyper_filter(time=index==5) %>% 
  hyper_tibble() %>%
  distinct(longitude,latitude) %>% 
  mutate(grididx=row_number()) # add grid ID number for later matching

# convert to spatial object in lat/lon
latlon_sf <- latlon_grd %>% 
  st_as_sf(coords=c('longitude','latitude'),crs=4326)

# Vertical coordinate: GLORYS depths
glorys_depths <- glorys_grid %>% 
  activate('D3') %>% 
  hyper_tibble()

# join GLORYS grids with Atlantis boxes and faces (u and v grids)
# Boxes: join any GLORYS point within 10km of a box; make sure to check units (e.g., meters vs. km)
boxes_glorys_join_key <- atlantis_sf %>% st_join(latlon_sf,join = st_is_within_distance,dist=10000)

# Atlantis faces
faces <- bgm$faces %>% select(-label)

# convert faces to spatial dataframe
faces_sf <- bgm %>% face_sf() %>% 
  mutate(label = 0:(length(label)-1)) %>% # creates a new index 'face_id' starting from 0 and increasing, as the 'label' column produced by rbgm::face_sf() is incorrect (tested in R 4.0.4)
  # join attribute data
  left_join(faces,by=c('label'='.fx0')) %>% 
  rename(.fx0=label)

# get the angle of each face in the direction we need it to calculate flows
get_angle <- function(geomstring) {
  coords <- st_coordinates(geomstring)
  this_atan <- atan2(coords[2,2]-coords[1,2], coords[2,1]-coords[1,1]) # as per R help, atan2(y,x)
  return(this_atan)
}

# faces angles reference key
faces_angles <- faces_sf %>% 
  rowwise() %>% 
  mutate(face_angle = get_angle(geometry),
         sine_new = sin(face_angle),
         cosine_new = cos(face_angle)) %>%
  select(.fx0, face_angle) %>% 
  st_set_geometry(NULL)

# Hyperdiffusion correction factors
# these have been calculated in a separate script (calc_hyperdiffusion_corrections.R)
# they measure the distance from the midpoint of each face to the
# far side of the adjoining 'left' and 'right' boxes, at an angle which is
# orthogonal to the face. Basically, how far does water have entering through
# a face have to travel to reach the far side of the box?
hd_corrections <- read_rds(here('GLORYS_ESM_data','GLORYS_Atlantis_translation','hyper_diffusion_factors.rds'))

# construct a 10km buffer around each face (this could be altered or turned into an option)
faces_buffer <- st_buffer(faces_sf,dist=10000) %>% st_transform(4326)
# join u points to corresponding faces
faces_u_join <- faces_buffer %>% st_join(latlon_sf,join = st_is_within_distance,dist=10000)
# join v points to corresponding faces
faces_v_join <- faces_buffer %>% st_join(latlon_sf,join = st_is_within_distance,dist=10000)

# just a couple of checks
# insert a check for which boxes have no overlapping latlon points
empty_boxes<- boxes_glorys_join_key %>% 
  st_set_geometry(NULL) %>% 
  filter(is.na(grididx)) %>% 
  select(.bx0) %>% 
  distinct() %>% pull()
print(paste0("Atlantis boxes with no GLORYS points are boxes ",paste(empty_boxes,collapse = ",")))
atlantis_glorys_overlay <- atlantis_sf %>% 
  mutate(is_empty=ifelse(.bx0%in%empty_boxes,"Yes","No")) %>% 
  ggplot()+
  geom_sf(aes(fill=factor(is_empty)),col=NA)+
  geom_sf(data=latlon_sf,size=0.5)+
  labs(fill="Empty?")

# ggsave(here('GLORYS_ESM_data','GLORYS_Atlantis_translation','glorys_atlantis_overlay.png'),atlantis_glorys_overlay,h=7,w=7)

# ... and one for which faces do not intercept u and v points
empty_faces_u<- faces_u_join %>% 
  st_set_geometry(NULL) %>% 
  filter(is.na(grididx)) %>% 
  select(.fx0) %>% 
  distinct() %>% pull()
print(paste0("Atlantis faces with no u points are faces ",paste(empty_faces_u,collapse = ",")))

empty_faces_v<- faces_v_join %>% 
  st_set_geometry(NULL) %>% 
  filter(is.na(grididx)) %>% 
  select(.fx0) %>% 
  distinct() %>% pull()
print(paste0("Atlantis faces with no v points are faces ",paste(empty_faces_v,collapse = ",")))

# Atlantis depth: This part is somewhat specific to each Atlantis model, need to generalize more
# enter the depth breaks in the model
# THIS COULD BE BROUGHT TO TOP AND/OR MADE AN ARGUMENT, OR CALCULATED DIRECTLY FROM THE BGM FILE
atlantis_z_diff <- c(-50,-50,-100,-350,-650,-1200) #TODO: make this an argument

# bottom-most depth of each layer
atlantis_z <- cumsum(atlantis_z_diff)
# middle of each layer (used for ESM interpolation)
atlantis_z_mid <- tibble(atlantis_layer=1:6,midz=(c(0,atlantis_z)+c(atlantis_z_diff/2,0)) %>% head(6))

depth_layers_plot <- tibble(dz=c(0,atlantis_z_diff,-1),lyr=8:1) %>% 
  ggplot(aes(dz,x=1,fill=factor(lyr)))+
  geom_col(col='black')+
  scale_fill_brewer(palette="PuBu",direction = -1,guide="none")+
  labs(x="",y="Depth",fill="",title="CalCurrent Atlantis Depth Layers")+
  theme_minimal()+
  theme(axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor=element_blank())
depth_layers_plot

# small function to build enough depth layers for each box, starting with the defined layers above
# in this function, botz is the bottom depth given for each box, which is available in the .bgm file 
build_Atlantis_depths <- function(botz,lyrs){
  # bottom of each layer, starting from shallowest
  lyrbot<-lyrs
  # layers to use are all those that are shallower than the given botz
  lyr_vec <- lyrbot[lyrbot>botz]
  # the depth of the deepest layer is equal to botz
  lyr_vec <- c(lyr_vec,botz)
  # in Atlantis, each box has the same number of depth layers, but some layers have zero thickness
  # so we have to pad with zeroes to make all boxes have the same number of layers
  nzeroes <- length(lyrs)-length(lyr_vec)
  lyr_vec <- c(lyr_vec,rep(0,nzeroes))
  return(lyr_vec)
}
# construct the depth profiles of each Atlantis box

atlantis_depths <- bgm$boxes %>% 
  select(.bx0,botz) %>% 
  filter(botz<0) %>% 
  # apply the function above to create the layers
  mutate(maxz=purrr::map(botz,build_Atlantis_depths,lyrs=atlantis_z)) %>% 
  unnest(cols=c(maxz)) %>% 
  # add a minimum depth for each layer
  group_by(.bx0) %>% 
  mutate(minz=lag(maxz,1,default = 0),atlantis_layer=1:length(atlantis_z)) %>% 
  # add a layer thickness calculation
  mutate(dz=minz-maxz) %>% 
  # "dummy" layers (layers too deep for a given Atlantis box) should have minz and dz=0
  mutate(minz=ifelse(maxz==0,0,minz),dz=ifelse(maxz==0,0,dz)) %>% 
  ungroup() %>% 
  left_join(atlantis_z_mid,by='atlantis_layer') %>% 
  select(.bx0,atlantis_layer,minz,maxz,dz,midz)

# Eventually, we will need to flip around the way that Atlantis depth layers are numbered
# depth layer swapping: 
# swap from "1 is the shallowest layer" to "highest number is always the shallowest layer",
# which is what Atlantis input files need

depth_swap <- atlantis_depths %>% 
  filter(dz>0) %>% 
  dplyr::select(.bx0,atlantis_layer) %>% 
  group_by(.bx0) %>% 
  arrange(desc(atlantis_layer)) %>% 
  mutate(atlantis_input_depth_layer=row_number()-1) %>% 
  ungroup()

# face depths (for later flux calcs)
face_depths <- faces %>% 
  left_join(atlantis_depths %>% select(.bx0,atlantis_layer,dz),by=c("left"=".bx0")) %>% 
  mutate(left_area=length*dz) %>% 
  rename(dz_left=dz) %>% 
  left_join(atlantis_depths %>% select(.bx0,atlantis_layer,dz),by=c("right"=".bx0","atlantis_layer")) %>% 
  mutate(right_area=length*dz) %>% 
  rename(dz_right=dz) %>% 
  # area of the face is the smallest area, maintaining NAs (if one box is deeper than its neighbor, no flux)
  rowwise() %>%
  mutate(dz_max = max(dz_left,dz_right),
         face_area=ifelse((left_area>0 & right_area >0), min(left_area, right_area), NA)) 

# Matching keys for boxes and faces
boxes_glorys_join_with_depth <- atlantis_depths %>% 
  left_join(boxes_glorys_join_key %>% st_set_geometry(NULL),by=c(".bx0")) %>% 
  ungroup()

# make vectors of grid point indices
# makes a nested df where each row is a box/layer combination, with the joined GLORYS gridpoints associated with each as a list-column
boxes_depths_grdpts <- boxes_glorys_join_with_depth %>% 
  select(-dz) %>% 
  group_by(.bx0,atlantis_layer,minz,maxz) %>% 
  nest(grdpts=c(grididx)) %>% 
  ungroup() %>% 
  mutate(ngrdpts=purrr::map(grdpts,function(x) length(x %>% pluck(1))))

# Do the same for Atlantis faces (for GLORYS, u and v are at the same points)

faces_u_join_with_depth <- face_depths %>%
  left_join(faces_u_join, by = c(".fx0")) %>%
  ungroup() %>%
  drop_na() %>% 
  select(.fx0,atlantis_layer,dz_max,grididx,face_area) %>%
  group_by(grididx,.fx0) %>%
  mutate(maxz=-cumsum(dz_max),minz=-lag(-maxz,default=0)) %>%
  ungroup()%>%
  select(-dz_max)

# again for v (don't need this for GLORYS)
# faces_v_join_with_depth <- face_depths %>%
#   left_join(faces_v_join, by = c(".fx0")) %>%
#   ungroup() %>%
#   #drop_na() %>% # turn this on or off depending on whether we want to have NA fluxes in non-existing layers or not, cannot recall what HydroCOnstruct wants
#   select(.fx0,atlantis_layer,dz_max,grididx,face_area) %>%
#   group_by(grididx,.fx0) %>%
#   mutate(maxz=-cumsum(dz_max),minz=-lag(-maxz,default=0)) %>%
#   ungroup()%>%
#   select(-dz_max)

# make vectors of u point indices
faces_depths_grdpts <- faces_u_join_with_depth %>% 
  group_by(.fx0,atlantis_layer,minz,maxz) %>% 
  nest(grdpts=c(grididx)) %>% 
  mutate(ngrdpts=purrr::map(grdpts,function(x) length(x %>% pluck(1))))
