# Try to find correct hyperdiffusion correction
# Basic logic:
# try to find the shortest distance across each box, but from the perspective
# of each individual Atlantis face. Use angled lines to do this
library(tidyverse)
library(rbgm)
library(sf)

# bgm file
bgm <- read_bgm(here('GLORYS_ESM_data','CalCurrentV3_utm.bgm'))
# atlantis polygons
atlantis_sf <- bgm %>% box_sf() %>%
  dplyr::select(label:area,insideX,insideY,.bx0,box_id)
# leave these in projected coordinates for now, because we will be doing distance calculations
st_crs(atlantis_sf) <- st_crs(attr(atlantis_sf$geometry, "crs")$proj)

faces <- bgm$faces %>% dplyr::select(-label)

faces_sf <- bgm %>% face_sf() %>% 
  mutate(label = 0:(length(label)-1)) %>% # creates a new index 'face_id' starting from 0 and increasing, as the 'label' column produced by rbgm::face_sf() is incorrect (tested in R 4.0.4)
  # join attribute data
  left_join(faces,by=c('label'='.fx0')) %>% 
  rename(.fx0=label)

# get the angle of each face in the direction we need it
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
  dplyr::select(.fx0, face_angle) %>% 
  st_set_geometry(NULL)

faces_angles_midpoints <- faces_sf %>% st_centroid() %>% left_join(faces_angles) %>% 
  mutate(angle_short=round(face_angle,digits=2))

# map out what this looks like
faces_example <- faces_sf %>% 
  left_join(faces_angles) %>% 
  filter(left==60|right==67) %>% 
  mutate(focal=ifelse(.fx0==265,"Yes","No"))
angles_example <- faces_angles_midpoints %>% 
  filter(left==60|right==67)
polys_example <- atlantis_sf %>% 
  filter(.bx0%in%c(60,67))

# Let's pick a focal face (.fx0=265,which adjoins boxes 60 and 67)
ggplot()+
  geom_sf(data=polys_example,fill=NA)+
  geom_sf(data=faces_example,aes(col=focal))+
  geom_sf_text(data=angles_example,aes(label=angle_short),check_overlap = T)

# Logic: create a long, angled line going through the centroid
# what's the longest length we could have??
longest_dist <-purrr::map_dbl(atlantis_sf$.bx0,function(box) {
  # find the longest among distances between 100 sampled points on each box (pretty crude but okay for this)
  atlantis_sf %>% filter(.bx0==box) %>% st_sample(100) %>% st_distance() %>% max()
})
# seems like the longest possible distance is somewhere around 650000m

# this is the main function. Calculates the distance across a box from the perspective and angle of a particular face 
calc_face_distances <- function(face_id,rline=6.5e5,plotit=F){
  # focal face
  focalface <- faces_sf %>% filter(.fx0==face_id)
  # find line centroid
  x <- faces_sf %>% filter(.fx0==face_id) %>% st_centroid() %>% st_coordinates()
  
  # find angle
  ang <- faces_angles %>% filter(.fx0==face_id) %>% pull(face_angle)

  # create points
  dx=rline*sin(ang)
  dy=rline*cos(ang)
  
  x1 <- x[1] - dx
  y1 <- x[2] + dy
  x2 <- x[1] + dx
  y2 <- x[2] - dy
  l1 <- st_linestring(cbind(c(x1,x2), c(y1,y2))) %>% st_sfc() %>% st_as_sf()
  st_crs(l1) <- st_crs(faces_sf)
  
  # left and right boxes
  leftbox=atlantis_sf %>% filter(.bx0==focalface$left)
  rightbox=atlantis_sf %>% filter(.bx0==focalface$right)
  leftintersect=st_intersection(l1,leftbox)
  rightintersect=st_intersection(l1,rightbox)
  
  # plot?
  if(plotit){
    outplot <- ggplot()+
      geom_sf(data=leftbox)+
      geom_sf(data=rightbox)+
      geom_sf(data=focalface,size=2)+
      geom_sf(data=leftintersect,aes(col="left intersect"))+
      geom_sf(data=rightintersect,aes(col="right intersect"))+
      labs(col='')
    print(outplot)
  }
  
  leftdist <- st_length(leftintersect)
  rightdist <- st_length(rightintersect)
  out<-tibble(.fx0=face_id,left_distance=leftdist,right_distance=rightdist)
  
  return(out)
}

# try it out
calc_face_distances(face_id=263,plotit = T)

# apply to every face.
# This could be used as a face-specific hyperdiffusion correction
hd_corrections <- unique(faces_sf$.fx0) %>% 
  purrr::map_df(calc_face_distances)

# SAVE
write_rds(hd_corrections,here('GLORYS_ESM_data','GLORYS_Atlantis_translation','hyper_diffusion_factors.rds'))
