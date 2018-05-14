##------------------------------------
## this file pulls in raw street data and creates intersections

source('../load_packages.R')

data_folder = '../../data/'

## the functions we'll use here:

source('../data_functions/GetAspectRatio.R')
source('../data_functions/ApproximateDistance.R')
source('../data_functions/GetIntersections.R')

##------------------------------------

road_file = paste0(data_folder, 'raw/streets/tl_2016_42101_roads.shp')
streets = st_read(road_file)

## get lengths in metres
streets$length <- as.numeric(st_length(streets))

## reorder by length
streets %<>% arrange(desc(length))

## this step is entirely dependend on your analysis
## We don't want all streets:
##  - removing 'S1100' (highways)
##  - removing "S", "U", and NA routes, don't seem to have real intersections
keep_ind = streets$MTFCC != 'S1100' & !(streets$RTTYP %in% c(NA, 'U', 'S'))

streets %<>% filter(keep_ind)

save(streets,
     file = paste0(data_folder, 'streets.rdata'))

##------------------------------------

## form the intersections
## hope you like printouts
## make coffee etc...
inter_points <- GetIntersections(streets,
                                 min_dist = 100)

## add street info to these points
inter_points$id <- 1:nrow(inter_points)

inter_points$street = streets$FULLNAME[inter_points$road_l]
inter_points$cross_street = streets$FULLNAME[inter_points$road_s]

inter_points$street_length = streets$length[inter_points$road_l]
inter_points$cross_street_length = streets$length[inter_points$road_s]

## rearrange
inter_points %<>%
    select(-road_l, -road_s) %>%
    select(id, everything())

## points for plots etc
inter_sf <- st_as_sf(inter_points %>% select(id, lon, lat),
                     coords = c('lon', 'lat'),
                     crs = st_crs(streets))

save(inter_points,
     inter_sf,
     file = paste0(data_folder, 'inter_points.rdata'))

##------------------------------------
