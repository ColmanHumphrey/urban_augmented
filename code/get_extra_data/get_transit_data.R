##------------------------------------
## this file pulls in raw street data and creates intersections

source('../load_packages.R')

data_folder = '../../data/'

load(file = paste0(data_folder, 'prior_data/block_and_group.rdata'))

##------------------------------------##

library(RCurl)
library(jsonlite)

## note: decided not to use regional rail
## you basically need to look at septa's site for this...
## I guess this doesn't generalise well to other cities
route_names <- c(1:133,
                 139, 150, 201, 204, 205, 206, 310, 311,
                 '15B', '47M',
                 'G', 'H', 'XH', 'J', 'K', 'L', 'R', 'LUCY', 'D',
                 'BSL', 'BSO', 'MFL', 'MFO', 'NHSL')
                 
route_list <- list()
for(nr in route_names){
    temp_json = getURL(paste0('http://www3.septa.org/hackathon/Stops/', nr),
                       ssl.verifypeer = FALSE, useragent="R")
    if(nchar(temp_json) > 0){
        route_list[[nr]] = fromJSON(temp_json)
    }
    print(nr)
}

## remove the heathens:
## (old errors, should be fixed now)
route_list <- route_list[lengths(route_list) == 4]

## again, pretty Philly specific stuff here:

## delete the norristown high speed line
route_list[['NHSL']] <- NULL
## 100 is NHSL too somehow
route_list[['100']] <- NULL

trolley_names = c(10, 11, 13, 15, 34, 36, 101, 102)
subway_names = c('BSO', 'MFO')
bus_names = names(route_list)[!(names(route_list) %in% c(trolley_names, subway_names))]
## 15B is a bus that makes up the rest of route 15, which is a trolley

route_frame <- data_frame(route_names = names(route_list))
route_frame$bus <- names(route_list) %in% bus_names
route_frame$trolley <- names(route_list) %in% trolley_names
route_frame$subway <- names(route_list) %in% subway_names

all_stop_names <- lapply(route_list, '[[', 'stopname')

## ok only happens twice:
## (old, now fixed)
del_names <- lapply(all_stop_names, function(x){grep('not a stop', x, ignore.case = TRUE)})
route_list[['101']] <- route_list[['101']][-del_names[['101']],]

## convert to large frame
large_route <- as_data_frame(do.call(rbind, route_list))
large_route$route_name <- rep(names(route_list),
                              times = unlist(lapply(route_list, nrow)))

large_route %<>%
    inner_join(route_frame, by = c('route_name' = 'route_names')) %>%
    mutate(lng = as.numeric(lng),
           lat = as.numeric(lat))

transit_stops = st_as_sf(large_route,
                       coords = c('lng', 'lat'),
                       crs = st_crs(block_geom))

save(transit_stops,
     file = paste0(data_folder, 'transit_stops.rdata'))

##------------------------------------
