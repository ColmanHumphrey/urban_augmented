##------------------------------------
## this is a quick re-do of
## crime from the first analysis, for more time etc

source('../load_packages.R')

data_folder = '../../data/'

load(file = paste0(data_folder, 'prior_data/block_and_group.rdata'))

##------------------------------------##
## crime

## old:
## crime_file <- paste0(data_folder, '/raw/crime/crimes.csv')
crime_file <- paste0(data_folder, '/raw/crime/incidents_part1_part2.csv')

## library(data.table)
## crime <- as_data_frame(fread(crime_file))

crime <- read_csv(crime_file)

## I'm finding this awkward, but I'm sure there's a way to do it.
## regardless, it's nice to have longs and lats etc for some operations, so no harm
## crime <- st_read(crime_file, geometry_column = 'Shape')

source('crime_functions.R')

## this file cleans data as it came from the Philly police
## at some point
## if yours is different, hopefully it's clear how it's different

crime <- CleanCrime(crime)

## remove NAs immediately
crime %<>% filter(!is.na(lng) & !is.na(lat))

## in sf form
## keep at least one extra column for plotting etc etc
crime_points <- st_as_sf(crime %>% select(crimetype, lng, lat),
                         coords = c('lng', 'lat'),
                         crs = st_crs(block_geom))

## now we might as well remove points outsidet the county
in_county_ind <- st_contains(County_geom, crime_points, sparse = FALSE)
## mean(in_county_ind) # or sum(!in_county_ind)
## these points are basically right on the border, but just cleaner this way

crime %<>% filter(in_county_ind)

## sort crime by timedate, then I guess ucr
crime %<>% arrange(timedate, ucr)

## note: big adjustment!!
## federal definition of rape, sexoffense changed in 2013
crime %<>%
    mutate(crimetype = if_else(crimetype %in% c('rape', 'sexoffense'),
                               'sexcrime',
                               crimetype,
                               NULL))

## add violent, vice, nonviolen, other cats:
violent_type = c('assault', 'murder', 'otherassault', 'robbery', 'sexcrime')
vice_type = c('drugviolation', 'gambling', 'prostitution')
nonviolent_type = c('arson', 'theft', 'disorderly', 'motortheft', 'vandalism', 'burglary')
other_type = c('forgery', 'fraud', 'embezzlement', 'receivestolen',
               'weaponviolation', 'familyoffense', 'dui', 'liquorlaw',
               'publicdrunk', 'vagrancy', 'other')
crime %<>%
    mutate(crime_cat = case_when(
               crimetype %in% violent_type ~ 'violent',
               crimetype %in% nonviolent_type ~ 'nonviolent',
               crimetype %in% vice_type ~ 'vice',
               crimetype %in% other_type ~ 'other',
               TRUE ~ 'no_type'))

## throw away no type
crime %<>% filter(crime_cat != 'no_type')

## save(crime, crime_points, file = paste0(data_folder, '/crime.rdata'))

## FILTER ACCORDING TO YOUR OWN TIME ETC
min_time_prior <- as.POSIXct("2009-01-01 EST")
max_time_prior <- as.POSIXct("2016-08-01 EST")

prior_crime <- crime %>% filter(timedate >= min_time_prior,
                                timedate < max_time_prior)

prior_crime_points <- st_as_sf(prior_crime %>% select(crimetype, lng, lat),
                               coords = c('lng', 'lat'),
                               crs = st_crs(block_geom))

min_time_post <- as.POSIXct("2016-08-01 EST")
max_time_post <- as.POSIXct("2018-01-01 EST")

post_crime <- crime %>% filter(timedate >= min_time_post,
                                timedate < max_time_post)

post_crime_points <- st_as_sf(post_crime %>% select(crimetype, lng, lat),
                               coords = c('lng', 'lat'),
                               crs = st_crs(block_geom))

save(prior_crime,
     prior_crime_points,
     post_crime,
     post_crime_points,
     file = paste0(data_folder, 'crime.rdata'))
