##------------------------------------
## now we actually create inter_frame

## NOTE:
## run combine_new and combine_old first!
## !!!
## avoid this sort of thing in general, but here I guess it's the best
## of a bad bunch of options

source('../load_packages.R')

data_folder = '../../data/'

##------------------------------------

## load inter_points
load(file = paste0(data_folder, 'inter_points.rdata'))

##------------------------------------

## loading the summaries from combine_new and combine_old

old_file = paste0(data_folder, 'summary_lists/summary_list_old.rdata')
new_file = paste0(data_folder, 'summary_lists/summary_list_new.rdata')

if(!file.exists(old_file)){
    message(
        'Did you run combine_old.R already?')
}
if(!file.exists(new_file)){
    message(
        'Did you run combine_new.R already?')
}

load(file = old_file)
load(file = new_file)

##------------------------------------

## first, crime:

crime_inter_frame <- inter_points %>% select(id, lon, lat)

##----------------
## prior
crime_inter_frame %<>% left_join(summary_list_old[['prior_crime']],
                                  by = c('id' = 'inter_index'))

## fill missing with zeros

zero_l <- list(
    nonviolent = 0,
    violent = 0,
    total = 0)

crime_inter_frame %<>% replace_na(zero_l)

## finally rename:

crime_inter_frame %<>% rename(
                           prior_nonviolent = nonviolent,
                           prior_violent = violent,
                           prior_total = total)

##----------------
## post
crime_inter_frame %<>% left_join(summary_list_old[['post_crime']],
                                  by = c('id' = 'inter_index'))

## fill missing with zeros

zero_l <- list(
    nonviolent = 0,
    violent = 0,
    total = 0)

crime_inter_frame %<>% replace_na(zero_l)

## finally rename:

crime_inter_frame %<>% rename(
                           post_nonviolent = nonviolent,
                           post_violent = violent,
                           post_total = total)

save(crime_inter_frame,
     file = paste0(data_folder, 'crime_inter_frame.rdata'))

##------------------------------------
## landuse

landuse_inter_frame <- inter_points %>% select(id, lon, lat)

landuse_inter_frame %<>% left_join(summary_list_old[['landuse_area']],
                                   by = c('id' = 'inter_index'))

landuse_inter_frame %<>% left_join(summary_list_old[['landuse_count']],
                                   by = c('id' = 'inter_index'))

## EZ, just fill NA with zeros
landuse_inter_frame[is.na(landuse_inter_frame)] = 0

save(landuse_inter_frame,
     file = paste0(data_folder, 'landuse_inter_frame.rdata'))

##------------------------------------

