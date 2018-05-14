##------------------------------------
## now we actually create inter_frame

## NOTE:
## run combine_new and combine_old first!
## !!!
## avoid this sort of thing in general, but here I guess whatever

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

inter_frame <- inter_points

##----------------
## demo:

inter_frame %<>% left_join(summary_list_old[['demo']],
                           by = c('id' = 'inter_index'))

## all other five are the same:
inter_frame$missing_prop = is.na(inter_frame$asian_prop)
## conceptually imperfect for sure, but we'll impute zeros
## for the missing proportions (only a few of them)
zero_l <- list(
    white_prop = 0,
    black_prop = 0,
    asian_prop = 0,
    other_prop = 0,
    hispanic_prop = 0)

inter_frame %<>% replace_na(zero_l)

## and income, poverty
## only want one indicator to cover both situations...
inter_frame$missing_money <- is.na(inter_frame$income) |  is.na(inter_frame$poverty)

## replace both with the median
## because we'll rank them
zero_l <- list(
    income = median(inter_frame$income, na.rm = TRUE),
    poverty = median(inter_frame$poverty, na.rm = TRUE))

inter_frame %<>% replace_na(zero_l)

## rename for ease etc

inter_frame %<>% rename('total_population' = 'total')

##----------------
## property

inter_frame %<>% left_join(summary_list_new[['property']],
                           by = c('id' = 'inter_index'))

## 1060 just have no properties
## apart from that:
## the two ways are to have missing age
## and missing price per sqft info
inter_frame$missing_properties = is.na(inter_frame$count)
inter_frame$missing_age_property = is.na(inter_frame$mean_age)
inter_frame$missing_sqft_property = is.na(inter_frame$price_per_sqft)

## replacing with zero when that's the conceptual limiting value
## median when not (and when variable will be ranked)
zero_l <- list(
    count = 0,
    mean_value = median(inter_frame$mean_value, na.rm = TRUE),    
    mean_age = median(inter_frame$mean_age, na.rm = TRUE),
    age_dev = 0,
    mean_stories = 0,
    mean_garages = 0,
    price_per_sqft = median(inter_frame$price_per_sqft, na.rm = TRUE),
    price_sqft_dev = 0)

inter_frame %<>% replace_na(zero_l)

## some renaming:
inter_frame %<>% rename(
                     property_count = count,
                     property_mean_value = mean_value,
                     property_mean_age = mean_age,
                     property_age_dev = age_dev,
                     property_mean_stories = mean_stories,
                     property_mean_garages = mean_garages, 
                     property_price_per_sqft = price_per_sqft,
                     property_price_sqft_dev = price_sqft_dev)

##----------------
## transit
st_geometry(summary_list_new[['transit']]) = NULL

inter_frame %<>% left_join(summary_list_new[['transit']],
                           by = c('id' = 'inter_index'))

## these are counts, so just all to zero:
zero_l <- list(
    subway_routes = 0,
    bus_routes = 0,
    trolley_routes = 0)

inter_frame %<>% replace_na(zero_l)

##----------------
## school

inter_frame %<>% left_join(summary_list_new[['school']],
                           by = c('id' = 'inter_index'))

## no NAs!
## rename:
inter_frame %<>% rename(
                     school_elementary_dist = elem_dist,
                     school_high_dist = high_dist,
                     school_elementary_count = elem_count,
                     school_high_count = high_count,
                     school_elementary_smooth = elem_smooth,
                     school_high_smooth = high_smooth)

##----------------
## controls

inter_frame %<>% left_join(summary_list_new[['controls']],
                           by = c('id' = 'inter_index'))

## no NAs, no renaming

##----------------
## last step:
## "legend" frame:
inter_frame_legend <-
    data_frame(
        columns = names(inter_frame),
        cats = rep(c('points', 'demographics',
                     'property', 'transit',
                     'school', 'controls'),
                   times = c(7, 10, 
                             11, 3,
                             6, 2)))
inter_frame_legend %<>%
    mutate(missing_indicator =
               grepl('missing', columns))

save(inter_frame,
     inter_frame_legend,
     file = paste0(data_folder, 'inter_frame.rdata'))

##------------------------------------
