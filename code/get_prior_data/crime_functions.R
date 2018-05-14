CleanCrime <- function(crime){
    crime <- crime %>% rename(ucr = ucr_general,
                              hour = hour_)

    ## kill a few
    crime <- crime %>%
        select(-c(the_geom, the_geom_webmercator,
                  objectid, dc_key, location_block, psa, dc_dist))

    ## crime type oh yes
    message('relabelling crime types')
    crimetypes <- c('murder', 'rape', 'robbery', 'assault',
                    'burglary', 'theft', 'motortheft',
                    'otherassault', 'arson', 'forgery',
                    'fraud', 'embezzlement', 'receivestolen',
                    'vandalism', 'weaponviolation',
                    'prostitution', 'sexoffense', 'drugviolation',
                    'gambling', 'familyoffense', 'dui', 'liquorlaw',
                    'publicdrunk', 'disorderly', 'vagrancy',
                    'other')

    crimeframe <- data_frame(ucr = sort(unique(crime$ucr)),
                             crimetype = crimetypes)

    crime <- crime %>%
        left_join(crimeframe, by = c('ucr'))


    crime <- crime %>%
        rename(date = dispatch_date,
               time = dispatch_time,
               timedate = dispatch_date_time)

    crime <- crime %>%
        mutate(weekday = weekdays(timedate))

    ## another delete / rename:
    crime <- crime %>%
        rename(crime_cat = text_general_code)

    return(crime)
}

#' left join by adding zeros
ZeroLJoin <- function(left_table, join_table, by_vec, fill_val = 0){
    left_table %<>%
        left_join(join_table, by = by_vec)

    new_cols = names(join_table)[!names(join_table) == by_vec]
    na_list_0 <- as.list(rep(NA, ncol(left_table)))
    names(na_list_0) = colnames(left_table)
    na_list_0[new_cols] = fill_val

    left_table %<>% replace_na(na_list_0)

    return(left_table)    
}

