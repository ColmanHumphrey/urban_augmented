##------------------------------------
## plotting maps:
## intersections

source('../../load_packages.R')

data_folder = '../../../data/'
plot_folder = '../../../plots/'

plot_functions_folder = '../plot_functions/'
for(pfile in list.files(plot_functions_folder)){
    ## avoid temp files
    if(!grepl('~', pfile)){
        source(paste0(plot_functions_folder, pfile))
    }
}

load(file = paste0(data_folder, 'prior_data/block_and_group.rdata'))

##------------------------------------

## school plot
load(file = paste0(data_folder, 'school_data.rdata'))

## like in the analysis, drop below 150

school_data %<>% filter(ENROLLMENT > 150)

size_cex = sqrt(school_data$ENROLLMENT) / 30

## rel_cols <- viridis_pal()(50)
use_cols <- c('royalblue', # high
              'seagreen2', # both
              'tomato2') # elem

col_vec <- ifelse(school_data$elem_cat,
           ifelse(school_data$high_cat, use_cols[2],
                  use_cols[3]),
           use_cols[1])

## don't plot this
## pch_vec <- ifelse(school_data$TYPE_SPECIFIC == 'District',
##                   19,
##            ifelse(school_data$TYPE_SPECIFIC == 'Private',
##                   17, 15))
                  
pdf(file = paste0(plot_folder, 'plot_maps/plot_school_info.pdf'))

PlotGoogleMap(County_geom,
              g_zoom = 11,
              g_maptype = 'roadmap',
              fill_alp = 0.8)

plot(st_geometry(school_data),
     cex = size_cex + 0.1,
     col = 'black',
     pch = 20,
     add = TRUE)
plot(st_geometry(school_data),
     cex = size_cex,
     col = col_vec,
     pch = 20,
     add = TRUE)

## order_enroll = order(school_data$ENROLLMENT)
## use_size_vec = c(20, 150, 360)
## size_vec = school_data$ENROLLMENT[order_enroll[use_size_vec]]
## size_cex_leg = size_cex[order_enroll[use_size_vec]]
size_vec = c(150, 500, 1500)
size_cex_leg = sqrt(size_vec) / 30

legend(x = 'bottomright',
       legend = c(paste0(size_vec, ' ', 'students'), '',
                  'High School',
                  'Elementary School',
                  'Both'),## , '',                  
                  ## 'District School',
                  ## 'Private School',
                  ## 'Archdiocese'),
       col = c(rep('black', 3),
                'transparent',
                use_cols[c(1, 3, 2)]),
                ## 'transparent',
                ## rep('black', 3)),
       pch = c(19, 19, 19, 1,
               19, 19, 19), #1,
               ## 19, 17, 15),
       pt.cex = c(size_cex_leg, 1,
                  rep(1.3, 3)),# 1,
                  ## rep(1, 3)),
       bty = 'n')
       
dev.off()

##------------------------------------

