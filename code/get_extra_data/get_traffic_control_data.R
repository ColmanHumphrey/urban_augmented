##------------------------------------
## this file pulls in raw street data and creates intersections

source('../load_packages.R')

data_folder = '../../data/'

load(file = paste0(data_folder, 'prior_data/block_and_group.rdata'))

##------------------------------------

controls_file = paste0(data_folder, 'raw/traffic_controls/Intersection_Controls.shp')
inter_controls <- st_transform(st_read(dsn = controls_file),
                               crs = st_crs(block_geom))

## lots of duplicates? why? like... data goes to 4% of what it loads in as. 
keep_ind <- !duplicated(inter_controls$NODE_ID)
inter_controls <- inter_controls[keep_ind,]

save(inter_controls,
     file = paste0(data_folder, 'inter_controls.rdata'))

##------------------------------------
