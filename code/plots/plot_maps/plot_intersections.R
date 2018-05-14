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

## FIRST UP:

## streets, intersection of center city.
## defined by census tract
center_city_geom <- st_union(blockgroup_geom %>% filter(censustract < 1300))

load(file = paste0(data_folder, 'streets.rdata'))
load(file = paste0(data_folder, 'inter_points.rdata'))

## get the streets we care about:
## rel_street_ind <- st_intersects(streets, center_city_geom, sparse = FALSE)
## actually plot them all
rel_street_ind = TRUE

## and the points:
## rel_points_ind <- st_contains(center_city_geom,
##                               inter_sf,
##                               sparse = FALSE)
rel_points_ind = TRUE

pdf(file = paste0(plot_folder, 'plot_maps/philly_intersections_center_city.pdf'),
    width = 8, height = 7)
PlotGoogleMap(center_city_geom,
              g_zoom = 14,
              g_maptype = 'terrain',
              fill_alp = 0.7)

colvec = rev(ColGen(rank(streets$length[rel_street_ind]),
                plot_cols = viridis_pal()(50)[1:40]))

plot(st_geometry(streets[rel_street_ind,]),
     col = colvec,
     lwd = 1.3,
     add = TRUE)
plot(st_geometry(inter_sf[rel_points_ind,]),
     add = TRUE,
     pch  = 20,
     cex = 1.4,
     col = 'black')
plot(st_geometry(inter_sf[rel_points_ind,]),
     add = TRUE,
     col = 'pink',
     cex = 1.15,
     pch  = 20)
dev.off()

##------------------------------------
##------------------------------------
##------------------------------------

## combine all with center city.

pdf(file = paste0(plot_folder, 'plot_maps/philly_intersections_all.pdf'),
    width = 8, height = 7)

## par(mfcol = c(1, 2))

PlotGoogleMap(County_geom,
              g_zoom = 11,
              g_maptype = 'terrain',
              fill_alp = 0.7)

colvec = rev(ColGen(rank(streets$length[rel_street_ind]),
                plot_cols = viridis_pal()(50)[1:40]))

plot(st_geometry(streets[rel_street_ind,]),
     col = colvec,
     lwd = 0.3,
     add = TRUE)
plot(st_geometry(inter_sf[rel_points_ind,]),
     add = TRUE,
     pch  = 20,
     cex = 0.4,
     col = 'black')
plot(st_geometry(inter_sf[rel_points_ind,]),
     add = TRUE,
     col = 'pink',
     cex = 0.35,
     pch  = 20)

## cc box:
box_cc = st_make_grid(center_city_geom, n = c(1, 1))
plot(box_cc, add = TRUE, 
     col = 'transparent', lwd = 3)
dev.off()
