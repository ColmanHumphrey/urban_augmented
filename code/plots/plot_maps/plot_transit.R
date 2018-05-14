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

load(file = paste0(data_folder, 'transit_stops.rdata'))

### gather up all routes to plot...
route_geojson = list.files(path = paste0(data_folder, 'raw/transit/geojson'))
route_geojson = route_geojson[-which(route_geojson == 'all.json')]

route_plot_data <- list()
for(rg in route_geojson){
    print(rg)
    rg_strip = gsub('.geojson', '', rg)
    route_plot_data[[rg_strip]] = st_transform(st_read(
        paste0(data_folder, 'raw/transit/geojson/', rg)),
        crs = st_crs(block_geom))    
}

## add kml:
route_kml <- list.files(path = paste0(data_folder, 'raw/transit/kml_files'))
for(rk in route_kml){
    print(rk)
    rk_strip = gsub('.kml', '', rk)
    route_plot_data[[rk_strip]] = st_transform(st_read(
        paste0(data_folder, 'raw/transit/kml_files/', rk)),
        crs = st_crs(block_geom))    
}

## keep the ones we use:
route_plot_data <- route_plot_data[names(route_plot_data) %in% transit_stops$route_name]

route_frame <- transit_stops %>% select(route_name, bus, trolley, subway)
st_geometry(route_frame) <- NULL
route_frame %<>% distinct()

col_vec = rep(NA, length(route_plot_data))
line_w = rep(NA, length(route_plot_data))

colFbus <- colorRampPalette(c('forestgreen', 'seagreen1'))
col_vals_bus = colFbus(sum(route_frame$bus))[60]
col_vec[route_frame$bus] = col_vals_bus
line_w[route_frame$bus] = 0.3

colFtrolley <- colorRampPalette(c('salmon', 'plum1'))
col_vals_trolley = colFtrolley(sum(route_frame$trolley))[5]
col_vec[route_frame$trolley] = col_vals_trolley
line_w[route_frame$trolley] = 0.6

colFsubway <- colorRampPalette(c('royalblue4', 'dodgerblue'))
col_vals_subway = colFsubway(sum(route_frame$subway))
col_vec[route_frame$subway] = col_vals_subway
line_w[route_frame$subway] = 1.5

pdf(file = paste0(plot_folder, 'plot_maps/plot_transit_info.pdf'))

PlotGoogleMap(County_geom,
              g_zoom = 11,
              g_maptype = 'roadmap',
              fill_alp = 0.9)
plot(County_geom, add = TRUE, lwd = 1, col = 'transparent')

for(j in seq_along(route_plot_data)){
    route_n = route_frame$route_name[j]
    plot(st_intersection(st_geometry(route_plot_data[[route_n]]), County_geom),
         add = TRUE,
         col = col_vec[j], lwd = line_w[j])
    
    plot(st_intersection(st_geometry(transit_stops %>% filter(route_name == route_n)), County_geom),
         add = TRUE, col = col_vec[j],
         cex = (line_w[j] + 0.3) / 2,
         pch = '+')
}

## add legend

legend(x = 'bottomright',
       legend = c('Bus', 'Trolley', 'Subway BSL', 'Subway MSL', '', 'Station / Stop'),
       col = c(col_vals_bus, col_vals_trolley,
               col_vals_subway, NA, 'black'),
       lty = c(rep(1, 4), NA, NA),
       lwd = c(rep(2, 4), NA, NA),
       pch = c(rep(NA, 5), '+'),
       pt.cex = c(rep(NA, 5), 1.2),
       bty = 'n')

dev.off()
