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

require(png)

## load in traffic controls
load(file = paste0(data_folder, 'inter_controls.rdata'))
## streets again for context
load(file = paste0(data_folder, 'streets.rdata'))

## constrain to... censustracts 15:18
rel_geom <- st_union(blockgroup_geom %>% filter(censustract >= 1500 & censustract < 1900))

## load in images:
## stop_png <- readPNG('../../../images/stop_sign_small.png')
## yield_png <- readPNG('../../../images/yield_sign_small.png')
## lights_png <- readPNG('../../../images/traffic_light_small.png')
stop_png <- readPNG('../../../images/stop_sign.png')
yield_png <- readPNG('../../../images/yield_sign.png')
lights_png <- readPNG('../../../images/traffic_light.png')

lights_h = 55
sign_alp = 1

## plot_ind <- c(st_contains(rel_geom, inter_controls, sparse = FALSE))
plot_ind = TRUE

## oddly plotting pngs is where quartz shines and pdfs suck
## hence the png
png(file = paste0(plot_folder, 'plot_maps/philly_traffic_controls.png'),
    width = 2000, height = 2000)
PlotGoogleMap(rel_geom,
              g_zoom = 15,
              fill_alp = 0.6)

colvec = rev(ColGen(rank(streets$length),
                plot_cols = viridis_pal()(50)[1:40]))

plot(st_geometry(streets),
     col = colvec,
     lwd = 4, ## 4 only because of the png
     add = TRUE)

RasterPlot(st_geometry(inter_controls %>% filter(plot_ind & STOPTYPE == 'Signalized')),
           lights_png,
           lights_h,
           lights_h,
           sign_alp)

stop_h = lights_h * 0.7
RasterPlot(st_geometry(inter_controls %>% filter(plot_ind & STOPTYPE == 'All Way')),
           stop_png,
           stop_h,
           stop_h,
           sign_alp)

## conv_h = lights_h * 0.7
## RasterPlot(st_geometry(inter_controls %>% filter(plot_ind & STOPTYPE == 'Conventional')),
##            yield_png,
##            conv_h,
##            conv_h,
##            sign_alp)
dev.off()

