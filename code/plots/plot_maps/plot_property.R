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

## property plot
load(file = paste0(data_folder, 'prop_data.rdata'))




size_cex = sqrt(prop_data$Total_Area) / 1000
## temp = size_cex[size_cex > 0.1]
## size_cex[size_cex > 0.1] = (temp-0.1) / 10 + 0.1 ## fine...

value_col = ColGen(prop_data$Market_Value,
                   quantile_col = TRUE,
                   n_col = 40,
                   plot_cols = viridis_pal()(50)[40:1])
col_leg = ColGen(prop_data$Market_Value,
                 quantile_col = TRUE,
                 n_col = 40,
                 plot_cols = viridis_pal()(50)[40:1],
                 ret_legend = TRUE)

pdf(file = paste0(plot_folder, 'plot_maps/plot_market_value.pdf'))

layout(mat = matrix(1:2, nrow = 2),
       heights = c(1, 0.2))

PlotGoogleMap(County_geom,
              g_zoom = 11,
              g_maptype = 'satellite',
              fill_alp = 0.9)

plot(prop_sf,
     cex = size_cex,
     col = value_col,
     pch = 18,
     add = TRUE)

## draw a legend:

par(mar = c(0,0,0,0))
plot(x = 0.5, y = 0.5,
     axes = FALSE,
     col = 'transparent',     
     xlim = c(0, 1),
     ylim = c(0, 1),
     xaxt = 'n',
     yaxt = 'n')

x_seq <- seq(from = 0.1,
             to = 0.9,
             length.out = length(col_leg$cols) + 1)
fudge = (x_seq[2] - x_seq[1]) / 20
rect(xleft = x_seq[1:length(col_leg$cols)] - fudge,
     xright = x_seq[2:(length(col_leg$cols) + 1)] + fudge,
     ybottom = 0.2, ytop = 0.5,
     col = col_leg$cols, border = NA)

text_ind = 0:10 * 4 + 1

text(x = x_seq[text_ind],
     y = 0.5,
     pos = 3,
     labels = round(col_leg$divs[text_ind] / 1000, 1),
     cex = 0.7)
text(x = mean(x_seq),
     y = 0.7,
     pos = 3,
     labels = 'Market Value ($1,000s)',
     font = 1)

dev.off()

##------------------------------------
