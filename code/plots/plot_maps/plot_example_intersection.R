##------------------------------------
## this file pulls the old stuff together
## organises by nearest intersection
## i.e. data from the previous paper

source('../../load_packages.R')

data_folder = '../../../data/'
plot_folder = '../../../plots/'

source('../plot_functions/AddMetreLL.R')
source('../plot_functions/PlotGoogleMap.R')

source('../../data_functions/CreateCircles.R')

##------------------------------------

load(file = paste0(data_folder, 'inter_points.rdata'))
load(file = paste0(data_folder, 'prior_data/block_and_group.rdata'))

##------------------------------------

inter_circles = CreateCircles(inter_sf,
                              radius = 50)

## here we redo some of the work of combine_old...:

block_geom$block_index = 1:nrow(block_geom)

block_inters <- st_intersection(inter_circles %>% select(id, geometry),
                                block_geom %>% select(block_index, geometry))
block_inters %<>% rename(inter_index = id)
block_inters %<>% mutate(area = st_area(geometry))
## check areas if you want
## basically all will be exactly as they should, except "border" circles
## something to bear in mind actually...

block_inter_matrix <- sparseMatrix(i = block_inters$inter_index,
                                   j = block_inters$block_index,
                                   x = as.numeric(block_inters$area),
                                   dims = c(nrow(inter_sf),
                                            nrow(block_data)))
## turn this into proportion matrix:
block_areas_diag <- .sparseDiagonal(n = nrow(block_geom), x = 1 / as.numeric(block_geom$area))
block_prop_matrix <- block_inter_matrix %*% block_areas_diag

## population,
## we want area weighted
block_pop_counts <- as.matrix(block_data %>% select(white, black, asian, other, hispanic))
areaprop_pop_counts <- as.matrix(block_prop_matrix %*% block_pop_counts)

##------------------------------------

## the plot:

 
ii = 555

## dev.off()
png(filename = paste0(plot_folder, 'plot_maps/plot_example_intersection.png'),
    width = 2000, height = 1600, res = 500, pointsize = 6)

PlotGoogleMap(CreateCircles(inter_sf[ii,], radius = 150))
plot(st_geometry(inter_circles[ii,]), add = TRUE)

plot(st_geometry(block_geom[block_inters %>% filter(inter_index == ii) %>% pull(block_index),]),
     col = AddAlpha('blue', 0.6),
     add = TRUE)

plot(st_geometry(block_inters %>% filter(inter_index == ii)),
     col = AddAlpha('red', 0.8),
     add = TRUE)

rel_centroid <- st_coordinates(st_centroid(
    block_geom[block_inters %>% filter(inter_index == ii) %>% pull(block_index),]))
text(rel_centroid[,'X'],
     rel_centroid[,'Y'],
     labels = block_inters %>% filter(inter_index == ii) %>% pull(block_index),
     col = 'greenyellow',
     cex = 0.8)

prop_rel = block_prop_matrix[ii,]
prop_nonzero <- which(prop_rel > 0)
prop_vals = prop_rel[prop_nonzero]

inter_centroid <- st_coordinates(st_centroid(
    block_inters %>% filter(inter_index == ii)))

## text(inter_centroid[,'X'],
##      inter_centroid[,'Y'],
##      labels = round(prop_vals, 3),
##      cex = 0.8,
##      col = 'white')

## manual adjustment...:
text(inter_centroid[-2,'X'],
     inter_centroid[-2,'Y'],
     labels = round(prop_vals[-2], 3),
     cex = 0.8,
     col = 'white')

text(inter_centroid[2,'X'],
     inter_centroid[2,'Y'],
     adj = c(0,0),
     labels = round(prop_vals[2], 3),
     cex = 0.8,
     col = 'white')

dev.off()
