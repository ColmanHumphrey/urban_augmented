##------------------------------------
## this file pulls the old stuff together
## organises by nearest intersection
## i.e. data from the previous paper

source('../load_packages.R')

library(FNN) # fast nearest neighbors
library(Matrix) ## sparse for block multiply

data_folder = '../../data/'

## loading in data functions

source('../data_functions/KNNApprox.R')
source('../data_functions/GetAspectRatio.R')
source('../data_functions/ApproximateDistance.R')
source('../data_functions/CreateCircles.R')

source('../plots/plot_functions/AddMetreLL.R')

##------------------------------------

## load inter_points
load(file = paste0(data_folder, 'inter_points.rdata'))

## old:
## blocks etc, i.e. demographic data
## note that these datasets (block_data for example) already
## contain aggregated information about landuse and crime, but
## we go back to the source for our intersections
load(file = paste0(data_folder, 'prior_data/block_and_group.rdata'))

## load landuse
load(file = paste0(data_folder, 'prior_data/landuse.rdata'))

## load crimes
## load(file = paste0(data_folder, 'prior_data/crime.rdata'))
## redone!
load(file = paste0(data_folder, 'crime.rdata'))

## load business frame
load(file = paste0(data_folder, 'prior_data/business_frame.rdata'))

##------------------------------------

## for anything that requires an intersection,
## landuse, blocks
## use the circles
inter_circles = CreateCircles(inter_sf,
                              radius = 50)

## else use the points, using KNNApprox (very fast)
inter_points_mat <- st_coordinates(inter_sf)

summary_list_old <- list()

##------------------------------------
## CRIME:

prior_crime_knn <- KNNApprox(inter_points_mat,
                             st_coordinates(prior_crime_points),
                             k = 1)
near_inter <- prior_crime_knn[['nn.index']][,1]
dist_to_inter <- prior_crime_knn[['nn.dist']][,1]
## just over half are within a circle
near_inter[dist_to_inter > 50] = NA

prior_crime$inter_index <- near_inter

post_crime_knn <- KNNApprox(inter_points_mat,
                             st_coordinates(post_crime_points),
                             k = 1)
near_inter <- post_crime_knn[['nn.index']][,1]
dist_to_inter <- post_crime_knn[['nn.dist']][,1]
## just over half are within a circle
near_inter[dist_to_inter > 50] = NA

post_crime$inter_index <- near_inter

summary_prior <- (prior_crime %>%
    group_by(inter_index, crime_cat) %>%
    summarise(count = n())) %>% spread(key = crime_cat, value = count)
## ignore NA in index
summary_prior %<>% filter(!is.na(inter_index))
## NAs are zeros for values
summary_prior[is.na(summary_prior)] = 0
## drop other, vice, add total:
summary_prior %<>% select(-c(other, vice)) %>% mutate(total = nonviolent + violent)

summary_post <- (post_crime %>%
    group_by(inter_index, crime_cat) %>%
    summarise(count = n())) %>% spread(key = crime_cat, value = count)
## ignore NA in index
summary_post %<>% filter(!is.na(inter_index))
## NAs are zeros for values
summary_post[is.na(summary_post)] = 0
## drop other, vice, add total:
summary_post %<>% select(-c(other, vice)) %>% mutate(total = nonviolent + violent)

summary_list_old[['prior_crime']] = summary_prior
summary_list_old[['post_crime']] = summary_post

##------------------------------------
## business frame:
## just want to add inter index, nothing else
business_knn <- KNNApprox(data_mat = inter_points_mat,
                          query_mat = as.matrix(business_frame %>% select(lng, lat)),
                          k = 1)

near_inter <- business_knn[['nn.index']][,1]
dist_from_inter <- business_knn[['nn.dist']][,1]
## only 40%
near_inter[dist_from_inter > 50] = NA

business_frame$inter_index = near_inter

save(business_frame,
     file = paste0(data_folder, 'business_frame.rdata'))

##------------------------------------
## now some "hard" work,
## intersections...

## first, landuse
landuse_within_ind <- st_within(landuse,
                                inter_circles)

## check that it's at most one
landuse$within_ind <- lengths(landuse_within_ind)

landuse_within <- landuse %>% filter(within_ind == 1)

landuse_not <- landuse %>% filter(within_ind == 0)

## first, within:
landuse_within$inter_index <- unlist(landuse_within_ind)

landuse_inters <- st_intersection(inter_circles %>% select(id, geometry),
                                  landuse_not %>% select(use_detail, geometry))

landuse_inters %<>% rename(inter_index = id)
landuse_inters$area = st_area(landuse_inters)

## combine the dataframes
landuse_circles <- rbind(
    landuse_inters,
    landuse_within %>% select_(.dots = names(landuse_inters)))

## easier aggregation (but bad for plotting!)
st_geometry(landuse_circles) <- NULL

## aggregate counts and areas:
summary_landuse_long <- landuse_circles %>%
    group_by(inter_index,
             use_detail) %>%
    summarise(count = n(),
              total_area = sum(area))

summary_landuse_area <- summary_landuse_long %>%
    select(-count) %>% 
    spread(key = use_detail,
           value = total_area)
## there should be no NAs in inter_index:
summary_landuse_area[is.na(summary_landuse_area)] = 0
summary_landuse_area$total_area <- rowSums(summary_landuse_area) - summary_landuse_area$inter_index

names(summary_landuse_area) = paste0('area_', names(summary_landuse_area))
names(summary_landuse_area)[1] = 'inter_index'
names(summary_landuse_area)[ncol(summary_landuse_area)] = 'total_area'

summary_landuse_count <- summary_landuse_long %>%
    select(-total_area) %>% 
    spread(key = use_detail,
           value = count)
## there should be no NAs in inter_index:
summary_landuse_count[is.na(summary_landuse_count)] = 0
summary_landuse_count$total_count <- rowSums(summary_landuse_count) - summary_landuse_count$inter_index

names(summary_landuse_count) = paste0('count_', names(summary_landuse_count))
names(summary_landuse_count)[1] = 'inter_index'
names(summary_landuse_count)[ncol(summary_landuse_count)] = 'total_count'

summary_list_old[['landuse_area']] = summary_landuse_area
summary_list_old[['landuse_count']] = summary_landuse_count

##------------------------------------
## last one
## blocks
## sparse matrices!

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

## now for calcs:

## population,
## we want area weighted
block_pop_counts <- as.matrix(block_data %>% select(white, black, asian, other, hispanic))
areaprop_pop_counts <- as.matrix(block_prop_matrix %*% block_pop_counts)

##----------------
## income, poverty
## here, we weight the blockgroups by population estimates
## but on the blockgroup scale, so have to redo the above...
blockgroup_geom$blockgroup_index = 1:nrow(blockgroup_geom)

blockgroup_inters <- st_intersection(inter_circles %>% select(id, geometry),
                                     blockgroup_geom %>% select(blockgroup_index, geometry))
blockgroup_inters %<>% rename(inter_index = id)
blockgroup_inters %<>% mutate(area = st_area(geometry))
## check areas if you want
## basically all will be exactly as they should, except "border" circles
## something to bear in mind actually...

blockgroup_inter_matrix <- sparseMatrix(i = blockgroup_inters$inter_index,
                                        j = blockgroup_inters$blockgroup_index,
                                        x = as.numeric(blockgroup_inters$area),
                                        dims = c(nrow(inter_sf),
                                                 nrow(blockgroup_data)))
## turn this into proportion matrix:
## but now it's the area proportion TIMES pop. count
## for pop. weighted sums
blockgroup_areapop_diag <-.sparseDiagonal(
    n = nrow(blockgroup_geom),
    x = (blockgroup_data %>% pull(total)) / (as.numeric(blockgroup_geom$area)))
blockgroup_proppop_matrix <- blockgroup_inter_matrix %*% blockgroup_areapop_diag

## and the relevant stuff:
blockgroup_money <- as.matrix(blockgroup_data %>% select(income, poverty_metric))

## dealing with NAs:
## replace with zero
## both in proportion and in the blockgroup_money matrix


## allprop_money_counts_pre <- as.matrix(blockgroup_proppop_matrix %*% blockgroup_money)
## allprop_money_counts <- sweep(allprop_money_counts_pre, 1, rowSums(blockgroup_proppop_matrix), '/')

blockgroup_proppop_matrix_income <- blockgroup_proppop_matrix
blockgroup_proppop_matrix_income[,is.na(blockgroup_money[,1])] = 0

blockgroup_proppop_matrix_poverty <- blockgroup_proppop_matrix
blockgroup_proppop_matrix_poverty[,is.na(blockgroup_money[,2])] = 0

blockgroup_money[is.na(blockgroup_money)] = 0

income_counts <- as.numeric(blockgroup_proppop_matrix_income %*% blockgroup_money[,1,drop = FALSE]) /
    rowSums(blockgroup_proppop_matrix_income)
poverty_counts <- as.numeric(blockgroup_proppop_matrix_poverty %*% blockgroup_money[,2,drop = FALSE]) /
    rowSums(blockgroup_proppop_matrix_poverty)

popprop_money_counts <- cbind(income_counts,
                              poverty_counts)
                                  
##
summary_demo <- data_frame(
    inter_index = 1:nrow(inter_sf),

    white_prop = areaprop_pop_counts[,1],
    black_prop = areaprop_pop_counts[,2],
    asian_prop = areaprop_pop_counts[,3],
    other_prop = areaprop_pop_counts[,4],
    hispanic_prop = areaprop_pop_counts[,5],
    total = rowSums(areaprop_pop_counts),

    income = popprop_money_counts[,1],
    poverty = popprop_money_counts[,2])

summary_demo %<>%
    mutate(white_prop = white_prop / total,
           black_prop = black_prop / total,
           asian_prop = asian_prop / total,
           other_prop = other_prop / total,
           hispanic_prop = hispanic_prop / total)

summary_list_old[['demo']] = summary_demo

##------------------------------------

save(summary_list_old,
     file = paste0(data_folder, 'summary_lists/summary_list_old.rdata'))

##------------------------------------
