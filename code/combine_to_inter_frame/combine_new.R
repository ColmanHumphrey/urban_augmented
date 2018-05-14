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

## load property data
load(file = paste0(data_folder, 'prop_data.rdata'))

## load school data
load(file = paste0(data_folder, 'school_data.rdata'))

## load transit data
load(file = paste0(data_folder, 'transit_stops.rdata'))

## load traffic controls
load(file = paste0(data_folder, 'inter_controls.rdata'))

##------------------------------------

## for anything that requires an intersection,
## landuse, blocks
## use the circles
inter_circles = CreateCircles(inter_sf,
                              radius = 50)

## else use the points, using KNNApprox (very fast)
inter_points_mat <- st_coordinates(inter_sf)

summary_list_new <- list()

##------------------------------------
## PROPERTY

property_knn <- KNNApprox(inter_points_mat,
                          st_coordinates(prop_sf),
                          k = 1)
near_inter <- property_knn[['nn.index']][,1]
dist_to_inter <- property_knn[['nn.dist']][,1]
## sadly only 30% within circles
near_inter[dist_to_inter > 50] = NA

prop_data$inter_index = near_inter

summary_property <- prop_data %>%
    mutate(Number_Stories = ifelse(Number_Stories == 0, 1, Number_Stories),
           Total_Area = ifelse(Number_Stories == 1, pmax(Total_Area, Total_Livable_Area),
                               Total_Area),
           Year_Built = ifelse(Year_Built < 1801, NA, Year_Built),
           full_area = Total_Area * Number_Stories,
           price_sqft = Market_Value / full_area,
           use_area_ind = Total_Area > 100 & Total_Area < 30000) %>% 
    group_by(inter_index) %>%
    summarise(count = n(),
              mean_value = mean(Market_Value, na.rm = TRUE),
              mean_age = mean(2018 - Year_Built, na.rm = TRUE),
              age_dev = mean(abs(Year_Built - median(Year_Built, na.rm = TRUE)), na.rm = TRUE),
              mean_stories = mean(Number_Stories, na.rm = TRUE),
              mean_garages = mean(Garage_Spaces, na.rm = TRUE),
              price_per_sqft = sum(Market_Value[use_area_ind], na.rm = TRUE) /
                  sum(full_area[use_area_ind], na.rm = TRUE),
              price_sqft_dev = sum(abs(price_sqft[use_area_ind] -
                                       median(price_sqft[use_area_ind], na.rm = TRUE)) *
                                   full_area[use_area_ind], na.rm = TRUE) /
                  sum(full_area[use_area_ind], na.rm = TRUE))

summary_property %<>% filter(!is.na(inter_index))

summary_list_new[['property']] = summary_property

##------------------------------------
## SCHOOL

## let's ignore schools with <= 150 students
school_data %<>% filter(ENROLLMENT > 150)

school_dist_mat <- matrix(NA,
                          nrow = nrow(inter_points_mat),
                          ncol = nrow(school_data))

school_coords = st_coordinates(school_data)
for(j in 1:nrow(school_data)){
    school_dist_mat[,j] = distGeo(school_coords[j,],
                                  inter_points_mat)
}

## nearest elem and its count,
## nearest high and its count
elem_dist <- apply(school_dist_mat[,school_data$elem_cat], 1, min)
elem_count <- school_data$ENROLLMENT[
                              which(school_data$elem_cat)[
                                  apply(school_dist_mat[,school_data$elem_cat], 1, which.min)]]
high_dist <- apply(school_dist_mat[,school_data$high_cat], 1, min)
high_count <- school_data$ENROLLMENT[
                              which(school_data$high_cat)[
                                  apply(school_dist_mat[,school_data$high_cat], 1, which.min)]]

## the smoothing
kern = 2

kern_dist_mult <- exp(-kern * (school_dist_mat / 1000)^2)

enroll_kern <- kern_dist_mult %*% diag(school_data$ENROLLMENT)

elem_smooth <- rowSums(enroll_kern[,school_data$elem_cat])
high_smooth <- rowSums(enroll_kern[,school_data$high_cat])

summary_school <- data_frame(
    inter_index = 1:nrow(inter_points_mat),
    elem_dist = elem_dist,
    elem_count = elem_count,
    elem_smooth = elem_smooth,
    high_dist = high_dist,
    high_count = high_count,
    high_smooth = high_smooth)

summary_list_new[['school']] = summary_school
    
##------------------------------------

## transit

transit_knn <- KNNApprox(data_mat = inter_points_mat,
                         query_mat = st_coordinates(transit_stops),
                         k = 1)

near_inter <- transit_knn[['nn.index']][,1]
dist_to_inter <- transit_knn[['nn.dist']][,1]
## half within
## 84% of subway stops
near_inter[dist_to_inter > 50] = NA

transit_stops$inter_index = near_inter

summary_transit <- transit_stops %>%
    group_by(inter_index) %>%
    summarise(count = n(),
              routes = length(unique(route_name)),
              subway_routes = length(unique(route_name[subway])),
              subway = sum(subway),
              bus_routes = length(unique(route_name[bus])),
              bus = sum(bus),
              trolley_routes = length(unique(route_name[trolley])),
              trolley = sum(trolley))
              
summary_transit %<>% filter(!is.na(inter_index))

## go with routes

summary_transit %<>% select(
                         inter_index,
                         subway_routes,
                         bus_routes,
                         trolley_routes)

summary_list_new[['transit']] = summary_transit

##------------------------------------

## the stop signs etc...:
## switch order!!
## inter points are the query
control_knn <- KNNApprox(data_mat = st_coordinates(inter_controls),
                         query_mat = inter_points_mat,
                         k = 1)

near_inter <- control_knn[['nn.index']][,1]
dist_from_inter <- control_knn[['nn.dist']][,1]
## oddly nearly all right around one
## 84% are < 5m
near_inter[dist_from_inter > 5] = NA

## those that are NA will be "conventional"

summary_controls <- data_frame(
    inter_index = 1:nrow(inter_points_mat),
    traffic_lights = FALSE,
    stop_sign_allway = FALSE)

stop_type = inter_controls$STOPTYPE[near_inter]
stop_type[is.na(stop_type)] = 'Conventional'

summary_controls$traffic_lights[stop_type == 'Signalized'] = TRUE
summary_controls$stop_sign_allway[stop_type == 'All Way'] = TRUE

summary_list_new[['controls']] = summary_controls

##------------------------------------

save(summary_list_new,
     file = paste0(data_folder, 'summary_lists/summary_list_new.rdata'))

##------------------------------------

