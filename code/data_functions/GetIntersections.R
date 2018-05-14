#' this function gives all intersections that are at least 40m apart
#' NOTE: order is important... since it tries to succesively add
#' AS OF NOW every new potential inter is checked against all others,
#' so don't throw a million streets in
#'
#' The function will not add an intersection between roads with the
#' name name,
#' using ST_LABEL,
#' because many roads are split that way, but form "fake" intersections
#' 
#' @param rel_streets
#' frame with geometry (lines), ST_LABEL (names)
#' that will form our intersections
#'
#' @param block_same_name
#' logical, default TRUE: do you want to block "self intersections"?
#' i.e. can E 70th intersect with a different section of E 70th?
#' set to false for all vertices
#'
#' @param approx_dist
#' logical, default TRUE;
#' use exact distances, or just use an approximation by
#' assuming an x/y plane, and using GetAspectRatio to solve?
#'
#' @param min_dist
#' min_dist in M (I guess) that new inters must be apart from
#' already grabbed ones
GetIntersections <- function(rel_streets,
                             block_same_name = TRUE,
                             approx_dist = TRUE,                      
                             min_dist = 40){
    if(nrow(rel_streets) > 10000){
        message('More than 10,000 streets: this could mean a lot of pairwise comparisons')
    }
    
    inter_frame <- data_frame(
        road_l = rep(0, 0),
        road_s = rep(0, 0),
        lon = rep(0, 0),
        lat = rep(0, 0))

    for(j in 1:(nrow(rel_streets) - 1)){
        print(j)

        street_label = rel_streets$FULLNAME[j]
        
        ## plot(rel_streets[j,] %>% select(len), add = TRUE)

        ## find intersecting streets        
        has_inter = which(
            suppressMessages(
                st_intersects(rel_streets[j,],
                              rel_streets[(j + 1):nrow(rel_streets),], sparse = FALSE))) + j

        ## go through each, adding points if "viable" (far enough away to be relevant)
        for(hi in has_inter){
            ## a street shouldn't self intersect:
            inter_label = rel_streets$FULLNAME[hi]
            if(!block_same_name || !identical(street_label, inter_label)){        
                inter_temp = suppressMessages(suppressWarnings(st_intersection(rel_streets[j,],
                                                                               rel_streets[hi,])))
                geom_temp = inter_temp %>% pull(geometry)

                ## if this is multipoint,
                ## break up into points and do this for each
                if('sfc_POINT' %in% class(geom_temp) || 'sfc_MULTIPOINT' %in% class(geom_temp)){
                    p_mat = st_coordinates(geom_temp)

                    if(is.null(nrow(p_mat)) || nrow(p_mat) < 1){
                        stop(paste0('nrow(p_mat) error at j = ', j, ', hi = ', hi))
                    }
                    
                    for(k in 1:nrow(p_mat)){
                        p_vec = p_mat[k,1:2,drop = FALSE] ## dont' think we need to keep the matrix, but it's OK
                        
                        if(nrow(inter_frame) > 0 && min_dist > 0){
                            if(approx_dist && nrow(inter_frame) > 200){
                                inter_dists = ApproximateDistance(
                                    rel_pos = p_vec,
                                    dist_pos = as.matrix(inter_frame %>% select(lon, lat)))
                            } else {
                                inter_dists = distGeo(p1 = p_vec,
                                                      p2 = as.matrix(inter_frame %>% select(lon, lat)))
                            }
                            if(min(inter_dists) >= min_dist){
                                inter_frame[nrow(inter_frame) + 1,] = c(j, hi, p_vec)
                            }
                        } else {
                            ## i.e. if it's the first row, or you don't care about min_dist
                            inter_frame[nrow(inter_frame) + 1,] = c(j, hi, p_vec)
                        }
                    }
                } else {
                    print(paste0('With j = ', j, ' and hi = ', hi, ', we have class = ', class(geom_temp)))
                }
            }
        } ## end has inter loop
    } ## end streets loop

    return(inter_frame)
}

