#' takes in lon/lat positions to get distances from,
#' adjusts for sphere etc, calcs distances.
#'
#' This function has some checks! Speed ratios for nrow(dist_pos):
#' nrow   ratio: time in pure distGeo / time here
#' ------- | --------
#'     10    0.3
#'    100    0.8
#'    200    1 (!) 
#'  1,000    3.3
#' 10,000    8.6
#'
#' @param rel_pos
#' position to calc distance from;
#' want to be accurate from here, so this is our
#' reference point
#'
#' @param dist_pos
#' position to calculate distances to
ApproximateDistance <- function(rel_pos,
                                dist_pos){
    if(length(rel_pos) > 2){
        stop("nrow(rel_pos) > 2, just run a loop, won't be slower")
    }
    
    lng_lat_mult <- GetAspectRatio(rel_pos, ret_ratio = FALSE)

    if(length(dist_pos) > 2){        
        diff_pos = sweep(dist_pos, 2, rel_pos, '-')
        diff_pos = sweep(diff_pos, 2, lng_lat_mult, '*')

        dist_vec = sqrt(rowSums(diff_pos^2))
    } else {
        dist_vec = distGeo(rel_pos,
                           dist_pos)
    }

    return(dist_vec)
}
