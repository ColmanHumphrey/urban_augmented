#' take long/lat coords, and (x,y) metres to add,
#' return new coords
#'
#' @param coords_mat
#' matrix of coordintes, long/lat
#'
#' @param metres_mat
#' matrix of dists in metres to add
AddMetreLL <- function(coords_mat,
                       metres_mat){
    ## earth's radius in metres
    earth_radius = 6378137

    if(length(coords_mat) == 2){
        coords_mat = matrix(coords_mat, nrow = nrow(metres_mat), ncol = 2, byrow = TRUE)
    }

    d_lat = metres_mat[,2] / earth_radius
    d_lon = metres_mat[,1] / (earth_radius * cos(pi * coords_mat[,2] / 180))

    new_coords = coords_mat + cbind(d_lon * 180 / pi, d_lat * 180 / pi)

    return(new_coords)
}
