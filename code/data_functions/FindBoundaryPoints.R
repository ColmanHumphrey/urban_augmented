#' find "boundary" points...:
#' basically, find the maximum angle to see the edge,
#' and then see which obey this
#'
#' @param points_mat
#' matrix of lon/lat points
#'
#' @param max_angle
#' angle to edge requried to be an edge points
FindBoundaryPoints <- function(points_mat,
                               max_angle = pi / 2){
    ll_ratio <- GetAspectRatio(apply(points_mat[,1:2], 2, median))

    points_mat[,1] <- points_mat[,1] * ll_ratio

    max_angle_vec = rep(NA, nrow(points_mat))
    
    for(j in 1:nrow(points_mat)){
        x_vec = points_mat[,1] - points_mat[j,1]
        y_vec = points_mat[,2] - points_mat[j,2]
        angle_vec = atan(y_vec / x_vec)
        angle_vec[x_vec < 0] = angle_vec[x_vec < 0] + pi
        
        sort_angles = sort(angle_vec[-j])
        diff_angles = c(sort_angles[2:(length(sort_angles))] -
                        sort_angles[1:(length(sort_angles) - 1)],
                        2 * pi + min(sort_angles) - max(sort_angles))
        max_angle_vec[j] = max(diff_angles)
    }
    
    return(max_angle_vec > max_angle)
}
