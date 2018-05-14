#' planar approx, squishing the lng/lat axes
#' ASSUMING first col is long, second is lat.
#' lon is the one that gets squished, so
#' adjust any other cols accordingly
#'
#' @param data_mat
#' this is the data matrix:
#' these are the neighbours to be found
#'
#' @param query_mat
#' this is the query matrix:
#' these are rows we want to find neighbours for
#'
#' @param k
#' how many neighbours to return?
#'
#' @param true_dist
#' logical, default TRUE; this multiplies up the
#' lonlats so that the KNN distance matrix is the
#' actual distance.
KNNApprox <- function(data_mat, query_mat, k = 500, true_dist = TRUE){
    if(true_dist){
        med_data = apply(data_mat[,1:2], 2, median)

        ll_ratio <- GetAspectRatio(med_data, FALSE)

        ## if you don't shift to something close to zero,
        ## you get severe floating point stuff
        data_mat[,1] <- (data_mat[,1] - med_data[1]) * ll_ratio[1]
        data_mat[,2] <- (data_mat[,2] - med_data[2]) * ll_ratio[2]

        query_mat[,1] <- (query_mat[,1] - med_data[1]) * ll_ratio[1]
        query_mat[,2] <- (query_mat[,2] - med_data[2]) * ll_ratio[2]
    } else {
        ll_ratio <- GetAspectRatio(apply(data_mat[,1:2], 2, median), TRUE)

        data_mat[,1] <- data_mat[,1] * ll_ratio
        query_mat[,1] <- query_mat[,1] * ll_ratio
    }
    
    knn_out = get.knnx(data = data_mat,
                       query = query_mat,
                       k = k) ## this is an approximation
    return(knn_out)
}


