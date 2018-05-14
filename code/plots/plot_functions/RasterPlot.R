#' for plotting pngs instead of boring points
#'
#' @param points
#' points to be plotted, can be sf or spatial points or just long/lat mat
#'
#' @param png_image
#' image you want to plot,
#' generally will be an array,
#' something like 256 x 256 x 4
#'
#' @param png_height
#' how tall in metres?
#'
#' @param png_width
#' how wide in metres? if null, same as height
#'
#' @param alpha_val
#' how opaque / transparent
#' actually it's a multiplier:
#' if the image is already at half transparency,
#' alpha_val = 0.5 will make it a quarter
#' (and 2 will bring it back up to full!)
RasterPlot <- function(points,
                        png_image,
                        png_height = 20,
                        png_width = NULL,
                        alpha_val = 1){

    if(sum(match(class(points), c('SpatialPointsDataFrame', 'SpatialPoints'), nomatch = 0)) > 0){
        coords_mat = slot(points, 'coords')
    } else if('sf' %in% class(points) || 'sfc' %in% class(points)){
        coords_mat = st_coordinates(points)
    } else {
        coords_mat = points
    }

    if(is.null(png_width)){
        png_width = png_height
    }

    bottomleft_mat <- matrix(cbind(rep(-png_height / 2, nrow(coords_mat)),
                                   rep(-png_width / 2, nrow(coords_mat))),
                             ncol = 2)    
    topright_mat <- - bottomleft_mat
    
    bottom_left_m <- AddMetreLL(coords_mat, bottomleft_mat)
    top_right_m <- AddMetreLL(coords_mat, topright_mat)

    if(alpha_val < 1){
        png_image[,,4] <- png_image[,,4] * alpha_val
    }

    rasterImage(png_image,
                xleft = bottom_left_m[,1],
                ybottom = bottom_left_m[,2],
                xright = top_right_m[,1],
                ytop = top_right_m[,2])
}
