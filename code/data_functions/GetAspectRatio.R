#' Gives the lon/lat ratio near your point
#' Or the actual metre distance if you need that
#'
#' @param lonlat
#' vector with a lon and lat value
#'
#' @param ret_ratio
#' logical, default TRUE;
#' return the ratio? or both mults?
GetAspectRatio <- function(lonlat, ret_ratio = TRUE){
    lng_mult <- distGeo(p1 = lonlat - c(0.005,0),
                        p2 = lonlat + c(0.005,0)) * 100
    lat_mult <- distGeo(p1 = lonlat - c(0, 0.005),
                        p2 = lonlat + c(0, 0.005)) * 100
    ll_ratio <- lng_mult / lat_mult
    if(ret_ratio){
        return(ll_ratio)
    } else {
        return(c(lng_mult, lat_mult))
    }
}
