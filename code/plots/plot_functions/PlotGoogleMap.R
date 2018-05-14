#' You give a shapefile, it adds a (Google) map
#' behind it, with optional color,
#' and potentially a 'fading' box over the map
#'
#' @param shape_file
#' Standard shapefile, with a bounding box being
#' some (long/lat) coordinates
#'
#' @param plot_shape_file
#' logical, default FALSE; plot the shape?
#'
#' @param expand_factor
#' Expands the bounding box so the plot has some outline
#'
#' @param default_par
#' logical, do you want to use 'my' defaults of no
#' perimeter?
#'
#' @param g_col
#' 'bw' or 'color' for the google map
#'
#' @param g_zoom
#' you can leave this NULL and have the function choose,
#' but it can be conservative. If you run the code and want
#' more detail, try a larger zoom (by 1)
#'
#' @param g_maptype
#' same as in get_googlemap:
#' "terrain", "satellite", "roadmap", and "hybrid"
#'
#' @param add_box
#' do you want to add a slightly opaque rectangle over your map?
#' useful for making the map less invasive on details that you're plotting
#'
#' @param fill_colour
#' what colour for the above box
#'
#' @param fill_alp
#' what alpha value for the above box
#'
#' @param shape_border_col
#' You want to plot the actual shapefile, what colour?
#'
#' @param ret_xy
#' logical, default FALSE; do you want to return the outlines?
#'
#' @param shape_lwd
#' and what line width?
PlotGoogleMap <- function(shape_file = block_geom,
                          plot_shape_file = FALSE,
                          use_box = FALSE,
                          expand_factor = 1.05,
                          g_size = c(640, 640),
                          default_par = TRUE,
                          g_col = 'bw',
                          g_zoom = NULL,
                          g_maptype = 'roadmap',
                          add_box = TRUE,
                          fill_colour = 'white',
                          fill_alp = 0.5,
                          shape_border_col = 'black',
                          ret_xy = FALSE,
                          shape_lwd = 2){
    
    b_box = ExpandBox(st_bbox(shape_file), 1.05)

    b_center = apply(b_box, 1, mean)

    if(is.null(g_zoom)){
        g_zoom = MaxZoom(b_box['y',], b_box['x',], size = g_size)
    }

    message(paste0('Zoom Level: ', g_zoom))
    g_map = get_googlemap(b_center,
                          zoom = g_zoom,
                          size = g_size,
                          scale = 2,
                          maptype = g_maptype,
                          color = g_col)

    xlim_lon = as.numeric(attr(g_map,'bb')[c(2,4)])
    ylim_lat = as.numeric(attr(g_map,'bb')[c(1,3)])    

    ## asp_ratio = size[2] / size[1] * xdiff / ydiff

    if(default_par){
        par(usr = c(0,1,0,1), mar = c(0,0,0,0), omi = c(0,0,0,0), mai = c(0,0,0,0),
            xaxs = 'i', yaxs = 'i')
    }

    ## other: plot the bounding box
    box_poly = CreateBoxPoly(b_box)

    ## just a plain box
    if(use_box){
        plot(st_geometry(box_poly), border = 'transparent', col = 'transparent')
    } else {
        plot(st_geometry(shape_file), border = NA, col = NA)
    }
    
    ## par(xpd = NA)
    rasterImage(g_map, xlim_lon[1], ylim_lat[1], xlim_lon[2], ylim_lat[2])
    if(add_box){
        fill_col = AddAlpha(fill_colour, fill_alp)
        rect(xlim_lon[1], ylim_lat[1], xlim_lon[2], ylim_lat[2], col = fill_col, border = NA)
    }
    if(plot_shape_file){
        plot(shape_file, add = TRUE, border = shape_border_col, lwd = shape_lwd)
    }

    if(ret_xy){        
        return(list(lon = xlim_lon,
                    lat = ylim_lat))
    }
}

## takes in box_vec and infl factor, returns larger box
#'
#' @param box_vec
#' output from st_bbox:
#' c(xmin, ymin, xmax, ymax)
#'
#' @param infl
#' how much bigger do you want the box to be
ExpandBox <- function(box_vec, infl = 1.05){
    box = matrix(box_vec, 2, 2)
    
    center = apply(box, 1, mean)    
    lengths <- box[,2] - box[,1]
    
    expanded_box <- rbind(c(center[1] - (lengths[1]/2) * infl,
                            center[1] + (lengths[1]/2) * infl),
                          c(center[2] - (lengths[2]/2) * infl,
                            center[2] + (lengths[2]/2) * infl))
    
    rownames(expanded_box) = c('x','y')
    colnames(expanded_box) = c('min','max')
    return(expanded_box)
}

#' turns box into sf polygon
#'
#' @param box
#' (x,y) x (min, max)
#' i.e.
#' x0 x1
#' y0 y1
#' 
CreateBoxPoly <- function(box, box_crs = 4269){
    ## spread box to x, y
    x_vec_pre = box[1,]
    y_vec_pre = box[2,]

    x_vec = x_vec_pre[c(1, 1, 2, 2, 1)]
    y_vec = y_vec_pre[c(1, 2, 2, 1, 1)]

    make_poly = st_polygon(list(cbind(x_vec,
                                      y_vec)))

    poly_box = st_sf(id = 1,
                     st_sfc(make_poly),
                     crs = box_crs)
    
    return(poly_box)
}
