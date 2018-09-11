#' Create cellplan polygons
#'
#' Create cellplan polygons. The type of polygons that are created are specified by \code{param$poly_shape}. One option is \code{"Voronoi"} which creates a Voronoi tesselation. Otherwise, a circle is created for omnidirectional antennas, and an \code{"oval"} or \code{"pie"} for directional antennas. The range and horizontal beam width are taken into account. The range is the mimimum of \code{param$range} and \code{param$area_expension} times the square root of a/$pi$ where a is the area size of the corresponding Voronoi polygon.
#' @param cp cellplan
#' @param land land
#' @param bbox bounding box
#' @param param parameters
#' @import dplyr
#' @import sf
#' @export
#' @return cellplan polygons
create_cellplan_polygons <- function(cp, land, bbox, param) {
    Cell_name <- x <- y <- direction <- beam_h <- small <- NULL

    crs <- st_crs(cp)
    if (param$poly_shape == "Voronoi") {
        cp_poly <- create_voronoi(cp, land, bbox)
        #cp_poly <- crop_to_land(cp_poly, land)
    } else {

        vor <- create_voronoi(cp, land, bbox)

        vor_area <- as.numeric(st_area(vor))
        #rng <- ifelse(cp$small, param$max_range_small, param$max_range)

        cp$rng <- pmin(sqrt(vor_area/pi) * param$area_expension, cp$range)
        st_geometry(cp) <- NULL

        cp <- cp %>% dplyr::select(Cell_name, x, y, rng, direction, beam_h)

        #suppressWarnings(start_cluster())

        m <- do.call(mapply, c(list(FUN=create_poly, SIMPLIFY = FALSE, MoreArgs = list(poly_shape = param$poly_shape)), cp))
        cp_poly <- st_sf(geometry = do.call(st_sfc, c(m, list(crs = crs))))
#browser()
        cp_poly <- crop_to_land(cp_poly, land)
    }



    cp_poly
}

create_poly <- function(Cell_name = NULL, x, y, rng, direction, beam_h, poly_shape = "pie", line_points_per_circle = 360) {

    directionL <- direction - beam_h
    directionR <- direction + beam_h

    if (is.na(direction)) { # circle
        a <- seq(0, 360, length.out=line_points_per_circle)
        a[line_points_per_circle] <- 0 # to make sure the polygon is closed
        st_polygon(list(matrix(c(x + SIN(a) * rng,
                                 y + COS(a) * rng), ncol=2)))
    } else if (poly_shape == "pie") {
        a <- seq(directionL, directionR, length.out =  (directionR-directionL) / 360 * line_points_per_circle)
        st_polygon(list(matrix(c(x, x + SIN(a) * rng, x,
                                 y, y + COS(a) * rng, y), ncol=2)))
    } else { # oval
        plot.new()

        xs <- c(x, x + SIN(directionL) * rng, x + SIN((directionR+directionL)/2) * rng, x + SIN(directionR) * rng)
        ys <- c(y, y + COS(directionL) * rng, y + COS((directionR+directionL)/2) * rng, y + COS(directionR) * rng)

        coords <- xspline(x=xs, y=ys, shape = c(-1, -1, -1), draw = FALSE, open = FALSE)
        coords <- do.call(cbind, coords)

        coords <- rbind(coords, coords[1,])

        st_polygon(list(coords))
    }
}
