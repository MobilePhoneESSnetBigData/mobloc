#' Create cellplan polygons
#'
#' Create cellplan polygons
#' @param cp cellplan
#' @param land land
#' @param bbox bounding box
#' @param param parameters
#' @import dplyr
#' @import sf
#' @export
#' @return cellplan polygons
create_cellplan_polygons <- function(cp, land, bbox, param) {
    crs <- st_crs(cp)
    if (param$poly_shape == "Voronoi") {
        cp_poly <- create_voronoi(cp, land, bbox)
        #cp_poly <- crop_to_land(cp_poly, land)
    } else {

        vor <- create_voronoi(cp, land, bbox)

        vor_area <- as.numeric(st_area(vor))
        rng <- ifelse(cp$small, param$max_range_small, param$max_range)

        cp$rng <- pmin(sqrt(vor_area/pi) * param$area_expension, rng)
        st_geometry(cp) <- NULL

        cp <- cp %>% dplyr::select(Cell_name, x, y, rng, direction, beam_h, small)

        suppressWarnings(start_cluster())

        m <- do.call(mapply, c(list(FUN=create_poly, SIMPLIFY = FALSE, MoreArgs = list(poly_shape = param$poly_shape)), cp))
        cp_poly <- st_sf(geometry = do.call(st_sfc, c(m, list(crs = crs))))
#browser()
        cp_poly <- crop_to_land(cp_poly, land)
    }


    # if (plot && "dir" %in% names(prj)) {
    #     dir.create(prj$dir, recursive = TRUE, showWarnings = FALSE)
    #     ############## plot cellplan
    #     OSM <- read_osm(cp_poly, zoom = 10)
    #
    #     sp_rect <- bb_sp(matrix(prj$bbox, ncol=2), projection = st_crs(prj$crs)$proj4string)
    #
    #     tm <- qtm(OSM) +
    #         qtm(cp_poly) +
    #         qtm(sp_rect, fill=NULL)
    #
    #     save_tmap(tm, filename = file.path(prj$dir, "cellplan.png"), height = OSM@grid@cells.dim[2], width = OSM@grid@cells.dim[1], outer.margins = 0)
    # }

    # if ("dir" %in% names(prj)) {
    #     dir.create(prj$dir, recursive = TRUE, showWarnings = FALSE)
    #     saveRDS(cp_poly, file = file.path(prj$dir, "cp_poly.rds"))
    #     invisible()
    # } else {
    #     cp_poly
    # }
    cp_poly
}

create_poly <- function(Cell_name = NULL, x, y, rng, direction, beam_h, small = FALSE, poly_shape = "pie", line_points_per_circle = 360) {

    directionL <- direction - beam_h
    directionR <- direction + beam_h

    if (small) {
        a <- seq(0, 360, length.out=line_points_per_circle)
        a[line_points_per_circle] <- 0 # to make sure the polygon is closed
        st_polygon(list(matrix(c(x + SIN(a) * rng,
                                 y + COS(a) * rng), ncol=2)))
    } else if (poly_shape == "pie") {
        a <- seq(directionL, directionR, length.out =  (directionR-directionL) / 360 * line_points_per_circle)
        st_polygon(list(matrix(c(x, x + SIN(a) * rng, x,
                                 y, y + COS(a) * rng, y), ncol=2)))
    } else {
        plot.new()
        # plectrum
        xs <- c(x, x + SIN(directionL) * rng, x + SIN((directionR+directionL)/2) * rng, x + SIN(directionR) * rng)
        ys <- c(y, y + COS(directionL) * rng, y + COS((directionR+directionL)/2) * rng, y + COS(directionR) * rng)

        coords <- xspline(x=xs, y=ys, shape = c(-1, -1, -1), draw = FALSE, open = FALSE)
        coords <- do.call(cbind, coords)

        coords <- rbind(coords, coords[1,])

        st_polygon(list(coords))
    }
}
