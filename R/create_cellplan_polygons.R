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
        # extract the ranges from the Voronoi tesselation, but lowerbound it by max_range_small
        vor <- create_voronoi(cp, land, bbox)

        if (nrow(vor) != nrow(cp)) warning("Number of voronoi polygons is not equal to the number of antennas. Please check the cellplan.", call. = FALSE)

        vor_area <- as.numeric(st_area(vor))
        vor_range <- sqrt(vor_area/pi) * param$area_expension

        cp$rng <- ifelse(cp$small,
                         pmax(pmin(vor_range, param$max_range_small), param$min_range_small),
                         pmax(pmin(vor_range, param$max_range), param$min_range))

        cp <- cp %>% dplyr::select(Cell_name, x, y, rng, direction, beam_h)

        res <- create_shape(cp, type = param$poly_shape)

        cp$cls <- res$cls
        shapes <- c(list(create_circle()), res$shapes)


        shp <- st_sf(do.call(st_sfc, c(shapes, list(crs = st_crs(land)))))
        shp$id <- c("circle", paste("beam_h =", res$beams))

        png(file.path(TPDIR, "polygons_basic_shapes.png"), width = 1000, height = 800)
        tmod <- tmap_mode("plot")
        print(tm_shape(shp) +
            tm_polygons() +
            tm_facets(by = "id", free.coords = FALSE, drop.units = TRUE) +
            tm_layout(scale = 2) +
            tm_grid())
        dev.off()
        tmap_mode(tmod)

        rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

        clust <- current_cluster(verbose = FALSE)

        if (clust == 0) {
            warning("No cluster defined. Please define a cluster to run faster")
            m <- mapply(function(cl, x, y, rng, direction) {
                if (is.na(direction)) direction <- 0
                shapes[[cl+1L]] * rot(direction / 180 * pi) * rng + c(x, y)
            }, cp$cls, cp$x, cp$y, cp$rng, cp$direction, SIMPLIFY = FALSE)
        } else {
            m <- mcmapply(function(cl, x, y, rng, direction) {
                if (is.na(direction)) direction <- 0
                shapes[[cl+1L]] * rot(direction / 180 * pi) * rng + c(x, y)
            }, cp$cls, cp$x, cp$y, cp$rng, cp$direction, SIMPLIFY = FALSE)
        }

        #m <- do.call(mapply, c(list(FUN=create_poly, SIMPLIFY = FALSE, MoreArgs = list(poly_shape = param$poly_shape, ovals = ovals)), cp))
        cp_poly <- st_sf(geometry = do.call(st_sfc, c(m, list(crs = crs))))
#browser()
        cp_poly <- crop_to_land(cp_poly, land)
    }

    if (nrow(cp) != nrow(cp_poly)) warning("Number of polygons is not equal to the number of antennas. Please check the cellplan.", call. = FALSE)

    list(poly = cp_poly, vor = vor)
}

create_shape <- function(cp, type = c("oval", "pie"), line_points_per_circle = 360) {
    cp_ovals <- cp %>% filter(!is.na(direction))
    ids <- which(!is.na(cp$direction))

    beams <- sort(unique(cp_ovals$beam_h))

    if (length(beams) > 10) {
        km <- kmeans(cp_ovals$beam_h, centers = 2)
        cls <- km$cluster
        beams <- km$centers
    } else {
        cls <- match(cp_ovals$beam_h, beams)
    }

    shapes <- lapply(beams, function(b) {
        directionL <- -b
        directionR <- b

        x <- 0
        y <- 0
        rng <- 1

        if (type == "oval") {
            plot.new()

            xs <- c(x, x + SIN(directionL) * rng * .75, x + SIN(0) * rng, x + SIN(directionR) * rng * .75)
            ys <- c(y, y + COS(directionL) * rng * .75, y + COS(0) * rng, y + COS(directionR) * rng * .75)

            coords <- xspline(x=xs, y=ys, shape = c(-1, -1, -1), draw = FALSE, open = FALSE)
            coords <- do.call(cbind, coords)

            coords <- rbind(coords, coords[1,])

            st_polygon(list(coords))
        } else { # pie
            a <- seq(directionL, directionR, length.out =  (directionR-directionL) / 360 * line_points_per_circle)
            st_polygon(list(matrix(c(x, x + SIN(a) * rng, x,
                                     y, y + COS(a) * rng, y), ncol=2)))
        }
    })

    cls2 <- rep(0L, nrow(cp))
    cls2[ids] <- cls

    return(list(shapes = shapes, cls = cls2, beams = beams))
}

create_circle <- function(line_points_per_circle = 360) {
    a <- seq(0, 360, length.out=line_points_per_circle)
    a[line_points_per_circle] <- 0 # to make sure the polygon is closed
    st_polygon(list(matrix(c(SIN(a),
                             COS(a)), ncol=2)))
}
