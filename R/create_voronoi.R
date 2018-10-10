create_voronoi <- function(cp, land, bbox) {


    cp2 <- move_cp_to_direction(cp)

    if (packageVersion("tmaptools") >= "2.0") {
        box <- tmaptools::bb_poly(bbox, projection = st_crs(cp)$proj4string)
    } else {
        box <- st_as_sf(tmaptools::bb_sp(matrix(bbox, ncol=2), projection = st_crs(cp)$proj4string))
    }


    v <- st_sf(geometry=st_cast(st_voronoi(st_union(cp2), box$geometry)))
    vint <- unlist(st_intersects(cp2, v))

    x <- v[vint, ]

    x <- st_intersection(x, box)
    y <- crop_to_land(x, land)

    y$Cell_name <- cp$Cell_name
    y
}

move_cp_to_direction <- function(cp, distance = 100) {
    cp$x2 <- cp$x + ifelse(cp$small | is.na(cp$direction), 0, (SIN(cp$direction) * distance))
    cp$y2 <- cp$y + ifelse(cp$small | is.na(cp$direction), 0, (COS(cp$direction) * distance))

    cp2 <- st_set_geometry(cp, NULL)

    st_as_sf(cp2, coords = c("x2", "y2"), crs = st_crs(cp))
}
