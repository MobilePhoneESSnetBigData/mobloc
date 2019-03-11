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

    y$antenna <- cp$antenna
    y
}
