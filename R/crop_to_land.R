crop_to_land <- function(shp, land) {
    suppressWarnings(st_intersection(shp, st_union(st_combine(land))))
}
