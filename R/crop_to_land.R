crop_to_land <- function(shp, land) {
    suppressWarnings({
        if (!(length(land) == 1)) land <- st_union(st_combine(land))
        st_intersection(shp, land)
    })
}
