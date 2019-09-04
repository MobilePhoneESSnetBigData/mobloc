#' Move cells into propagation direction
#'
#' @param cp cellplan, validated with \code{\link{validate_cellplan}}
#' @param offset offset in meters
#' @export
move_cells_into_prop_direction <- function(cp, offset = 100) {
    cp$x2 <- cp$x + ifelse(cp$small | is.na(cp$direction), 0, (SIN(cp$direction) * offset))
    cp$y2 <- cp$y + ifelse(cp$small | is.na(cp$direction), 0, (COS(cp$direction) * offset))

    cp2 <- st_set_geometry(cp, NULL)

    st_as_sf(cp2, coords = c("x2", "y2"), crs = st_crs(cp))
}
