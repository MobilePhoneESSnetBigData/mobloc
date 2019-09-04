#' Create Voronoi likelihood
#'
#' @param cp cellplan, validated with \code{\link{validate_cellplan}}
#' @param raster raster object that contains the raster tile index numbers (e.g. created with \code{\link{create_raster}})
#' @param offset offset in meters of the cells in the direction of progagation
#' @export
create_voronoi_llh <- function(cp, raster, offset = 100) {
    if (!is_cellplan_valid(cp)) stop("Cellplan (cp) is not valid yet. Please validate it with validate_cellplan")

    check_raster(raster)

    raster_ext <- extent(raster)[c(1,3,2,4)]
    names(raster_ext) <- c("xmin", "ymin", "xmax", "ymax")
    bbx_poly <- create_bbx_rect(sf::st_bbox(raster_ext, crs = st_crs(cp)))


    cp2 <- move_cells_into_prop_direction(cp, offset)

    v <- st_sf(geometry=st_cast(st_voronoi(st_union(cp2), bbx_poly)))

    rco <- as.data.frame(coordinates(raster))
    dfsf <- st_as_sf(rco, coords = c("x", "y"), crs = st_crs(cp))

    # recover order
    tmp <- st_intersects(cp2, v)
    v <- v[unlist(tmp),]

    res <- st_intersects(v, dfsf)


    dts <- mapply(function(rs, id) {
        data.table(rid = rs, cellid = id)
    }, res, 1L:nrow(res), SIMPLIFY = FALSE)

    dt <- rbindlist(dts)

    dt[, cell:=factor(cp$cell[cellid], levels = cp$cell)][, list(cell, rid, pag = 1)] %>%
        attach_class("mobloc_llh")
}
