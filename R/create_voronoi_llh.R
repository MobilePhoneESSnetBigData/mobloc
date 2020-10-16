#' Create Voronoi likelihood
#'
#' @param cp cellplan, validated with \code{\link{validate_cellplan}}
#' @param raster raster object that contains the raster tile index numbers (e.g. created with \code{\link{create_raster}})
#' @param offset offset in meters of the cells in the direction of progagation
#' @export
create_voronoi_llh <- function(cp, raster, offset = 100) {
    cell <- cellid <- rid <- NULL

    if (!is_cellplan_valid(cp)) stop("Cellplan (cp) is not valid yet. Please validate it with validate_cellplan")

    check_raster(raster)

    raster_ext <- extent(raster)[c(1,3,2,4)]
    names(raster_ext) <- c("xmin", "ymin", "xmax", "ymax")
    bbx_poly <- create_bbx_rect(sf::st_bbox(raster_ext, crs = st_crs(cp)))


    cp2 <- move_cells_into_prop_direction(cp, offset)

    id_norm <- which(!cp$small)
    id_small <- which(cp$small)
    c_norm <- cp2[id_norm, ]
    c_small <- cp[id_small,]

    cx = st_coordinates(c_norm)[,1]
    cy = st_coordinates(c_norm)[,2]

    v <- st_sf(geometry=st_cast(st_voronoi(st_union(c_norm), bbx_poly)))

    rco <- as.data.frame(coordinates(raster))
    dfsf <- st_as_sf(rco, coords = c("x", "y"), crs = st_crs(cp))

    tmp <- st_intersects(c_norm, v)
    stopifnot(all(sapply(tmp, length) == 1L))

    v <- v[unlist(tmp),]

    res <- st_intersects(v, dfsf)
    dts <- mapply(function(rs, id) {
        data.table(rid = rs, cellid = id)
    }, res, id_norm, SIMPLIFY = FALSE)

    dt_norm <- rbindlist(dts)

    # small cells
    dist = units::drop_units(st_distance(c_small, dfsf))

    dt_small = data.table(rid = apply(dist, MARGIN = 1, FUN = which.min), cellid = id_small)

    dt = rbindlist(list(dt_norm[!(rid %in% dt_small$rid),],
                        dt_small))

    dt[, cell:=factor(cp$cell[cellid], levels = cp$cell)][, list(cell, rid, pag = 1)] %>%
        attach_class("mobloc_llh")
}
