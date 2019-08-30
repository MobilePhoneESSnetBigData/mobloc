# cp <- ZL_cellplan
# raster <- ZL_raster


create_voronoi_llh <- function(cp, raster, offset = 100) {
    if (!is_cellplan_valid(cp)) stop("Cellplan (cp) is not valid yet. Please validate it with validate_cellplan")

    check_raster(raster)

    raster_ext <- extent(raster)[c(1,3,2,4)]
    names(raster_ext) <- c("xmin", "ymin", "xmax", "ymax")
    bbx_poly <- create_bbx_rect(sf::st_bbox(raster_ext, crs = st_crs(cp)))

    cp$x2 <- cp$x + ifelse(cp$small | is.na(cp$direction), 0, (SIN(cp$direction) * offset))
    cp$y2 <- cp$y + ifelse(cp$small | is.na(cp$direction), 0, (COS(cp$direction) * offset))

    cp2 <- st_set_geometry(cp, NULL)

    cp2 <- st_as_sf(cp2, coords = c("x2", "y2"), crs = st_crs(cp))


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

    dt[, cell:=cp$cell[cellid]][, list(cell, rid, pag = 1)] %>%
        attach_class("mobloc_llh")
}


