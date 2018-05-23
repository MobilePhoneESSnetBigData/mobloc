rasterize_region <- function(shp, r100, file) {
    r <- p <- NULL
    #shp <- readRDS("shapes/gm.rds")
    #r100 <- create_100m(NL2$bbox)
    r100 <- raster::setValues(r100, 1L:length(r100))

    qres <- quandrantify(shp, r100)

    x <- foreach(p = qres$shps, r = qres$rs, .packages = c("sf", "raster"), .combine = "rbind") %dopar% {
        co <- as.data.frame(sp::coordinates(r))

        co$id <- r[]
        r2 <- st_as_sf(co, coords = c("x", "y"), crs = st_crs(p))
        res <- st_intersects(p, r2, prepared = FALSE)

        res2 <- mapply(function(ri, pid) {
            if (length(ri)==0) {
                data.frame(pid = numeric(), rid = numeric())
            } else {
                data.frame(pid = pid, rid = r2$id[ri])
            }
        }, res, p$id, SIMPLIFY=FALSE)

        res3 <- do.call(rbind, res2)
    }
    v <- rep(NA, length(r100))
    v[x$rid] <- x$pid
    r100b <- setValues(r100, v)

    saveRDS(r100b, file)
    invisible(r100b)
}
