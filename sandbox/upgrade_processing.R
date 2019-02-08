library(raster)


# create a parallel cluster
require(parallel)
require(doParallel)
ncores <- detectCores()
cl <- makeCluster(ncores)
registerDoParallel(cl)
stopCluster(cl)





rids <- get_raster_ids(r, land)


cp <- validate_cellplan(ZL_cellplan, param = ZL_param, land = ZL_land, elevation = ZL_elevation)
land <- ZL_land
param <- prop_param()
param <- attach_mapping(param)
ZL_bbox <- st_bbox(c(xmin = 172700, ymin = 306800, xmax = 204800, ymax = 342700), crs = st_crs(28992))
r <- create_raster(ZL_bbox)


cpsel <- cp %>%
    st_set_geometry(NULL) %>%
    select(x, y, z, height, direction, tilt, beam_h, beam_v, W, range, ple)


rext <- extent(r)
rres <- xres(r)




rdf <- get_raster_ids(r, land)
rdf$z <- ZL_elevation[][rdf$rid]


res <- do.call(mapply, c(list(FUN = find_raster_ids, MoreArgs = list(param = param, rext = rext, rres = rres)), as.list(cpsel)))

if (FALSE) {
    r2 <- raster(r)

    r2[][res[[152]]] <- 1

    r2 <- trim(r2)
    qtm(r2) + qtm(cp[152,])
}

res2 <- lapply(res, function(rs) {
    rdf[rdf$rid %in% rs, ]
})

length(res2)
str(cpsel, 1)






