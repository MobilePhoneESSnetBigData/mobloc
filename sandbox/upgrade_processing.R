library(raster)


# create a parallel cluster
require(parallel)
require(doParallel)
ncores <- detectCores()
cl <- makeCluster(ncores)
registerDoParallel(cl)
stopCluster(cl)






ZL_param <- prop_param()
cp <- validate_cellplan(ZL_cellplan, param = ZL_param, land = ZL_land, elevation = ZL_elevation)
land <- ZL_land
param <- prop_param()
param <- attach_mapping(param)
ZL_bbox <- st_bbox(c(xmin = 172700, ymin = 306800, xmax = 204800, ymax = 342700), crs = st_crs(28992))
r <- create_raster(ZL_bbox)


cpsel <- cp %>%
    st_set_geometry(NULL) %>%
    dplyr::select(x, y, z, height, direction, tilt, beam_h, beam_v, W, range, ple)


rext <- extent(r)
rres <- xres(r)




rdf <- get_raster_ids(r, land)
rdf$z <- ZL_elevation[][rdf$rid]


cpsellist <- as.list(cpsel)
names(cpsellist$x) <- cp$antenna


res <- do.call(mapply, c(list(FUN = find_raster_ids, MoreArgs = list(param = param, rext = rext, rres = rres), USE.NAMES = TRUE), cpsellist))


if (FALSE) {
    r2 <- raster(r)

    r2[][res[[52]]] <- 1

    r2 <- trim(r2)
    qtm(r2) + qtm(cp[52,])
}

res2 <- lapply(res, function(rs) {
    rdf[rdf$rid %in% rs, ]
})

if (FALSE) {
    r2 <- raster(r)

    r2[][res2[[132]]$rid] <- 1

    r2 <- trim(r2)
    qtm(r2) + qtm(cp[132,])
}

length(res2)
str(cpsel, 1)

df3 <- do.call(mcmapply, c(list(FUN = function(df, x, y, z, height, direction, tilt, beam_h, beam_v, W, range, ple, param) {
    df2 <- signal_strength(cx=x, cy=y, cz=z,
                    direction = direction,
                    tilt = tilt,
                    beam_h = beam_h,
                    beam_v = beam_v,
                    W = W,
                    co = df[, c("x", "y", "z")],
                    ple = ple,
                    param = param)
    cbind(df, as.data.frame(df2))
}, df = res2, MoreArgs = list(param = param), SIMPLIFY = FALSE, USE.NAMES = TRUE), as.list(cpsel)))

antennas <- cp$antenna
df4 <- do.call(rbind, mapply(function(d,nm) {
    d$antenna <- factor(nm, levels = antennas)
    d
}, df3, names(df3), SIMPLIFY = FALSE, USE.NAMES = FALSE))





str(df3[[1]])



df5 <- df4 %>%
    group_by(rid) %>%
    filter(order(s)<=param$max_overlapping_cells) %>%
    mutate(pag = s / sum(s)) %>%
    ungroup()

# df <- df %>%
#     group_by(pid) %>%
#     mutate(pr = pr / sum(pr),
#            s = p / sum(p)) %>%
#     ungroup()

df5 %>% dplyr::select(antenna=antenna, rid=rid, dist=dist, dBm = dBm, s = s, pag = pag)
