\dontrun{
# set parameters
ZL_param <- mobloc_param()

# load data
data("ZL_cellplan", "ZL_muni", "ZL_elevation", "ZL_landuse")

# create environment layer (needed to calculate path loss exponent (ple))
ZL_envir <- combine_raster_layers(ZL_landuse, weights = c(1, 1, 1, 0, 0))

# validate cellplan
ZL_cellplan <- validate_cellplan(ZL_cellplan, param = ZL_param, region = ZL_muni,
                                 envir = ZL_envir, elevation = ZL_elevation)

# create raster
ZL_bbox <- sf::st_bbox(c(xmin = 4012000, ymin = 3077000, xmax = 4048000, ymax = 3117000),
                       crs = sf::st_crs(3035))
ZL_raster <- create_raster(ZL_bbox)

# compute the signal strength model
ZL_strength <- compute_sig_strength(cp = ZL_cellplan, raster = ZL_raster,
                                    elevation = ZL_elevation, param = ZL_param)

# create likelihoods
ZL_strength_llh <- create_strength_llh(ZL_strength, param = ZL_param)
ZL_voronoi_llh <- create_voronoi_llh(ZL_cellplan, ZL_raster)

bsm_strength <- create_best_server_map(ZL_strength_llh, raster = ZL_raster)
bsm_voronoi <- create_best_server_map(ZL_voronoi_llh, raster = ZL_raster)

bsms <- brick(bsm_strength, bsm_voronoi)
names(bsms) <- c("strength", "voronoi")

if (require(tmap)) {
    tmap_mode("view")
    tm_shape(bsms) +
        tm_raster(stretch.palette = FALSE) +
    qtm(move_cells_into_prop_direction(ZL_cellplan, 100))
}
}
