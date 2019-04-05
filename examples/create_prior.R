\dontrun{
    # set parameters
    ZL_param <- prop_param()

    # load data
    data("ZL_cellplan", "ZL_muni", "ZL_elevation", "ZL_landuse")

    # create environment layer (needed to calculate path loss exponent (ple))
    ZL_envir <- combine_raster_layers(ZL_landuse, weights = c(1, 1, 1, 0, 0))

    # validate cellplan
    ZL_cellplan <- validate_cellplan(ZL_cellplan, param = ZL_param, region = ZL_muni, envir = ZL_envir, elevation = ZL_elevation)

    # create raster
    ZL_bbox <- sf::st_bbox(c(xmin = 4012000, ymin = 3077000, xmax = 4048000, ymax = 3117000), crs = sf::st_crs(3035))
    ZL_raster <- create_raster(ZL_bbox)

    # process cellplan (result is propagation model and connection likelihood)
    ZL_prop <- process_cellplan(cp = ZL_cellplan, raster = ZL_raster, elevation = ZL_elevation, param = ZL_param)

    # create priors
    ZL_uniform_prior <- create_uniform_prior(ZL_raster)
    ZL_network_prior <- create_network_prior(ZL_prop, ZL_raster)
    ZL_landuse_prior <- create_prior(ZL_landuse, weights = c(1, 1, .1, 0, .5))
    ZL_comp_prior <- create_prior(ZL_network_prior, ZL_landuse_prior, weights = c(.25, .75))
}
