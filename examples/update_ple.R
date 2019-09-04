\dontrun{
# set parameters
ZL_param <- prop_param()

# load data
data("ZL_cellplan", "ZL_muni", "ZL_elevation", "ZL_landuse")

# validate cellplan
ZL_cellplan <- validate_cellplan(ZL_cellplan, param = ZL_param, region = ZL_muni,
    envir = ZL_envir, elevation = ZL_elevation)

# default values of path loss exponent (ple) values have been used for imputation
table(ZL_cellplan$ple)

# create environment layer (needed to calculate path loss exponent (ple))
ZL_envir <- combine_raster_layers(ZL_landuse, weights = c(1, 1, 1, 0, 0))

# inspect this raster object
if (requireNamespace("tmap")) {
    library(tmap)
    tmap_mode("view")
    qtm(ZL_envir)
}

# impute the path loss exponent (ple) values using the ZL_envir object
ZL_cellplan <- update_ple(ZL_cellplan, ZL_envir, ple_0 = 2, ple_1 = 4, ple_small = 6)

# plot the distribution
hist(ZL_cellplan$ple, breaks = 30)
}
