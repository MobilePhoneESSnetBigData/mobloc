# load data
data("ZL_landuse")

# create environment layer
ZL_envir <- combine_raster_layers(ZL_landuse, weights = c(1, 1, 1, 0, 0))

\dontrun{
if (requireNamespace("tmap")) {
    tmap_mode("view")
    qtm(ZL_envir)
}
}
