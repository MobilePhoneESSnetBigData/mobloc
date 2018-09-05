### Demo mobloc
### This demo is almost similar to the R code in the vignette
library(mobloc)
library(tmap)
library(sf)
library(dplyr)

param_default <- location_model_parameters()
str(param_default)

param_current <- cell_modelling_tool(param_default)

radiation_plot(beam_width = 65, db_back = -30)
radiation_plot(type = "e", db_back = -30, beam_width = 9)

data("ZL_cellplan", "ZL_land", "ZL_elevation")

head(ZL_cellplan)


ZL_cellplan2 <- check_cellplan(ZL_cellplan, param_current)










tmap_mode("view")
qtm(ZL_elevation) + qtm(ZL_land, fill=NULL) + qtm(ZL_cellplan)

ZL_bbox <- st_bbox(c(xmin = 172700, ymin = 306800, xmax = 204800, ymax = 342700), crs = st_crs(28992))

param_voronoi <- update_model_parameters(param_current, poly_shape = "Voronoi")
ZL_voronoi <- create_cellplan_polygons(ZL_cellplan, ZL_land, ZL_bbox, param = param_voronoi)

# workaround for bug in CRAN 2.0 (fixed in github/mtennekes/tmap)
if (packageVersion("tmap") == "2.0") {
    ZL_voronoi$x <- 0 # dummy variable
    tm_shape(ZL_voronoi, point.per = "feature") + tm_polygons() + qtm(ZL_cellplan)
} else {
    qtm(ZL_voronoi) + qtm(ZL_cellplan)
}

ZL_poly <- create_cellplan_polygons(ZL_cellplan, ZL_land, ZL_bbox, param = param_current)

# workaround for bug in CRAN 2.0 (fixed in github/mtennekes/tmap)
if (packageVersion("tmap") == "2.0") {
    ZL_poly$x <- 0 # dummy variable
    tm_shape(ZL_poly, point.per = "feature") + tm_polygons() + qtm(ZL_cellplan)
} else {
    qtm(ZL_poly) + qtm(ZL_cellplan)
}


ZL_raster <- create_raster(ZL_bbox)
ZL_prob <- rasterize_cellplan(cp = ZL_cellplan, cp_poly = ZL_poly, raster = ZL_raster, elevation = ZL_elevation, param = param_current)

cell_inspection_tool(ZL_cellplan, ZL_poly, ZL_raster, ZL_prob, param_current)
