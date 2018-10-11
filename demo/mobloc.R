### Demo mobloc
### This demo is almost similar to the R code in the vignette
library(mobloc)
library(tmap)
library(sf)
library(dplyr)

param <- location_model_parameters()
str(param)

cell_modelling_tool(param)

radiation_plot(beam_width = 65, db_back = -30)
radiation_plot(type = "e", db_back = -30, beam_width = 9)

data("ZL_cellplan", "ZL_land", "ZL_elevation", "ZL_prior")

head(ZL_cellplan)


ZL_cellplan <- check_cellplan(ZL_cellplan, param)





tmap_mode("view")
qtm(ZL_elevation) + qtm(ZL_land, fill=NULL) + qtm(ZL_cellplan)

#ZL_bbox <- st_bbox(c(xmin = 172700, ymin = 306800, xmax = 204800, ymax = 342700), crs = st_crs(28992))

if (packageVersion("tmaptools") >= "2.0") {
    ZL_bbox <- tmaptools::bb(ZL_elevation)
} else {
    ZL_bbox <- structure(as.vector(tmaptools::bb(ZL_elevation)), class = "bbox", crs = st_crs(ZL_land))
}

res <- create_cellplan_polygons(ZL_cellplan, ZL_land, ZL_bbox, param = param)

ZL_poly <- res$poly
ZL_voronoi <- res$vor

qtm(ZL_voronoi) + qtm(ZL_cellplan)
qtm(ZL_poly) + qtm(ZL_cellplan)

ZL_raster <- create_raster(ZL_elevation)
ZL_prob <- rasterize_cellplan(cp = ZL_cellplan, cp_poly = ZL_poly, raster = ZL_raster, elevation = ZL_elevation, param = param)

cell_inspection_tool(ZL_cellplan, ZL_poly, ZL_raster, ZL_prob, param_current, ZL_prior)

