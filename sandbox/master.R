

## cluster management: object .cl is used
current_cluster()
start_cluster(3)

param_default <- location_model_parameters()

setup_location_model(param_default)

radiation_plot(beam_width = 65, db_back = -30)
radiation_plot(type = "e", db_back = -30, beam_width = 9)

# ZL <- list(name = "ZL",
#            description = "Zuid-Limburg",
#            bbox = structure(c(xmin = 172700, ymin = 306800, xmax = 204800, ymax = 342700),
#                             crs = st_crs(28992), class = "bbox"),
#            crs = 28992,
#            param = param_default,
#            dir = "output/ZL")

library(sf)
ZL_bbox <- st_bbox(c(xmin = 172700, ymin = 306800, xmax = 204800, ymax = 342700), crs = st_crs(28992))
ZL_raster <- create_raster(ZL_bbox)


data("ZL_land", "ZL_elevation", "ZL_cellplan")

ZL_poly <- create_cellplan_polygons(ZL_cellplan, ZL_land, ZL_bbox, param = param_default)

param_default2 <- update_model_parameters(param_default, poly_shape = "Voronoi")
ZL_voronoi <- create_cellplan_polygons(ZL_cellplan, ZL_land, ZL_bbox, param = param_default2)

ZL_prob <- rasterize_cellplan(cp = ZL_cellplan, cp_poly = ZL_poly, raster = ZL_raster, elevation = ZL_elevation, param = param_default)

shiny_cells(ZL_cellplan, ZL_poly, ZL_raster, ZL_prob, param_default)

