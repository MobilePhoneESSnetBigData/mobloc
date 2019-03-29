# create raster
ZL_bbox <- sf::st_bbox(c(xmin = 4012000, ymin = 3077000, xmax = 4048000, ymax = 3117000), crs = st_crs(3035))
ZL_raster <- create_raster(ZL_bbox)
