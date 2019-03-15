library(raster)
library(sf)

# download from https://land.copernicus.eu/land-files/fdac92901442ed49bf899a997791c5a7faac0b87.zip
r <- raster("eu_dem_v11_E40N30.TIF")

# data("ZL_muni")
# ZL_muni <- st_transform(ZL_muni, crs = r@crs@projargs)
#
# st_bbox(ZL_muni)
#
#
# library(DBI)
#
# con <- dbConnect(RSQLite::SQLite(), "Netherlands.sqlite")
# dbListTables(con)
#
# x <- dbReadTable(con, "idx_nl_1km_GEOMETRY")
# min(x$xmin)
# min(x$ymin)
# max(x$xmax)
# max(x$ymax)
#
# st_bbox()


# p <- tmaptools::bb_poly(ZL_bbox)
# qtm(p) + qtm(ZL_muni)

ZL_bbox <- st_bbox(c(xmin = 4012000, ymin = 3077000, xmax = 4048000, ymax = 3117000), crs = st_crs(3035))

r2 <- raster::crop(r, extent(as.vector(ZL_bbox)[c(1,3,2,4)]))

r3 <- raster::aggregate(r2, fact = 4, fun = mean)


names(r3) <- "elevation"

tmap_arrange(
    qtm(ZL_elevation),
    qtm(r3), sync = TRUE)


ZL_elevation <- r3

save(ZL_elevation, file = "../mobloc/data/ZL_elevation.rda", compress = "xz")
