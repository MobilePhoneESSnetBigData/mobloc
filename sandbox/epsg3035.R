library(sf)
library(leaflet)

data("ZL_muni")


crs3035 <- leafletCRS(crsClass = "L.Proj.CRS",
           code='EPSG:3035',
           proj4def="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
           resolutions = 2^(7:0))


leaflet(options = leafletOptions(crs = crs3035)) %>%
    addPolygons(data = st_transform(ZL_muni, 4326)) %>%
    addWMSTiles("https://image.discomap.eea.europa.eu/arcgis/services/GioLandPublic/DEM/MapServer/WmsServer", layers = "Image")



leaflet(options = leafletOptions(crs = crs3035)) %>%
    addPolygons(data = st_transform(ZL_muni, 4326)) %>%
    addRasterImage(raster::projectRaster(ZL_elevation, crs = raster::crs(st_crs(3035)$proj4string))) %>%
    addWMSTiles("https://image.discomap.eea.europa.eu/arcgis/services/GioLandPublic/DEM/MapServer/WmsServer", layers = "Image")


leaflet(options = leafletOptions(crs = crs3035)) %>%
    addPolygons(data = st_transform(ZL_muni, 4326)) %>%
    addRasterImage(ZL_elevation, project = F)
    #addWMSTiles("https://image.discomap.eea.europa.eu/arcgis/services/GioLandPublic/DEM/MapServer/WmsServer", layers = "Image")


leaflet() %>%
    addPolygons(data = st_transform(ZL_muni, 4326)) %>%
    addTiles() %>%
    addRasterImage(ZL_elevation, project = TRUE) %>%
    addWMSTiles("https://image.discomap.eea.europa.eu/arcgis/services/GioLandPublic/DEM/MapServer/WmsServer", layers = "Image")


library(mapview)

rst <- poppendorf[[5]]

leaflet(options = leafletOptions(minZoom = -100,
                                 crs = leafletCRS(crsClass =
                                                      "L.CRS.Simple"))) %>%
    addRasterImage(rst, project = FALSE) %>%
    addMouseCoordinates()
