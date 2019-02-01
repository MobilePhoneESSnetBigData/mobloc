library(tmap)
library(tmaptools)
library(sf)
library(dplyr)
library(lwgeom)
ZL_bbox <- st_bbox(c(xmin = 172700, ymin = 306800, xmax = 204800, ymax = 342700), crs = st_crs(28992))



tmp <- tempfile(fileext=".zip")
download.file("http://download.cbs.nl/regionale-kaarten/2017-CBSvierkant100m_2017v1.zip", destfile = tmp)
download.file("http://download.cbs.nl/regionale-kaarten/2017-CBSvierkant100m_2016v1.zip", destfile = tmp)


tmpdir <- tempdir()
unzip(tmp, exdir = tmpdir)


shp <- read_sf(file.path(tmpdir, "CBSvierkant100m_2016_v1.shp"))

shp <- read_sf(file.path(tmpdir, "CBSvierkant100m_2017_v1.shp"))
st_geometry(shp) <- NULL

View(head(shp))



download.file("http://geodata.nationaalgeoregister.nl/bestandbodemgebruik2015/extract/bestandbodemgebruik2015.zip", destfile = tmp)


####### ZL_land: wijk_ZL minus water
library(osmdata)
bbL <- bb(ZL_bbox, current.projection = st_crs(28992)$proj4string, projection = "longlat")
q <- opq(bbL)


landuses <- c("commerical", "construction", "industrial", "residential", "retail", "depot", "farmyard", "forest", "garages", "greenhouse_horticulture", "landfill")


lu <- lapply(landuses, function(x) {
    cat(x, "\n")
    q2 <- add_osm_feature(q1, key="landuse", value=x)
    y <- osmdata_sf(q2)

    poly <- y$osm_polygons
    mpoly <- y$osm_multipolygons

    if (is.null(poly) && is.null(mpoly)) {
        NULL
    } else if (is.null(poly)) {
        st_geometry(y$osm_multipolygons)
    } else if (is.null(mpoly)) {
        st_geometry(y$osm_polygons)
    } else {
        c(st_geometry(y$osm_polygons), st_geometry(y$osm_multipolygons))
    }
})
names(lu) <- landuses




q3 <- add_osm_feature(q1, key="landuse", value="construction")
osm3 <- osmdata_sf(q3)
osm <- c(osm3$osm_polygons$geometry, osm2$osm_polygons$geometry, osm2$osm_multipolygons$geometry)
osm <- osm[as.numeric(st_area(osm))>(200^2)]

st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))
zl2 <- st_erase(zl, st_transform(osm, crs= st_crs(zl)))
ZL_land <- zl2

