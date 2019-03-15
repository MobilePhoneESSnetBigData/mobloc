#library(tmap)
devtools::load_all("../tmap")
library(tmaptools)
library(sf)
library(raster)
library(dplyr)
library(lwgeom)

ZL_bbox <- st_bbox(c(xmin = 4012000, ymin = 3077000, xmax = 4048000, ymax = 3117000), crs = st_crs(3035))
#ZL_bbox <- st_bbox(c(xmin = 172700, ymin = 306800, xmax = 204800, ymax = 342700), crs = st_crs(28992))

library(osmdata)
bbL <- bb(ZL_bbox, current.projection = st_crs(3035)$proj4string, projection = "longlat")
q <- opq(bbL)

polykeys <- c(rep("landuse", 14), "natural", "waterway")
polyvalues <- c("commercial", "construction", "industrial", "residential", "retail", "depot", "farmyard", "forest", "garages", "greenhouse_horticulture", "landfill", "orchard", "plant_nursery", "vineyard", "water", "canal")
polycat <- c("building", "building", "building", "residential", "building", "building", "building", "forest", "building", "building", "building", "forest", "forest", "forest", "water", "water")
polys <- mapply(function(k, v) {
    cat(k, v, "\n")
    q2 <- add_osm_feature(q, key=k, value=v)
    for (i in 1:5) {
        y <- tryCatch({
            osmdata_sf(q2)
        }, error = function(e) {
            NULL
        })
        if (!is.null(y)) break
    }
    if (is.null(y)) return(NULL)

    poly <- y$osm_polygons
    mpoly <- y$osm_multipolygons

    if (is.null(poly) && is.null(mpoly)) {
        NULL
    } else {
        y <- if (is.null(poly)) {
            st_geometry(y$osm_multipolygons)
        } else if (is.null(mpoly)) {
            st_geometry(y$osm_polygons)
        } else {
            c(st_geometry(y$osm_polygons), st_geometry(y$osm_multipolygons))
        }
        z <- lwgeom::st_make_valid(st_cast(y, "MULTIPOLYGON"))
        names(z) <- NULL

        if (any(st_geometry_type(z) == "GEOMETRYCOLLECTION")) z <- st_collection_extract(z, "POLYGON")
        z
    }
}, polykeys, polyvalues, SIMPLIFY = FALSE)
names(polys) <- polyvalues
isnull <- sapply(polys, is.null)

polycat <- polycat[!isnull]
polyvalues <- polyvalues[!isnull]
polykeys <- polykeys[!isnull]

polys <- polys[!isnull]


plevels <- unique(polycat)

shp_landuse <- do.call(rbind, lapply(plevels, function(ct) {
    cat(ct, "\n")
    geom <- st_union(lwgeom::st_make_valid(do.call(c, polys[polycat == ct])))
    st_sf(geometry = geom, category = factor(ct, levels = plevels))
}))
shp_landuse <- st_transform(shp_landuse, 3035)

devtools::load_all()
ZL_raster <- create_raster(ZL_bbox)
rs <- lapply(plevels, function(ct) {
    cat(ct, "\n")
    shp <- st_cast(shp_landuse[shp_landuse$category == ct, ], "POLYGON")
    rasterize(shp, ZL_raster, getCover = TRUE)
})
rb <- do.call(brick, rs)


## ROADS
keys <- c("highway", "highway", "highway", "highway", "highway", "highway", "highway", "highway") #railway
values <- c("motorway", "trunk", "primary", "secondary", "motorway_link", "trunk_link", "primary_link", "secondary_link") #rail
widths <- c(30, 15, 15, 15, 15, 15, 15, 15)
wy <- mapply(function(k, v) {
    cat(k, v, "\n")
    q2 <- add_osm_feature(q, key=k, value=v)
    for (i in 1:5) {
        y <- tryCatch({
            osmdata_sf(q2)
        }, error = function(e) {
            NULL
        })
        if (!is.null(y)) break
    }
    if (is.null(y)) return(NULL)
    line <- y$osm_lines
    mline <- y$osm_multilines

    if (is.null(line) && is.null(mline)) {
        NULL
    } else {
        y <- if (is.null(line)) {
            st_geometry(y$osm_multilines)
        } else if (is.null(mline)) {
            st_geometry(y$osm_lines)
        } else {
            c(st_geometry(y$osm_lines), st_geometry(y$osm_multilines))
        }
        z <- lwgeom::st_make_valid(st_cast(y, "MULTILINESTRING"))
        names(z) <- NULL

        if (any(st_geometry_type(z) == "GEOMETRYCOLLECTION")) z <- st_collection_extract(z, "LINESTRING")
        z
    }
}, keys, values, SIMPLIFY = FALSE)
wy <- lapply(wy, st_transform, 3035)
wyb <- mapply(function(w, wd) {
    st_union(st_buffer(w, dist = wd))
}, wy, widths, SIMPLIFY = FALSE)
wybt <- st_cast(do.call(c, wyb), "MULTIPOLYGON") %>%
    st_union() %>%
    st_cast("POLYGON")
wr <- rasterize(st_sf(geometry = wybt), ZL_raster, getCover = TRUE)
# qtm(wr) + qtm(wybt)

lyrs <- c(rs, list(wr))
ZL_landuse <- do.call(brick, lyrs)
names(ZL_landuse) <- c("building", "residential", "forest", "water", "roads")

# forest is at most 1 - water
ZL_landuse$forest <-   min(1 - ZL_landuse$water, ZL_landuse$forest)

# residential areas is at most (1 - building - forest - water)
ZL_landuse$residential <-   max(min(1 - ZL_landuse$building - ZL_landuse$forest - ZL_landuse$water, ZL_landuse$residential), 0)

# building is at most (1 - forest - water)
ZL_landuse$building <-   min(1 - ZL_landuse$forest - ZL_landuse$water, ZL_landuse$building)

# make place for roads
plus <- max(sum(ZL_landuse) - 1, 0)
sums <- sum(ZL_landuse$building, ZL_landuse$forest, ZL_landuse$water, ZL_landuse$residential)

ZL_landuse$building <- max(ZL_landuse$building - (ZL_landuse$building / sums) * plus, 0)
ZL_landuse$residential <- max(ZL_landuse$residential - (ZL_landuse$residential / sums) * plus, 0)
ZL_landuse$forest <- max(ZL_landuse$forest - (ZL_landuse$forest / sums) * plus, 0)
ZL_landuse$water <- max(ZL_landuse$water - (ZL_landuse$water / sums) * plus, 0)
ZL_landuse[is.na(ZL_landuse)] <- 0

save(ZL_landuse, file = "data/ZL_landuse.rda", compress = "xz")
