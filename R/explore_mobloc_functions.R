move_cp_to_direction <- function(cp, distance = 100) {
    cp$x2 <- cp$x + ifelse(cp$small | is.na(cp$direction), 0, (SIN(cp$direction) * distance))
    cp$y2 <- cp$y + ifelse(cp$small | is.na(cp$direction), 0, (COS(cp$direction) * distance))

    cp2 <- st_set_geometry(cp, NULL)

    st_as_sf(cp2, coords = c("x2", "y2"), crs = st_crs(cp))
}

create_connection_lines <- function(cp1, cp2) {
    c1 <- st_coordinates(cp1)
    c2 <- st_coordinates(cp2)

    st_sf(geometry = do.call(st_sfc, lapply(1:nrow(c1), function(i) {
        co <- rbind(c1[i,],
                    c2[i,])
        st_linestring(co)
    })), antenna = cp1$antenna, crs = st_crs(cp1))
}

get_leafletCRS <- function(epsg) {
    if (epsg == 3035) {
        leafletCRS(crsClass = "L.Proj.CRS",
                   code='EPSG:3035',
                   proj4def="+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
                   resolutions = 2^(7:0))
    } else {
        leafletCRS(crsClass = "L.CRS.EPSG3857")
    }
}

get_epsg_tiles <- function(map, epsg) {
    if (epsg == 3035) {
        addWMSTiles(map, "https://image.discomap.eea.europa.eu/arcgis/services/GioLandPublic/DEM/MapServer/WmsServer", layers = "Image")
    } else {
        addTiles(map)
    }
}


base_map <- function(cp, offset, epsg) {
    cp2 <- move_cp_to_direction(cp, offset)
    cp_lines <- create_connection_lines(cp, cp2)

    lf <- leaflet(options = leafletOptions(crs = get_leafletCRS(epsg))) %>%
        addPolylines(data = cp_lines %>% st_transform(crs = 4326), color = "#777777", opacity = 1, weight = 3, group = "Antenna locations") %>%
        get_epsg_tiles(epsg)

}

viz_p <- function(cp, rst, var, trans, pnames, offset) {
    cp$sel <- factor(ifelse(cp$sel == 2, "Selected", ifelse(cp$small, "Small cell", "Normal antenna")), levels = c("Selected", "Small cell", "Normal antenna"))

    cp2 <- move_cp_to_direction(cp, offset)
    cp_lines <- create_connection_lines(cp, cp2)


    pal <- colorFactor(c("red", "gray70", "gold"), levels = c("Selected", "Small cell", "Normal antenna"))

    if (all(is.na(rst[]))) var <- "empty"


    title <- switch(var,
                    dBm = "Signal strength in dBm",
                    s = "Signal quality - s",
                    bsm = "Best server map",
                    #lu = "Land use prior (in %)",
                    pag = "Likelihood - P(a|g) (in 1 / 1,000)",
                    pg = "Composite prior - P(g) (in 1/1,000,000)",
                    pga = "Posterior - P(g|a) (in 1/1,000,000)",
                    paste("Prior", pnames[var], "(in 1/1,000,000)"))


    cls <- if (var == "dBm")  {
        dBm_classes
    } else {
        qty_classes
    }

    numpal <- ifelse(var %in% c("dBm", "s"), "Blues",
                     ifelse(var == "pga", "Purples",
                            ifelse(var == "pag", "Greens", "Blues")))

    if (var %in% c("dBm", "s")) {
        pal2 <- colorBin(cls$colors, bins = cls$breaks, na.color = "#00000000")#, dBm_classes$labels)
        #rst2 <- raster::projectRaster(rst, crs = st_crs(4326)$proj4string)
        rst2 <- rst
    } else if (var == "bsm") {
        rst2 <- raster::projectRaster(rst, crs = st_crs(3857)$proj4string, method = "ngb")
        lvls <- raster::levels(rst)[[1]]
        cols <- rep(RColorBrewer::brewer.pal(8, "Dark2"), length.out = nrow(lvls))
        pal2 <- colorFactor(palette = cols, domain = lvls$ID, na.color = "transparent")
    } else if (var != "empty") {
        rst2 <- raster::projectRaster(rst, crs = st_crs(4326)$proj4string)
        if (var == "pag") {
            values <- pmin(pmax(rst2[] * 1000, 0), 1000)
        } else {
            values <- pmin(pmax(rst2[] * 1000000, 0), 1000000)
        }

        rst2[] <- values
        rng <- range(values, na.rm = TRUE)
        pal2 <- colorNumeric(palette = numpal, rng,
                     na.color = "transparent")
    }

    lf <- leafletProxy("map") %>%
        clearMarkers() %>%
        clearImages() %>%
        clearControls() %>%
        clearShapes()


    if (offset > 0) {
        lf <- lf %>%
            addPolylines(data = cp_lines %>% st_transform(crs = 4326), color = "#777777", opacity = 1, weight = 3, group = "Antenna locations") %>%
            addCircleMarkers(data = cp2 %>% st_transform(crs = 4326), fillColor = ~pal(sel), color = "black", fillOpacity = 1, radius = 5, weight = 1, group = "Antenna locations", layerId = ~antenna)
    } else {
        lf <- lf %>%
            addCircleMarkers(data = cp %>% st_transform(crs = 4326), fillColor = ~pal(sel), color = "black", fillOpacity = 1, radius = 5, weight = 1, group = "Antenna locations", layerId = ~antenna)
    }


    if (var %in% c("dBm", "s")) {
        lf %>% addRasterImage(x = rst2, opacity = trans, group = title, colors = pal2) %>%
            leaflet::addLayersControl(overlayGroups = c("Antenna locations", title), position = "topleft", options = layersControlOptions(collapsed = FALSE)) %>%
            addLegend(colors = cls$colors, labels = cls$labels, opacity = trans, title = title)

    } else if (var == "bsm") {
        lf %>% addRasterImage(x = rst2, opacity = trans, group = title, colors = cols) %>%
            leaflet::addLayersControl(overlayGroups = c("Antenna locations", title), position = "topleft", options = layersControlOptions(collapsed = FALSE)) %>%
            addLegend(colors = cols, labels = as.character(lvls$antenna), opacity = trans, title = title)
    } else if (var == "empty") {
        lf %>% leaflet::addLayersControl(overlayGroups = c("Antenna locations"), position = "topleft")
    } else {
        lf %>% addRasterImage(x = rst2, opacity = trans, group = title, colors = pal2) %>%
            leaflet::addLayersControl(overlayGroups = c("Antenna locations", title), position = "topleft", options = layersControlOptions(collapsed = FALSE)) %>%
            addLegend(pal = pal2, values = rng, opacity = trans, title = title)
    }

}
