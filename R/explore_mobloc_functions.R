create_connection_lines <- function(cp1, cp2) {
    c1 <- st_coordinates(cp1)
    c2 <- st_coordinates(cp2)

    st_sf(geometry = do.call(st_sfc, lapply(1:nrow(c1), function(i) {
        co <- rbind(c1[i,],
                    c2[i,])
        st_linestring(co)
    })), cell = cp1$cell, crs = st_crs(cp1))
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
    cp2 <- move_cells_into_prop_direction(cp, offset)
    cp_lines <- create_connection_lines(cp, cp2)

    lf <- leaflet(options = leafletOptions(crs = get_leafletCRS(epsg))) %>%
        addPolylines(data = cp_lines %>% st_transform(crs = 4326), color = "#777777", opacity = 1, weight = 3, group = "Cell locations") %>%
        get_epsg_tiles(epsg)

}

viz_p <- function(cp, rst, var, trans, pnames, offset, rect) {
    cp$sel <- factor(ifelse(cp$sel == 2, "Selected", ifelse(cp$small, "Small cell", "Normal cell")), levels = c("Selected", "Small cell", "Normal cell"))

    cp2 <- move_cells_into_prop_direction(cp, offset)
    cp_lines <- create_connection_lines(cp, cp2)


    pal <- colorFactor(c("red", "gray70", "gold"), levels = c("Selected", "Small cell", "Normal cell"))

    if (all(is.na(rst[]))) var <- "empty"




    cls <- if (var == "dBm")  {
        dBm_classes
    } else {
        qty_classes
    }

    numpal <- ifelse(var %in% c("dBm", "s"), "Blues",
                     ifelse(var == "pga", "viridis",
                            ifelse(var == "pag", "Greens", "Blues")))

    if (var %in% c("dBm", "s")) {
        pal2 <- colorBin(cls$colors, bins = cls$breaks, na.color = "#00000000")#, dBm_classes$labels)
        #rst2 <- raster::projectRaster(rst, crs = st_crs(4326)$proj4string)
        rst2 <- rst
    } else if (var == "bsm") {
        rst2 <- raster::projectRaster(rst, crs = st_crs(3857)$proj4string, method = "ngb")
        lvls <- raster::levels(rst)[[1]]
        cols <- rep(RColorBrewer::brewer.pal(8, "Set2"), length.out = nrow(lvls))

        bsm_sel <- which(cp$sel == "Selected")

        cols[bsm_sel] <- rep(RColorBrewer::brewer.pal(8, "Dark2"), length.out = nrow(lvls))[bsm_sel]

        if (length(na.omit(unique(rst2[])))==1) {
            cols2 <- cols[bsm_sel]
        } else {
            cols2 <- cols
        }
        #pal2 <- colorFactor(palette = cols, domain = lvls$ID, na.color = "transparent")
    } else if (var != "empty") {
        rst2 <- raster::projectRaster(rst, crs = st_crs(4326)$proj4string, method = "bilinear")
        if (any(is.nan(rst2[]))) {
            rst2 <- raster::projectRaster(rst, crs = st_crs(4326)$proj4string, method = "ngb")
        }

        if (var == "pag") {
            allOnes <- (min(rst2[], na.rm = TRUE) > .9)
            if (allOnes) {
                values <- pmin(pmax(rst2[], 0), 1)
            } else {
                values <- pmin(pmax(rst2[] * 1000, 0), 1000)
            }
        } else {
            values <- pmin(pmax(rst2[] * 1000000, 0), 1000000)
        }

        rst2[] <- values
        rng <- range(values, na.rm = TRUE)

        var_as_discrete <- ((rng[2]- rng[1]) < 1e-9)

        if (var_as_discrete) {
            rng_value <- round(rng[1], 8)
            rst2[!is.na(rst2[])] <- rng_value
            cols2 <- if (numpal == c("viridis")) viridis::viridis(7)[4] else RColorBrewer::brewer.pal(7, numpal)[6]
            labels2 <- format(rng_value)
        } else {
            pal2 <- colorNumeric(palette = numpal, rng, reverse = (numpal != "viridis"),
                                 na.color = "transparent")
        }



    }


    title <- switch(var,
                    dBm = "Signal strength in dBm",
                    s = "Signal dominance - s",
                    bsm = "Best server map",
                    #lu = "Land use prior (in %)",
                    pag = paste0("Connection likelihood - P(a|g)", ifelse(allOnes, "", "<br>(in 1 / 1,000)")),
                    pg = "Composite prior - P(g)<br>(in 1/1,000,000)",
                    pga = "Location posterior - P(g|a)<br>(in 1/1,000,000)",
                    paste("Prior", pnames[var], " - P(g)<br>(in 1/1,000,000)"))



    lf <- leafletProxy("map") %>%
        clearMarkers() %>%
        clearImages() %>%
        clearControls() %>%
        clearShapes()


    if (offset > 0) {
        lf <- lf %>%
            addPolylines(data = cp_lines %>% st_transform(crs = 4326), color = "#777777", opacity = 1, weight = 3, group = "Cell locations") %>%
            addCircleMarkers(data = cp2 %>% st_transform(crs = 4326), fillColor = ~pal(sel), color = "black", fillOpacity = 1, radius = 5, weight = 1, group = "Cell locations", layerId = ~cell)
    } else {
        lf <- lf %>%
            addCircleMarkers(data = cp %>% st_transform(crs = 4326), fillColor = ~pal(sel), color = "black", fillOpacity = 1, radius = 5, weight = 1, group = "Cell locations", layerId = ~cell)
    }


    if (var %in% c("dBm", "s")) {
        lf <- lf %>% addRasterImage(x = rst2, opacity = trans, group = title, colors = pal2) %>%
            leaflet::addLayersControl(overlayGroups = c("Cell locations", title), position = "topleft", options = layersControlOptions(collapsed = FALSE)) %>%
            addLegend(colors = cls$colors, labels = cls$labels, opacity = trans, title = title)

    } else if (var == "bsm") {
        lf <- lf %>% addRasterImage(x = rst2, opacity = trans, group = title, colors = cols2) %>%
            leaflet::addLayersControl(overlayGroups = c("Cell locations", title), position = "topleft", options = layersControlOptions(collapsed = FALSE)) %>%
            addLegend(colors = cols, labels = as.character(lvls$cell), opacity = trans, title = title)
    } else if (var == "empty") {
        lf <- lf %>% leaflet::addLayersControl(overlayGroups = c("Cell locations"), position = "topleft")
    } else {


        if (var_as_discrete) {
            lf <- lf %>% addRasterImage(x = rst2, opacity = trans, group = title, colors = cols2) %>%
                leaflet::addLayersControl(overlayGroups = c("Cell locations", title), position = "topleft", options = layersControlOptions(collapsed = FALSE)) %>%
                addLegend(colors = cols2, labels = labels2, opacity = trans, title = title)
        } else {
            lf <- lf %>% addRasterImage(x = rst2, opacity = trans, group = title, colors = pal2) %>%
                leaflet::addLayersControl(overlayGroups = c("Cell locations", title), position = "topleft", options = layersControlOptions(collapsed = FALSE)) %>% addLegend(pal = pal2, values = rng, opacity = trans, title = title)

        }


    }

    lf %>% addPolygons(data = rect, color = "#000000", weight = 1, fill = FALSE)
}
