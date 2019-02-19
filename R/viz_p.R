base_map <- function(cp, offset) {
    cp2 <- move_cp_to_direction(cp, offset)
    cp_lines <- create_connection_lines(cp, cp2)

    leaflet() %>% addPolylines(data = cp_lines %>% st_transform(crs = 4326), color = "#777777", opacity = 1, weight = 3, group = "Antenna locations") %>%
        addTiles()
}

viz_p <- function(cp, rst, var, trans, pnames, offset) {
    cp$sel <- factor(ifelse(cp$sel == 2, "Selected", ifelse(cp$small, "Small cell", "Normal antenna")), levels = c("Selected", "Small cell", "Normal antenna"))

    cp2 <- move_cp_to_direction(cp, offset)
    cp_lines <- create_connection_lines(cp, cp2)


    pal <- colorFactor(c("red", "gray70", "gold"), levels = c("Selected", "Small cell", "Normal antenna"))

    if (all(is.na(rst[]))) var <- "empty"


    title <- switch(var,
                    dBm = "Signal strength in dBm",
                    s = "Signal quality - s (in %)",
                    bsm = "Best server map",
                    lu = "Land use prior (in %)",
                    pag = "Likelihood - P(a|g) (in %)",
                    pg = "Composite prior - P(g) (in %)",
                    pga = "Probability - P(g|a) (in %)",
                    paste("Prior", pnames[var], "(in %)"))


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
        rst2 <- raster::projectRaster(rst, crs = st_crs(4326)$proj4string)
    } else if (var == "bsm") {
        rst2 <- raster::projectRaster(rst, crs = st_crs(3857)$proj4string, method = "ngb")
        lvls <- raster::levels(rst)[[1]]
        cols <- rep(RColorBrewer::brewer.pal(8, "Dark2"), length.out = nrow(lvls))
        pal2 <- colorFactor(palette = cols, domain = lvls$ID, na.color = "transparent")
    } else if (var != "empty") {
        rst2 <- raster::projectRaster(rst, crs = st_crs(4326)$proj4string)
        values <- pmin(pmax(rst2[] * 100, 0), 100)
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
