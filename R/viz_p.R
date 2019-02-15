base_map <- function(cp) {
    cp2 <- move_cp_to_direction(cp, 200)
    cp_lines <- create_connection_lines(cp, cp2)

    leaflet() %>% addPolylines(data = cp_lines %>% st_transform(crs = 4326), color = "grey60", opacity = 1, weight = 1, group = "Antenna locations") %>%
        addTiles()
}

viz_p <- function(cp, rst, var, trans, pnames) {
    cp2 <- move_cp_to_direction(cp, 200)
    cp_lines <- create_connection_lines(cp, cp2)

    cp2$sel <- factor(ifelse(cp2$sel == 2, "Selected", ifelse(cp2$small, "Small cell", "Normal antenna")), levels = c("Selected", "Small cell", "Normal antenna"))

    pal <- colorFactor(c("red", "gray60", "steelblue"), levels = c("Selected", "Small cell", "Normal antenna"))

    if (all(is.na(rst[]))) var <- "empty"


    title <- switch(var,
                    dBm = "Signal strength in dBm",
                    s = "Signal quality - s (in %)",
                    bsm = "Best server map",
                    lu = "Land use prior (in %)",
                    pag = "Likelihood - P(a|g) (in %)",
                    pg = "Composite prior - P(g) (in %)",
                    pga = "Probability - P(g|a) (in %)",
                    paste("Prior", pnames[var]))


    cls <- if (var == "dBm")  {
        dBm_classes
    } else {
        qty_classes
    }




    numpal <- ifelse(var %in% c("dBm", "s"), "Blues",
              ifelse(var == "pga", "Greens",
              ifelse(var == "pag", "Purples", "Oranges")))



    if (var %in% c("dBm", "s")) {
        pal2 <- colorBin(cls$colors, bins = cls$breaks, na.color = "#00000000")#, dBm_classes$labels)
    } else if (var == "bsm") {
        lvls <- raster::levels(rst)[[1]]
        cols <- rep(brewer.pal(13, "Set3"), length.out = nrow(lvls))
        pal2 <- colorFactor(palette = cols, domain = lvls$ID, na.color = "transparent")
    } else if (var != "empty") {
        rng <- range(rst[], na.rm = TRUE)
        pal2 <- colorNumeric(palette = numpal, rng,
                     na.color = "transparent")
    }

    lf <- leafletProxy("map") %>%
        clearShapes() %>%
        clearImages() %>%
        clearControls() %>%
        addCircleMarkers(data = cp2 %>% st_transform(crs = 4326), fillColor = ~pal(sel), color = "black", fillOpacity = 1, radius = 4, weight = 1, group = "Antenna locations", layerId = ~antenna)
        #addTiles() %>%


    if (var %in% c("dBm", "s")) {
        lf %>% addRasterImage(x = rst %>% raster::projectRaster(crs = st_crs(3857)$proj4string), opacity = trans, group = title, colors = pal2) %>%
            leaflet::addLayersControl(overlayGroups = c("Antenna locations", title), position = "topleft") %>%
            addLegend(colors = cls$colors, labels = cls$labels, opacity = trans)

    } else if (var == "bsm") {
        lf %>% addRasterImage(x = rst %>% raster::projectRaster(crs = st_crs(3857)$proj4string, method = "ngb"), opacity = trans, group = title, colors = cols) %>%
            leaflet::addLayersControl(overlayGroups = c("Antenna locations", title), position = "topleft") %>%
            addLegend(colors = cols, labels = as.character(lvls$antenna), opacity = trans)
    } else if (var == "empty") {
        lf %>% leaflet::addLayersControl(overlayGroups = c("Antenna locations"), position = "topleft")
    } else {
        lf %>% addRasterImage(x = rst %>% raster::projectRaster(crs = st_crs(3857)$proj4string), opacity = trans, group = title, colors = pal2) %>%
            leaflet::addLayersControl(overlayGroups = c("Antenna locations", title), position = "topleft") %>%
            addLegend(pal = pal2, values = rng, opacity = trans)
    }

}
