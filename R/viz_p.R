viz_p <- function(cp, cp_poly, raster, prob, param, cellid = NA, var = c("p", "s", "db", "pga", "none")) {
    Cell_name <- beam_h <- dBm <- db <- dbLoss <- deg <- direction <- dist <- distance <- likelihood <- r <- rid <- s <- small <- x <- y <- NULL


    var <- match.arg(var)


#    if (var == "pga") browser()

    ## create borders
    cp_lines <- cp_poly
    cp_lines$geometry <- st_cast(cp_lines$geometry, "MULTILINESTRING", group_or_split = FALSE)

    ## create id variable and set "sel" to 2 for selected cells
    cellnames <- as.character(cp$Cell_name)
    cp$sel <- 1
    cp_lines$sel <- 1
    #cp$id <- 1L:nrow(cp)
    #cp_lines$id <- 1L:nrow(cp)
    cp_lines <- cp_lines %>% left_join(cp %>% st_set_geometry(NULL) %>% mutate(sel=NULL), by = "Cell_name")


    rindex <- getValues(raster)


    if (!is.na(cellid[1])) {
        cellnames <- cellid
        cp$sel[cp$Cell_name %in% cellid] <- 2
        cp_lines$sel[cp_lines$Cell_name %in% cellid] <- 2
    }

    prob2 <- prob %>%
        filter(Cell_name %in% cellnames) %>%
        #filter(Cell_name %in% cells) %>%
        group_by(rid)

    r <- raster(raster)

    if (var=="p") {
        # prob2 <- prob2 %>%
        #     summarise(p=sum(p)) %>%
        #     ungroup()
        raster::values(r)[match(prob2$rid, rindex)] <- prob2$p * 1000   #log(df2$p * 10000)
        title <- "probability (in 1/1000)"
    } else if (var=="s") {
        # prob2 <- prob2 %>%
        #     summarise(s=sum(s)) %>%
        #     ungroup()
        raster::values(r)[match(prob2$rid, rindex)] <- prob2$s * 1000   #log(df2$p * 10000)
        title <- "relative signal strength (in 1/1000)"
    } else if (var=="db") {
        # prob2 <- prob2 %>%
        #     summarise(db=max(db)) %>%
        #     ungroup()
        raster::values(r)[match(prob2$rid, rindex)] <- prob2$db * 1000   #log(df2$p * 10000)
        title <- "dBm"
    } else {
        raster::values(r)[match(prob2$rid, rindex)] <- prob2$pga * 1000   #log(df2$p * 10000)
        title <- "pga"
    }

    r <- raster::trim(r)

    r[r==0] <- NA


    varname <- switch(var,
                      p = "Likelihood P(a|g)",
                      s = "Relative signal strength s",
                      db = "Signal strength dBm",
                      pga = "Probability P(g|a)",
                      "var")


    #tmm <- tmap_mode("view")

    tm <- tm_shape(cp_lines) +
        tm_lines(lwd = "sel", col = "sel", scale = 4, palette = c("gray60", "red"), auto.palette.mapping = FALSE, legend.lwd.show = FALSE, legend.col.show = FALSE, popup.vars = TRUE, id = "id", group = "polygon borders") +
        tm_shape(cp) +
        tm_dots(col = "sel", size = .1, legend.show = FALSE, palette = c("gray60", "red"), style = "cat", popup.vars = TRUE, id = "id", clustering = TRUE, group = "antenna locations") +
        tm_basemap("OpenStreetMap")

    tm <- tm + qtm(r, raster.title = title, group = varname)

    tm

}
