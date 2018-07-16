viz_p <- function(cp, cp_poly, raster, prob, param, type=c("map", "3d"), cellid = NA, var = c("p", "db", "none")) {
    Cell_name <- beam_h <- dBm <- db <- dbLoss <- deg <- direction <- dist <- distance <- likelihood <- r <- rid <- s <- small <- x <- y <- NULL

    #     show.raster <- file.exists(file.path(prj$dir, "prob.rds"))
    #
    #     if (show.raster) prob <- readRDS(file.path(prj$dir, "prob.rds"))
    #     cp <- readRDS(file.path(prj$dir, "cp.rds"))
    #     cp_poly <- readRDS(file.path(prj$dir, "cp_poly.rds"))
    #
    #     bbx <- prj$bbox
    #     crs <- st_crs(prj$crs)$proj4string
    #     sp_rect <- bb_sp(matrix(bbx, ncol=2), projection = crs)
    #
    #
    #     r100 <- create_100m(prj$bbox)
    # }

    var <- match.arg(var)

    cp_lines <- cp_poly
    cp_lines$geometry <- st_cast(cp_lines$geometry, "MULTILINESTRING", group_or_split = FALSE)

    cellnames <- as.character(cp$Cell_name)
    cp$sel <- 1
    cp_lines$sel <- 1
    cp$id <- 1L:nrow(cp)
    cp_lines$id <- 1L:nrow(cp)
    cp_lines <- cp_lines %>% left_join(cp %>% st_set_geometry(NULL) %>% mutate(sel=NULL), by = "id")

    if (!is.na(cellid[1])) {
        cellnames <- cellnames[cellid]
        cp$sel[cellid] <- 2
        cp_lines$sel[cellid] <- 2
    }

    # if (show.raster) {
        prob2 <- prob %>%
            filter(Cell_name %in% cellnames) %>%
            #filter(Cell_name %in% cells) %>%
            group_by(rid)

        r <- raster(raster)

        if (var=="p") {
            prob2 <- prob2 %>%
                summarise(p=sum(p)) %>%
                ungroup()
            raster::values(r)[prob2$rid] <- prob2$p * 1000   #log(df2$p * 10000)
            title <- "probability (in 1/1000)"
        } else {
            prob2 <- prob2 %>%
                summarise(db=max(db)) %>%
                ungroup()
            raster::values(r)[prob2$rid] <- prob2$db * 1000   #log(df2$p * 10000)
            title <- "dBm"
        }
        r <- raster::trim(r)
        # if ("3d" %in% type) {
        #     require(rasterVis)
        #     rgl::rgl.clear()
        #     rasterVis::plot3D(r)
        # }
    # }




    if ("map" %in% type) {
        #tmm <- tmap_mode("view")

        tm <- tm_shape(cp_lines) +
            tm_lines(lwd = "sel", col = "sel", scale = 2, palette = c("gray60", "red"), auto.palette.mapping = FALSE, legend.lwd.show = FALSE, legend.col.show = FALSE, popup.vars = TRUE, id = "id") +
            tm_shape(cp) +
            tm_dots(col = "sel", legend.show = FALSE, palette = c("gray60", "red"), style = "cat", popup.vars = TRUE, id = "id")

        tm <- tm + qtm(r, raster.title = title)
        #if (!is.null(sp_rect)) tm <- tm + qtm(sp_rect, fill=NULL)
        #print(tm)
        #suprobessMessages(tmap_mode(tmm))
        tm
    } else {
        invisible()
    }
}
