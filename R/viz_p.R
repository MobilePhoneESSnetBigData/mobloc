viz_cells <- function(prj) {
    cp <- readRDS(file.path(prj$dir, "cp.rds"))
    cp_poly <- readRDS(file.path(prj$dir, "cp_poly.rds"))

    bbx <- prj$bbox
    crs <- st_crs(prj$crs)$proj4string
    sp_rect <- bb_sp(matrix(bbx, ncol=2), projection = crs)

    tmm <- tmap_mode("view")

    print(qtm(cp_poly, fill=NULL) + qtm(cp) + qtm(sp_rect, fill=NULL))

    suprobessMessages(tmap_mode(tmm))
}

#
# art_mast_prob <- function(prj) {
#     cp <- prj$cp %>%
#         mutate(beam_h = beam_h_deg / 180 * pi,
#                beam_v = beam_v_deg / 180 * pi,
#                direction = direction_deg / 180 * pi,
#                directionL = (direction - beam_h),
#                directionR = (direction + beam_h)
#         )
#
#     prj$cp <- cp
#
#     #cp <- data.frame(Cell_name = "test", x = 120076, y = 486161, rng = 10000, a1 = a1, a2 = a2, indoor = indoor)
#
#     cp_poly <- create_cellplan_polygons(prj) #construct_cp_poly(cp %>% select(Cell_name, x, y, rng, a1, a2, indoor), shape = "plectrum")
#     cp_poly$x <- cp$x
#     cp_poly$y <- cp$y
#     cp_poly$z <- cp$z
#     cp_poly$height <- cp$height
#     cp_poly$direction <- cp$direction
#     cp_poly$tilt <- cp$tilt
#     cp_poly$beam_h <- cp$beam_h
#     cp_poly$beam_v <- cp$beam_v
#     cp_poly$indoor <- cp$indoor
#     cp_poly$id <- 1:nrow(cp_poly)
#
#     r100 <- raster(ext=extent(as(cp_poly, "Spatial")), resolution = c(100,100), crs = st_crs(prj$crs)$proj4string)
#     r100b <- r100
#     r100 <- setValues(r100, 0)
#     r100b <- setValues(r100, 1:length(r100))
#     b100 <- brick(r100b, r100)
#
#
#     prob <- calculate_probabilities(list(cp_poly), list(b100), prj$param)
#     prob$Cell_name <- cp$Cell_name[prob$pid]
#
#     cp <- cp %>%
#         mutate(x_rd = x,
#                y_rd = y) %>%
#         st_as_sf(coords = c("x_rd", "y_rd"), crs = prj$crs)
#     list(prob = prob %>% select(Cell_name, rid, p, db), cp_poly = cp_poly, cp = cp, r100 = b100)
# }



viz_p <- function(cp, cp_poly, raster, prob, param, type=c("map", "3d"), cellid = NA, var = c("p", "db", "none")) {
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
            values(r)[prob2$rid] <- prob2$p * 1000   #log(df2$p * 10000)
            title <- "probability (in 1/1000)"
        } else {
            prob2 <- prob2 %>%
                summarise(db=max(db)) %>%
                ungroup()
            values(r)[prob2$rid] <- prob2$db * 1000   #log(df2$p * 10000)
            title <- "dBm"
        }
        r <- trim(r)
        if ("3d" %in% type) {
            require(rasterVis)
            rgl::rgl.clear()
            plot3D(r)
        }
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
