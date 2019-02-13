viz_p <- function(cp, rst, title, trans) {

    cp2 <- move_cp_to_direction(cp, 200)
    cp_lines <- create_connection_lines(cp, cp2)

    # cp2 <<- cp2
    # cp_lines <<- cp_lines
    # cp_poly <<- cp_poly
    # rst <<- rst

    cp2$sel <- factor(ifelse(cp2$sel == 2, "Selected", ifelse(cp2$small, "Small cell", "Normal antenna")), levels = c("Selected", "Small cell", "Normal antenna"))


    tm <- tm_shape(cp_lines) +
        tm_lines(group = "Antenna locations", id = "antenna") +
        tm_shape(cp2) +
        tm_dots(col = "sel", size = .1, legend.show = TRUE, palette = c("red", "gray60", "gold"), style = "cat", popup.vars = TRUE, id = "antenna", clustering = FALSE, group = "Antenna locations") +
        #tm_shape(cp_poly) +
        #tm_lines(lwd = "sel", col = "sel", scale = 6, palette = c("gray60", "red"), style = "cat", legend.lwd.show = FALSE, legend.col.show = FALSE, popup.vars = TRUE, group = "Polygon borders") +
        tm_basemap("OpenStreetMap")


    tm <- tm + tm_shape(rst) + tm_raster(alpha = trans, title = title, group = title, stretch.palette = FALSE)

    tm
}
