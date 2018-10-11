viz_p <- function(cp, cp_poly, raster, title, trans) {

    cp2 <- move_cp_to_direction(cp, 20)
    cp_lines <- create_connection_lines(cp, cp2)

print(cp_poly)

    tm <- tm_shape(cp_lines) +
        tm_lines(group = "Antenna locations", id = "Cell_name") +
        tm_shape(cp2) +
        tm_dots(col = "sel", size = .1, legend.show = FALSE, palette = c("gray60", "red"), style = "cat", popup.vars = TRUE, id = "Cell_name", clustering = FALSE, group = "Antenna locations") +
        tm_shape(cp_poly) +
        tm_lines(lwd = "sel", col = "sel", scale = 6, palette = c("gray60", "red"), style = "cat", legend.lwd.show = FALSE, legend.col.show = FALSE, popup.vars = TRUE, group = "Polygon borders") +
        tm_basemap("OpenStreetMap")

    # tm <- tm_shape(cp_poly) +
    #     tm_lines(id = "Cell_name")

    tm <- tm + tm_shape(raster) + tm_raster(alpha = trans, title = title, group = title)

    tm
}
