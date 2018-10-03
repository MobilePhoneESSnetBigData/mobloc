#' Visualize signal strengths and probabilities per raster cell
#'
#' Visualize signal strengths and probabilities per raster cell
#'
#' @param cp cellplan
#' @param cp_poly cellplan polygons
#' @param raster raster
#' @param prob probabilities per raster cell
#' @param param parameter list
#' @import tmap
#' @import shiny
#' @import leaflet
#' @importFrom graphics plot.new xspline
#' @export
cell_inspection_tool <- function(cp, cp_poly, raster, prob, param) {
    tmm <- tmap_mode("view")

    n <- nrow(cp)

    cp <- move_cp_to_direction(cp, 20)

    cells <- paste0("c", 1L:n)
    names(cells) <- paste("Cell", 1L:n)

    app <- shinyApp(
        ui = fluidPage(
            titlePanel("Cell Inspection Tool"),
            sidebarLayout(
                sidebarPanel(
                    radioButtons("var", "Variable", c("Signal strength (dB)" = "db",
                                                      "Relative signal strength" = "s",
                                                      "Probability" = "p"), selected = "s"),
                    #checkboxInput("threed", "3d plot", value = FALSE),
                    selectInput("sel", "Selected cells", cells, selected = "c1")),
                    #checkboxGroupInput("sel", "Selected cells", cells, selected = "c1")),
                mainPanel(
                    leafletOutput("map", height=1000)
                ))
        ),
        server = function(input, output) {

            # observe({
            #     if (!input$threed) rgl::rgl.clear()
            # })


            output$map <- renderLeaflet({
                #type <- if (input$threed) c("map", "3d") else "map"
                tm <- viz_p(cp, cp_poly, raster, prob, param, type = "map", var = input$var, cellid = match(input$sel, cells))
                tmap_leaflet(tm)
            })
        }
    )

    suppressWarnings(runApp(app)) # to suppress: Ignoring appended content; appendContent can't be used in a Shiny render call
    tmap_mode(tmm)
}
