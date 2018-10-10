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
cell_inspection_tool <- function(cp, cp_poly, raster, prob, param, prior = NULL) {
    tmm <- tmap_mode("view")

    n <- nrow(cp)

    cp <- move_cp_to_direction(cp, 20)

    pg <- raster::getValues(prior)

    prob$pg <- pg[match(prob$rid, getValues(raster))]

    cells <- as.character(cp$Cell_name)
    #names(cells) <- paste("Cell", 1L:n)

    app <- shinyApp(
        ui = fluidPage(
            titlePanel("Cell Inspection Tool"),
            sidebarLayout(
                sidebarPanel(
                    radioButtons("var", "Variable", c("Signal strength - dB" = "db",
                                                      "Relative signal strength - s" = "s",
                                                      "Likelihood - P(a|g)" = "p",
                                                      "Probability - P(g|a)" = "pga"), selected = "s"),
                    conditionalPanel(
                        condition = "input.var == 'pga'",
                        sliderInput("alpha", "Weight Land Use  - alpha", min = 0, max = 1, value = 0, step  = 0.05),
                        sliderInput("beta", "Weight Network - beta", min = 0, max = 1, value = 0, step  = 0.05)),
                    checkboxInput("showall", "Show all antennas", value = FALSE),
                    selectInput("sel", "Antenna", cells, selected = cells[1])),

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

                #idx <- match(input$sel, cells)

                sel <- input$sel
                probsel <- prob %>% filter(Cell_name %in% sel)
                rids <- unique(probsel$rid)
                ng <- length(rids)


                if (input$var == "pga") {
                    alpha <- input$alpha
                    beta <- input$beta

                    probsel <- probsel %>%
                        mutate(pgland = pg / sum(pg), # normalize prior for selected antenna
                               pgunif = 1/ ng) %>% # create uniform prior
                        mutate(pg2 = alpha * pg + (1-alpha) * pgunif) %>%
                        mutate(pag = beta * s + (1-beta) * p) %>%
                        mutate(pga = pag * pg2) %>%
                        mutate(pga = pga / sum(pga))
                }


                if (!input$showall) {
                    sel2  <- prob %>% filter(rid %in% rids) %>% dplyr::select(Cell_name) %>% unlist() %>% as.character() %>%  unique()

                    cpsel <- cp %>% filter(Cell_name %in% sel2)
                    #cp_polysel <- cp_poly %>% filter(Cell_name %in% sel)


                    cp_polysel <- cp_poly %>% filter(Cell_name %in% sel2)

                } else {
                    cpsel <- cp
                    cp_polysel <- cp_poly
                }

                tm <- viz_p(cpsel, cp_polysel, raster, probsel, param, var = input$var, cellid = input$sel)
                #tmap_leaflet(qtm(cpsel))

                tmap_leaflet(tm)
            })
        }
    )

    suppressWarnings(runApp(app)) # to suppress: Ignoring appended content; appendContent can't be used in a Shiny render call
    tmap_mode(tmm)
}
