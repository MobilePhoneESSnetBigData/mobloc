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
cell_inspection_tool <- function(cp, cp_poly, raster, prob, param, prior = NULL, tm = NULL) {
    tmm <- tmap_mode("view")

    n <- nrow(cp)

    lu <- raster::getValues(prior)

    prob$lu <- lu[match(prob$rid, getValues(raster))]

    cells <- as.character(cp$Cell_name)
    #names(cells) <- paste("Cell", 1L:n)

    app <- shinyApp(
        ui = fluidPage(
            titlePanel("Cell Inspection Tool"),
            sidebarLayout(
                sidebarPanel(
                    radioButtons("var", "Variable", c("Signal strength - dB" = "db",
                                                      "Relative signal strength - s" = "s",
                                                      "Landuse" = "lu",
                                                      "Likelihood - P(a|g)" = "p",
                                                      "Probability - P(g|a)" = "pga"), selected = "s"),
                    conditionalPanel(
                        condition = "input.var == 'pga'",
                        sliderInput("alpha", "Weight Land Use  - alpha", min = 0, max = 1, value = 0, step  = 0.05),
                        sliderInput("beta", "Weight Network - beta", min = 0, max = 1, value = 0, step  = 0.05)),
                    sliderInput("trans", "Transparency", min = 0, max = 1, value = 1, step = 0.1),
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


                ## subset data
                sel <- input$sel
                probsel <- prob %>% filter(Cell_name %in% sel)
                rids <- unique(probsel$rid)

                if (!input$showall) {
                    sel2  <- prob %>% filter(rid %in% rids) %>% dplyr::select(Cell_name) %>% unlist() %>% as.character() %>%  unique()
                    cpsel <- cp %>% filter(Cell_name %in% sel2)
                    cp_polysel <- cp_poly %>% filter(Cell_name %in% sel2)

                } else {
                    cpsel <- cp
                    cp_polysel <- cp_poly
                }

                cp_polysel$geometry <- st_cast(cp_polysel$geometry, "MULTILINESTRING", group_or_split = FALSE)

                cpsel$sel <- 1L
                cpsel$sel[cpsel$Cell_name %in% sel] <- 2L

                cp_polysel$sel <- 1L
                cp_polysel$sel[cp_polysel$Cell_name %in% sel] <- 2L


                ## create raster
                rst <- create_p_raster(raster, probsel, type = input$var, alpha = input$alpha, beta = input$beta)

                title <- switch(input$var,
                                db = "Signal strength in dBm",
                                s = "Relative signal strength - s (in %)",
                                lu = "Land use prior (in %)",
                                p = "Likelihood - P(a|g) (in %)",
                                pga = "Probability - P(g|a) (in %)")


                visp <- viz_p(cp = cpsel, cp_poly = cp_polysel, raster = rst, title = title, trans = input$trans)

                if (!is.null(tm)) {
                    tmap_leaflet(visp)
                } else {
                    tmap_leaflet(visp + tm)
                }

            })

            observeEvent(input$map_marker_click, { # update the location selectInput on map clicks
                p <- input$map_marker_click
                print(p)
                NULL
                #updateSelectInput(session, "gm",
                #                  selected = p$id)
            })


        }
    )

    suppressWarnings(runApp(app)) # to suppress: Ignoring appended content; appendContent can't be used in a Shiny render call
    tmap_mode(tmm)
}


create_p_raster <- function(raster, ppr, type, alpha = 0, beta = 0) {
    rindex <- getValues(raster)
    r <- raster(raster)

    if (type == "db") {
        ppr <- ppr %>%
            mutate(x = db)
    } else if (type == "s") {
        ppr <- ppr %>%
            mutate(x = s)
    } else if (type == "lu") {
        ppr <- ppr %>%
            mutate(x = lu)
    } else if (type == "p") {
        ppr <- ppr %>%
            mutate(x = p)
    } else {
        ppr <- ppr %>%
            mutate(pgland = lu / sum(lu), # normalize prior for selected antenna
                   pgunif = 1/ length(r)) %>%
            mutate(pg2 = alpha * pgland + (1-alpha) * pgunif) %>%
            mutate(pag = beta * s + (1-beta) * p) %>%
            mutate(x = pag * pg2)
    }


    if (type != "db") {
        ppr <- ppr %>%
            mutate(x = x / sum(x) * 100)
    }

    raster::values(r)[match(ppr$rid, rindex)] <- ppr$x
    r <- raster::trim(r)
    r[r==0] <- NA
    r
}

