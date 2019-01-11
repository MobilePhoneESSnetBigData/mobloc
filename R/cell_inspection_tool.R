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

    # number of active grid cells (needed for normalization)
    rv <- raster::getValues(raster)
    rva <- unique(prob$rid)
    nrva <- length(rva)

    # get and normalize land use prior
    if (missing(prior)) {
        pr_lu <- rep(1, length(ZL_raster))
    } else {
        pr_lu <- raster::getValues(prior)
    }
    pr_lu[!(rv %in% rva)] <- 0
    pr_lu <- pr_lu / sum(pr_lu)

    # create uniform prior
    pr_u <- 1/nrva

    # get and normalize network prior
    totals <- sum(prob$s)
    prob <- prob %>%
        group_by(rid) %>%
        mutate(pr_s = sum(s) / totals) %>%
        ungroup()

    prob$pr_lu <- pr_lu[match(prob$rid, rv)]
    prob$pr_u <- pr_u

    cells <- as.character(cp$Cell_name)
    #names(cells) <- paste("Cell", 1L:n)

    app <- shinyApp(
        ui = fluidPage(
            titlePanel("Cell Inspection Tool"),
            sidebarLayout(
                sidebarPanel(
                    radioButtons("var", "Variable", c("Signal strength - dBm" = "dBm",
                                                      "Relative signal strength - s" = "s",
                                                      "Landuse" = "lu",
                                                      "Likelihood - P(a|g)" = "pag",
                                                      "Composite prior - P(g)" = "pg (see slider below)",
                                                      "Probability - P(g|a)" = "pga"), selected = "s"),
                    conditionalPanel(
                        condition = "(input.var == 'pga') || (input.var == 'pg')",
                        sliderInput("priormix", "Composite Prior (move slider below to change composition)", min = 0, max = 1, value = c(.1,.4), step  = 0.05),
                        shiny::textOutput("priortext")),
                    sliderInput("trans", "Transparency", min = 0, max = 1, value = 1, step = 0.1),
                    checkboxInput("showall", "Show all antennas", value = FALSE),
                    selectInput("sel", "Antenna", cells, selected = cells[1])),

                    #checkboxGroupInput("sel", "Selected cells", cells, selected = "c1")),
                mainPanel(
                    leafletOutput("map", height=1000)
                ))
        ),
        server = function(input, output, session) {

            # observe({
            #     if (!input$threed) rgl::rgl.clear()
            # })


            output$priortext <- renderText({
                paste0(round(input$priormix[1] * 100), "% uniform, ",
                       round((input$priormix[2] - input$priormix[1])  * 100), "% land use, ",
                       round((1-input$priormix[2]) * 100), "% signal strength")
            })

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
                rst <- create_p_raster(raster, probsel, type = input$var, priormix = input$priormix)

                title <- switch(input$var,
                                dBm = "Signal strength in dBm",
                                s = "Relative signal strength - s (in %)",
                                lu = "Land use prior (in %)",
                                pag = "Likelihood - P(a|g) (in %)",
                                pg = "Composite prior - P(g) (in %)",
                                pga = "Probability - P(g|a) (in %)")


                visp <- viz_p(cp = cpsel, cp_poly = cp_polysel, rst = rst, title = title, trans = input$trans)

                if (!is.null(tm)) {
                    tmap_leaflet(visp)
                } else {
                    tmap_leaflet(visp + tm)
                }

            })

            observeEvent(input$map_marker_click, { # update the location selectInput on map clicks
                p <- input$map_marker_click

                id <- which(sapply(cells, function(cl) {
                    length(grep(cl, p$id, fixed = TRUE)) == 1
                }))[1]


                if (length(id)!=0) {
                    updateSelectInput(session, "sel",
                                      selected = cells[id])
                }

            })


        }
    )

    suppressWarnings(runApp(app)) # to suppress: Ignoring appended content; appendContent can't be used in a Shiny render call
    tmap_mode(tmm)
}


create_p_raster <- function(rst, ppr, type, priormix = c(1,1)) {
    rindex <- raster::getValues(rst)
    r <- raster::raster(rst)

    if (type == "dBm") {
        ppr <- ppr %>%
            mutate(x = dBm)
    } else if (type == "s") {
        ppr <- ppr %>%
            mutate(x = s)
    } else if (type == "lu") {
        ppr <- ppr %>%
            mutate(x = pr_lu)
    } else if (type == "pag") {
        ppr <- ppr %>%
            mutate(x = pag)
    } else {
        ppr2 <<- ppr
        ppr <- ppr %>%
            mutate(pg = pr_u * priormix[1] + pr_lu * (priormix[2] - priormix[1]) + pr_s * (1 - priormix[2]))

        if (type == "pg") {
            ppr <- ppr %>%
                mutate(x = pg)
        } else {
            ppr <- ppr %>%
                mutate(x = pag * pg)
        }
    }


    if (type != "dBm") {
        ppr <- ppr %>%
            mutate(x = x / sum(x) * 100)
    }

    raster::values(r)[match(ppr$rid, rindex)] <- ppr$x
    r <- raster::trim(r)
    r[r==0] <- NA
    r
}

