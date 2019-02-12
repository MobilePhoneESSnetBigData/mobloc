#' Explore the Visualize signal strengths and probabilities per raster cell
#'
#' Visualize signal strengths and probabilities per raster cell
#'
#' @param cp cellplan
#' @param cp_poly cellplan polygons
#' @param raster raster
#' @param prop propagation model
#' @param priorlist list of priors
#' @param param parameter list
#' @param tm tmap object (optional), that is plotted on top of the map
#' @import tmap
#' @import shiny
#' @import leaflet
#' @importFrom graphics plot.new xspline
#' @export
explore_mobloc <- function(cp, raster, prop, priorlist = NULL, param, tm = NULL) {
    tmm <- tmap_mode("view")

    pnames <- names(priorlist)

    nprior <- length(pnames)
    choices_prior <- paste0("p", 1L:nprior)
    names(choices_prior) <- paste0("Prior ", pnames)
    names(pnames) <- choices_prior

    choices <- c("Signal strength - dBm" = "dBm",
                 "Signal quality - s" = "s",
                 choices_prior,
                 "Likelihood - P(a|g)" = "pag",
                 "Composite prior - P(g) (see slider below)" = "pg",
                 "Probability - P(g|a)" = "pga")

    cells <- as.character(cp$antenna)
    #names(cells) <- paste("Cell", 1L:n)


    sliders <- mapply(function(i, nm) {
        if (i == choices_prior[length(choices_prior)]) {
            shiny::htmlOutput("plast")
        } else {
            sliderInput(i, paste("Faction", nm), min = 0, max = 1, value = 1/nprior, step  = 0.01)
        }

    }, choices_prior, pnames, SIMPLIFY = FALSE)

    app <- shinyApp(
        ui = fluidPage(
            titlePanel("Antenna prop exploration"),
            sidebarLayout(
                sidebarPanel(
                    radioButtons("var", "Variable", choices, selected = "s"),
                    wellPanel(
                    conditionalPanel(
                        condition = "(input.var == 'pga') || (input.var == 'pg')",
                        sliders)),
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



            get_composition <- reactive({
                values <- sapply(choices_prior[-nprior], function(x) {
                    input[[x]]
                })
                if (sum(values) > 1) {
                    showW <- TRUE
                    values <- values / sum(values)
                } else {
                    showW <- FALSE
                }
                composition <- c(values, 1-sum(values))
                attr(composition, "showW") <- showW
                composition
            })

            output$plast <- renderUI({
                composition <- get_composition()
                showW <- attr(composition, "showW")
                HTML(paste0("<b>Faction ", pnames[nprior], ": ", round(composition[nprior], 2), ifelse(showW, " (warning: the sum of slider values is greater than 1)", ""),  "</b>"))
            })

            output$map <- renderLeaflet({

                ## subset data
                sel <- input$sel

                composition <- get_composition()


                # if (input$showall) {
                #     psel <- prop
                # } else {
                psel <- prop %>% filter(antenna %in% sel)
                # }

                # cpsel <- cp
                # psel <- prop %>% filter(antenna %in% sel)
                # rids <- unique(psel$rid)

                # if (!input$showall) {
                #
                #
                #     #sel2  <- prop %>% filter(rid %in% rids) %>% dplyr::select(antenna) %>% unlist() %>% as.character() %>%  unique()
                #     #cpsel <- cp %>% filter(antenna %in% sel2)
                #     #cp_polysel <- cp_poly %>% filter(antenna %in% sel2)
                #
                # } else {
                #     cpsel <- cp
                #     #cp_polysel <- cp_poly
                # }

                #cp_polysel$geometry <- st_cast(cp_polysel$geometry, "MULTILINESTRING", group_or_split = FALSE)

                cp$sel <- 1L
                cp$sel[cp$antenna %in% sel] <- 2L

                # cp_polysel$sel <- 1L
                # cp_polysel$sel[cp_polysel$antenna %in% sel] <- 2L


                ## create raster
                rst <- create_p_raster(raster, psel, type = input$var, choices_prior, composition = composition, priorlist)

                title <- switch(input$var,
                                dBm = "Signal strength in dBm",
                                s = "Signal quality - s (in %)",
                                lu = "Land use prior (in %)",
                                pag = "Likelihood - P(a|g) (in %)",
                                pg = "Composite prior - P(g) (in %)",
                                pga = "Probability - P(g|a) (in %)",
                                paste("Prior", pnames[input$var]))


                visp <- viz_p(cp = cp, rst = rst, title = title, trans = input$trans)

                if (is.null(tm)) {
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


create_p_raster <- function(rst, ppr, type, choices_prior, composition, priorlist) {
    rindex <- raster::getValues(rst)
    r <- raster::raster(rst)

    if (type == "dBm") {
        ppr <- ppr %>%
            mutate(x = dBm)
    } else if (type == "s") {
        ppr <- ppr %>%
            mutate(x = s)
    } else if (type %in% choices_prior) {
        priordf <- prior_to_df(priorlist[[as.integer(substr(type, 2, 2))]], rst)
        ppr <- ppr %>%
            mutate(x = priordf$p[match(ppr$rid, priordf$rid)])
    } else if (type == "pag") {
        ppr <- ppr %>%
            mutate(x = pag)
    } else {
        #composition <- c(priormix[1], (priormix[2] - priormix[1]), (1 - priormix[2]))

        priordf <- prior_to_df(do.call(create_prior, c(unname(priorlist), list(name = "composite", weights = composition))), rst)
        ppr <- ppr %>%
            mutate(pg = priordf$p[match(ppr$rid, priordf$rid)])

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

