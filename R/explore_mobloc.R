#' Explore the visualize propagation, prior, likelihood and posterior probabilities per raster tile
#'
#' Explore the visualize propagation, prior, likelihood and posterior probabilities per raster tile
#'
#' @param cp cellplan, validated with \code{\link{validate_cellplan}}
#' @param raster raster object that contains the raster tile index numbers (e.g. created with \code{\link{create_raster}})
#' @param prop a propagation object, which is the result of \code{\link{process_cellplan}}
#' @param priorlist list of priors
#' @param param parameter list created with \code{prop_param}
#' @import shiny
#' @import leaflet
#' @importFrom graphics plot.new xspline
#' @export
explore_mobloc <- function(cp, raster, prop, priorlist = NULL, param) {

    crs <- st_crs(raster)

    epsg <- 4326

#     epsg <- if(crs$proj4string == "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")  {
#         3035
#     } else {
#         4326
#     }


    antenna <- NULL

    pnames <- names(priorlist)

    nprior <- length(pnames)
    choices_prior <- paste0("p", 1L:nprior)
    names(choices_prior) <- paste0("Prior ", pnames)
    names(pnames) <- choices_prior


    choices1 <- c("Signal strength - dBm" = "dBm",
                 "Signal quality - s" = "s",
                 "Best server map" = "bsm",
                 choices_prior,
                 "Composite prior - P(g) (see slider below)" = "pg")

    choices2 <- c("Likelihood - P(a|g)" = "pag",
                 "Probability - P(g|a)" = "pga")

    cells <- as.character(cp$antenna)
    #names(cells) <- paste("Cell", 1L:n)

    message("Creating coverage and best server maps...")
    cm_dBm <- create_coverage_map(prop, raster, type = "dBm")
    cm_s <- create_coverage_map(prop, raster, type = "s")
    bsm <- create_best_server_map(prop, raster)

    offset_value <- 150


    sliders <- mapply(function(i, nm) {
        if (i == choices_prior[length(choices_prior)]) {
            shiny::htmlOutput("plast")
        } else {
            sliderInput(i, paste("Faction", nm), min = 0, max = 1, value = 1/nprior, step  = 0.01)
        }

    }, choices_prior, pnames, SIMPLIFY = FALSE)

    app <- shinyApp(
        ui = fluidPage(
            titlePanel("Mobile location exploration"),
            sidebarLayout(
                sidebarPanel(
                    tabsetPanel(
                        tabPanel("Map setup",
                                 radioButtons("show", "Selection",  c("Whole grid" = "grid", "One antenna" = "ant"), selected = "grid"),
                                 radioButtons("var", "Variable", choices1, selected = "s"),
                                 wellPanel(
                                     conditionalPanel(
                                         condition = "(input.var == 'pga') || (input.var == 'pg')",
                                         sliders)),
                                 sliderInput("trans", "Transparency", min = 0, max = 1, value = 1, step = 0.1),
                                 checkboxInput("offset", "Antenna offset", value = TRUE)),
                        tabPanel("Antenna data",
                                 selectInput("sel", "Antenna", cells, selected = cells[1]),
                                 dataTableOutput("antennainfo"))
                    )),
                mainPanel(
                    leafletOutput("map", height=1000)
                ))
        ),
        server = function(input, output, session) {

            observe({
                show <- input$show
                var <- input$var
                if (!is.null(show)) {
                    choices <- if (show == "grid") choices1 else c(choices1, choices2)
                    selected <- if (var %in% choices) var else choices[1]
                    updateRadioButtons(session, "var", choices = choices, selected = selected)
                }
            })



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
                base_map(cp, offset_value, epsg)
            })


            output$antennainfo <- renderDataTable({
                cpant <- as.list(cp[cp$antenna == input$sel, ] %>% st_set_geometry(NULL))
                cpant$x <- sprintf("%.2f", cpant$x)
                cpant$y <- sprintf("%.2f", cpant$y)
                cpant$z <- sprintf("%.2f", cpant$z)
                cpant$ple <- sprintf("%.2f", cpant$ple)
                data.frame(Variable = names(cpant), Value = unname(unlist(cpant)))
            }, options = list(searching = FALSE, scrollx = FALSE, paging = FALSE, info = FALSE))

            observe({
                type <- input$var
                sel <- input$sel
                cp$sel <- 1L
                cp$sel[cp$antenna %in% sel] <- 2L
                if (input$show == "grid") {
                    composition <- get_composition()
                    rst <- create_q_raster(raster, psel, type = type, choices_prior, composition = composition, priorlist, cm_dBm, cm_s, bsm)
                } else {
                    if (type == "bsm") {
                        rst <- create_best_server_map(prop, raster, antennas = sel)
                    } else {
                        composition <- get_composition()
                        psel <- prop %>% filter(antenna == sel)

                        rst <- create_p_raster(raster, psel, type = type, choices_prior, composition = composition, priorlist)
                    }
                }

                viz_p(cp = cp, rst = rst, var = input$var, trans = input$trans, pnames = pnames, offset = ifelse(input$offset, offset_value, 0))
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
}


create_q_raster <- function(rst, ppr, type, choices_prior, composition, priorlist, cm_dBm, cm_s, bsm) {
    #rindex <- raster::getValues(rst)
    #r <- raster::raster(rst)

    if (type == "dBm") {
        r <- cm_dBm
    } else if (type == "s") {
        r <- cm_s
    } else if (type == "bsm") {
        r <- bsm
    } else if (type %in% choices_prior) {
        r <- priorlist[[as.integer(substr(type, 2, 2))]]
    } else if (type == "pg") {
        #composition <- c(priormix[1], (priormix[2] - priormix[1]), (1 - priormix[2]))
        r <- do.call(create_prior, c(unname(priorlist), list(name = "composite", weights = composition)))
    }

    # if (type != "dBm") {
    #     ppr <- ppr %>%
    #         mutate(x = x / sum(x) * 100)
    # }

    # raster::values(r)[match(ppr$rid, rindex)] <- ppr$x
    # r <- raster::trim(r)
    # r[r==0] <- NA
    raster::trim(r)
}

create_p_raster <- function(rst, ppr, type, choices_prior, composition, priorlist) {
    dBm <- s <- pag <- pg <- NULL

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

#
#     if (type != "dBm") {
#         ppr <- ppr %>%
#             mutate(x = x / sum(x) * 100)
#     }

    raster::values(r)[match(ppr$rid, rindex)] <- ppr$x
    r <- raster::trim(r)
    r[r==0] <- NA
    r
}

