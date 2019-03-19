#' Explore the visualize propagation, prior, likelihood and posterior probabilities per raster tile
#'
#' Explore the visualize propagation, prior, likelihood and posterior probabilities per raster tile. When the raster is large (say larger than 30 by 30 kilometers), we recommend to specify the filter arguemnt.
#'
#' @param cp cellplan, validated with \code{\link{validate_cellplan}}
#' @param raster raster object that contains the raster tile index numbers (e.g. created with \code{\link{create_raster}})
#' @param prop a propagation object, which is the result of \code{\link{process_cellplan}}
#' @param priorlist list of priors
#' @param filter bounding box of the filter of the visualized raster. If not specified, the whole raster is shown, which could be very slow. Therefore, we recommand to use a filter when the raster covers a large area (say 30 by 30 kilometers).
#' @param coverage_map_dBm coverage map, created with \code{\link{create_coverage_map}} (with \code{type = "dBm"}). If not specified, it will be created (which takes some time).
#' @param coverage_map_s coverage map, created with \code{\link{create_coverage_map}} (with \code{type = "s"}). If not specified, it will be created (which takes some time).
#' @param best_server_map best server map, created with \code{\link{create_best_server_map}}. If not specified, it will be created (which takes some time).
#' @note Note that duo to the reprojection of the raster to the web mercator projection (for interactive maps), the visualized raster does not correspond exactly to the output raster.
#' @import shiny
#' @importFrom shinyjs useShinyjs disable
#' @import leaflet
#' @importFrom graphics plot.new xspline
#' @seealso \href{../doc/mobloc.html}{\code{vignette("mobloc")}}
#' @export
explore_mobloc <- function(cp, raster, prop, priorlist, filter = NULL, coverage_map_dBm = NULL, coverage_map_s = NULL, best_server_map = NULL) {

    crs <- st_crs(raster)

    epsg <- 4326

#     epsg <- if(crs$proj4string == "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")  {
#         3035
#     } else {
#         4326
#     }



    if (!missing(filter)) {

        raster <- mobloc_crop_raster(raster, bbx = filter)

        a <- mobloc_find_antennas(prop, raster)
        prop <- mobloc_filter_antenna(prop, a, raster)
        cp <- mobloc_filter_antenna(cp, a)

        priorlist <- lapply(priorlist, mobloc_crop_raster, bbx = filter)
    }


    rect <- create_bbx_rect(raster2bbx(raster)) %>% st_transform(crs = 4326)



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
                 "Posterior - P(g|a)" = "pga")

    choices <- c("Signal strength - dBm" = "dBm",
                  "Signal quality - s" = "s",
                  "Best server map" = "bsm",
                 "Likelihood - P(a|g)" = "pag",
                  choices_prior,
                  "Composite prior - P(g) (see slider below)" = "pg",
                 "Posterior - P(g|a)" = "pga")


    #https://stackoverflow.com/questions/34733147/unable-to-disable-a-shiny-app-radio-button-using-shinyjs

    cells <- as.character(cp$antenna)
    #names(cells) <- paste("Cell", 1L:n)


    if (missing(coverage_map_dBm)) {
        message("Creating coverage maps (dBm)...")
        coverage_map_dBm <- create_coverage_map(prop, raster, type = "dBm") # cm_dBm
    }
    if (missing(coverage_map_s)) {
        message("Creating coverage maps (s)...")
        coverage_map_s <- create_coverage_map(prop, raster, type = "s") #cm_s
    }
    if (missing(best_server_map)) {
        message("Creating best server maps...")
        best_server_map <- create_best_server_map(prop, raster) #bsm
    }

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
            useShinyjs(),

            tags$head(
                tags$style(HTML("
                  .disabled {
                    opacity: 0.4;
                  }
                "))
            ),

            titlePanel("Mobile location exploration"),
            sidebarLayout(
                sidebarPanel(
                    tabsetPanel(
                        tabPanel("Map setup",
                                 radioButtons("show", "Selection",  c("All antennas" = "grid", "Single antenna" = "ant"), selected = "grid"),
                                 radioButtons("var", "show", choices, selected = "s"),
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

            # observe({
            #     show <- input$show
            #     var <- input$var
            #     if (!is.null(show)) {
            #         #choices <- if (show == "grid") choices1 else c(choices1, choices2)
            #         #selected <- if (var %in% choices) var else choices[1]
            #         selected <- if (show == "grid") cho
            #         updateRadioButtons(session, "var", choices = choices, selected = selected)
            #     }
            # })

            get_var <- reactive({
                show <- input$show
                var <- input$var

                if (show == "grid" && var %in% c("pag", "pga")) choices[1] else var
            })


            observe({
                show <- input$show
                var <- get_var()

                if (show == "grid") {
                    if (var != input$var) updateRadioButtons(session, "var", choices = choices, selected = var)
                    shinyjs::runjs("$('#var input[value=pag]').parent().parent().addClass('disabled')")
                    shinyjs::runjs("$('#var input[value=pga]').parent().parent().addClass('disabled')")
                } else {
                    shinyjs::runjs("$('#var input[value=pag]').parent().parent().removeClass('disabled')")
                    shinyjs::runjs("$('#var input[value=pga]').parent().parent().removeClass('disabled')")

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
                type <- get_var()
                sel <- input$sel
                cp$sel <- 1L
                cp$sel[cp$antenna %in% sel] <- 2L
                if (input$show == "grid") {
                    composition <- get_composition()
                    rst <- create_q_raster(raster, psel, type = type, choices_prior, composition = composition, priorlist, coverage_map_dBm, coverage_map_s, best_server_map)
                } else {
                    if (type == "bsm") {
                        rst <- create_best_server_map(prop, raster, antennas = sel)
                    } else {
                        composition <- get_composition()
                        psel <- prop %>% filter(antenna == sel)

                        rst <- create_p_raster(raster, psel, type = type, choices_prior, composition = composition, priorlist)
                    }
                }

                viz_p(cp = cp, rst = rst, var = type, trans = input$trans, pnames = pnames, offset = ifelse(input$offset, offset_value, 0), rect = rect)
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

