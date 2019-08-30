#' Explore the visualize propagation, prior, connection likelihood and location posterior probabilities per raster tile
#'
#' Explore the visualize propagation, prior, connection likelihood and location posterior probabilities per raster tile. When the raster is large (say larger than 30 by 30 kilometers), we recommend to specify the filter arguemnt.
#'
#' @param cp cellplan, validated with \code{\link{validate_cellplan}}
#' @param raster raster object that contains the raster tile index numbers (e.g. created with \code{\link{create_raster}})
#' @param prop a propagation object, which is the result of \code{\link{process_cellplan}}
#' @param priorlist list of priors
#' @param param parameter list created with \code{prop_param}
#' @param filter bounding box of the filter of the visualized raster. If not specified, the whole raster is shown, which could be very slow. Therefore, we recommand to use a filter when the raster covers a large area (say 30 by 30 kilometers).
#' @param coverage_map_dBm coverage map, created with \code{\link{create_coverage_map}} (with \code{type = "dBm"}). If not specified, it will be created (which takes some time).
#' @param coverage_map_s coverage map, created with \code{\link{create_coverage_map}} (with \code{type = "s"}). If not specified, it will be created (which takes some time).
#' @param best_server_map best server map, created with \code{\link{create_best_server_map}}. If not specified, it will be created (which takes some time).
#' @note Note that duo to the reprojection of the raster to the web mercator projection (for interactive maps), the visualized raster does not correspond exactly to the output raster.
#' @import shiny
#' @importFrom shinyjs useShinyjs disable
#' @import leaflet
#' @importFrom graphics plot.new xspline
#' @example ./examples/explore_mobloc.R
#' @seealso \href{../doc/mobloc.html}{\code{vignette("mobloc")}}
#' @export
explore_mobloc <- function(cp, raster, strength, priorlist, llhlist, param, filter = NULL, coverage_map_dBm = NULL, coverage_map_s = NULL, best_server_map = NULL) {

    crs <- st_crs(raster)


    epsg <- 4326
    # Note: background maps are available for EPSG 3035, but raster images do not work (see https://github.com/rstudio/leaflet/issues/610)
#     epsg <- if(crs$proj4string == "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")  {
#         3035
#     } else {
#         4326
#     }



    if (!missing(filter)) {

        raster <- mobloc_crop_raster(raster, bbx = filter)

        a <- local({
            cells <- do.call(c, lapply(llhlist, function(llh) llh$cell))
            rids <- do.call(c, lapply(llhlist, function(llh) llh$rid))
            unique(cells[rids %in% raster[]])
        })
        cp <- mobloc_filter_cell(cp, a)

        priorlist <- lapply(priorlist, mobloc_crop_raster, bbx = filter)
        llhlist <- lapply(llhlist, mobloc_filter_cell, a = a)
    }


    rect <- create_bbx_rect(raster2bbx(raster)) %>% st_transform(crs = 4326)



    cell <- NULL

    pnames <- c(names(priorlist), "composite")
    nprior <- length(pnames)
    choices_prior <- paste0("p", 1L:nprior)
    names(choices_prior) <- paste0("Prior ", pnames)
    names(pnames) <- choices_prior

    lnames <- names(llhlist)
    nllh <- length(lnames)
    choices_llh <- paste0("l", 1L:nllh)
    names(choices_llh) <- paste0("Likelihood ", lnames)
    names(lnames) <- choices_llh


    choices <- c("Signal strength - dBm" = "dBm",
                 "Signal dominance - s" = "s",
                 "Best server map" = "bsm",
                 "Location prior - P(g)" = "pg",
                 "Connection likelihood - P(a|g)" = "pag",
                 "Location posterior - P(g|a)" = "pga")


    #https://stackoverflow.com/questions/34733147/unable-to-disable-a-shiny-app-radio-button-using-shinyjs

    cells <- as.character(cp$cell)
    #names(cells) <- paste("Cell", 1L:n)


    if (missing(coverage_map_dBm)) {
        message("Creating coverage maps (dBm)...")
        coverage_map_dBm <- create_coverage_map(strength, raster, type = "dBm") # cm_dBm
    }
    if (missing(coverage_map_s)) {
        message("Creating coverage maps (s)...")
        coverage_map_s <- create_coverage_map(strength, raster, type = "s") #cm_s
    }
    if (missing(best_server_map)) {
        message("Creating best server maps...")
        best_server_maps <- lapply(llhlist, create_best_server_map, raster = raster)
        #best_server_map <- create_best_server_map(strength, raster) #bsm
    }

    offset_value <- 150


    sliders <- mapply(function(i, nm) {
        if (i == choices_prior[nprior - 1]) {
            shiny::htmlOutput("plast")
        } else {
            sliderInput(i, paste("Faction", nm), min = 0, max = 1, value = 1/nprior, step  = 0.01)
        }

    }, choices_prior[-nprior], pnames[-nprior], SIMPLIFY = FALSE)

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
                                 radioButtons("show", "Selection",  c("All cells" = "grid", "Single cell" = "ant"), selected = "grid"),
                                 radioButtons("varP", "Location Prior", choices_prior, selected = "p1"),
                                 conditionalPanel(condition = paste0("(input.varP == 'p", nprior, "')"),
                                                  wellPanel(sliders)),
                                 radioButtons("varL", "Connection Likelihood", choices_llh, selected = "l1"),
                                 radioButtons("var", "Show", choices, selected = "dBm"),
                                 conditionalPanel(
                                     condition = "input.var == 'pga'",
                                     checkboxInput("TA", "Enable Timing Advance", value = FALSE),
                                     conditionalPanel(
                                         condition = "input.TA",
                                         sliderInput("TAvalue", "Timing Advance", min = 0, max = param$TA_max, value = 0, step = 1),
                                         shiny::htmlOutput("TAband")
                                         )),
                                 sliderInput("trans", "Transparency", min = 0, max = 1, value = 1, step = 0.1),
                                 checkboxInput("offset", "Cell offset", value = TRUE)),
                        tabPanel("Cell data",
                                 selectInput("sel", "Cell", cells, selected = cells[1]),
                                 dataTableOutput("cellinfo"))
                    )),
                mainPanel(
                    leafletOutput("map", height=1000)
                ))
        ),
        server = function(input, output, session) {
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
                varP <- input$varP
                if (varP != paste0("p", nprior)) {
                    composition <- rep(0, nprior-1)
                    composition[as.integer(substr(varP, 2, nchar(varP)))] <- 1
                    attr(composition, "showW") <- FALSE
                } else {
                    values <- sapply(choices_prior[1:(nprior-2)], function(x) {
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
                }
                composition
            })

            get_prior <- reactive({
                composition <- get_composition()
                do.call(create_prior, c(unname(priorlist), list(name = "composite", weights = composition)))
            })


            get_llh <- reactive({
                varL <- input$varL
                llhlist[[as.integer(substr(varL, 2, nchar(varL)))]]
            })

            get_bsm <- reactive({
                varL <- input$varL
                best_server_maps[[as.integer(substr(varL, 2, nchar(varL)))]]
            })


            output$plast <- renderUI({
                composition <- get_composition()
                showW <- attr(composition, "showW")
                HTML(paste0("<b>Faction ", pnames[nprior], ": ", round(composition[nprior], 2), ifelse(showW, " (warning: the sum of slider values is greater than 1)", ""),  "</b>"))
            })

            output$TAband <- renderUI({
                TA  <- input$TAvalue
                TA_buffer <- param$TA_buffer
                TA_step <- param$TA_step

                TA_min <- TA * TA_step
                TA_max <- (TA+1) * TA_step


                TA_min_band <- (max(0, TA-TA_buffer)) * TA_step
                TA_max_band <- (TA+TA_buffer+1) * TA_step

                if (TA_buffer > 0) {
                    HTML(paste0("Timing Advance band: [", fN(TA_min_band), ", ", fN(TA_max_band), "] m, without buffer: [", fN(TA_min), ", ", fN(TA_max), "] m"))
                } else {
                    HTML(paste0("Timing Advance band: [", fN(TA_min), ", ", fN(TA_max), "] m"))
                }

                #HTML(paste0("<b>Faction ", pnames[nprior], ": ", round(composition[nprior], 2), ifelse(showW, " (warning: the sum of slider values is greater than 1)", ""),  "</b>"))
            })



            output$map <- renderLeaflet({
                base_map(cp, offset_value, epsg)
            })


            output$cellinfo <- renderDataTable({
                cpant <- as.list(cp[cp$cell == input$sel, ] %>% st_set_geometry(NULL))
                cpant$x <- sprintf("%.2f", cpant$x)
                cpant$y <- sprintf("%.2f", cpant$y)
                cpant$z <- sprintf("%.2f", cpant$z)
                cpant$ple <- sprintf("%.2f", cpant$ple)
                data.frame(Variable = names(cpant), Value = unname(unlist(cpant)))
            }, options = list(searching = FALSE, scrollx = FALSE, paging = FALSE, info = FALSE))

            observe({
                type <- get_var()
                sel <- input$sel
                ta <- if (input$TA) input$TAvalue else NA
                cp$sel <- 1L
                cp$sel[cp$cell %in% sel] <- 2L

                if (type %in% c("bsm", "pag", "pga")) llh <- get_llh() else llh <- NULL
                if (type == "bsm") bsm <- get_bsm() else bsm <- NULL
                if (type %in% c("pg", "pga")) prior <- get_prior() else prior <- NULL

                if (input$show == "grid") {
                    rst <- create_q_raster(raster, type = type, prior = prior, coverage_map_dBm, coverage_map_s, bsm)
                } else {
                    if (type == "bsm") {
                        rst <- create_best_server_map(strength, raster, cells = sel)
                    } else {
                        strength_psel <- strength[cell == sel]
                        rst <- create_p_raster(raster, strength_psel, type = type, choices_prior, composition = composition, priorlist, ta, param)
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


create_q_raster <- function(rst, type, prior, cm_dBm, cm_s, bsm) {
    #rindex <- raster::getValues(rst)
    #r <- raster::raster(rst)

    if (type == "dBm") {
        r <- cm_dBm
    } else if (type == "s") {
        r <- cm_s
    } else if (type == "bsm") {
        r <- bsm
    } else if (type == "pg") {
        #composition <- c(priormix[1], (priormix[2] - priormix[1]), (1 - priormix[2]))
        r <- prior
    }


    raster::trim(r)
}

create_p_raster <- function(rst, ppr, type, choices_prior, composition, priorlist, ta, param) {
    dBm <- s <- pag <- pg <- pga <- TA <- NULL

    rindex <- raster::getValues(rst)
    r <- raster::raster(rst)


    # if (!is.na(ta)) {
    #     ppr <- ppr %>%
    #         filter(TA == ta)
    # }

    # if (nrow(ppr) == 0) {
    #     return(r)
    # }


    if (type == "dBm") {
        ppr <- ppr[, x:= dBm]
    } else if (type == "s") {
        ppr <- ppr[, x:=s]
    } else if (type %in% choices_prior) {
        priordf <- prior_to_df(priorlist[[as.integer(substr(type, 2, 2))]], rst)
        ppr <- ppr[, x:= priordf$p[match(ppr$rid, priordf$rid)]]
    } else if (type == "pag") {
        ppr <- ppr[, x := pag]
    } else {
        #composition <- c(priormix[1], (priormix[2] - priormix[1]), (1 - priormix[2]))

        priordf <- prior_to_df(do.call(create_prior, c(unname(priorlist), list(name = "composite", weights = composition))), rst)
        ppr <- ppr[, pg:= priordf$p[match(ppr$rid, priordf$rid)]]

        if (type == "pg") {
            ppr <- ppr[, x:= pg]
        } else {
            setnames(ppr, "pg", "p")
            ppr <- calculate_mobloc(ppr, timing.advance = !is.na(ta), param = param)
            setnames(ppr, "pga", "x")

            if (!is.na(ta)) {
                ppr <- ppr[TA == ta]
            }

            if (nrow(ppr) == 0) {
                return(r)
            }

            # ppr <- ppr %>%
            #     mutate(x = pag * pg) %>%
            #     mutate(x = x / sum(x))

        }
    }

#
#     if (type != "dBm") {
#         ppr <- ppr %>%
#             mutate(x = x / sum(x) * 100)
#     }

    raster::values(r)[match(ppr$rid, rindex)] <- ppr$x
    r <- raster::trim(r)
    #r[r==0] <- NA
    r
}


fN <- function(x) {
    formatC(x, big.mark = ",", format = "f", digits = 0)
}



