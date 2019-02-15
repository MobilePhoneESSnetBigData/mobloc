#' Create prior
#'
#' Functions to create a prior. A prior is a raster layer where all values add up to 1. The function \code{create_prior} creates a prior using one or more raster layers. The function \code{create_uniform_prior} extracts a uniform prior from the propagation model. The function \code{create_network_prior} extracts a prior from the propagation model. The function \code{prior_filter} can be used to filter a prior raster using a polygons object.
#' @rdname create_prior
#' @name create_prior
#' @param ... one of more raster objects, or a list of it. When multiple raster layers are used, these are first combined using a weighed sum (see argument \code{weights} and the underlying function \code{\link{combine_raster_layers}}). After that, these values are normalised such that the values add up to 1
#' @param weights The weights of the raster objects (see \code{...}). See also the underlying function \code{\link{combine_raster_layers}}.
#' @param raster a raster object that contains the raster numbers (e.g. created with \code{\link{create_raster}})
#' @param region a polygons object (class \code{sf}) that specifies a region
#' @param prop a propagation object, which is the result of \code{\link{process_cellplan}}
#' @param name name of the prior
#' @param prior object created with \code{create_prior}, \code{create_network_prior} or \code{create_uniform_prior}
#' @return data frame with two columns, raster id number \code{rid} and probability \code{p}. These probabilities will add up to 1.
create_prior <- function(..., name = "composite", weights = NULL) {
    x <- combine_raster_layers(..., weights = weights)

    x[][is.na(x[])] <- 0
    x[] <- x[] / sum(x[])
    set_mobloc_prior(x, name)
}


#' @rdname create_prior
#' @export
create_uniform_prior <- function(raster) {
    if (missing(raster)) stop("Please specify a raster object")

    check_raster(raster)

    y <- raster::raster(raster)

    y[] <- 1/raster::ncell(raster)
    set_mobloc_prior(y, "uniform")
}

#' @rdname create_prior
#' @export
create_network_prior <- function(prop, raster) {
    check_raster(raster)
    totals <- sum(prop$s)
    z <- prop %>%
        group_by(rid) %>%
        summarize(p = sum(s) / totals) %>%
        ungroup()

    y <- raster::raster(raster)
    y[] <- 0
    y[][match(z$rid, raster[])] <- z$p
    set_mobloc_prior(y, "network")
}


#' @rdname create_prior
#' @export
prior_filter <- function(prior, region) {
    name <- get_mobloc_prior(prior)
    r <- raster(prior)
    r[] <- 1:length(r)

    rids <- get_raster_ids(r, region)$rid

    r[] <- 0
    r[][rids] <- prior[][rids]

    r[][rids] <- r[][rids] / sum(r[])
    set_mobloc_prior(r, name)
}

#' Functions to estimate a coverage map and a best server map.
#'
#' The function \code{create_coverage_map} is used to create a coverage map, and the function \code{create_best_server_map} to create a best (area) server map. The former shows the best signal strength per raster cell (either in dBm or in signal quality). The latter shows the best antenna per raster cell.
#'
#' @param prop propagation model
#' @param raster raster raster object
#' @param type either \code{"dBm"} for absolute signal strength values and \code{"s"} for signal quality values
#' @param antennas selection of antennas
#' @export
create_coverage_map <- function(prop, raster, type = c("dBm", "s"), antennas = NULL) {
    check_raster(raster)
    if (!missing(antennas)) {
        prop <- prop %>% filter(antenna %in% antennas)
    }
    type <- match.arg(type)
    z <- prop %>%
        group_by(rid) %>%
        rename_("x" = type) %>%
        summarize(x = max(x)) %>%
        ungroup()

    y <- raster::raster(raster)
    y[][match(z$rid, raster[])] <- z$x
    names(y) <- type
    y
}

create_best_server_map <- function(prop, raster, antennas = NULL) {
    check_raster(raster)
    if (!missing(antennas)) {
        rids <- unique(prop$rid[prop$antenna %in% antennas])
        prop <- prop %>% filter(rid %in% rids)
    }

    z <- prop %>%
        group_by(rid) %>%
        summarize(antenna = antenna[which.max(dBm)[1]])

    if (!missing(antennas)) {
        z <- z %>% filter(antenna %in% antennas)
    }


    z <- z %>% mutate(antenna = factor(antenna))
    ants <- levels(z$antenna)

    y <- raster::raster(raster)
    y[][match(z$rid, raster[])] <- as.integer(z$antenna)
    y <- raster::ratify(y)
    levels(y) <- list(data.frame(ID = 1L:length(ants), antenna = ants))
    names(y) <- "antenna"
    y
}


#' Combine raster layers to a single raster layer
#'
#' Combine raster layers to a single raster layer. Weights can be assigned to the raster layers.
#'
#' @param ... Either one raster brick/stack, or multiple raster layer objects
#' @param weights numeric vector that specifies the weights of the raster layers
#' @export
combine_raster_layers <- function(..., weights = NULL) {
    args <- list(...)
    if (length(args) == 1 && is.list(args[[1]])) args <- args[[1]]

    sapply(args, function(a) {
        if (!inherits(a, "Raster")) stop("Not all arguments are raster objects")
    })

    if (inherits(args[[1]], "RasterBrick")) {
        x <- args[[1]]
    } else {
        tryCatch({
            x <- do.call(brick, args)
        }, error = function(e) {
            stop("Could not convert the raster object(s) to a raster brick")
        })
    }

    k <- raster::nlayers(x)

    if (is.null(weights)) {
        weights <- rep(1, k)
        message("weights set to c(", paste(rep(1, k), collapse = ", "), ").")
    } else {
        if (length(weights) != k) stop("The length of weights does not correspond to ", k, ", the number of specified layers")
    }

    a <- x[] %*% weights
    r <- raster(x)
    r[] <- a
    r
}


get_mobloc_prior <- function(x) {
    if (is.null(attr(x, "mobloc_prior"))) stop("Invalid prior object")
    names(x)
}

set_mobloc_prior <- function(x, name) {
    names(x) <- name
    attr(x, "mobloc_prior") <- TRUE
    x
}

prior_to_df <- function(prior, raster) {
    tibble(rid = raster[],
           p = prior[]) %>%
        filter(!is.na(p))
}

attach_class <- function(x, cls) {
    class(x) <- c(class(x), cls)
    x
}




#' Calculate mobile location
#'
#' Calculate the mobile location given the calculated propagation and a prior.
#' @param prop propagation object, the result of \code{\link{process_cellplan}}
#' @param prior prior object, the result of \code{\link{create_uniform_prior}}, \code{\link{create_prior}}, or \code{\link{create_network_prior}}
#' @export
calculate_mobloc <- function(prop, prior) {
    priordf <- prior_to_df(prior)
    prop %>%
        left_join(prior, by = 'rid') %>%
        mutate(pga = pag * p) %>%
        group_by(antenna) %>%
        mutate(pga = pga / sum(pga)) %>%
        ungroup() %>%
        select(antenna, rid, pga)
}


