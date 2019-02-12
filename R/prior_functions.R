prior_filter <- function(prior, region) {
    name <- get_mobloc_prior(prior)
    r <- raster(x)
    r[] <- 1:length(r)
    land <- st_union(region)
    rids <- get_raster_ids(r, land)$rid

    r[] <- 0
    r[][rids] <- x[][rids]

    r[][rids] / sum(r[])
    set_mobloc_prior(y, name)
}

#' @rdname extract_prior
#' @export
create_uniform_prior <- function(raster) {
    if (missing(raster)) stop("Please specify a raster object")

    y <- raster::raster(raster)

    y[] <- 1/raster::ncell(raster)
    set_mobloc_prior(y, "uniform")
}

#' @rdname extract_prior
#' @export
create_network_prior <- function(prop, raster) {
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


create_coverage_map <- function(prop, raster) {
    z <- prop %>%
        group_by(rid) %>%
        summarize(dBm = max(dBm)) %>%
        ungroup()

    y <- raster::raster(raster)
    y[] <- 0
    y[][match(z$rid, raster[])] <- z$dBm
    y
}

create_coverage_map2 <- function(prop, raster) {
    z <- prop %>%
        group_by(rid) %>%
        summarize(s = sum(s)) %>%
        ungroup()

    y <- raster::raster(raster)
    y[] <- 0
    y[][match(z$rid, raster[])] <- z$s
    y
}

#' Extract prior information
#'
#' Extract prior information. The function \code{extract_prior} extracts the prior from a raster object. The function \code{extract_uniform_prior} extracts a uniform prior from the propagation model. The function \code{extract_network_prior} extracts a prior from the propagation model.
#' @rdname extract_prior
#' @name extract_prior
#' @param x a raster object with per raster cell a prior value.
#' @param prop a propagation object, which is the result of \code{\link{process_cellplan}}
#' @return data frame with two columns, raster id number \code{rid} and probability \code{p}. These probabilities will add up to 1.



create_prior <- function(..., name = "composite", weights = NULL) {
    args <- list(...)
    nms <- names(args)

    if (length(args) == 1) {
        a <- args[[1]]
        if (inherits(a, "RasterBrick")) {
            x <- combine_raster_layers(a, weights)
            if (!is.null(nms)) names(x) <- nms
        } else if (!inherits(a, "RasterLayer")) {
            stop("First object is not a RasterLayer nor a RasterBrick")
        } else {
            x <- a
        }
    } else {
        sapply(args, function(a){
            if (!inherits(a, "RasterLayer")) stop("Not all objects are RasterLayers nor RasterBricks")
        })
        x <- combine_raster_layers(do.call(brick, unname(args)), weights)
        if (!is.null(nms)) names(x) <- nms
    }
    x[][is.na(x[])] <- 0
    x[] <- x[] / sum(x[])
    set_mobloc_prior(x, name)
}

combine_raster_layers <- function(x, weights = NULL) {
    if (!inherits(x, "RasterBrick")) stop("x should be a raster brick")

    k <- raster::nlayers(x)

    if (is.null(weights)) {
        weights <- rep(1, k)
        message("weights set to c(", paste(rep(1, k), collapse = ", "), ").")
    } else {
        if (length(weights) != k) stop("The length of the comp does not correspond to ", k, ", the number of specified priors")
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




# check_mobloc_prior <- function(x) {
#     mb <- attr(x, "mobloc_prior")
#     if (is.null(mb)) {
#         if (abs(sum(x[]) - 1) < 1e-10) {
#             attr(y, "mobloc_prior") <- TRUE
#         } else {
#             stop("values of the raster object do not add up to 1")
#         }
#     }
# }


#
#
# prior_to_raster <- function(x, prior) {
#     x <- raster::raster(x)
#     x[][prior$rid] <- prior$p
#     x
# }

prior_to_df <- function(prior, raster) {
    tibble(rid = raster[],
           p = prior[]) %>%
        filter(!is.na(p))
}





attach_class <- function(x, cls) {
    class(x) <- c(class(x), cls)
    x
}


#' #' Create a composite prior
#' #'
#' #' Create a composite prior
#' #' @param priorlist list of priors (see \code{\link{extract_prior}})
#' #' @param comp composition of priors, a vector with fractions that should add up to 1
#' #' @return data frame with two columns, raster id number \code{rid} and probability \code{p}. These probabilities will add up to 1.
#' #' @export
#' composite_prior <- function(priorlist, comp = NULL) {
#'
#'     k <- length(priorlist)
#'
#'     if (k == 0) {
#'         stop("No priors specified.")
#'     } else if (k == 1) {
#'         return(priorlist[[1]])
#'     }
#'     if (missing(comp)) {
#'         comp <- rep(1/k, k)
#'         message("comp set to c(", paste(rep(round(comp, 3), k), collapse = ", "), ").")
#'     } else {
#'         if (length(comp) != k) stop("The length of the comp does not correspond to ", k, ", the number of specified priors")
#'     }
#'
#'     rids_list <- lapply(priorlist, function(prior) unique(prior$rid))
#'
#'     rids <- sort(unique(unlist(rids_list)))
#'
#'     m <- do.call(cbind, lapply(priorlist, function(prior) {
#'         prior$p[match(rids, prior$rid)]
#'     }))
#'
#'     tibble(rid = rids, p = (m %*% comp)[,1])
#' }

#' Calculate mobile location
#'
#' Calculate the mobile location given the calculated propagation and a prior.
#' @param prop propagation object, the result of \code{\link{process_cellplan}}
#' @param prior prior object, the result of \code{\link{extract_uniform_prior}}, \code{\link{extract_prior}}, or \code{\link{extract_network_prior}}
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


