#' Extract prior information
#'
#' Extract prior information. The function \code{extract_prior} extracts the prior from a raster object. The function \code{extract_uniform_prior} extracts a uniform prior from the propagation model. The function \code{extract_network_prior} extracts a prior from the propagation model.
#' @rdname extract_prior
#' @name extract_prior
#' @param x a raster object with per raster cell a prior value.
#' @param prop a propagation object, which is the result of \code{\link{process_cellplan}}
#' @return data frame with two columns, raster id number \code{rid} and probability \code{p}. These probabilities will add up to 1.
extract_prior <- function(x, prop) {
    xv <- raster::getValues(x)

    rv <- 1L:length(x)
    rva <- unique(prop$rid)

    df <- tibble(rid = rva, p = xv[rva])

    df$p <- df$p / sum(df$p)
    df
}


#' @rdname extract_prior
#' @export
extract_network_prior <- function(prop) {
    totals <- sum(prop$s)
    prop %>%
        group_by(rid) %>%
        summarize(p = sum(s) / totals) %>%
        ungroup()
}

#' @rdname extract_prior
#' @export
extract_uniform_prior <- function(prop) {
    rva <- unique(prop$rid)
    nrva <- length(rva)
    tibble(rid = rva, p = 1/nrva)
}

#' Create a composite prior
#'
#' Create a composite prior
#' @param priorlist list of priors (see \code{\link{extract_prior}})
#' @param comp composition of priors, a vector with fractions that should add up to 1
#' @return data frame with two columns, raster id number \code{rid} and probability \code{p}. These probabilities will add up to 1.
#' @export
composite_prior <- function(priorlist, comp = NULL) {

    k <- length(priorlist)

    if (k == 0) {
        stop("No priors specified.")
    } else if (k == 1) {
        return(priorlist[[1]])
    }
    if (missing(comp)) {
        comp <- rep(1/k, k)
        message("comp set to c(", paste(rep(round(comp, 3), k), collapse = ", "), ").")
    } else {
        if (length(comp) != k) stop("The length of the comp does not correspond to ", k, ", the number of specified priors")
    }

    rids_list <- lapply(priorlist, function(prior) unique(prior$rid))

    rids <- sort(unique(unlist(rids_list)))

    m <- do.call(cbind, lapply(priorlist, function(prior) {
        prior$p[match(rids, prior$rid)]
    }))

    tibble(rid = rids, p = (m %*% comp)[,1])
}

#' Calculate mobile location
#'
#' Calculate the mobile location given the calculated propagation and a prior.
#' @param prop propagation object, the result of \code{\link{process_cellplan}}
#' @param prior prior object, the result of \code{\link{extract_uniform_prior}}, \code{\link{extract_prior}}, or \code{\link{extract_network_prior}}
#' @export
calculate_mobloc <- function(prop, prior) {
    prop %>%
        left_join(prior, by = 'rid') %>%
        mutate(pga = pag * p) %>%
        group_by(antenna) %>%
        mutate(pga = pga / sum(pga)) %>%
        ungroup() %>%
        select(antenna, rid, pga)
}

combine_raster_layers <- function(x, comp = NULL) {
    if (!inherits(x, "RasterBrick")) stop("x should be a raster brick")

    k <- raster::nlayers(x)

    if (missing(comp)) {
        comp <- rep(1, k)
        message("comp set to c(", paste(rep(1, k), collapse = ", "), ").")
    } else {
        if (length(comp) != k) stop("The length of the comp does not correspond to ", k, ", the number of specified priors")
    }

    a <- x[] %*% comp
    r <- raster(x)
    r[] <- a
    r
}
