#' Calculate mobile location
#'
#' Calculate the mobile location given the calculated propagation and a prior.
#' @param prop propagation object, the result of \code{\link{process_cellplan}}
#' @param prior prior object, the result of \code{\link{create_uniform_prior}}, \code{\link{create_prior}}, or \code{\link{create_network_prior}}
#' @param raster raster raster object
#' @export
calculate_mobloc <- function(prop, prior, raster) {
    pag <- antenna <- pga <- rid <- NULL

    check_raster(raster)
    priordf <- prior_to_df(prior, raster)
    prop %>%
        left_join(priordf, by = 'rid') %>%
        mutate(pga = pag * p) %>%
        group_by(antenna) %>%
        mutate(pga = pga / sum(pga)) %>%
        ungroup() %>%
        select(antenna, rid, pga)
}
