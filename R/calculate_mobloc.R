#' Calculate mobile location
#'
#' Calculate the (estimated) mobile device location given the modelled propagation and a prior.
#' @param prop a propagation object, which is the result of \code{\link{process_cellplan}}
#' @param prior prior object, the result of \code{\link{create_uniform_prior}}, \code{\link{create_prior}}, or \code{\link{create_network_prior}}
#' @param raster raster object that contains the raster tile index numbers (e.g. created with \code{\link{create_raster}})
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
