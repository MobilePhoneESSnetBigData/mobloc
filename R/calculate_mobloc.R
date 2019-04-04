#' Calculate the estimated location of mobile devices
#'
#' Calculate the estimated location of mobile devices, i.e. the location posterior probabilities. This is the final step of the mobloc process (see \href{../doc/mobloc.html}{\code{vignette("mobloc")}}).
#'
#' @param prop a propagation object, which is the result of \code{\link{process_cellplan}}
#' @param prior prior object, the result of \code{\link{create_uniform_prior}}, \code{\link{create_prior}}, or \code{\link{create_network_prior}}
#' @param raster raster object that contains the raster tile index numbers (e.g. created with \code{\link{create_raster}})
#' @param timing.advance logical that determines whether timing advance is enabled. If \code{TRUE}, the location posterior probabilities will add up to 1 for each antenna TA combination (with the condition that there is signal within the TA band).
#' @param param parameter list created with \code{prop_param}
#' @example ./examples/calculate_mobloc.R
#' @seealso \href{../doc/mobloc.html}{\code{vignette("mobloc")}}
#' @export
calculate_mobloc <- function(prop, prior = NULL, raster = NULL, timing.advance = FALSE, param = NULL) {
    pag <- antenna <- pga <- rid <- NULL

    if (!missing(prior) && !missing(raster)) {
        check_raster(raster)
        priordf <- prior_to_df(prior, raster)

        prop <- prop %>%
            left_join(priordf, by = 'rid')
    } else {
        if (!("p" %in% names(prop))) stop("prior/raster not specified, and variable 'p' is missing in the data.frame prop.")
    }

    if (timing.advance) {
        if (missing(param)) stop("param is missing")
        ids <- -param$TA_buffer:param$TA_buffer

        prop2 <- prop %>%
            #left_join(priordf, by = 'rid') %>%
            mutate(pga = pag * p) %>%
            select(antenna, TA, rid, pga)


        prop3 <- bind_rows(lapply(ids, function(id) {
             prop2$TA <- pmax(0, prop2$TA + id)
             prop2
        }))

        prop3 %>%
            group_by(antenna, TA) %>%
            mutate(pga = pga / sum(pga)) %>%
            ungroup() %>%
            select(antenna, TA, rid, pga)

    } else {
        prop %>%
            #left_join(priordf, by = 'rid') %>%
            mutate(pga = pag * p) %>%
            group_by(antenna) %>%
            mutate(pga = pga / sum(pga)) %>%
            ungroup() %>%
            select(antenna, rid, pga)
    }

}
