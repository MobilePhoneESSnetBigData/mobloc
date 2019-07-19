#' Calculate the estimated location of mobile devices
#'
#' Calculate the estimated location of mobile devices, i.e. the location posterior probabilities. This is the final step of the mobloc process (see \href{../doc/mobloc.html}{\code{vignette("mobloc")}}).
#'
#' @param prop a propagation object, which is the result of \code{\link{process_cellplan}}
#' @param prior prior object, the result of \code{\link{create_uniform_prior}}, \code{\link{create_prior}}, or \code{\link{create_network_prior}}
#' @param raster raster object that contains the raster tile index numbers (e.g. created with \code{\link{create_raster}})
#' @param timing.advance logical that determines whether timing advance is enabled. If \code{TRUE}, the location posterior probabilities will add up to 1 for each cell TA combination (with the condition that there is signal within the TA band). Note that when the parameter \code{TA_buffer > 0}, the TA band will be made broader. See the documentation of \code{\link{prop_param}} for details.
#' @param param parameter list created with \code{prop_param}
#' @example ./examples/calculate_mobloc.R
#' @seealso \href{../doc/mobloc.html}{\code{vignette("mobloc")}}
#' @export
calculate_mobloc <- function(prop, prior = NULL, raster = NULL, timing.advance = FALSE, param = NULL) {
    pag <- cell <- pga <- rid <- TA <- NULL

    prop <- copy(prop)

    if (!missing(prior) && !missing(raster)) {
        check_raster(raster)
        priordf <- prior_to_df(prior, raster)


        prop <- priordf[prop, on = "rid"]
    } else {
        if (!("p" %in% names(prop))) stop("prior/raster not specified, and variable 'p' is missing in the data.frame prop.")
    }

    if (timing.advance) {
        if (missing(param)) stop("param is missing")
        ids <- -param$TA_buffer:param$TA_buffer

        prop <- prop[, list(cell, TA, rid, pga = pag * p)]

        prop2 <- rbindlist(lapply(ids, function(id) {
            dt <- copy(prop)
            dt[, TA:= pmax(0, dt$TA + id)]
            dt
        }))

        prop2[, list(rid, pga = pga / sum(pga)), by = c("cell", "TA")]
    } else {
        prop[, pga:= pag * p]
        prop[, list(rid, pga = pga / sum(pga, na.rm = TRUE)), by = cell]
    }

}
