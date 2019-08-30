#' Calculate the estimated location of mobile devices
#'
#' Calculate the estimated location of mobile devices, i.e. the location posterior probabilities. This is the final step of the mobloc process (see \href{../doc/mobloc.html}{\code{vignette("mobloc")}}).
#'
#' @param strength a propagation object, which is the result of \code{\link{process_cellplan}}
#' @param prior prior object, the result of \code{\link{create_uniform_prior}}, \code{\link{create_prior}}, or \code{\link{create_network_prior}}
#' @param raster raster object that contains the raster tile index numbers (e.g. created with \code{\link{create_raster}})
#' @param timing.advance logical that determines whether timing advance is enabled. If \code{TRUE}, the location posterior probabilities will add up to 1 for each cell TA combination (with the condition that there is signal within the TA band). Note that when the parameter \code{TA_buffer > 0}, the TA band will be made broader. See the documentation of \code{\link{prop_param}} for details.
#' @param param parameter list created with \code{prop_param}
#' @example ./examples/calculate_mobloc.R
#' @seealso \href{../doc/mobloc.html}{\code{vignette("mobloc")}}
#' @export
calculate_mobloc <- function(strength, prior = NULL, raster = NULL, timing.advance = FALSE, param = NULL) {
    pag <- cell <- pga <- rid <- TA <- NULL

    strength <- copy(strength)

    if (!missing(prior) && !missing(raster)) {
        check_raster(raster)
        priordf <- prior_to_df(prior, raster)


        strength <- priordf[strength, on = "rid"]
    } else {
        if (!("p" %in% names(strength))) stop("prior/raster not specified, and variable 'p' is missing in the data.frame strength.")
    }

    if (timing.advance) {
        if (missing(param)) stop("param is missing")
        ids <- -param$TA_buffer:param$TA_buffer

        strength <- strength[, list(cell, TA, rid, pga = pag * p)]

        strength2 <- rbindlist(lapply(ids, function(id) {
            dt <- copy(strength)
            dt[, TA:= pmax(0, dt$TA + id)]
            dt
        }))

        strength2[, list(rid, pga = pga / sum(pga)), by = c("cell", "TA")]
    } else {
        strength[, pga:= pag * p]
        strength[, list(rid, pga = pga / sum(pga, na.rm = TRUE)), by = cell]
    }

}
