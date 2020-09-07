#' Calculate the estimated location of mobile devices
#'
#' Calculate the estimated location of mobile devices, i.e. the location posterior probabilities. This is the final step of the mobloc process (see \href{../doc/mobloc.html}{\code{vignette("mobloc")}}).
#'
#' @param prior prior object, the result of \code{\link{create_uniform_prior}}, \code{\link{create_prior}}, or \code{\link{create_network_prior}}
#' @param llh a likelihood object, e.g. the result of \code{\link{create_voronoi_llh}} and \code{\link{create_strength_llh}}
#' @param raster raster object that contains the raster tile index numbers (e.g. created with \code{\link{create_raster}})
#' @example ./examples/calculate_posterior.R
#' @seealso \href{../doc/mobloc.html}{\code{vignette("mobloc")}}
#' @export
calculate_posterior <- function(prior, llh, raster) {
    pag <- cell <- pga <- rid <- TA <- NULL

    x <- copy(llh)

    priordf <- prior_to_df(prior, raster)
    x <- priordf[x, on = "rid"]

    x[, pga:= pag * p]
    x <- x[, list(rid, pga = pga / sum(pga, na.rm = TRUE)), by = cell]


    x %>% attach_class("mobloc_post")
}


create_TA <- function(llh){
    llh[, TA:=dist %/% param$TA_step][
        TA <= param$TA_max, .(cell, TA, rid, dist, dBm, s, pag)] %>%
        attach_class("mobloc_llh")

}


#' Update posterior with Timing Advance
#'
#' Update posterior with Timing Advance. The posterior created with \code{\link{calculate_posterior}} will be used as a prior. The likelihood will be 1 if the grid call is contained in a specific TA band and 0 otherwise. When the parameter \code{TA_buffer > 0}, the TA band will be made broader. See the documentation of \code{\link{mobloc_param}} for details
#' @param post posterior created with \code{\link{calculate_posterior}}
#' @param raster raster object that contains the raster tile index numbers (e.g. created with \code{\link{create_raster}})
#' @param cp cellplan
#' @param param parameter list created with \code{mobloc_param}
#' @param elev elevation data. If specified, it is used to calculate the distance using the z coordinate as well.
#' @export
#' @seealso \code{\link{calculate_posterior}} for examples
update_posterior_TA <- function(post, raster, cp, param, elev = NULL) {
    if (!inherits(post, "mobloc_post")) stop("post is not a mobloc_post object")
    check_raster(raster)
    if (!is_cellplan_valid(cp)) stop("cp is not a valid cell plan")
    if (missing(param)) stop("param is missing")


    prior <- calculate_dist(post, cp = ZL_cellplan, raster = raster, elev)[, list(cell, rid, pg=pga, dist)]

    prior <- prior[, TA:=dist %/% param$TA_step][
        TA <= param$TA_max, .(cell, TA, rid, pg)]

    ids <- -param$TA_buffer:param$TA_buffer
    dt <- rbindlist(lapply(ids, function(id) {
        dt <- copy(prior)
        dt[, TA:= pmax(0, dt$TA + id)]
        dt
    }))

    x <- dt[, list(rid, pga = pg / sum(pg)), by = c("cell", "TA")]

    x %>% attach_class("mobloc_post")
}


#
#
# if (timing.advance) {
#     if (missing(param)) stop("param is missing")
#     ids <- -param$TA_buffer:param$TA_buffer
#
#     llh <- llh[, list(cell, TA, rid, pga = pag * p)]
#
#     llh2 <- rbindlist(lapply(ids, function(id) {
#         dt <- copy(llh)
#         dt[, TA:= pmax(0, dt$TA + id)]
#         dt
#     }))
#
#     x <- llh2[, list(rid, pga = pga / sum(pga)), by = c("cell", "TA")]
# } else {
#     llh[, pga:= pag * p]
#     x <- llh[, list(rid, pga = pga / sum(pga, na.rm = TRUE)), by = cell]
# }
