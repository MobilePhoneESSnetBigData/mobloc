#' Create a best server map and a coverage map.
#'
#' The function \code{create_coverage_map} to create a coverage map. It shows the best signal strength per raster tile (either in dBm or in signal dominance). The function \code{create_best_server_map} is used to create a best (area) server map. It shows the best cell per raster tile.
#'
#' @name create_coverage_map
#' @rdname create_coverage_map
#' @param strength a signal strength model object, which is the result of \code{\link{compute_sig_strength}}
#' @param raster raster object that contains the raster tile index numbers (e.g. created with \code{\link{create_raster}})
#' @param type either \code{"dBm"} for absolute signal strength values and \code{"s"} for signal dominance values
#' @param llh a likelihood object, e.g. the result of \code{\link{create_voronoi_llh}} and \code{\link{create_strength_llh}}
#' @param cells selection of cells
#' @example ./examples/create_best_server_map.R
#' @seealso \href{../doc/mobloc.html}{\code{vignette("mobloc")}}
#' @export
create_coverage_map <- function(strength, raster, type = c("dBm", "s"), cells = NULL) {
    rid <- cell <- x <- . <- NULL

    check_raster(raster)
    if (!missing(cells)) {
        strength <- copy(strength[cell %in% cells])
    } else {
        strength <- copy(strength)
    }
    type <- match.arg(type)

    z <- strength[, x:=get(type)][, by = rid, .(x = max(x))]

    y <- raster::raster(raster)
    rids <- raster[]
    zsel <- z[rid %in% rids]

    y[][match(zsel$rid, raster[])] <- zsel$x
    names(y) <- type
    y
}

#' @name create_best_server_map
#' @rdname create_coverage_map
#' @export
create_best_server_map <- function(llh, raster, cells = NULL) {
    rid <- cell <- pag <- NULL

    check_raster(raster)

    if (!missing(cells)) {
        rids <- unique(llh$rid[llh$cell %in% cells])
        llh <- copy(llh[rid %in% rids])
    } else {
        llh <- copy(llh)
    }

    z <- llh[, cell:= cell[which.max(pag)[1]], by = rid]

    if (!missing(cells)) {
        z <- z[cell %in% cells]
    }

    #z <- z[, cell:= factor(cell)]
    ants <- levels(z$cell)

    y <- raster::raster(raster)

    rids <- raster[]
    zsel <- z[rid %in% rids]


    if (nrow(zsel) != 0) {
        y[][match(zsel$rid, raster[])] <- as.integer(zsel$cell)
        y <- raster::ratify(y)
        suppressWarnings({levels(y) <- list(data.frame(ID = 1L:length(ants), cell = ants))})
        names(y) <- "cell"
    }

    y
}
