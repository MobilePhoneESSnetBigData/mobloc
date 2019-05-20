#' Create a best server map and a coverage map.
#'
#' The function \code{create_coverage_map} to create a coverage map. It shows the best signal strength per raster tile (either in dBm or in signal dominance). The function \code{create_best_server_map} is used to create a best (area) server map. It shows the best cell per raster tile.
#'
#' @name create_coverage_map
#' @rdname create_coverage_map
#' @param prop a propagation object, which is the result of \code{\link{process_cellplan}}
#' @param raster raster object that contains the raster tile index numbers (e.g. created with \code{\link{create_raster}})
#' @param type either \code{"dBm"} for absolute signal strength values and \code{"s"} for signal dominance values
#' @param cells selection of cells
#' @seealso \href{../doc/mobloc.html}{\code{vignette("mobloc")}}
#' @export
create_coverage_map <- function(prop, raster, type = c("dBm", "s"), cells = NULL) {
    rid <- cell <- x <- NULL

    check_raster(raster)
    if (!missing(cells)) {
        prop <- prop %>% filter(cell %in% cells)
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

#' @name create_best_server_map
#' @rdname create_coverage_map
#' @export
create_best_server_map <- function(prop, raster, cells = NULL) {
    rid <- cell <- dBm <- NULL

    check_raster(raster)
    if (!missing(cells)) {
        rids <- unique(prop$rid[prop$cell %in% cells])
        prop <- prop %>% filter(rid %in% rids)
    }

    z <- prop %>%
        group_by(rid) %>%
        summarize(cell = cell[which.max(dBm)[1]])

    if (!missing(cells)) {
        z <- z %>% filter(cell %in% cells)
    }


    z <- z %>% mutate(cell = factor(cell))
    ants <- levels(z$cell)

    y <- raster::raster(raster)

    if (nrow(z) != 0) {
        y[][match(z$rid, raster[])] <- as.integer(z$cell)
        y <- raster::ratify(y)
        levels(y) <- list(data.frame(ID = 1L:length(ants), cell = ants))
        names(y) <- "cell"
    }

    y
}
