#' Create a best server map and a coverage map.
#'
#' The function \code{create_best_server_map} to create a best (area) server map. It shows the best signal strength per raster tile (either in dBm or in signal quality). The function \code{create_coverage_map} is used to create a coverage map. It shows the best antenna per raster tile.
#'
#' @name create_coverage_map
#' @rdname create_coverage_map
#' @param prop a propagation object, which is the result of \code{\link{process_cellplan}}
#' @param raster raster object that contains the raster tile index numbers (e.g. created with \code{\link{create_raster}})
#' @param type either \code{"dBm"} for absolute signal strength values and \code{"s"} for signal quality values
#' @param antennas selection of antennas
#' @export
create_coverage_map <- function(prop, raster, type = c("dBm", "s"), antennas = NULL) {
    rid <- antenna <- x <- NULL

    check_raster(raster)
    if (!missing(antennas)) {
        prop <- prop %>% filter(antenna %in% antennas)
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
create_best_server_map <- function(prop, raster, antennas = NULL) {
    rid <- antenna <- dBm <- NULL

    check_raster(raster)
    if (!missing(antennas)) {
        rids <- unique(prop$rid[prop$antenna %in% antennas])
        prop <- prop %>% filter(rid %in% rids)
    }

    z <- prop %>%
        group_by(rid) %>%
        summarize(antenna = antenna[which.max(dBm)[1]])

    if (!missing(antennas)) {
        z <- z %>% filter(antenna %in% antennas)
    }


    z <- z %>% mutate(antenna = factor(antenna))
    ants <- levels(z$antenna)

    y <- raster::raster(raster)

    if (nrow(z) != 0) {
        y[][match(z$rid, raster[])] <- as.integer(z$antenna)
        y <- raster::ratify(y)
        levels(y) <- list(data.frame(ID = 1L:length(ants), antenna = ants))
        names(y) <- "antenna"
    }

    y
}
