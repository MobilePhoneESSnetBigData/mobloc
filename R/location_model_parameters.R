#' Set and update location model parameters
#'
#' Set and update location model parameters. The function \code{location_model_parameters} specifies model parameters, which can be updated with \code{update_model_parameters}
#'
#' @name location_model_parameters
#' @rdname location_model_parameters
#' @param db0_tower signal strength in dBm near a large cell (placed in a cell tower or rooftop site)
#' @param db0_small signal strength in dBm near a small cell (omnidirectional)
#' @param azim_min3dB default horizontal beam width. At \code{azim_min3dB/2}, the signal strength is halved (so -3dBM)
#' @param azim_dB_back difference in signal strength between front and back
#' @param elev_min3dB default vertical beam width. At \code{elev_min3dB/2}, the signal strength is halved (so -3dBM)
#' @param elev_dB_back difference in signal strength between front and back
#' @param db_mid middle point in the logistic function to map signal strength to probability
#' @param db_width width of the logistic function to map signal strength to probability
#' @param poly_shape shape of the polygon that defines the coverage area of a cell. One of \code{"pie"}, \code{"oval"} (default), \code{"Voronoi"}.
#' @param max_range maximum range of large cells
#' @param max_range_small maximum range of small cells
#' @param area_expension when \code{poly_shape} is \code{"pie"} or \code{"plectrum"}, the size is determined by the corresponding Voronoi size and multiplied by \code{area_expension} to allow overlap.
#' @param max_overlapping_cells maximum number of polygons that may overlap per raster cell. If the actual number exceeds this parameter, the \code{max_overlapping_cells} cells with the highest signal strength are selected
#' @return parameter list
#' @export
location_model_parameters <- function(
    db0_tower = -45,
    db0_small = -60,
    azim_min3dB = 65,
    azim_dB_back = -30,
    elev_min3dB = 9,
    elev_dB_back = -30,
    db_mid = -92.5,
    db_width = 5,
    poly_shape = "oval",
    max_range = 10000,
    max_range_small = 100,
    area_expension = 4,
    max_overlapping_cells = 20) {

    nms <- names(formals(location_model_parameters))
    lst <- sapply(nms, get, envir=environment(), simplify = FALSE)
    class(lst) <- "location_model_parameters"
    lst
}

#' @rdname location_model_parameters
#' @param param parameter list created with \code{location_model_parameters}
#' @param ... parameter updates, see arguments of \code{location_model_parameters}
#' @export
update_model_parameters <- function(param, ...) {
    args <- list(...)
    if (!all(names(args) %in% names(param))) stop("Unknown arguments")

    param[names(args)] <- args
    param
}
