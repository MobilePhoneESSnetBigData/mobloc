#' Set and update propagation model parameters
#'
#' Set and update propagation model parameters. The function \code{prop_param} specifies model parameters, which can be updated with \code{update_prop_param}
#'
#' @name prop_param
#' @rdname prop_param
#' @param W_tower default power in Watt of a normal antennas (placed in a cell tower or rooftop site)
#' @param W_small default power in Watt of a small cell (omnidirectional)
#' @param azim_min3dB default horizontal beam width. At \code{azim_min3dB/2}, the signal strength is halved (so -3dBM)
#' @param azim_dB_back difference in signal strength between front and back
#' @param elev_min3dB default vertical beam width. At \code{elev_min3dB/2}, the signal strength is halved (so -3dBM)
#' @param elev_dB_back difference in signal strength between front and back
#' @param dBm_mid middle point in the logistic function to map signal strength to probability
#' @param dBm_width width of the logistic function to map signal strength to probability
#' @param poly_shape shape of the polygon that defines the coverage area of a cell. One of \code{"pie"}, \code{"oval"} (default), \code{"Voronoi"}.
#' @param max_range maximum range of normal antennas
#' @param max_range_small maximum range of small cells
#' @param min_range minimum range of normal antennas
#' @param min_range_small minimum range of small cells
#' @param height default height of normal antennas
#' @param height_small default height of small cells
#' @param tilt default (horizontal) tilt. Only applicable for directional antennas
#' @param beam_v default vertical beam width. Only applicable for directional antennas
#' @param beam_h default horizontal beam width. Only applicable for directional antennas
#' @param area_expension when \code{poly_shape} is \code{"pie"} or \code{"oval"}, the sizes of the polygons set to the area size of the corresponding Voronoi are multiplied by \code{area_expension}. These sizes are subsequently adjusted such that the range is bounded by \code{min_range} and \code{max_range} for normal cells and \code{min_range_small} and \code{max_range_small} for small cells. The purpose of \code{area_expension} is to allow overlap. This number (which should be greater than 1) contols the amout of overlap. The default value 1.5.
#' @param max_overlapping_cells maximum number of polygons that may overlap per raster cell. If the actual number exceeds this parameter, the \code{max_overlapping_cells} cells with the highest signal strength are selected
#' @return parameter list
#' @export
prop_param <- function(
    W_tower = 10,
    W_small = 5,
    ple = 3.25,
    ple_small = 6,
    azim_min3dB = 65,
    azim_dB_back = -30,
    elev_min3dB = 9,
    elev_dB_back = -30,
    dBm_mid = -92.5,
    dBm_width = 5,
    poly_shape = "oval",
    range = 20000,
    range_small = 5000,
    # max_range = 10000,
    # max_range_small = 100,
    # min_range = 400,
    # min_range_small = 100,
    height = 30,
    height_small = 8,
    tilt = 5,
    beam_v = 9,
    beam_h = 65,
    dBm_th = -100,
    #area_expension = 4,
    max_overlapping_cells = 20) {

    nms <- names(formals(prop_param))
    lst <- sapply(nms, get, envir=environment(), simplify = FALSE)
    class(lst) <- "prop_param"
    lst
}

#' @rdname prop_param
#' @param param parameter list created with \code{prop_param}
#' @param ... parameter updates, see arguments of \code{prop_param}
#' @export
update_prop_param <- function(param, ...) {
    args <- list(...)
    if (!all(names(args) %in% names(param))) stop("Unknown arguments")

    param[names(args)] <- args
    param
}
