#' Set and update propagation model parameters
#'
#' Set and update propagation model parameters. The function \code{prop_param} specifies model parameters, which can be updated with \code{update_prop_param}
#'
#' @name prop_param
#' @rdname prop_param
#' @param W default power in Watt of a normal antennas (placed in a cell tower or rooftop site)
#' @param W_small default power in Watt of a small cell (omnidirectional)
#' @param ple default path loss exponent
#' @param ple_small path loss exponent for small cells
#' @param ple_0 path loss exponent for free space
#' @param ple_1 path loss exponent for dense environments
#' @param azim_min3dB default horizontal beam width. At \code{azim_min3dB/2}, the signal strength is halved (so -3dBM)
#' @param azim_dB_back difference in signal strength between front and back
#' @param elev_min3dB default vertical beam width. At \code{elev_min3dB/2}, the signal strength is halved (so -3dBM)
#' @param elev_dB_back difference in signal strength between front and back
#' @param dBm_mid middle point in the logistic function to map signal strength to probability
#' @param dBm_width width of the logistic function to map signal strength to probability
#' @param range maximum range of normal antennas
#' @param range_small maximum range of small cells
#' @param height default height of normal antennas
#' @param height_small default height of small cells
#' @param tilt default (horizontal) tilt. Only applicable for directional antennas
#' @param beam_v default vertical beam width. Only applicable for directional antennas
#' @param beam_h default horizontal beam width. Only applicable for directional antennas
#' @param dBm_th dBm threshold
#' @param max_overlapping_cells maximum number of polygons that may overlap per raster cell. If the actual number exceeds this parameter, the \code{max_overlapping_cells} cells with the highest signal strength are selected
#' @return parameter list
#' @export
prop_param <- function(
    W = 10,
    W_small = 5,
    ple = 3.75,
    ple_small = 6,
    ple_0 = 3.5,
    ple_1 = 4,
    azim_min3dB = 65,
    azim_dB_back = -30,
    elev_min3dB = 9,
    elev_dB_back = -30,
    dBm_mid = -92.5,
    dBm_width = 5,
    range = 10000,
    range_small = 1000,
    height = 30,
    height_small = 8,
    tilt = 5,
    beam_v = 9,
    beam_h = 65,
    dBm_th = -120,
    max_overlapping_cells = 100) {

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
