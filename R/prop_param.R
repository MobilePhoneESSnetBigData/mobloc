#' Set the propagation model parameters
#'
#' Set the propagation model parameters. The result is a list.
#'
#' @name prop_param
#' @rdname prop_param
#' @param W default power in Watt of a normal antennas (placed in a cell tower or rooftop site)
#' @param W_small default power in Watt of a small cell (omnidirectional)
#' @param ple default path loss exponent
#' @param ple_small path loss exponent for small cells
#' @param ple_0 path loss exponent for free space
#' @param ple_1 path loss exponent for dense environments
#' @param dBm_mid middle point in the logistic function to map signal strength to probability
#' @param dBm_width width of the logistic function to map signal strength to probability
#' @param midpoint midpoint of the logistic function used to map signal strength to signal quality
#' @param steepness steepness of the logistic function used to map signal strength to signal quality
#' @param range maximum range of normal antennas
#' @param range_small maximum range of small cells
#' @param height default height of normal antennas
#' @param height_small default height of small cells
#' @param tilt default (horizontal) tilt. Only applicable for directional antennas
#' @param beam_v default vertical beam width. Only applicable for directional antennas
#' @param beam_h default horizontal beam width. Only applicable for directional antennas
#' @param azim_dB_back difference in signal strength between front and back
#' @param elev_dB_back difference in signal strength between front and back
#' @param sig_q_th signal quality threshold
#' @param max_overlapping_antennas maximum number of antennas that may overlap per raster tile. If the actual number exceeds this parameter, the \code{max_overlapping_antennas} cells with the highest signal strength are selected
#' @param TA_step meters that correspond to one Timing Advance (TA) step. This parameter depends on the network technology and psychical properties such as air pressure. In GSM networks it is approximately 554 meters \url{https://people.csail.mit.edu/bkph/cellular_repeater_TA.shtml}, and LTE (4G) networks 78.12 meters.\url{https://www.scribd.com/doc/290553975/Timing-Advance-TA-in-LTE}
#' @param TA_max maximum Timing Advance (TA) value (integer). In other words, TA can have a value between 0 and \code{TA_max}. In GSM it is 63, and in LTE 1282.
#' @param TA_buffer buffer to prevent artifacts in the TA to grid tile conversion. These artifacts occur when \code{TA_step} is similar or smaller than the width of a grid tile. \code{TA_buffer} is an integer that determines the number of TA steps that are added in front of behind the actual TA band.
#' @seealso \href{../doc/mobloc.html}{\code{vignette("mobloc")}}
#' @example ./examples/prop_param.R
#' @return parameter list
#' @export
prop_param <- function(
    W = 10,
    W_small = 5,
    ple = 3.75,
    ple_small = 6,
    ple_0 = 3.5,
    ple_1 = 4,
    dBm_mid = -92.5,
    dBm_width = 5,
    midpoint = -92.5,
    steepness = 1/5,
    range = 10000,
    range_small = 1000,
    height = 30,
    height_small = 8,
    tilt = 5,
    beam_v = 9,
    beam_h = 65,
    azim_dB_back = -30,
    elev_dB_back = -30,
    sig_q_th = 0.005,
    max_overlapping_antennas = 100,
    TA_step = 78.12,
    TA_max = 1282,
    TA_buffer = 1
    ) {

    nms <- names(formals(prop_param))
    lst <- sapply(nms, get, envir=environment(), simplify = FALSE)
    class(lst) <- "prop_param"
    lst
}
