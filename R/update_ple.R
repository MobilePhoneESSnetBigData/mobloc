#' Update path loss exponent values
#'
#' Update path loss exponent values. This function estimates the path loss exponent using a raster object that contains information about the environment. Buildings and trees are notorious propapation blockers. Therefore, this raster object should contain an indicator for the amount of buildings, trees and other obstacles per grid tile. This function is called from \code{\link{validate_cellplan}}.
#'
#' Method: the values of \code{envir} is taken at a couple of points near the cell. The mean value is computed, and linearly transformed from the range [0, 1] to the range [\code{ple_0}, \code{ple_1}]. The method to select the sample point is the following. For omnidirectional cells, points are taken at 0, 90, 180 and 270 degrees. For directional cells, points are taken at the propatation direction plus -1, -.5, -.25, 0, .25, .5, and 1 times the horizontal beam width. For each direction, points are taken at 50, 150, 250, 500, and 1000 meter distance.
#'
#' @param cp cellplan, validated with \code{\link{validate_cellplan}}
#' @param envir raster object that contains per grid tile an indicator of the objects that block the propagation, e.g. buildings and trees.
#' @param ple_0 lowest path loss exponent value. This value is mapped to \code{envir} raster tile values of 0.
#' @param ple_1 highest loss exponent value. This value is mapped to \code{envir} raster tile values of 1.
#' @param ple_small the path loss exponent of small cells. If \code{NA} (default), the small cells are considered as normal cells. Otherwise, this value will be used for path loss exponent values of small cells.
#' @return cellplan (\code{data.frame}) with imputed values for the variable \code{ple}
#' @example ./examples/update_ple.R
#' @export
update_ple <- function(cp, envir, ple_0 = 2.5, ple_1 = 4, ple_small = NA) {
    if (!is_cellplan_valid(cp)) stop("cp is not a validated cellplan. Please validate it with validate_cellplan.")

    x <- sample_envir_points(cp, envir)

    ple_rng <- ple_1 - ple_0

    values <- (x * ple_rng) + ple_0


    if (!is.na(ple_small)) {
        cp$ple <- ifelse(cp$small, ple_small, values)
    } else {
        cp$ple <- values
    }
    cp
}

sample_envir_points <- function(cp, envir, omnidir_angles = c(0, 90, 180, 270), dir_angles_mply = c(-1, -.5, -.25, 0, .25, .5, 1), radius = c(50, 150, 250, 500, 1000)) {
    rd <- NULL

    angles <- mapply(function(dir, bh) {
        res <- if (is.na(dir)) {
            omnidir_angles
        } else {
            dir + dir_angles_mply * bh
        }
        res <- ifelse(res < 0, res + 360, ifelse(res > 360, res - 360, res))
    }, cp$direction, cp$beam_h, SIMPLIFY = FALSE)

    mapply(function(x, y, a, rng, ant) {
        rs <- radius[radius < rng]

        df <- expand.grid(a = a, rd = rs, ant = ant, x = x, y = y)

        df$x <- df$x + SIN(df$a) * df$rd
        df$y <- df$y + COS(df$a) * df$rd

        df <- st_as_sf(df, coords = c("x", "y"), crs = st_crs(cp))
        x <- raster::extract(envir, df)
        mean(x, na.rm = TRUE)
    }, cp$x, cp$y, angles, cp$range, cp$cell, SIMPLIFY = TRUE)
}
