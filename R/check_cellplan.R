#' Check cellplan
#'
#' Function to the check cellplan, i.e. the format and whether are all required variables present.
#'
#' @param cp cellplan \code{sf} object containing the antenna data. Each data record should be a point (i.e., `st_geometry_type(cp)` should return `POINT`s). The variables (of which only the first is required) are used:
#' \itemize{
#' \item \code{z} or \code{height} (required). Both indicate the height of the antenna, but \code{z} is including and \code{height} is excluding the elevation. If only \code{height} is present, also the argument \code{elevation} is required.
#' \item \code{direction}. Direction of the antanna in degrees. Use \code{NA} for omnidirectional antennas.
#' \item \code{tilt}. Tilt of the antennas in degrees. Only applicable for directional cells. If omitted, the default value \code{tilt} from the parameter list \code{param} will be used.
#' \item \code{beam_h}. Horizontal beam width in degrees. The signal loss at \code{-beam_h/2} and \code{+beam_h/2} degrees is 3 dB. Run \code{radiation_plot(beam_width = 65, db_back = -30)}. If omitted, the default value \code{beam_h} from the parameter list \code{param} will be used.
#' \item \code{beam_v}. Vertical beam width in degrees. The signal loss at \code{-beam_v/2} and \code{+beam_v/2} degrees is 3 dB. Run \code{radiation_plot(type = "e", beam_width = 9, db_back = -30)}. If omitted, the default value \code{beam_v} from the parameter list \code{param} will be used.
#' \item \code{small}. Logical value that determines whether the antenna is a 'small cell'.
#' \item \code{range}. The maximum range of the antenna. If omitted, the value \code{max_range} from the parameter list \code{param} will be used. If \code{small} is defined, the value \code{max_range_small} is used for each antenna for which \code{small == TRUE}.
#' }
#' Required variables:
#' @import sf
#' @import sp
check_cellplan <- function(cp, param, elevation=NULL) {
    if (!inherits(cp, "sf") || !(all(st_geometry_type(cp) == "POINT"))) stop("cp should be an sf object of points", call. = FALSE)

    nms <- names(cp)

    coor <- st_coordinates(cp)

    if (!"x" %in% nms) cp$x <- unname(coor[,1])
    if (!"y" %in% nms) cp$y <- unname(coor[,2])

    if ("small" %in% names(cp)) {
        if (is.numeric(cp$small)) cp$small <- as.logical(cp$small)
        warning("small is missing. Therefore, all antennas are assumed to be normal", call. = FALSE)
    } else {
        cp$small <- FALSE
    }


    if (!any(c("height", "z") %in% nms)) {
        warning("Neither 'height' nor 'z' were found. Therefore, the height of small cell antennas is set to ", param$height_small, " and of other antennas to ", param$height)
        cp$height <- ifelse(cp$small, param$height_small, param$height)
    }


    if (!"z" %in% names(cp)) {
        if (missing(elevation)) stop("Variable 'z' is missing. Please add this variable or specify the argument 'elevation' (since z = height + elevation).", call. = FALSE)

        cpsp <- as(cp, "Spatial")
        #cpsp <- set_projection(cpsp, current.projection = st_crs(crs)$proj4string)
        cp$z <- as.vector(extract(elevation, cpsp))

    }

    if (!"direction" %in% nms) {
        warning("The variable 'direction' is missing. All antennas are assumed to be omni-directional.", call. = FALSE)
        cp$direction <- NA
    } else {
        if (max(cp$direction, na.rm = TRUE) < 2 * pi) warning("Probably, direction are in radials. Please provide them in degrees")
    }

    if (!"range" %in% nms) {
        warning("'range' is missing. Therefore, the range of small antennas are set to the parameter max_range_small (", param$max_range_small, ") and the range of other antennas to max_range (", param$max_range, ").", call. = FALSE)
        cp$range <- ifelse(cp$small, param$max_range_small, param$max_range)
    }


    if (!"db0" %in% nms) {
        warning("'db0' is missing. Therefore, the db0 of small antennas are set to the parameter db0_small (", param$db0_small, ") and the db0 of other antennas to db0_tower (", param$db0_tower, ").", call. = FALSE)
        cp$db0 <- ifelse(cp$small, param$db0_small, param$db0_tower)
    }




    if (any(!is.na(cp$direction[cp$small])) ||
        any(!is.na(cp$tilt[cp$small])) ||
        any(!is.na(cp$beam_h[cp$small])) ||
        any(!is.na(cp$beam_v[cp$small]))) {

        warning("some small cells have non-missing values for direction, tilt, beam_h and beam_v. They are set to NA, since small cells are modeled as omnidirectional", call. = FALSE)
        cp$direction[cp$small] <- NA
        cp$tilt[cp$small] <- NA
        cp$beam_h[cp$small] <- NA
        cp$beam_v[cp$small] <- NA
    }



    if (!all(c("direction", "tilt", "beam_h", "beam_v") %in% nms)) {
        warning("Variables 'direction', 'tilt', 'beam_h', and/or 'beam_v' are missing. Therefore, directional component of the signal strenth model will not be used.", call. = FALSE)
        cp$direction <- NA
        cp$tilt <- NA
        cp$beam_h <- NA
        cp$beam_v <- NA
    } else {
        na_tilt <- is.na(cp$tilt) & !is.na(cp$direction)
        if (any(na_tilt)) {
            warning("Tilt of ", sum(na_tilt), " directional antennas is missing. They are imputed with ", param$tilt)
            cp$tilt[na_tilt] <- param$tilt
        }

        na_beam_v <- is.na(cp$beam_v) & !is.na(cp$direction)
        if (any(na_beam_v)) {
            warning("beam_v of ", sum(na_beam_v), " directional antennas is missing. They are imputed with ", param$beam_v)
            cp$beam_v[na_beam_v] <- param$beam_v
        }

        na_beam_h <- is.na(cp$beam_h) & !is.na(cp$direction)
        if (any(na_beam_h)) {
            warning("beam_h of ", sum(na_beam_h), " directional antennas is missing. They are imputed with ", param$beam_h)
            cp$beam_h[na_beam_h] <- param$beam_h
        }

    }




    attr(cp, "cellplan_checked") <- TRUE
    cp
}
