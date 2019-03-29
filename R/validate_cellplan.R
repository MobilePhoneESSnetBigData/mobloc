check_cp_var <- function(x, small, param_small, param_normal, name, fix) {
    if (is.null(x)) {
        if (!fix) stop("Variable '", name, "' is missing. Set fix = TRUE to impute the values.")
        message("Variable '", name, "' is missing. These have been imputed with the parameters '", name, "' and '", name, "_small' for normal and small cells respectively." )
        x <- ifelse(small, param_small, param_normal)

    } else {
        isna <- is.na(x)
        if (any(isna)) {
            if (!fix) stop("Variable '", name, "' contains missing values. Set fix = TRUE to impute them.")
            message("Variable '", name, "' contains missing values. These have been imputed with the parameters '", name, "' and '", name, "_small' for normal and small cells respectively." )
            x[isna] <- ifelse(small[isna], param_small, param_normal)
        }
    }
    x
}

#' Validate cellplan
#'
#' Validate the cellplan. The cellplan is made valid whenever possible. It checks the format and whether are all required variables present. When \code{land}, a mutlipolygon object that defines the region of interest, is specified, it checks whether all antennas are inside this region.
#'
#' @param cp cellplan \code{sf} object containing the antenna data. Each data record should be a point (i.e., \code{st_geometry_type(cp)} should return \code{POINT}s). The optional variables are:
#' \itemize{
#' \item \code{height}. Height of the antenna. If omitted, it will be derived from variable \code{z} if specified and otherwise, the default value \code{height} from the parameter list \code{param} will be used.
#' \item \code{z}. Note that \code{z = elevation + height}. Elevation is taken from the argument \code{elevation}. If this is not specified, it will be assumed 0.
#' \item \code{direction}. Direction of the antanna in degrees. Use \code{NA} for omnidirectional antennas.
#' \item \code{tilt}. Tilt of the antennas in degrees. Only applicable for directional antennas If omitted, the default value \code{tilt} from the parameter list \code{param} will be used.
#' \item \code{beam_h}. Horizontal beam width in degrees. The signal loss at \code{-beam_h/2} and \code{+beam_h/2} degrees is 3 dB. Run \code{radiation_plot(beam_width = 65, db_back = -30)}. If omitted, the default value \code{beam_h} from the parameter list \code{param} will be used.
#' \item \code{beam_v}. Vertical beam width in degrees. The signal loss at \code{-beam_v/2} and \code{+beam_v/2} degrees is 3 dB. Run \code{radiation_plot(type = "e", beam_width = 9, db_back = -30)}. If omitted, the default value \code{beam_v} from the parameter list \code{param} will be used.
#' \item \code{W}. Power of the antenna in Watt.
#' \item \code{small}. Logical value that determines whether the antenna is a 'small cell'. If omitted, it will be set to \code{FALSE}. In the \code{mobloc} package, small cells have different default values for a couple of parameters (i.e. the \code{"_small"} parameters in \code{param}).
#' \item \code{range}. The maximum range of the antenna. If omitted, the value \code{max_range} from the parameter list \code{param} will be used. If \code{small} is defined, the value \code{max_range_small} is used for each antenna for which \code{small == TRUE}.
#' \item \code{ple}. The path loss exponent, which is an approximation of the level of reflection: 2 is open field, 4 is urban area, 6 is inside buildings. It can be derived when \code{envir} is specific.
#' }
#' @param param parameter list created with \code{prop_param}
#' @param region polygon shape. If specified, it checks if all antennas are contained inside it
#' @param envir raster layer that specifies the environment. For each raster tile, an indicator between 0 and 1 is required (open space is 0 and dense environment is 1). These values are scaled to the parameters \code{ple_0} and \code{ple_1}.
#' @param elevation see argument \code{cp} (variable \code{z})
#' @param fix should the cellplan that is not yet valid be made valid? If \code{FALSE}, only errors, warnings, and messages regarding the validation will be returned. If \code{TRUE}, the cellplan will be returned with a validation stamp (specifically, the attribute \code{valid_cellplan} is set to code{TRUE})
#' @import sf
#' @import sp
#' @import dplyr
#' @importFrom methods as
#' @example ./examples/validate_cellplan.R
#' @seealso \href{../doc/mobloc.html}{\code{vignette("mobloc")}}
#' @export
validate_cellplan <- function(cp, param, elevation=NULL, region=NULL, envir = NULL, fix = TRUE) {
    if (!inherits(cp, "sf") || !(all(st_geometry_type(cp) == "POINT"))) stop("cp should be an sf object of points")

    nms <- names(cp)

    coor <- st_coordinates(cp)

    if (!"x" %in% nms) {
        if (!fix) stop("The variable 'x' is missing. Set fix = TRUE to fix this issue.")
        cp$x <- unname(coor[,1])
    }
    if (!"y" %in% nms) {
        if (!fix) stop("The variable 'y' is missing. Set fix = TRUE to fix this issue.")
        cp$y <- unname(coor[,2])
    }

    if ("small" %in% names(cp)) {
        if (!fix && !is.logical(cp$small)) stop("The variable 'small' should be a logical. Set fix = TRUE to fix this issue.")
        if (is.numeric(cp$small)) cp$small <- as.logical(cp$small)
        if (!is.logical(cp$small)) stop("The variable 'small' should be a logical")
        if (any(is.na(cp$small))) {
            if (!fix) stop("The variable 'small' contains missing values. Set fix = TRUE to fix this issue")
            message("The variable 'small' contains missing values. These have been imputed with FALSE")
            cp$small[is.na(cp$small)] <- FALSE
        }
    } else {
        if (!fix) stop("The variable 'small' is missing. Set fix = TRUE to fix this issue.")
        message("The variable 'small' is missing. Therefore, all antennas are assumed to be normal")
        cp$small <- FALSE
    }


    if (!missing(elevation)) {
        cpsp <- as(cp, "Spatial")
        cp$elev <- as.vector(extract(elevation, cpsp))

        if (all(c("z", "height") %in% nms)) {
            if (!all(cp$z == cp$elev + cp$height)) {
                stop("Inconsistencies found. ")
            }
        }

    } else {
        if (all(c("z", "height") %in% nms)) {
            cp$elev <- cp$z - cp$height
            if (any(cp$elev != 0)) stop("elevation not specified. Since the values for 'z' and 'height' are not always the same, the elevation is not always 0. Please specify elevation in order to check the elevation, or ignore the elevation data and set z = height.")
        } else {
            message("elevation not specified, therefore, the elevation is assumed to be 0. In other words, 'z' will be set to 'height' (since z = height + elevation)")
            cp$elev <- 0
        }
    }

    #if (missing(elevation)) stop("Variable 'z' is missing. Please add this variable or specify the argument 'elevation' (since z = height + elevation).")



    if (!("height" %in% nms)) {
        if (!fix) stop("Variable 'height' not found. Set fix = TRUE to fix this issue.")
        if ("z" %in% nms) {
            cp$height <- cp$z - cp$elev
        } else {
            cp$height <- check_cp_var(cp$height, cp$small, param$height_small, param$height, "height", fix)
        }
    } else {
        cp$height <- check_cp_var(cp$height, cp$small, param$height_small, param$height, "height", fix)
    }

    if (!("z" %in% nms)) {
        if (!fix) {
            stop("Variable 'z' not found. Set fix = TRUE to fix this issue.")
        } else {
            cp$z <- cp$elev + cp$height
        }
    }

    cp$elev <- NULL


    if (!"direction" %in% nms) {
        if (!fix) stop("The variable 'direction' is missing. Set fix = TRUE to fix this issue.")
        message("The variable 'direction' is missing. All antennas are assumed to be omni-directional.")
        cp$direction <- NA
    } else {
        if (max(cp$direction, na.rm = TRUE) < 2 * pi) message("Probably, direction are in radials. Please check manually, and if needed, provide them in degrees")
    }

    cp$W <- check_cp_var(cp$W, cp$small, param$W_small, param$W, "W", fix)


    if (any(cp$small) && (any(!is.na(cp$direction[cp$small])) ||
        any(!is.na(cp$tilt[cp$small])) ||
        any(!is.na(cp$beam_h[cp$small])) ||
        any(!is.na(cp$beam_v[cp$small])))) {

        if (!fix) stop("The variable 'direction', 'tilt', 'beam_h' and/or 'beam_v' are missing for some antennas. Set fix = TRUE to fix this issue.")

        message("Some small cells have non-missing values for 'direction', 'tilt', 'beam_h' and/or 'beam_v'. They are set to NA, since small cells are modeled as omnidirectional")
        cp$direction[cp$small] <- NA
        cp$tilt[cp$small] <- NA
        cp$beam_h[cp$small] <- NA
        cp$beam_v[cp$small] <- NA
    }



    if (!all(c("direction", "tilt", "beam_h", "beam_v") %in% nms)) {
        if (!fix) stop("The variables 'direction', 'tilt', 'beam_h' and/or 'beam_v' are missing. Set fix = TRUE to fix this issue.")
        message("Variables 'direction', 'tilt', 'beam_h', and/or 'beam_v' are missing. Therefore, directional component of the signal strenth model will not be used.")
        cp$direction <- NA
        cp$tilt <- NA
        cp$beam_h <- NA
        cp$beam_v <- NA
    } else {
        na_tilt <- is.na(cp$tilt) & !is.na(cp$direction)
        if (any(na_tilt)) {
            if (!fix) stop("Tilt of ", sum(na_tilt), " directional antennas is missing. Set fix = TRUE to fix this issue.")
            message("Tilt of ", sum(na_tilt), " directional antennas is missing. They are imputed with ", param$tilt)
            cp$tilt[na_tilt] <- param$tilt
        }

        na_beam_v <- is.na(cp$beam_v) & !is.na(cp$direction)
        if (any(na_beam_v)) {
            if (!fix) stop("beam_v of ", sum(na_beam_v), " directional antennas is missing. Set fix = TRUE to fix this issue.")
            message("beam_v of ", sum(na_beam_v), " directional antennas is missing. They are imputed with ", param$beam_v)
            cp$beam_v[na_beam_v] <- param$beam_v
        }

        na_beam_h <- is.na(cp$beam_h) & !is.na(cp$direction)
        if (any(na_beam_h)) {
            if (!fix) stop("beam_h of ", sum(na_beam_h), " directional antennas is missing. Set fix = TRUE to fix this issue.")
            message("beam_h of ", sum(na_beam_h), " directional antennas is missing. They are imputed with ", param$beam_h)
            cp$beam_h[na_beam_h] <- param$beam_h
        }

    }

    cp$range <- check_cp_var(cp$range, cp$small, param$range_small, param$range, "range", fix)

    if (!missing(envir)) {
        if (!fix) stop("The variable 'ple' is missing. Set fix = TRUE to fix this issue.")
        attr(cp, "valid_cellplan") <- TRUE
        cp <- update_ple(cp, envir, param$ple_0, param$ple_1, param$ple_small)
        message("Path loss exponent ('ple') values updated with envir object")
    } else {
        cp$ple <- check_cp_var(cp$ple, cp$small, param$ple_small, param$ple, "ple", fix)
    }

    if (!missing(region)) {
        sel <- which_inside(cp, region)
        if (any(!sel)) {
            if (!fix) stop("some antennas are not inside land: ", paste(which(!sel), collapse = ", "), "Set fix = TRUE to fix this issue.")
            message("some antennas are not inside land: ", paste(which(!sel), collapse = ", "), "These are omitted.")
        }
        cp <- cp[sel, ]
    }

    if (fix) {
        message("The cellplan has been made valid")
    } else{
        message("The cellplan is valid")
    }

    attr(cp, "valid_cellplan") <- TRUE
    cp
}

which_inside <- function(points, poly) {
    it <- sapply(st_intersects(points, poly), length)
    it>=1L
}


is_cellplan_valid <- function(cp) {
    identical(attr(cp, "valid_cellplan"), TRUE)
}


