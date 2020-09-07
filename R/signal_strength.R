# library(geosphere)
# distGeo(c(5.970648, 50.895076), c(5.968393, 50.893516))
# measure_distance <- 235
# measure_db <- -100
#
# db0 <-  10*log10(measure_distance^2) + measure_db (~50)
# zie ook https://powerfulsignal.com/cell-signal-strength/
# distance2dB <- function(r) {
#
# }
# https://www.cisco.com/c/en/us/support/docs/wireless-mobility/wireless-lan-wlan/82068-omni-vs-direct.html
# https://community.arubanetworks.com/t5/tkb/articleprintpage/tkb-id/ControllerBasedWLANs/article-id/521
# https://en.wikipedia.org/wiki/E-plane_and_H-plane
# https://electronics.stackexchange.com/questions/299889/how-much-power-is-radiated-by-cell-towers
# https://www.repeaterstore.com/pages/femtocell-and-microcell

r2d <- function(x) x * 180 / pi
d2r <- function(x) x / 180 * pi

COS <- function(x) cos(d2r(x))
SIN <- function(x) sin(d2r(x))
TAN <- function(x) tan(d2r(x))

ACOS <- function(x) r2d(acos(x))
ASIN <- function(x) r2d(asin(x))
ATAN <- function(x) r2d(atan(x))
ATAN2 <- function(y, x) r2d(atan2(y, x))


# https://www.rapidtables.com/electric/dBW.html

#' Converter functions
#'
#' Converter functions between W, dBW, and dBm.
#'
#' @param W watt
#' @param dBW decibelwatt
#' @param dBm decibel-milliwatts, a unit of level used to indicate that a power ratio is expressed in decibels (dB) with reference to one milliwatt (mW)
#' @export
#' @rdname dBW2dBm
dBW2dBm <- function(dBW) {
    dBW + 30
}

#' @export
#' @rdname dBW2dBm
dBW2W <- function(dBW) {
  10^(dBW / 10)
}

#' @export
#' @rdname dBW2dBm
W2dBW <- function(W) {
    10 * log10(W)
}

#' @export
#' @rdname dBW2dBm
W2dBm <- function(W) {
  dBW2dBm(W2dBW(W))
}

#' @export
#' @rdname dBW2dBm
dBm2dBW <- function(dBm) {
  dBm - 30
}

#' @export
#' @rdname dBW2dBm
dBm2W <- function(dBm) {
  dBW2W(dBm - 30)
}




normalize_angle <- function(a) {
  a <- abs(a) %% 360
  a[a>180] <- 360 - a[a>180]
  a
}

#' Signal strength model functions
#'
#' Signal strength model functions. \code{norm_dBloss} approximates the dB loss at a certain direction using normal distributions. \code{distance2dB} approximates the signal strength in dB as a function of distance, given the power of the cell and the path loss exponent. \code{db2s} transforms dBm to signal dominance (s) using a sigmoid function.
#'
#' @param a angle of direction (in degrees)
#' @param db_back difference in signal strength between front and back (in dB)
#' @param sd standard deviation of the normal distribution that is used to model the radiation pattern, such that at 180 degrees, the dB loss is db_back and at beam_width degrees the dB loss is 3dB.
#' @param beam_width beam width
#' @param r distance
#' @param ple path loss exponent
#' @param W watt
#' @param dBm dBm to convert
#' @param midpoint which dBm value corresponds to the mid point of the distribution (0.5)
#' @param steepness steepness of the fitted curve
#' @export
#' @rdname norm_dBloss
norm_dBloss <- function(a, db_back = -30, sd = 90, beam_width = NULL) {
  if (!is.null(beam_width)) sd <- find_sd(beam_width = beam_width, db_back = db_back)
  a <- normalize_angle(a)
  inflate <- -db_back / (dnorm(0, 0, sd) - dnorm(180, 0, sd))
  (dnorm(a, mean = 0, sd = sd) - dnorm(0, 0, sd)) * inflate
}

#' @export
#' @rdname norm_dBloss
distance2dB <- function(r, ple, W) {
  #-75 - 10 * log10(r/1)

  db0 <- W2dBm(W)

  db0 - ple * 10 * log10(r)
  #db0 - .05 * r
}


get_min3db <- function(sd, db_back) {
  df <- data.frame(a = seq(0, 180, length.out = 720))
  df$dbLoss <- norm_dBloss(df$a, db_back = db_back, sd = sd)
  df$a[which.min(abs(-3 - df$dbLoss))]
}

create_mapping <- function(db_back) {
  idf <- data.frame(sd = seq(180/1000, 180, by = 180/1000))
  idf$deg <- sapply(idf$sd, get_min3db, db_back = db_back)

  df <- data.frame(deg = 1:180)

  df$sd <- sapply(df$deg, function(dg) {
    idf$sd[which.min(abs(idf$deg - dg))]
  })
  df
}

find_sd <- function(beam_width, db_back = NULL, mapping = NULL) {
  if (is.null(mapping)) {
    stopifnot(!is.null(db_back))
    mapping <- create_mapping(db_back)
  }
  mapping$sd[which.min(abs(mapping$deg - beam_width/2))]
}

attach_mapping <- function(param) {
  param$azim_mapping <- create_mapping(param$azim_dB_back)
  param$elev_mapping <- create_mapping(param$elev_dB_back)
  param
}


# angle2dBloss <- function(a, min3dB=65/180*pi, pmin3dB=.95) {
#
#     sd_x <- qnorm(.5 + pmin3dB / 2)
#
#     sd <- (min3dB / 2) / sd_x
#
#     dens_max <- dnorm(0, 0, sd)
#     dens_3db <- dnorm((min3dB/2), 0, sd)
#
#     inflate <- 3 / (dens_max - dens_3db)
#
#     (dnorm(a, 0, sd) - dens_max) * inflate
# }


#' @export
#' @rdname norm_dBloss
db2s <- function(dBm, midpoint, steepness) {
    scale <- (dBm - midpoint) * steepness
    1 / (1 + exp(1)^(-scale))
}

# bs <- -100:100
# es <- project_to_e_plane(bs, 40, 40/180*pi)
# plot(bs, es)

project_to_e_plane <- function(b, c, beta) {
  if (length(c)!=length(b)) c <- rep(c, length.out = length(b))
  if (length(beta)!=length(b)) beta <- rep(beta, length.out = length(b))

  d <- sqrt(b^2+c^2)
  lambda <- ATAN2(c, abs(b))
  d <- sqrt(b^2 + c^2)

  cases <- ifelse(b > 0, # in front of cell?
                  ifelse(beta < lambda, 1, 2), # below elevation plane?
                  ifelse(lambda + beta < 90, 3, 4)) # projected point in front of cell (=rare case)

  e <- rep(0, length.out = length(b))

  e[cases == 1] <- COS(lambda[cases==1] - beta[cases==1]) * d[cases==1]
  e[cases == 2] <- COS(beta[cases == 2] - lambda[cases == 2]) * d[cases == 2]
  e[cases == 3] <- -COS(lambda[cases==3] + beta[cases==3]) * d[cases==3]
  e[cases == 4] <- COS(180 - lambda[cases == 4] - beta[cases == 4]) * d[cases == 4]
  attr(e, "cases") <- cases
  e
}



#' Signal strength
#'
#' Low-level function to calculate signal strength. Used by compute_sig_strength.
#'
#' @param cx The x-coordinate of the source
#' @param cy The y-coordinate of the source
#' @param cz The z-coordinate of the source
#' @param direction The propagation direction
#' @param tilt The propagation tilt
#' @param beam_h The propagation horizontal beam width
#' @param beam_v The propagation vertical beam width
#' @param W The power (Watt) of the source
#' @param co The coordinates and id numbers of the tiles for which the signal strength should be calculated. Should be a data.frame with the variables \code{x}, \code{y}, \code{z}, \code{rid}
#' @param ple Path loss exponent
#' @param param mobloc parameters
#' @param enable character vector that determines which components are enabled for the calculation of signal strength. More specifically, these components determine whether to include signal loss as a function of distance (\code{"d"}), horizontal deviation (\code{"h"}) and vertical deviation (\code{"v"}) of the main direction.
#' @export
signal_strength <- function(cx, cy, cz, direction, tilt, beam_h, beam_v, W, co, ple, param, enable = c("d", "h", "v")) {

    # tilt was assumed to be negative?
    # tilt is now positive

    r <- sqrt((co$x - cx)^2 + (co$y - cy)^2 + (co$z - cz)^2)
    rxy <- sqrt((co$x - cx)^2 + (co$y - cy)^2)
    #rbeta <- dbeta(r/param$r_max, param$shape_1, param$shape_2) + param$const

    if ("d" %in% enable) {
        dBm <- distance2dB(pmax(r, 0.01), ple, W)
    } else{
        dBm <- rep(param$midpoint + 1/param$steepness, length(r))
    }

    if ("h" %in% enable && !any(is.na(direction)) && !any(is.na(beam_h))) {
        if (!"azim_mapping" %in% names(param)) param <- attach_mapping(param)
        # calculate horizontal angle w.r.t. main direction
        theta_azim <- (90 - ATAN2(co$y-cy, co$x-cx)) # * 180 / pi
        theta_azim[theta_azim < 0] <- theta_azim[theta_azim < 0] + 360
        azim <- (theta_azim - direction) %% 360
        azim[azim > 180] <- azim[azim > 180] - 360
        azim[azim < -180] <- azim[azim < -180] + 360

        # project azim to elevation plane -> azim2
        a <- SIN(azim) * rxy
        b <- COS(azim) * rxy

        e <- project_to_e_plane(b, cz - co$z, tilt)
        azim2 <- ATAN2(a, e)

        sd <- find_sd(beam_width = beam_h, db_back = param$azim_dB_back, mapping = param$azim_mapping) #param$azim_min3dB
        dBm <- dBm + norm_dBloss(azim2, db_back = param$azim_dB_back, sd = sd)
    }

    if ("v" %in% enable && !any(is.na(tilt))) {
        gamma_elev <- ATAN2(cz - co$z, sqrt((co$x-cx)^2 + (co$y-cy)^2))
        elev <- (gamma_elev - tilt) %% 360
        elev[elev > 180] <- elev[elev > 180] - 360
        elev[elev < -180] <- elev[elev < -180] + 360

        sd <- find_sd(beam_width = beam_v, db_back = param$elev_dB_back, mapping = param$elev_mapping) #param$elev_min3dB
        dBm <- dBm + norm_dBloss(elev, db_back = param$elev_dB_back, sd = sd)
    }

    s <- db2s(dBm, midpoint = param$midpoint, steepness = param$steepness)

    #list(lh = lh, dists = r, dBm = azim2) # plot projected angles
    list(s = s, dist = r, dBm = dBm)
}
