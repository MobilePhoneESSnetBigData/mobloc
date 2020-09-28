#' Compute signal strength
#'
#' Compute signal strength (propagation). The propagation is modelled based on the physical properties of the cells. Also, the likelihood distribution is calculated, which takes the overlap of cells into account.
#'
#' @param cp cellplan, validated with \code{\link{validate_cellplan}}
#' @param raster raster object that contains the raster tile index numbers (e.g. created with \code{\link{create_raster}})
#' @param elevation raster with elevation data
#' @param param parameter list created with \code{mobloc_param}
#' @param region polygon shape. If specified, only the signal strength will be calculated for raster tiles inside the polygons
#' @importFrom stats dnorm
#' @import parallel
#' @import doParallel
#' @import foreach
#' @import data.table
#' @return a data.frame is return with the following colums: cell (cell id), rid (raster tile id), dist (distance between cell and grid tile), dBm (signal strength), s (signal dominance), pag (likelihood probability). This data.frame is required to run the interactive tool \code{explore_mobloc} from the \code{mobvis} package and to compute the connection likelihood with \code{\link{create_strength_llh}}.
#' @example ./examples/compute_sig_strength.R
#' @seealso \href{../doc/mobloc.html}{\code{vignette("mobloc")}}
#' @export
compute_sig_strength <- function(cp, raster, elevation, param, region = NULL) {
    x <- y <- z <- height <- direction <- tilt <- beam_h <- beam_v <- W <- ple <- rid <- dBm <- s <- cell <- dist <- pag <- TA <- NULL

    if (!is_cellplan_valid(cp)) stop("Cellplan (cp) is not valid yet. Please validate it with validate_cellplan")

    check_raster(raster)

    parallel <- check_parallel()
    if (!parallel) message("No parallel backend found, so procell_cellplan will run single threaded")


    # precalculate mapping (needed to calculate the dB loss other directions)
    param <- attach_mapping(param)


    # select required cp variables
    cpsel <- (cp %>%
        st_set_geometry(NULL) %>%
        as.data.table())[, list(x, y, z, height, direction, tilt, beam_h, beam_v, W, range, ple)]

    # determine raster specs
    rext <- raster::extent(raster)
    rres <- raster::xres(raster)

    # select raster id numbers
    if (!missing(region)) {
        message("Determining which raster tiles intersect with region polygon")

        rdf <- get_raster_ids(raster, region)
        rdf$z <- elevation[][rdf$rid]
    } else {
        rdf <- as.data.table(coordinates(raster))
        rdf$rid <- raster[]
        rdf$z <- elevation[]
    }

    if (any(is.na(rdf$z))) {
        warning("elevation contains NAs: these are replaced by 0")
        rdf[is.na(z), z := 0]
    }

    message("Determining coverage area per cell")

    # for each cell determine range for which signal strength is within param$sig_d_th (start at +/- range, calculate signal strength and stop when it reached sig_d_th)
    cpsellist <- as.list(cpsel)
    names(cpsellist$x) <- cp$cell
    res <- do.call(mcmapply, c(list(FUN = find_raster_ids, MoreArgs = list(param = param, rext = rext, rres = rres, rids = raster[]), USE.NAMES = TRUE), cpsellist))


    # debugging mode
    if (FALSE) {
        r2 <- raster(raster)

        r2[][res[[52]]] <- 1

        r2 <- trim(r2)
        qtm(r2) + qtm(cp[52,])
    }

    # create data.frame for each cell of selected rids
    res2 <- mclapply(res, function(rs) {
        rdf[rdf$rid %in% rs, ]
    })

    # debugging mode
    if (FALSE) {
        r2 <- raster(raster)

        r2[][res2[[132]]$rid] <- 1

        r2 <- trim(r2)
        qtm(r2) + qtm(cp[132,])
    }

    # calculate signal strength
    message("Determine signal strength per cell for raster tiles inside coverage area")
    df3 <- do.call(mcmapply, c(list(FUN = function(df, x, y, z, height, direction, tilt, beam_h, beam_v, W, range, ple, param) {
        df2 <- signal_strength(cx=x, cy=y, cz=z,
                               direction = direction,
                               tilt = tilt,
                               beam_h = beam_h,
                               beam_v = beam_v,
                               W = W,
                               co = df[, c("x", "y", "z")],
                               ple = ple,
                               param = param)
        cbind(df, as.data.table(df2))
    }, df = res2, MoreArgs = list(param = param), SIMPLIFY = FALSE, USE.NAMES = TRUE), as.list(cpsel)))

    # attach cell name and put in one data.frame
    message("Creating data.frame and compute pag values")
    cells <- cp$cell


    df4 <- rbindlist(df3, idcol = "cell")

    df4[, cell:=factor(cell, levels = cp$cell)]

    setkey(df4, rid)
    df4[, list(cell, rid, dist, dBm, s)] %>%
        attach_class("mobloc_strength")
}


calculate_dist <- function(df, cp, raster, elev = NULL) {
    rid <- cell <- x <- y <- z <- dist <- cx <- cy <- cz <- NULL

    if (!inherits(df, c("mobloc_llh", "mobloc_post"))) stop("x is not a mobloc_llh nor mobloc_post")
    if (!is_cellplan_valid(cp)) stop("cellplan has not been not validated")
    check_raster(raster)


    rdf <- as.data.table(coordinates(raster))
    rdf$rid <- raster[]
    if (!is.null(elev)) rdf$z <- elev[]

    setkey(rdf, rid)

    cpsel <- (cp %>%
        sf::st_set_geometry(NULL) %>%
        as.data.table())[, list(cell, cx=x,cy=y, cz=z)]

    df2 <- rdf[df, on = "rid"]

    df3 <- cpsel[df2, on = "cell"]

    if (is.null(elev)) {
        df3[, dist:= sqrt((x - cx)^2 + (y - cy)^2)]
    } else {
        df3[, dist:= sqrt((x - cx)^2 + (y - cy)^2 + (z - cz)^2)]
    }

    df3[, `:=`(cx = NULL, cy = NULL, cz = NULL)]
}


