#' Process cellplan
#'
#' Process cellplan. The propagation is modelled based on the physical properties of the antennas. Also, the likelihood distribution is calculated, which takes the overlap of antennas into account.
#'
#' @param cp cellplan, validated with \code{\link{validate_cellplan}}
#' @param raster raster object that contains the raster tile index numbers (e.g. created with \code{\link{create_raster}})
#' @param elevation raster with elevation data
#' @param param parameter list created with \code{prop_param}
#' @param region polygon shape. If specified, only the signal strength will be calculated for raster tiles inside the polygons
#' @importFrom stats dnorm
#' @import parallel
#' @import doParallel
#' @import foreach
#' @return a data.frame is return with the following colums: antenna (antenna id), rid (raster tile id), dist (distance between antenna and grid tile), dBm (signal strength), s (signal quality), pag (likelihood probability). This data.frame is required to run the interactive tool \code{\link{explore_mobloc}} and to compute the location posterior with \code{\link{calculate_mobloc}}.
#' @example ./examples/process_cellplan.R
#' @seealso \href{../doc/mobloc.html}{\code{vignette("mobloc")}}
#' @export
process_cellplan <- function(cp, raster, elevation, param, region = NULL) {
    x <- y <- z <- height <- direction <- tilt <- beam_h <- beam_v <- W <- ple <- rid <- dBm <- s <- antenna <- dist <- pag <- NULL

    if (!is_cellplan_valid(cp)) stop("Cellplan (cp) is not valid yet. Please validate it with validate_cellplan")

    check_raster(raster)

    parallel <- check_parallel()
    if (!parallel) message("No parallel backend found, so procell_cellplan will run single threaded")


    # precalculate mapping (needed to calculate the dB loss other directions)
    param <- attach_mapping(param)


    # select required cp variables
    cpsel <- cp %>%
        st_set_geometry(NULL) %>%
        dplyr::select(x, y, z, height, direction, tilt, beam_h, beam_v, W, range, ple)

    # determine raster specs
    rext <- raster::extent(raster)
    rres <- raster::xres(raster)

    # select raster id numbers
    if (!missing(region)) {
        message("Determining which raster tiles intersect with region polygon")

        rdf <- get_raster_ids(raster, region)
        rdf$z <- elevation[][rdf$rid]
    } else {
        rdf <- as.data.frame(coordinates(raster))
        rdf$rid <- raster[]
        rdf$z <- elevation[]
    }

    message("Determining coverage area per antenna")

    # for each antenna determine range for which signal strength is within param$sig_q_th (start at +/- range, calculate signal strength and stop when it reached sig_q_th)
    cpsellist <- as.list(cpsel)
    names(cpsellist$x) <- cp$antenna
    res <- do.call(mcmapply, c(list(FUN = find_raster_ids, MoreArgs = list(param = param, rext = rext, rres = rres, rids = raster[]), USE.NAMES = TRUE), cpsellist))


    # debugging mode
    if (FALSE) {
        r2 <- raster(raster)

        r2[][res[[52]]] <- 1

        r2 <- trim(r2)
        qtm(r2) + qtm(cp[52,])
    }

    # create data.frame for each antenna of selected rids
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
    message("Determine signal strength per antenna for raster tiles inside coverage area")
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
        cbind(df, as.data.frame(df2))
    }, df = res2, MoreArgs = list(param = param), SIMPLIFY = FALSE, USE.NAMES = TRUE), as.list(cpsel)))

    # attach antenna name and put in one data.frame
    message("Creating data.frame and compute pag values")
    antennas <- cp$antenna

    df4 <- bind_rows(df3, .id = "antenna")

    # select top [param$max_overlapping_antennas] antennas for each rid, and calculate pag
    df5 <- df4 %>%
        group_by(rid) %>%
        filter(s >= param$sig_q_th) %>%
        filter(order(s)<=param$max_overlapping_antennas) %>%
        mutate(pag = s / sum(s)) %>%
        add_timing_advance() %>%
        ungroup() %>%
        dplyr::select(antenna=antenna, TA=TA, rid=rid, dist=dist, dBm = dBm, s = s, pag = pag) %>%
        attach_class("mobloc_prop")
}






