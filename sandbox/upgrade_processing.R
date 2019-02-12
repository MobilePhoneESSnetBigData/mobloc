process_cellplan <- function(cp, raster, elevation, param, region = NULL) {
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
    if (!is.missing(region)) {
        land <- st_union(region)
        rdf <- get_raster_ids(raster, land)
        rdf$z <- elevation[][rdf$rid]
    } else {
        rdf <- as.data.frame(coordinates(raster))
        rdf$rid <- raster[]
        rdf$z <- elevation[]
    }

    # for each antenna determine range for which signal strength is within param$dBm_th (start at +/- range, calculate signal strength and stop when it reached dBm_th)
    cpsellist <- as.list(cpsel)
    names(cpsellist$x) <- cp$antenna
    res <- do.call(mapply, c(list(FUN = find_raster_ids, MoreArgs = list(param = param, rext = rext, rres = rres), USE.NAMES = TRUE), cpsellist))


    # debugging mode
    if (FALSE) {
        r2 <- raster(raster)

        r2[][res[[52]]] <- 1

        r2 <- trim(r2)
        qtm(r2) + qtm(cp[52,])
    }

    # create data.frame for each antenna of selected rids
    res2 <- lapply(res, function(rs) {
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
    df3 <- do.call(mapply, c(list(FUN = function(df, x, y, z, height, direction, tilt, beam_h, beam_v, W, range, ple, param) {
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
    antennas <- cp$antenna
    df4 <- do.call(rbind, mapply(function(d,nm) {
        d$antenna <- factor(nm, levels = antennas)
        d
    }, df3, names(df3), SIMPLIFY = FALSE, USE.NAMES = FALSE))



    # select top [param$max_overlapping_cells] cells for each rid, and calculate pag
    df5 <- df4 %>%
        group_by(rid) %>%
        filter(order(s)<=param$max_overlapping_cells) %>%
        mutate(pag = s / sum(s)) %>%
        ungroup()

    dplyr::select(antenna=antenna, rid=rid, dist=dist, dBm = dBm, s = s, pag = pag)
}






