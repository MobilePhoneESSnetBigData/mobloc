calculate_probabilities <- function(shps, rs, param, parallel) {
    r <- rid <- prior <- pn <- pr <- pid <- s <- dist <- dBm <- Cell_name <- x <- y <- direction <- NULL

    `%fun%` <- if (parallel) `%dopar%` else `%do%`

    fnames <- ls(2)

    x <- foreach(p = shps, r = rs, .packages = c("sp", "sf", "raster", "mobloc"), .export = fnames, .combine = "c") %fun% {
        #devtools::load_all()
        #source("scripts/src/signal_strength.R", local = TRUE)
        co <- as.data.frame(sp::coordinates(r))

        co$id <- r[][,1]
        co$z <- r[][,2]
        r2 <- st_as_sf(co, coords = c("x", "y", "z"), crs = st_crs(p))
        res <- st_intersects(p, r2, prepared = FALSE)

        pinfo <- as.data.frame(t(st_set_geometry(p[, c("x", "y", "z", "direction", "tilt", "beam_h", "beam_v", "small")], NULL)))


        res2 <- mapply(function(pinf, ri) {
            # include nearest point later, and only in case no intersection points were found in all subrasters
            if (length(ri)==0) {
                list(ids = numeric(), prior = numeric(), lh = numeric(), dists = numeric(), dBm=numeric())
            } else {

                #prior <- rep(1L, length(ri))
                ps_r <- signal_strength(cx = pinf[1],
                                        cy = pinf[2],
                                        cz = pinf[3],
                                        direction = pinf[4],
                                        tilt = pinf[5],
                                        beam_h = pinf[6],
                                        beam_v = pinf[7],
                                        small = pinf[8],
                                        co = co[ri, c("x", "y", "z")],
                                        param = param) # returns list(lh = lh, dists = r, dBm = dBm)

                c(list(ids = r2$id[ri]), ps_r)
            }

        }, pinfo, res, SIMPLIFY = FALSE)

        res2a <- lapply(res2, "[[", 1)
        #res2b <- lapply(res2, "[[", 2)
        res2c <- lapply(res2, "[[", 2)
        res2d <- lapply(res2, "[[", 3)
        res2e <- lapply(res2, "[[", 4)

        list(pid = p$id, rid = res2a, s = res2c, dist = res2d, dBm = res2e)
    }

    ## create data.frame
    pids <- do.call(c, x[seq(1, length(x), by = 5)])
    rids <- do.call(c, x[seq(2, length(x), by = 5)])
    #priors <- do.call(c, x[seq(3, length(x), by = 6)])
    ss <- do.call(c, x[seq(3, length(x), by = 5)])
    dists <- do.call(c, x[seq(4, length(x), by = 5)])
    dbs <- do.call(c, x[seq(5, length(x), by = 5)])


    ## polygons without raster points
    plns <- sapply(rids, length)
    pids_sel <- plns != 0

    pids_v <- rep(pids[pids_sel], plns[pids_sel])
    rids_v <- unlist(rids[pids_sel])
    #priors_v <- unlist(priors[pids_sel])
    ss_v <- unlist(ss[pids_sel])
    dists_v <- unlist(dists[pids_sel])
    dbs_v <- unlist(dbs[pids_sel])
    df <- data.frame(pid=pids_v, rid=rids_v, s=ss_v, dist=dists_v, dBm = dbs_v)


    # # normalize p (i.e. sum of p's is 1 per polygon)
    # df <- df %>%
    #     group_by(pid) %>%
    #     mutate(priorn=prior/sum(prior)) %>%
    #     ungroup()
    #
    # # reduce number of overlapping cells to max_overlap (by default 5) and divide the p's by the (truncated) number of overlapping cells
    df <- df %>%
        group_by(rid) %>%
        filter(order(s)<=param$max_overlapping_cells) %>%
        mutate(pag = s / sum(s)) %>%
        ungroup()

    # df <- df %>%
    #     group_by(pid) %>%
    #     mutate(pr = pr / sum(pr),
    #            s = p / sum(p)) %>%
    #     ungroup()

    df %>% select(pid=pid, rid=rid, dist=dist, dBm = dBm, s = s, pag = pag)
}
