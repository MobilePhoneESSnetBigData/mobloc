calculate_probabilities <- function(shps, rs, param, parallel) {
    `%fun%` <- if (parallel) `%dopar%` else `%do%`

    fnames <- ls(2)

    x <- foreach(p = shps, r = rs, .packages = c("sf", "raster"), .export = fnames, .combine = "c") %fun% {
        #devtools::load_all()
        #source("scripts/src/signal_strength.R", local = TRUE)
        co <- as.data.frame(coordinates(r))

        co$id <- r[][,1]
        co$z <- r[][,2]
        r2 <- st_as_sf(co, coords = c("x", "y", "z"), crs = st_crs(p))
        res <- st_intersects(p, r2, prepared = FALSE)

        pinfo <- as.data.frame(t(st_set_geometry(p[, c("x", "y", "z", "direction", "tilt", "beam_h", "beam_v", "small")], NULL)))


        res2 <- mapply(function(pinf, ri) {
            # include nearest point later, and only in case no intersection points were found in all subrasters
            if (length(ri)==0) {
                list(ids = numeric(), prior = numeric(), lh = numeric(), dists = numeric(), db=numeric())
            } else {

                prior <- rep(1L, length(ri))
                ps_r <- signal_strength(cx = pinf[1],
                                        cy = pinf[2],
                                        cz = pinf[3],
                                        direction = pinf[4],
                                        tilt = pinf[5],
                                        beam_h = pinf[6],
                                        beam_v = pinf[7],
                                        small = pinf[8],
                                        co = co[ri, c("x", "y", "z")],
                                        param = param) # returns list(lh = lh, dists = r, db = db)

                c(list(ids = r2$id[ri], prior = prior), ps_r)
            }

        }, pinfo, res, SIMPLIFY = FALSE)

        res2a <- lapply(res2, "[[", 1)
        res2b <- lapply(res2, "[[", 2)
        res2c <- lapply(res2, "[[", 3)
        res2d <- lapply(res2, "[[", 4)
        res2e <- lapply(res2, "[[", 5)

        list(pid = p$id, rid = res2a, prior = res2b, p = res2c, dist = res2d, db = res2e)
    }

    ## create data.frame
    pids <- do.call(c, x[seq(1, length(x), by = 6)])
    rids <- do.call(c, x[seq(2, length(x), by = 6)])
    priors <- do.call(c, x[seq(3, length(x), by = 6)])
    ps <- do.call(c, x[seq(4, length(x), by = 6)])
    dists <- do.call(c, x[seq(5, length(x), by = 6)])
    dbs <- do.call(c, x[seq(6, length(x), by = 6)])


    ## polygons without raster points
    plns <- sapply(rids, length)
    pids_sel <- plns != 0

    pids_v <- rep(pids[pids_sel], plns[pids_sel])
    rids_v <- unlist(rids[pids_sel])
    priors_v <- unlist(priors[pids_sel])
    ps_v <- unlist(ps[pids_sel])
    dists_v <- unlist(dists[pids_sel])
    dbs_v <- unlist(dbs[pids_sel])
    df <- data.frame(pid=pids_v, rid=rids_v, p=ps_v, prior=priors_v, dist=dists_v, db = dbs_v)


    # # normalize p (i.e. sum of p's is 1 per polygon)
    # df <- df %>%
    #     group_by(pid) %>%
    #     mutate(priorn=prior/sum(prior)) %>%
    #     ungroup()
    #
    # # reduce number of overlapping cells to max_overlap (by default 5) and divide the p's by the (truncated) number of overlapping cells
    df <- df %>%
        group_by(rid) %>%
        #filter(order(p)<=param$max_overlap) %>%
        #mutate(p = p/n()) %>%
        mutate(pn = p / sum(p),
               pr = prior * pn) %>%
        filter(order(pr)<=param$max_overlapping_cells) %>%
        ungroup()

    df <- df %>%
        group_by(pid) %>%
        mutate(pr = pr / sum(pr),
               s = p / sum(p)) %>%
        ungroup()


    df %>% select(pid=pid, rid=rid, p=pr, s = s, dist=dist, db = db)
}
