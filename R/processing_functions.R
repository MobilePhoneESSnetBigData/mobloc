determine_range <- function(x, y, z, height, direction, tilt, beam_h, beam_v, W, range, ple, param, range_dir) {
    x1 <- x + SIN(range_dir) * range
    y1 <- y + COS(range_dir) * range

    steps1 <- ceiling(range / 100)

    x1s <- seq(x1, x, length.out = steps1)[-steps1]
    y1s <- seq(y1, y, length.out = steps1)[-steps1]



    co <- data.frame(
        x = x1s,
        y = y1s,
        z = z - height
    )

    ss1 <- signal_strength(x, y, z, direction = direction,
                           tilt = tilt,
                           beam_h = beam_h,
                           beam_v = beam_v,
                           W = W,
                           co = co,
                           ple = ple,
                           param = param)$dBm
    w1 <- which(ss1 > param$dBm_th)[1]

    if (is.na(w1)) w1 <- length(ss1)

    c(x1s[w1], y1s[w1])
}

circle_coords <- function(cx, cy, rad) {
    a <- seq(0, 2*pi, length.out = 100)

    list(x = cx + cos(a) * rad,
         y = cy + sin(a) * rad)
}

find_raster_ids <- function(x, y, z, height, direction, tilt, beam_h, beam_v, W, range, ple, antenna, param, rext, rres) {


    if (!is.na(direction)) {
        p1 <- determine_range(x, y, z, height, direction, tilt, beam_h, beam_v, W, range, ple, param, range_dir = direction)
        p2 <- determine_range(x, y, z, height, direction, tilt, beam_h, beam_v, W, range, ple, param, range_dir = direction + 180)

        pc <- (p1 + p2) / 2
    } else {
        p1 <- determine_range(x, y, z, height, direction, tilt, beam_h, beam_v, W, range, ple, param, range_dir = 90)
        pc <- c(x, y)
    }

    if (FALSE) {
        # debugging
        sf = st_as_sf(as.data.frame(matrix(c(p1, pc, x, y), ncol=2, byrow = TRUE)), coords = c("V1", "V2"), crs = 28992)
        sf$x = 1
        qtm(sf)
    }

    rng <- sqrt(sum((p1 - pc)^2))

    circle_range <- lapply(circle_coords(pc[1], pc[2], rng), range)


    if (FALSE) {
        sf2 <- st_as_sf(do.call(data.frame, circle_coords(pc[1], pc[2], rng)), coords = c("x", "y"), crs = 28992)
        qtm(sf2) + qtm(sf, dots.col = "red")
    }


    xlim <- rext[1:2]
    ylim <- rext[3:4]

    xs <- seq(xlim[1] + rres/2, xlim[2] - rres/2, by = rres)
    ys <- seq(ylim[1] + rres/2, ylim[2] - rres/2, by = rres)

    colid <- which(xs > circle_range$x[1] & xs < circle_range$x[2])
    rowid <- which(ys > circle_range$y[1] & ys < circle_range$y[2])

    mcol <- sum(range(colid)) / 2
    mrow <- sum(range(rowid)) / 2

    rad <- ceiling((max(colid) - min(colid)) / 2)

    df <- expand.grid(colid=colid, rowid=rowid, KEEP.OUT.ATTRS = FALSE)

    df$x <- xs[df$colid]
    df$y <- ys[df$rowid]

    df <- df[sqrt((df$x - pc[1])^2 + (df$y - pc[2])^2) <= rng,  ]

    rids <- calculate_rid(df$colid, df$rowid, length(xs), length(ys))


    if (FALSE) {
        r2 <- raster(r)
        r2[][rids] <- 1
        r2 <- trim(r2)
        qtm(r2) + qtm(sf2, is.master = TRUE) + qtm(sf) + tm_grid()
    }
    rids
}



calculate_rid <- function(colid, rowid, nc, nr) {
    ((nr - rowid) * nc) + colid
}



##### intersection raster land
get_raster_ids <- function(r, region) {

    rco <- as.data.frame(coordinates(r))
    nr <- nrow(rco)
    rco$rid <- r[]
    rco_cnk <- split(rco, ceiling((1:nr)/100))

    raster_id_fun <- function(df, region) {
        dfsf <- st_as_sf(df, coords = c("x", "y"), crs = 28992)
        sel <- which_inside(dfsf, region)
        df$rid[sel]
    }


    res <- parallel::mclapply(rco_cnk, FUN = raster_id_fun, region = region)

    res2 <- unname(sort(unlist(res)))

    rco[res2, ]
}
