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

        df <- expand.grid(a = a, rd = rs, ant = ant, x = x, y = y) %>%
            mutate(x = x + SIN(a) * rd,
                   y = y + COS(a) * rd) %>%
            st_as_sf(coords = c("x", "y"), crs = st_crs(cp))
        x <- raster::extract(envir, df)
        mean(x, na.rm = TRUE)
    }, cp$x, cp$y, angles, cp$range, cp$antenna, SIMPLIFY = TRUE)
}
