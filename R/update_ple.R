update_ple <- function(cp, envir, ple_0 = 2.5, ple_1 = 4, ple_small = NA) {
    if (!is_cellplan_valid(cp)) stop("cp is not a validated cellplan. Please validate it with validate_cellplan.")

    x <- raster::extract(envir, cp)

    ple_rng <- ple_1 - ple_0

    values <- (x * ple_rng) + ple_0


    if (!is.na(ple_small)) {
        cp$ple <- ifelse(cp$small, ple_small, values)
    } else {
        cp$ple <- values
    }
    cp
}
