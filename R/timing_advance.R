add_timing_advance <- function(prop, step = 550, max = 63) {
    prop %>%
        mutate(TA = dist %/% step) %>%
        filter(TA <= max) %>%
        select(antenna, TA, rid, dist, dBm, s, pag)
}
