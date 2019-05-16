add_timing_advance <- function(prop, param) { # step = 550, max = 63) {
    TA <- rid <- cell <- pag <- s <- dBm <- dist <- NULL
    prop %>%
        mutate(TA = dist %/% param$TA_step) %>%
        filter(TA <= param$TA_max) %>%
        select(cell, TA, rid, dist, dBm, s, pag)
}
