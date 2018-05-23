.onLoad <- function(...) {
    assign(".cl", NULL, envir = .MOBLOC_CACHE)
}

.MOBLOC_CACHE <- new.env(FALSE, parent=globalenv())
