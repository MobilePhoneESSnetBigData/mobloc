#' Create signal strenth likelihood
#'
#' @param strength a signal strength model object, which is the result of \code{\link{compute_sig_strength}}
#' @param param parameter list created with \code{mobloc_param}
#' @export
create_strength_llh <- function(strength, param) {
    strength[s >= param$sig_d_th][
        , by = rid, .(os = order(s), cell, dist, dBm, s)][
            os <=param$max_overlapping_cells, pag:= s / sum(s), by = rid][
                !is.na(pag), list(cell, rid, dist, pag)] %>%
        attach_class("mobloc_llh")
}
