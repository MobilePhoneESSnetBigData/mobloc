#' Rasterize cellplan
#'
#' Rasterize cellplan
#'
#' @param cp cellplan
#' @param cp_poly cellplan polygons
#' @param raster raster with indices
#' @param elevation raster with elevation data
#' @param param list
#' @export
rasterize_cellplan <- function(cp, cp_poly, raster, elevation, param) {

    r <- brick(raster, elevation)

    cp_poly$x <- cp$x
    cp_poly$y <- cp$y

    cp_poly$z <- cp$z
    cp_poly$direction <- cp$direction
    cp_poly$tilt <- cp$tilt
    cp_poly$beam_h <- cp$beam_h
    cp_poly$beam_v <- cp$beam_v
    cp_poly$small <- cp$small
    # cp_poly <- cbind(cp_poly, cp %>% select(height, a, tilt3, indoor)) currently not working...

    suppressWarnings(start_cluster())

    qres <- quandrantify(cp_poly, r)

    param <- attach_mapping(param)

    ppr <- calculate_probabilities(qres$shps, qres$rs, param)
    ## 67 min, 4core i5 16GB, swap-5GB
    ## 37 min, 16 cores Xeon E5, 38 GB

    # attach cell name
    ppr$Cell_name <- cp$Cell_name[ppr$pid]

    ppr %>% select(Cell_name, rid, p, lh, dist, db)
}
