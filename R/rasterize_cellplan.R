#' Rasterize cellplan
#'
#' Rasterize cellplan
#'
#' @param cp cellplan
#' @param cp_poly cellplan polygons
#' @param raster raster with indices
#' @param elevation raster with elevation data
#' @param param list
#' @importFrom stats dnorm
#' @export
rasterize_cellplan <- function(cp, cp_poly, raster, elevation, param) {
    dist <- dBm <- Cell_name <- rid <- s <- NULL

    r <- brick(raster, elevation)

    shp <- cp_poly$poly

    shp$x <- cp$x
    shp$y <- cp$y

    shp$z <- cp$z
    shp$direction <- cp$direction
    shp$tilt <- cp$tilt
    shp$beam_h <- cp$beam_h
    shp$beam_v <- cp$beam_v
    shp$small <- cp$small
    # shp <- cbind(shp, cp %>% select(height, a, tilt3, indoor)) currently not working...

    #suppressWarnings(start_cluster())

    parallel <- check_parallel()


    qres <- quandrantify(shp, r)

    param <- attach_mapping(param)

    ppr <- calculate_probabilities(qres$shps, qres$rs, param, parallel = parallel)
    ## 67 min, 4core i5 16GB, swap-5GB
    ## 37 min, 16 cores Xeon E5, 38 GB

    # attach cell name
    ppr$Cell_name <- cp$Cell_name[ppr$pid]

    ppr %>% select(Cell_name, rid, dist, dBm, s, pag)
}
