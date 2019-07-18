# borrowed from tmaptools:::create_sf_rect
create_bbx_rect <- function (bbx, steps = 100, stepsize = 10)
{
    projection <- st_crs(bbx)
    x1 <- bbx[1]
    x2 <- bbx[3]
    y1 <- bbx[2]
    y2 <- bbx[4]
    dx <- x2 - x1
    dy <- y2 - y1
    ny <- round(dy/stepsize + 1)
    nx <- round(dx/stepsize + 1)
    crds <- matrix(c(rep(x1, ny), seq(x1 + stepsize, x2 - stepsize,
                                      length.out = nx - 2), rep(x2, ny), seq(x2 - stepsize,
                                                                             x1 + stepsize, length.out = nx - 2), seq(y1, y2, length.out = ny),
                     rep(y2, nx - 2), seq(y2, y1, length.out = ny), rep(y1,
                                                                        nx - 2)), ncol = 2)
    sf::st_sfc(sf::st_polygon(x = list(rbind(crds, crds[1, ]))),
               crs = projection)
}

raster2bbx <- function(raster) {
    crs <- raster@crs@projargs
    ex <- extent(raster)
    st_bbox(c(xmin = ex[1], ymin = ex[3], xmax = ex[2], ymax = ex[4]), crs = crs)
}

mobloc_crop_raster <- function(r, bbx) {
    raster::crop(r, extent(as.vector(bbx)[c(1,3,2,4)]))
}
mobloc_find_cells <- function(prop, raster) {
    unique(prop$cell[prop$rid %in% raster[]])
}
mobloc_filter_cell <- function(x, a, raster = NULL) {
    cell <- rid <- NULL
    y <- x[cell %chin% a]
    if (!missing(raster)) {
        y[rid %in% raster[]]
    } else {
        y
    }
}
