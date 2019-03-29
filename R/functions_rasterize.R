#' Create empty raster
#'
#' Create empty raster from a bounding box with a specified projection.
#'
#' @param x either a bounding box (see \code{\link[sf:st_bbox]{st_bbox}}) where the projection (\code{crs}) must be specified, or a raster object (where the elevation object is supposed to be used)
#' @param tile.size cell size (which is both the width and the height) in meters (assuming the coordinate system is specified in meters)
#' @return raster layer
#' @import sf
#' @importFrom raster raster setValues brick extent crop extract
#' @example ./examples/create_raster.R
#' @export
create_raster <- function(x, tile.size = 100) {
    if (inherits(x, "bbox")) {
        nc <- (x[3] - x[1]) / tile.size
        nr <- (x[4] - x[2]) / tile.size

        r <- raster(nrows=nr, ncols=nc, xmn=x[1], xmx=x[3], ymn=x[2], ymx=x[4], crs=attr(x, "crs")$proj4string)
    } else if (inherits(x, "Raster")) {
        r <- raster(x)
    } else stop("x has a wrong format: it should either be a raster object or an sf bbox", call. = FALSE)
    setValues(r, 1L:length(r))
}

check_raster <- function(x) {
    values <- x[]
    if (any(is.na(values))) stop("Invalid raster. It contains missing values", call. = FALSE)
    if (anyDuplicated(values)) stop("Invalid raster. It contains duplicated values", call. = FALSE)
    NULL
}



get_chunks <- function(n, chunks_per_core = 4) {
    cores <- detectCores()
    k <- cores * chunks_per_core
    m <- ceiling(n/k)
    ids <- rep(1:m, each=k, length.out=n)
    split(1:n, ids)
}

raster_dim <- function(r) {
    ex <- extent(r)

    c(width = (ex[2] - ex[1]) / ncol(r),
      height = (ex[4] - ex[3]) / nrow(r))
}


quandrantify <- function(shp, r, depth.max = 5) {
    # calculate bbox per polygon
    bbx <- as.data.frame(t(sapply(shp$geometry, function(f) as.vector(st_bbox(f)))))
    names(bbx) <- c("xmin", "ymin", "xmax", "ymax")
    bbx$id <- 1L:nrow(bbx)
    #st_bbox(shp)

    # split raster bounding box recursively
    #r <- raster::setValues(r, 1L:length(r))

    rdim <- raster_dim(r)
    q <- qdivide(as.vector(extent(r)), depth.max = depth.max, rdim = rdim)

    # split bboxes recursively and get corrsponding cellplan polygons
    bbxs <- assign_to_q(bbx, q)
    shp$id <- 1L:nrow(shp)
    get_sub_shps <- function(b) {
        if (!is.list(b)) {
            shp[b,]
        } else {
            list(get_sub_shps(b[[1]]),
                 get_sub_shps(b[[2]]),
                 get_sub_shps(b[[3]]),
                 get_sub_shps(b[[4]]))
        }
    }
    shps <- get_sub_shps(bbxs)

    # split raster recursively
    rs <- get_sub_rasters(r, q)

    # remove empty raster cells and flatten nested list
    ne <- find_non_empty(bbxs)
    rs2 <- remove_empty_r(ne, rs)
    shps2 <- remove_empty_r(ne, shps)
    rs3 <- unlist(rs2)
    shps3 <- unlist2(shps2)

    list(shps=shps3, rs=rs3)
}

qdivide <- function(v, depth.current=1, depth.max=16, rdim) {
    xmid <- round(mean(v[1:2]) / rdim[1]) * rdim[1]
    ymid <- round(mean(v[3:4]) / rdim[2]) * rdim[2]
    w <- c(xmid, ymid)

    if (depth.current == depth.max) {
        v
    } else {
        list(c(w, v),
             qdivide(c(v[1], xmid, ymid, v[4]), depth.current = depth.current + 1, depth.max = depth.max, rdim= rdim),
             qdivide(c(xmid, v[2], ymid, v[4]), depth.current = depth.current + 1, depth.max = depth.max, rdim = rdim),
             qdivide(c(xmid, v[2], v[3], ymid), depth.current = depth.current + 1, depth.max = depth.max, rdim = rdim),
             qdivide(c(v[1], xmid, v[3], ymid), depth.current = depth.current + 1, depth.max = depth.max, rdim = rdim))

    }
}


get_sub_rasters <- function(r, q) {
    if (!is.list(q)) {
        crop(r, extent(q))
    } else {
        e <- q[[1]][3:6]
        r2 <- crop(r, extent(e))
        list(get_sub_rasters(r2, q[[2]]),
             get_sub_rasters(r2, q[[3]]),
             get_sub_rasters(r2, q[[4]]),
             get_sub_rasters(r2, q[[5]]))
    }
}

assign_to_q <- function(b, q) {
    if (!is.list(q)) {
        b$id
    } else {
        v <- q[[1]]

        list(assign_to_q(b[b$xmin < v[1] & b$ymax > v[2], ], q[[2]]),
             assign_to_q(b[b$xmax > v[1] & b$ymax > v[2], ], q[[3]]),
             assign_to_q(b[b$xmax > v[1] & b$ymin < v[2], ], q[[4]]),
             assign_to_q(b[b$xmin < v[1] & b$ymin < v[2], ], q[[5]]))
    }
}



find_non_empty <- function(b) {
    if (is.list(b[[1]])) {
        lapply(b, find_non_empty)
    } else {
        sapply(b, function(x) length(x) > 0)
    }
}

remove_empty_r <- function(nlist, rlist) {
    if (!is.list(nlist)) {
        if (any(nlist)) rlist[which(nlist)] else NULL
    } else {
        res <- mapply(remove_empty_r, nlist, rlist, SIMPLIFY = FALSE)
        isn <- sapply(res, is.null)
        if (all(isn)) NULL else res[which(!isn)]
    }
}


unlist2 <- function(l) {
    if (inherits(l[[1]], "sf")) {
        l
    } else {
        do.call(c, lapply(l, unlist2))
    }
}

