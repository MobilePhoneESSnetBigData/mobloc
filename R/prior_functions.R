extract_network_prior <- function(propagation) {
    totals <- sum(propagation$s)
    propagation %>%
        group_by(rid) %>%
        summarize(p = sum(s) / totals) %>%
        ungroup()
}

extract_prior <- function(x, propagation) {
    xv <- raster::getValues(x)

    rv <- 1L:length(x)
    rva <- unique(propagation$rid)

    df <- tibble(rid = rva, p = xv[rva])

    df$p <- df$p / sum(df$p)
    df
}

extract_uniform_prior <- function(propagation) {
    rva <- unique(propagation$rid)
    nrva <- length(rva)
    tibble(rid = rva, p = 1/nrva)
}

composite_prior <- function(priorlist, composition = NULL) {

    k <- length(priorlist)

    if (k == 0) {
        stop("No priors specified.")
    } else if (k == 1) {
        return(priorlist[[1]])
    }
    if (missing(composition)) {
        composition <- rep(1/k, k)
        message("composition set to c(", paste(round(composition, 3), collapse = ", "), ").")
    } else {
        if (length(composition) != k) stop("The length of the composition does not correspond to ", k, ", the number of specified priors")
    }

    rids_list <- lapply(priorlist, function(prior) unique(prior$rid))

    rids <- sort(unique(unlist(rids_list)))

    m <- do.call(cbind, lapply(priorlist, function(prior) {
        prior$p[match(rids, prior$rid)]
    }))

    tibble(rid = rids, p = (m %*% composition)[,1])
}
