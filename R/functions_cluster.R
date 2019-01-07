#' Stop cluster
#'
#' Stop a cluster that is started with start cluster
#'
#' @import parallel
#' @import doParallel
#' @import foreach
stop_cluster <- function() {
    current_nodes <- nrow(showConnections())
    cluster_defined <- !is.null(get(".cl", envir = .MOBLOC_CACHE))

    if (!cluster_defined) {
        if (current_nodes != 0) {
            stopImplicitCluster()
            warning("Unknown cluster found. Use stopCluster to stop it.")
        } else {
            message("No cluster defined.")
        }
    } else {
        .cl <- get(".cl", envir=.MOBLOC_CACHE)
        stopCluster(.cl)
        rm(".cl", envir = .MOBLOC_CACHE)
        message("Cluster with ", current_nodes, " nodes stopped.")
    }
    invisible()
}

#' Start cluster with desired number of cores.
#'
#' Start a cluster, only if not already defined.
#'
#' @param nodes Number of nodes that are used in this cluster. If missing the number of codes is used.
#' @import parallel
#' @import doParallel
#' @import foreach
start_cluster <- function(nodes = NA) {
    current_nodes <- nrow(showConnections())
    cluster_defined <- !is.null(get(".cl", envir = .MOBLOC_CACHE))

    if (!cluster_defined && current_nodes != 0) {
        warning("Unknown cluster found. Use stopCluster to stop it.")
        return(invisible())
    }

    if (cluster_defined) {
        if (is.na(nodes) || nodes == current_nodes) {
            warning("Cluster with ", current_nodes, " nodes already defined.")
            return(invisible())
        } else {
            stop_cluster()
        }
    }

    if (is.na(nodes)) nodes <- detectCores()

    .cl <- makeCluster(nodes)
    registerDoParallel(.cl)
    assign(".cl", .cl, envir = .MOBLOC_CACHE)
    message("Cluster with ", nodes, " nodes created.")
    invisible()
}

#' Returns number of cors in the current cluster
#'
#' Returns number of cors in the current cluster
#'
#' @param verbose should warnings be given?
#' @import parallel
#' @import doParallel
#' @import foreach
#' @return number of cores
current_cluster <- function(verbose = TRUE) {
    current_nodes <- nrow(showConnections())
    cluster_defined <- !is.null(get(".cl", envir = .MOBLOC_CACHE))

    if (!cluster_defined && current_nodes != 0) {
        if (verbose) warning("Unknown cluster found. Use stopCluster to stop it.")
        return(invisible(current_nodes))
    } else if (cluster_defined) {
        if (verbose) message("Cluster with ", current_nodes, " nodes found.")
        return(invisible(current_nodes))
    } else {
        if (verbose) message("No cluster defined.")
        return(invisible(0L))
    }
}

check_parallel <- function(verbose = FALSE) {
    nc <- nrow(showConnections())
    if (verbose) if (nc==0) {
        message("Function running single threaded. Define a parallel backend to run it in parallel. This can be done with parallel::makeCluster and doParallel::registerDoParallel")
    } else {
        message("Function running with ", nc, " parallel treads")
    }
    (nc != 0)
}
