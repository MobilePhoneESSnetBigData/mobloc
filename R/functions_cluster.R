check_parallel <- function(verbose = FALSE) {
    nc <- nrow(showConnections())
    if (verbose) if (nc==0) {
        message("Function running single threaded. Define a parallel backend to run it in parallel. This can be done with parallel::makeCluster and doParallel::registerDoParallel")
    } else {
        message("Function running with ", nc, " parallel treads")
    }
    (nc != 0)
}
