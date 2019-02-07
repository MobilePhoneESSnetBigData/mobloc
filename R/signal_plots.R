get_grid_coor <- function(range) {

    as.data.frame(expand.grid(x=seq(-range/2, range, by = range / 100), y=seq(-range/2, range/2, by = range/100), z=0))
}

dBm_classes <- list(breaks = c(-Inf, seq(-120, -70, by = 10), Inf),
                    labels = c("-120 or less", "-120 to -110", "-110 to -100",
                               "-100 to -90", "-90 to -80", "-80 to -70", "-70 or better"),
                    colors = RColorBrewer::brewer.pal(7, "Spectral"),
                    lims = c(-120, -70),
                    tit = "Signal strength (dBm)")

qty_classes <- list(breaks = seq(0, 1, by = .1),
                    labels = NULL,
                    colors = RColorBrewer::brewer.pal(10, "Spectral"),
                    lims = c(0, 1),
                    tit = "Signal quality")

heatmap_ground <- function(co, param_model, param_plots, param) {

    co <- get_grid_coor(range  = param_plots$range)

    lh <- x <- y <- db <- NULL
    if (length(param_plots$enable)) {
        param$dBm_mid <- param_model$dBm_mid
        param$dBm_width <- param_model$dBm_width

        co2 <- cbind(co, signal_strength(0,0, param_model$height, direction = param_model$direction, tilt = param_model$tilt, beam_h = param_model$h3dB, beam_v =  param_model$v3dB, W = param_model$W, co = co, ple = param_model$ple, param = param, enable = param_plots$enable))


        if (param_plots$type == "quality") {
            co2$value <- co2$s
            cls <- qty_classes
            mr <- param_plots$maskrangelh
        } else {
            co2$value <- co2$dBm
            cls <- dBm_classes
            mr <- param_plots$maskrangedb
        }


        if (param_plots$mask) co3 <- co2[co2$value >= mr[1] & co2$value <= mr[2], ]


        co2$valueCat <- cut(co2$value, breaks = cls$breaks, labels = cls$labels, include.lowest = TRUE, right = FALSE)

        co2$value[co2$value > cls$lims[2]] <- cls$lims[2]
        co2$value[co2$value < cls$lims[1]] <- cls$lims[1]
#
#         if (param_plots$type == "quality") {
#
#             if (param_plots$mask) {
#                 values <- sort(co2$s, decreasing = TRUE)
#                 thvalue <- values[which.min(abs(cumsum(values) - param_plots$maskrangelh))]
#                 co3 <- co2[co2$s >= thvalue, ]
#             }
#
#             co2$valueCat <- cut(co2$s, breaks = seq(0, 1, by = .1), include.lowest = TRUE, right = FALSE)
#
#             co2$value <- co2$s
#
#             lims <- c(0, 1)
#             tit <- "Quality"
#
#         } else {
#
#             if (param_plots$mask) co3 <- co2[co2$dBm >= param_plots$maskrangedb[1] & co2$dBm <= param_plots$maskrangedb[2], ]
#
#             co2$valueCat <- cut(co2$dBm, dBm_classes$breaks, dBm_classes$labels, include.lowest = TRUE, right = FALSE)
#
#             co2$value <- co2$dBm
#             co2$value[co2$value > -70] <- -70
#             co2$value[co2$value < -120] <- -120
#
#
#             lims <- c(-120, -70)
#             tit <- "dBm"
#         }

        if (param_plots$colors == "discrete") {
            gg <- ggplot(co2, mapping = aes(x=x, y=y, fill = valueCat)) + geom_tile() + coord_fixed() + scale_fill_manual(cls$tit, values = cls$colors, drop = FALSE) # viridisLite::viridis(9, option = "C")
        } else {
            gg <- ggplot(co2, mapping = aes(x=x, y=y, fill = value)) + geom_tile() + coord_fixed() + scale_fill_gradient(cls$tit, limits = cls$lims)
        }

        if (param_plots$mask) {
            gg <- gg + geom_tile(data = co3, fill = "red")
        }

        if (is.na(param_model$direction)) {
            gg + ggtitle("Top view of a small cell antenna")
        } else {
            gg + ggtitle("Top view of an antenna directed eastwards")
        }
    }

}

#' Signal strength plot functions
#'
#' Signal strength plot functions. The \code{radiation_plot} plots the radiation in the horizontal (azimuth) or vertical (elevation) plane, the \code{distance_plot} the relation between distance and signal loss, and the \code{signal_quality} plots the relation between signal stregth and likelihood, which is modelled as a logistic function
#'
#' @name distance_plot
#' @rdname plot_functions
#' @param W power of a cell
#' @param base_size base size of the plot
#' @export
distance_plot <- function(W, ple, range, base_size = 11) {
  distance <- NULL
  dBm <- W2dBm(W)
  df <- data.frame(distance = seq(10, range, by=10))
  df$dBm <- distance2dB(df$distance, ple, W)
  df <- df[df$dBm >= -110 & df$dBm <= 0, ]

  brks <- c(-130, dBm_classes$breaks[2:(length(dBm_classes$breaks) - 1)], 0)
  rangemin <- -range/30
  df2 <- data.frame(xmin = rangemin,
                    xmax = rangemin / 4,
                    ymin = brks[-length(brks)],
                    ymax = brks[-1],
                    fill = dBm_classes$colors)


  ggplot(df, aes(x=distance, y= dBm)) + geom_line() +
      geom_rect(data = df2, aes(fill = I(fill), xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, x = NULL)) +
      scale_x_continuous(limits = c(rangemin, range), breaks = pretty(c(0, range), n = 8)) +
      scale_y_continuous("Signal strength", limits = c(-130, 0), breaks = seq(-130, 0, by = 10), minor_breaks = NULL) + theme_bw(base_size = base_size) + ggtitle("Signal loss over distance")
}

#' @name signal_quality_plot
#' @rdname plot_functions
#' @param dBm_mid middle point in the logistic function to map signal strength to probability
#' @param dBm_width width of the logistic function to map signal strength to probability
signal_quality_plot <- function(dBm_mid, dBm_width, base_size = 11) {
    dBm <- likelihood <- NULL
  df <- data.frame(dBm = seq(-130, -50, length.out = 100))
  df$rsig <- db2s(df$dBm, dBm_mid, dBm_width)

  brks <- qty_classes$breaks
  df2 <- data.frame(xmin = -133,
                    xmax = -131,
                    ymin = brks[-length(brks)],
                    ymax = brks[-1],
                    fill = qty_classes$colors)


  brks <- c(-130, dBm_classes$breaks[2:(length(dBm_classes$breaks) - 1)], -50)
  df3 <- data.frame(xmin = brks[-length(brks)],
                    xmax = brks[-1],
                    ymin = -.1,
                    ymax = -.03,
                    fill = dBm_classes$colors)

  ggplot(df, aes(x=dBm, y=rsig)) + geom_line() +
      geom_rect(data = df3, aes(fill = I(fill), xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, x = NULL, y = NULL)) +
      geom_rect(data = df2, aes(fill = I(fill), xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, x = NULL, y = NULL)) +
      scale_y_continuous("Signal quality", limits = c(-.1, 1), breaks = qty_classes$breaks, minor_breaks = NULL) +
      scale_x_continuous("Signal strength", limits = c(-134, -50), breaks = seq(-130, 0, by = 10), minor_breaks = NULL) +
      theme_bw(base_size = base_size) + ggtitle("Signal quality")
}

#' @name radiation_plot
#' @rdname plot_functions
#' @param type \code{"a"} for azimuth (horizontal) plane and \code{"e"} for elevation/vertical plane
#' @param beam_width beam width
#' @param db_back difference in signal strength between front and back
#' @export
radiation_plot <- function(type = "a", beam_width, db_back = -30, base_size = 11) {
    deg <- dbLoss <- x <- y <- NULL
  start <- ifelse(type=="a", 0, pi/2)
  title <- ifelse(type=="a", "Azimuth Plane Pattern", "Elevation Plane Pattern")
  df <- data.frame(deg = seq(0, 360, length.out = 360))
  df$dbLoss <- norm_dBloss(a = df$deg, db_back = db_back, beam_width = beam_width)
  df2 <- data.frame(a = rep(0, 4), deg = c(0, -10, -20, -30))
  deg_dbmin3 <- round(df$deg[which.min(abs(-3 - df$dbLoss))])
  if (deg_dbmin3 > 180) deg_dbmin3 <- 360 - deg_dbmin3
  df3 <- data.frame(x = c(deg_dbmin3, 360 - deg_dbmin3), y = -3)

  brks <- seq(30, 330, by = 30)
  brks_diff <- pmin(abs(brks - df3$x[1]), abs(brks - df3$x[2]))

  if (min(brks_diff) < 5) {
    brks <- sort(c(brks[brks_diff>=5], df3$x))
  } else {
    brks <- sort(c(brks, df3$x))
  }

  ggplot(df, aes(x=deg, y= dbLoss)) +
    scale_x_continuous(title, breaks = brks) +
    geom_hline(yintercept = c(-30, -20, -10, 0), colour = "grey70", size = 0.4) +
    geom_vline(xintercept = seq(0, 330, by = 30), colour = "grey70", size = 0.4) +
      geom_line(size = 0.5) +
      geom_vline(xintercept = df3$x, colour = "red", size = 0.4) +
    geom_point(aes(x=x,y=y), data=df3, colour = "red", size = 1) +
    geom_text(aes(label = deg, x = a, y=deg-2), data = df2, size = 3, color = "grey20") +
    scale_y_continuous(limits = c(-35, 0)) +
    coord_polar(theta = "x", start=start) +
    theme_bw(base_size = base_size) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid=element_blank(),
          panel.border = element_blank())
}
