heatmap_ground <- function(co, param, input) {
    lh <- x <- y <- db <- NULL
  if (length(input$enable) || input$small) {
    enable <- if (input$small) "d" else input$enable
    co2 <- cbind(co, signal_strength(0,0,input$height, direction = 90, tilt = input$tilt, beam_h = input$h3dB, beam_v =  input$v3dB, small = input$small, co = co, param = param, enable = enable))

    if (input$type == "likelihood") {
      co2$lh <- co2$lh / sum(co2$lh)
      if (input$mask) {
        values <- sort(co2$lh, decreasing = TRUE)
        thvalue <- values[which.min(abs(cumsum(values) - input$maskrangelh))]
        co3 <- co2[co2$lh >= thvalue, ]
      }
      gg <- ggplot(co2, mapping = aes(x=x, y=y, fill = lh)) + geom_tile() + coord_fixed() + scale_fill_continuous("Probability")
    } else {
      if (input$mask) co3 <- co2[co2$db >= input$maskrangedb[1] & co2$db <= input$maskrangedb[2], ]
      co2$dbcat <- cut(co2$db, breaks = c(-Inf, seq(-130, -60, by = 10), Inf),
                       labels = c("-130 or less", "-130 to -120", "-120 to -110", "-110 to -100",
                                  "-100 to -90", "-90 to -80", "-80 to -70", "-70 to -60", "-60 or better"))

      #co2$dbcat <- cut(co2$db, breaks = c(-Inf, seq(-1,1, by=.25) * pi, Inf)) # test projected angles


      #gg <- ggplot(co2, mapping = aes(x=x, y=y, fill = dbcat)) + geom_tile() + coord_fixed() + scale_fill_manual("dB", values = viridis(9, option = "C"))
      co2$db[co2$db > -60] <- -60
      co2$db[co2$db < -130] <- -130


      gg <- ggplot(co2, mapping = aes(x=x, y=y, fill = db)) + geom_tile() + coord_fixed() + scale_fill_gradient("dBm", limits = c(-130, -60))
    }
    if (input$mask) {
      gg <- gg + geom_tile(data = co3, fill = "red")
    }
    gg
  }

}

#' Signal strength plot functions
#'
#' Signal strength plot functions. The \code{radiation_plot} plots the radiation in the horizontal (azimuth) or vertical (elevation) plane, the \code{distance_plot} the relation between distance and signal loss, and the \code{likelihood_plot} plots the relation between signal stregth and likelihood, which is modelled as a logistic function
#'
#' @name distance_plot
#' @rdname plot_functions
#' @param db0 signal strength in dBm near a cell
#' @param base_size base size of the plot
#' @export
distance_plot <- function(db0, base_size = 11) {
    distance <- dBm <- NULL
  df <- data.frame(distance = seq(10, 3000, by=10))
  df$dBm <- distance2dB(df$distance, db0)
  ggplot(df, aes(x=distance, y= dBm)) + geom_line() + theme_bw(base_size = base_size)
}

#' @name relative_signal_strength_plot
#' @rdname plot_functions
#' @param db_mid middle point in the logistic function to map signal strength to probability
#' @param db_width width of the logistic function to map signal strength to probability
relative_signal_strength_plot <- function(db_mid, db_width, base_size = 11) {
    dBm <- likelihood <- NULL
  df <- data.frame(dBm = seq(-130, -50, length.out = 100))
  df$rsig <- db2p(df$dBm, db_mid, db_width)
  ggplot(df, aes(x=dBm, y=rsig)) + geom_line() + scale_y_continuous("Relative signal strength") + theme_bw(base_size = base_size)
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
