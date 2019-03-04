param <- prop_param()


setup_prop_model()

distance_plot(10, 4, 10000)

g <- mobloc:::signal_quality_plot(param$dBm_mid, param$dBm_width, show_classes = FALSE)

ggsave(g, filename = "s_plot.png", width = 3, height = 1.5, scale = 2)


g2 <- distance_plot(param$W, ple = 4, range = 5000, show_classes = FALSE)

ggsave(g2, filename = "dist_plot.png", width = 3, height = 1.5, scale = 2)

W2dBm
