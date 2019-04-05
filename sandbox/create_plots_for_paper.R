param <- prop_param()


setup_prop_model()

distance_plot(10, 4, 10000)

g <- mobloc:::signal_quality_plot(param$dBm_mid, param$dBm_width, show_classes = FALSE)

ggsave(g, filename = "s_plot.png", width = 3, height = 1.5, scale = 2)


g2 <- distance_plot(param$W, ple = 4, range = 5000, show_classes = FALSE)

ggsave(g2, filename = "dist_plot.png", width = 3, height = 1.5, scale = 2)



param <- param_model <- prop_param(height = 55, tilt = 5, ple = 4, W = 10)

param_model$direction <- 90
param_model$h3dB <- param$beam_h
param_model$v3dB <- param$beam_v
param_model$hback <- param$azim_dB_back
param_model$vback <- param$elev_dB_back


gm <- theme_grey(base_size = 25) + theme(plot.margin = unit(c(0.1,0,0.1,0), "cm"))


param_plots1 <- list(enable = c("d", "h", "v"), range = 5500, mask = FALSE, colors = "discrete", type = "quality")
g1 <- heatmap_ground(param_model, param_plots1, param, title = FALSE) + gm
param_plots2 <- list(enable = c("d", "h", "v"), range = 5500, mask = FALSE, colors = "cont", type = "quality")
g2 <- heatmap_ground(param_model, param_plots2, param, title = FALSE) + gm


param_plots3 <- list(enable = c("d", "h", "v"), range = 5500, mask = FALSE, colors = "discrete", type = "dBm")
g3 <- heatmap_ground(param_model, param_plots3, param, title = FALSE) + gm
param_plots4 <- list(enable = c("d", "h", "v"), range = 5500, mask = FALSE, colors = "cont", type = "dBm")
g4 <- heatmap_ground(param_model, param_plots4, param, title = FALSE) + gm




library(gridExtra)

png("propagation_heatmap.png", width = 2000, height = 1200)
grid.arrange(g4, g3, g2, g1, nrow = 2)
dev.off()

