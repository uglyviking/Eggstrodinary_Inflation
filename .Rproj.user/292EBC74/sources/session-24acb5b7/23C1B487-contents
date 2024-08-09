library(ggpubr)
library(gridExtra)
# Arrange plots using ggarrange
combined_normal_plot <- ggarrange(
  histogram_original, qqplot_original, 
  histogram_egg_original, qqplot_egg_original,
  ncol = 2, nrow = 2,
  labels = c("A", "B", "C", "D"),
  common.legend = TRUE, legend = "bottom"
)

# Save the combined plot
ggsave("inflation_normality_combined_graphs.png", combined_normal_plot, width = 16, height = 12, dpi = 300)

combined_scatter_plots <- ggarrange(
  overall_inflation_scatterplot, egg_inflation_scatterplot,
  ncol = 1, nrow = 2,
  labels = c("A", "B"),
  common.legend = TRUE, legend = "bottom"
)

ggsave("inflation_scatter_combined_graphs.png", combined_scatter_plots, width = 16, height = 12, dpi = 300)


combined_time_series <- ggarrange(
  time_series_plot_25, time_series_plot_5,
  ncol = 1, nrow = 2,
  labels = c("A", "B"),
  common.legend = TRUE, legend = "bottom"
)

combined_violin_plots <- ggarrange(
  violinplot_10, violinplot_5,
  ncol = 1, nrow = 2,
  labels = c("A", "B"),
  common.legend = TRUE, legend = "bottom"
)

# Add extra space at the bottom for the legend
combined_violin_plots_with_space <- grid.arrange(
  combined_violin_plots, 
  heights = c(60, 1)
)

# Save the plot with adjusted dimensions
ggsave("inflation_violin_combined_graphs.png", 
       combined_violin_plots_with_space, 
       width = 16, 
       height = 12, 
       dpi = 300)
