save_plot <- function(plt, name, width, height, dpi = 300, ...) {
	dir <- "_site/figures"
	if (!dir.exists(dir))
		dir.create(dir, recursive = TRUE)
	ggplot2::ggsave(sprintf("%s/%s.pdf", dir, name), plt, width = width, height = height)
	ggplot2::ggsave(sprintf("%s/%s.jpg", dir, name), plt, width = width, height = height, dpi = dpi)
}

theme_set(
	theme_bw(
		base_size = 10
	) + 
	theme(
		legend.position = "top",
		legend.key.size = unit(.75,"line"),
		panel.grid.minor = element_blank(),
		legend.title = element_blank(),
		legend.text = element_text(size = 10),
		plot.title = element_text(size = 10, face = "bold", hjust = .5, vjust = 0),
		plot.subtitle = element_text(size = 10, hjust = .5, vjust = 0),
		plot.caption = element_text(size = 10, hjust = .5, vjust = 0),
		axis.title = element_text(size = 10),
		axis.text = element_text(size = 10),
		strip.text = element_text(size = 10),
		panel.spacing = unit(0.33, "lines")
	)
)
