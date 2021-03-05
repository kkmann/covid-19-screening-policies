save_plot <- function(plt, name, width = 7.5, height = width*3/4, dpi = 300, ...) {
	dir <- "_site/figures"
	if (!dir.exists(dir))
		dir.create(dir, recursive = TRUE)
	ggplot2::ggsave(sprintf("%s/%s.pdf", dir, name), plt, 
					width = width, height = height, dpi = dpi)
}

theme_set(
	theme_bw() + 
	theme(
		legend.position = "top",
		panel.grid.minor = element_blank(),
		legend.title = element_blank(),
		plot.caption = element_text(size = 11, hjust = .5, vjust = 0),
		panel.spacing = unit(1.25, "lines")
	)
)
