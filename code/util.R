save_plot <- function(plt, name, width = 7.5, height = width*3/4, dpi = 300, ...) {
	if (!dir.exists("figures"))
		dir.create("figures")
	plt <- plt + ggplot2::labs(...) # allow removal of caption etc.
	ggplot2::ggsave(sprintf("%s/%s.jpg", "figures", name), plt, 
					width = width, height = height, dpi = dpi)
	plt <- plt + ggplot2::labs(caption = NULL)  # no caption for pdf
	ggplot2::ggsave(sprintf("%s/%s.pdf", "figures", name), plt, 
					width = width, height = height, dpi = dpi)
}

theme_set(
	theme_bw() + 
	theme(
		legend.position = "top",
		panel.grid.minor = element_blank(),
		legend.title = element_blank(),
		plot.caption = element_text(size = 11, hjust = .5, vjust = 0)
	)
)
