plt1 <- do.call(school, scenario()) %>% 
	julia_call("get_adjacency_matrix", .) %>% 
	as_tbl_graph() %>% 
	mutate(
		tmp = "a"
	) %>% 
	ggraph("matrix") +
		geom_edge_tile(aes(fill = weight)) + 
		scale_edge_fill_continuous(
			"", 
			low = "#dddddd", high = "#000000", na.value = "white",
			limits = c(0, NA_real_)
		) +
		coord_fixed(expand = FALSE) +
		theme(
			legend.position = "top"
		) +
		labs(title = "3 bubbles per class")
plt2 <- do.call(school, scenario(n_bubble = 27, bubbles_per_class = 1)) %>% 
	julia_call("get_adjacency_matrix", .) %>% 
	as_tbl_graph() %>% 
	mutate(
		tmp = "a"
	) %>% 
	ggraph("matrix") +
		geom_edge_tile(aes(fill = weight)) + 
		scale_edge_fill_continuous(
			"", 
			low = "#dddddd", high = "#000000", na.value = "white",
			limits = c(0, NA_real_)
		) +
		coord_fixed(expand = FALSE) +
		theme(
			legend.position = "top"
		) +
		labs(title = "one bubble per class")
plt <- plt1 + plt2 + plot_layout(guides = "collect") & theme(legend.position = "right")
save_plot(plt, "adjacency-matrices", width = width, height = .66*height)
