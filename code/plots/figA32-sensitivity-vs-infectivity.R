params <- scenario()
lfd_test <- lfd(params)

plt <- expand_grid(
		tibble(
			`mean sensitivity` = c(.4, .6, .8),
			eta = map_dbl(`mean sensitivity`, ~eta(., params))
		),
		tibble(
			Rs = c(1.5, 3, 6),
			gamma = map_dbl(Rs, ~gamma(., params))
		)
	) %>% 
	expand_grid(
		`viral load` = 10^seq(0, 11, length.out = 1000)
	) %>% 
	mutate(
		sensitivity =  map2_dbl(`viral load`, eta,
				~julia_call("sensitivity.", lfd_test, ..1^..2, need_return = "R")
			),
		`1e6` = pmax(0, pmin(1, gamma * (log10(`viral load`) - 6))),
		`1e3` = pmax(0, pmin(1, gamma * (log10(`viral load`) - 3)))
	) %>% 
	pivot_longer(c(`1e3`, `1e6`), names_to = "LLI", values_to = "probability to infect") %>% 
	ggplot() +
		aes(`probability to infect`, color = LLI) +
		geom_line(aes(y = sensitivity)) +
		facet_grid(
			`mean sensitivity` ~ Rs, 
			 labeller = label_both, scales = "free"
		) +
		scale_color_discrete("LLI: ") + 
		labs(
			y = "scaled LFD sensitivity",
			x = "probability to infect during risk contact"
		) +
		theme(
			legend.title = element_text(),
			axis.text.x = element_text(angle = 45, hjust = 1)
		)
save_plot(plt, "figA32-sensitivity-vs-infectivity", width = width, height = 2.1*height)
