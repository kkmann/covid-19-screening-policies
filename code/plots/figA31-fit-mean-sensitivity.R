params <- scenario()

tbl_innova_data <- tibble(
		`viral load` = 10^(2:7),
		sensitivity = 1 - (c(621, 521, 344, 207, 144, 123) - 114) / (684 - 114)
	)
tbl_scenarios <- tibble(
		`mean sensitivity` = c(mean_sensitivity(1, params), .4, .6, .8),
		eta = map_dbl(`mean sensitivity`, ~eta(., params))
	)

params$mean_sensitivity <- mean_sensitivity(1, params)
lfd_test <- lfd(params)
plt <- tbl_scenarios %>% 
	expand_grid(
		`viral load` = 10^seq(0, 11, length.out = 1000),
	) %>% 
	mutate(
		sensitivity = map2_dbl(`viral load`, eta,
				~julia_call("sensitivity.", lfd_test, ..1^..2, need_return = "R")
			)
	) %>%
	ggplot() +
		aes(`viral load`, sensitivity) +
		geom_line(aes(linetype = sprintf("%.2f", `mean sensitivity`))) +
		geom_point(data = tbl_innova_data) +
		scale_x_log10() +
		scale_y_continuous("scaled LFD sensitivity", limits = c(0, 1)) +
		scale_linetype_manual("", 
			breaks = sprintf("%.2f", tbl_scenarios$`mean sensitivity`), 
			values = c(1, 4, 3, 2),
			labels = purrr::map2(
				sprintf("%.2f", tbl_scenarios$`mean sensitivity`),
				tbl_scenarios$eta,
				~bquote( .(..1) (eta == .(round(..2, 2))) )
			)
		) + 
		guides(linetype = guide_legend("LFD mean presymptomatic sensitivity", nrow = 2, byrow = TRUE)) +
		theme(
			legend.position = "top", 
			legend.title = element_text(size = 10)
		)
save_plot(plt, "figA31-fit-mean-sensitivity", width = doublecolwidth, height = height)
