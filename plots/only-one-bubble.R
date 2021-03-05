params <- scenario(n_bubble = 27, bubbles_per_class = 1)
tbl_results <- bind_rows(
	evaluate_performance(
		policies = lst_policies,
		params = scenario(),
		gamma = map_dbl(c(1.5, 3, 6), ~gamma(., scenario())),
		eta = eta(scenario(), target = 0.6)
	),
	evaluate_performance(
		policies = lst_policies,
		params = params,
		gamma = map_dbl(c(1.5, 3, 6), ~gamma(., scenario())),
		eta = eta(scenario(), target = 0.6)
	)
)
dir.create("_site/data", showWarnings = FALSE, recursive = TRUE)
write_rds(tbl_results, "_site/data/tbl_one_bubble.rds")

gamma2rs <- memoise::memoise(function(gamma) round(rzero(gamma, scenario()), 1))

plt1 <- tbl_results %>%
	mutate(
		`bubbles per class` = factor(bubbles_per_class, levels = c(3, 1)),
		Rs = map_dbl(gamma, gamma2rs)
	) %>% 
	ggplot() +
		aes(policy_name, `% infected (cumulative)`, color = `bubbles per class`) +
		geom_boxplot() +
		scale_y_continuous(labels = scales::percent, limits = c(0, NA_real_)) +
		facet_wrap(~Rs, labeller = label_both, nrow = 1) + 
		theme(
			axis.text.x = element_text(angle = 33, hjust = 1),
			axis.title.x = element_blank(),
			legend.position = "top",
			legend.title = element_text()
		)
plt2 <- tbl_results %>%
	mutate(
		`bubbles per class` = factor(bubbles_per_class, levels = c(3, 1)),
		Rs = map_dbl(gamma, gamma2rs)
	) %>% 
	filter(
		`bubbles per class` == 1
	) %>% 
	ggplot() +
		aes(`% infected (cumulative)`, `% schooldays missed (cumulative)`,  color = policy_name, fill = policy_name) +
		facet_grid(`bubbles per class` ~ Rs, labeller = label_both) + 
		geom_abline(slope = 1) +
		geom_point(alpha = .33, shape = 16, size = 2) +
		scale_x_continuous(labels = scales::percent, limits = c(0, NA_real_)) +
		scale_y_continuous(labels = scales::percent, limits = c(0, NA_real_)) +
		scale_color_discrete(guide = guide_legend(override.aes = list(alpha = 1))) +
		coord_cartesian(expand = FALSE) +
		theme(
			legend.text = element_text(size = rel(1.1))
		)
plt <- plt1 + plt2 + plot_layout(ncol = 1, heights = c(1, 1.5)) + plot_annotation(tag_levels = "A")
save_plot(plt, "results-schooldays-missed-vs-infectivity-only-one-bubble", width = width, height = 1.5*height)
