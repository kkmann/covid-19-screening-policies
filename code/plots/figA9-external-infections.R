params <- scenario()
tbl_results <- evaluate_performance(
		policies = lst_policies,
		params = params,
		expected_weekly_external_infections = c(1, 2, 3)
	)

plt1 <- tbl_results %>% 
	rename(
		`% infected` = `% infected (cumulative)`,
		`% schooldays missed` = `% schooldays missed (cumulative)`
	) %>%
	ggplot() +
		aes(policy_name, `% infected`, color = as.factor(expected_weekly_external_infections)) +
		geom_boxplot(size = .33) +
		scale_y_continuous(labels = scales::percent, limits = c(0, NA_real_)) +
		scale_color_discrete("E[ext. infections/w]:") +
		facet_wrap(~Rs, labeller = label_both, nrow = 1) + 
		theme(
			axis.text.x = element_text(angle = 33, hjust = 1),
			axis.title.x = element_blank(),
			legend.position = "top",
			legend.title = element_text()
		)

plt2 <- tbl_results %>%
	rename(
		`% infected` = `% infected (cumulative)`,
		`% schooldays missed` = `% schooldays missed (cumulative)`,
		`E[ext. infections/w]` = expected_weekly_external_infections
	) %>%
	ggplot() +
		aes(`% infected`, `% schooldays missed`,  color = policy_name, fill = policy_name) +
		facet_grid(Rs ~ `E[ext. infections/w]`, labeller = label_both) +
		geom_abline(slope = 1) +
		geom_point(alpha = .33, shape = 16, size = 1) +
		scale_x_continuous(labels = scales::percent, limits = c(0, NA_real_)) +
		scale_y_continuous(labels = scales::percent, limits = c(0, NA_real_)) +
		scale_color_discrete(guide = guide_legend(override.aes = list(alpha = 1), ncol = 3)) +
		coord_cartesian(expand = FALSE) +
		theme(
			legend.text = element_text(size = rel(1.1))
		)
plt <- plt1 + plt2 + plot_layout(ncol = 1, heights = c(1, 1.5)) + plot_annotation(tag_levels = "A")
save_plot(plt, "figA9-external-infections", width = doublecolwidth, height = 2*height)
