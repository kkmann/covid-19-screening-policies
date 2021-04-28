params <- scenario()
tbl_results2 <- evaluate_performance(
		policies = lst_policies[c("Mon/Wed screening", "Mon screening", "test for release")],
		params = params,
		mean_sensitivity = c(.2, .4, .6, .8),
		ar_coefficient = c(0, 0.75)
	)
plt <- tbl_results2 %>%
	rename(
		`mean sens.` = `mean sensitivity`,
		`% infected` = `% infected (cumulative)`
	) %>% 
	ggplot() +
		aes(policy_name, `% infected`, color = factor(ar_coefficient)) +
		geom_boxplot(size = .33) +
		scale_y_continuous(labels = scales::percent, limits = c(0, NA_real_)) +
		scale_color_discrete("AR coefficient:") +
		facet_grid(Rs ~ `mean sens.`, labeller = label_both) +
		theme(
			axis.text.x = element_text(angle = 33, hjust = 1),
			axis.title.x = element_blank(),
			legend.position = "top",
			legend.title = element_text(),
			panel.spacing = unit(.25, "lines")
		)
save_plot(plt, "figA52-sensitivity-autocorrelation", width = doublecolwidth, height = height)
