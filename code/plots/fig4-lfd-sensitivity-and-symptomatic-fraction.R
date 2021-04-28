params <- scenario()
tbl_results <- evaluate_performance(
		policies = lst_policies,
		params = params,
		rzero = c(1.5, 3, 6),
		mean_sensitivity = c(0.6),
		frac_symptomatic = c(0.25, 0.5, 0.75)
	)
tbl_results2 <- evaluate_performance(
		policies = lst_policies[c("Mon/Wed screening", "Mon screening", "test for release")],
		params = params,
		mean_sensitivity = c(.2, .4, .6, .8),
		ar_coefficient = c(0, 0.75)
)

plt1 <- tbl_results %>% 
	ggplot() +
		aes(policy_name, `% infected (cumulative)`, color = factor(1 - frac_symptomatic)) +
		geom_boxplot(size = .33) +
		scale_y_continuous(labels = scales::percent, limits = c(0, NA_real_)) +
		scale_color_discrete("% asymptomatic cases:") +
		facet_grid(Rs ~ `mean sensitivity`, labeller = label_both) +
		theme(
			axis.text.x = element_text(angle = 33, hjust = 1),
			axis.title.x = element_blank(),
			legend.position = "top",
			legend.title = element_text(),
			panel.spacing = unit(.5, "lines")
		)
plt3 <- tbl_results2 %>%
	filter(ar_coefficient == 0) %>% 
	ggplot() +
		aes(
			factor(`mean sensitivity`), 
			`% infected (cumulative)`, 
			color = factor(policy_name, levels = tbl_results$policy_name %>% levels())
		) +
		geom_boxplot(size = .33) +
		scale_y_continuous(labels = scales::percent, limits = c(0, NA_real_)) +
		scale_color_discrete("", drop = "FALSE") +
		facet_wrap(~Rs, labeller = label_both)  +
		guides(color = guide_legend(nrow = 3)) + 
		theme(
			axis.title.x = element_blank(),
			legend.position = "top",
			legend.title = element_text()
		) 

plt <- plt1 + plt3 + plot_layout(nrow = 2, heights = c(3, 1)) + plot_annotation(tag_levels = "A")
save_plot(plt, "fig4-lfd-sensitivity-and-symptomatic-fraction", width = width, height = 3*height)
