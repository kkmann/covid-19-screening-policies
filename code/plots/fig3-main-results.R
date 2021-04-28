params <- scenario()
tbl_results <- evaluate_performance(
		policies = lst_policies,
		params = params,
		rzero = c(1.5, 3, 6),
		mean_sensitivity = c(0.6),
		frac_symptomatic = c(0.25, 0.5, 0.75)
	)

plt_boxplots_infected_schooldays <- tbl_results %>% 
	filter(
		Rs == 3.0,
		frac_symptomatic == 0.5
	) %>% 
	select(
		policy_name,
		`% infected (cumulative)`,
		`% schooldays missed (cumulative)`
	) %>% 
	pivot_longer(-policy_name) %>% 
	mutate(
		name = str_remove(name, " \\(cumulative\\)$")
	) %>% 
	ggplot() +
		aes(policy_name, value) +
		geom_boxplot() +
		scale_y_continuous("", labels = scales::percent, limits = c(0, NA_real_)) +
		facet_wrap(~name) +
		theme(
			axis.text.x = element_text(angle = 33, hjust = 1),
			axis.title.x = element_blank(),
			legend.position = "right"
		)
plt_boxplots_n_tests <- tbl_results %>% 
	filter(
		Rs == 3.0,
		frac_symptomatic == 0.5
	) %>% 
	transmute(
		policy_name,
		LFD = n_lfd_tests/n_school(params),
		PCR = n_pcr_tests/n_school(params)
	) %>% 
	pivot_longer(-policy_name) %>% 
	ggplot() +
		aes(policy_name, value) +
		geom_boxplot() +
		scale_y_continuous("average number of tests", limits = c(0, NA_real_)) +
		facet_wrap(~name, nrow = 1, scales = "free_y") +
		theme(
			axis.text.x = element_text(angle = 33, hjust = 1),
			axis.title.x = element_blank(),
			legend.position = "right"
		)
plt_scatter <- tbl_results %>%
	filter(
		frac_symptomatic == .5,
		Rs == 3
	) %>% 
	rename(
		`% infected` = `% infected (cumulative)`,
		`% schooldays missed` = `% schooldays missed (cumulative)`
	) %>% 
	ggplot() +
		aes(`% infected`, `% schooldays missed`,  color = policy_name, fill = policy_name) +
		geom_abline(slope = 1) +
		scale_x_continuous(labels = scales::percent, limits = c(0, NA_real_)) +
		scale_y_continuous(labels = scales::percent, limits = c(0, NA_real_)) +
		geom_point(alpha = .33, shape = 16, size = 2) +
		coord_cartesian(expand = FALSE) +
		guides(
			color = guide_legend(ncol = 2, override.aes = list(alpha = 1)) 
		) +
		theme(
			legend.text = element_text(size = rel(1.1)),
			# axis.title.x = element_text(margin = margin(t = -50, unit = "pt"))
		)
plt <- plt_boxplots_infected_schooldays / 
	plt_scatter /
	plt_boxplots_n_tests +
	plot_annotation(tag_levels = "A") +
	plot_layout(heights = c(1, 2.5, 1))
save_plot(plt, "fig3-results-main", width = width, height = 3*height)
