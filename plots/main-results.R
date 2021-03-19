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
		facet_wrap(~name, scales = "free_y", ncol = 1) +
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
	ggplot() +
		aes(`% infected (cumulative)`, `% schooldays missed (cumulative)`,  color = policy_name, fill = policy_name) +
		# facet_grid(`mean sensitivity` ~ Rs, labeller = label_both) +
		geom_abline(slope = 1) +
		scale_x_continuous(labels = scales::percent, limits = c(0, NA_real_)) +
		scale_y_continuous(labels = scales::percent, limits = c(0, NA_real_)) +
		geom_point(alpha = .33, shape = 16, size = 2) +
		scale_color_discrete(guide = guide_legend(override.aes = list(alpha = 1))) +
		coord_cartesian(expand = FALSE) +
		theme(
			legend.text = element_text(size = rel(1.1))
		)
plt <- plt_boxplots_infected_schooldays / 
	(plt_boxplots_n_tests + (plt_scatter + theme(axis.title.x = element_text(margin = margin(t = -50, unit = "pt")))) ) + 
	plot_annotation(tag_levels = "A") +
	plot_layout(heights = c(1, 2), guides = "collect") &
	theme(legend.position = "bottom")
save_plot(plt, "results-main", width = width, height = 1.5*height)

plt1 <- tbl_results %>% 
	ggplot() +
		aes(policy_name, `% infected (cumulative)`, color = factor(1 - frac_symptomatic)) +
		geom_boxplot() +
		scale_y_continuous(labels = scales::percent, limits = c(0, NA_real_)) +
		scale_color_discrete("% asymptomatic cases:") +
		facet_grid(`mean sensitivity` ~ Rs, labeller = label_both) +
		theme(
			axis.text.x = element_text(angle = 33, hjust = 1),
			axis.title.x = element_blank(),
			legend.position = "top",
			legend.title = element_text()
		)
tbl_results2 <- evaluate_performance(
		policies = lst_policies[c("Mon/Wed screening", "Mon screening", "test for release")],
		params = params,
		mean_sensitivity = c(.4, .6, .8),
		ar_coefficient = c(0, 0.75)
	)
plt3 <- tbl_results2 %>%
	filter(ar_coefficient == 0) %>% 
	ggplot() +
		aes(policy_name, `% infected (cumulative)`, color = factor(`mean sensitivity`)) +
		geom_boxplot() +
		scale_y_continuous(labels = scales::percent, limits = c(0, NA_real_)) +
		scale_color_discrete("mean sensitivity:") +
		facet_wrap(~Rs, labeller = label_both) +
		theme(
			axis.text.x = element_text(angle = 33, hjust = 1),
			axis.title.x = element_blank(),
			legend.position = "top",
			legend.title = element_text()
		)

plt <- plt1 + plt3 + plot_layout(nrow = 2) + plot_annotation(tag_levels = "A")
save_plot(plt, "sensitivity-symptomatic-lfd-sensitivity", width = width, height = 1.5*height)

plt <- tbl_results2 %>%
	ggplot() +
		aes(policy_name, `% infected (cumulative)`, color = factor(ar_coefficient)) +
		geom_boxplot() +
		scale_y_continuous(labels = scales::percent, limits = c(0, NA_real_)) +
		scale_color_discrete("AR coefficient:") +
		facet_grid(Rs ~ `mean sensitivity`, labeller = label_both) +
		theme(
			axis.text.x = element_text(angle = 33, hjust = 1),
			axis.title.x = element_blank(),
			legend.position = "top",
			legend.title = element_text()
		)
save_plot(plt, "sensitivity-autocorrelation", width = width, height = .66*height)
