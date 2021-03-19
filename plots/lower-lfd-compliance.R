plt <- tibble(
		compliance = rbeta(10^5, shape1 = 2/15, shape2 = 1/15)
	) %>% 
	ggplot() +
		aes(compliance) +
		geom_histogram(aes(y = ..ncount..), binwidth = 0.05) +
		scale_y_continuous("", labels = scales::percent)
save_plot(plt, "beta-distribution", width = width, height = .66*height)


params <- scenario(a = 2/15, b = 1/15)
tbl_results <- bind_rows(
	evaluate_performance(
		policies = lst_policies[c("Mon/Wed screening", "Mon screening", "test for release")],
		params = scenario(), # standard compliance
		rzero = c(1.5, 3, 6)
	),
	evaluate_performance(
		policies = lst_policies,
		params = params,
		rzero = c(1.5, 3, 6)
	)
)

plt1 <- tbl_results %>% 
	filter(
		policy_name %in% c("Mon/Wed screening", "Mon screening", "test for release")
	) %>% 
	mutate(
		tmp = if_else(is.finite(a), a / (a + b), 1),
		`mean LFD test compliance:` = factor(
			tmp,
			levels = unique(tmp), 
			labels = sprintf("%.2f", unique(tmp))
		)
	) %>% 
	ggplot() +
		aes(policy_name, `% infected (cumulative)`, color = `mean LFD test compliance:`) +
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
		tmp = if_else(is.finite(a), a / (a + b), 1),
		`mean LFD test compliance:` = factor(
			tmp,
			levels = unique(tmp), 
			labels = sprintf("%.2f", unique(tmp))
		)
	) %>%
	filter(
		`mean LFD test compliance:` == "0.67"
	) %>% 
	ggplot() +
		aes(`% infected (cumulative)`, `% schooldays missed (cumulative)`,  color = policy_name, fill = policy_name) +
		facet_grid(`mean sensitivity` ~ Rs, labeller = label_both) +
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
save_plot(plt, "sensitivity-lfd-compliance", width = width, height = 1.33*height)

