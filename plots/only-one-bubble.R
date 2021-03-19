params_baseline <- scenario()
params <- scenario(n_bubble = 27, bubbles_per_class = 1)

match_gamma1 <- function(r) {
	uniroot(function(x) gamma(r, params_baseline) - gamma(x, params), interval = c(r, 3*r))$root
}
match_gamma2 <- function(r) {
	uniroot(function(x) gamma(x, params_baseline) - gamma(r, params), interval = c(r/3, r/2))$root
}

match_gamma1(6)

# we want the same gamma as under the baseline scenario
tbl_results <- bind_rows(
	evaluate_performance(
		policies = lst_policies,
		params = params_baseline,
		rzero = c(1.5, 3, 6),
		mean_sensitivity = 0.6
	) %>% 
	mutate(Rs = as.numeric(as.character(Rs))),
	evaluate_performance(
		policies = lst_policies,
		params = params,
		rzero = map_dbl(c(1.5, 3, 6), match_gamma1),
		mean_sensitivity = 0.6
	) %>% 
	mutate(
		Rs_star = Rs,
		Rs = map_dbl(Rs, match_gamma2) %>% round(1),
	)
) %>% 
mutate(
	`Rs*` = factor(Rs),
	`bubbles per class` = factor(bubbles_per_class, levels = c(3, 1))
)
plt1 <- tbl_results %>%
	ggplot() +
		aes(policy_name, `% infected (cumulative)`, color = `bubbles per class`) +
		geom_boxplot() +
		scale_y_continuous(labels = scales::percent, limits = c(0, NA_real_)) +
		facet_wrap(~`Rs*`, labeller = label_both, nrow = 1) + 
		theme(
			axis.text.x = element_text(angle = 33, hjust = 1),
			axis.title.x = element_blank(),
			legend.position = "top",
			legend.title = element_text()
		)
plt2 <- tbl_results %>%
	filter(
		`bubbles per class` == 1
	) %>% 
	ggplot() +
		aes(`% infected (cumulative)`, `% schooldays missed (cumulative)`,  color = policy_name, fill = policy_name) +
		facet_grid(`bubbles per class` ~ `Rs*`, labeller = label_both) + 
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
save_plot(plt, "sensitivity-one-bubble", width = width, height = 1.33*height)
