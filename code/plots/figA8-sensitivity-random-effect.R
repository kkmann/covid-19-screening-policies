# boxplots of cumulative infection =============================================
params_baseline <- scenario()
tbl_results <- evaluate_performance(
		policies = lst_policies[c("test for release", "Mon screening", "Mon/Wed screening")],
		params = params_baseline,
		rzero = c(1.5, 3, 6),
		mean_sensitivity = c(0.6),
		lfd_ranef = c(0, 1.5)
	) %>%
	mutate(
		`random effect` = sprintf("%.1f", lfd_ranef)
	)

plt_boxplots <- tbl_results %>% 
	ggplot() +
		aes(policy_name, `% infected (cumulative)`, color = `random effect`) +
		geom_boxplot(size = .33) +
		scale_y_continuous(labels = scales::percent, limits = c(0, NA_real_)) +
		scale_color_discrete("random effect:") +
		facet_wrap(~Rs, labeller = label_both, nrow = 1) + 
		theme(
			axis.text.x = element_text(angle = 33, hjust = 1),
			axis.title.x = element_blank(),
			legend.position = "none"
		)

# plot trajectories ============================================================
individuals <- julia_call("Individual.", 
	disease_model(params_baseline), 
	rep(params_baseline$pr_noncovid_symptoms, 100), 
	need_return = "Julia"
)
julia_call("infect!.", individuals, need_return = "None")
julia_call("steps!.", individuals, 30L, need_return = "None")
tbl_u <- tibble(
	uuid = julia_call("string.",
			julia_call("getproperty.",  
				individuals, julia_call("Symbol", "uuid")
			)
		),
	u = julia_call("getproperty.", 
			individuals, julia_call("Symbol", "u_sensitivity", need_return = "Julia")
		)
)
tbl <- julia_call("get_status_logs", individuals, need_return = "R") %>% 
	as_tibble() %>% 
	left_join(tbl_u, by = "uuid") %>% 
	mutate(
		`0.0` = map2_dbl(viral_load, u, 
				~julia_call("get_probability_positive", 
					 lfd(params_baseline), ..1, u = ..2)
			),
		`1.5` = map2_dbl(viral_load, u, 
				~{
					tmp <- params_baseline
					tmp$lfd_ranef = 1.5
					julia_call("get_probability_positive", lfd(tmp), ..1, u = ..2)
				}
			)
	) %>% 
	pivot_longer(c(`0.0`, `1.5`), names_to = "random effect", values_to = "sensitivity") %>%
	pivot_longer(c(infection_probability, sensitivity)) %>%
	mutate(
		symptomatic = factor(symptomatic, levels = c(FALSE, TRUE), labels = c("non-symptomatic", "symptomatic"))
	) %>%
	filter(
		viral_load > 0
	) %>%
	group_by(uuid) %>% 
	mutate(
		`any symptomatic` = any(symptomatic == "symptomatic")
	) %>% 
	ungroup()

plt_viral_load <- tbl %>% 
	filter(name == "infection_probability" & `random effect` == "0.0") %>% 
	ggplot() +
		aes(day, viral_load, group = uuid) +
	 	geom_hline(yintercept = params_baseline$lli) +
		geom_line(alpha = 0.1) +
		scale_y_log10() +
		scale_x_continuous("day post infection", breaks = seq(0, 35, by = 7)) +
		ylab("viral load")

plt_infection_probability <- tbl %>% 
	filter(name == "infection_probability" & `random effect` == "0.0") %>% 
	ggplot() +
		aes(day, value) +
		geom_line(aes(group = uuid), alpha = 0.1) +
		scale_x_continuous("day post infection", breaks = seq(0, 35, by = 7)) +
		scale_y_continuous("infection probability", limits = c(0, 1), breaks = seq(0, 1, by = .1), labels = scales::percent)

plt_sensitivity <- tbl %>% 
	filter(name == "sensitivity") %>% 
	ggplot() +
		aes(day, value, color = `random effect`) +
		geom_line(aes(group = interaction(uuid, `random effect`)), alpha = 0.1) +
		geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE) +
		scale_x_continuous("day post infection", breaks = seq(0, 35, by = 7)) +
		scale_y_continuous("scaled LFD sensitivity", limits = c(0, 1), breaks = seq(0, 1, by = .1), labels = scales::percent) +
		scale_color_discrete("random effect:") +
		guides(color = guide_legend(override.aes = list(alpha = 1) ) ) +
		theme(
			legend.title = element_text()
		)

plt <- plt_viral_load / 
	(plt_infection_probability + plt_sensitivity) / 
	plt_boxplots +
	plot_annotation(tag_levels = "A") +
	plot_layout(guides = "collect")
save_plot(plt, "figA8-sensitivity-random-effects", width = doublecolwidth, height = 3*height)
