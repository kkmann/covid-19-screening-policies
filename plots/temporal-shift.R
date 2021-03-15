params_baseline <- scenario()
params_baseline$gamma <- gamma(3.0, params_baseline)

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
params04 <- params_baseline
params04$eta <- eta(params04, target = 0.4)
params08 <- params_baseline
params08$eta <- eta(params08, target = 0.8)
tbl <- julia_call("get_status_logs", individuals, need_return = "R") %>% 
	as_tibble() %>% 
	left_join(tbl_u, by = "uuid") %>% 
	mutate(
		`0.4` = map2_dbl(viral_load, u, ~julia_call("get_probability_positive", lfd(params04), ..1, u = ..2) ),
		`0.8` = map2_dbl(viral_load, u, ~julia_call("get_probability_positive", lfd(params08), ..1, u = ..2) )
	) %>% 
	pivot_longer(c(`0.4`, `0.8`), names_to = "mean sensitivity", values_to = "sensitivity") %>%
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

plt_sensitivity <- tbl %>% 
	filter(name == "sensitivity") %>% 
	ggplot() +
		aes(day, value, color = `mean sensitivity`) +
		geom_line(aes(group = interaction(uuid, `mean sensitivity`)), alpha = 0.1) +
		geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE) +
		scale_x_continuous("day post infection", breaks = seq(0, 35, by = 7)) +
		scale_y_continuous("LFD sensitivity", limits = c(0, 1), breaks = seq(0, 1, by = .1), labels = scales::percent) +
		scale_color_discrete("mean sensitivity:") +
		guides(color = guide_legend(override.aes = list(alpha = 1) ) ) +
		theme(
			legend.title = element_text()
		)
save_plot(plt_sensitivity, "temporal-impact-of-sensitivity", height = .66*height)
