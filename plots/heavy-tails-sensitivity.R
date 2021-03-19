# boxplots of cumulative infection =============================================
scale <- 3/sqrt(3)
params_baseline <- scenario(scale = 0.0, l = 3, df = 3.0)
f <- function(scale) {
	params <- params_baseline
	params$scale <- scale
	evaluate_performance(
		policies = lst_policies,
		params = params,
		rzero = c(1.5, 3, 6)
	) %>%
	mutate(
		scale = if_else(scale == 0, "default Larremore", "additional VL noise") 
	)
}
tbl_results <- bind_rows(f(0), f(scale))

plt_boxplots <- tbl_results %>% 
	ggplot() +
		aes(policy_name, `% infected (cumulative)`, color = scale) +
		geom_boxplot() +
		scale_y_continuous(labels = scales::percent, limits = c(0, NA_real_)) +
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
tbl_baseline <- julia_call("get_status_logs", individuals, need_return = "R") %>% 
	as_tibble() %>% 
	left_join(tbl_u, by = "uuid") %>% 
	mutate(
		type = "default Larremore",
		sensitivity = map2_dbl(viral_load, u, 
				~julia_call("get_probability_positive", 
					 lfd(params_baseline), ..1, u = ..2)
			)
	)
params <- params_baseline
params$scale <- scale
individuals <- julia_call("Individual.", 
	disease_model(params), 
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
		type = "additional VL noise",
		sensitivity = map2_dbl(viral_load, u, 
				~julia_call("get_probability_positive", 
					 lfd(params), ..1, u = ..2)
			)
	)

tbl_plot <- bind_rows(
		tbl_baseline,
		tbl
	) %>% 
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

plt_viral_load <- tbl_plot %>% 
	ggplot() +
		aes(day, viral_load, group = uuid, color = type) +
	 	geom_hline(yintercept = params_baseline$lli) +
		geom_line(alpha = 0.2) +
		scale_y_log10() +
		scale_x_continuous("day post infection", breaks = seq(0, 35, by = 7)) +
		ylab("viral load") +
		guides(color = guide_legend(override.aes = list(alpha = 1) ) )

plt_infection_probability <- tbl_plot %>% 
	ggplot() +
		aes(day, infection_probability, color = type) +
		geom_line(aes(group = uuid), alpha = 0.2) +
		scale_x_continuous("day post infection", breaks = seq(0, 35, by = 7)) +
		scale_y_continuous("infection probability", limits = c(0, 1), breaks = seq(0, 1, by = .1), labels = scales::percent) +
		guides(color = guide_legend(override.aes = list(alpha = 1) ) )

plt_sensitivity <- tbl_plot %>% 
	ggplot() +
		aes(day, sensitivity, color = type) +
		geom_line(aes(group = uuid), alpha = 0.2) +
		# geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE) +
		scale_x_continuous("day post infection", breaks = seq(0, 35, by = 7)) +
		scale_y_continuous("scaled LFD sensitivity", limits = c(0, 1), breaks = seq(0, 1, by = .1), labels = scales::percent) +
		guides(color = guide_legend(override.aes = list(alpha = 1) ) )
	
plt <- plt_viral_load / 
	(plt_infection_probability + plt_sensitivity) / 
	plt_boxplots +
	plot_annotation(tag_levels = "A") +
	plot_layout(guides = "collect")
save_plot(plt, "sensitivity-heavy-tails", height = 1.5*height)
