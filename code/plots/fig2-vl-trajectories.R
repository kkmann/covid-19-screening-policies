params <- scenario()

dm <- julia_call("LarremoreModel", 
	gamma(R = 3, params), frac_symptomatic = params$frac_symptomatic, 
	l10vl_clearance = log10(params$lli),
	need_return = "Julia"
)
individuals <- julia_call("Individual.", dm, rep(params$pr_noncovid_symptoms, 20), need_return = "Julia")
julia_call("infect!.", individuals, need_return = "Julia")
julia_call("steps!.", individuals, 21L, need_return = "Julia")

plt <- julia_call("get_status_logs", individuals, need_return = "R") %>% 
	as_tibble() %>%
	mutate(
		symptomatic = factor(symptomatic, levels = c(FALSE, TRUE), labels = c("non-symptomatic", "symptomatic"))
	) %>%
	ggplot() +
		aes(day, viral_load) +
	 	geom_hline(yintercept = params$lli) +
		geom_bar(aes(fill = symptomatic), stat = "identity", alpha = .66, width = 1) +
		scale_y_log10() +
		scale_x_continuous("day post infection", breaks = seq(0, 35, by = 7)) +
		scale_fill_manual(breaks = c("non-symptomatic", "symptomatic"), values = c("darkgray", "black")) +
		ylab("viral load") +
		facet_wrap(~uuid, ncol = 4) +
		theme(
			strip.text = element_blank(),
			strip.background = element_blank(),
			panel.spacing = unit(.25, "lines")
		)
save_plot(plt, "fig2-vl-trajectories", width = width, height = 2*height)
