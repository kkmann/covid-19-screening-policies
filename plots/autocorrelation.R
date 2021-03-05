get_ar <- function(params, seed = 42L, nrsmpl = 1e4) {
	julia_call("Random.seed!", as.integer(seed))
	lfd <- lfd(params)
	dm <- julia_call("LarremoreModel", params$gamma, frac_symptomatic = params$frac_symptomatic, need_return = "Julia")
	individuals <- julia_call("Individual.", dm, rep(params$pr_noncovid_symptoms, nrsmpl), need_return = "Julia")
	julia_call("infect!.", individuals, need_return = "Julia")
	for (i in 1:21) {
		if ((i %% 7) %in% 0:4) { # only test on weekends
			julia_call("conduct_test!.", lfd, individuals, need_return = "None")
		}
		julia_call("step!.", individuals, need_return = "None")
	}
	tbl_status <- julia_call("get_status_logs", individuals, need_return = "R") %>% 
		as_tibble()
	tbl_tests <- julia_call("get_test_logs", individuals, need_return = "R") %>% 
		as_tibble() %>% 
		select(-viral_load)
	
	left_join(tbl_tests, tbl_status, by = c("uuid", "day")) %>% 
		select(uuid, day, result, viral_load, probability_positive, symptomatic) %>% 
		group_by(uuid) %>% 
		filter(
			!any(symptomatic) | (row_number() < which(symptomatic)[1])
		) %>%
		summarize(
			acf = list(tibble(
				lag = acf(as.numeric(result), plot = FALSE, lag.max = 7)$lag[,,1],
				ac = acf(as.numeric(result), plot = FALSE, lag.max = 7)$acf[,,1]
			)),
			.groups = "drop"
		) %>% 
		unnest(acf) %>% 
		mutate(
			ac = if_else(is.nan(ac), 1, ac) # constant signal has ac 1
		) %>% 
	group_by(lag) %>% 
	summarize(
		mid = mean(ac), 
		lo = quantile(ac, .05), 
		hi = quantile(ac, .95), 
		.groups = "drop"
	)
}

plt <- tibble(
		coefficient = c(0, .25, .5, .75)
	) %>% 
	mutate(
		ar = map(coefficient, ~get_ar(scenario(ar_coefficient = .), nrsmpl = 1e4))
	) %>% 
	unnest(ar) %>%
	mutate(
		coefficient = sprintf("%.2f", coefficient)
	) %>% 
	ggplot() +
		aes(lag, mid, color = coefficient) +
		geom_line(aes(y = mid)) + 
		scale_color_grey("") +
		ylab("autocorrelation") +
		scale_x_continuous("lag [days]", breaks = seq(0, 21, by = 1)) +
		theme(
			legend.position = "right"
		)

save_plot(plt, "retest-autocorrelation", width = width, height = height/2)