params <- list(
	iterations = 100L
)

## ----r-setup, include=FALSE, cache=FALSE----------------------------------------------------------------------------------------------------------------------------------
library(JuliaCall)
library(tidyverse)
library(tidygraph)
library(ggraph)
library(patchwork)
library(pscl)

source("code/util.R")

width <- 8
height <- width/1.61

knitr::opts_chunk$set(
	cache = TRUE,
	echo = FALSE,
	fig.width = width,
	fig.height = height
)

JuliaCall::julia_setup(force = TRUE)
JuliaCall::julia_source("code/setup.jl")


## ----plot-school-structure------------------------------------------------------------------------------------------------------------------------------------------------
          n_bubble <- 9
 bubbles_per_class <- 3
classes_per_school <- 12

 n_class <- n_bubble * bubbles_per_class
n_school <- classes_per_school * n_class

pr_meet_class <- 3/(n_class - 1)
pr_meet_school <- 1/(n_school - 1)

plt <- julia_call("school", 
		n_bubble, bubbles_per_class, classes_per_school, pr_meet_class, pr_meet_school, 
		0.0, # gamma
		0.5, # frac symptomatic
		0.01 # pr_noncov_symptoms
	) %>% 
	julia_call("get_adjacency_matrix", .) %>% 
	as_tbl_graph() %>% 
	ggraph("matrix") +
		geom_edge_tile(aes(fill = weight)) + 
		scale_edge_fill_continuous(
			"", 
			low = "#dddddd", high = "#000000", na.value = "white",
			limits = c(0, NA_real_)
		) +
		coord_fixed(expand = FALSE) +
		theme(
			legend.position = "right"
		) +
		labs(
			caption = "Adjacency matrix of school population;\r\nconnection strength is 'expected pairwise daily risk contacts' (across all shared groups)."
		)
save_plot(plt, "adjacency-matrix", width = 2*width/3, height = 2*height/3)



## ----calibrate-infectivity------------------------------------------------------------------------------------------------------------------------------------------------
tbl_r_zero <- tibble(
	gamma = seq(0, 0.1, length.out = 1000),
	R = julia_call("simulate_r_zero.", 
		julia_call("school.", 
			n_bubble, bubbles_per_class, classes_per_school, pr_meet_class, pr_meet_school, 
			gamma,
			0.5, # frac symptomatic
			0.01 # pr_noncov_symptoms
		)
	)
)

fit <- zeroinfl(formula = R ~ gamma | gamma, dist = "negbin", data = tbl_r_zero)

# define inverse function
get_gamma <- function(R) {
	f <- function(gamma) as.numeric(predict(fit, newdata = tibble(gamma = gamma), type = "response"))
	as.numeric(uniroot(function(x) f(x) - R, interval = c(0, .1))$root)
}

plt <- ggplot(tbl_r_zero) +
	aes(gamma, R) +
	geom_point(alpha = 0.2, shape = 16) +
	geom_line(
		data = tibble(
			gamma = seq(0, .1, length.out = 100),
			R = as.numeric(predict(fit, newdata = tibble(gamma = gamma), type = "response"))
		)
	) +
	labs(
		x = expression(gamma),
		caption = "Fitted zero-inflated negative binomial regression for r-zero vs. infectivity."
	)
save_plot(plt, "calibrating-infectivity", width = width, height = height/2)



## ----plot-vl-trajectories, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------
dm <- julia_call("LarremoreModel", get_gamma(R = 3), frac_symptomatic = 0.5, need_return = "Julia")
individuals <- julia_call("Individual.", dm, rep(.01, 32), need_return = "Julia")
julia_call("infect!.", individuals, need_return = "Julia")
julia_call("steps!.", individuals, 21L, need_return = "Julia")

plt <- julia_call("get_status_logs", individuals, need_return = "R") %>% 
	as_tibble() %>%
	mutate(
		symptomatic = factor(symptomatic, levels = c(FALSE, TRUE), labels = c("non-symptomatic", "symptomatic"))
	) %>%
	ggplot() +
		aes(day, viral_load) +
	 	geom_hline(yintercept = 1e6) +
		geom_bar(aes(fill = symptomatic), stat = "identity", alpha = .66, width = 1) +
		scale_y_log10() +
		scale_x_continuous("day post infection", breaks = seq(0, 35, by = 7)) +
		scale_fill_manual(breaks = c("non-symptomatic", "symptomatic"), values = c("darkgray", "black")) +
		ylab("viral load") +
		facet_wrap(~uuid, nrow = 4) +
		theme(
			strip.text = element_blank(),
			strip.background = element_blank()
		)
save_plot(plt, "vl-trajectories")



## ----define-pcr-test------------------------------------------------------------------------------------------------------------------------------------------------------
pcr <- julia_call("FixedTest", "pcr", 0.975, 1.0, lod = 300.0, need_return = "Julia")


## ----fit-innova-data------------------------------------------------------------------------------------------------------------------------------------------------------
# screen grabbed pixels
tbl_innova_data <- tibble(
	`viral load` = 10^(2:7),
	sensitivity = 1 - (c(621, 521, 344, 207, 144, 123) - 114) / (684 - 114)
)

sensitivity <- function(vl, slope, intercept) {
	lfd <- julia_call("LogRegTest", "lfd", slope, intercept, 0.998, need_return = "Julia")
	julia_call("sensitivity.", lfd, vl, need_return = "R")
}

innova <- optim(
	c(.76, -5), 
	function(x) {
		sum((
			tbl_innova_data$sensitivity - 
			sensitivity(tbl_innova_data$`viral load`, slope = x[1], intercept = x[2])
		)^2)
	},
	method = "L-BFGS-B",
	lower = c(.1, -10),
	upper = c(2.5, 5),
	control = list(maxit = 1e4)
)
innova$slope <- innova$par[1]
innova$intercept <- innova$par[2]


## ----recalibrate-innova-lfd-test------------------------------------------------------------------------------------------------------------------------------------------
dm <- julia_call("LarremoreModel", get_gamma(R = 3), frac_symptomatic = 0.5, need_return = "Julia")
individuals <- julia_call("Individual.", dm, rep(.01, 1e5), need_return = "Julia")
julia_call("infect!.", individuals, need_return = "Julia")
julia_call("steps!.", individuals, 21L, need_return = "Julia")

tbl_vl <- julia_call("get_status_logs", individuals) %>% 
	as_tibble() %>% 
	arrange(uuid, day) %>%
	group_by(uuid) %>%
	filter(
		row_number() >= which(viral_load > 300)[1],
		row_number() < which(symptomatic)[1]
	) %>%
	sample_n(1) %>%
	ungroup() %>%
	select(uuid, day, viral_load)

mean_sensitivity <- function(eta) {
	tbl_vl %>%
		mutate(
			sensitivity = sensitivity(viral_load^eta, slope = innova$slope, intercept = innova$intercept)
		) %>%
		pull(sensitivity) %>%
		summary() %>%
		mean()
}

# define inverse
get_eta <- function(sens) uniroot(function(x) mean_sensitivity(x) - sens, interval = c(0, 5))$root

tbl_scenarios <- tibble(
		`mean sensitivity` = c(mean_sensitivity(1), .4, .6, .8),
		eta = map_dbl(`mean sensitivity`, get_eta)
	)

plt <- tbl_scenarios %>% 
	expand_grid(
		`viral load` = 10^seq(0, 11, length.out = 1000),
	) %>% 
	mutate(
		sensitivity = sensitivity(`viral load`^eta, slope = innova$slope, intercept = innova$intercept)
	) %>%
	ggplot() +
		aes(`viral load`, sensitivity) +
		geom_line(aes(linetype = sprintf("%.2f", `mean sensitivity`))) +
		geom_point(data = tbl_innova_data) +
		scale_x_log10() +
		scale_y_continuous("", limits = c(0, 1)) +
		scale_linetype_manual("", 
			breaks = sprintf("%.2f", tbl_scenarios$`mean sensitivity`), 
			values = c(1, 4, 3, 2),
			labels = purrr::map2(
				sprintf("%.2f", tbl_scenarios$`mean sensitivity`),
				tbl_scenarios$eta,
				~bquote( .(..1) (eta == .(round(..2, 2))) )
			)
		) +
		labs(
			caption = "Fitted Innova LFD test sensitivity."
		) +
		theme(
			legend.position = "right"
		)
save_plot(plt, "fit-sensitivity", width = width, height = height/2)



## ----plot-acf-------------------------------------------------------------------------------------------------------------------------------------------------------------
get_ar <- function(ar_coefficient = 0, ar_window = 0L, seed = 42L, nrsmpl = 1e4) {
	julia_call("Random.seed!", as.integer(seed))
	lfd <- julia_call("LogRegTest", 
		"lfd", innova$slope, innova$intercept, 0.998, ar_window = ar_window, ar_coefficient = ar_coefficient,
		need_return = "Julia"
	)
	dm <- julia_call("LarremoreModel", get_gamma(R = 3), frac_symptomatic = 0.5, need_return = "Julia")
	individuals <- julia_call("Individual.", dm, rep(.01, nrsmpl), need_return = "Julia")
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
		ar = map(coefficient, ~get_ar(., 3L, nrsmpl = 1e4))
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

save_plot(plt, "test-autocorrelation", width = width, height = height/2)



## ----plot-sensitivity-vs-infection-probability----------------------------------------------------------------------------------------------------------------------------
plt <- expand_grid(
		tibble(
			`mean sensitivity` = c(.4, .6, .8),
			eta = map_dbl(`mean sensitivity`, get_eta)
		),
		tibble(
			R = c(1.5, 3, 6),
			gamma = map_dbl(R, get_gamma)
		)
	) %>% 
	expand_grid(
		`viral load` = 10^seq(0, 11, length.out = 1000)
	) %>% 
	mutate(
		sensitivity = sensitivity(`viral load`^eta, slope = innova$slope, intercept = innova$intercept),
		`probability to infect` = pmax(0, pmin(1, gamma * (log10(`viral load`) - 6)))
	) %>% 
	ggplot() +
		aes(`probability to infect`) +
		geom_line(aes(y = sensitivity)) +
		facet_grid(
			`mean sensitivity` ~ R, 
			 labeller = label_both
		) +
		ylab("sensitivity")
save_plot(plt, "sensitivity-vs-infectivity", width = width, height = height)



## ----define-scenarios-----------------------------------------------------------------------------------------------------------------------------------------------------
tbl_sim_scenarios <- bind_rows(
		# fully crossed for normal pr_meet_class
		tibble(
			n_bubble = n_bubble,
			bubbles_per_class = bubbles_per_class,
			classes_per_school = classes_per_school,
			pr_meet_class = pr_meet_class,
			pr_meet_school = pr_meet_school,
		) %>% expand_grid(
			tibble(
				R = c(1.5, 3, 6),
				gamma = purrr::map_dbl(R, get_gamma)
			),
			frac_symptomatic = c(0.25, 0.5, 0.75),
			pr_external_infections = 1/(n_school*7), # expected 1 per week
			pr_noncovid_symptoms = .01,
			pcr = list(pcr),
			tibble(
				mean_sensitivity = c(.4, .6, .8),
				eta = purrr::map_dbl(mean_sensitivity, get_eta)
			),
			ar_coefficient = c(0.0, 0.75)
		) %>%
		mutate(
			slope = innova$slope, intercept = innova$intercept,
			ar_window = 3L
		),
		# reduced set for pr_meet_class = 1
		tibble(
			n_bubble = 27,
			bubbles_per_class = 1,
			classes_per_school = classes_per_school,
			pr_meet_class = pr_meet_class,
			pr_meet_school = pr_meet_school,
		) %>% expand_grid(
			tibble(
				R = c(1.5, 3, 6),
				gamma = purrr::map_dbl(R, get_gamma)
			),
			frac_symptomatic = 0.5,
			pr_external_infections = 1/(n_school*7), # expected 1 per week
			pr_noncovid_symptoms = .01,
			pcr = list(pcr),
			tibble(
				mean_sensitivity = c(.4, .6, .8),
				eta = purrr::map_dbl(mean_sensitivity, get_eta)
			),
			ar_coefficient = 0.0
		) %>%
		mutate(
			slope = innova$slope, intercept = innova$intercept,
			ar_window = 3L
		)	
	) %>%
	expand_grid(
		policy_name = c("default", "Thu/Fri off", "Mon screening", "Mon/Wed screening", "test & release"),
		iter = 1:(params$iterations)
	)


## ----run-simulation-------------------------------------------------------------------------------------------------------------------------------------------------------
tbl_results <- bind_cols(
	tbl_sim_scenarios,
	with(tbl_sim_scenarios,
		julia_call("evaluate_performance",
			n_bubble, bubbles_per_class, classes_per_school, pr_meet_class, pr_meet_school,
			gamma, frac_symptomatic, pr_external_infections, pr_noncovid_symptoms,
			pcr,
			eta, slope, intercept, ar_window, ar_coefficient,
			policy_name,
			need_return = "R"
		)
	)
) %>%
rename(
	`mean sensitivity` = mean_sensitivity
) %>%
mutate(
	mean_weekly_infections = round(pr_external_infections*n_school*7, 2)
)
readr::write_rds(tbl_results, "_site/tbl_results.rds", compress = "gz")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- tbl_results %>%
	filter(
		n_bubble == 9 & bubbles_per_class == 3,
		ar_coefficient == 0,
		frac_symptomatic == 0.5,
		mean_weekly_infections == 1
	) %>%
	ggplot() +
		aes(n_infected/n_school, mean_daily_infectious, color = policy_name, fill = policy_name) +
		geom_vline(xintercept = 6/n_school) +
		geom_point(alpha = .2, shape = 16) +
		stat_ellipse(geom = "polygon", alpha = 0.33, color = NA, level = 0.9) + 
		scale_y_continuous("mean infectious (daily)") +
		scale_x_continuous("% infected (cumulative)", labels = scales::percent, limits = c(NA, 1)) +
		facet_grid(`mean sensitivity` ~ R, labeller = label_both)
save_plot(plt, "results-infectiousness-vs-infectivity", width = width, height = 1.5*height)



## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt1 <- tbl_results %>%
	filter(
		n_bubble == 9 & bubbles_per_class == 3,
		frac_symptomatic == 0.5,
		mean_weekly_infections == 1
	) %>%
	ggplot() +
		aes(policy_name, n_infected/n_school, color = factor(ar_coefficient)) +
		geom_hline(yintercept = 6/n_school) +
		geom_boxplot() +
		scale_y_continuous("% infected (cumulative)", labels = scales::percent, limits = c(0, 1)) +
		# scale_color_grey() +
		facet_grid(`mean sensitivity` ~ R, labeller = label_both) +
		theme(
			axis.text.x = element_text(angle = 33, hjust = 1),
			axis.title.x = element_blank(),
			legend.position = "right"
		) +
		ggtitle("Distribution of % infected by AR coefficient")
plt2 <- tbl_results %>%
	filter(
		n_bubble == 9 & bubbles_per_class == 3,
		ar_coefficient == 0,
		mean_weekly_infections == 1
	) %>%
	ggplot() +
		aes(policy_name, n_infected/n_school, color = factor(1 - frac_symptomatic)) +
		geom_hline(yintercept = 6/n_school) +
		geom_boxplot() +
		scale_y_continuous("% infected (cumulative)", labels = scales::percent, limits = c(0, 1)) +
		# scale_color_grey() +
		facet_grid(`mean sensitivity` ~ R, labeller = label_both) +
		theme(
			axis.text.x = element_text(angle = 33, hjust = 1),
			axis.title.x = element_blank(),
			legend.position = "right"
		) +
		ggtitle("Distribution of % infected by % asymptomatic")
plt <- plt1 + plt2 + plot_layout(ncol = 1)
save_plot(plt, "results-infectivity-marginal", width = width, height = 2.2*height)



## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- tbl_results %>%
	filter(
		n_bubble == 9 & bubbles_per_class == 3,
		ar_coefficient == 0,
		frac_symptomatic == 0.5,
		mean_weekly_infections == 1
	) %>%
	ggplot() +
		aes(policy_name, n_pcr_tests) +
		geom_hline(yintercept = 6/n_school) +
		geom_boxplot() +
		scale_y_continuous("PCR test required (cumulative)") +
		facet_grid(`mean sensitivity` ~ R, labeller = label_both) +
		theme(
			axis.text.x = element_text(angle = 33, hjust = 1),
			axis.title.x = element_blank(),
			legend.position = "right"
		)
save_plot(plt, "results-pcr-marginal", width = width, height = 1.2*height)



## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- tbl_results %>%
	filter(
		n_bubble == 9 & bubbles_per_class == 3,
		ar_coefficient == 0,
		frac_symptomatic == 0.5,
		mean_weekly_infections == 1
	) %>%
	ggplot() +
		aes(n_infected/n_school, n_pcr_tests, color = policy_name, fill = policy_name) +
		geom_vline(xintercept = 6/n_school) +
		geom_point(alpha = .2, shape = 16) +
		stat_ellipse(geom = "polygon", alpha = 0.33, color = NA, level = 0.9) + 
		scale_y_continuous("PCR test required (cumulative)") +
		scale_x_continuous("% infected (cumulative)", labels = scales::percent, limits = c(NA, 1)) +
		facet_grid(`mean sensitivity` ~ R, labeller = label_both)
save_plot(plt, "results-pcr-vs-infectivity", width = width, height = 1.5*height)



## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- tbl_results %>%
	filter(
		n_bubble == 9 & bubbles_per_class == 3,
		ar_coefficient == 0,
		frac_symptomatic == 0.5,
		mean_weekly_infections == 1
	) %>%
	ggplot() +
		aes(n_infected/n_school, workdays_missed/n_school/5/6, color = policy_name, fill = policy_name) +
		geom_abline(slope = 1) +
		geom_point(alpha = .2, shape = 16) +
		stat_ellipse(geom = "polygon", alpha = 0.33, color = NA, level = 0.9) + 
		scale_x_continuous("% infected (cumulative)", labels = scales::percent, limits = c(NA, 1)) +
		scale_y_continuous(labels = scales::percent) +
		facet_grid(`mean sensitivity` ~ R, labeller = label_both) +
		labs(
			y = "% schooldays missed (cumulative)"
		)
save_plot(plt, "results-schooldays-missed-vs-infectivity", width = width, height = 1.5*height)



## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- tbl_results %>%
	filter(
		n_bubble == 9 & bubbles_per_class == 3,
		ar_coefficient == 0.75,
		frac_symptomatic == 0.5,
		mean_weekly_infections == 1
	) %>%
	ggplot() +
		aes(n_infected/n_school, workdays_missed/n_school/5/6, color = policy_name, fill = policy_name) +
		geom_abline(slope = 1) +
		geom_point(alpha = .2, shape = 16) +
		stat_ellipse(geom = "polygon", alpha = 0.33, color = NA, level = 0.9) + 
		scale_x_continuous("% infected (cumulative)", labels = scales::percent, limits = c(NA, 1)) +
		scale_y_continuous(labels = scales::percent) +
		facet_grid(`mean sensitivity` ~ R, labeller = label_both) +
		labs(
			y = "% schooldays missed (cumulative)"
		)
save_plot(plt, "sensitivity-schooldays-missed-vs-infectivity-ar", width = width, height = 1.5*height)



## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- tbl_results %>%
	filter(
		n_bubble == 27 & bubbles_per_class == 1,
		ar_coefficient == 0,
		frac_symptomatic == 0.5,
		mean_weekly_infections == 1
	) %>%
	rename(
		`R*` = R
	) %>% 
	ggplot() +
		aes(n_infected/n_school, workdays_missed/n_school/5/6, color = policy_name, fill = policy_name) +
		geom_abline(slope = 1) +
		geom_point(alpha = .2, shape = 16) +
		stat_ellipse(geom = "polygon", alpha = 0.33, color = NA, level = 0.9) + 
		scale_x_continuous("% infected (cumulative)", labels = scales::percent, limits = c(NA, 1)) +
		scale_y_continuous(labels = scales::percent) +
		facet_grid(`mean sensitivity` ~ `R*`, labeller = label_both) +
		labs(
			y = "% schooldays missed (cumulative)"
		)
save_plot(plt, "sensitivity-schooldays-missed-vs-infectivity-bubbles", width = width, height = 1.5*height)



## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plt <- tbl_results %>%
	filter(
		ar_coefficient == 0,
		frac_symptomatic == 0.5,
		mean_weekly_infections == 1
	) %>%
	rename(
		`R*` = R
	) %>% 
	ggplot() +
		aes(policy_name, n_infected/n_school, color = factor(bubbles_per_class)) +
		geom_hline(yintercept = 6/n_school) +
		geom_boxplot() +
		scale_y_continuous("% infected (cumulative)", labels = scales::percent, limits = c(0, 1)) +
		# scale_color_grey() +
		facet_grid(`mean sensitivity` ~ `R*`, labeller = label_both) +
		theme(
			axis.text.x = element_text(angle = 33, hjust = 1),
			axis.title.x = element_blank()
		)
save_plot(plt, "sensitivity-infectivity-marignal-bubbles", width = width, height = 1.25*height)


