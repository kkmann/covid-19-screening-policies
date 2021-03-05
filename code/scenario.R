scenario <- function(
	n_bubble = 9L,
	bubbles_per_class = 3L,
	classes_per_school = 12L,
	expected_class_contacts = 3,
	expected_school_contacts = 1,
	frac_symptomatic = 0.5,
	pr_noncovid_symptoms = 0.01,
	expected_weekly_external_infections = 1,
	a = Inf,
	b = 1,
	lli = 1e6,
	pcr_lod = 300.0,
	pcr_sens = .975,
	pcr_spec = 1.0,
	gamma = 0.0,
	eta = 1.0,
	lfd_spec = 0.998,
	ar_window = 3L,
	ar_coefficient = 0.0,
	days = as.integer(6*7),
	...
) {
	res <- 	as.list(environment())

	res$pr_meet_class <- expected_class_contacts/(n_class(res) - 1)
	res$expected_class_contacts <- NULL
	res$pr_meet_school <- expected_school_contacts/(n_school(res) - 1)
	res$expected_school_contacts <- NULL
	res$pr_external_infection <- max(0, min(1, expected_weekly_external_infections / n_school(res) / 7))
	res$expected_weekly_external_infections <- NULL
	
	tbl_innova_data <- tibble(
		`viral load` = 10^(2:7),
		sensitivity = 1 - (c(621, 521, 344, 207, 144, 123) - 114) / (684 - 114)
	)
	sensitivity <- function(vl, slope, intercept) {
		lfd <- julia_call("LogRegTest", "lfd", slope, intercept, lfd_spec, need_return = "Julia")
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
	res$lfd_slope <- innova$par[1]
	res$lfd_intercept <- innova$par[2]
	
	res
}

sensitivity <- function(vl, params) {
	lfd <- julia_call("LogRegTest", "lfd", params$lfd_slope, params$lfd_intercept, params$lfd_spec, need_return = "Julia")
	julia_call("sensitivity.", lfd, vl, need_return = "R")
}
	
n_class <- function(params) with(params, n_bubble * bubbles_per_class)
n_school <- function(params) with(params, classes_per_school * n_class(params))
n_weekly_infections <- function(params) with(params, 7*n_school(params)*pr_external_infection)

schooldays <- function(params) with(params, sum(((1:days) %% 7) %in% 1:5))

pcr <- function(params) with(params, julia_call("FixedTest", "pcr", pcr_sens, pcr_spec, lod = pcr_lod, need_return = "Julia"))

lfd <- function(params) {
	with(params,
		julia_call("LogRegTest", "lfd", 
			eta*params$lfd_slope, params$lfd_intercept, lfd_spec,
			ar_window = ar_window, ar_coefficient = ar_coefficient
		)
	)
}

fit_rzero_ <- function(params, gamma_min = 0, gamma_max = .1) {
	tbl_rzero <- tibble(
		gamma = seq(gamma_min, gamma_max, length.out = 100),
			R = map(gamma, ~{
					params$gamma <- .
					sample_rzero(do.call(school, params), n = 10L)
				})
		) %>% 
		unnest(R)
	fit <- lm(formula = R ~ gamma - 1, data = tbl_rzero)
	list(fit = fit, data = tbl_rzero)
}
fit_rzero <- memoise::memoise(fit_rzero_)
rzero <- function(gamma, params, gamma_min = 0, gamma_max = .1) {
	fit <- fit_rzero(params, gamma_min, gamma_max)$fit
	as.numeric(predict(fit, newdata = tibble(gamma = gamma), type = "response"))
}
gamma <- function(R, params, gamma_min = 0, gamma_max = .1) {
	as.numeric(uniroot(function(x) rzero(x, params, gamma_min, gamma_max) - R, interval = c(gamma_min, gamma_max))$root)
}

sample_presymptomatic_vl <- function(params, n = 1e5) {
	dm <- julia_call("LarremoreModel", params$gamma, frac_symptomatic = params$frac_symptomatic, need_return = "Julia")
	individuals <- julia_call("Individual.", dm, rep(params$pr_noncovid_symptoms, n), need_return = "Julia")
	julia_call("infect!.", individuals, need_return = "Julia")
	julia_call("steps!.", individuals, 21L, need_return = "Julia")
	julia_call("get_status_logs", individuals) %>%
		as_tibble() %>%
		arrange(uuid, day) %>%
		group_by(uuid) %>%
		filter(
			row_number() >= which(viral_load > params$pcr_lod)[1],
			row_number() < which(symptomatic)[1]
		) %>%
		sample_n(1) %>%
		ungroup() %>%
		select(uuid, day, viral_load)
}
sample_presymptomatic_vl_ <- memoise::memoise(sample_presymptomatic_vl)
mean_sensitivity <- function(params, eta, ...) {
	sample_presymptomatic_vl_(params, ...) %>%
	mutate(
		sensitivity = sensitivity(viral_load^eta, params)
	) %>%
	pull(sensitivity) %>%
	summary() %>%
	mean()
}
eta <- function(params, target, ...) {
	as.numeric(uniroot(function(x) mean_sensitivity(params, x, ...) - target, interval = c(0, 5))$root)
}

expand_scenario <- function(params = scenario(), ...) {
	tbl <- if (length(list(...)) == 0) {
		as_tibble(params)
	} else {
		for (name in names(list(...))) {
			params[[name]] <- NULL
		}
		expand_grid(do.call(expand_grid, list(...)), as_tibble(params))
	}
	tbl %>% 
		group_by(row_number()) %>% 
		nest() %>% 
		mutate(data = map(data, as.list)) %>% 
		pull(data) %>% 
		enframe(value = "params") %>% 
		select(params) %>% 
		mutate(
			tmp = map(params, ~as_tibble(.))
		) %>% 
		unnest(tmp)
}

julia_eval('@everywhere include("code/evaluate_performance.jl")')
evaluate_performance_ <- function(params, policy, n = 1L) {
	school <- do.call(school, c(params, list(policy = policy(params))))
	julia_call("f", school, params$pr_external_infection, params$days, n = n) %>% 
		as_tibble() %>% 
		mutate(
			`% infected (cumulative)` = n_infected/n_school(params),
			`% schooldays missed (cumulative)` = workdays_missed/n_school(params)/schooldays(params)
		)
}
evaluate_performance <- function(policies, params = scenario(), n = if (!is.null(n_resample)) {n_resample} else {25L}, ...) {
	gamma2rs <- memoise::memoise(function(gamma) round(rzero(gamma, params), 1) )
	eta2mean_sensitivity <- memoise::memoise(function(eta) round(mean_sensitivity(params, eta), 2) )
	expand_scenario(params, ...) %>% 
		expand_grid(tibble(policy = policies)) %>% 
		mutate(
			policy_name = names(policy),
			results = map2(params, policy, evaluate_performance_, n)
		) %>% 
		unnest(results) %>% 
		select(-params) %>% 
		mutate(
			policy_name = factor(policy_name, levels = names(lst_policies)),
			Rs = map_dbl(.data$gamma, gamma2rs),
			`mean sensitivity` = map_dbl(.data$eta, eta2mean_sensitivity)
		)
}
