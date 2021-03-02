expand_scenarios <- function(
	n_bubble = 9,
	bubbles_per_class = 3,
	classes_per_school = 12,
	pr_meet_class = 3/(n_bubble*bubbles_per_class - 1),
	pr_meet_school = 1/(n_bubble*bubbles_per_class*classes_per_school - 1),
	R = c(1.5, 3, 6),
	frac_symptomatic = 0.5,
	pr_external_infections = 1/(n_bubble*bubbles_per_class*classes_per_school*7), # expected 1 per week
	pr_noncovid_symptoms = .01,
	mean_sensitivity = c(.4, .6, .8),
	ar_coefficient = 0.0,
	ar_window = 3L,
	a = Inf,
	b = 1.0,
	pcr_test = list(pcr),
	slope = innova$slope, 
	intercept = innova$intercept,
	iter = 1:(params$iterations),
	policy_name = c("default", "Thu/Fri off", "Mon screening", "Mon/Wed screening", "test & release")
) {
	args <- as.list(environment())
	args[["R"]] <- tibble(R = R, gamma = purrr::map_dbl(R, get_gamma))
	args[["mean_sensitivity"]] <- tibble(
			mean_sensitivity = mean_sensitivity, 
			eta = purrr::map_dbl(mean_sensitivity, get_eta)
		)
	do.call(expand_grid, args) %>% 
		mutate(
			gamma = R$gamma, 
			R = R$R,
			eta = mean_sensitivity$eta,
			mean_sensitivity = mean_sensitivity$mean_sensitivity
		)
}
