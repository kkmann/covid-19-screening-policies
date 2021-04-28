f <- function(params) with(params, fit_rzero(
		n_bubble, bubbles_per_class, classes_per_school, pr_meet_class, pr_meet_school,
		frac_symptomatic, pr_noncovid_symptoms, l, scale, df, lli, gamma_min, gamma_max
	))

params1 <- scenario()
plt1 <- ggplot(f(params1)$data) +
	aes(gamma, R) +
	geom_point(alpha = 0.2, shape = 16, size = .5) +
	geom_line(
		data = tibble(
			gamma = seq(0, .1, length.out = 100),
			R = as.numeric(predict( f(params1)$fit, newdata = tibble(gamma = gamma), type = "response"))
		)
	) +
	ylim(c(0, 16)) +
	xlim(c(0, .1)) +
	labs(
		y = "",
		x = expression(gamma),
		title = "LLI = 1e6"
	) +
	theme(
		axis.text.x = element_text(angle = 30, hjust = 1)
	)
params2 <- scenario(lli = 1e3, gamma_max = 0.06)
plt2 <- ggplot(f(params2)$data) +
	aes(gamma, R) +
	geom_point(alpha = 0.2, shape = 16, size = .5) +
	geom_line(
		data = tibble(
			gamma = seq(0, .06, length.out = 100),
			R = as.numeric(predict( f(params2)$fit, newdata = tibble(gamma = gamma), type = "response"))
		)
	) +
	ylim(c(0, 16)) +
	xlim(c(0, .1)) +
	labs(
		y = "",
		x = expression(gamma),
		title = "LLI = 1e3"
	) +
	theme(
		axis.text.x = element_text(angle = 30, hjust = 1)
	)
params3 <- scenario(scale = 3/sqrt(3), l = 3, df = 3)
plt3 <- ggplot(f(params3)$data) +
	aes(gamma, R) +
	geom_point(alpha = 0.2, shape = 16, size = .5) +
	geom_line(
		data = tibble(
			gamma = seq(0, .1, length.out = 100),
			R = as.numeric(predict( f(params3)$fit, newdata = tibble(gamma = gamma), type = "response"))
		)
	) +
	ylim(c(0, 16)) +
	xlim(c(0, .1)) +
	labs(
		y = "",
		x = expression(gamma),
		title = "Heavy Tails"
	) +
	theme(
		axis.text.x = element_text(angle = 30, hjust = 1)
	)
plt <- plt1 + plt2 + plt3
save_plot(plt, "figA2-calibrating-infectivity", width = doublecolwidth, height = .66*height)
