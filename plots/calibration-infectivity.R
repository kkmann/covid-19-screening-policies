params1 <- scenario()
plt1 <- ggplot(fit_rzero(params1)$data) +
	aes(gamma, R) +
	geom_point(alpha = 0.2, shape = 16) +
	geom_line(
		data = tibble(
			gamma = seq(0, .1, length.out = 100),
			R = as.numeric(predict( fit_rzero(params1)$fit, newdata = tibble(gamma = gamma), type = "response"))
		)
	) +
	ylim(c(0, 16)) +
	xlim(c(0, .1)) +
	labs(
		y = "",
		x = expression(gamma),
		title = "LLI = 1e6"
	)
params2 <- scenario(lli = 1e3)
plt2 <- ggplot(fit_rzero(params2, gamma_max = .06)$data) +
	aes(gamma, R) +
	geom_point(alpha = 0.2, shape = 16) +
	geom_line(
		data = tibble(
			gamma = seq(0, .06, length.out = 100),
			R = as.numeric(predict( fit_rzero(params2, gamma_max = .06)$fit, newdata = tibble(gamma = gamma), type = "response"))
		)
	) +
	ylim(c(0, 16)) +
	xlim(c(0, .1)) +
	labs(
		y = "",
		x = expression(gamma),
		title = "LLI = 1e3"
	)
plt <- plt1 + plt2
save_plot(plt, "calibrating-infectivity", width = width, height = .66*height)
