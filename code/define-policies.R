pcr_turnaround <- 2L
isolation_duration <- 10L

lst_policies <- list(
	reference = function(params) julia_call("SymptomaticIsolation",
			lfd(params), pcr_test = pcr(params),
			pcr_turnaround = pcr_turnaround, isolation_duration = isolation_duration,
			need_return = "Julia"
		),
	`test for release` = function(params) julia_call("DynamicScreening",
			lfd(params), pcr_test = pcr(params),
			pcr_turnaround = pcr_turnaround, isolation_duration = isolation_duration,
			followup_duration = 7L,
			need_return = "Julia"
		),
	`Thu/Fri off` = function(params) julia_call("SymptomaticIsolation",
			lfd(params), pcr_test = pcr(params),
			pcr_turnaround = pcr_turnaround, isolation_duration = isolation_duration,
			fixed_isolation_weekdays = as.integer(c(3, 4)),
			need_return = "Julia"
		),
	`Mon screening` = function(params) julia_call("SymptomaticIsolation",
			lfd(params), pcr_test = pcr(params),
			pcr_turnaround = pcr_turnaround, isolation_duration = isolation_duration,
			screening_test_weekdays = julia_eval("[0]", need_return = "Julia"),
			need_return = "Julia"
		),
	`Mon/Wed screening` = function(params) julia_call("SymptomaticIsolation",
			lfd(params), pcr_test = pcr(params),
			pcr_turnaround = pcr_turnaround, isolation_duration = isolation_duration,
			screening_test_weekdays = as.integer(c(0, 2)),
			need_return = "Julia"
		)
)
