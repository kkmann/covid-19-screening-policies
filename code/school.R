julia_eval('@everywhere include("code/school.jl")')

school <- function(policy = julia_call("DoNothing", need_return = "Julia"), ...){
	params <- scenario(...)
	with(params,
	julia_call("school",
		n_bubble, bubbles_per_class, classes_per_school, pr_meet_class, pr_meet_school,
		gamma,
		frac_symptomatic, pr_noncovid_symptoms, l, scale, df,
		a, b,
		lli,
		policy
	))
}

sample_rzero <- function(school, days = 21L, n = 1L) julia_call("sample_rzero", school, days, n)
