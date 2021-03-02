function evaluate_performance_(
	n_per_bubble, bubbles_per_class, m_classes, pr_meet_class, pr_meet_school,
	gamma, frac_symptomatic, pr_external_infections, pr_noncovid_symptoms,
	pcr,
	eta, slope, intercept, 	ar_window, ar_coefficient,
	policy_name, a, b
)
	lfd = LogRegTest("lfd", eta*slope, intercept, .998; ar_window = ar_window, ar_coefficient = ar_coefficient)
	s = school(
		n_per_bubble, bubbles_per_class, m_classes, pr_meet_class, pr_meet_school, 
		gamma, frac_symptomatic, pr_noncovid_symptoms, a, b;
		policy = get_policy(policy_name, lfd)
	)
	for i in 1:(6*7)
		for indv in s.individuals
			if rand(Bernoulli(pr_external_infections))
				infect!(indv)
			end
		end
    	step!(s)
    end
    evaluate(s)
end

function evaluate_performance(
	n_per_bubble, bubbles_per_class, m_classes, pr_meet_class, pr_meet_school,
	gamma, frac_symptomatic, pr_external_infections, pr_noncovid_symptoms,
	pcr,
	eta, slope, intercept, 	ar_window, ar_coefficient,
	policy_name, a, b
)
	vcat(pmap(evaluate_performance_,
		n_per_bubble, bubbles_per_class, m_classes, pr_meet_class, pr_meet_school,
		gamma, frac_symptomatic, pr_external_infections, pr_noncovid_symptoms,
		pcr,
		eta, slope, intercept, 	ar_window, ar_coefficient,
		policy_name, a, b
	)...)
end