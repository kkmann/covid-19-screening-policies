function sample_school(gamma, policy, pr_noncov_symptoms, frac_symptomatic)
	dm = LarremoreModel(
		gamma;
	    frac_symptomatic = frac_symptomatic,
    	l10vl_onset = 3.0, day_onset_min = 2.5, day_onset_max = 3.5,
    	l10vl_peak_min = 7.0, l10vl_peak_max = 11.0, peak_delay_max = 3.0, peak_delay_shape = 1.5,
		symptom_delay_min = 0.0, symptom_delay_max = 3.0,
    	l10vl_clearance = 6.0, clearance_delay_min = 4.0, clearance_delay_max = 9.0
	)
    ThreeLevelPopulation(
    	n_per_bubble = 9,
	    bubbles_per_class = 3,
	    m_classes = 12,
	    pr_meet_bubble = 1.0,
	    pr_meet_class = 0.33,
	    pr_meet_school = 1/323,
	    policy_bubble = policy,
	    policy_class = DoNothing(),
	    policy_school = DoNothing(),
	    meeting_days = collect(0:4),
	    disease_model = dm,
	    pr_unrelated_symptoms = pr_noncov_symptoms
    )
end

score(schools) = vcat(evaluate.(schools)...)