function school(
	n_per_bubble, bubbles_per_class, m_classes, pr_meet_class, pr_meet_school, 
	gamma, frac_symptomatic, pr_noncov_symptoms;
	policy = DoNothing()
)
	dm = LarremoreModel(gamma; frac_symptomatic = frac_symptomatic)
    ThreeLevelPopulation(
    	n_per_bubble = n_per_bubble,  
    	bubbles_per_class = bubbles_per_class, 
    	m_classes = m_classes,
	    pr_meet_bubble = 1.0,
	    pr_meet_class = pr_meet_class,
	    pr_meet_school = pr_meet_school,
	    policy_bubble = policy,
	    policy_class = DoNothing(),
	    policy_school = DoNothing(),
	    meeting_days = collect(0:4),
	    disease_model = dm,
	    pr_unrelated_symptoms = pr_noncov_symptoms
    )
end