function school(
	n_per_bubble, bubbles_per_class, m_classes, pr_meet_class, pr_meet_school, 
	gamma, frac_symptomatic, pr_noncov_symptoms, a, b, lli, 
	policy
)
	dm = LarremoreModel(gamma; frac_symptomatic = frac_symptomatic, l10vl_clearance = log10(lli))
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
	    pr_unrelated_symptoms = pr_noncov_symptoms,
	    a = a,
	    b = b
    )
end

function sample_rzero(school::T, days::Int, n::Int) where {T<:Population}
	function f()
		m = length(school.individuals)
		tmp = resample(school)
		ind = rand(1:m)
    	infect!.(tmp.individuals[ind])
    	steps!(tmp, days)
    	res = get_contact_log(tmp.individuals[ind])
    	return nrow(res[res[:, :infected_other], :])
	end
	pmap(i -> f(), 1:n)
end