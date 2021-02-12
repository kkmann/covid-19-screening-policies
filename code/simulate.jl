function simulate(school, external_infection_probability, m) 
	# create independent instances of the model school
	populations = [deepcopy(school) for i = 1:m]
	@everywhere function f(x, days, pr)
		for i = 1:days
			for j in 1:n_individuals(x)
				indv = x.individuals[j]
				rand(Bernoulli(pr)) ? infect!(indv) : nothing
			end
			step!(x)
		end
		x
	end
	pmap(f, 
		populations, [days for i = 1:m], [external_infection_probability for i = 1:m]
	)
end
