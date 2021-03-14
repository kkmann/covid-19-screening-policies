function f(school, pr_external_infections, days; n = 1L)
	function g(x)
		for i in 1:(days)
			for indv in x.individuals
				if rand(Bernoulli(pr_external_infections))
					infect!(indv)
				end
			end
	    	step!(x)
	    end
    	return evaluate(x; tests = ["pcr", "lfd"])
	end
	return vcat(pmap(g, [resample(school) for i = 1:n])...)
end
