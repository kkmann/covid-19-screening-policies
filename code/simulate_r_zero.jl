function simulate_r_zero(school::T; days = 21) where {T<:Population}
	school = resample(school)
    infect!.(school.individuals[1])
    steps!(school, days)
    res = get_contact_log(school.individuals[1])
    nrow(res[res[:, :infected_other], :])
end

# pmap vectorized form
function simulate_r_zero(schools::Vector{T}; days = 21) where {T<:Population}
	pmap(x -> simulate_r_zero(x, days = days), schools)
end