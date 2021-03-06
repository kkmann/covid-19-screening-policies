using Pkg
Pkg.activate(".julia")
Pkg.instantiate()
Pkg.precompile()

@time using cov19sim, DataFrames, Distributions, Random

import Random.seed!
seed!(42)

# load environment on worker processors
using Distributed
rmprocs(workers())
addprocs(Sys.CPU_THREADS) # memory limitations...
@everywhere begin 
	using Pkg
	Pkg.activate(".julia")
	using cov19sim, DataFrames, Distributions, Random
	Random.seed!(myid())
	include("school.jl")
	include("evaluate_performance.jl")
end
