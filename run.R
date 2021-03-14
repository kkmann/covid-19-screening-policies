library(JuliaCall)
library(tidyverse)
library(tidygraph)
library(ggraph)
library(patchwork)

width <- 8
height <- width/1.5

n_resample <- 250L

mcache <- cachem::cache_mem(max_size = 1024^3)

JuliaCall::julia_setup(force = TRUE)
JuliaCall::julia_source("code/setup.jl")
walk(list.files("code", pattern = "*.R", full.names = TRUE), source)

# define policies --------------------------------------------------------------
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

# calibrate-infectivity (standard case) ----------------------------------------
source("plots/population-structure.R")

# calibrate-infectivity (standard case) ----------------------------------------
source("plots/calibration-infectivity.R")

# plot viral load sample trajectories ------------------------------------------
source("plots/vl-trajectories.R")
source("plots/vl-trajectories-heavy-tails.R")

# recalibrate innova lfd mean sensitivity --------------------------------------
source("plots/fit-mean-sensitivity.R")

# plot autocorrelation ---------------------------------------------------------
source("plots/autocorrelation.R")

# plot sensitivity vs infection probability ------------------------------------
source("plots/sensitivity-vs-infectivity.R")

# main results, base scenario --------------------------------------------------
source("plots/main-results.R")

# only one bubble per class ----------------------------------------------------
source("plots/only-one-bubble.R")

# less compliance --------------------------------------------------------------
source("plots/lower-lfd-compliance.R")

# lower limit of infectivity ---------------------------------------------------
source("plots/lower-lli.R")

# heavy tails ------------------------------------------------------------------
source("plots/heavy-tails-sensitivity.R")

# random effects ---------------------------------------------------------------
source("plots/random-effect-sensitivity.R")
