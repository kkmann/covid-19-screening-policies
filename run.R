library(JuliaCall)
library(tidyverse)
library(tidygraph)
library(ggraph)
library(patchwork)

width <- 4.2126 # 107 mm
doublecolwidth <- 6.1 # measured from LPH file
height <- width/1.5

n_resample <- 250L

mcache <- cachem::cache_disk("_site/.cachem", max_size = 5*1024^3)

JuliaCall::julia_setup(force = TRUE)
JuliaCall::julia_source("code/setup.jl")

source("code/util.R")
source("code/scenario.R")
source("code/school.R")


# define policies --------------------------------------------------------------
source("code/define-policies.R")


# fig2: plot viral load sample trajectories ------------------------------------
source("code/plots/fig2-vl-trajectories.R")

# fig3: main results -----------------------------------------------------------
source("code/plots/fig3-main-results.R")

# fig4 test sensitivity & symptom fraction -------------------------------------
source("code/plots/fig4-lfd-sensitivity-and-symptomatic-fraction.R")

# fig5: only one bubble per class ----------------------------------------------
source("code/plots/fig5-three-vs-one-bubble.R")

# fig6: less LFD compliance ----------------------------------------------------
source("code/plots/fig6-lower-lfd-compliance.R")

# figA1: contact matrices ------------------------------------------------------
source("code/plots/figA1-population-structure.R")

# figA2: calibrate-infectivity -------------------------------------------------
source("code/plots/figA2-calibration-infectivity.R")

# figA3.1: recalibrate innova lfd mean sensitivity -----------------------------
source("code/plots/figA31-fit-mean-sensitivity.R")

# figA3.2: plot sensitivity vs infection probability ---------------------------
source("code/plots/figA32-sensitivity-vs-infectivity.R")

# figA3.3: plot sensitivity vs infection probability ---------------------------
source("code/plots/figA33-temporal-shift.R")

# fig A5.1: plot autocorrelation -----------------------------------------------
source("code/plots/figA51-autocorrelation.R")

# fig A5.2: results autocorrelation --------------------------------------------
source("code/plots/figA52-sensitivity-autocorrelation.R")

# fig A6: lower limit of infectivity -------------------------------------------
source("code/plots/figA6-lower-lli.R")

# fig A7: heavy tails ----------------------------------------------------------
source("code/plots/figA7-sensitivity-heavy-tails.R")

# fig A8: random effects -------------------------------------------------------
source("code/plots/figA8-sensitivity-random-effect.R")

# fig A9: ext. infection rate --------------------------------------------------
source("code/plots/figA9-external-infections.R")