# Load multicore computing pkgs, set number of cores to 2 less than max
library(parallel)
library(pbapply)
no_cores <- detectCores() - 2

# Load pkgs for network analyses
library(statnet)
library(igraph)
library(tnet)

# Load pkg for model 2 regressions
library(lmodel2)

# Load pkg for comparing distributions
library(segmented)
library(dgof)

# Load pkgs for graphing
library(ggplot2)
library(grid)
library(gridExtra)
library(plotrix)

# Load pkgs for pgls
library(caper)
library(dplyr)
library(MuMIn)
