library(furrr)
library(purrr)
library(XML)
library(here)

# path to the netlogo jar file. 
# This is different for different versions of netlogo 
nl.path <- "/home/jason/NetLogo 6.1.0/app"
# The exact name of the jar is different too
jar_name <- "netlogo-6.1.0.jar"

# create a temporary folder to save temporary experiment files
experiment_path <- file.path(here(), "temp_experiments")
dir.create(experiment_path)
# load some hacky custom functions to run models in netlogo from R
source("R/net_logo_funcs.R")

model_path <- file.path(nl.path, "/models/Sample Models/Earth Science/Fire.nlogo")

metric <- "burned-trees"
possible_trees <- 63001
variables <- "density"
runModel<-make_run_model_command(nl.path,model_path,experiment_path, 
                                 metric,variables, 
                                 jar_name = "netlogo-6.1.0.jar",
                                 stepLimit=1000)

density <- 60

# we can run the models in parallel using future_map_dbl from the furrr 
# package.
# 'plan' is needed to set up the parallel backend
plan(multiprocess)
# map functions (from the purrr package) take a list or vector, 
# and apply a function to each value of it.
# ('runModel') in this case. the 'dbl' suffice indicates the function output is
# a scalar double (ie a single number like 23.4 or -0.345)
# The furrr package gives parallel versions of map functions using the futures
# package.
burned_trees <- future_map_dbl(rep(density, 100),
                           runModel,
                           .progress=T)

burned_trees_df <- tibble(burned_trees=burned_trees, 
                          density=density/100,
                          simulation_trees=possible_trees*density) %>%
  mutate(proportion_burned=burned_trees/simulation_trees)

dir.create("example_data")
saveRDS(burned_trees_df,file.path("example_data", "burned_trees_100.rds"))

burned_trees <- future_map_dbl(rep(density, 1000),
                               runModel,
                               .progress=T)

burned_trees_df <- tibble(burned_trees=burned_trees, 
                          density=density/100,
                          simulation_trees=possible_trees*density) %>%
  mutate(proportion_burned=burned_trees/simulation_trees)

dir.create("example_data")
saveRDS(burned_trees_df,file.path("example_data", "burned_trees_1000.rds"))



