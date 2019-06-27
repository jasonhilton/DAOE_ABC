library(furrr)
library(purrr)
library(magrittr)
library(dplyr)
library(XML)
library(here)
library(rsm)

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

model.path <- file.path(nl.path, "models/Sample Models/Social Science/Segregation.nlogo")

# names for outputs and inputs in nlogo code.
metric <-  "percent-similar"
variables <- c("%-similar-wanted",  "density")
# create a function to allow running the model through netlogo from R
runModel <- make_run_model_command(nl.path, model.path, 
                                   experiment_path, 
                                   metric,variables,
                                   jar_name = jar_name,
                                   stepLimit=500)

# Create the design
n_inputs <- length(variables)
ccd_design <- ccd(n_inputs)

# The ranges of the inputs we want to vary over
input_ranges <- list(
  x1_range = c(5,95),
  x2_range = c(40, 70)
)

# Fcuntion to scale inputs
scale_input <- function(design_in, par_range){
  # scale inputs to line in [0,1]
  scaled_in <- ((design_in - min(design_in))/
                  (max(design_in) - min(design_in)))
  # scale [0,1] to lie within par_range
  scaled_out <- scaled_in * diff(par_range) + par_range[1]
  return(scaled_out)
}

# scale each input so they are the appropriate scale for nlogo
# assuming the input design has labels x1, x2, 
ccd_design_scaled_l <- map(1:n_inputs, 
    function(i, design, in_range){
      column <- design %>% select(paste0("x",i)) %>% unlist()
      return(scale_input(column, in_range[[paste0("x", i, "_range")]]))
    },
    design=ccd_design,
    in_range = input_ranges
  ) # returns a list

# bind together list into matrix
ccd_design_scaled <- do.call(cbind, ccd_design_scaled_l)
plot(ccd_design_scaled[,1], ccd_design_scaled[,2])

# pc_similar <- future_map2_dbl(ccd_design_scaled[,1],
#                               ccd_design_scaled[,2],
#                               runModel,
#                               .progress=T)
# (memory intensive)


pc_similar <- map2_dbl(ccd_design_scaled[,1],
                       ccd_design_scaled[,2],
                       runModel)


ccd_design$Response <- pc_similar

#ccd_design

saveRDS(ccd_design,"example_data/schelling_ccd.rds")

rsm_mod <- rsm(Response~SO(x1,x2), data=ccd_design)


summary(rsm_mod)

this <- ccd_design %>% mutate(y_hat=predict(rsm_mod),error=Response-y_hat)

ggplot(this, aes(x=x1,y=error))+ geom_point()

## replicates ----------------------------------------------------------------

ccd_design_scaled

ccd_design_scaled <- ccd_design_scaled[!duplicated(ccd_design_scaled),]

ccd_design_scaled <- ccd_design_scaled[rep(seq_len(nrow(ccd_design_scaled)), each=10),]

# takes a while!
pc_similar <- map2_dbl(ccd_design_scaled[,1],
                       ccd_design_scaled[,2],
                       runModel)

# ugly repeated code here.
ccd_raw <- cbind(ccd_design$x1,ccd_design$x2) %>% as_tibble()
ccd_raw <- ccd_raw[!duplicated(ccd_raw),]
ccd_raw <- ccd_raw[rep(seq_len(nrow(ccd_raw)), each=10),]

ccd_reps <- as_tibble(ccd_raw) %>% mutate(pc_similar=pc_similar) %>%
  rename(pc_desired=V1, density=V2)



saveRDS(ccd_reps,"example_data/schelling_ccd_reps.rds")



# Latin Hyper cube model ---------------------------------------------

library(lhs)

lhs_design <- improvedLHS(30,2)
lhs_design <- lhs_design[rep(seq_len(nrow(lhs_design)), 
                                           each=3),] %>% as_tibble()

lhs_design_scaled_l <- map(1:n_inputs, 
                           function(i, design, in_range){
                             column <- design %>% select(paste0("V",i)) %>% unlist()
                             return(scale_input(column, in_range[[paste0("x", i, "_range")]]))
                           },
                           design=lhs_design,
                           in_range = input_ranges
) # returns a list


lhs_design_scaled <- do.call(cbind, lhs_design_scaled_l)


pc_similar <- map2_dbl(lhs_design_scaled[,1],
                       lhs_design_scaled[,2],
                       runModel)

lhs_design %<>% mutate(pc_similar=pc_similar) %>%
  rename(pc_desired=V1, density=V2)

saveRDS(lhs_design, "example_data/lhs_runs.rds")



