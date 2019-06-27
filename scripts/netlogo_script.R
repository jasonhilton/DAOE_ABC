# We need to install the XML package - this is to create 'experiment' files that netlogo can read.
# install.packages("XML")
library(XML)
### V CHANGE THIS PATH V ###
#nl.path <- "C:\\Program Files (x86)\\NetLogo 5.2.1"
nl.path <- "/home/jason/NetLogo 6.1.0/app"

# V some directory where a temporary file can be created V (Choose your own!)

library(here)
experiment_path <- file.path(here(), "temp_experiments")
dir.create(experiment_path)

#load some helper functions 
# you need to set your working directory to the location where this file apple_funcs is saved
source("net_logo_funcs.R")


model.path <- file.path(nl.path, "models/Sample Models/Social Science/Segregation.nlogo")

metric <-  "percent-similar"
variables <- c("%-similar-wanted",  "density")
# create a function to allow running the model in a similar way to using NetLogo
runModel <- make_run_model_command(nl.path, model.path, experiment_path, 
                                   metric,variables,
                                   jar_name = "netlogo-6.1.0.jar"
                                  )

percent_similar_wanted <- 50
density <- 58
runModel(percent_similar_wanted, density)


model_path <- file.path(nl.path, "/models/Sample Models/Earth Science/Fire.nlogo")

metric <- "burned-trees"
possible_trees <- 63001
variables <- "density"
runModel<-make_run_model_command(nl.path,model_path,experiment_path, 
                                 metric,variables, jar_name = "netlogo-6.1.0.jar",
                                 stepLimit=1000)

density <- 60

for (i in 1:4) {
  print(runModel(density)/(possible_trees * (density/100.0 )))
}

density <- rep(60,50)
system.time( proportion_burned_60 <- sapply(density, runModel )/(possible_trees * (density/100.0 )))
hist(proportion_burned_60)


# run model using mclapply, and combine results with those from before.
library("parallel")

# I wasn't able to test this on a MAC unfortunately - but it should work

# how many processors shall I use?
cores=detectCores() - 1 

system.time(
  # For technical reasons relating to the way this runModel works, we need to also supply a 
  # unique run number to each simulation when parallelising. Don't worry about this!
  number_burned_60_par <- mcmapply(runModel,rep(60,100), run_no=1:100, mc.cores=cores)
)

proportion_burned_60_par <- number_burned_60_par / (possible_trees * (60/100.0 ))

proportion_burned_60 <- c( proportion_burned_60,  proportion_burned_60_par)

hist(proportion_burned_60,breaks = 15)
plot( density( proportion_burned_60 ) )
var(proportion_burned_60)

densities<-seq(51, 81, 6)

# make a vector of the density points we want to run, including repeats
densities_rep<-unlist(lapply(densities, rep, 25))
# run the model for these densties
number_burned <- mcmapply(runModel, densities_rep, run_no=1:(length(densities_rep)))
proportion_burned <- number_burned /(possible_trees * (densities_rep/100.0 ))  
fire_outputs<-data.frame(densities_rep, proportion_burned)

# example histogram 
with (fire_outputs, hist(proportion_burned[densities_rep==57]))

#get variance at each density
variances<-sapply(densities, 
                  function(den){
                    var(fire_outputs$proportion_burned[fire_outputs$densities_rep==den ])
                  }) 


plot(densities, variances, ylim=c(0,0.05))
points(60, var(proportion_burned_60))


prior<-rnorm(100,56,1.5)

number_burned <- mcmapply(runModel, prior, run_no=1:100)
proportion_burned <- number_burned /(possible_trees * (prior/100.0 ))  

hist(proportion_burned)

number_burned_mean <- mcmapply(runModel, rep(56,50), run_no=1:50)
proportion_burned_mean <- number_burned_mean /(possible_trees * (prior/100.0 ))  
hist(proportion_burned_mean)


lhs_design$response<-lhs_response
library("DiceKriging")

kriging_m1<-km(response~similar_desired + density, lhs_design[,c(1,2)], lhs_design[,3], nugget.estim=T)

kriging_m1


similar_ins<-seq(0,100,2)
densities_ins<-seq(20,95,5)

fullSpace<- data.frame(expand.grid(similar_ins, densities_ins))
dim(fullSpace)
#lots of points!

colnames(fullSpace)<-colnames(lhs_design[,1:2])


predictions<-predict(kriging_m1, fullSpace, "UK")

prediction.surface<-matrix(predictions$mean, 51)

filled.contour(similar_ins, densities_ins, prediction.surface )

