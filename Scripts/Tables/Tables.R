## In this script the tables with the results of the simulations study
## are constructed


library(dplyr)

# Load the data 
# data contains the estimates of the bias, RMSE, standard deviation for each 
# category combinations for each population for each scenario and each estimation 
# method, for both the simulation and the bootstrap analysis
# 
# results contains the estimates for each category combinations for each population
# for each scenario and each estimation method, for both the simulation and
# the bootstrap analysis. It also includes the marginal and conditional frechet
# bounds and the value in the population

data = 
  readRDS("./Tables/PlotData_Simulation.RData")

results = 
  readRDS("./Tables/PlotData_Estimates.RData")

# Define the width of the frechet bounds
results$marginal_width = 
  results$marginal_upper - results$marginal_lower

results$conditional_width = 
  results$conditional_upper - results$conditional_lower

# aggregate across populations and situations
width = 
  aggregate(marginal_width ~ population + situation, data = results, FUN = mean)

conditional_width = 
  aggregate(conditional_width ~population + situation, data = results, FUN = mean)

width = 
  width[order(width$population),]

conditional_width = 
  conditional_width[order(conditional_width$population),]

# add widths to data
data$marginal_width =
  width$marginal_width

data$conditional_width = 
  conditional_width$conditional_width

# population as factor
data$population = factor(data$population)

# round all widths to 3 decimals
data$conditional_width = 
  data$conditional_width %>% round(3)

# population as factor
results$population = factor(results$population)

# round to 2 digits
results[,c(1,3:4,8:11,14:16,18)] = 
  apply(results[,c(1,3:4,8:11,14:16,18)], 
        MARGIN = 2,
        FUN = round,
        digits = 2)

# round standard deviation (simulation and bootstrap) to 3 decimals
results[,c(2,17)] = 
  apply(results[,c(2,17)], 
        MARGIN = 2,
        FUN = round,
        digits = 3)



View(data)

View(results)
