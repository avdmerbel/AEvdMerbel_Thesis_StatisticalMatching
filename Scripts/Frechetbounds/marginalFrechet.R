# This file contains the calculation of the marginal Fréchet bounds for each category
# combination and and population

setwd("./Data/Populations")

pops = readRDS("main_populations.RData")

library(dplyr)

#--------------------- Common mediator --------------------#

# A function that calculates the marginal Frechet bounds when the mediator
# is the common variable
frechet_mediator <-
  function(population){
    
    # get the full population distribution
    full =
      population$FullPopulation
    
    # get the joint distribution of outcome and instrumental when mediator 
    # is common
    aggregated = 
      population$AggregatedDistribution$CommonMediator
    
    # define conditional distributions of the mediator
    instrumental_given_mediator =
      xtabs(proportion ~ mediator + instrumental,
            data = full) %>% prop.table(margin = "mediator")
    
    outcome_given_mediator =
      xtabs(proportion ~ mediator + outcome,
            data = full) %>% prop.table(margin = "mediator")
    
    # get marginal distribtion of all variables
    p_mediator = 
      aggregate(proportion ~ mediator,
                data = full,
                FUN = sum)
    
    p_instrumental = 
      aggregate(proportion ~ instrumental,
                data = full,
                FUN = sum)
    
    p_outcome = 
      aggregate(proportion ~ outcome,
                data = full,
                FUN = sum)
    
    # a function calculating the marginal lower bound
    marginal_lowerbound <-
      function(m){
        
        max(0, p_instrumental[m[1], 2] + p_outcome[m[2], 2] - 1)
        
      }
    
    # a function calculating the marginal upper bound
    marginal_upperbound <-
      function(m){
        
        min(p_instrumental[m[1], 2], p_outcome[m[2], 2])
        
      }
    
    # apply the marginal lower bound function to the aggregated data
    aggregated$marginal_lowerbound = 
      apply(aggregated, 
            MARGIN = 1, 
            FUN = marginal_lowerbound) 
    
    # apply the marginal upper bound function to the aggregated data
    aggregated$marginal_upperbound = 
      apply(aggregated, 
            MARGIN = 1, 
            FUN = marginal_upperbound)
    
    return(aggregated)
    
  }

# apply the frechet mediator function to the populations
lapply(pops, frechet_mediator)

#--------------------- Common outcome --------------------#

# A function that calculates the marginal frechet bounds in the situation that
# the outcome is the common variable 
frechet_outcome <-
  function(population){
    
    # get full distribution
    full =
      population$FullPopulation
    
    # aggregated distribution when outcome variable overlaps
    aggregated = 
      population$AggregatedDistribution$CommonOutcome
    
    # get conditional distributions 
    instrumental_given_outcome =
      xtabs(proportion ~ outcome + instrumental,
            data = full) %>% prop.table(margin = "outcome")
    
    mediator_given_outcome =
      xtabs(proportion ~ outcome + mediator,
            data = full) %>% prop.table(margin = "outcome")
    
    # get marginal distributions
    p_mediator = 
      aggregate(proportion ~ mediator,
                data = full,
                FUN = sum)
    
    p_instrumental = 
      aggregate(proportion ~ instrumental,
                data = full,
                FUN = sum)
    
    p_outcome = 
      aggregate(proportion ~ outcome,
                data = full,
                FUN = sum)
    
    # function that calculates the marginal lower bound
    marginal_lowerbound <-
      function(m){
        
        max(0, p_instrumental[m[1], 2] + p_mediator[m[2], 2] - 1)
        
      }
    
    # function that calculates the marginal upper bound
    marginal_upperbound <-
      function(m){
        
        min(p_instrumental[m[1], 2], p_mediator[m[2], 2])
        
      }
    
    # apply marginal bound functions to aggregate
    aggregated$marginal_lowerbound = 
      apply(aggregated, 
            MARGIN = 1, 
            FUN = marginal_lowerbound) 
    
    aggregated$marginal_upperbound = 
      apply(aggregated, 
            MARGIN = 1, 
            FUN = marginal_upperbound)
    
    return(aggregated)
    
  }

# apply frechet outcome function to all populations
lapply(pops, frechet_outcome)

#--------------------- Common instrumental --------------------#

# A function calculating marginal frechet bounds when the instrumental variable
# overlaps in the data
frechet_instrumental <-
  function(population){

    # get full distribution
    full =
      population$FullPopulation
    
    # aggregated distribution when instrumental overlaps
    aggregated = 
      population$AggregatedDistribution$CommonInstrumental
    
    # get conditional distributions
    outcome_given_instrumental =
      xtabs(proportion ~ instrumental + outcome ,
            data = full) %>% prop.table(margin = "instrumental")
    
    mediator_given_instrumental =
      xtabs(proportion ~ instrumental + mediator,
            data = full) %>% prop.table(margin = "instrumental")
    
    # get marginal distributions
    p_mediator = 
      aggregate(proportion ~ mediator,
                data = full,
                FUN = sum)
    
    p_instrumental = 
      aggregate(proportion ~ instrumental,
                data = full,
                FUN = sum)
    
    p_outcome = 
      aggregate(proportion ~ outcome,
                data = full,
                FUN = sum)
    
    # functions to calculate the marginal bounds
    marginal_lowerbound <-
      function(m){
        
        max(0, p_mediator[m[1], 2] + p_outcome[m[2], 2] - 1)
        
      }
    
    marginal_upperbound <-
      function(m){
        
        min(p_mediator[m[1], 2], p_outcome[m[2], 2])
        
      }
    
    # apply marginal bounds functions to aggregate
    aggregated$marginal_lowerbound = 
      apply(aggregated, 
            MARGIN = 1, 
            FUN = marginal_lowerbound) 
    
    aggregated$marginal_upperbound = 
      apply(aggregated, 
            MARGIN = 1, 
            FUN = marginal_upperbound)

    return(aggregated)
    
  }

# apply frechet instrumental function to all populations 
lapply(pops, frechet_instrumental)
