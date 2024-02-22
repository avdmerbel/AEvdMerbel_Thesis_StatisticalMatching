################################################################################
##                                                                            ##
##              A function that performs the full simulation analysis         ##
##                                                                            ##
################################################################################

StatisticalMatching_FullSimulation <-
  function(M = 500,
           population,
           assumption = "IVA",
           restriction = FALSE,
           bootstraps = FALSE,
           nr.bootstraps = 10,
           n1 = 2000,
           n2 = 2000,
           situations = c("instrumental", 
                          "mediator", 
                          "outcome")  
  ){
    
    # INPUT:
    # M: numeric, indicates the number of simulations to run
    # population: output from GeneratePopulation() containing the population to 
    # simulate from
    # assumption: string variable indicating which estimation method to use
    # restriction: boolean indicating whether to restrict the probabilities
    # bootstraps: boolean indicating whether to save the samples to apply bootstrap
    # on later
    # nr.bootstraps: numeric, indicating how many samples to save for bootstrapping
    # n1: numeric, indicating the sample size for sample 1
    # n2: numeric, indicating the sample size for sample 2
    # situations: string, indicating which variable should be overlapping in the data
    
    # OUTPUT:
    # samplesBootstrap: list of samples that were saved for the bootstrapping (if TRUE)
    # EstimationError: dataframe containing the variable combinations and the 
    # estimated bias, RMSE, standard deviation
    # TrueParameters: dataframe containing the variable combinations and the point
    # estimates of the mean, standard deviation, confidence intervals
    
    # load necessary functions
    source("StatisticalMatching_Simulation.R")
    source("StatisticalMatching_EstimationError.R")
    source("StatisticalMatching_TrueParameters.R")
    
    
    #-------------------------------- SIMULATION ------------------------------#
    
    
    # check whether to save the bootstrap samples
    # if bootstraps == TRUE initialize a list to save the samples
    if(bootstraps == FALSE) samples = NULL else samples = vector("list", length = nr.bootstraps) 
    
    # initialize variable combinations and a list for the simulations
    variable_combinations = NA
    simulations = vector("list", length = M)
    
    # for loop where in each iteration a spot in simulations is filled with a 
    # sample and for the first nr.bootstraps iterations (if bootstraps == TRUE)
    # a spot in samples is filled with the current sample 
    for(i in 1:M){
      
      simmies = 
        StatisticalMatching_Simulation(population = population$FullPopulation, 
                                       assumption = assumption,
                                       restriction = restriction,
                                       situations = situations,
                                       n1 = n1,
                                       n2 = n2,
                                       bootstraps = bootstraps)
      
      if(i == 1) variable_combinations = simmies[[2]]
      
      if(i <= nr.bootstraps & bootstraps == TRUE) samples[[i]] = simmies[[3]]
      
      simulations[[i]] = 
        simmies[[1]]
    }
    
    # turn list into array
    simulations =
      simplify2array(simulations)
    
    # data wrangling to get a nice matrix with the situations and the estimates
    together = 
      lapply(seq(1, nrow(simulations)), 
             FUN = function(x) do.call(cbind, 
                                       simulations[x,]))
    
    names(together) = names(variable_combinations)
    
    
    #------------------------- CONFIDENCE INTERVALS ---------------------------#
    
    
    # sort all estimates
    sorted = 
      lapply(names(together),
             FUN = function(x) apply(together[[x]],
                                     MARGIN = 1,
                                     FUN = sort)
      )  
    
    # get 95% confidence intervals from sorted estimates
    intervals = 
      lapply(seq(1, length(sorted)),
             FUN = function(x) apply(sorted[[x]],
                                     MARGIN = 2,
                                     FUN = quantile,
                                     probs = c(0.025, 0.975)))
    
    names(intervals) = names(together)
    
    
    #------------------------------ ESTIMATION --------------------------------#
    
    
    # Get the estimation error from the M simulations
    estimation_error =
      StatisticalMatching_EstimationError(simulations = simulations,
                                          variable_combinations = variable_combinations,
                                          population_distribution = population$AggregatedDistribution)
    
    
    # Get the true parameters from the M simulations
    true_parameters =
      StatisticalMatching_TrueParameters(simulations = simulations,
                                         variable_combinations = variable_combinations)
    
    # add confidence intervals to true parameters
    true_parameters = 
      lapply(names(true_parameters), FUN = function(x) cbind(true_parameters[[x]], 
                                                             lower_CI = intervals[[x]][1,],
                                                             upper_CI = intervals[[x]][2,]))
    
    names(true_parameters) = names(variable_combinations)
    
    #---------------------------------- OUTPUT --------------------------------#
    output = 
      list("SamplesBootstrap" = samples,
           "EstimationError" = estimation_error,
           "TrueParameters" = true_parameters)
    
    return(output)
    
  }
