#############################################################################################
#                                                                                           #
#  Function to calculate the true parameters of the statistical matching from simulations   #  
#                                                                                           #
#############################################################################################


StatisticalMatching_TrueParameters <-
  
  function(simulations,
           variable_combinations
  ){
    
    # INPUT:
    # simulation is a matrix with lists of vectors which includes the simulations 
    # ran using StatisticalMatching_Simulation
    # variable_combinations is a list of vectors which represents the order of the 
    # combinations of variables this part of the output from the function StatisticalMatching_Simulation
    
    # OUTPUT:
    # result is a list with a matrix for each situation containing the combination 
    # of variables, the mean and median, and the variance and the standard deviation
    
    
    
    # data wrangling to get estimations together per situation
    # simulation is a matrix with lists of vectors which is a bit challenging to work with
    # together is a list of matrices which is easier to work with later on
    together = 
      lapply(seq(1, nrow(simulations)), 
             FUN = function(x) do.call(cbind, 
                                       simulations[x,]))
    
    names(together) = names(variable_combinations)
    
    # calculate variance and standard deviation over all simulations, 
    # per situation and bind to variable combinations for reference 
    result = 
      Map(cbind, 
          variable_combinations,
          mean = lapply(names(together),
                        FUN = function(x) apply(together[[x]],
                                                MARGIN = 1,
                                                FUN = mean)),
          median = lapply(names(together),
                          FUN = function(x) apply(together[[x]],
                                                  MARGIN = 1, 
                                                  FUN = median)),
          variance = lapply(names(together), 
                            FUN = function (x) apply(together[[x]], 
                                                     MARGIN = 1, 
                                                     FUN = var)),
          standard_deviation = lapply(names(together), 
                                      FUN = function (x) apply(together[[x]], 
                                                               MARGIN = 1, 
                                                               FUN = sd))
      )
    
    return(result)
    
  }