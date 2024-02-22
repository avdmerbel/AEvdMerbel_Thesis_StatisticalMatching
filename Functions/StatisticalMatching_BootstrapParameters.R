#############################################################################
#                                                                           #
#       Function to get bootstrap parameters for statistical matching       #  
#                                                                           #
#############################################################################

StatisticalMatching_BootstrapParameters <-
  function(bootstraps,
           variable_combinations,
           sample_distribution_unobserved
  ){
    
    # INPUT:
    # bootstraps is a matrix of lists containing vectors which include the
    # estimations for all variable combination in each situation. It is the
    # output of StatisticalMatching_Bootstrap
    # variable_combinations is a list with all variable combinations for each
    # situation. This is also in the output of StatisticalMatching_Bootstrap
    
    # OUTPUT:
    # A list of dataframes (one for each situation) with the variable
    # combinations, the mean, median, variance and standard deviations
    # of the bootstrapped distributions
    
    
    # data wrangling to get estimations together per situation
    # This way the data is easier to deal with than the datatype of bootstraps
    together = 
      lapply(seq(1, nrow(bootstraps)), 
             FUN = function(x) do.call(cbind, 
                                       bootstraps[x,]))
    
    # adjust the names so Map will work also to know which distributions
    # belong to which situation
    names(together) = names(variable_combinations)
    
    # get difference estimated value per bootstrap and sample distribution (sneaky)
    differences = 
      lapply(names(variable_combinations),
             FUN = 
               function(x) together[[x]]- sample_distribution_unobserved[[x]]$proportion)
    
    names(differences) = names(variable_combinations)
    
    # geet squared differences
    squared_differences = 
      lapply(names(variable_combinations),
             FUN = 
               function(x) differences[[x]]**2)
    
    names(squared_differences) = names(variable_combinations)
    
    # calculate bootstrapped mean
    mean = 
      lapply(names(variable_combinations),
             FUN = function(x) apply(together[[x]],
                                     MARGIN = 1,
                                     FUN = mean))
    
    # bootstrapped variance
    variance = 
      lapply(names(variable_combinations), 
             FUN = function(x) apply(together[[x]], 
                                     MARGIN = 1, 
                                     FUN = var))
    
    # Bias of the bootstrap
    Bias = 
      lapply(names(variable_combinations),
             FUN = function(x) apply(differences[[x]],
                                     MARGIN = 1,
                                     FUN = mean))
    
    # RMSE of the bootstrap
    RMSE = 
      lapply(names(variable_combinations),
             FUN = function(x) apply(squared_differences[[x]],
                                     MARGIN = 1,
                                     FUN = mean)) %>%
      lapply(FUN = function(x) sqrt(x))
    
    # bind all parameters to variable combinations
    result = 
      Map(cbind, 
          variable_combinations, 
          mean = mean,
          variance = variance,
          standard_deviation = lapply(variance, FUN = function(x) sqrt(x)),
          Bias = Bias,
          RMSE = RMSE
      )
    
    names(result) = names(variable_combinations)
    
    return(result)
  }