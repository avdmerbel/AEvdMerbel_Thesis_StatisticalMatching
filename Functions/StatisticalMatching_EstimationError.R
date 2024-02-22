################################################################################
#                                                                              #    
#        Function to get estimation error statistics from the simulation       #
#                                                                              #
################################################################################


StatisticalMatching_EstimationError <-
  
  function(simulations,
           variable_combinations,
           population_distribution
  ){
    
    # INPUT:
    # simulations is a matrix with lists of vectors which includes the simulations 
    # ran using StatisticalMatching_Simulation
    # variable_combinations is a list of matrices which represents the order of the 
    # combinations of variables. This is part of the output from the function 
    # StatisticalMatching_Simulation. Make sure these are named properly! 
    # population_distribution is a list of matrices which represent the population
    # distribution of the not commonly observed variables for each situation, this
    # is part of the output from GeneratePopulation_(Not)InstrumentalVariable
    
    # OUTPUT:
    # result is a list of matrices, one matrix for each matching situation. These
    # matrices include the variable combinations, the mean and median of the absolute
    # difference between the simulated estimated distributions and the population
    # distribution, and the MSE and RMSE of these differences
    
    #-------------------------------- PREPARATION ---------------------------------#
    
    source("Check_Order.R")
    
    # Apply the Check_Order function to the input arguments and get the new 
    # population distribution input (either nothing or only the order changed)
    new_aggregate = 
      Check_Order(population_distribution = population_distribution,
            variable_combinations = variable_combinations)
    
    
    #------------------------------ CALCULATE ERRORS ------------------------------#
    
    # initialize empty lists for MAE, the MSE and the RMSE
    Bias = list()
    MAE = list()
    MSE = list()
    RMSE = list()
    
    for(i in 1:length(new_aggregate)){
      
      # Difference between the elements of the estimated joint distribution
      # and the elements of the true distribution in the population
      # for easy bias calculation
      difference = 
        sapply(seq(1, length(simulations[i,])),
               FUN = 
                 function(x) simulations[i,][[x]] - new_aggregate[[i]]$proportion)
      
      # squared difference to calculate the MSE
      squared_difference = 
        difference^2
      
      # absolute differenc to calculate the MAE (?)
      abs_difference = 
        abs(difference)
      
      # Bias
      Bias[[i]] = 
        apply(difference,
              MARGIN = 1,
              FUN = mean)
      
      # Mean absolute error
      MAE[[i]] = 
        apply(abs_difference,
              MARGIN = 1,
              FUN = mean)
      
      # Mean squared error 
      MSE[[i]] = 
        apply(squared_difference,
              MARGIN = 1,
              FUN = mean)
      
      # Root mean squared error
      RMSE[[i]] = 
        sapply(seq(1, length(MSE[[i]]) ),
               FUN = 
                 function(x) sqrt( MSE[[i]][x])
               )
      
    }
    
    #----------------------------------- OUTPUT -----------------------------------#
    
    result = 
      Map(cbind, 
          variable_combinations, 
          Bias = Bias,
          MAE = MAE,
          MSE = MSE,
          RMSE = RMSE)
    
    
    return(result)
  }