################################################################################
##                                                                            ##
##              A function that performs the full bootstrap analysis          ##
##                                                                            ##
################################################################################

StatisticalMatching_FullBootstrap <-
  function(B,
           sample_input,
           assumption = c("IVA", 
                          "CIA"),
           restriction = FALSE
  ){
    
    # INPUT:
    # B: numeric, number of bootstrap samples to be taken
    # sample_input: output from generate_sample that contains the samples 
    # to be bootstrapped
    # assumption: string indicating whether to use IVA or CIA for estimation
    # restriction: Boolean indicating whether to restrict the probabilities
    
    # OUTPUT:
    # bootstrap_parameters: dataframe containing all variable combinations and
    # their estimates (mean, standard deviation, bias, RMSE, confidence intervals)
    
    # Load necessary functions
    source("StatisticalMatching_Bootstrap.R")
    source("StatisticalMatching_BootstrapParameters.R")
    source("Check_Order.R")
    
    # Initiate empty bootstraps and variable combinations
    variable_combinations = NA
    bootstraps = vector("list", length = B)
    
    # bootstrap loop
    for(i in 1:B){
      
      # draw 1 bootstrap sample for each scenario
      boots = 
        lapply(names(sample_input), 
               FUN = function(x) StatisticalMatching_Bootstrap(sample_input[[x]], 
                                                               assumption = assumption,
                                                               restriction = restriction))
      
      # extract the variable_combinations from the bootstraps
      if(i == 1) variable_combinations = sapply(boots, "[", 2)
      
      # add bootstrap sample to bootstraps list
      bootstraps[[i]] = 
        sapply(boots, "[[", 1, simplify = FALSE)
    }
    
    # change the names of variable combinations to the scenarios
    names(variable_combinations) = 
      names(sample_input)
    
    # data wrangling
    bootstraps =
      simplify2array(bootstraps)
    
    rownames(bootstraps) = 
      names(sample_input)
    
    # get bootstraps together for easier calculating
    together = 
      lapply(seq(1, nrow(bootstraps)), 
             FUN = function(x) do.call(cbind, 
                                       bootstraps[x,]))
    
    names(together) = names(variable_combinations)
    
    # sort bootstraps from smallest to largest to get confidence intervals
    sorted = 
      lapply(names(together),
             FUN = function(x) apply(together[[x]],
                                     MARGIN = 1,
                                     FUN = sort)
      )  
    
    # confidence intervals (95%)
    intervals = 
      lapply(seq(1, length(sorted)),
             FUN = function(x) apply(sorted[[x]],
                                     MARGIN = 2,
                                     FUN = quantile,
                                     probs = c(0.025, 0.975)))
    
    names(intervals) = names(together)
    
    # extract the unobserved distribution in the sample (sneaky)
    sample_distribution_unobserved = 
      lapply(sample_input, with, UnobservedDistribution)
    
    # Apply the Check_Order function to the input arguments and get the new 
    # population distribution input (either nothing or only the order changed)
    sample_distribution_unobserved = 
      Check_Order(population_distribution = sample_distribution_unobserved,
                  variable_combinations = variable_combinations)
    
    # compute bootstrap estimates
    bootstrap_parameters = 
      StatisticalMatching_BootstrapParameters(bootstraps = bootstraps,
                                              variable_combinations = variable_combinations,
                                              sample_distribution_unobserved = sample_distribution_unobserved)
    
    # bind confidence intervals to parameter output
    bootstrap_parameters = 
      lapply(names(bootstrap_parameters), FUN = function(x) cbind(bootstrap_parameters[[x]], 
                                                                  lower_CI = intervals[[x]][1,],
                                                                  upper_CI = intervals[[x]][2,]))
    
    names(bootstrap_parameters) = names(variable_combinations)
    
    return(bootstrap_parameters)
  }