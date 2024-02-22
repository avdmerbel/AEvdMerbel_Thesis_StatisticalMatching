

StatisticalMatching_Simulation <-
  
  function(population,
           assumption = c("IVA",
                          "CIA"),
           restriction = FALSE,
           situations = c("instrumental", 
                          "mediator", 
                          "outcome"),
           n1 = 2000,
           n2 = 2000,
           bootstraps = FALSE
  ){
    
    # INPUT
    # population is a dataframe of the population we want to sample from; 
    # should be in the form of FullPopulation from Generate_Population
    # assumption is a string indicating under which assumption we are estimating the probabilities; 
    # instrumental variable or conditional independence
    # restriction is a boolean indicating whether to restrict the probabilities to be between 0 and 1
    # situations is a string indicating which situations are under investigation;
    # ergo which variables are common (defaults to all situations)
    # n1 and n2 are numeric indicating what the sample sizes should be 
    
    # OUTPUT
    # estimated is a vector with the estimated probabilities of the joint distribution of the not jointly observed variables
    # variable_combinations is a data frame with the category combinations of the variables under investigation (for reference)
    
    # Load the required functions
    source("GenerateSample.R")
    source("EstimateDistribution_InstrumentalVariable.R")
    source("EstimateDistribution_ConditionalIndependence.R")
    
    #------------------------------ SAMPLE POPULATION -----------------------------#
    
    # Generate the samples from the provided population for each situation (common variable)
    samples = 
      lapply(situations,
             FUN = function(x) GenerateSample(population = population,
                                              common_variable = x,
                                              n1 = n1,
                                              n2 = n2)
      )
    
    names(samples) =
      c("CommonInstrumental",
        "CommonMediator",
        "CommonOutcome"
      )
    
    #-------------------------------- MATCHING IVA --------------------------------#
    
    # If the IVA is used
    if(assumption == "IVA"){
      
      # Perform statistical matching based on the new samples
      matching =
        lapply(samples,
               FUN = 
                 function(x) EstimateDistribution_InstrumentalVariable(sample_input = x)
        )
      #-------------------------------- MATCHING CIA --------------------------------#
      
      # if the CIA is used
    } else if(assumption == "CIA") {
      
      # Perform statistical matching based on the new samples
      matching =
        lapply(samples,
               FUN = 
                 function(x) EstimateDistribution_ConditionalIndependence(sample_input = x)
        )
      
    } 
    
    #--------------------------------- ESTIMATION ---------------------------------#
    
    # If restriction = TRUE use the restricted joint probability
    if(restriction == TRUE){ 
      
      estimated = 
        lapply(matching, with, joint_probability_restricted) 
      
    } else {
      
      estimated = 
        lapply(matching, with, joint_probability)
      
    }
    
    # Save the combinations so later we can see which probability belongs to which combo
    variable_combinations = 
      lapply(matching, '[', 1:2) # select first 2 columns of each list element
    
    #----------------------------------- OUTPUT -----------------------------------#
    
    # if we want to take bootstrap samples, also output the samples that were used
    if(bootstraps == TRUE){
      output = list(estimated,
                    variable_combinations,
                    samples)
    } else {
      output = list(estimated,
                    variable_combinations)
    }
    
    return(output)
    
  }




