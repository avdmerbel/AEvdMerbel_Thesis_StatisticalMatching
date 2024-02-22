#############################################################################
#                                                                           #
#      Function to get bootstrap estimations for statistical matching       #  
#                                                                           #
#############################################################################

StatisticalMatching_Bootstrap <- 
  function(sample_input,
           assumption = c("IVA", 
                          "CIA"),
           restriction = FALSE
  ){
    
    # INPUT:
    # sample_input is a dataframe with the sample you want to use for matching, should be the same format as the output from Generate_Sample
    # assumption is which assumption is used for estimation, Instrumental Variable or Conditional Independence
    # restriction means if the probabilities are restricted between 0 and 1 (FALSE by default)
    
    
    # OUTPUT:
    # bootstraps is a numeric vector with the estimated joint distribution for each combination
    
    # Load required function
    source("EstimateDistribution_InstrumentalVariable.R")
    source("EstimateDistribution_ConditionalIndependence.R")
    
    
    #------------------------------ BOOTSTRAP SAMPLES -----------------------------#
    
    # First create the bootstrap samples by resampling the data with replacement
    # this is done by sampling the numbers 1 through 4, corresponding to the category
    # combinations in the data set.
    
    # Sample 1
    new_sample1 = 
      sample(x = 1:4, 
             size = sum(sample_input$FirstSample$count), 
             prob = sample_input$FirstSample$proportion,
             replace = TRUE)
    
    # replace the counts in the original sample with the new counts from resampling
    boot_sample1 = sample_input$FirstSample
    boot_sample1$count = table(new_sample1)
    boot_sample1$proportion = boot_sample1$count/sum(boot_sample1$count)
    
    # Get the proportion into a contingency table
    boot_sample1_table = 
      xtabs(proportion ~ .,
            # remove the third column (count)
            data = boot_sample1[, -3])
    
    # Sample 2
    new_sample2 = 
      sample(x = 1:4, 
             size = sum(sample_input$SecondSample$count), 
             prob = sample_input$SecondSample$proportion,
             replace = TRUE)
    
    boot_sample2 = sample_input$SecondSample
    boot_sample2$count = table(new_sample2)
    boot_sample2$proportion = boot_sample2$count/sum(boot_sample2$count)
    
    boot_sample2_table = 
      xtabs(proportion ~ .,
            data = boot_sample2[, -3])
    
    
    # Define sample_input for the estimation functions
    bootstrap_sample_input = 
      list("DistributionFirstSample" = boot_sample1_table,
           "DistributionSecondSample" = boot_sample2_table,
           "CommonVariable" = sample_input$CommonVariable,
           "N1" = sum(boot_sample1$count),
           "N2" = sum(boot_sample2$count))
    
    
    #-------------------------------- MATCHING IVA --------------------------------#
    
    if(assumption == "IVA"){
      
     
      
      # Perform statistical matching based on the new samples
      matching =
        EstimateDistribution_InstrumentalVariable(sample_input = bootstrap_sample_input)
    } 
    
    #-------------------------------- MATCHING CIA --------------------------------#
    
    else {
      
      
      # Perform statistical matching based on the new samples
      matching =
        EstimateDistribution_ConditionalIndependence(sample_input = bootstrap_sample_input)
      
    }
    
    #--------------------------------- ESTIMATION ---------------------------------#
    
    
    # If restriction = TRUE use the restricted joint probability
    
    if(restriction == TRUE){ 
      
      estimated = matching[, "joint_probability_restricted"] 
      
    } else {
      
      estimated = matching[, "joint_probability"]
      
    }
    
    # Save the combinations so later we can see which probability belongs to which combo
    variable_combinations = matching[,1:2]
    
    #------------------------------------ OUTPUT ----------------------------------#
    
    return(list(estimated, variable_combinations))
    
  }

