#######################################################################################
#                                                                                     #
# Function calculating the distribution under the conditional independence assumption #
#                                                                                     #
#######################################################################################

# We can estimate P(Z,Y) in the following way:
# P(Z,Y) = sum_x( P(Z | X=x)P(Y | X=x)P(X = x) )
# here X is always the common variable and Z and Y are always the disjoint variables


EstimateDistribution_ConditionalIndependence <-
  function(sample_input
  ){
    
    require(tools)
    require(dplyr)
    require(stats)
    require(sampling)
    
    # INPUT:
    # sample_input is the output from Generate_InstrumentalVariable_Sample 
    
    # OUTPUT:
    # Conditional probability tables from each of the data sets
    # The estimation of the joint probability for the not jointly observed variables
    # The joint distribution tables of the samples
    
    #--------------------------------- PREPARATION --------------------------------#
    
    # Define necessary samples from the input
    sample1 = sample_input$DistributionFirstSample
    sample2 = sample_input$DistributionSecondSample
    common_variable = sample_input$CommonVariable
    n1 = sample_input$N1
    n2 = sample_input$N2
    
    # Save variable names
    y_variable = 
      names(dimnames(sample1))[names(dimnames(sample1)) != common_variable]
    z_variable = 
      names(dimnames(sample2))[names(dimnames(sample2)) != common_variable]
    
    # Adapt column names in the samples for easier subsetting
    names(dimnames(sample1))[names(dimnames(sample1)) == common_variable] = "x"
    names(dimnames(sample1))[names(dimnames(sample1)) == y_variable] = "y"
    names(dimnames(sample2))[names(dimnames(sample2)) == common_variable] = "x"
    names(dimnames(sample2))[names(dimnames(sample2)) == z_variable] = "z"
    

    # Define the function that calculates the joint distribution of the not jointly observed variables
    probability_z_y <- 
      function(m){
        
        ( y_given_x_probabilities[1, m[1]] * z_given_x_probabilities[1, m[2]] * p_x) +
          ( y_given_x_probabilities[2, m[1]] * z_given_x_probabilities[2, m[2]] * (1 - p_x))
      
      }
    
    # check if the x variable is in the rows because we condition on that one
    if(names(dimnames(sample1))[1] != "x") sample1 = t(sample1) 
    if(names(dimnames(sample2))[1] != "x") sample2 = t(sample2) 
    
    #-------------------------------- PROBABILITIES -------------------------------#
    
    
    # Create conditional probability tables
    # We want y given x, z given x and the result z,y
    # The conditional variable is in the rows of the contingency table
    
    # Create the conditional probability tables y given x and z given x from the data
    y_given_x_probabilities =
      sample1 %>%
      prop.table(margin = 1)
    
    z_given_x_probabilities = 
      sample2 %>%
      prop.table(margin = 1)
    
    # Get total probability of x
    p_x = 
      ( sum(sample1[1,]) + sum(sample2[1,]) ) / 2
    
    # Initialize the data frame with all possible combinations for the not jointly observed variables z and y
    z_y = 
      expand.grid(y = c(1,2),
                  z = c(1,2)
      )
    
    # Apply the function probability_z_y defined earlier to each row of the data frame
    # This will give the joint probability of z and y for each combination of z and y
    z_y$joint_probability = 
      apply(z_y,
            MARGIN = 1,
            FUN = probability_z_y
      )
    
    # Add a column with restricted probabilities
    # probabilities can only be between 0 and 1, but when the assumption does not hold or because of sampling
    # this might not hold, so coerce the probabilities between 0 and 1
    # note that when the conditional independence assumption holds, these probabilities will be 
    # equivalent to the non-restricted probabilities 
    
    z_y$joint_probability_restricted = 
      z_y$joint_probability
    
    z_y$joint_probability_restricted[which(z_y$joint_probability < 0)] = 0
    z_y$joint_probability_restricted[which(z_y$joint_probability > 1)] = 1
  
    
    
    # Add counts based on full sample
    # Rounding while preserving the correct totals
    z_y$count = 
      trunc(z_y$joint_probability * sum(n1, n2)) + 
      sampling::UPrandomsystematic(z_y$joint_probability * sum(n1, n2) - 
                                     trunc(z_y$joint_probability * sum(n1, n2))
      )
    
    # For the restricted probabilities
    # Rounding while preserving the correct totals
    z_y$count_restricted = 
      trunc(z_y$joint_probability_restricted * sum(n1, n2)) + 
      sampling::UPrandomsystematic(z_y$joint_probability_restricted * sum(n1, n2) - 
                                     trunc(z_y$joint_probability_restricted * sum(n1, n2))
      )
    
    #----------------------------------- OUTPUT -----------------------------------#
    
    names(z_y)[2] = z_variable
    names(z_y)[1] = y_variable
    
    return(z_y)
  }

