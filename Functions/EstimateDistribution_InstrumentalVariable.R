####################################################################
##                                                                ##
##  Function to estimate the distribution of not jointly observed ##
##  variables in the instrumental variable situation(2x2)         ##
##                                                                ##
####################################################################

# The formula for the conditional probability in the instrumental variable situation looks like:
# P(Outcome = k | Instrumental = i) = \sum_J P(Mediator = j | Instrumental = i)*P(Outcome = k | Mediator = j)

# In the simplest situation, in one data set the mediator and instrumental variable are jointly observed
# and the mediator and outcome are jointly observed in the other data set. 
# In that case all conditional probabilities can be calculated directly from the data set and filled in in the equation
# In the other situations (where the mediator is not the commonly observed variable),
# calculating the conditional probabilities means we have to solve some equations.

EstimateDistribution_InstrumentalVariable <- 
  function(sample_input,
           instrumental = "instrumental",
           mediator = "mediator",
           outcome = "outcome"
  ){
    
    # INPUT:
    # sample_input is the generated sample from GenerateSample
    # In the instrumental, mediator and outcome arguments the user can specify which variable in the
    # data set has which role. Defaults are set to the output of GenerateSample
    
    # OUTPUT:
    # A list containing:
    # Conditional probability tables for all combinations of variables
    
    #------------------------------- SOME DEFINITIONS -----------------------------#
    
    sample1 = sample_input$DistributionFirstSample
    sample2 = sample_input$DistributionSecondSample
    common_variable = sample_input$CommonVariable
    n1 = sample_input$N1
    n2 = sample_input$N2
    
    #--------------------------------- PREPARATION --------------------------------#
 
    
    # Define the function that calculates the outcome given the instrumental probabilities
    probability_out_conditional_ins <- 
      function(m){
        ( mediator_given_instrumental[m[1], 1] * outcome_given_mediator[1, m[2]] ) +
          ( mediator_given_instrumental[m[1], 2] * outcome_given_mediator[2, m[2]] )
      }
    
    
    
    #-------------------------------- PROBABILITIES -------------------------------#
    
    
    
    # Create conditional probability tables
    # We want 
    # outcome given instrumental
    # outcome given mediator
    # mediator given instrumental
    # The conditional variable is then in the rows of the contingency table
    # most of these tables can be estimated directly from the data
    
    
    #------------------------------- MEDIATOR COMMON ------------------------------#
    
    
    # If the mediator is the common variable (scenario 1)
    # so we need to calculate: outcome conditional mediator
    if(common_variable == mediator){
      
      scenario = 1
      
      # check if the conditional variable is in the rows
      if((names(dimnames(sample1))[1] != "mediator" & "outcome" %in% names(dimnames(sample1))) |
         (names(dimnames(sample1))[1] == "mediator" & "instrumental" %in% names(dimnames(sample1)))){
        sample1 = t(sample1) 
      }
      
      if(names(dimnames(sample2))[1] != "mediator"& "outcome" %in% names(dimnames(sample2))|
         (names(dimnames(sample1))[1] == "mediator" & "instrumental" %in% names(dimnames(sample1)))){
        sample2 = t(sample2) 
      }
      
      
      # if the outcome is in the first sample
      if("outcome" %in% names(dimnames(sample1))){
        
        # Create the conditional probability tables from the data
        outcome_given_mediator = 
          sample1 %>% prop.table(margin = 1)
        
        mediator_given_instrumental = 
          sample2 %>% prop.table(margin = 1)
        
        
        # Distribution of the instrumental variable to calculate the joint probability
        # put it here so we don't need an extra if statement
        
        # Get P(instrumental | mediator) so we can calculate P(instrumental) from the full data set
        instrumental_given_mediator = 
          t(sample2) %>% prop.table(margin = 1)
        
        # Get P(mediator) from the whole data set
        p_mediator = 
          ( sum(sample1[1,]) + sum(sample2[,1]) ) / 2
        
        # Get P(instrumental) from the whole data set
        # P(instrumental | mediator = m)P(mediator = m) = P(instrumental)
        p_instrumental = 
          (instrumental_given_mediator[1,1] * p_mediator) +
          (instrumental_given_mediator[2,1] * (1 - p_mediator))
      
      # if the outcome is in the second sample  
      } else {
        
        # Create the conditional probability tables from the data
        outcome_given_mediator = 
          sample2 %>% prop.table(margin = 1)
        
        mediator_given_instrumental = 
          sample1 %>% prop.table(margin = 1)
        
        
        # Distribution of the instrumental variable to calculate the joint probability
        # put it here so we don't need an extra if statement
        
        # Get P(instrumental | mediator) so we can calculate P(instrumental) from the full data set
        instrumental_given_mediator = 
          t(sample1) %>% prop.table(margin = 1)
        
        # Get P(mediator) from the whole data set
        p_mediator = 
          ( sum(sample1[,1]) + sum(sample2[1,]) ) / 2
        
        # Get P(instrumental) from the whole data set
        # P(instrumental | mediator = m)P(mediator = m) = P(instrumental)
        p_instrumental = 
          (instrumental_given_mediator[1,1] * p_mediator) +
          (instrumental_given_mediator[2,1] * (1 - p_mediator))

      }
      
      # Initialize the data frame with all possible combinations for the not jointly observed variables
      outcome_given_instrumental = 
        expand.grid(instrumental = c(1,2),
                    outcome = c(1,2)
        )
      
      # Apply the function probability_out_conditional_ins defined earlier to each row of the data frame
      outcome_given_instrumental$conditional_probability = 
        apply(outcome_given_instrumental, 
              MARGIN = 1,
              FUN = probability_out_conditional_ins
        )
      
      # Add a column with restricted probabilities
      # probabilities can only be between 0 and 1, but when the assumption does not hold or because of sampling
      # this might not hold, so coerce the probabilities between 0 and 1
      # note that when the instrumental variable assumption holds, these probabilities will be 
      # equivalent to the non-restricted probabilities 
      outcome_given_instrumental$conditional_probability_restricted = 
        outcome_given_instrumental$conditional_probability
      
      outcome_given_instrumental$conditional_probability_restricted[which(outcome_given_instrumental$conditional_probability_restricted < 0)] = 0
      outcome_given_instrumental$conditional_probability_restricted[which(outcome_given_instrumental$conditional_probability_restricted > 1)] = 1
    
 
      # Add the joint probabilities for the outcome and the instrumental
      # using the chain rule: P(A,B) = P(A|B)P(B)
      
      outcome_given_instrumental$joint_probability = 
        ifelse(outcome_given_instrumental$instrumental == 1,
               outcome_given_instrumental$conditional_probability * p_instrumental,
               outcome_given_instrumental$conditional_probability * (1-p_instrumental)
        )
      
      # For the restricted probabilities
      outcome_given_instrumental$joint_probability_restricted = 
        ifelse(outcome_given_instrumental$instrumental == 1,
               outcome_given_instrumental$conditional_probability_restricted * p_instrumental,
               outcome_given_instrumental$conditional_probability_restricted * (1-p_instrumental)
        )
      
      # Add counts based on full sample
      # Rounding while preserving the correct totals
      outcome_given_instrumental$count = 
        trunc(outcome_given_instrumental$joint_probability * sum(n1, n2)) + 
        sampling::UPrandomsystematic(outcome_given_instrumental$joint_probability * sum(n1, n2) - 
                                       trunc(outcome_given_instrumental$joint_probability * sum(n1, n2))
        )
      
      # For the restricted probabilities
      # Rounding while preserving the correct totals
      outcome_given_instrumental$count_restricted = 
        trunc(outcome_given_instrumental$joint_probability_restricted * sum(n1, n2)) + 
        sampling::UPrandomsystematic(outcome_given_instrumental$joint_probability_restricted * sum(n1, n2) - 
                                       trunc(outcome_given_instrumental$joint_probability_restricted * sum(n1, n2))
        )
      
      
      #------------------------------ OUTCOME COMMON ------------------------------#
      
      
      # If the outcome is the common variable (scenario 2)
      # so we need to calculate: mediator conditional instrumental
    } else if(common_variable == outcome){
      
      scenario = 2
      
      # check if the conditional variable is in the rows
      if(names(dimnames(sample1))[1] == "outcome") sample1 = t(sample1) 
      if(names(dimnames(sample2))[1] == "outcome") sample2 = t(sample2)
  
      
      # if the instrumental is in the first sample
      if(names(dimnames(sample1))[1] == "instrumental"){
        
        # Create the conditional probability tables from the data
        outcome_given_instrumental = 
          sample1 %>% prop.table(margin = 1)
        
        outcome_given_mediator = 
          sample2 %>% prop.table(margin = 1)
        
        # Distribution of the instrumental variable to calculate the joint probability
        # put it here so we don't need an extra if statement
        
        # Get P(instrumental | outcome) so we can calculate P(instrumental) from the full data set
        instrumental_given_outcome = 
          t(sample1) %>% prop.table(margin = 1)
        
        # Get P(outcome) from the whole data set
        p_outcome = 
          ( sum(sample1[,1]) + sum(sample2[,1]) ) / 2
        
        # Get P(instrumental) from the whole data set
        # P(instrumental | outcome = o)P(outcome = o) = P(instrumental)
        p_instrumental = 
          (instrumental_given_outcome[1,1] * p_outcome) +
          (instrumental_given_outcome[2,1] * (1-p_outcome))
        
      # if the instrumental is in the second sample
      } else {
        
        # Create the conditional probability tables from the data
        outcome_given_instrumental = 
          sample2 %>% prop.table(margin = 1)
        
        outcome_given_mediator = 
          sample1 %>% prop.table(margin = 1)
        
        
        # Distribution of the instrumental variable to calculate the joint probability
        # put it here so we don't need an extra if statement
        
        # Get P(instrumental | outcome) so we can calculate P(instrumental) from the full data set
        instrumental_given_outcome = 
          t(sample2) %>% prop.table(margin = 1)
        
        # Get P(outcome) from the whole data set
        p_outcome = 
          ( sum(sample1[1,]) + sum(sample2[1,]) ) / 2
        
        # Get P(instrumental) from the whole data set
        # P(instrumental | outcome = o)P(outcome = o) = P(instrumental)
        p_instrumental = 
          (instrumental_given_outcome[1,1] * p_outcome) +
          (instrumental_given_outcome[2,1] * (1-p_outcome))
        
      }
      
      
      # Initialize the data frame with all possible combinations of the not jointly observed variables
      mediator_given_instrumental = 
        expand.grid(mediator = c(1,2),
                    instrumental = c(1,2)
        )
      
      # We use the solve function to solve the set of equations to get the probabilities of
      # the mediator given the levels of the instrumental variable
      
      # for instrumental = 1
      mediator_given_instrumental$conditional_probability[1:2] = 
        solve(t(outcome_given_mediator), 
              outcome_given_instrumental[1,]
        )
      
      # for instrumental = 2
      mediator_given_instrumental$conditional_probability[3:4] = 
        solve(t(outcome_given_mediator), 
              outcome_given_instrumental[2,]
        )
      
      # Add a column with restricted probabilities
      # probabilities can only be between 0 and 1, but when the assumption does not hold or because of sampling
      # this might not hold, so coerce the probabilities between 0 and 1
      # note that when the instrumental variable assumption holds, these probabilities will be 
      # equivalent to the non-restricted probabilities 
      mediator_given_instrumental$conditional_probability_restricted = 
        mediator_given_instrumental$conditional_probability
      
      mediator_given_instrumental$conditional_probability_restricted[which(mediator_given_instrumental$conditional_probability_restricted < 0)] = 0
      mediator_given_instrumental$conditional_probability_restricted[which(mediator_given_instrumental$conditional_probability_restricted > 1)] = 1

      
      # Add the joint probabilities for the instrumental and the mediator
      # using the chain rule: P(A,B) = P(A|B)P(B)
      
      mediator_given_instrumental$joint_probability = 
        ifelse(mediator_given_instrumental$instrumental == 1,
               mediator_given_instrumental$conditional_probability * p_instrumental,
               mediator_given_instrumental$conditional_probability * (1-p_instrumental)
        )
      
      # For the restricted probabilities
      mediator_given_instrumental$joint_probability_restricted = 
        ifelse(mediator_given_instrumental$instrumental == 1,
               mediator_given_instrumental$conditional_probability_restricted * p_instrumental,
               mediator_given_instrumental$conditional_probability_restricted * (1-p_instrumental)
        )
      
      # Add counts based on full sample? Also might not be necessary if you have the proportion?
      # Rounding while preserving the correct totals
      mediator_given_instrumental$count = 
        trunc(mediator_given_instrumental$joint_probability * sum(n1, n2)) + 
        sampling::UPrandomsystematic(mediator_given_instrumental$joint_probability * sum(n1, n2) - 
                                       trunc(mediator_given_instrumental$joint_probability * sum(n1, n2))
        )
    
      # For the restricted probabilities
      # Rounding while preserving the correct totals
      mediator_given_instrumental$count_restricted = 
        trunc(mediator_given_instrumental$joint_probability_restricted * sum(n1, n2)) + 
        sampling::UPrandomsystematic(mediator_given_instrumental$joint_probability_restricted * sum(n1, n2) - 
                                       trunc(mediator_given_instrumental$joint_probability_restricted * sum(n1, n2))
        )
      
      
      #--------------------------- INSTRUMENTAL COMMON ----------------------------#
      
      
      # If the instrumental is the common variable (scenario 3)
      # so we need to calculate: outcome conditional mediator
    } else {
      
      scenario = 3
      
      # check if the conditional variable is in the rows
      if(names(dimnames(sample1))[1] != "instrumental") sample1 = t(sample1) 
      if(names(dimnames(sample2))[1] != "instrumental") sample2 = t(sample2) 
      
      # if the mediator is in the first sample
      if(names(dimnames(sample1))[2] == "mediator"){
        
        # Create the conditional probability tables from the data
        mediator_given_instrumental = 
          sample1 %>% prop.table(margin = 1)
        
        outcome_given_instrumental = 
          sample2 %>% prop.table(margin = 1)
        
        # Distribution of the mediating variable to calculate the joint probability
        # put it here so we don't need an extra if statement
        
        # Get P(instrumental) from the whole data set
        p_instrumental =
          ( sum(sample1[1,]) + sum(sample2[1,]) ) / 2
        
        # Get P(mediator) from the whole data set
        # P(mediator | instrumental = i)P(instrumental = i) = P(mediator)
        p_mediator = 
          (mediator_given_instrumental[1,1] * p_instrumental ) + 
          (mediator_given_instrumental[2,1] * (1 - p_instrumental))
  
      # if the mediator is in the second sample
      } else {
        
        # Create the conditional probability tables from the data
        mediator_given_instrumental = 
          sample2 %>% prop.table(margin = 1)
        
        outcome_given_instrumental = 
          sample1 %>% prop.table(margin = 1)
        
        
        # Distribution of the mediating variable to calculate the joint probability
        # put it here so we don't need an extra if statement
        
        # Get P(instrumental) from the whole data set
        p_instrumental =
          ( sum(sample1[1,]) + sum(sample2[1,]) ) / 2
        
        # Get P(mediator) from the whole data set
        # P(mediator | instrumental = i)P(instrumental = i) = P(mediator)
        p_mediator = 
          (mediator_given_instrumental[1,1] * p_instrumental ) + 
          (mediator_given_instrumental[2,1] * (1 - p_instrumental))
        
      }
      
      # Initialize the data frame with all possible combinations of the not jointly observed variables
      outcome_given_mediator = 
        expand.grid(mediator = c(1,2),
                    outcome = c(1,2)
        )
      
      # We use the solve function to solve the set of equations to get 
      # the probabilities of the outcome given the levels of the mediator
      
      # for mediator = 1
      outcome_given_mediator$conditional_probability[1:2] = 
        solve(mediator_given_instrumental, 
              outcome_given_instrumental[,1]
        )
      # for mediator = 2
      outcome_given_mediator$conditional_probability[3:4] = 
        solve(mediator_given_instrumental, 
              outcome_given_instrumental[,2]
        )
      
      # Add a column with restricted probabilities
      # probabilities can only be between 0 and 1, but when the assumption does not hold or because of sampling
      # this might not hold, so coerce the probabilities between 0 and 1
      # note that when the instrumental variable assumption holds, these probabilities will be 
      # equivalent to the non-restricted probabilities 
      outcome_given_mediator$conditional_probability_restricted = 
        outcome_given_mediator$conditional_probability
      
      outcome_given_mediator$conditional_probability_restricted[which(outcome_given_mediator$conditional_probability_restricted < 0)] = 0
      outcome_given_mediator$conditional_probability_restricted[which(outcome_given_mediator$conditional_probability_restricted > 1)] = 1
      
      
      # Add the joint probabilities for the outcome and the mediator
      # using the chain rule: P(A,B) = P(A|B)P(B)
      
      outcome_given_mediator$joint_probability = 
        ifelse(outcome_given_mediator$mediator == 1,
               outcome_given_mediator$conditional_probability * p_mediator,
               outcome_given_mediator$conditional_probability * (1-p_mediator)
        )
      
      # For the restricted probabilities
      outcome_given_mediator$joint_probability_restricted = 
        ifelse(outcome_given_mediator$mediator == 1,
               outcome_given_mediator$conditional_probability_restricted * p_mediator,
               outcome_given_mediator$conditional_probability_restricted * (1-p_mediator)
        )
    
      
      # Add counts based on total sample size (both samples summed)
      # Rounding while preserving the correct totals
      outcome_given_mediator$count = 
        trunc(outcome_given_mediator$joint_probability * sum(n1, n2)) + 
        sampling::UPrandomsystematic(outcome_given_mediator$joint_probability * sum(n1, n2) - 
                                       trunc(outcome_given_mediator$joint_probability * sum(n1, n2))
        )
      
      # For the restricted probabilities
      outcome_given_mediator$count_restricted= 
        outcome_given_mediator$joint_probability_restricted * sum(n1, n2)
      
      # Rounding while preserving the correct totals
      outcome_given_mediator$count_restricted = 
        trunc(outcome_given_mediator$joint_probability_restricted * sum(n1, n2)) + 
        sampling::UPrandomsystematic(outcome_given_mediator$joint_probability_restricted * sum(n1, n2) - 
                                       trunc(outcome_given_mediator$joint_probability_restricted * sum(n1, n2))
        )
      
    }
    
    #----------------------------------- OUTPUT -----------------------------------#
    
    if(scenario == 1) output = outcome_given_instrumental 
      
    else if(scenario == 2) output = mediator_given_instrumental
      
    else output = outcome_given_mediator
    
    return(output)
    
  }


