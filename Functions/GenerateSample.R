################################################################################
##                                                                            ##
##  Function for generating a sample in the instrumental variable situation   ##
##                                                                            ##
################################################################################

GenerateSample <- 
  function(population,
           common_variable,
           n1 = 2000,
           n2 = 2000
  ){
    
    # Load required packages
    require(dplyr)
    require(extraDistr)
    require(data.table)
    
    # First some checks on the input arguments
    if(!is.data.frame(population)) {
      stop("population must be a dataframe")
    }
    
    if(!is.numeric(n1) | !is.numeric(n2)){
      stop("n1 and n2 must be numeric")
    }
    
    # INPUT:
    # population is the Full Population table generated 
    # with Generate_InstrumentalVariable_Population.
    # common_variable specifies which variable is jointly observed
    # choose from instrumental, mediator and outcome.
    # n1 is the desired sample size for the first sample.
    # n2 is the desired sample size for the second sample
    
    # OUTPUT:
    # A data frame for each sample containing the counts and 
    # proportions for each category combination (similar to population data frame).
    # 2 numeric outputs with the sample sizes
    # 2 xtabs objects with the contingency tables for the category
    # combinations per sample
    
    #--------------------------------- BUILD SAMPLE ---------------------------------#
    
    # Define which variables are not jointly observed
    not_common_variable = 
      colnames(population)[!(colnames(population) %in%
                               c(common_variable,
                                 "count",
                                 "proportion"
                               )
      )]
    
    # Use rmvhyper function from extraDistr package
    # Use the hypergeometric distribution to sample without replacement k samples out of N population with n distinct situations 
    # This function automatically samples from the specified situations in the population
    # nn = 2: take two samples
    # n = population$count: sample using the distribution of situations in the population
    # k = n: sample size specified in the function arguments
    # use sapply to get samples with different sample sizes, returns a matrix
    sample_population = 
      sapply(c(n1,n2), 
             function(x) extraDistr::rmvhyper(nn = 1, 
                                              n = population$count, 
                                              k = x)
      )
    
    
    #-------------------------------- FIRST SAMPLE --------------------------------#
    
    
    # Initialize the first sample data frame
    first_sample = 
      population[,c(common_variable,
                    not_common_variable[1]
      )]
    
    # Fill category combinations from the sampled numbers
    first_sample$count = 
      sample_population[,1]
    
    # Save the unobserved variable as well for reference
    first_sample_unobserved = first_sample
    first_sample_unobserved[, paste0("unobserved_", not_common_variable[2])] = 
      population[, not_common_variable[2]]
    
    first_sample_unobserved$proportion =
      first_sample_unobserved$count / sum(first_sample_unobserved$count)
    
    # Get unobserved variables distribution from the first sample
    unobserved_firstsample = 
      aggregate(proportion ~ as.matrix(first_sample_unobserved[,2]) + as.matrix(first_sample_unobserved[,4]),
                data = first_sample_unobserved,
                FUN = sum)
    
    colnames(unobserved_firstsample)[1:2] = 
      c(not_common_variable[1], not_common_variable[2])
    
    # Aggregate the sample to remove duplicate situations
    first_sample = aggregate(count ~ .,
                             data = first_sample,
                             FUN = sum
    )
    
    # Add the proportions per situation
    first_sample$proportion = 
      first_sample$count/n1
    
    # Add distribution of the two variables
    first_sample_distribution = 
      xtabs(proportion ~ .,
            data = first_sample[, -3] # dropping the third count column
      )
    
    #-------------------------------- SECOND SAMPLE -------------------------------#
    
    
    # Initialize the second sample data frame
    second_sample = 
      population[,c(common_variable,
                    not_common_variable[2]
      )]
    
    # Fill category combinations from the sampled numbers 
    second_sample$count = 
      sample_population[,2]
    
    # Save the unobserved variable as well for reference
    second_sample_unobserved = second_sample
    second_sample_unobserved[, paste0("unobserved_", not_common_variable[1])] = 
      population[, not_common_variable[1]]
    
    second_sample_unobserved$proportion =
      second_sample_unobserved$count / sum(second_sample_unobserved$count)
    
    # get unobserved variable distribution for the second sample
    unobserved_secondsample = 
      aggregate(proportion ~ as.matrix(second_sample_unobserved[,4]) + as.matrix(second_sample_unobserved[,2]),
                data = second_sample_unobserved,
                FUN = sum)
    
    colnames(unobserved_secondsample)[1:2] = 
      c(not_common_variable[1], not_common_variable[2])
    
    # Aggregate the sample to remove duplicate situations
    second_sample = aggregate(count ~ .,
                              data = second_sample,
                              FUN = sum
    )
    
    # Add the proportions per situation
    second_sample$proportion = 
      second_sample$count/n2
    
    # Distribution of the two variables
    second_sample_distribution = 
      xtabs(proportion ~ .,
            data = second_sample[, -3] # dropping the third count column
      )
    
    #--------------------------------- UNOBSERVED ---------------------------------#
    
    # Get the unobserved (sneaky) distribution
    
    # suppress the message from summarise 
    options(dplyr.summarise.inform = FALSE)
    
    unobserved_distribution = 
        bind_rows(unobserved_firstsample, unobserved_secondsample) %>%
        group_by(across(c(1,2))) %>%
        aggregate(proportion ~., FUN = mean) 
    
    #----------------------------------- OUTPUT -----------------------------------#
    
    
    output = 
      list(first_sample,
           n1,
           first_sample_distribution,
           second_sample,
           n2,
           second_sample_distribution,
           first_sample_unobserved,
           second_sample_unobserved,
           common_variable,
           unobserved_distribution
      )
    
    names(output) = 
      c("FirstSample",
        "N1",
        "DistributionFirstSample",
        "SecondSample",
        "N2",
        "DistributionSecondSample",
        "FirstSampleUnobserved",
        "SecondSampleUnobserved",
        "CommonVariable",
        "UnobservedDistribution"
      )
    
    return(output)
  }



