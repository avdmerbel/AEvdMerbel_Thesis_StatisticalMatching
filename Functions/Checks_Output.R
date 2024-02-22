# Some functions that give some additional output for functions in the statistical matching project


#----------------------------------- POPULATION -----------------------------------#

check_population <-
  function(population){
    
    # INPUT:
    # population: the output of generate_population() 
    
    # Check the odds ratios actualized
    
    # Odds ratios for the mediator and instrumental, and the outcome and mediator
    # These should approximately match the odds ratios specified in the function arguments
    population_OR_med_ins = 
      fisher.test(xtabs(count ~.,
                        data = population$FullPopulation[, c("mediator", 
                                                             "instrumental", 
                                                             "count")]
      ))$estimate
    
    population_OR_out_med = 
      fisher.test(xtabs(count ~.,
                        data = population$FullPopulation[, c("outcome", 
                                                             "mediator", 
                                                             "count")]
      ))$estimate
    
    population_OR_out_ins = 
      fisher.test(xtabs(count ~.,
                        data = population$FullPopulation[, c("instrumental", 
                                                             "outcome", 
                                                             "count")]
      ))$estimate  
    
    # Odds ratio for the outcome and the instrumental variable given that the mediator = 1
    # In the instrumental variable situation these should be ~1
    population_OR_out_ins_1 = 
      population$DistributionMediator1$count %>% 
      matrix(ncol = 2,
             byrow = T) %>% 
      fisher.test 
    
    population_OR_out_ins_1 = 
      population_OR_out_ins_1$estimate
    
    # Odds ratio for the outcome and the instrumental variable given that the mediator = 2
    # In the instrumental variable situation these should be ~1
    population_OR_out_ins_2 = 
      population$DistributionMediator2$count %>% 
      matrix(ncol = 2,
             byrow = T) %>% 
      fisher.test
    
    population_OR_out_ins_2 =
      population_OR_out_ins_2$estimate
    
    # Define the ratio of odds ratios between the two levels of mediator
    population_OR_ratio = 
      population_OR_out_ins_1 / population_OR_out_ins_2
    
    # Check conditional probabilities actualized
    
    # Define the conditional probability for mediator given instrumental
    probability_med_conditional_ins = 
      aggregate(population$FullPopulation$count,
                by = list(population$FullPopulation$instrumental,
                          population$FullPopulation$mediator),
                FUN = sum)$x %>% 
      matrix(nrow = 2) %>% 
      prop.table(margin = 1)
    
    # Define the conditional probability for outcome given mediator
    probability_out_conditional_med = 
      aggregate(population$FullPopulation$count,
                by = list(population$FullPopulation$mediator,
                          population$FullPopulation$outcome),
                FUN = sum)$x %>% 
      matrix(nrow = 2) %>% 
      prop.table(margin = 1)
    
    # Define the conditional probability for outcome given instrumental
    probability_out_conditional_ins = 
      aggregate(population$FullPopulation$count,
                by = list(population$FullPopulation$instrumental,
                          population$FullPopulation$outcome),
                FUN = sum)$x %>% 
      matrix(nrow = 2) %>% 
      prop.table(margin = 1) 
    
    # Conditional probability of outcome given instrumental for mediator = 1 and mediator = 2
    probability_out_conditional_ins_med1 = 
      xtabs(count ~., 
            data = population$DistributionMediator1[, c("instrumental", 
                                                        "outcome", 
                                                        "count")]
      ) %>%
      prop.table(margin = 1)
    
    probability_out_conditional_ins_med2 = 
      xtabs(count ~., 
            data = population$DistributionMediator2[, c("instrumental", 
                                                        "outcome", 
                                                        "count")]
      ) %>%
      prop.table(margin = 1)
    
    # Check the distribution of the variables
    
    distribution_instrumental =
      sum(population$FullPopulation[population$FullPopulation$instrumental == 1, ]$count) / sum(population$FullPopulation$count)
    
    distribution_mediator = 
      sum(population$FullPopulation[population$FullPopulation$mediator == 1, ]$count) / sum(population$FullPopulation$count)
    
    distribution_outcome = 
      sum(population$FullPopulation[population$FullPopulation$outcome == 1, ]$count) / sum(population$FullPopulation$count)
    
    # output
    
    output = 
      list(population_OR_med_ins,
           population_OR_out_med,
           population_OR_out_ins,
           population_OR_out_ins_1,
           population_OR_out_ins_2,
           population_OR_ratio,
           probability_med_conditional_ins,
           probability_out_conditional_med,
           probability_out_conditional_ins,
           probability_out_conditional_ins_med1,
           probability_out_conditional_ins_med2,
           distribution_instrumental,
           distribution_mediator,
           distribution_outcome)
    
    return(output)
  }

#------------------------------------ SAMPLING ------------------------------------#

check_sampling <-
  function(sample_input){
    
    # INPUT:
    # sample_input: output from GenerateSample()
    
    first_sample = sample_input$FirstSample
    second_sample = sample_input$SecondSample
    common_variable = sample_input$CommonVariable
    
    # Calculate the distributions of the variables in both samples
    # for the common variable in sample 1
    p_common_sample1 = 
      sum(subset(first_sample, 
                 first_sample[, common_variable] == 1)$count) / 
      sum(first_sample$count)
    
    # for the other variable in sample 1
    p_other_sample1 = 
      sum(subset(first_sample,
                 first_sample[, not_common_variable[1]] == 1)$count) / 
      sum(first_sample$count)
    
    # for the common variable in sample 2
    p_common_sample2 = 
      sum(subset(second_sample, 
                 second_sample[, common_variable] == 1)$count) / 
      sum(second_sample$count)
    
    # for the other variable in sample 2
    p_other_sample2 = 
      sum(subset(second_sample,
                 second_sample[, not_common_variable[2]] == 1)$count) / 
      sum(second_sample$count)
    
    # for the common variable in both samples
    p_common = 
      mean(c(p_common_sample1, p_common_sample2))
    
    # Calculate odds ratios for both samples
    oddsratio_1 = 
      fisher.test(matrix(first_sample$count,
                         nrow = 2))$estimate
    
    oddsratio_2 = 
      fisher.test(matrix(second_sample$count,
                         nrow = 2))$estimate
    
    # output
    
    output = 
      list(p_common_sample1,
           p_other_sample1,
           p_common_sample2,
           p_other_sample2,
           oddsratio_1,
           oddsratio_2,
           p_common
      )
    
    names(output) = 
      c(paste0("Distribution", common_variable, "Sample1"),
        paste0("Distribution", not_common_variable[1], "Sample1"),
        paste0("Distribution", common_variable, "Sample2"),
        paste0("Distribution", not_common_variable[2], "Sample2"),
        "OddsRatioFirstSample",
        "OddsRatioSecondSample",
        paste0("Distribution", common_variable)
      )
    
    return(output)
    
  }