########################################################################################
##                                                                                    ##
##     Function for generating a population in the instrumental variable situation    ##
##                                                                                    ##
########################################################################################

GeneratePopulation_InstrumentalVariable <- 
  function(N = 1000000, 
           p_instrumental = runif(1), 
           p_mediator = runif(1),
           p_outcome = runif(1),
           oddsratio_ins_med = 7,
           oddsratio_med_out = 4
  ){
    
    # Load function to calculate the frequency table for a given odds ratio
    source("Reverse_OddsRatio.R")
    
    # Required packages
    require(dplyr)
    require(sampling)
    
    # First some checks on the input arguments
    if(!is.numeric(N)) {
      stop("N must be numeric")
    }
    
    if(!is.numeric(p_outcome) | !is.numeric(p_mediator) | !is.numeric(p_instrumental)){
      stop("probabilities of all three variables must be numeric")
    }
    
    if(!is.numeric(oddsratio_ins_med) | !is.numeric(oddsratio_med_out)){
      stop("odds ratios should be numeric")
    }
    
    # INPUT:
    # N is numeric and represents the population size
    # p_instrumental is numeric and represents the distribution of the instrumental 
    # variable in the population 
    # p_mediator is numeric and represents the distribution of the mediating variable 
    # in the population
    # p_outcome is numeric and represents the distribution of the outcome variable in 
    # the population
    # oddsratio_ins_med is numeric and represents the desired odds ratio between the 
    # instrumental and the mediating variable in the population 
    # oddsratio_med_out is numeric and represents the desired odds ratio between the 
    # mediating and outcome variable in the population
    
    # OUTPUT:
    # a list containing:
    # FullPopulation is a data frame containing the distribution of the variables
    # in the population, in probability and counts
    # DistributionMediator1 and DistributionMediator2 are data frames containing the 
    # distribution of the variables in the population separate for the levels of the 
    # mediator (to use in Generate_NotInstrumentalVariable_Population)
    # AggregatedDistribution is a list containing data frames with the distribution 
    # in each IVA statistical matching situation (to use in estimating the error)
    
    
    
    #--------------------- DISTRIBUTION MEDIATOR INSTRUMENTAL ---------------------#
    
    # Define distribution in population if the odds ratio is equal to the specified 
    # OR (7 by default)
    population_med_ins =
      Reverse_OddsRatio(rowmarginals = c((N*p_instrumental),
                                         (N*(1-p_instrumental))
      ),
      columnmarginals = c((N*p_mediator),
                          (N*(1-p_mediator))
      ),
      oddsratio = oddsratio_ins_med
      )
    
    # Define the conditional probability for the mediator given de instrumental
    probability_med_conditional_ins = 
      population_med_ins %>% 
      prop.table(margin = 1)
    
    #------------------------ DISTRIBUTION MEDIATOR OUTCOME -----------------------#
    
    # Define distribution in population if the odds ratio is equal to the specified 
    # OR (4 by default)
    population_out_med = 
      Reverse_OddsRatio(rowmarginals = c((N*p_mediator),
                                         (N*(1-p_mediator))
      ),
      columnmarginals = c((N*p_outcome),
                          (N * (1-p_outcome))
      ),
      oddsratio = oddsratio_med_out
      )
    
    # Define the conditional probability for outcome given mediator
    probability_out_conditional_med =  
      population_out_med %>% 
      prop.table(margin = 1)
    
    #----------------- DISTRIBUTION MEDIATOR OUTCOME INSTRUMENTAL -----------------#
    
    
    # A function that calculates P(instrumental = i, mediator = j, outcome = k)
    # when the instrumental variable assumption holds
    # P(instrumental = i, mediator = j, outcome = k) = 
    # P(outcome = k | instrumental = i, mediator = j) * P(instrumental = i , mediator = j)
    # under IV assumption this reduces to:
    # P(instrumental = i, mediator = j, outcome = k) = 
    # P(outcome = k | mediator = j) * P(instrumental = i , mediator = j)
    
    probability_ins_med_out <- 
      function(m){
        
        population_med_ins[m[1], m[2]] * probability_out_conditional_med[m[2], m[3]]
        
      }
    
    # Define all combinations of all variables in the population that are possible 
    # with 2 categories for each variable
    population_med_ins_out = 
      expand.grid(instrumental = c(1,2),
                  mediator = c(1,2),
                  outcome = c(1,2)
      )
    
    # Define counts of different X, Y, Z combinations in the population
    population_med_ins_out$count = 
      apply(population_med_ins_out, 
            MARGIN = 1,
            FUN = probability_ins_med_out
      ) 
    
    # Round in such a way that the totals are correct
    population_med_ins_out$count = 
      trunc(population_med_ins_out$count) + 
      sampling::UPrandomsystematic(population_med_ins_out$count - 
                                     trunc(population_med_ins_out$count)
      )
    
    # Proportion of combinations in the population
    population_med_ins_out$proportion = 
      population_med_ins_out$count/N
    
    
    #----------------------- DISTRIBUTION BY MEDIATOR LEVEL -----------------------#
    
    
    # Define the distribution of the instrumental and the outcome, given the mediator = 1
    mediator_1 = 
      population_med_ins_out[population_med_ins_out$mediator == 1,] 
    
    # Define the distribution of the instrumental and the outcome, given the mediator = 2
    mediator_2 = 
      population_med_ins_out[population_med_ins_out$mediator == 2,]
    
    
    #-------------------- AGGREGATE DISTRIBUTION BY SITUATION ---------------------#
    
    
    distribution_aggregated = 
      list("CommonMediator" = 
             aggregate(proportion ~ instrumental + outcome,
                       FUN = sum,
                       data = population_med_ins_out),
           "CommonInstrumental" = 
             aggregate(proportion ~ mediator + outcome,
                       FUN = sum,
                       data = population_med_ins_out),
           "CommonOutcome" = 
             aggregate(proportion ~ instrumental + mediator,
                       FUN = sum,
                       data= population_med_ins_out)
      )
    
    #----------------------------------- OUTPUT -----------------------------------#
    
    output = 
      list(population_med_ins_out,
           mediator_1,
           mediator_2,
           distribution_aggregated
      )
    
    names(output) = 
      c("FullPopulation",
        "DistributionMediator1",
        "DistributionMediator2",
        "AggregatedDistribution"
      )
    
    return(output)
  }

