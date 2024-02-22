#################################################################################
##                                                                             ##
##  Function for generating a population with Instrumental Variable violation  ##
##                                                                             ##
#################################################################################

GeneratePopulation_NotInstrumentalVariable <-
  function(mediator_1,
           mediator_2,
           oddsratio_mediator_1 = 1,
           oddsratio_mediator_2 = 1
  ){
    
    # Load function to calculate the frequency table for a given odds ratio
    source("Reverse_OddsRatio.R")
    
    # Required packages
    require(dplyr)
    
    # First some checks on the input arguments
    if(!is.data.frame(mediator_1) | !is.data.frame(mediator_2)) {
      stop("mediator_1 and mediator_2 must be dataframes")
    }
    
    if(!is.numeric(oddsratio_mediator_1) | !is.numeric(oddsratio_mediator_2)){
      stop("odds ratios should be numeric")
    }
    
    # INPUT:
    # mediator_1 and mediator_2 are data frames with counts for mediator = 1 and 
    # mediator = 2 (data.frame)
    # oddsratio_mediator_1 and oddsratio_mediator_2 are numeric and represent the 
    # desired odds ratios between the outcome and the instrumental for both categories 
    # of mediator. Default is 1 so the outcome will be the same as for the IV situation 
    
    # OUTPUT:
    # a list containing:
    # FullPopulation is a data frame containing the distribution of the variables
    # in the population, in probability and counts
    # DistributionMediator1 and DistributionMediator2 are data frames containing the 
    # distribution of the variables in the population separate for the levels of the 
    # mediator (to use in Generate_NotInstrumentalVariable_Population)
    # AggregatedDistribution is a list containing data frames with the distribution 
    # in each IVA statistical matching situation (to use in estimating the error)
    
    
    #------------------------------- NEW POPULATION -------------------------------#
    
    # Get the contingency table from the data
    mediator_1_table = 
      xtabs(count ~ instrumental + outcome,
            data = mediator_1
      )
    
    # Define the population for the given odds ratio for mediator = 1
    population_mediator_1 =
      Reverse_OddsRatio(rowmarginals = c(sum(mediator_1_table[1,]), 
                                         sum(mediator_1_table[2,])
      ),
      columnmarginals = c(sum(mediator_1_table[,1]), 
                          sum(mediator_1_table[,2])
      ),
      oddsratio = oddsratio_mediator_1
      )
    
    # Get the contingency table from the data
    mediator_2_table = 
      xtabs(count ~ instrumental + outcome,
            data = mediator_2
      )
    
    # Define the population for the given odds ratio for mediator = 2
    population_mediator_2 =
      Reverse_OddsRatio(rowmarginals = c(sum(mediator_2_table[1,]), 
                                         sum(mediator_2_table[2,])
      ),
      columnmarginals = c(sum(mediator_2_table[,1]), 
                          sum(mediator_2_table[,2])
      ),
      oddsratio = oddsratio_mediator_2
      )
    
    
    # Define the new separate data frames for both levels of mediator
    new_mediator_1 = 
      mediator_1
    # Replacing the old count with the new count
    new_mediator_1$count = 
      as.numeric(population_mediator_1)
    
    new_mediator_2 = 
      mediator_2
    new_mediator_2$count = 
      as.numeric(population_mediator_2)
    
    # Define the new full population
    new_population_med_ins_out = 
      rbind(new_mediator_1,
            new_mediator_2
      )
    
    # Add proportions to each situation in the population
    new_population_med_ins_out$proportion = 
      new_population_med_ins_out$count/sum(new_population_med_ins_out$count)
    
    #-------------------- AGGREGATE DISTRIBUTION BY SITUATION ---------------------#
    
    distribution_aggregated = 
      list("CommonMediator" = 
             aggregate(proportion ~ instrumental + outcome,
                       FUN = sum,
                       data = new_population_med_ins_out),
           "CommonInstrumental" = 
             aggregate(proportion ~ mediator + outcome,
                       FUN = sum,
                       data = new_population_med_ins_out),
           "CommonOutcome" = 
             aggregate(proportion ~ instrumental + mediator,
                       FUN = sum,
                       data= new_population_med_ins_out)
      )
    
    
    #---------------------------------- OUTPUT ------------------------------------#
    
    # Output list
    output = list(new_population_med_ins_out,
                  new_mediator_1,
                  new_mediator_2,
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

