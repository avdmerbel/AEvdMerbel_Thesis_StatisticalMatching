######################################################################################
#                                                                                    #
# Function to check the order of the aggregated population vs simulations/bootstraps #
#                                                                                    #
######################################################################################

# chech whether the order of aggregated population is the same as 
# how the simulations were calculated. Important because the distributions
# have to be added to the right variable combinations.
#
#
# INPUT:
# population_distribution: dataframe which contains the variable names, 
# the category combinations and joint probability
# (from aggregatedDistribution of generate_population())
# variable_combinations: dataframe which contains the variable names and 
# category combinations extracted from either the bootstrap or the simulation
# 
# OUTPUT:
# new_aggregate: population_distribution reordered according to the order of
# variable_combinations

Check_Order <-
  function(population_distribution,
           variable_combinations) {
    
    # Change order of situations in the aggregate to match the order of
    # situations in the variable combinations
    new_aggregate = 
      population_distribution[names(variable_combinations)]
    
    
    # define a function that changes the order of the columns if there are
    # different
    change_columns <- 
      function(new_aggregate, 
               variable_combinations){
        
        if(sum(colnames(new_aggregate[1:2]) == colnames(variable_combinations)) == 2){
          
          result = new_aggregate
        } else{
      
        indices = match(colnames(variable_combinations),
                        colnames(new_aggregate))
        
        result = new_aggregate[,c(indices,3)]
        }
        
        return(result)
      }
    
    # apply column changing function to the aggregate
    new_aggregate = 
      lapply(names(new_aggregate),
             function(x) change_columns(new_aggregate[[x]],
                                        variable_combinations[[x]]))
    
    names(new_aggregate) = names(variable_combinations)
    
    # define a function that changes the order of the rows if they do not match
    change_rows <- 
      function(new_aggregate,
               variable_combinations){
        
        if(sum(new_aggregate[,1] == variable_combinations[,1]) == 4){
          
          result = new_aggregate
          
        } else {
          
          result = 
            new_aggregate[order(match(variable_combinations[,1],
                                      new_aggregate[,1])),]
          
        }
        
        return(result)
      }
    
    # apply change row function to the aggregate
    new_aggregate = 
      lapply(names(new_aggregate),
             function(x) change_rows(new_aggregate[[x]],
                                     variable_combinations[[x]]))
    
    names(new_aggregate) = names(variable_combinations)
    
    return(new_aggregate)
  }