################################################################################
##                                                                            ##
## Function to calculate the frequency table belonging to  a given Odds Ratio ##
##                                                                            ##
################################################################################

Reverse_OddsRatio <- 
  function(rowmarginals, 
           columnmarginals, 
           oddsratio
  ){
    
    # Required packages
    require(stats)
    
    # INPUT: 
    # numeric vector of row marginals (ordered by category), 
    # numeric vector of column marginals (ordered by category)
    # numeric vector of desired odds ratio 
    
    # OUTPUT:
    # a frequency table containing the values that would generate the desired 
    # odds ratio of type matrix
    
    # First some checks on the input arguments
    if(!is.numeric(rowmarginals) | !is.numeric(columnmarginals)) {
      stop("row and column marginals should be numeric")
    }
    
    if(length(rowmarginals) != 2 | length(columnmarginals) != 2){
      stop("vector for row and column marginals should be of length 2")
    }
    
    if(!is.numeric(oddsratio)){
      stop("odds ratio should be numeric")
    }
    
    # Odds Ratio is calculated as ad/bc where:
    #
    #        var y
    # var x |  0   |  1   |
    # ------|------|------|---------
    #     0 |  a   |  b   | rowtotal
    #     1 |  c   |  d   | rowtotal
    # ------|------|------|---------
    #       |column|column| total
    #       |total |total |
    
    # When the row and column totals are known, it is possible to rewrite the odds
    # ratio equation in terms of one value and find the value that corresponds to the
    # given odds ratio. Given this value we can derive the other values in the matrix
    
    # The arguments rowmarginals and columnmarginals should be ordered by category
    # so first the marginal for category 0, then category 1.
    
    # We first solve for one value in the table, in our case let's use a
    # Think about what the other value would be in terms of a, so use the method
    # of substitution
    
    # b = rowtotal_0 - a
    # c = columntotal_0 - a
    # d = rowtotal_1 - (columntotal_0 - a)
    
    # So we can rewrite the formula for the Odds Ratio as:
    #
    #       a * (rowtotal_1 - (columntotal_0 - a))
    # OR = ---------------------------------------
    #       (rowtotal_0 - a) * (columntotal_0 - a)
    #
    
    
    # Define the equation to get value a from given oddsratio
    # Subtract the desired OR from the function so we can use uniroot
    # to find the solution
    a_oddsratio <- 
      function(a, oddsratio) {
        
        ( ( a * (rowmarginals[2] - (columnmarginals[1] - a)) ) /
            ( (rowmarginals[1] - a) * (columnmarginals[1] - a) ) ) - oddsratio
        
      }
    
    # Solving for a and rounding to nearest integer (because we deal with counts).
    # Upper limit is the smallest value possible for a, which is the smallest 
    # marginal for category 0
    a = 
      round( stats::uniroot(a_oddsratio,
                            oddsratio = oddsratio,
                            lower = 0, 
                            upper = min(rowmarginals[1], 
                                        columnmarginals[1]
                            ))$root
      )
    
    # Derive all values using the a we found
    b = 
      round(rowmarginals[1] - a)
    c = 
      round(columnmarginals[1] - a)
    d = 
      round(columnmarginals[2] - b)
    
    # Put all these values together in a matrix to output the frequency table
    output = 
      matrix(c(a, b, c, d),
             ncol = 2,
             byrow = TRUE
      )
    
    return(output)
  }
