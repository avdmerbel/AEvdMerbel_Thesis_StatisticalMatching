# In this script the odds ratio of Z,Y | X is calculated to find out to what
# degree the CIA is violated in the different populations


library(dplyr)

pops = 
  readRDS("./Data/Populations/main_populations.RData")

# Extract the full population dataframe from each population
pops = lapply(pops, with, FullPopulation)

# A function that outputs the oddsratio under the CIA for each of the three
# matching scenarios
oddsratio_CIA <- 
  function(pop){
    
    common_mediator =   
      xtabs(count ~ outcome + instrumental + mediator,
            data = pop)
    
    med1 = 
      fisher.test(common_mediator[,,1])$estimate
    med2 =
      fisher.test(common_mediator[,,2])$estimate
    
    common_outcome = 
      xtabs(count ~ mediator + instrumental + outcome,
            data = pop)
    
    out1 = 
      fisher.test(common_outcome[,,1])$estimate
    out2 =
      fisher.test(common_outcome[,,2])$estimate
    
    common_instrumental = 
      xtabs(count ~ mediator + outcome + instrumental,
            data = pop)
    
    ins1 = 
      fisher.test(common_instrumental[,,1])$estimate
    ins2 =
      fisher.test(common_instrumental[,,2])$estimate
    
    
    results = 
      data.frame(common_variable = c("Mediator", " Outcome", " Instrumental"),
                 OR_ZY_X1 = c(med1, out1, ins1),
                 OR_ZY_X2 = c(med2, out2, ins2))
    
    return(results)
    
  }

# apply the function to all populations
all = 
  lapply(pops, FUN = oddsratio_CIA)
all.frame = 
  bind_rows(all)

all.frame$population = c(rep(1,3),
                         rep(2,3),
                         rep(3,3),
                         rep(4,3),
                         rep(5,3))

View(all.frame)
