# This script defines the populations used to sample from in the simulation study

#setwd("./Functions_StatisticalMatching")

# load the necessary functions
source("GeneratePopulation_InstrumentalVariable.R")
source("GeneratePopulation_NotInstrumentalVariable.R")


# This population needs no violations so it can be defined directly
population1_IVAholds = 
  GeneratePopulation_InstrumentalVariable(N = 1000000,
                                          oddsratio_med_out = 4,
                                          oddsratio_ins_med = 7,
                                          p_instrumental = .429,
                                          p_mediator = .615,
                                          p_outcome = .652)

# This population does have a violation so first define without
population2_prep = 
  GeneratePopulation_InstrumentalVariable(N = 1000000,
                                          oddsratio_med_out = 4,
                                          oddsratio_ins_med = 7,
                                          p_instrumental = .429,
                                          p_mediator = .615,
                                          p_outcome = .652)

# add violation
population2_IVAviolated_type1_1 = 
  GeneratePopulation_NotInstrumentalVariable(mediator_1 = population2_prep$DistributionMediator1,
                                             mediator_2 = population2_prep$DistributionMediator2,
                                             oddsratio_mediator_1 = 2,
                                             oddsratio_mediator_2 = 2)

# same story
population3_prep = 
  GeneratePopulation_InstrumentalVariable(N = 1000000,
                                          oddsratio_med_out = 4,
                                          oddsratio_ins_med = 7,
                                          p_instrumental = .429,
                                          p_mediator = .615,
                                          p_outcome = .652)

population3_IVAviolated_type1_2 = 
  GeneratePopulation_NotInstrumentalVariable(mediator_1 = population3_prep$DistributionMediator1,
                                             mediator_2 = population3_prep$DistributionMediator2,
                                             oddsratio_mediator_1 = 4,
                                             oddsratio_mediator_2 = 4)

# This population only has a low association between the instrumental and the 
# mediator so can be directly defined with the first function
population4_IVAviolated_type2 = 
  GeneratePopulation_InstrumentalVariable(N = 1000000,
                                          oddsratio_med_out = 4,
                                          oddsratio_ins_med = 2,
                                          p_instrumental = .429,
                                          p_mediator = .615,
                                          p_outcome = .652)

# this population has both violations so in two steps again
population5_prep = 
  GeneratePopulation_InstrumentalVariable(N = 1000000,
                                          oddsratio_med_out = 4,
                                          oddsratio_ins_med = 2,
                                          p_instrumental = .429,
                                          p_mediator = .615,
                                          p_outcome = .652)

population5_IVAviolated_both = 
  GeneratePopulation_NotInstrumentalVariable(mediator_1 = population5_prep$DistributionMediator1,
                                             mediator_2 = population5_prep$DistributionMediator2,
                                             oddsratio_mediator_1 = 2,
                                             oddsratio_mediator_2 = 2)

# save the populations to a list object
main_populations = 
  list(population1 = population1_IVAholds,
       population2 = population2_IVAviolated_type1_1,
       population3 = population3_IVAviolated_type1_2,
       population4 = population4_IVAviolated_type2,
       population5 = population5_IVAviolated_both)

# populations are saved using saveRDS, open them using readRDS
#saveRDS(main_populations, file = "main_populations.RData")
