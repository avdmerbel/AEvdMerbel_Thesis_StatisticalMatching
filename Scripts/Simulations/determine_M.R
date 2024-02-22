
setwd("F:/Documents/Thesis_StatisticalMatching/R/Populations")
populations = 
  readRDS("main_populations.RData")

setwd("F:/Documents/Thesis_StatisticalMatching/R/Functions_StatisticalMatching")
file.sources = list.files(pattern="*.R$")
sapply(file.sources,source,.GlobalEnv)

set.seed(2310)

# define population with extreme distribution and slight violation
populationM_prep = 
  GeneratePopulation_InstrumentalVariable(N = 1000000,
                                          oddsratio_med_out = 4,
                                          oddsratio_ins_med = 7,
                                          p_instrumental = .9,
                                          p_mediator = .85,
                                          p_outcome = .652)

populationM = 
  GeneratePopulation_NotInstrumentalVariable(mediator_1 = populationM_prep$DistributionMediator1,
                                             mediator_2 = populationM_prep$DistributionMediator2,
                                             oddsratio_mediator_1 = 2,
                                             oddsratio_mediator_2 = 2)

# do simulations 10, 50, 100, 250, 500, 750, 1000, 5000, 10000, 100000 times to assess 

res10 = 
  StatisticalMatching_FullSimulation(M = 10, 
                                     population = populationM, 
                                     assumption = "IVA", 
                                     restriction = FALSE, 
                                     bootstraps = TRUE, 
                                     nr.bootstraps = 10, 
                                     n1 = 2000, 
                                     n2 = 2000)

saveRDS(res10, file = "F:/Documents/Thesis_statisticalMatching/R/DetermineM/res10.RData")

res50 = 
  StatisticalMatching_FullSimulation(M = 50, 
                                     population = populationM, 
                                     assumption = "IVA", 
                                     restriction = FALSE, 
                                     bootstraps = FALSE, 
                                     nr.bootstraps = 10, 
                                     n1 = 2000, 
                                     n2 = 2000)

saveRDS(res50, file = "F:/Documents/Thesis_statisticalMatching/R/DetermineM/res50.RData")

res100 = 
  StatisticalMatching_FullSimulation(M = 100, 
                                     population = populationM, 
                                     assumption = "IVA", 
                                     restriction = FALSE, 
                                     bootstraps = FALSE, 
                                     nr.bootstraps = 10, 
                                     n1 = 2000, 
                                     n2 = 2000)

saveRDS(res100, file = "F:/Documents/Thesis_statisticalMatching/R/DetermineM/res100.RData")

res250 = 
  StatisticalMatching_FullSimulation(M = 250, 
                                     population = populationM, 
                                     assumption = "IVA", 
                                     restriction = FALSE, 
                                     bootstraps = FALSE, 
                                     nr.bootstraps = 10, 
                                     n1 = 2000, 
                                     n2 = 2000)

saveRDS(res250, file = "F:/Documents/Thesis_statisticalMatching/R/DetermineM/res250.RData")

res500 = 
  StatisticalMatching_FullSimulation(M = 500, 
                                     population = populationM, 
                                     assumption = "IVA", 
                                     restriction = FALSE, 
                                     bootstraps = FALSE, 
                                     nr.bootstraps = 10, 
                                     n1 = 2000, 
                                     n2 = 2000)

saveRDS(res500, file = "F:/Documents/Thesis_statisticalMatching/R/DetermineM/res500.RData")

res750 = 
  StatisticalMatching_FullSimulation(M = 750, 
                                     population = populationM, 
                                     assumption = "IVA", 
                                     restriction = FALSE, 
                                     bootstraps = FALSE, 
                                     nr.bootstraps = 5, 
                                     n1 = 2000, 
                                     n2 = 2000)


saveRDS(res750, file = "F:/Documents/Thesis_statisticalMatching/R/DetermineM/res750.RData")

res1000 = 
  StatisticalMatching_FullSimulation(M = 1000, 
                                     population = populationM, 
                                     assumption = "IVA", 
                                     restriction = FALSE, 
                                     bootstraps = FALSE, 
                                     nr.bootstraps = 5, 
                                     n1 = 2000, 
                                     n2 = 2000)


saveRDS(res1000, file = "F:/Documents/Thesis_statisticalMatching/R/DetermineM/res1000.RData")

res5000 = 
  StatisticalMatching_FullSimulation(M = 5000, 
                                     population = populationM, 
                                     assumption = "IVA", 
                                     restriction = FALSE, 
                                     bootstraps = FALSE, 
                                     nr.bootstraps = 5, 
                                     n1 = 2000, 
                                     n2 = 2000)


saveRDS(res5000, file = "F:/Documents/Thesis_statisticalMatching/R/DetermineM/res5000.RData")

res10000 = 
  StatisticalMatching_FullSimulation(M = 10000, 
                                     population = populationM, 
                                     assumption = "IVA", 
                                     restriction = FALSE, 
                                     bootstraps = FALSE, 
                                     nr.bootstraps = 5, 
                                     n1 = 2000, 
                                     n2 = 2000)


saveRDS(res10000, file = "F:/Documents/Thesis_statisticalMatching/R/DetermineM/res10000.RData")

# res100000 = 
#   StatisticalMatching_FullSimulation(M = 100000, 
#                                      population = populationM, 
#                                      assumption = "IVA", 
#                                      restriction = FALSE, 
#                                      bootstraps = FALSE, 
#                                      nr.bootstraps = 5, 
#                                      n1 = 2000, 
#                                      n2 = 2000)
# 
# 
# 
# 
# saveRDS(res100000, file = "F:/Documents/Thesis_statisticalMatching/R/DetermineM/res100000.RData")


