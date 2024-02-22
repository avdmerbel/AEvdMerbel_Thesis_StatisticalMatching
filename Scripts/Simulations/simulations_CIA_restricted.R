setwd("F:/Documents/Thesis_StatisticalMatching/R/Populations")
populations = 
  readRDS("main_populations.RData")

setwd("F:/Documents/Thesis_StatisticalMatching/R/Functions_StatisticalMatching")
file.sources = list.files(pattern="*.R$")
sapply(file.sources,source,.GlobalEnv)

set.seed(2310)

simulate_population1_CIA_res = 
  StatisticalMatching_FullSimulation(M = 500, 
                                     population = populations[[1]], 
                                     assumption = "CIA", 
                                     restriction = TRUE, 
                                     bootstraps = TRUE, 
                                     nr.bootstraps = 10, 
                                     n1 = 2000, 
                                     n2 = 2000)

saveRDS(simulate_population1_CIA_res, file = "simulate_population1_CIA_res.RData")

simulate_population2_CIA_res = 
  StatisticalMatching_FullSimulation(M = 500, 
                                     population = populations[[2]], 
                                     assumption = "CIA", 
                                     restriction = TRUE, 
                                     bootstraps = TRUE, 
                                     nr.bootstraps = 10, 
                                     n1 = 2000, 
                                     n2 = 2000)

saveRDS(simulate_population2_CIA_res, file = "simulate_population2_CIA_res.RData")

simulate_population3_CIA_res= 
  StatisticalMatching_FullSimulation(M = 500, 
                                     population = populations[[3]], 
                                     assumption = "CIA", 
                                     restriction = TRUE, 
                                     bootstraps = TRUE, 
                                     nr.bootstraps = 10, 
                                     n1 = 2000, 
                                     n2 = 2000)

saveRDS(simulate_population3_CIA_res, file = "simulate_population3_CIA_res.RData")

simulate_population4_CIA_res = 
  StatisticalMatching_FullSimulation(M = 500, 
                                     population = populations[[4]], 
                                     assumption = "CIA", 
                                     restriction = TRUE, 
                                     bootstraps = TRUE, 
                                     nr.bootstraps = 10, 
                                     n1 = 2000, 
                                     n2 = 2000)

saveRDS(simulate_population4_CIA_res, file = "simulate_population4_CIA_res.RData")

simulate_population5_CIA_res = 
  StatisticalMatching_FullSimulation(M = 500, 
                                     population = populations[[5]], 
                                     assumption = "CIA", 
                                     restriction = TRUE, 
                                     bootstraps = TRUE, 
                                     nr.bootstraps = 10, 
                                     n1 = 2000, 
                                     n2 = 2000)

saveRDS(simulate_population5_CIA_res, file = "simulate_population5_CIA_res.RData")
