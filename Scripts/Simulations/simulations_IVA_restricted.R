setwd("F:/Documents/Thesis_StatisticalMatching/R/Populations")
populations = 
  readRDS("main_populations.RData")

setwd("F:/Documents/Thesis_StatisticalMatching/R/Functions_StatisticalMatching")
file.sources = list.files(pattern="*.R$")
sapply(file.sources,source,.GlobalEnv)

set.seed(2310)

simulate_population1_IVA_res = 
  StatisticalMatching_FullSimulation(M = 500, 
                                     population = populations[[1]], 
                                     assumption = "IVA", 
                                     restriction = TRUE, 
                                     bootstraps = TRUE, 
                                     nr.bootstraps = 10, 
                                     n1 = 2000, 
                                     n2 = 2000)

saveRDS(simulate_population1_IVA_res, file = "simulate_population1_IVA_res.RData")

simulate_population2_IVA_res = 
  StatisticalMatching_FullSimulation(M = 500, 
                                     population = populations[[2]], 
                                     assumption = "IVA", 
                                     restriction = TRUE, 
                                     bootstraps = TRUE, 
                                     nr.bootstraps = 10, 
                                     n1 = 2000, 
                                     n2 = 2000)

saveRDS(simulate_population2_IVA_res, file = "simulate_population2_IVA_res.RData")

simulate_population3_IVA_res= 
  StatisticalMatching_FullSimulation(M = 500, 
                                     population = populations[[3]], 
                                     assumption = "IVA", 
                                     restriction = TRUE, 
                                     bootstraps = TRUE, 
                                     nr.bootstraps = 10, 
                                     n1 = 2000, 
                                     n2 = 2000)

saveRDS(simulate_population3_IVA_res, file = "simulate_population3_IVA_res.RData")

simulate_population4_IVA_res = 
  StatisticalMatching_FullSimulation(M = 500, 
                                     population = populations[[4]], 
                                     assumption = "IVA", 
                                     restriction = TRUE, 
                                     bootstraps = TRUE, 
                                     nr.bootstraps = 10, 
                                     n1 = 2000, 
                                     n2 = 2000)

saveRDS(simulate_population4_IVA_res, file = "simulate_population4_IVA_res.RData")

simulate_population5_IVA_res = 
  StatisticalMatching_FullSimulation(M = 500, 
                                     population = populations[[5]], 
                                     assumption = "IVA", 
                                     restriction = TRUE, 
                                     bootstraps = TRUE, 
                                     nr.bootstraps = 10, 
                                     n1 = 2000, 
                                     n2 = 2000)

saveRDS(simulate_population5_IVA_res, file = "simulate_population5_IVA_res.RData")
