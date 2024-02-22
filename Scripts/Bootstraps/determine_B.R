library(plyr)
library(dplyr)

# load all functions
setwd("F:/Documents/Thesis_StatisticalMatching/R/Functions_StatisticalMatching")
file.sources = list.files(pattern="*.R$")
sapply(file.sources,source,.GlobalEnv)

# how many boots do we neeeeeeed
set.seed(2310)

sims = 
  readRDS("F:/Documents/Thesis_statisticalMatching/R/DetermineM/res10.RData")

samples_populationboots = 
  sims$SamplesBootstrap


start = Sys.time()
test = 
  lapply(seq(1, length(samples_populationboots)),
         FUN = function(x) StatisticalMatching_FullBootstrap(B = 2,
                                                             sample_input = samples_populationboots[[x]],
                                                             assumption = "IVA",
                                                             restriction = FALSE))

bootres = 
  list("CommonInstrumental" = as.data.frame(aaply(laply(lapply(test, 
                                                               with, 
                                                               CommonInstrumental), 
                                                        as.matrix), 
                                                  c(2,3), 
                                                  mean)),
       "CommonMediator" = as.data.frame(aaply(laply(lapply(test, 
                                                           with, 
                                                           CommonMediator), 
                                                    as.matrix), 
                                              c(2,3), 
                                              mean)),
       "CommonOutcome" = as.data.frame(aaply(laply(lapply(test, 
                                                          with, 
                                                          CommonOutcome), 
                                                   as.matrix), 
                                             c(2,3), 
                                             mean)))


end = Sys.time()
end-start
  
  
bootstraps_10 = 
  lapply(seq(1, length(samples_populationboots)),
         FUN = function(x) StatisticalMatching_FullBootstrap(B = 10,
                                                             sample_input = samples_populationboots[[x]],
                                                             assumption = "IVA",
                                                             restriction = FALSE))

bootstraps_10 = 
  list("CommonInstrumental" = as.data.frame(aaply(laply(lapply(bootstraps_10, 
                                                               with, 
                                                               CommonInstrumental), 
                                                        as.matrix), 
                                                  c(2,3), 
                                                  mean)),
       "CommonMediator" = as.data.frame(aaply(laply(lapply(bootstraps_10, 
                                                           with, 
                                                           CommonMediator), 
                                                    as.matrix), 
                                              c(2,3), 
                                              mean)),
       "CommonOutcome" = as.data.frame(aaply(laply(lapply(bootstraps_10, 
                                                          with, 
                                                          CommonOutcome), 
                                                   as.matrix), 
                                             c(2,3), 
                                             mean)))

saveRDS(bootstraps_10, "F:/Documents/Thesis_statisticalMatching/R/DetermineB/bootstraps_10.RData")

bootstraps_50 = 
  lapply(seq(1, length(samples_populationboots)),
         FUN = function(x) StatisticalMatching_FullBootstrap(B = 50,
                                                             sample_input = samples_populationboots[[x]],
                                                             assumption = "IVA",
                                                             restriction = FALSE))

bootstraps_50 = 
  list("CommonInstrumental" = as.data.frame(aaply(laply(lapply(bootstraps_50, 
                                                               with, 
                                                               CommonInstrumental), 
                                                        as.matrix), 
                                                  c(2,3), 
                                                  mean)),
       "CommonMediator" = as.data.frame(aaply(laply(lapply(bootstraps_50, 
                                                           with, 
                                                           CommonMediator), 
                                                    as.matrix), 
                                              c(2,3), 
                                              mean)),
       "CommonOutcome" = as.data.frame(aaply(laply(lapply(bootstraps_50, 
                                                          with, 
                                                          CommonOutcome), 
                                                   as.matrix), 
                                             c(2,3), 
                                             mean)))

saveRDS(bootstraps_50, "F:/Documents/Thesis_statisticalMatching/R/DetermineB/bootstraps_50.RData")

bootstraps_100 = 
  lapply(seq(1, length(samples_populationboots)),
         FUN = function(x) StatisticalMatching_FullBootstrap(B = 100,
                                                             sample_input = samples_populationboots[[x]],
                                                             assumption = "IVA",
                                                             restriction = FALSE))

bootstraps_100 = 
  list("CommonInstrumental" = as.data.frame(aaply(laply(lapply(bootstraps_100, 
                                                               with, 
                                                               CommonInstrumental), 
                                                        as.matrix), 
                                                  c(2,3), 
                                                  mean)),
       "CommonMediator" = as.data.frame(aaply(laply(lapply(bootstraps_100, 
                                                           with, 
                                                           CommonMediator), 
                                                    as.matrix), 
                                              c(2,3), 
                                              mean)),
       "CommonOutcome" = as.data.frame(aaply(laply(lapply(bootstraps_100, 
                                                          with, 
                                                          CommonOutcome), 
                                                   as.matrix), 
                                             c(2,3), 
                                             mean)))

saveRDS(bootstraps_100, "F:/Documents/Thesis_statisticalMatching/R/DetermineB/bootstraps_100.RData") 

bootstraps_250 = 
  lapply(seq(1, length(samples_populationboots)),
         FUN = function(x) StatisticalMatching_FullBootstrap(B = 250,
                                                             sample_input = samples_populationboots[[x]],
                                                             assumption = "IVA",
                                                             restriction = FALSE))

bootstraps_250 = 
  list("CommonInstrumental" = as.data.frame(aaply(laply(lapply(bootstraps_250, 
                                                               with, 
                                                               CommonInstrumental), 
                                                        as.matrix), 
                                                  c(2,3), 
                                                  mean)),
       "CommonMediator" = as.data.frame(aaply(laply(lapply(bootstraps_250, 
                                                           with, 
                                                           CommonMediator), 
                                                    as.matrix), 
                                              c(2,3), 
                                              mean)),
       "CommonOutcome" = as.data.frame(aaply(laply(lapply(bootstraps_250, 
                                                          with, 
                                                          CommonOutcome), 
                                                   as.matrix), 
                                             c(2,3), 
                                             mean)))

saveRDS(bootstraps_250, "F:/Documents/Thesis_statisticalMatching/R/DetermineB/bootstraps_250.RData")

bootstraps_500 = 
  lapply(seq(1, length(samples_populationboots)),
         FUN = function(x) StatisticalMatching_FullBootstrap(B = 500,
                                                             sample_input = samples_populationboots[[x]],
                                                             assumption = "IVA",
                                                             restriction = FALSE))

bootstraps_500 = 
  list("CommonInstrumental" = as.data.frame(aaply(laply(lapply(bootstraps_500, 
                                                               with, 
                                                               CommonInstrumental), 
                                                        as.matrix), 
                                                  c(2,3), 
                                                  mean)),
       "CommonMediator" = as.data.frame(aaply(laply(lapply(bootstraps_500, 
                                                           with, 
                                                           CommonMediator), 
                                                    as.matrix), 
                                              c(2,3), 
                                              mean)),
       "CommonOutcome" = as.data.frame(aaply(laply(lapply(bootstraps_500, 
                                                          with, 
                                                          CommonOutcome), 
                                                   as.matrix), 
                                             c(2,3), 
                                             mean)))

saveRDS(bootstraps_500, "F:/Documents/Thesis_statisticalMatching/R/DetermineB/bootstraps_500.RData")



bootstraps_750 = 
  lapply(seq(1, length(samples_populationboots)),
         FUN = function(x) StatisticalMatching_FullBootstrap(B = 750,
                                                             sample_input = samples_populationboots[[x]],
                                                             assumption = "IVA",
                                                             restriction = FALSE))

bootstraps_750 = 
  list("CommonInstrumental" = as.data.frame(aaply(laply(lapply(bootstraps_750, 
                                                               with, 
                                                               CommonInstrumental), 
                                                        as.matrix), 
                                                  c(2,3), 
                                                  mean)),
       "CommonMediator" = as.data.frame(aaply(laply(lapply(bootstraps_750, 
                                                           with, 
                                                           CommonMediator), 
                                                    as.matrix), 
                                              c(2,3), 
                                              mean)),
       "CommonOutcome" = as.data.frame(aaply(laply(lapply(bootstraps_750, 
                                                          with, 
                                                          CommonOutcome), 
                                                   as.matrix), 
                                             c(2,3), 
                                             mean)))

saveRDS(bootstraps_750, "F:/Documents/Thesis_statisticalMatching/R/DetermineB/bootstraps_750.RData")

bootstraps_1000 = 
  lapply(seq(1, length(samples_populationboots)),
         FUN = function(x) StatisticalMatching_FullBootstrap(B = 1000,
                                                             sample_input = samples_populationboots[[x]],
                                                             assumption = "IVA",
                                                             restriction = FALSE))

bootstraps_1000 = 
  list("CommonInstrumental" = as.data.frame(aaply(laply(lapply(bootstraps_1000, 
                                                               with, 
                                                               CommonInstrumental), 
                                                        as.matrix), 
                                                  c(2,3), 
                                                  mean)),
       "CommonMediator" = as.data.frame(aaply(laply(lapply(bootstraps_1000, 
                                                           with, 
                                                           CommonMediator), 
                                                    as.matrix), 
                                              c(2,3), 
                                              mean)),
       "CommonOutcome" = as.data.frame(aaply(laply(lapply(bootstraps_1000, 
                                                          with, 
                                                          CommonOutcome), 
                                                   as.matrix), 
                                             c(2,3), 
                                             mean)))

saveRDS(bootstraps_1000, "F:/Documents/Thesis_statisticalMatching/R/DetermineB/bootstraps_1000.RData")
