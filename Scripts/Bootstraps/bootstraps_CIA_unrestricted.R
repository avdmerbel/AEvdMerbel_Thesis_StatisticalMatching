# load all functions
setwd("F:/Documents/Thesis_StatisticalMatching/R/Functions_StatisticalMatching")
file.sources = list.files(pattern="*.R$")
sapply(file.sources,source,.GlobalEnv)


library(dplyr)
library(plyr)

set.seed(2310)

# population 1
sims_pop1 = 
  readRDS("F:/Documents/Thesis_statisticalMatching/R/Simulations/CIA_unrestricted/simulate_population1_CIA.RData")

samples_pop1 = 
  sims_pop1$SamplesBootstrap

bootstraps_pop1 = 
  lapply(seq(1, length(samples_pop1)),
         FUN = function(x) StatisticalMatching_FullBootstrap(B = 100,
                                                             sample_input = samples_pop1[[x]],
                                                             assumption = "CIA",
                                                             restriction = FALSE))

bootstraps_pop1 = 
  list("CommonInstrumental" = as.data.frame(aaply(laply(lapply(bootstraps_pop1, 
                                                               with, 
                                                               CommonInstrumental), 
                                                        as.matrix), 
                                                  c(2,3), 
                                                  mean)),
       "CommonMediator" = as.data.frame(aaply(laply(lapply(bootstraps_pop1, 
                                                           with, 
                                                           CommonMediator), 
                                                    as.matrix), 
                                              c(2,3), 
                                              mean)),
       "CommonOutcome" = as.data.frame(aaply(laply(lapply(bootstraps_pop1, 
                                                          with, 
                                                          CommonOutcome), 
                                                   as.matrix), 
                                             c(2,3), 
                                             mean)))

saveRDS(bootstraps_pop1, "F:/Documents/Thesis_statisticalMatching/R/Bootstraps/CIA_unrestricted/bootstraps_pop1_CIA.RData")


# population 2

sims_pop2 = 
  readRDS("F:/Documents/Thesis_statisticalMatching/R/Simulations/CIA_unrestricted/simulate_population2_CIA.RData")

samples_pop2 = 
  sims_pop2$SamplesBootstrap

bootstraps_pop2 = 
  lapply(seq(1, length(samples_pop2)),
         FUN = function(x) StatisticalMatching_FullBootstrap(B = 100,
                                                             sample_input = samples_pop2[[x]],
                                                             assumption = "CIA",
                                                             restriction = FALSE))

bootstraps_pop2 = 
  list("CommonInstrumental" = as.data.frame(aaply(laply(lapply(bootstraps_pop2, 
                                                               with, 
                                                               CommonInstrumental), 
                                                        as.matrix), 
                                                  c(2,3), 
                                                  mean)),
       "CommonMediator" = as.data.frame(aaply(laply(lapply(bootstraps_pop2, 
                                                           with, 
                                                           CommonMediator), 
                                                    as.matrix), 
                                              c(2,3), 
                                              mean)),
       "CommonOutcome" = as.data.frame(aaply(laply(lapply(bootstraps_pop2, 
                                                          with, 
                                                          CommonOutcome), 
                                                   as.matrix), 
                                             c(2,3), 
                                             mean)))

saveRDS(bootstraps_pop2, "F:/Documents/Thesis_statisticalMatching/R/Bootstraps/CIA_unrestricted/bootstraps_pop2_CIA.RData")

# population 3

sims_pop3 = 
  readRDS("F:/Documents/Thesis_statisticalMatching/R/Simulations/CIA_unrestricted/simulate_population3_CIA.RData")

samples_pop3 = 
  sims_pop3$SamplesBootstrap

bootstraps_pop3 = 
  lapply(seq(1, length(samples_pop3)),
         FUN = function(x) StatisticalMatching_FullBootstrap(B = 100,
                                                             sample_input = samples_pop3[[x]],
                                                             assumption = "CIA",
                                                             restriction = FALSE))

bootstraps_pop3 = 
  list("CommonInstrumental" = as.data.frame(aaply(laply(lapply(bootstraps_pop3, 
                                                               with, 
                                                               CommonInstrumental), 
                                                        as.matrix), 
                                                  c(2,3), 
                                                  mean)),
       "CommonMediator" = as.data.frame(aaply(laply(lapply(bootstraps_pop3, 
                                                           with, 
                                                           CommonMediator), 
                                                    as.matrix), 
                                              c(2,3), 
                                              mean)),
       "CommonOutcome" = as.data.frame(aaply(laply(lapply(bootstraps_pop3, 
                                                          with, 
                                                          CommonOutcome), 
                                                   as.matrix), 
                                             c(2,3), 
                                             mean)))

saveRDS(bootstraps_pop3, "F:/Documents/Thesis_statisticalMatching/R/Bootstraps/CIA_unrestricted/bootstraps_pop3_CIA.RData")

# population 4

sims_pop4 = 
  readRDS("F:/Documents/Thesis_statisticalMatching/R/Simulations/CIA_unrestricted/simulate_population4_CIA.RData")

samples_pop4 = 
  sims_pop4$SamplesBootstrap

bootstraps_pop4 = 
  lapply(seq(1, length(samples_pop4)),
         FUN = function(x) StatisticalMatching_FullBootstrap(B = 100,
                                                             sample_input = samples_pop4[[x]],
                                                             assumption = "CIA",
                                                             restriction = FALSE))

bootstraps_pop4 = 
  list("CommonInstrumental" = as.data.frame(aaply(laply(lapply(bootstraps_pop4, 
                                                               with, 
                                                               CommonInstrumental), 
                                                        as.matrix), 
                                                  c(2,3), 
                                                  mean)),
       "CommonMediator" = as.data.frame(aaply(laply(lapply(bootstraps_pop4, 
                                                           with, 
                                                           CommonMediator), 
                                                    as.matrix), 
                                              c(2,3), 
                                              mean)),
       "CommonOutcome" = as.data.frame(aaply(laply(lapply(bootstraps_pop4, 
                                                          with, 
                                                          CommonOutcome), 
                                                   as.matrix), 
                                             c(2,3), 
                                             mean)))

saveRDS(bootstraps_pop4, "F:/Documents/Thesis_statisticalMatching/R/Bootstraps/CIA_unrestricted/bootstraps_pop4_CIA.RData")

# population 5

sims_pop5 = 
  readRDS("F:/Documents/Thesis_statisticalMatching/R/Simulations/CIA_unrestricted/simulate_population5_CIA.RData")

samples_pop5 = 
  sims_pop5$SamplesBootstrap

bootstraps_pop5 = 
  lapply(seq(1, length(samples_pop5)),
         FUN = function(x) StatisticalMatching_FullBootstrap(B = 100,
                                                             sample_input = samples_pop5[[x]],
                                                             assumption = "CIA",
                                                             restriction = FALSE))

bootstraps_pop5 = 
  list("CommonInstrumental" = as.data.frame(aaply(laply(lapply(bootstraps_pop5, 
                                                               with, 
                                                               CommonInstrumental), 
                                                        as.matrix), 
                                                  c(2,3), 
                                                  mean)),
       "CommonMediator" = as.data.frame(aaply(laply(lapply(bootstraps_pop5, 
                                                           with, 
                                                           CommonMediator), 
                                                    as.matrix), 
                                              c(2,3), 
                                              mean)),
       "CommonOutcome" = as.data.frame(aaply(laply(lapply(bootstraps_pop5, 
                                                          with, 
                                                          CommonOutcome), 
                                                   as.matrix), 
                                             c(2,3), 
                                             mean)))

saveRDS(bootstraps_pop5, "F:/Documents/Thesis_statisticalMatching/R/Bootstraps/CIA_unrestricted/bootstraps_pop5_CIA.RData")