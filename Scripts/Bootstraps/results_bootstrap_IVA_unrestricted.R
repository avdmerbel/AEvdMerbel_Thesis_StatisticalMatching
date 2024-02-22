setwd("F:/Documents/Thesis_StatisticalMatching/R/Bootstraps/IVA_unrestricted")

pop1 = 
  readRDS("bootstraps_pop1_IVA.RData")

pop1_Bias = lapply(pop1, FUN = function(x) mean(abs(x$Bias)))
pop1_est = lapply(pop1, round, digits = 4)

pop1_Bias;pop1_est

pop2 = 
  readRDS("bootstraps_pop2_IVA.RData")

pop2_Bias = lapply(pop2, FUN = function(x) mean(abs(x$Bias)))
pop2_est = lapply(pop2, round, digits = 4)

pop2_Bias;pop2_est

pop3 = 
  readRDS("bootstraps_pop3_IVA.RData")

pop3_Bias = lapply(pop3, FUN = function(x) mean(abs(x$Bias)))
pop3_est = lapply(pop3, round, digits = 4)

pop3_Bias;pop3_est

pop4 = 
  readRDS("bootstraps_pop4_IVA.RData")

pop4_Bias = lapply(pop4, FUN = function(x) mean(abs(x$Bias)))
pop4_est = lapply(pop4, round, digits = 4)

pop4_Bias;pop4_est

pop5 = 
  readRDS("bootstraps_pop5_IVA.RData")

pop5_Bias = lapply(pop5, FUN = function(x) mean(abs(x$Bias)))
pop5_est = lapply(pop5, round, digits = 4)

pop5_Bias;pop5_est