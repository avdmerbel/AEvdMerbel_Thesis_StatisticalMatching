setwd("F:/Documents/Thesis_StatisticalMatching/R/Simulations/IVA_unrestricted")

pop1 = 
  readRDS("simulate_population1_IVA.RData")

pop1_error = 
  pop1$EstimationError
pop1_true = 
  pop1$TrueParameters

pop1_RMSE = lapply(pop1_error, FUN = function(x) mean(x$RMSE))
pop1_Bias = lapply(pop1_error, FUN = function(x) mean(abs(x$Bias)))
pop1_true = lapply(pop1_true, round, digits = 4)

pop1_RMSE;pop1_Bias;pop1_true

pop2 = 
  readRDS("simulate_population2_IVA.RData")

pop2_error = 
  pop2$EstimationError
pop2_true = 
  pop2$TrueParameters

pop2_RMSE = lapply(pop2_error, FUN = function(x) mean(x$RMSE))
pop2_Bias = lapply(pop2_error, FUN = function(x) mean(abs(x$Bias)))
pop2_true = lapply(pop2_true, round, digits = 4)

pop2_RMSE;pop2_Bias;pop2_true

pop3 = 
  readRDS("simulate_population3_IVA.RData")

pop3_error = 
  pop3$EstimationError
pop3_true = 
  pop3$TrueParameters

pop3_RMSE = lapply(pop3_error, FUN = function(x) mean(x$RMSE))
pop3_Bias = lapply(pop3_error, FUN = function(x) mean(abs(x$Bias)))
pop3_true = lapply(pop3_true, round, digits = 4)

pop3_RMSE;pop3_Bias;pop3_true

pop4 = 
  readRDS("simulate_population4_IVA.RData")

pop4_error = 
  pop4$EstimationError
pop4_true = 
  pop4$TrueParameters

pop4_RMSE = lapply(pop4_error, FUN = function(x) mean(x$RMSE))
pop4_Bias = lapply(pop4_error, FUN = function(x) mean(abs(x$Bias)))
pop4_true = lapply(pop4_true, round, digits = 4)

pop4_RMSE;pop4_Bias;pop4_true

pop5 = 
  readRDS("simulate_population5_IVA.RData")

pop5_error = 
  pop5$EstimationError
pop5_true = 
  pop5$TrueParameters

pop5_RMSE = lapply(pop5_error, FUN = function(x) mean(x$RMSE))
pop5_Bias = lapply(pop5_error, FUN = function(x) mean(abs(x$Bias)))
pop5_true = lapply(pop5_true, round, digits = 4)

pop5_RMSE;pop5_Bias;pop5_true