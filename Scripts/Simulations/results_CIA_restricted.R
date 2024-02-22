setwd("F:/Documents/Thesis_StatisticalMatching/R/Simulations/CIA_restricted")

pop1 = 
  readRDS("simulate_population1_CIA_res.RData")

pop1_error = 
  pop1$EstimationError
pop1_true = 
  pop1$TrueParameters

pop1_RMSE = lapply(pop1_error, FUN = function(x) mean(x$RMSE))
pop1_Bias = lapply(pop1_error, FUN = function(x) mean(abs(x$Bias)))
pop1_true = lapply(pop1_true, round, digits = 4)

#pop1_RMSE;pop1_Bias

pop1_true

pop2 = 
  readRDS("simulate_population2_CIA_res.RData")

pop2_error = 
  pop2$EstimationError
pop2_true = 
  pop2$TrueParameters

pop2_RMSE = lapply(pop2_error, FUN = function(x) mean(x$RMSE))
pop2_Bias = lapply(pop2_error, FUN = function(x) mean(abs(x$Bias)))
pop2_true = lapply(pop2_true, round, digits = 4)

#pop2_RMSE;pop2_Bias

pop2_true

pop3 = 
  readRDS("simulate_population3_CIA_res.RData")

pop3_error = 
  pop3$EstimationError
pop3_true = 
  pop3$TrueParameters

pop3_RMSE = lapply(pop3_error, FUN = function(x) mean(x$RMSE))
pop3_Bias = lapply(pop3_error, FUN = function(x) mean(abs(x$Bias)))
pop3_true = lapply(pop3_true, round, digits = 4)

#pop3_RMSE;pop3_Bias

pop3_true

pop4 = 
  readRDS("simulate_population4_CIA_res.RData")

pop4_error = 
  pop4$EstimationError
pop4_true = 
  pop4$TrueParameters

pop4_RMSE = lapply(pop4_error, FUN = function(x) mean(x$RMSE))
pop4_Bias = lapply(pop4_error, FUN = function(x) mean(abs(x$Bias)))
pop4_true = lapply(pop4_true, round, digits = 4)

#pop4_RMSE;pop4_Bias

pop4_true

pop5 = 
  readRDS("simulate_population5_CIA_res.RData")

pop5_error = 
  pop5$EstimationError
pop5_true = 
  pop5$TrueParameters

pop5_RMSE = lapply(pop5_error, FUN = function(x) mean(x$RMSE))
pop5_Bias = lapply(pop5_error, FUN = function(x) mean(abs(x$Bias)))
pop5_true = lapply(pop5_true, round, digits = 4)

#pop5_RMSE;pop5_Bias

pop5_true

BiasRMSE_CIA = 
  data.frame(scenario = rep(c("Common Instrumental",
                              "Common Mediator",
                              "Common Outcome"),
                            5),
             population = c(rep(1, 3), 
                            rep(2, 3), 
                            rep(3, 3), 
                            rep(4, 3), 
                            rep(5, 3)),
             RMSE = c(unlist(pop1_RMSE),
                      unlist(pop2_RMSE),
                      unlist(pop3_RMSE),
                      unlist(pop4_RMSE),
                      unlist(pop5_RMSE)),
             Bias = c(unlist(pop1_Bias),
                      unlist(pop2_Bias),
                      unlist(pop3_Bias),
                      unlist(pop4_Bias),
                      unlist(pop5_Bias)),
             method = rep("CIA", 15))

biasrmse = 
  readRDS("F:\\Documents\\Thesis_StatisticalMatching\\Tables_Thesis\\PlotData_Simulation.RData")

biasrmse = rbind(biasrmse, BiasRMSE_CIA)

saveRDS(biasrmse, "F:\\Documents\\Thesis_StatisticalMatching\\Tables_Thesis\\PlotData_Simulation.RData")
###############################################################################

results_ins = 
  rbind(pop1_true$CommonInstrumental,
        pop2_true$CommonInstrumental,
        pop3_true$CommonInstrumental,
        pop4_true$CommonInstrumental,
        pop5_true$CommonInstrumental)

results_ins$population = c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4))
results_ins$combination = rep(c("M1O1", "M2O1", "M1O2", "M2O2"), 5)

results_ins = results_ins[,c(3, 6,7,8,9,10)]

results_ins$situation = rep("Instrumental", 20)

results_med = 
  rbind(pop1_true$CommonMediator,
        pop2_true$CommonMediator,
        pop3_true$CommonMediator,
        pop4_true$CommonMediator,
        pop5_true$CommonMediator)

results_med$population = c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4))
results_med$combination = rep(c("I1O1", "I2O1", "I1O2", "I2O2"), 5)

results_med = results_med[,c(3, 6,7,8,9,10)]

results_med$situation = rep("Mediator", 20)

results_out = 
  rbind(pop1_true$CommonOutcome,
        pop2_true$CommonOutcome,
        pop3_true$CommonOutcome,
        pop4_true$CommonOutcome,
        pop5_true$CommonOutcome)

results_out$population = c(rep(1,4), rep(2,4), rep(3,4), rep(4,4), rep(5,4))
results_out$combination = rep(c("M1I1", "M2I1", "M1I2", "M2I2"), 5)

results_out = results_out[,c(3, 6,7,8,9,10)]

results_out$situation = rep("Outcome", 20)

results = 
  rbind(results_ins, results_med, results_out)

rel = 
  results[results$combination == "M2I1" | results$combination == "M1I2",][,c(1:4)]

rel = 
  rel[seq_len(nrow(rel)) + c(1,-1),]

results[results$combination == "M2I1" | results$combination == "M1I2",][,c(1:4)] = rel

results$method = "CIA"
results$restriction = "YES"

results_total = 
  readRDS("F:\\Documents\\Thesis_StatisticalMatching\\Tables_Thesis\\PlotData_Estimates.RData")

results_total = 
  dplyr::full_join(results_total, results)


results_total[61:120, 8:11] = results_total[1:60, 8:11]

saveRDS(results_total, "F:\\Documents\\Thesis_StatisticalMatching\\Tables_Thesis\\PlotData_Estimates.RData")



