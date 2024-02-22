setwd("F:/Documents/Thesis_StatisticalMatching/R/Bootstraps/IVA_restricted")

pop1 = 
  readRDS("bootstraps_pop1_IVA_res.RData")

pop1_Bias = lapply(pop1, FUN = function(x) mean(abs(x$Bias)))
pop1_RMSE = lapply(pop1, function(x) mean(x$RMSE))
pop1_est = lapply(pop1, round, digits = 4)

pop1_Bias;pop1_est

pop2 = 
  readRDS("bootstraps_pop2_IVA_res.RData")

pop2_Bias = lapply(pop2, FUN = function(x) mean(abs(x$Bias)))
pop2_RMSE = lapply(pop2, function(x) mean(x$RMSE))
pop2_est = lapply(pop2, round, digits = 4)

pop2_Bias;pop2_est

pop3 = 
  readRDS("bootstraps_pop3_IVA_res.RData")

pop3_Bias = lapply(pop3, FUN = function(x) mean(abs(x$Bias)))
pop3_RMSE = lapply(pop3, function(x) mean(x$RMSE))
pop3_est = lapply(pop3, round, digits = 4)

pop3_Bias;pop3_est

pop4 = 
  readRDS("bootstraps_pop4_IVA_res.RData")

pop4_Bias = lapply(pop4, FUN = function(x) mean(abs(x$Bias)))
pop4_RMSE = lapply(pop4, function(x) mean(x$RMSE))
pop4_est = lapply(pop4, round, digits = 4)

pop4_Bias;pop4_est

pop5 = 
  readRDS("bootstraps_pop5_IVA_res.RData")

pop5_Bias = lapply(pop5, FUN = function(x) mean(abs(x$Bias)))
pop5_RMSE = lapply(pop5, function(x) mean(x$RMSE))
pop5_est = lapply(pop5, round, digits = 4)

pop5_Bias;pop5_est

Bootstrap_BiasRMSE = 
  data.frame(scenario = rep(c("Common Instrumental",
                              "Common Mediator",
                              "Common Outcome"),
                            5),
             population = c(rep(1, 3), 
                            rep(2, 3), 
                            rep(3, 3), 
                            rep(4, 3), 
                            rep(5, 3)),
             bootstrap_RMSE = c(unlist(pop1_RMSE),
                                unlist(pop2_RMSE),
                                unlist(pop3_RMSE),
                                unlist(pop4_RMSE),
                                unlist(pop5_RMSE)),
             bootstrap_Bias = c(unlist(pop1_Bias),
                                unlist(pop2_Bias),
                                unlist(pop3_Bias),
                                unlist(pop4_Bias),
                                unlist(pop5_Bias)),
             method = rep("IVA", 15))

biasrmse = 
  readRDS("F:\\Documents\\Thesis_StatisticalMatching\\Tables_Thesis\\PlotData_Simulation.RData")

biasrmse = dplyr::full_join(biasrmse, Bootstrap_BiasRMSE)

saveRDS(biasrmse, "F:\\Documents\\Thesis_StatisticalMatching\\Tables_Thesis\\PlotData_Simulation.RData")

###############################################################################

results_ins = 
  rbind(pop1_est$CommonInstrumental,
        pop2_est$CommonInstrumental,
        pop3_est$CommonInstrumental,
        pop4_est$CommonInstrumental,
        pop5_est$CommonInstrumental)

results_ins = 
  results_ins[,c(3,5,8,9)]

results_med = 
  rbind(pop1_est$CommonMediator,
        pop2_est$CommonMediator,
        pop3_est$CommonMediator,
        pop4_est$CommonMediator,
        pop5_est$CommonMediator)

results_med = 
  results_med[,c(3,5,8,9)]

results_out = 
  rbind(pop1_est$CommonOutcome,
        pop2_est$CommonOutcome,
        pop3_est$CommonOutcome,
        pop4_est$CommonOutcome,
        pop5_est$CommonOutcome)

results_out = 
  results_out[,c(3,5,8,9)]

res = rbind(results_ins,results_med,results_out)


setwd("F:\\Documents\\Thesis_StatisticalMatching\\Tables_Thesis")

results = 
  readRDS("PlotData_Estimates.RData")

results$bootstrap_mean[1:60] = res$mean
results$bootstrap_stdev[1:60] = res$standard_deviation
results$bootstrap_lower_CI[1:60] = res$lower_CI
results$bootstrap_upper_CI[1:60] = res$upper_CI

saveRDS(results, "PlotData_Estimates.RData")

