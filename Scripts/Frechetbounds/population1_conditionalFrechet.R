# In this script the conditional frechet bounds are calculated for population 1

setwd("./Data/Populations")

populations = 
  readRDS("main_populations.RData")

results = 
  readRDS("./Data/PlotData_Estimates.RData")

pop1 = populations[[1]]

agg = 
  lapply(pop1$AggregatedDistribution, xtabs, formula = proportion ~.)

cond = 
  lapply(agg, prop.table, margin = 1)

cond2 =
  lapply(agg, prop.table, margin = 2)

out_ins = cond$CommonMediator
med_ins = cond$CommonOutcome

out_med = cond$CommonInstrumental
ins_med = t(cond2$CommonOutcome)

med_out = t(cond2$CommonInstrumental)
ins_out = t(cond2$CommonMediator)

ins = .429
med = .615
out = .652


# common instrumental

lower_ins = 
  c(sum((max(0, (out_ins[1,1] + med_ins[1,1] - 1)) * ins),
        (max(0, (out_ins[2,1] + med_ins[2,1] - 1)) * (1-ins))),
    sum((max(0, (out_ins[1,1] + med_ins[1,2] - 1)) * ins),
        (max(0, (out_ins[2,1] + med_ins[2,2] - 1)) * (1-ins))),
    sum((max(0, (out_ins[1,2] + med_ins[1,1] - 1)) * ins),
        (max(0, (out_ins[2,2] + med_ins[2,1] - 1)) * (1-ins))),
    sum((max(0, (out_ins[1,2] + med_ins[1,2] - 1)) * ins),
        (max(0, (out_ins[2,2] + med_ins[2,2] - 1)) * (1-ins))))

upper_ins = 
  c(sum((min(out_ins[1,1], med_ins[1,1]) * ins),
        (min(out_ins[2,1], med_ins[2,1]) * (1-ins))),
    sum((min(out_ins[1,1], med_ins[1,2]) * ins),
        (min(out_ins[2,1], med_ins[2,2]) * (1-ins))),
    sum((min(out_ins[1,2], med_ins[1,1]) * ins),
        (min(out_ins[2,2], med_ins[2,1]) * (1-ins))),
    sum((min(out_ins[1,2], med_ins[1,2]) * ins),
        (min(out_ins[2,2], med_ins[2,2]) * (1-ins))))

results[results$method == "IVA" & results$population == 1 & results$situation == "Instrumental", "conditional_lower"] = lower_ins
results[results$method == "IVA" & results$population == 1 & results$situation == "Instrumental", "conditional_upper"] = upper_ins
 
results[results$method == "CIA" & results$population == 1 & results$situation == "Instrumental", "conditional_lower"] = lower_ins
results[results$method == "CIA" & results$population == 1 & results$situation == "Instrumental", "conditional_upper"] = upper_ins

# common mediator

lower_med = 
  c(sum((max(0, (out_med[1,1] + ins_med[1,1] - 1)) * med),
        (max(0, (out_med[2,1] + ins_med[2,1] - 1)) * (1-med))),
    sum((max(0, (out_med[1,1] + ins_med[1,2] - 1)) * med),
        (max(0, (out_med[2,1] + ins_med[2,2] - 1)) * (1-med))),
    sum((max(0, (out_med[1,2] + ins_med[1,1] - 1)) * med),
        (max(0, (out_med[2,2] + ins_med[2,1] - 1)) * (1-med))),
    sum((max(0, (out_med[1,2] + ins_med[1,2] - 1)) * med),
        (max(0, (out_med[2,2] + ins_med[2,2] - 1)) * (1-med))))

upper_med = 
  c(sum((min(out_med[1,1], ins_med[1,1]) * med),
        (min(out_med[2,1], ins_med[2,1]) * (1-med))),
    sum((min(out_med[1,1], ins_med[1,2]) * med),
        (min(out_med[2,1], ins_med[2,2]) * (1-med))),
    sum((min(out_med[1,2], ins_med[1,1]) * med),
        (min(out_med[2,2], ins_med[2,1]) * (1-med))),
    sum((min(out_med[1,2], ins_med[1,2]) * med),
        (min(out_med[2,2], ins_med[2,2]) * (1-med))))

results[results$method == "IVA" & results$population == 1 & results$situation == "Mediator", "conditional_lower"] = lower_med
results[results$method == "IVA" & results$population == 1 & results$situation == "Mediator", "conditional_upper"] = upper_med

results[results$method == "CIA" & results$population == 1 & results$situation == "Mediator", "conditional_lower"] = lower_med
results[results$method == "CIA" & results$population == 1 & results$situation == "Mediator", "conditional_upper"] = upper_med

# common outcome

lower_out = 
  c(sum((max(0, (med_out[1,1] + ins_out[1,1] - 1)) * out),
        (max(0, (med_out[2,1] + ins_out[2,1] - 1)) * (1-out))),
    sum((max(0, (med_out[1,2] + ins_out[1,1] - 1)) * out),
        (max(0, (med_out[2,2] + ins_out[2,1] - 1)) * (1-out))),
    sum((max(0, (med_out[1,1] + ins_out[1,2] - 1)) * out),
        (max(0, (med_out[2,1] + ins_out[2,2] - 1)) * (1-out))),
    sum((max(0, (med_out[1,2] + ins_out[1,2] - 1)) * out),
        (max(0, (med_out[2,2] + ins_out[2,2] - 1)) * (1-out))))

upper_out = 
  c(sum((min(med_out[1,1], ins_out[1,1]) * out),
        (min(med_out[2,1], ins_out[2,1]) * (1-out))),
    sum((min(med_out[1,2], ins_out[1,1]) * out),
        (min(med_out[2,2], ins_out[2,1]) * (1-out))),
    sum((min(med_out[1,1], ins_out[1,2]) * out),
        (min(med_out[2,1], ins_out[2,2]) * (1-out))),
    sum((min(med_out[1,2], ins_out[1,2]) * out),
        (min(med_out[2,2], ins_out[2,2]) * (1-out))))

results[results$method == "IVA" & results$population == 1 & results$situation == "Outcome", "conditional_lower"] = lower_out
results[results$method == "IVA" & results$population == 1 & results$situation == "Outcome", "conditional_upper"] = upper_out

results[results$method == "CIA" & results$population == 1 & results$situation == "Outcome", "conditional_lower"] = lower_out
results[results$method == "CIA" & results$population == 1 & results$situation == "Outcome", "conditional_upper"] = upper_out

# saveRDS(results, 
#         "F:\\Documents\\Thesis_StatisticalMatching\\Tables_Thesis\\PlotData_Estimates.RData")


