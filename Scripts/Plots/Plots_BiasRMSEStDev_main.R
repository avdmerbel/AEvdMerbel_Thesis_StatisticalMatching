# this file contains plot construction for figures 4.3 and 4.4

setwd("./Data")

library(ggplot2)
library(patchwork)

data = 
  readRDS("PlotData_Simulation.RData")

results = 
  readRDS("PlotData_Estimates.RData")

# below is some code that was used to add columns
# to the data, the columns are now in so no need to use
# the code but still here for reference
# 
# agg_stdev = 
#   aggregate(standard_deviation ~ population + situation + method, data = results, FUN = mean)
# agg_stdev=
# agg_stdev[order(agg_stdev$method, decreasing = TRUE),]
# 
# agg_stdev_bootstrap =
#   aggregate(bootstrap_stdev ~ population + situation + method, data = results, FUN = mean)
# 
# agg_stdev_bootstrap =
#   agg_stdev_bootstrap[order(agg_stdev_bootstrap$method, decreasing = TRUE),]
# 
# colnames(agg_stdev_bootstrap)[4] = "standard_deviation"
# 
# agg_stdev_all = rbind(agg_stdev, agg_stdev_bootstrap)
# 
# agg_stdev_all$method = factor(agg_stdev_all$method,
#                               levels = c("IVA", "CIA"),
#                               labels = c("Instrumental Variable Assumption",
#                                          "Conditional Independence Assumption"))
# 
# agg_stdev_all$analysis = c(rep("Simulation", 30), rep("Bootstrap", 30))
# 
# agg_stdev_all = agg_stdev_all[order(agg_stdev_all$analysis,agg_stdev_all$method, agg_stdev_all$population),]
# 
# agg_stdev_all = agg_stdev_all[order(agg_stdev_all$analysis, decreasing = TRUE),]
# 
# data$StandardDeviation = agg_stdev_all$standard_deviation

windowsFonts(Times = windowsFont("TT Times New Roman"))


data$analysis = 
  factor(data$analysis,
         levels = c("Simulation", "Bootstrap"))

data$scenario_factor =
  factor(data$scenario,
         levels = c("Mediator", "Outcome", "Instrumental"),
         labels = c("Mediator (scenario 1)", "Outcome (scenario 2)", "Instrumental (scenario 3)"))

data$population_factor = 
  factor(data$population,
         levels = c(1,4,2,3,5),
         labels = c("No violation",
                    "Type B \n violation",
                    "Slight \n type A \n violation",
                    "Severe \n type A \n violation",
                    "Type B and \n slight type A \n violation"))

general = 
  list(theme(panel.background = element_rect(fill = "gray98"),
             panel.grid.major = element_line(color = "gray95"),
             axis.line.x.bottom = element_line(color = "gray65"),
             axis.line.y.left = element_line(color = "gray65"),
             text = element_text(family = "Times"),
             plot.subtitle = element_text(size = 12)),
       scale_color_discrete(type = RColorBrewer::brewer.pal(3, "Dark2")),
       scale_shape_manual(values = c(16,17)))


# RMSE sim vs bootstrap
rmse_boot = 
  ggplot(data = data[data$method == "Instrumental Variable Assumption",])+
  geom_point(mapping = aes(y = RMSE,
                           x = population_factor,
                           shape = analysis,
                           group = analysis,
                           color = scenario_factor),
             size = 1.7,
             position = position_dodge(width = .2)) +
  labs(subtitle = "RMSE",
       y = "RMSE",
       color = "Overlapping variable (scenario)",
       shape = "Analysis",
       x = NULL) +
  scale_y_continuous(limits = c(0, 0.16),
                     breaks = c(0, 0.04, 0.08, 0.12, 0.16)) +
  general

# Bias sim vs bootstrap

bias_boot = 
  ggplot(data = data[data$method == "Instrumental Variable Assumption",]) +
  geom_point(mapping = aes(y = Bias,
                           x = population_factor,
                           shape = analysis,
                           group = analysis,
                           color = scenario_factor),
             size = 1.7,
             position = position_dodge(width = 0.2)) +
  labs(subtitle = "Bias",
       y = "Bias",
       color = "Overlapping variable (scenario)",
       shape = "Analysis",
       x = NULL) +
  scale_y_continuous(limits = c(0, 0.16),
                     breaks = c(0, 0.04, 0.08, 0.12, 0.16)) +
  general

# standard deviation vs bootstrap
stdev_boot = 
  ggplot(data = data[data$method == "Instrumental Variable Assumption",])+
  geom_point(mapping = aes(y = StandardDeviation,
                           x = population_factor,
                           shape = analysis,
                           group = analysis,
                           color = scenario_factor),
             size = 1.7,
             position = position_dodge(width = .2)) +
  labs(subtitle = "Standard deviation",
       y = "Standard deviation",
       color = "Overlapping variable (scenario)",
       shape = "Analysis",
       x = NULL) +
  scale_y_continuous(limits = c(0, 0.04)) +
  general

# RMSE IVA vs CIA
rmse_IVACIA = 
  ggplot(data = data[data$analysis == "Simulation",])+
  geom_point(mapping = aes(y = RMSE,
                           x = population_factor,
                           color = scenario_factor,
                           shape = method,
                           group = method),
             size = 1.7,
             position = position_dodge(width = .2)) +
  labs(subtitle = "RMSE",
       color = "Overlapping variable (scenario)",
       shape = "Assumption",
       x = NULL) +
  scale_y_continuous(limits = c(0, 0.16),
                     breaks = c(0, 0.04, 0.08, 0.12, 0.16)) +
  general

# Bias IVA vs CIA
bias_IVACIA = 
  ggplot(data = data[data$analysis == "Simulation",]) +
  geom_point(mapping = aes(y = Bias,
                           x = population_factor,
                           color = scenario_factor,
                           shape = method,
                           group = method),
             size = 1.7,
             position = position_dodge(width = .2)) +
  labs(subtitle = "Bias",
       color = "Overlapping variable (scenario)",
       shape = "Assumption",
       x = NULL) +
  scale_y_continuous(limits = c(0, 0.16),
                     breaks = c(0, 0.04, 0.08, 0.12, 0.16)) +
  general

# variance IVA vs CIA

stdev_IVACIA = 
  ggplot(data = data[data$analysis == "Simulation",]) +
  geom_point(mapping = aes(y = StandardDeviation,
                           x = population_factor,
                           color = scenario_factor,
                           shape = method,
                           group = method),
             size = 1.7,
             position = position_dodge(width = .2)) +
  labs(subtitle = "Standard Deviation",
       color = "Overlapping variable (scenario)",
       shape = "Assumption",
       y = "Standard deviation",
       x = NULL) +
  scale_y_continuous(limits = c(0, 0.04)) +
  general




plot_bootstraps = (bias_boot + rmse_boot + stdev_boot + guide_area())

plot_bootstraps + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a")





plot_IVACIA = (bias_IVACIA + rmse_IVACIA +stdev_IVACIA + guide_area())

plot_IVACIA + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a")



