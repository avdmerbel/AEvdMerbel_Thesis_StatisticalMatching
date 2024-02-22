# This file contains the plot construction of figure 4.1
setwd("./Data")

library(ggplot2)
library(patchwork)

data = 
  readRDS("PlotData_Simulation.RData")

results = 
  readRDS("PlotData_Estimates.RData")

windowsFonts(Times = windowsFont("TT Times New Roman"))


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
             plot.subtitle = element_text(size = 12),
             plot.title = element_text(face = "bold", size = 12),
             plot.caption.position = "panel",
             plot.caption = element_text(hjust = 0))
  )

bias_frechet1 =
  ggplot(data = data[data$method == "Instrumental Variable Assumption" & data$analysis == "Simulation",],
         mapping = aes(x = population_factor,
                       y = Bias,
                       fill = scenario_factor)) +
  geom_col(mapping = aes(x = population_factor,
                         y = conditional_width,
                         group = scenario_factor),
           fill = "gray85",
           color = "gray83",
           alpha = .85,
           position = position_dodge()) +
  geom_col(position = position_dodge()) +
  labs(x = NULL,
       fill = "Overlapping variable (scenario)",
       subtitle = "Bias") +
  scale_fill_discrete(type = RColorBrewer::brewer.pal(3, "Dark2")) +
  scale_y_continuous(limits = c(0, 0.4),
                     expand = c(0,0)) +
  general


RMSE1 =
  ggplot(data = data[data$method == "Instrumental Variable Assumption" & data$analysis == "Simulation",],
         mapping = aes(x = population_factor,
                       y = RMSE,
                       fill = scenario_factor)) +
  geom_col(position = position_dodge()) +
  labs(x = NULL,
       fill = "Overlapping variable (scenario)",
       subtitle = "RMSE") +
  scale_fill_discrete(type = RColorBrewer::brewer.pal(3, "Dark2")) +
  scale_y_continuous(limits = c(0,0.4),
                     expand = c(0,0)) +
  general


stdev1 =
  ggplot(data = data[data$method == "Instrumental Variable Assumption" & data$analysis == "Simulation",],
         mapping = aes(x = population_factor,
                       y = StandardDeviation,
                       fill = scenario_factor)) +
  geom_col(position = position_dodge()) +
  labs(x = NULL,
       y = "Standard deviation",
       fill = "Overlapping variable (scenario)",
       subtitle = "Standard deviation") +
  scale_fill_discrete(type = RColorBrewer::brewer.pal(3, "Dark2")) +
  scale_y_continuous(limits = c(0,0.2),
                     expand = c(0,0)) +
  general



plot_mainresults = (bias_frechet1 + RMSE1 + stdev1 + guide_area())

plot_mainresults + 
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a")



##########################################
##########################################
# ########################################## 
# bias_frechet1 =
# ggplot(data = data[data$method == "Conditional Independence Assumption" & data$analysis == "Simulation",],
#        mapping = aes(x = population,
#                      y = Bias,
#                      fill = scenario)) +
#   geom_col(mapping = aes(x = population,
#                          y = conditional_width,
#                          group = scenario),
#            fill = "gray85",
#            color = "gray83",
#            alpha = .85,
#            position = position_dodge()) +
#   geom_col(position = position_dodge()) +
#   labs(x = "Population",
#        fill = "Overlapping variable",
#        subtitle = "Simulated bias") +
#   scale_fill_discrete(type = RColorBrewer::brewer.pal(3, "Dark2")) +
#   scale_y_continuous(limits = c(0, 0.4),
#                      expand = c(0,0)) +
#   general
# 
# ggplot(data = data[data$analysis == "Simulation",],
#        mapping = aes(x = population,
#                      y = Bias,
#                      fill = scenario,
#                      group = method)) +
#   geom_col(mapping = aes(x = population,
#                          y = conditional_width,
#                          group = scenario),
#            fill = "gray85",
#            color = "gray83",
#            alpha = .85,
#            position = position_dodge()) +
#   geom_col(position = position_dodge2()) +
#   labs(x = "Population",
#        fill = "Overlapping variable",
#        subtitle = "Simulated bias") +
#   scale_fill_discrete(type = RColorBrewer::brewer.pal(3, "Dark2")) +
#   scale_y_continuous(limits = c(0, 0.4),
#                      expand = c(0,0)) +
#   general
# 
# RMSE1 =
#   ggplot(data = data[data$method == "Conditional Independence Assumption" & data$analysis == "Simulation",],
#          mapping = aes(x = population,
#                        y = RMSE,
#                        fill = scenario)) +
#   geom_col(position = position_dodge()) +
#   labs(x = "Population",
#        fill = "Overlapping variable",
#        subtitle = "Simulated RMSE") +
#   scale_fill_discrete(type = RColorBrewer::brewer.pal(3, "Dark2")) +
#   scale_y_continuous(limits = c(0,0.4),
#                      expand = c(0,0)) +
#   general
# 
# 
# stdev1 =
#   ggplot(data = data[data$method == "Conditional Independence Assumption" & data$analysis == "Simulation",],
#          mapping = aes(x = population,
#                        y = StandardDeviation,
#                        fill = scenario)) +
#   geom_col(position = position_dodge()) +
#   labs(x = "Population",
#        y = "Standard deviation",
#        fill = "Overlapping variable",
#        subtitle = "Simulated standard deviation") +
#   scale_fill_discrete(type = RColorBrewer::brewer.pal(3, "Dark2")) +
#   scale_y_continuous(limits = c(0,0.2),
#                      expand = c(0,0)) +
#   general


# plot_mainresults = (bias_frechet1 + RMSE1 + stdev1 + guide_area())
# 
# plot_mainresults + 
#   plot_layout(guides = "collect") +
#   plot_annotation(tag_levels = "a")