# This file contains plot construction of figure 4.2

library(ggplot2)
library(patchwork)


setwd("./Data")

results = 
  readRDS("PlotData_Estimates.RData")

results$combination[results$combination == "M1I1"] <- "I1M1"
results$combination[results$combination == "M2I1"] <- "I1M2"
results$combination[results$combination == "M1I2"] <- "I2M1"
results$combination[results$combination == "M2I2"] <- "I2M2"

label_situation = 
  c("Mediator",
    "Outcome",
    "Instrumental")

names(label_situation) = unique(results$situation)

results$situation_factor = factor(results$situation,
                                  levels = c("Mediator",
                                             "Outcome",
                                             "Instrumental"),
                                  labels = c("Mediator (scenario 1)",
                                             "Outcome (scenario 2)",
                                             "Instrumental (scenario 3)"))

results$combination <- factor(results$combination, 
                              levels = sapply(unique(results$combination), toString)[c(5:12, 1:4)])


windowsFonts(Times = windowsFont("TT Times New Roman"))

general = 
  list(geom_crossbar(mapping = aes(y = (conditional_lower + conditional_upper)/2,
                                   ymin = conditional_lower,
                                   ymax = conditional_upper),
                     fill = "gray85",
                     color = "gray85",
                     width = .35,
                     alpha = .7),
       geom_point(size = 1.5,
                  position = position_dodge(width = .5)),
       geom_point(mapping = aes(y = population_value,
                                x = combination),
                  shape = 18,
                  color = "black",
                  size = 1),
       geom_errorbar(mapping = aes(ymin = lower_CI,
                                   ymax = upper_CI,
                                   width = .15),
                     position = position_dodge(width = 0.5)),
       theme(panel.background = element_rect(fill = "gray98"),
             panel.grid.major = element_line(color = "gray95"),
             axis.line.x.bottom = element_line(color = "gray65"),
             axis.line.y.left = element_line(color = "gray65"),
             axis.text.x.bottom = element_text(size = 7),
             axis.title = element_text(size = 10),
             plot.subtitle = element_text(size = 12),
             text = element_text(family = "Times")),
       scale_y_continuous(limits = c(0,.8)),
       scale_x_discrete(guide = guide_axis(angle = 60)),
       scale_color_discrete(type = RColorBrewer::brewer.pal(3, "Dark2")),
       scale_shape_manual(values = c(0,2)))


pop1 = 
  ggplot(data = results[results$population == 1,],
         mapping = aes(y = mean,
                       x = combination,
                       color = situation_factor,
                       shape = factor(method, 
                                      levels = c("IVA", "CIA"),
                                      labels = c("Instrumental Variable Assumption", 
                                                 "Conditional Independence Assumption")),
                       group = factor(method, 
                                      levels = c("IVA", "CIA"),
                                      labels = c("Instrumental Variable Assumption", 
                                                 "Conditional Independence Assumption")))) +
  labs(y = "Estimate",
       x = NULL,
       color = "Overlapping variable (scenario)",
       shape = "Assumption",
       subtitle = "No violation") +
  general

pop2 = 
  ggplot(data = results[results$population == 2,],
         mapping = aes(y = mean,
                       x = combination,
                       color = situation_factor,
                       shape = factor(method, 
                                      levels = c("IVA", "CIA"),
                                      labels = c("Instrumental Variable Assumption", 
                                                 "Conditional Independence Assumption")),
                       group = factor(method, 
                                      levels = c("IVA", "CIA"),
                                      labels = c("Instrumental Variable Assumption", 
                                                 "Conditional Independence Assumption"))))+
  labs(y = NULL,
       x = "Category combination",
       color = "Overlapping variable (scenario)",
       shape = "Assumption",
       subtitle = "Slight type A violation") +
  general


pop3 = 
  ggplot(data = results[results$population == 3,],
         mapping = aes(y = mean,
                       x = combination,
                       color = situation_factor,
                       shape = factor(method, 
                                      levels = c("IVA", "CIA"),
                                      labels = c("Instrumental Variable Assumption", 
                                                 "Conditional Independence Assumption")),
                       group = factor(method, 
                                      levels = c("IVA", "CIA"),
                                      labels = c("Instrumental Variable Assumption", 
                                                 "Conditional Independence Assumption"))))+
  labs(y = "Estimate",
       x = "Category combination",
       color = "Overlapping variable (scenario)",
       shape = "Assumption",
       subtitle = "Severe type A violation") +
  general

pop4 = 
  ggplot(data = results[results$population == 4,],
         mapping = aes(y = mean,
                       x = combination,
                       color = situation_factor,
                       shape = factor(method, 
                                      levels = c("IVA", "CIA"),
                                      labels = c("Instrumental Variable Assumption", 
                                                 "Conditional Independence Assumption")),
                       group = factor(method, 
                                      levels = c("IVA", "CIA"),
                                      labels = c("Instrumental Variable Assumption", 
                                                 "Conditional Independence Assumption")))) +
  labs(y = NULL,
       x = NULL,
       color = "Overlapping variable (scenario)",
       shape = "Assumption",
       subtitle = "Type B violation") +
  general



pop5 = 
  ggplot(data = results[results$population == 5,],
         mapping = aes(y = mean,
                       x = combination,
                       color = situation_factor,
                       shape = factor(method, 
                                      levels = c("IVA", "CIA"),
                                      labels = c("Instrumental Variable Assumption", 
                                                 "Conditional Independence Assumption")),
                       group = factor(method, 
                                      levels = c("IVA", "CIA"),
                                      labels = c("Instrumental Variable Assumption", 
                                                 "Conditional Independence Assumption")))) +
  labs(y = NULL,
       color = "Overlapping variable (scenario)",
       shape = "Assumption",
       x = "Category combination",
       subtitle = "Type B and slight type A violation") +
  general


result_plot = 
  pop1 + pop4 + pop2 + pop3 + pop5 + guide_area()

# note = expression(paste(italic("Note:"), "Grey areas represent conditional Fréchèt bounds for that specific category combination."))

result_plot + 
  plot_layout(guides = "collect") 



# 
# plot_annotation(title = "Figure 4.3",
#                 subtitle = "Simulated estimated probabilities and their 95% confidence intervals for each category combination for all populations, for each method and scenario",
#                 caption = note)&
#   theme(text = element_text(family = "Times"),
#         plot.title = element_text(face = "bold", size = 12),
#         plot.subtitle = element_text(face = "italic", size = 11),
#         plot.caption.position = "panel",
#         plot.caption = element_text(hjust = 0))