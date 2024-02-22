options(scipen = 0)

library(dplyr)
library(reshape2)
library(ggplot2)
library(patchwork)

setwd("F:/Documents/Thesis_StatisticalMatching/R/DetermineM")

res10 = readRDS("res10.RData")
res50 = readRDS("res50.RData")
res100 = readRDS("res100.RData")
res250 = readRDS("res250.RData")
res500 = readRDS("res500.RData")
res750 = readRDS("res750.RData")
res1000 = readRDS("res1000.RData")
res5000 = readRDS("res5000.RData")
res10000 = readRDS("res10000.RData")
res100000 = readRDS("res10000.RData")

bias_10 = lapply(res10$EstimationError, with, Bias)
var_10 = lapply(res10$EstimationError, "[", 1:2)
bias_50 = lapply(res50$EstimationError, with, Bias)
bias_100 = lapply(res100$EstimationError, with, Bias)
bias_250 = lapply(res250$EstimationError, with, Bias)
bias_500 = lapply(res500$EstimationError, with, Bias)
bias_750 = lapply(res750$EstimationError, with, Bias)
bias_1000 = lapply(res1000$EstimationError, with, Bias)
bias_5000 = lapply(res5000$EstimationError, with, Bias)
bias_10000 = lapply(res10000$EstimationError, with, Bias)
bias_100000 = lapply(res100000$EstimationError, with, Bias)

bias_perSit_perM = 
  Map(cbind,
      var = var_10,
      bias_10 = bias_10,
      bias_50 = bias_50,
      bias_100 = bias_100,
      bias_250 = bias_250,
      bias_500 = bias_500,
      bias_750 = bias_750,
      bias_1000 = bias_1000,
      bias_5000 = bias_5000,
      bias_10000 = bias_10000,
      bias_100000 = bias_100000)

common_instrumental = 
  as.data.frame(bias_perSit_perM$CommonInstrumental) %>% melt(id.vars = c("var.mediator", "var.outcome"))

common_mediator = 
  as.data.frame(bias_perSit_perM$CommonMediator) %>% melt(id.vars = c("var.instrumental", "var.outcome"))

common_outcome = 
  as.data.frame(bias_perSit_perM$CommonOutcome) %>% melt(id.vars = c("var.instrumental", "var.mediator"))

windowsFonts(Times = windowsFont("TT Times New Roman"))

general = 
  list(scale_x_discrete(expand = c(0,0),
                        labels = c("10", "50", "100", "250", "500", "750", "1000", "5000", "10000", "100000")),
       theme(panel.background = element_rect(fill = "gray98"),
             panel.grid.major = element_line(color = "gray95"),
             axis.line.x.bottom = element_line(color = "gray65"),
             axis.line.y.left = element_line(color = "gray65"),
             axis.text.x.bottom = element_text(size = 7),
             axis.title = element_text(size = 10),
             plot.subtitle = element_text(size = 12),
             text = element_text(family = "Times")))


instrumental = aggregate(value ~ variable, common_instrumental, mean)
outcome = aggregate(value ~ variable, common_outcome, mean)
mediator = aggregate(value ~ variable, common_mediator, mean)

common_instrumental_plot = 
  ggplot(instrumental,
         mapping = aes(x = variable,
                       y = value,
                       group = 1)) + 
  geom_line(lwd = 1,
            color = "#7570B3") +
  labs(subtitle = "Common Instrumental",
       x = "Number of simulations",
       y = "Bias") +
  scale_y_continuous(limits = c(-4*1e-17, -1*1e-17),
                     breaks = c(-4*1e-17, -2.5*1e-17, -1*1e-17)) +
  general


common_outcome_plot = 
  ggplot(outcome,
         mapping = aes(x = variable,
                       y = value,
                       group = 1)) + 
  geom_line(lwd = 1,
            color = "#D95F02") +
  labs(subtitle = "Common Outcome",
       x = "Number of simulations",
       y = "Bias") +
  scale_y_continuous(limits = c(-2*1e-17, -1*1e-18),
                     breaks = c(-2*1e-17, -1*1e-17, -1*1e-18)) +
  general

common_mediator_plot = 
  ggplot(mediator,
         mapping = aes(x = variable,
                       y = value,
                       group = 1)) + 
  geom_line(lwd = 1,
            color = "#1B9E77") +
  labs(subtitle = "Common Mediator",
       x = "Number of simulations",
       y = "Bias")  +
  scale_y_continuous(limits = c(-2*1e-17, 1*1e-17),
                     breaks = c(-2*1e-17, -.5*1e-17, 1*1e-17)) +
  general



  common_mediator_plot / common_outcome_plot / common_instrumental_plot 
