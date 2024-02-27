options(scipen = 0)

#setwd("/.Data/DetermineB")


library(ggplot2)
library(patchwork)
library(dplyr)
library(reshape2)

boot10 = readRDS("bootstraps_10.RData")
boot50 = readRDS("bootstraps_50.RData")
boot100 = readRDS("bootstraps_100.RData")
boot250 = readRDS("bootstraps_250.RData")
boot500 = readRDS("bootstraps_500.RData")
boot750 = readRDS("bootstraps_750.RData")
boot1000 = readRDS("bootstraps_1000.RData")


bias_10 = lapply(boot10, with, Bias)
var_10 = lapply(boot10, "[", 1:2)
bias_50 = lapply(boot50, with, Bias)
bias_100 = lapply(boot100, with, Bias)
bias_250 = lapply(boot250, with, Bias)
bias_500 = lapply(boot500, with, Bias)
bias_750 = lapply(boot750, with, Bias)
bias_1000 = lapply(boot1000, with, Bias)

bias_perSit_perM = 
  Map(cbind,
      var = var_10,
      bias_10 = bias_10,
      bias_50 = bias_50,
      bias_100 = bias_100,
      bias_250 = bias_250,
      bias_500 = bias_500,
      bias_750 = bias_750,
      bias_1000 = bias_1000)

common_instrumental = 
  as.data.frame(bias_perSit_perM$CommonInstrumental) %>% melt(id.vars = c("var.mediator", "var.outcome"))

common_mediator = 
  as.data.frame(bias_perSit_perM$CommonMediator) %>% melt(id.vars = c("var.instrumental", "var.outcome"))

common_outcome = 
  as.data.frame(bias_perSit_perM$CommonOutcome) %>% melt(id.vars = c("var.instrumental", "var.mediator"))

instrumental = aggregate(value ~ variable, common_instrumental, FUN = function(x) mean(abs(x)))
outcome = aggregate(value ~ variable, common_outcome, FUN = function(x) mean(abs(x)))
mediator = aggregate(value ~ variable, common_mediator, FUN = function(x) mean(abs(x)))

# plots

windowsFonts(Times = windowsFont("TT Times New Roman"))

general = 
  list(scale_x_discrete(expand = c(0,0),
                        labels = c("10", "50", "100", "250", "500", "750", "1000")),
       theme(panel.background = element_rect(fill = "gray98"),
             panel.grid.major = element_line(color = "gray95"),
             axis.line.x.bottom = element_line(color = "gray65"),
             axis.line.y.left = element_line(color = "gray65"),
             axis.text.x.bottom = element_text(size = 7),
             axis.title = element_text(size = 10),
             plot.subtitle = element_text(size = 12),
             text = element_text(family = "Times")))

common_instrumental_plot =
  ggplot(instrumental,
         mapping = aes(x = variable,
                       y = value,
                       group = 1)) +
  geom_line(lwd = 1,
            color = "#7570B3") +
  labs(subtitle = "Common Instrumental",
       x = "Number of bootstraps",
       y = "Bias") +
  scale_y_continuous(limits = c(.126, .129)) +
  general


common_mediator_plot = 
  ggplot(mediator,
         mapping = aes(x = variable,
                       y = value,
                       group = 1)) + 
  geom_line(lwd = 1,
            color = "#1B9E77") +
  labs(subtitle = "Common Mediator",
       x = "Number of bootstraps",
       y = "Bias")+
  scale_y_continuous(limits = c(.131, .134)) +
  general


common_outcome_plot = 
  ggplot(outcome,
         mapping = aes(x = variable,
                       y = value,
                       group = 1)) + 
  geom_line(lwd = 1,
            color = "#D95F02") +
  labs(subtitle = "Common Outcome",
       x = "Number of bootstraps",
       y = "Bias")+
  scale_y_continuous(limits = c(.045, .06)) +
  general

 common_mediator_plot / common_outcome_plot / common_instrumental_plot 

