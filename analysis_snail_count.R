#' ---
#' title: Linear model snail count data
#' author: Bribiesca J., Terui, A. & Kim, S.
#' output:
#'   html_document:
#'     theme: paper
#'     toc: true
#'     toc_float: true
#'     number_sections: TRUE
#' ---


#' # Load package -----
rm(list = ls())
library(tidyverse)
library(patchwork)
library(MASS)
library(PerformanceAnalytics)
library(jtools)
library(lmerTest)
library(optimx) # this is to optimize convergence in lmer
library(moonBook)
library(ggpubr)
library(effects)

#' # Data -----
df_snail <- read_csv("./data_format/proj_snail_count_env_20210823.csv")
df_snail$X1 <- NULL
df_snail$Occasion <- as.factor(df_snail$Occasion) # Make occasion as factor
df_snail$Transect <- as.factor(df_snail$Transect) # Make Transect as factor
df_snail$Quadrat <- as.factor(df_snail$Quadrat) # Make Quadrat as factor


#' # Summary of data -----

#'- Overall table
mytable(df_snail, digits = 2)

#'- Table by occasion
mytable(Occasion~ Width + Depth + Velocity + per_BR + per_BO + per_CO + per_PE + per_GV + per_SD + per_SL + coarseness,data= df_snail, digits = 2)



#' # Analysis -----

#' ## Correlation plot - target environmental variables and count data
  chart.Correlation(df_snail[, c(24, 4:6, 22)], method="spearman", histogram=TRUE, cex = 10) # we do not need to worry about multicolinearity.
  
  
#' ## Linear model
#'- Simple linear model
  mod01 <- lm(Count ~ scale(Depth) + scale(Velocity) + scale(coarseness), data= df_snail)
  summary(mod01)

  
#' ## Poisson GLM
  mod02 <- glm(Count ~ scale(Depth) + scale(Velocity) + scale(coarseness), data= df_snail, family= "poisson")
  summary(mod02) # heavy overdispersion

 #'- Check the frequency of count data
  ggplot(data= df_snail, aes(Count)) + 
    geom_histogram(binwidth = 2)

  
#' ## Negative binomial (NBGLM)
  mod03 <-glm.nb(Count ~ scale(Depth) + scale(Velocity) + scale(coarseness), data= df_snail)
  summary(mod03) # overdispersion issue resolved
  
 # Regression plot
  effect_plot(model = mod03, pred = Depth, interval = TRUE,int.type = "confidence", int.width = .95, plot.points=TRUE, 
              data = df_snail, x.label= "Depth (cm)", y.label= "Count")
  
  effect_plot(model = mod03, pred = coarseness, interval = TRUE,int.type = "confidence", int.width = .95, plot.points=TRUE, 
              data = df_snail, x.label= "Coarseness", y.label= "Count")

  
#' ## NBGLM with Occasion as factor
  mod04 <-glm.nb(Count ~ scale(Depth) + scale(Velocity) + scale(coarseness) + Occasion, data= df_snail)
  summary(mod04)
  
  
#' ## NBGLMM using Transect as a random effect
  mod05 <- glmer.nb(Count ~ scale(Depth) + scale(Velocity) + scale(coarseness) + (1|Transect) + Occasion, data=df_snail, control = glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))
  summary(mod05)
  
#'- Regression plot
  effect_plot(model = mod05, pred = Depth, interval = TRUE,int.type = "confidence", int.width = .95, plot.points= TRUE, 
              data = df_snail, x.label= "Depth (cm)", y.label= "Count")
  
  effect_plot(model = mod05, pred = coarseness, interval = TRUE,int.type = "confidence", int.width = .95, plot.points=TRUE, 
              data = df_snail, x.label= "Coarseness", y.label= "Count")

  
#'- Regression plot using ggplot2

# Extract effect size suing package effects   
  mod05_dep <- effect("scale(Depth)", mod = mod05, xlevels = list(Depth = seq(0, 31, length.out = 50)))
  mod05_dep <- as.data.frame(mod05_dep)
  mod05_cor <- effect("scale(coarseness)", mod = mod05, xlevels = list(coarseness = seq(1, 7, length.out = 50)))
  mod05_cor <- as.data.frame(mod05_cor)
  
  dep_fig <- ggplot() +
    geom_ribbon(data = mod05_dep, aes(x = Depth, ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line(data = mod05_dep, aes(x = Depth, y = fit), size = 1) +
    geom_point(data = df_snail, aes(x = Depth, y = Count), shape=21, colour="black") +
    scale_y_continuous("Number of snails", limits=c(0,80), breaks = seq(0, 80, 20)) +
    scale_x_continuous("Depth (cm)",  breaks = seq(0, 30, 5)) +     
    theme_bw() +
    theme(text=element_text(face="bold", size=8),  
          legend.position = "none",                     # remove legend panel
          panel.border=element_rect(colour='black'),
          panel.grid.major=element_line(colour=NA),
          panel.grid.minor=element_line(colour=NA),
          axis.title.y = element_text(size = rel(1.5)),
          axis.title.x = element_text(size = rel(1.5)),
          axis.text.x = element_text(size = rel(1.5)),
          axis.text.y = element_text(size = rel(1.5)))  # remove grid
  theme(panel.background = element_rect(fill = "white")) 

  
  cor_fig <- ggplot() +
    geom_ribbon(data = mod05_cor, aes(x = coarseness, ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line(data = mod05_cor, aes(x = coarseness, y = fit), size = 1) +
    geom_point(data = df_snail, aes(x = coarseness, y = Count), shape=21, colour="black") +
    scale_y_continuous("Number of snails", limits=c(0,80), breaks = seq(0, 80, 20)) +
    scale_x_continuous("Coarseness",  breaks = seq(1, 7, 2)) +     
    theme_bw() +
    theme(text=element_text(face="bold", size=8),  
          legend.position = "none",                     # remove legend panel
          panel.border=element_rect(colour='black'),
          panel.grid.major=element_line(colour=NA),
          panel.grid.minor=element_line(colour=NA),
          axis.title.y = element_text(size = rel(1.5)),
          axis.title.x = element_text(size = rel(1.5)),
          axis.text.x = element_text(size = rel(1.5)),
          axis.text.y = element_text(size = rel(1.5)))  # remove grid
  theme(panel.background = element_rect(fill = "white")) 

  
# Combine figures
  ggarrange(dep_fig, cor_fig,
            ncol = 2, nrow = 1, align = "hv")
  
  