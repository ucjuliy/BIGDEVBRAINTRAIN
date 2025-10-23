# This script analyses data with imputation

# Set working directory
setwd("Z:/Roser/Papers/academic_ef/Analysis")

# Load packages
library(tidyverse)
library(readxl)
library(writexl)
library(emmeans)
library(lme4)
library(lmerTest)
library(MuMIn)
library(effectsize)
library(ggeffects)
library(broom.mixed)
library(brms)
library(mice)
library(reshape2)
library(summarytools)
library(GGally)
library(finalfit)
library(VIM)
library(RColorBrewer)
library(readr)
library(Hmisc)
library(plyr)
library(AICcmodavg)
library(correlation)
library(dplyr)
library(ppcor)
library(mitml)
library(multcomp)
library(parameters)
library(contrast)
library(patchwork)
library(ggpubr)
library(plotrix)
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

# Turn off scientific notation
options(scipen = 999)
options(max.print=1000000)

# Read in dataset
data_excel <- read_excel("data_factor_T0-T1_imputed.xlsx")
data <- as.data.frame(data_excel)
attach(data) #access variables in data

# Specify factors
data$id <- factor(data$id)
data$timepoint <- factor(data$timepoint)
data$group <- factor(data$group,
                     labels = c("control","experimental"),
                     levels = c("control", "experimental"))
data$gender <- factor(data$gender)

# Change working directory
setwd("Z:/Roser/Papers/academic_ef/Analysis/imputation_factor")

# Define measures for loops
measures <- c('inhibition_factor_T0','switch_factor_T0','memory_factor_T0','academic_total_scores')
ac_measures <- c('academic_total_scores')
ef_measures <- c('inhibition_factor_T0','switch_factor_T0','memory_factor_T0')

# Break down data for some analyses
dataT0 <- data[which(data$timepoint=='T0'),]
# dataT0Con <- dataT0[which(dataT0$group=='control'),]
# dataT0Exp <- dataT0[which(dataT0$group=='experimental'),]

# Correlations at T0

## Academic performance and EF
sink("corr-lm_Ac-EF_T0.txt")
#AcTotal-inhibition_factor
print("AcTotal-inhibition_factor")
model <- lm(academic_total_scores ~ inhibition_factor_T0, data = dataT0)
print(summary(model))
#AcTotal-switch_factor
print("AcTotal-switch_factor")
model <- lm(academic_total_scores ~ switch_factor_T0, data = dataT0)
print(summary(model))
#AcTotal-memory_factor
print("AcTotal-memory_factor")
model <- lm(academic_total_scores ~ memory_factor_T0, data = dataT0)
print(summary(model))
sink()

## Academic performance and EF with SES, IQ
sink("corr-lm_Ac-EF-SES-IQ_T0.txt")
#AcTotal-inhibition_factor
print("AcTotal-inhibition_factor")
model <- lm(academic_total_scores ~ inhibition_factor_T0 + SES + IQ, data = dataT0)
print(summary(model))
#AcTotal-switch_factor
print("AcTotal-switch_factor")
model <- lm(academic_total_scores ~ switch_factor_T0 + SES + IQ, data = dataT0)
print(summary(model))
#AcTotal-memory_factor
print("AcTotal-memory_factor")
model <- lm(academic_total_scores ~ memory_factor_T0 + SES + IQ, data = dataT0)
print(summary(model))
sink()

## EF with age, SES, IQ
sink("corr-lm_EF-age-SES-IQ_T0.txt")
#AcTotal-inhibition_factor
print("AcTotal-inhibition_factor")
model <- lm(inhibition_factor_T0 ~ age + SES + IQ, data = dataT0)
print(summary(model))
#AcTotal-switch_factor
print("AcTotal-switch_factor")
model <- lm(switch_factor_T0 ~ age + SES + IQ, data = dataT0)
print(summary(model))
#AcTotal-memory_factor
print("AcTotal-memory_factor")
model <- lm(memory_factor_T0 ~ age + SES + IQ, data = dataT0)
print(summary(model))
sink()

## Plots ac_measures total & ef_measures
plotList <- list()
for(n in 1:length(ef_measures)){
    plotName <- paste( 'p', n, sep = '' )
    (plotList[[plotName]] <- local({
        n <- n
        ggplot(dataT0, aes(y = academic_total_scores, x = eval(parse(text=ef_measures[[n]])))) +
            geom_point(color = "lightseagreen", alpha = 0.2) +
            geom_smooth(method = "lm",color = "gray30") +
            labs(x=ef_measures[[n]]) +
            theme_grey(base_size=15)
    }))
}
ggarrange(plotList$p1, plotList$p2, plotList$p3,
          ncol=3, nrow=1)
ggsave(paste("corr_AcTotal_EF_T0.png",sep=""),
       width = 2500, height = 900,units = c("px"))

# Training effects on academic performance

for(n in 1:length(ac_measures)){
    # Specify model
    model <- lmer(eval(parse(text=ac_measures[[n]])) ~ timepoint*group + (1|id), data = data, REML = TRUE)
    model_data <- emmeans(model, pairwise~timepoint*group, adjust = "bonferroni")
    model_means <- summary(model_data$emmeans)
    # Save output
    sink(paste("lmm_",ac_measures[[n]],"_time-group.txt",sep=""))
    print(anova(model))
    print(emmeans(model, pairwise~timepoint, adjust = "bonferroni"))
    print(emmeans(model, pairwise~group, adjust = "bonferroni"))
    print(emmeans(model, pairwise~timepoint*group, adjust = "bonferroni"))
    sink()
    # Plot training effects
    model_data_frame <- as.data.frame(model@frame)
    colnames(model_data_frame)[1] <- c(ac_measures[[n]])
    (plot <- ggplot(data = model_data_frame, aes(y = eval(parse(text=ac_measures[[n]])), x = group, fill = timepoint, color = timepoint)) +
            # Raw datapoints
            geom_point(aes(y = eval(parse(text=ac_measures[[n]])), x = group, color = timepoint), position = position_jitterdodge(dodge.width = .8, jitter.width = .15), size = 2, alpha = 0.2) +
            # Modelled mean
            geom_point(data = model_means, aes(y = emmean, x = group, color = timepoint), position = position_dodge(width = .8), size = 4) +
            # Modelled CI
            geom_errorbar(data = model_means, aes(y = emmean, x = group, color = timepoint, ymin = emmean-SE, ymax = emmean+SE), position = position_dodge(width = .8), width = 0.1, size = 1) +
            # Other features
            #expand_limits(x = 3) +
            theme_grey(base_size = 15) +
            scale_color_manual(values=c("lightseagreen", "tomato1","orange1")) +
            labs(y=paste(ac_measures[[n]],sep="")))
    ggsave(paste("lmm_",ac_measures[[n]],"_time-group.png",sep=""),
           width = 2665,height = 1484,units = c("px"))
}

# Training effects on academic performance with SES, IQ

for(n in 1:length(ac_measures)){
    # Specify model
    model <- lmer(eval(parse(text=ac_measures[[n]])) ~ timepoint*group + SES + IQ + (1|id), data = data, REML = TRUE)
    model_data <- emmeans(model, pairwise~timepoint*group, adjust = "bonferroni")
    model_means <- summary(model_data$emmeans)
    # Save output
    sink(paste("lmm_",ac_measures[[n]],"_time-group-SES-IQ.txt",sep=""))
    print(anova(model))
    print(emmeans(model, pairwise~timepoint, adjust = "bonferroni"))
    print(emmeans(model, pairwise~group, adjust = "bonferroni"))
    print(emmeans(model, pairwise~timepoint*group, adjust = "bonferroni"))
    sink()
    # Plot training effects
    model_data_frame <- as.data.frame(model@frame)
    colnames(model_data_frame)[1] <- c(ac_measures[[n]])
    (plot <- ggplot(data = model_data_frame, aes(y = eval(parse(text=ac_measures[[n]])), x = group, fill = timepoint, color = timepoint)) +
            # Raw datapoints
            geom_point(aes(y = eval(parse(text=ac_measures[[n]])), x = group, color = timepoint), position = position_jitterdodge(dodge.width = .8, jitter.width = .15), size = 2, alpha = 0.2) +
            # Modelled mean
            geom_point(data = model_means, aes(y = emmean, x = group, color = timepoint), position = position_dodge(width = .8), size = 4) +
            # Modelled CI
            geom_errorbar(data = model_means, aes(y = emmean, x = group, color = timepoint, ymin = emmean-SE, ymax = emmean+SE), position = position_dodge(width = .8), width = 0.1, size = 1) +
            # Other features
            #expand_limits(x = 3) +
            theme_grey(base_size = 15) +
            scale_color_manual(values=c("lightseagreen", "tomato1","orange1")) +
            labs(y=paste(ac_measures[[n]],sep="")))
    ggsave(paste("lmm_",ac_measures[[n]],"_time-group-SES-IQ.png",sep=""),
           width = 2665,height = 1484,units = c("px"))
}

