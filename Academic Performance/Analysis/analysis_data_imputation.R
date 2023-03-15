# This script analyses data with imputation

# Set working directory
setwd("Z:/Roser/analyses_20220621_AcEF")

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
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

# Turn off scientific notation
options(scipen = 999)
options(max.print=1000000)

# Read in dataset
data_excel <- read_excel("data_T0-T1_imputed.xlsx")
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
setwd("Z:/Roser/analyses_20220621_AcEF/imputation")

# Define measures for loops
measures <- c('pHit','pStop','muSSRT','sigmaSSRT','tauSSRT','muGoRT','sigmaGoRT','tauGoRT',
              'academic_english_scores','academic_maths_scores','academic_total_scores','academic_time',
              'SSRT','FlankInh','Stroop','PBI','CogFlexRT','CogFlex','FlankSwitch','CorsiMaxWM','dprimeOneBack','dprimeTwoBack')
ac_measures <- c('academic_english_scores','academic_maths_scores','academic_total_scores')
ef_measures <- c('pHit','pStop','muSSRT','sigmaSSRT','tauSSRT','muGoRT','sigmaGoRT','tauGoRT',
                 'SSRT','FlankInh','Stroop','PBI','CogFlexRT','CogFlex','FlankSwitch','CorsiMaxWM','dprimeOneBack','dprimeTwoBack')

# Break down data for some analyses
dataT0 <- data[which(data$timepoint=='T0'),]
dataT1 <- data[which(data$timepoint=='T1'),]
dataC <- data[which(data$group=='control'),]
dataE <- data[which(data$group=='experimental'),]
dataT0C <- dataT0[which(dataT0$group=='control'),]
dataT0E <- dataT0[which(dataT0$group=='experimental'),]
dataT1C <- dataT1[which(dataT1$group=='control'),]
dataT1E <- dataT1[which(dataT1$group=='experimental'),]

# Training effects

## ef_measures
for(n in 1:length(ef_measures)){
    # Specify model
    model <- lmer(eval(parse(text=ef_measures[[n]])) ~ timepoint*group + (1|id), data = data, REML = TRUE)
    model_data <- emmeans(model, pairwise~timepoint*group, adjust = "bonferroni")
    model_means <- summary(model_data$emmeans)
    # Save output
    sink(paste("lmm_",ef_measures[[n]],"_time-group.txt",sep=""))
    print(anova(model))
    print(emmeans(model, pairwise~timepoint, adjust = "bonferroni"))
    print(emmeans(model, pairwise~group, adjust = "bonferroni"))
    print(emmeans(model, pairwise~timepoint*group, adjust = "bonferroni"))
    sink()
    # Plot training effects
    model_data_frame <- as.data.frame(model@frame)
    colnames(model_data_frame)[1] <- c(ef_measures[[n]])
    (plot <- ggplot(data = model_data_frame, aes(y = eval(parse(text=ef_measures[[n]])), x = group, fill = timepoint, color = timepoint)) +
            # Raw datapoints
            geom_point(aes(y = eval(parse(text=ef_measures[[n]])), x = group, color = timepoint), position = position_jitterdodge(dodge.width = .8, jitter.width = .15), size = 2, alpha = 0.2) +
            # Modelled mean
            geom_point(data = model_means, aes(y = emmean, x = group, color = timepoint), position = position_dodge(width = .8), size = 4) +
            # Modelled CI
            geom_errorbar(data = model_means, aes(y = emmean, x = group, color = timepoint, ymin = emmean-SE, ymax = emmean+SE), position = position_dodge(width = .8), width = 0.1, size = 1) +
            # Other features
            #expand_limits(x = 3) +
            theme_grey(base_size = 15) +
            scale_color_manual(values=c("lightseagreen", "tomato1")) +
            labs(y=paste(ef_measures[[n]],sep="")))
    ggsave(paste("lmm_",ef_measures[[n]],"_time-group.png",sep=""),
           width = 2665,height = 1484,units = c("px"))
}

## ac_measures
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

## ac_measures with actime
for(n in 1:length(ac_measures)){
    # Specify model
    model <- lmer(eval(parse(text=ac_measures[[n]])) ~ timepoint*group + academic_time + (1|id), data = data, REML = TRUE)
    model_data <- emmeans(model, pairwise~timepoint*group, adjust = "bonferroni")
    model_means <- summary(model_data$emmeans)
    # Save output
    sink(paste("lmm_",ac_measures[[n]],"_time-group-actime.txt",sep=""))
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
    ggsave(paste("lmm_",ac_measures[[n]],"_time-group-actime.png",sep=""),
           width = 2665,height = 1484,units = c("px"))
}

# Correlations between ac_measures & ef_measures at T0

## ac_measures - ef_measures
sink("corr_Ac-EF_T0_none.txt")
dataT0 %>%
    correlation(select = ac_measures,
                select2 = ef_measures, p_adjust = "none")
sink()
sink("corr_Ac-EF_T0_fdr.txt")
dataT0 %>%
    correlation(select = ac_measures,
                select2 = ef_measures, p_adjust = "fdr")
sink()
sink("corr_Ac-EF_T0_bonferroni.txt")
dataT0 %>%
    correlation(select = ac_measures,
                select2 = ef_measures, p_adjust = "bonferroni")
sink()

## ac_measures total - ef_measures
sink("corr_AcTotal-EF_T0_none.txt")
dataT0 %>%
    correlation(select = "academic_total_scores",
                select2 = ef_measures, p_adjust = "none")
sink()
sink("corr_AcTotal-EF_T0_fdr.txt")
dataT0 %>%
    correlation(select = "academic_total_scores",
                select2 = ef_measures, p_adjust = "fdr")
sink()
sink("corr_AcTotal-EF_T0_bonferroni.txt")
dataT0 %>%
    correlation(select = "academic_total_scores",
                select2 = ef_measures, p_adjust = "bonferroni")
sink()

## Plots ac_measures total & ef_measures
ef_measures_plot <- c('pHit','pStop','SSRT','muSSRT','sigmaSSRT','tauSSRT','muGoRT','sigmaGoRT','tauGoRT',
                      'FlankInh','Stroop','PBI','CogFlexRT','CogFlex','FlankSwitch','CorsiMaxWM','dprimeOneBack','dprimeTwoBack')
plotList <- list()
for(n in 1:length(ef_measures_plot)){
    plotName <- paste( 'p', n, sep = '' )
    (plotList[[plotName]] <- local({
        n <- n
        ggplot(dataT0, aes(y = academic_total_scores, x = eval(parse(text=ef_measures_plot[[n]])))) +
            geom_point(color = "lightseagreen", alpha = 0.2) +
            geom_smooth(method = "lm",color = "gray30") +
            labs(x=ef_measures_plot[[n]]) +
            theme_grey(base_size=15)
    }))
}
### ssrt measures
ggarrange(plotList$p1, plotList$p2, plotList$p3, plotList$p4, plotList$p5, plotList$p6, plotList$p7, plotList$p8, plotList$p9,
          ncol=3, nrow=3)
ggsave(paste("corr_AcTotal_EF-SSRT_T0.png",sep=""),
       width = 2665, height = 2500,units = c("px"))  
### ef measures
ggarrange(plotList$p10, plotList$p11, plotList$p12, plotList$p13, plotList$p14, plotList$p15, plotList$p16, plotList$p17, plotList$p18,
          ncol=3, nrow=3)
ggsave(paste("corr_AcTotal_EF-EF_T0.png",sep=""),
       width = 2665, height = 2500,units = c("px")) 
