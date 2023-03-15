# This script imputed data

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
library(miceadds)
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

# Turn off scientific notation
options(scipen = 999)
options(max.print=1000000)

# Read in dataset
data_excel <- read_excel("data_T0-T1.xlsx")
data <- as.data.frame(data_excel)
attach(data) #access variables in data

# Specify factors
data$id <- factor(data$id)
data$timepoint <- factor(data$timepoint)
data$group <- factor(data$group,
                        labels = c("control","experimental"),
                        levels = c("control", "experimental"))
data$gender <- factor(data$gender)

# Summary data
summary(data)

# Define measures for loops
measures <- c('pHit','pStop','muSSRT','sigmaSSRT','tauSSRT','muGoRT','sigmaGoRT','tauGoRT',
              'academic_english_scores','academic_maths_scores','academic_total_scores','academic_time',
              'SSRT','FlankInh','Stroop','PBI','CogFlexRT','CogFlex','FlankSwitch','CorsiMaxWM','dprimeOneBack','dprimeTwoBack')
tags <- c('A','B','C','D','E','F','G','H',
          'I','J','K','L',
          'M','N','O','P','Q','R','S','T','U','V')

# Check for outliers & replace by NA
for(n in 1:length(measures)){
  outlier_values <- boxplot.stats(eval(parse(text=paste("data","$",measures[[n]],sep=""))), coef = 2)$out # outlier values.
  assign(paste("outlier_rowID_",measures[[n]],sep=""),which(eval(parse(text=paste("data","$",measures[[n]],sep=""))) %in% c(outlier_values)))
  boxplot(eval(parse(text=paste("data","$",measures[[n]],sep=""))), main=measures[n], boxwex=0.1)
  mtext(paste("Outliers: ", paste(eval(parse(text=paste("outlier_rowID_",measures[[n]],sep=""))), collapse=", ")), cex=0.6)
  eval(parse(text=paste("data","$",measures[[n]],'[outlier_rowID_',measures[[n]],']<-NA',sep="")))
}

# Change working directory
setwd("Z:/Roser/analyses_20220621_AcEF/imputation")

# Checks on missing data
## Percentage missing data
sink("check_missing_data_percentage.txt")
### Whole dataset
print("Percentage missing data for whole dataset")
print(sum(is.na(data))/(nrow(data)*ncol(data)))
### Measure-wise
for(n in 1:length(measures)){
  print(paste("Percentage missing data for ",measures[[n]],sep=""))
  print(sum(is.na(eval(parse(text=paste("data","$",measures[[n]],sep="")))))/(nrow(data)))
}
sink()
## Pattern of missing data
missing_plot(data)
ggsave(paste("check_missing_data_map.png",sep=""),
       width = 7,height = 7)

# Impute data
set.seed(123)
ini <- mice(data, maxit=0, pri=F) #get predictor matrix
pred <- ini$predictorMatrix
pred[,c("id","gender","SES")] <- 0 #don't use as predictor
meth <- ini$method
meth[c("id","age","gender","SES","timepoint","group")] <- "" #don't impute these variables, may use only as predictors
imp <- mice(data, m=100, maxit=20, printFlag=TRUE, predictorMatrix=pred, method=meth) #impute data with 100 imputations and 20 iterations

# Change working directory
setwd("Z:/Roser/analyses_20220621_AcEF")

# Select a single dataset for post-hoc tests
implist <- mids2mitml.list(imp) #create a list of completed data sets
data.imp <- implist[[1]]
write_xlsx(data.imp,"data_T0-T1_imputed.xlsx") #save imputed dataset to use for analyses

# Change working directory
setwd("Z:/Roser/analyses_20220621_AcEF/imputation")

# Checks on missing/imputed data: check distribution of imputed data (red) is similar to observed data (blue)
## create function
fortify.mids <- function(x){
  imps <- do.call(rbind, lapply(seq_len(x$m), function(i){
    data.frame(complete(x, i), Imputation = i, Data = "imputed")
  }))
  orig <- cbind(x$data, Imputation = NA, Data = "observed")
  rbind(imps, orig)
}
## pooled: plot & save
plotList <- list()
for(n in 1:length(measures)){
  plotName <- paste( 'p', n, sep = '' )
  (plotList[[plotName]] <- local({
    n <- n
    ggplot(fortify.mids(imp), aes(x = eval(parse(text=measures[[n]])), colour = Data, group = Imputation)) +
      geom_density(size = 1) +
      scale_colour_manual(values = c(imputed = "lightsalmon2", observed = "dodgerblue1")) +
      labs(x=measures[[n]], tag=tags[[n]]) +
      theme_grey(base_size=15) +
      theme(legend.position="bottom")
  }))
}
thisLegend <- get_legend(plotList$p4)
ggarrange(plotList$p1, plotList$p2, plotList$p3, plotList$p4, plotList$p5, plotList$p6, plotList$p7, plotList$p8,
          plotList$p9, plotList$p10, plotList$p11, plotList$p12,
          plotList$p13, plotList$p14, plotList$p15, plotList$p16,plotList$p17, plotList$p18, plotList$p19, plotList$p20, plotList$p21, plotList$p22,
          ncol=5, nrow=5, common.legend = TRUE, legend="bottom", legend.grob = thisLegend)
ggsave("check_imputed_data_densplot.png",
       width = 3500, height = 3500,units = c("px"))
## single: plot & save
plotList <- list()
for(n in 1:length(measures)){
  plotName <- paste( 'p', n, sep = '' )
  (plotList[[plotName]] <- local({
    n <- n
    ggplot() +
      geom_density(aes(x = eval(parse(text=measures[[n]])), colour = "imputed"), alpha = .2, size = 1, data = data.imp) +
      geom_density(aes(x = eval(parse(text=measures[[n]])), colour = "observed"), , alpha = .2, size = 1, data = data) +
      scale_colour_manual(values = c(imputed = "lightsalmon2", observed = "dodgerblue1")) +
      labs(x=measures[[n]], tag=tags[[n]]) +
      theme_grey(base_size=15) +
      theme(legend.position="bottom")
  }))
}
thisLegend <- get_legend(plotList$p4)
ggarrange(plotList$p1, plotList$p2, plotList$p3, plotList$p4, plotList$p5, plotList$p6, plotList$p7, plotList$p8,
          plotList$p9, plotList$p10, plotList$p11, plotList$p12,
          plotList$p13, plotList$p14, plotList$p15, plotList$p16,plotList$p17, plotList$p18, plotList$p19, plotList$p20, plotList$p21, plotList$p22,
          ncol=5, nrow=5, common.legend = TRUE, legend="bottom", legend.grob = thisLegend)
ggsave("check_imputed_data_densplot_single.png",
       width = 3500, height = 3500,units = c("px"))

# Pooled stats for training effects

## Set working directory
setwd("Z:/Roser/analyses_20220621_AcEF/imputation")

## Subset dataset
dataT0 <- subset(implist, timepoint=='T0')
dataT1 <- subset(implist, timepoint=='T1')
dataC <- subset(implist, group=='control')
dataE <- subset(implist, group=='experimental')
dataT0C <- subset(dataT0, group=='control')
dataT0E <- subset(dataT0, group=='experimental')
dataT1C <- subset(dataT1, group=='control')
dataT1E <- subset(dataT1, group=='experimental')

## LMM

###  Define measures
ac_measures <- c('academic_english_scores','academic_maths_scores','academic_total_scores','academic_time')
ef_measures <- c('SSRT','FlankInh','Stroop','PBI','CogFlexRT','CogFlex','FlankSwitch','CorsiMaxWM','dprimeOneBack','dprimeTwoBack')

### Run LMM ef_measures
for(n in 1:length(ef_measures)){
  # Specify model
  model.null <- with(implist, lmer(eval(parse(text=ef_measures[[n]])) ~ 1 + (1|id), REML = TRUE))
  model.T <- with(implist, lmer(eval(parse(text=ef_measures[[n]])) ~ timepoint + (1|id), REML = TRUE))
  model.T.G <- with(implist, lmer(eval(parse(text=ef_measures[[n]])) ~ timepoint + group + (1|id), REML = TRUE))
  model.T.G.TG <- with(implist, lmer(eval(parse(text=ef_measures[[n]])) ~ timepoint*group + (1|id), REML = TRUE))
  model.T.G.TG.A <- with(implist, lmer(eval(parse(text=ef_measures[[n]])) ~ timepoint*group + age + (1|id), REML = TRUE))
  model.T.G.TG.A.TA <- with(implist, lmer(eval(parse(text=ef_measures[[n]])) ~ timepoint*group + timepoint*age + (1|id), REML = TRUE))
  model.T.G.TG.A.TA.GA <- with(implist, lmer(eval(parse(text=ef_measures[[n]])) ~ timepoint*group + timepoint*age + group*age + (1|id), REML = TRUE))
  model.T.G.TG.A.TA.GA.TGA <- with(implist, lmer(eval(parse(text=ef_measures[[n]])) ~ timepoint*group*age + (1|id), REML = TRUE))
  # Test effects & save output (higher R2 is better)
  sink(paste("pooled_lmm_",ef_measures[[n]],"_time-group-age-inter.txt",sep=""))
  print(testModels(model.T, model.null, method = "D1"))
  paste("AIC:", print(mean(sapply(model.T, AIC))))
  print(testModels(model.T.G, model.T, method = "D1"))
  paste("AIC:", print(mean(sapply(model.T.G, AIC))))
  print(testModels(model.T.G.TG, model.T.G, method = "D1"))
  paste("AIC:", print(mean(sapply(model.T.G.TG, AIC))))
  print(testModels(model.T.G.TG.A, model.T.G.TG, method = "D1"))
  paste("AIC:", print(mean(sapply(model.T.G.TG.A, AIC))))
  print(testModels(model.T.G.TG.A.TA, model.T.G.TG.A, method = "D1"))
  paste("AIC:", print(mean(sapply(model.T.G.TG.A.TA, AIC))))
  print(testModels(model.T.G.TG.A.TA.GA, model.T.G.TG.A.TA, method = "D1"))
  paste("AIC:", print(mean(sapply(model.T.G.TG.A.TA.GA, AIC))))
  print(testModels(model.T.G.TG.A.TA.GA.TGA, model.T.G.TG.A.TA.GA, method = "D1"))
  paste("AIC:", print(mean(sapply(model.T.G.TG.A.TA.GA.TGA, AIC))))
  sink()
}

### Run LMM ac_measures with actime
for(n in 1:(length(ac_measures)-1)){
  # Specify model
  model.null <- with(implist, lmer(eval(parse(text=ac_measures[[n]])) ~ 1 + (1|id), REML = TRUE))
  model.T <- with(implist, lmer(eval(parse(text=ac_measures[[n]])) ~ timepoint + (1|id), REML = TRUE))
  model.T.G <- with(implist, lmer(eval(parse(text=ac_measures[[n]])) ~ timepoint + group + (1|id), REML = TRUE))
  model.T.G.TG <- with(implist, lmer(eval(parse(text=ac_measures[[n]])) ~ timepoint*group + (1|id), REML = TRUE))
  model.T.G.TG.AcT <- with(implist, lmer(eval(parse(text=ac_measures[[n]])) ~ timepoint*group + academic_time + (1|id), REML = TRUE))
  model.T.G.TG.AcT.A <- with(implist, lmer(eval(parse(text=ac_measures[[n]])) ~ timepoint*group + academic_time + age + (1|id), REML = TRUE))
  model.T.G.TG.AcT.A.TA <- with(implist, lmer(eval(parse(text=ac_measures[[n]])) ~ timepoint*group + academic_time + timepoint*age + (1|id), REML = TRUE))
  model.T.G.TG.AcT.A.TA.GA <- with(implist, lmer(eval(parse(text=ac_measures[[n]])) ~ timepoint*group + academic_time + timepoint*age + group*age + (1|id), REML = TRUE))
  model.T.G.TG.AcT.A.TA.GA.TGA <- with(implist, lmer(eval(parse(text=ac_measures[[n]])) ~ timepoint*group*age + academic_time + (1|id), REML = TRUE))
  # Test effects & save output (higher R2 is better)
  sink(paste("pooled_lmm_",ac_measures[[n]],"_time-group-actime-age-inter.txt",sep=""))
  print(testModels(model.T, model.null, method = "D1"))
  paste("AIC:", print(mean(sapply(model.T, AIC))))
  print(testModels(model.T.G, model.T, method = "D1"))
  paste("AIC:", print(mean(sapply(model.T.G, AIC))))
  print(testModels(model.T.G.TG, model.T.G, method = "D1"))
  paste("AIC:", print(mean(sapply(model.T.G.TG, AIC))))
  print(testModels(model.T.G.TG.AcT, model.T.G.TG, method = "D1"))
  paste("AIC:", print(mean(sapply(model.T.G.TG.AcT, AIC))))
  print(testModels(model.T.G.TG.AcT.A, model.T.G.TG.AcT, method = "D1"))
  paste("AIC:", print(mean(sapply(model.T.G.TG.AcT.A, AIC))))
  print(testModels(model.T.G.TG.AcT.A.TA, model.T.G.TG.AcT.A, method = "D1"))
  paste("AIC:", print(mean(sapply(model.T.G.TG.AcT.A.TA, AIC))))
  print(testModels(model.T.G.TG.AcT.A.TA.GA, model.T.G.TG.AcT.A.TA, method = "D1"))
  paste("AIC:", print(mean(sapply(model.T.G.TG.AcT.A.TA.GA, AIC))))
  print(testModels(model.T.G.TG.AcT.A.TA.GA.TGA, model.T.G.TG.AcT.A.TA.GA, method = "D1"))
  paste("AIC:", print(mean(sapply(model.T.G.TG.AcT.A.TA.GA.TGA, AIC))))
  sink()
}

### Run LMM ac_measures with actime FIRST
for(n in 1:(length(ac_measures)-1)){
  # Specify model
  model.null <- with(implist, lmer(eval(parse(text=ac_measures[[n]])) ~ 1 + (1|id), REML = TRUE))
  model.AcT <- with(implist, lmer(eval(parse(text=ac_measures[[n]])) ~ academic_time + (1|id), REML = TRUE))
  model.AcT.TG <- with(implist, lmer(eval(parse(text=ac_measures[[n]])) ~ academic_time + timepoint*group + (1|id), REML = TRUE))
  # Test effects & save output (higher R2 is better)
  sink(paste("pooled_lmm_",ac_measures[[n]],"_actime-time-group-age-inter.txt",sep=""))
  print(testModels(model.AcT, model.null, method = "D1"))
  paste("AIC:", print(mean(sapply(model.AcT, AIC))))
  print(testModels(model.AcT.TG, model.AcT, method = "D1"))
  paste("AIC:", print(mean(sapply(model.AcT.TG, AIC))))
  sink()
}

## Correlations between ac_measures & ef_measures at T0

### Set contrasts to average=0 to get main effects in summery
for(n in 1:100){
  contrasts(dataT0[[n]]$group) <- c(-.5,.5)
  contrasts(dataT1[[n]]$group) <- c(-.5,.5)
}

### Pearson corr
sink("pooled_corr_Ac-EF_T0.txt")
corr <- micombine.cor(mi.res=dataT0, variables=c(7:28))
print(corr)
sink()

### LM with AcTotal
sink("pooled_corr-lm_AcTotal-EF_T0.txt")
#AcTotal-pHit
print("AcTotal-pHit")
model <- with(dataT0, lm(academic_total_scores ~ pHit))
print(summary(pool(model)))
#AcTotal-pStop
print("AcTotal-pStop")
model <- with(dataT0, lm(academic_total_scores ~ pStop))
print(summary(pool(model)))
#AcTotal-muSSRT
print("AcTotal-muSSRT")
model <- with(dataT0, lm(academic_total_scores ~ muSSRT))
print(summary(pool(model)))
#AcTotal-sigmaSSRT
print("AcTotal-sigmaSSRT")
model <- with(dataT0, lm(academic_total_scores ~ sigmaSSRT))
print(summary(pool(model)))
#AcTotal-tauSSRT
print("AcTotal-tauSSRT")
model <- with(dataT0, lm(academic_total_scores ~ tauSSRT))
print(summary(pool(model)))
#AcTotal-muGoRT
print("AcTotal-muGoRT")
model <- with(dataT0, lm(academic_total_scores ~ muGoRT))
print(summary(pool(model)))
#AcTotal-sigmaGoRT
print("AcTotal-sigmaGoRT")
model <- with(dataT0, lm(academic_total_scores ~ sigmaGoRT))
print(summary(pool(model)))
#AcTotal-tauGoRT
print("AcTotal-tauGoRT")
model <- with(dataT0, lm(academic_total_scores ~ tauGoRT))
print(summary(pool(model)))
#AcTotal-SSRT
print("AcTotal-SSRT")
model <- with(dataT0, lm(academic_total_scores ~ SSRT))
print(summary(pool(model)))
#AcTotal-FlankInh
print("AcTotal-FlankInh")
model <- with(dataT0, lm(academic_total_scores ~ FlankInh))
print(summary(pool(model)))
#AcTotal-Stroop
print("AcTotal-Stroop")
model <- with(dataT0, lm(academic_total_scores ~ Stroop))
print(summary(pool(model)))
#AcTotal-PBI
print("AcTotal-PBI")
model <- with(dataT0, lm(academic_total_scores ~ PBI))
print(summary(pool(model)))
#AcTotal-CogFlexRT
print("AcTotal-CogFlexRT")
model <- with(dataT0, lm(academic_total_scores ~ CogFlexRT))
print(summary(pool(model)))
#AcTotal-CogFlex
print("AcTotal-CogFlex")
model <- with(dataT0, lm(academic_total_scores ~ CogFlex))
print(summary(pool(model)))
#AcTotal-FlankSwitch
print("AcTotal-FlankSwitch")
model <- with(dataT0, lm(academic_total_scores ~ FlankSwitch))
print(summary(pool(model)))
#AcTotal-CorsiMaxWM
print("AcTotal-CorsiMaxWM")
model <- with(dataT0, lm(academic_total_scores ~ CorsiMaxWM))
print(summary(pool(model)))
#AcTotal-dprimeOneBack
print("AcTotal-dprimeOneBack")
model <- with(dataT0, lm(academic_total_scores ~ dprimeOneBack))
print(summary(pool(model)))
#AcTotal-dprimeTwoBack
print("AcTotal-dprimeTwoBack")
model <- with(dataT0, lm(academic_total_scores ~ dprimeTwoBack))
print(summary(pool(model)))
sink()
