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
data_excel <- read_excel("data_factor_T0.xlsx")
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
measures <- c('academic_english_scores','academic_maths_scores','academic_total_scores',
              'inhibition_factor','switch_factor','memory_factor')
tags <- c('A','B','C','D','E','F')

# Check for outliers & replace by NA
for(n in 1:length(measures)){
  outlier_values <- boxplot.stats(eval(parse(text=paste("data","$",measures[[n]],sep=""))), coef = 2)$out # outlier values.
  assign(paste("outlier_rowID_",measures[[n]],sep=""),which(eval(parse(text=paste("data","$",measures[[n]],sep=""))) %in% c(outlier_values)))
  boxplot(eval(parse(text=paste("data","$",measures[[n]],sep=""))), main=measures[n], boxwex=0.1)
  mtext(paste("Outliers: ", paste(eval(parse(text=paste("outlier_rowID_",measures[[n]],sep=""))), collapse=", ")), cex=0.6)
  eval(parse(text=paste("data","$",measures[[n]],'[outlier_rowID_',measures[[n]],']<-NA',sep="")))
}

# Change working directory
setwd("Z:/Roser/analyses_20220621_AcEF/imputation_factor")

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
write_xlsx(data.imp,"data_factor_T0_imputed.xlsx") #save imputed dataset to use for analyses

# Change working directory
setwd("Z:/Roser/analyses_20220621_AcEF/imputation_factor")

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
ggarrange(plotList$p1, plotList$p2, plotList$p3, plotList$p4, plotList$p5, plotList$p6,
          ncol=3, nrow=2, common.legend = TRUE, legend="bottom", legend.grob = thisLegend)
ggsave("check_imputed_data_densplot.png",
       width = 3500, height = 2500,units = c("px"))
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
ggarrange(plotList$p1, plotList$p2, plotList$p3, plotList$p4, plotList$p5, plotList$p6,
          ncol=3, nrow=2, common.legend = TRUE, legend="bottom", legend.grob = thisLegend)
ggsave("check_imputed_data_densplot_single.png",
       width = 3500, height = 2500,units = c("px"))

# Pooled stats

## Set working directory
setwd("Z:/Roser/analyses_20220621_AcEF/imputation_factor")

##  Define measures
ac_measures <- c('academic_english_scores','academic_maths_scores','academic_total_scores')
ef_measures <- c('inhibition_factor','switch_factor','memory_factor')

## Correlations between ac_measures & ef_measures at T0

### Pearson corr
sink("pooled_corr_Ac-EF_T0.txt")
corr <- micombine.cor(mi.res=imp, variables=c(7:12))
print(corr)
sink()

### LM with AcTotal
sink("pooled_corr-lm_AcTotal-EF_T0.txt")
#AcTotal-inhibition_factor
print("AcTotal-inhibition_factor")
model <- with(imp, lm(academic_total_scores ~ inhibition_factor))
print(summary(pool(model)))
#AcTotal-switch_factor
print("AcTotal-switch_factor")
model <- with(imp, lm(academic_total_scores ~ switch_factor))
print(summary(pool(model)))
#AcTotal-memory_factor
print("AcTotal-memory_factor")
model <- with(imp, lm(academic_total_scores ~ memory_factor))
print(summary(pool(model)))
sink()
