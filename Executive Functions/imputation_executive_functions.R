# This script analyses IDEAS data

# Set working directory
setwd("C:/Users/keertana/Documents/Latent Score Modelling/")

# Load packages
library(Rcpp)
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
library(Rmisc)
library(ggplot2)

source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

# Turn off scientific notation
options(scipen = 999)
options(max.print=1000000)

# Read in dataset
#load data here
#########################IMPUTATION FOR Fssrt FIRST#############################################
# data_excel<-read.csv(file = "EF_alltasks_alltimepoints.csv")

# data_excel<-read.csv(file = "EF Long_RT.csv")
# data_excel<-read.csv(file = "EF Long_ERROR.csv")



data <- as.data.frame(data_excel)
#remove fmri data t1##
#data = subset(data, select = -c(adhd_in_casi_t)) 
#data = subset(data, select = -c(inhibitionnet_t)) 
attach(data) #access variables in data

# Specify factors
data$p_id <- factor(data$p_id)

# add file for online_covariates 

data$group<-factor(data$group, levels = c('1', '2'), labels = c('Exp', 'Con'))
data$session<-factor(data$session, levels = c('0', '1', '2'), labels = c('t0', 't1', 't2'))
#data$session<-as.factor(data$session)
#levels(data$Group)<-c('Exp', 'Con')

#data$gender <- factor(data$gender)

# Select only T0 & T1 data
# again depending on which measure here 
# data <- data[data$session %in% c("t0", "t1"), ]

# Summary data
summary(data)


# Define measures for loops

# RT ONLY + SSRT + memory 
measures <- c('meango_corrrtnr_t', 'Fssrtnr_t', 'cf_switchrt_t', 'dprimeONEBACK_t', 'dprimeTWOBACK_t', 'corsi_max_wm_t', 'oneback_hits_av_rt_t', 'twoback_hits_av_rt_t', 'stroop_rt_t', 'flankerswitch_rt_t', 'flank_inh_rt_t', 'PBI_RT_t', 'ay_bx_t')

#ERRORS MAINLY
measures <- c('corsi_acc_t',	'corsi_max_wm_t',	'Fssrtnr_t',	'ay_bx_error_t',	'PBI_error_t',	'cf_switch_acc_t'	,'psedooneFArate_t', 'psedotwoFArate_t',	'stroop_error_t',	'flankerswitch_error_t',	'flank_inh_error_t')

measures <- c('REzssrtnr_t',  'PBI_t',  'cogflex_t', 'meango_corrrtnr_t', 'corsi_max_wm_t')

#Check for outliers & replace by NA
# for(n in 1:length(measures)){

#   outlier_values <- boxplot.stats(eval(parse(text=paste("data","$",measures[[n]],sep=""))), coef = 2)$out # outlier values.
#   assign(paste("outlier_rowID_",measures[[n]],sep=""),which(eval(parse(text=paste("data","$",measures[[n]],sep=""))) %in% c(outlier_values)))
#   boxplot(eval(parse(text=paste("data","$",measures[[n]],sep=""))), main=measures[n], boxwex=0.1)
#   mtext(paste("Outliers: ", paste(eval(parse(text=paste("outlier_rowID_",measures[[n]],sep=""))), collapse=", ")), cex=0.6)
#   eval(parse(text=paste("data","$",measures[[n]],'[outlier_rowID_',measures[[n]],']<-NA',sep="")))
# }

# Checks on missing data
## Percentage missing data
sink("check_missing_data_percentage_EF_task.txt")
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
ggsave(paste("check_missing_data_map_EF_task.png",sep=""),
       width = 7,height = 7)
# dependent <- c('Fssrt','muFssrt','sigmaFssrt','tauFssrt','muGoRT','sigmaGoRT','tauGoRT',
#                'academic_x.wide_scores',
#                'DMN','VIS','FPN','CON','MOT','AUD',
#                'IN','rIFG','rCAU','rPUT','rTHAL','rSTN','lPSMA','rCAL')
# explanatory <- c('id','age','gender','session','group','training_slope','training_sessions','pHit','SSD','academic_time','FD')
# missing_pairs(data,dependent,explanatory)

# Impute data
set.seed(123)
ini <- mice(data, maxit=0, pri=F) #get predictor matrix
pred <- ini$predictorMatrix
pred[,c("p_id", "totalsessions" )] <- 0 #don't use as predictor
meth <- ini$method
meth[c("p_id","session","group", "age", "totalsessions", "trainingslope")] <- "" #don't impute these variables, use only as predictors
imp <- mice(data, m=100, maxit=20, printFlag=TRUE, predictorMatrix=pred, method=meth) #impute data with 100 imputations and 20 iterations

# Select a single dataset for post-hoc tests
implist <- mids2mitml.list(imp) #create a list of completed data sets
data.imp <- implist[[1]]
write_csv(data.imp,"EF_imputed_3timepointmeasures.csv") #save imputed dataset to use for analyses

# Checks on missing/imputed data: check distribution of imputed data (red) is similar to observed data (blue)
## create function
fortify.mids <- function(x){
  imps <- do.call(rbind, lapply(seq_len(x$m), function(i){
    data.frame(complete(x, i), Imputation = i, Data = "Imputed")
  }))
  orig <- cbind(x$data, Imputation = NA, Data = "Observed")
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
      scale_colour_manual(values = c(Imputed = "lightsalmon2", Observed = "dodgerblue1")) +
      labs(x=measures[[n]]) +
      theme_grey(base_size=10) +
      theme(legend.position="none")
  }))
}
ggarrange(plotList$p1, plotList$p2, plotList$p3, plotList$p4, plotList$p5, plotList$p6, plotList$p7, plotList$p8, plotList$p9,
          plotList$p10, plotList$p11, plotList$p12, plotList$p13, plotList$p14, plotList$p15, plotList$p16, plotList$p17, plotList$p18,
          plotList$p19, plotList$p20, plotList$p21, plotList$p22, plotList$p23, plotList$p24, plotList$p25, plotList$p26, plotList$p27,
          ncol=6, nrow=5)
ggsave("check_imputed_data_densplot_EF_tasks.png",
       width = 3500, height = 2000,units = c("px"))
## single: plot & save
plotList <- list()
for(n in 1:length(measures)){
  plotName <- paste( 'p', n, sep = '' )
  (plotList[[plotName]] <- local({
    n <- n
    ggplot() +
      geom_density(aes(x = eval(parse(text=measures[[n]])), colour = "data1"), alpha = .2, size = 1, data = data.imp) +
      geom_density(aes(x = eval(parse(text=measures[[n]])), colour = "data2"), , alpha = .2, size = 1, data = data) +
      scale_colour_manual(values = c(data1 = "lightsalmon2", data2 = "dodgerblue1")) +
      labs(x=measures[[n]]) +
      theme_grey(base_size=10) +
      theme(legend.position="none")
  }))
}
ggarrange(plotList$p1, plotList$p2, plotList$p3, plotList$p4, plotList$p5, plotList$p6, plotList$p7, plotList$p8, plotList$p9,
          plotList$p10, plotList$p11, plotList$p12, plotList$p13, plotList$p14, plotList$p15, plotList$p16, plotList$p17, plotList$p18,
          plotList$p19, plotList$p20, plotList$p21, plotList$p22, plotList$p23, plotList$p24, plotList$p25, plotList$p26, plotList$p27,
          ncol=6, nrow=5)
ggsave("check_imputed_data_densplot_single_EF_tasks.png",
       width = 3500, height = 2000,units = c("px"))
