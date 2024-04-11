#reliability analyses

setwd("/Users/zoeli/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/training study/mixed model")
# Load packages
# you'll need to install these packages if you don't have it with install.packages("")
library(Rcpp)
library(tidyverse)
library(readxl)
library(writexl)
library(emmeans)
library(lme4)
library(lmerTest)
library(effectsize)
library(ggeffects)
library(broom.mixed)
library(brms)
library(mice)
library(reshape2)
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
library(dplyr)
library(dbplyr)
library(tibble)
library(BayesFactor)
library(effectsize)
library(performance)


#load file
data.imp <- read.csv("training_data.csv")
#as factor
data.imp$id <- factor(data.imp$id)
data.imp$timepoint <- factor(data.imp$timepoint)
data.imp$Group <- factor(data.imp$Group,levels = c('1', '2'), 
                         labels = c('1', '2'))
data.imp$gender <- factor(data.imp$Gender)
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]

write.csv(data_T0T1, "data_T0T1.csv")


#T0T1
#academic score
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
if (require("lme4")) {
  model <- lmer(academic_score ~ timepoint + (1|id), data=data_T0T1)
  icc(model)
}


#creativity
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
if (require("lme4")) {
  model <- lmer(creativity ~ timepoint + (1|id), data=data_T0T1)
  icc(model)
}

#WASI
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
if (require("lme4")) {
  model <- lmer(wasi ~ timepoint + (1|id), data=data_T0T1)
  icc(model)
}

#UG_Offer_Accept
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
if (require("lme4")) {
  model <- lmer(UG_Offer_Accept ~ timepoint +(1|id), data=data_T0T1)
  icc(model)
}

#DG_Coins_Given
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
if (require("lme4")) {
  model <- lmer(DG_Coins_Given ~ timepoint + (1|id), data=data_T0T1)
  icc(model)
}

#tot_delay_perc
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
if (require("lme4")) {
  model <- lmer(tot_delay_perc ~ timepoint + (1|id), data=data_T0T1)
  icc(model)
}

#externalising_factor
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
if (require("lme4")) {
  model <- lmer(externalising_factor ~ timepoint*Group + Age + Gender + (1|id), data=data_T0T1)
  icc(model)
}

#internalising_factor
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
if (require("lme4")) {
  model <- lmer(internalising_factor ~ timepoint*Group + Age + Gender + (1|id), data=data_T0T1)
  icc(model)
}

#EFRTFACTOR
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
if (require("lme4")) {
  model <- lmer(EFRTFACTOR ~ timepoint*Group + Age + Gender + (1|id), data=data_T0T1)
  icc(model)
}

#EF_I_error
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
if (require("lme4")) {
  model <- lmer(EF_I_error ~ timepoint*Group + Age + Gender + (1|id), data=data_T0T1)
  icc(model)
}

#EF_M_error
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
if (require("lme4")) {
  model <- lmer(EF_M_error ~ timepoint*Group + Age + Gender + (1|id), data=data_T0T1)
  icc(model)
}

#prob_stopsig
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
if (require("lme4")) {
  model <- lmer(prob_stopsig ~ timepoint*Group + Age + Gender + (1|id), data=data_T0T1)
  icc(model)
}

#meango_corrrtnr_t
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
if (require("lme4")) {
  model <- lmer(meango_corrrtnr_t ~ timepoint*Group + Age + Gender + (1|id), data=data_T0T1)
  icc(model)
}


#HavardOxford_Cortl
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
if (require("lme4")) {
  model <- lmer(HavardOxford_Cortl ~ timepoint + (1|id), data=data_T0T1)
  icc(model, tolerance = 1e-100)
}

check_singularity(model)

#FPN
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
if (require("lme4")) {
  model <- lmer(FPN ~ timepoint*Group + Age + Gender + (1|id), data=data_T0T1)
  icc(model)
}

#CON
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
if (require("lme4")) {
  model <- lmer(CON ~ timepoint*Group + Age + Gender + (1|id), data=data_T0T1)
  icc(model)
}

#CorticalThickness
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
if (require("lme4")) {
  model <- lmer(CorticalThickness ~ timepoint*Group + Age + Gender + (1|id), data=data_T0T1)
  icc(model)
}

#Rputa.frontal_FA
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
if (require("lme4")) {
  model <- lmer(Rputa.frontal_FA ~ timepoint*Group + Age + Gender + (1|id), data=data_T0T1)
  icc(model)
}


#Rputa.frontal_MD
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
if (require("lme4")) {
  model <- lmer(Rputa.frontal_MD ~ timepoint*Group + Age + Gender + (1|id), data=data_T0T1)
  icc(model)
}



#T0T2
#UG_Offer_Accept
data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
if (require("lme4")) {
  model <- lmer(UG_Offer_Accept ~ timepoint*Group + Age + Gender + (1|id), data=data_T0T2)
  icc(model)
}


#DG_Coins_Given
data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
if (require("lme4")) {
  model <- lmer(DG_Coins_Given ~ timepoint*Group + Age + Gender + (1|id), data=data_T0T2)
  icc(model)
}



#tot_delay_perc
data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
if (require("lme4")) {
  model <- lmer(tot_delay_perc ~ timepoint*Group + Age + Gender + (1|id), data=data_T0T2)
  icc(model)
}



#externalising_factor
data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
if (require("lme4")) {
  model <- lmer(externalising_factor ~ timepoint*Group + Age + Gender + (1|id), data=data_T0T2)
  icc(model)
}



#internalising_factor
data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
if (require("lme4")) {
  model <- lmer(internalising_factor ~ timepoint*Group + Age + Gender + (1|id), data=data_T0T2)
  icc(model)
}



#corsi_max_wm_t
data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
if (require("lme4")) {
  model <- lmer(corsi_max_wm_t ~ timepoint*Group + Age + Gender + (1|id), data=data_T0T2)
  icc(model)
}



#PBI_t
data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
if (require("lme4")) {
  model <- lmer(PBI_t ~ timepoint*Group + Age + Gender + (1|id), data=data_T0T2)
  icc(model)
}


#cogflex_t
data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
if (require("lme4")) {
  model <- lmer(cogflex_t ~ timepoint*Group + Age + Gender + (1|id), data=data_T0T2)
  icc(model)
}

#prob_stopsig
data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
if (require("lme4")) {
  model <- lmer(prob_stopsig ~ timepoint*Group + Age + Gender + (1|id), data=data_T0T2)
  icc(model)
}



#meango_corrrtnr_t
data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
if (require("lme4")) {
  model <- lmer(meango_corrrtnr_t ~ timepoint*Group + Age + Gender + (1|id), data=data_T0T2)
  icc(model)
}


