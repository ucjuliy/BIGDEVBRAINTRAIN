#short term near/far transfer 
#training effect t0 vs. T1

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

####Mixed models - are there treatment effects?####
#Term of interest is timepoint*group 
#Does interaction stand when controlling for covariates (gender, age)

#academic score
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
formula <- academic_score ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T1, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T1, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T1 <- subset(data_T0T1, academic_score != "NA" )
bf = anovaBF(academic_score ~ timepoint*Group, data=data_T0T1, whichRandom = "id")
bf[4] / bf[3:2:1]


#creativity
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
formula <- creativity ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T1, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T1, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))

# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T1 <- subset(data_T0T1, creativity != "NA" )
bf = anovaBF(creativity ~ timepoint*Group, data=data_T0T1, whichRandom = "id")
bf[4] / bf[3]

#WASI
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
formula <- wasi ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T1, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T1, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))

# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T1 <- subset(data_T0T1, wasi != "NA" )
bf = anovaBF(wasi ~ timepoint*Group, data=data_T0T1, whichRandom = "id")
bf[4] / bf[3]


#decision making 
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
formula <- UG_Offer_Accept ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T1, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T1, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T1 <- subset(data_T0T1, UG_Offer_Accept != "NA" )
bf = anovaBF(UG_Offer_Accept ~ timepoint*Group, data=data_T0T1, whichRandom = "id")
bf[4] / bf[3]



data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
formula <- DG_Coins_Given ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T1, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T1, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T1 <- subset(data_T0T1, DG_Coins_Given != "NA" )
bf = anovaBF(DG_Coins_Given ~ timepoint*Group, data=data_T0T1, whichRandom = "id")
bf[4] / bf[3]


data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
formula <- tot_delay_perc ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T1, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T1, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T1 <- subset(data_T0T1, tot_delay_perc != "NA" )
bf = anovaBF(tot_delay_perc ~ timepoint*Group, data=data_T0T1, whichRandom = "id")
bf[4] / bf[3]


#mental health
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
formula <- externalising_factor ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T1, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T1, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T1 <- subset(data_T0T1, externalising_factor != "NA" )
bf = anovaBF(externalising_factor ~ timepoint*Group, data=data_T0T1, whichRandom = "id")
bf[4] / bf[3]


data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
formula <- internalising_factor ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T1, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T1, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T1 <- subset(data_T0T1, internalising_factor != "NA" )
bf = anovaBF(internalising_factor ~ timepoint*Group, data=data_T0T1, whichRandom = "id")
bf[4] / bf[3]


#EF factors
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
formula <- EFRTFACTOR ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T1, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T1, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))


data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
formula <- EF_I_error ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T1, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T1, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))



data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
formula <- EF_M_error ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T1, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T1, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T1 <- subset(data_T0T1, EF_M_error != "NA" )
bf = anovaBF(EF_M_error ~ timepoint*Group, data=data_T0T1, whichRandom = "id")
bf[4] / bf[3]


#EF task based
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
formula <- prob_stopsig ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T1, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T1, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))

data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
formula <- meango_corrrtnr_t ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T1, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T1, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))


#brain fMRI task based
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
formula <- HavardOxford_Cortl ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T1, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T1, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
data_T0T1 <- subset(data_T0T1, HavardOxford_Cortl != "NA" )
bf = anovaBF(HavardOxford_Cortl ~ timepoint*Group, data=data_T0T1, whichRandom = "id")
bf[4] / bf[3]


#brain resting state
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
formula <- DMN ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T1, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T1, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
data_T0T1 <- subset(data_T0T1, DMN != "NA" )
bf = anovaBF(DMN ~ timepoint*Group, data=data_T0T1, whichRandom = "id")
bf[4] / bf[3]

data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
formula <- FPN ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T1, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T1, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
data_T0T1 <- subset(data_T0T1, FPN != "NA" )
bf = anovaBF(FPN ~ timepoint*Group, data=data_T0T1, whichRandom = "id")
bf[4] / bf[3]

data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
formula <- CON ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T1, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T1, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
data_T0T1 <- subset(data_T0T1, CON != "NA" )
bf = anovaBF(CON ~ timepoint*Group, data=data_T0T1, whichRandom = "id")
bf[4] / bf[3]


#brain structural
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
formula <- CorticalThickness ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T1, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T1, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
data_T0T1 <- subset(data_T0T1, CorticalThickness != "NA" )
bf = anovaBF(CorticalThickness ~ timepoint*Group, data=data_T0T1, whichRandom = "id")
bf[4] / bf[3]



#brain DTI
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
formula <- Rputa.frontal_FA ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T1, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T1, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
data_T0T1 <- subset(data_T0T1, Rputa.frontal_FA != "NA" )
bf = anovaBF(Rputa.frontal_FA ~ timepoint*Group, data=data_T0T1, whichRandom = "id")
bf[4] / bf[3]


data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
formula <- Rputa.frontal_MD ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T1, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T1, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
data_T0T1 <- subset(data_T0T1, Rputa.frontal_MD != "NA" )
bf = anovaBF(Rputa.frontal_MD ~ timepoint*Group, data=data_T0T1, whichRandom = "id")
bf[4] / bf[3]


data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
formula <- Rputa.frontal_RD ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T1, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T1, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
data_T0T1 <- subset(data_T0T1, Rputa.frontal_RD != "NA" )
bf = anovaBF(Rputa.frontal_RD ~ timepoint*Group, data=data_T0T1, whichRandom = "id")
bf[4] / bf[3]

data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
formula <- Rcaud.frontal_FA ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T1, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T1, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
data_T0T1 <- subset(data_T0T1, Rcaud.frontal_FA != "NA" )
bf = anovaBF(Rcaud.frontal_FA ~ timepoint*Group, data=data_T0T1, whichRandom = "id")
bf[4] / bf[3]


data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
formula <- Rcaud.frontal_MD ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T1, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T1, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
data_T0T1 <- subset(data_T0T1, Rcaud.frontal_MD != "NA" )
bf = anovaBF(Rcaud.frontal_MD ~ timepoint*Group, data=data_T0T1, whichRandom = "id")
bf[4] / bf[3]


data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
formula <- Rcaud.frontal_RD ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T1, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T1, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]
data_T0T1 <- subset(data_T0T1, Rcaud.frontal_RD != "NA" )
bf = anovaBF(Rcaud.frontal_RD ~ timepoint*Group, data=data_T0T1, whichRandom = "id")
bf[4] / bf[3]


# FDR correction HochbergCorrection
#academic score
formula <- academic_score ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T1, REML=TRUE))
eta_squared(model, partial = TRUE)
AcademicScore_df<-model[nrow(model),4]
AcademicScore_F<-model[nrow(model),5]
AcademicScore_p<-model[nrow(model),6]

#creativity
formula <- creativity ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T1, REML=TRUE))
eta_squared(model, partial = TRUE)
creativity_df<-model[nrow(model),4]
creativity_F<-model[nrow(model),5]
creativity_p<-model[nrow(model),6]
#wasi
formula <- wasi ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T1, REML=TRUE))
eta_squared(model, partial = TRUE)
wasi_df<-model[nrow(model),4]
wasi_F<-model[nrow(model),5]
wasi_p<-model[nrow(model),6]

#decision making Dictator & Ultimatum game
formula <- UG_Offer_Accept ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T1, REML=TRUE))
eta_squared(model, partial = TRUE)
UG_Offer_Accept_df<-model[nrow(model),4]
UG_Offer_Accept_F<-model[nrow(model),5]
UG_Offer_Accept_p<-model[nrow(model),6]

formula <- DG_Coins_Given ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T1, REML=TRUE))
eta_squared(model, partial = TRUE)
DG_Coins_Given_df<-model[nrow(model),4]
DG_Coins_Given_F<-model[nrow(model),5]
DG_Coins_Given_p<-model[nrow(model),6]


formula <- tot_delay_perc ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T1, REML=TRUE))
eta_squared(model, partial = TRUE)
tot_delay_perc_df<-model[nrow(model),4]
tot_delay_perc_F<-model[nrow(model),5]
tot_delay_perc_p<-model[nrow(model),6]


#mental health 
formula <- externalising_factor ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T1, REML=TRUE))
eta_squared(model, partial = TRUE)
externalising_factor_df<-model[nrow(model),4]
externalising_factor_F<-model[nrow(model),5]
externalising_factor_p<-model[nrow(model),6]

formula <- internalising_factor ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T1, REML=TRUE))
eta_squared(model, partial = TRUE)
internalising_factor_df<-model[nrow(model),4]
internalising_factor_F<-model[nrow(model),5]
internalising_factor_p<-model[nrow(model),6]

#executive function factors
formula <- EFRTFACTOR ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T1, REML=TRUE))
eta_squared(model, partial = TRUE)
EFRTFACTOR_df<-model[nrow(model),4]
EFRTFACTOR_F<-model[nrow(model),5]
EFRTFACTOR_p<-model[nrow(model),6]

formula <- EF_I_error ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T1, REML=TRUE))
eta_squared(model, partial = TRUE)
EF_I_error_df<-model[nrow(model),4]
EF_I_error_F<-model[nrow(model),5]
EF_I_error_p<-model[nrow(model),6]

formula <- EF_M_error ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T1, REML=TRUE))
eta_squared(model, partial = TRUE)
EF_M_error_df<-model[nrow(model),4]
EF_M_error_F<-model[nrow(model),5]
EF_M_error_p<-model[nrow(model),6]

#executive function task
formula <- prob_stopsig ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T1, REML=TRUE))
eta_squared(model, partial = TRUE)
prob_stopsig_df<-model[nrow(model),4]
prob_stopsig_F<-model[nrow(model),5]
prob_stopsig_p<-model[nrow(model),6]

formula <- meango_corrrtnr_t ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T1, REML=TRUE))
eta_squared(model, partial = TRUE)
meango_corrrtnr_t_df<-model[nrow(model),4]
meango_corrrtnr_t_F<-model[nrow(model),5]
meango_corrrtnr_t_p<-model[nrow(model),6]


#brain fMRI Task based
formula <- HavardOxford_Cortl ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T1, REML=TRUE))
eta_squared(model, partial = TRUE)
HavardOxford_Cortl_df<-model[nrow(model),4]
HavardOxford_Cortl_F<-model[nrow(model),5]
HavardOxford_Cortl_p<-model[nrow(model),6]

#brain resting state
formula <- FPN ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T1, REML=TRUE))
eta_squared(model, partial = TRUE)
FPN_df<-model[nrow(model),4]
FPN_F<-model[nrow(model),5]
FPN_p<-model[nrow(model),6]

formula <- CON ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T1, REML=TRUE))
eta_squared(model, partial = TRUE)
CON_df<-model[nrow(model),4]
CON_F<-model[nrow(model),5]
CON_p<-model[nrow(model),6]

#brain structural 
formula <- CorticalThickness ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T1, REML=TRUE))
eta_squared(model, partial = TRUE)
CorticalThickness_df<-model[nrow(model),4]
CorticalThickness_F<-model[nrow(model),5]
CorticalThickness_p<-model[nrow(model),6]


#brain DTI
formula <- Rputa.frontal_FA ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T1, REML=TRUE))
eta_squared(model, partial = TRUE)
Rputa.frontal_FA_df<-model[nrow(model),4]
Rputa.frontal_FA_F<-model[nrow(model),5]
Rputa.frontal_FA_p<-model[nrow(model),6]

formula <- Rputa.frontal_MD ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T1, REML=TRUE))
eta_squared(model, partial = TRUE)
Rputa.frontal_MD_df<-model[nrow(model),4]
Rputa.frontal_MD_F<-model[nrow(model),5]
Rputa.frontal_MD_p<-model[nrow(model),6]





domain <-c('AcademicScore', 'creativity', 'wasi',
          'UG_Offer_Accept', 'DG_Coins_Given', 'tot_delay_perc',
          'externalising_factor', 'internalising_factor', 
          'EFRTFACTOR', 'EF_I_error', 'EF_M_error', 
          'prob_stopsig', 'meango_corrrtnr_t',
          'HavardOxford_Cortl', 'FPN', 'CON', 'CorticalThickness', 
          'Rputa.frontal_FA', 'Rputa.frontal_MD')
df <-as.numeric(list(AcademicScore_df, creativity_df, wasi_df,
                           UG_Offer_Accept_df, DG_Coins_Given_df, tot_delay_perc_df,
                           externalising_factor_df, internalising_factor_df, 
                           EFRTFACTOR_df, EF_I_error_df, EF_M_error_df,
                           prob_stopsig_df, meango_corrrtnr_t_df,
                           HavardOxford_Cortl_df, FPN_df, CON_df, CorticalThickness_df,
                           Rputa.frontal_FA_df, Rputa.frontal_MD_df))
F_scores <-as.numeric(list(AcademicScore_F, creativity_F, wasi_F,
                           UG_Offer_Accept_F, DG_Coins_Given_F, tot_delay_perc_F,
                           externalising_factor_F, internalising_factor_F, 
                           EFRTFACTOR_F, EF_I_error_F, EF_M_error_F,
                           prob_stopsig_F, meango_corrrtnr_t_F,
                           HavardOxford_Cortl_F, FPN_F, CON_F, CorticalThickness_F,
                           Rputa.frontal_FA_F, Rputa.frontal_MD_F))
P_values <-as.numeric(list(AcademicScore_p, creativity_p, wasi_p,
                           UG_Offer_Accept_p, DG_Coins_Given_p, tot_delay_perc_p,
                           externalising_factor_p, internalising_factor_p, 
                           EFRTFACTOR_p, EF_I_error_p, EF_M_error_p,
                           prob_stopsig_p, meango_corrrtnr_t_p,
                           HavardOxford_Cortl_p, FPN_p, CON_p, CorticalThickness_p,
                           Rputa.frontal_FA_p, Rputa.frontal_MD_p))

p<-p.adjust(P_values,method="fdr")
# number of variables
k <- data.frame(domain, p)
#k$p<-p
k<-k %>% mutate(TEST =
                  case_when(p < .05  ~ "SIG", 
                            p >.05  ~ "NSIG")
)

k$p_value<-P_values
k$df <- df
k$F.score <- F_scores
write.csv(k, "mixed_model_T0_T1_1.csv")




