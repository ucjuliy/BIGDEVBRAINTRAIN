#long term near/far transfer 
#training effect t0 vs. T2 four domains
#only applied for decicision making, mental health and EF data

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

#load file
data.imp <- read.csv("data_imp.csv")
#as factor
data.imp$id <- factor(data.imp$id)
data.imp$timepoint <- factor(data.imp$timepoint)
data.imp$Group <- factor(data.imp$Group,levels = c('1', '2'), 
                         labels = c('1', '2'))
data.imp$gender <- factor(data.imp$Gender)
data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]


#decision making 
data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
formula <- UG_Offer_Accept ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T2, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T2, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
data_T0T2 <- subset(data_T0T2, UG_Offer_Accept != "NA" )
bf = anovaBF(UG_Offer_Accept ~ timepoint*Group, data=data_T0T2, whichRandom = "id")
bf[4] / bf[3]



data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
formula <- DG_Coins_Given ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T2, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T2, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
data_T0T2 <- subset(data_T0T2, DG_Coins_Given != "NA" )
bf = anovaBF(DG_Coins_Given ~ timepoint*Group, data=data_T0T2, whichRandom = "id")
bf[4] / bf[3]


data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
formula <- DG_UG_Diff ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T2, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T2, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
data_T0T2 <- subset(data_T0T2, DG_UG_Diff != "NA" )
bf = anovaBF(DG_UG_Diff ~ timepoint*Group, data=data_T0T2, whichRandom = "id")
bf[4] / bf[3]



data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
formula <- tot_delay_perc ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T2, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T2, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
data_T0T2 <- subset(data_T0T2, tot_delay_perc != "NA" )
bf = anovaBF(tot_delay_perc ~ timepoint*Group, data=data_T0T2, whichRandom = "id")
bf[4] / bf[3]


#mental health
data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
formula <- externalising_factor ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T2, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T2, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
data_T0T2 <- subset(data_T0T2, externalising_factor != "NA" )
bf = anovaBF(externalising_factor ~ timepoint*Group, data=data_T0T2, whichRandom = "id")
bf[4] / bf[3]


data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
formula <- internalising_factor ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T2, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T2, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
data_T0T2 <- subset(data_T0T2, internalising_factor != "NA" )
bf = anovaBF(internalising_factor ~ timepoint*Group, data=data_T0T2, whichRandom = "id")
bf[4] / bf[3]




#EF task based
data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
formula <- corsi_max_wm_t ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T2, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T2, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
data_T0T2 <- subset(data_T0T2, corsi_max_wm_t != "NA" )
bf = anovaBF(corsi_max_wm_t ~ timepoint*Group, data=data_T0T2, whichRandom = "id")
bf[4] / bf[3]

data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
formula <- PBI_t ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T2, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T2, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
data_T0T2 <- subset(data_T0T2, PBI_t != "NA" )
bf = anovaBF(PBI_t ~ timepoint*Group, data=data_T0T2, whichRandom = "id")
bf[4] / bf[3]

data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
formula <- cogflex_t ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T2, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T2, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))
# if mixed model (interaction) is non-significant, we would want to run a BF test - tests absence of an effect
# if <1, evidence for NULL ( BF = )
data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
data_T0T2 <- subset(data_T0T2, cogflex_t != "NA" )
bf = anovaBF(cogflex_t ~ timepoint*Group, data=data_T0T2, whichRandom = "id")
bf[4] / bf[3]


data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
formula <- prob_stopsig ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T2, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T2, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))


data_T0T2 <- data.imp[data.imp$timepoint %in% c("0", "2"), ]
formula <- meango_corrrtnr_t ~ timepoint*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data_T0T2, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data_T0T2, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group, adjust = "bonferroni"))




#HochbergCorrection
formula <- UG_Offer_Accept ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T2, REML=TRUE))
eta_squared(model, partial = TRUE)
UG_Offer_Accept_df<-model[nrow(model),4]
UG_Offer_Accept_F<-model[nrow(model),5]
UG_Offer_Accept_p<-model[nrow(model),6]

formula <- DG_Coins_Given ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T2, REML=TRUE))
eta_squared(model, partial = TRUE)
DG_Coins_Given_df<-model[nrow(model),4]
DG_Coins_Given_F<-model[nrow(model),5]
DG_Coins_Given_p<-model[nrow(model),6]

formula <- tot_delay_perc ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T2, REML=TRUE))
eta_squared(model, partial = TRUE)
tot_delay_perc_df<-model[nrow(model),4]
tot_delay_perc_F<-model[nrow(model),5]
tot_delay_perc_p<-model[nrow(model),6]

#mental health 
formula <- externalising_factor ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T2, REML=TRUE))
eta_squared(model, partial = TRUE)
externalising_factor_df<-model[nrow(model),4]
externalising_factor_F<-model[nrow(model),5]
externalising_factor_p<-model[nrow(model),6]

formula <- internalising_factor ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T2, REML=TRUE))
eta_squared(model, partial = TRUE)
internalising_factor_df<-model[nrow(model),4]
internalising_factor_F<-model[nrow(model),5]
internalising_factor_p<-model[nrow(model),6]

#executive function task
formula <- corsi_max_wm_t ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T2, REML=TRUE))
eta_squared(model, partial = TRUE)
corsi_max_wm_t_df<-model[nrow(model),4]
corsi_max_wm_t_F<-model[nrow(model),5]
corsi_max_wm_t_p<-model[nrow(model),6]

formula <- PBI_t ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T2, REML=TRUE))
eta_squared(model, partial = TRUE)
PBI_t_df<-model[nrow(model),4]
PBI_t_F<-model[nrow(model),5]
PBI_t_p<-model[nrow(model),6]

formula <- cogflex_t ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T2, REML=TRUE))
eta_squared(model, partial = TRUE)
cogflex_t_df<-model[nrow(model),4]
cogflex_t_F<-model[nrow(model),5]
cogflex_t_p<-model[nrow(model),6]

formula <- prob_stopsig ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T2, REML=TRUE))
eta_squared(model, partial = TRUE)
prob_stopsig_df<-model[nrow(model),4]
prob_stopsig_F<-model[nrow(model),5]
prob_stopsig_p<-model[nrow(model),6]

formula <- meango_corrrtnr_t ~ timepoint*Group + Age + Gender + (1|id)
model<-anova(lmer( formula, data=data_T0T2, REML=TRUE))
eta_squared(model, partial = TRUE)
meango_corrrtnr_t_df<-model[nrow(model),4]
meango_corrrtnr_t_F<-model[nrow(model),5]
meango_corrrtnr_t_p<-model[nrow(model),6]




domain <-c('UG_Offer_Accept', 'DG_Coins_Given', 'tot_delay_perc',
           'externalising_factor', 'internalising_factor', 
           'corsi_max_wm_t', 'PBI_t', 'cogflex_t', 'prob_stopsig', 'meango_corrrtnr_t')
df <-as.numeric(list(UG_Offer_Accept_df, DG_Coins_Given_df, tot_delay_perc_df,
                           externalising_factor_df, internalising_factor_df, 
                           corsi_max_wm_t_df, PBI_t_df, cogflex_t_df, prob_stopsig_df, meango_corrrtnr_t_df))
F_scores <-as.numeric(list(UG_Offer_Accept_F, DG_Coins_Given_F, tot_delay_perc_F,
                           externalising_factor_F, internalising_factor_F, 
                           corsi_max_wm_t_F, PBI_t_F, cogflex_t_F, prob_stopsig_F, meango_corrrtnr_t_F))
P_values <-as.numeric(list(UG_Offer_Accept_p, DG_Coins_Given_p, tot_delay_perc_p,
                           externalising_factor_p, internalising_factor_p, 
                           corsi_max_wm_t_p, PBI_t_p, cogflex_t_p, prob_stopsig_p, meango_corrrtnr_t_p))

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
write.csv(k, "mixed_model_T0_T2.csv")





