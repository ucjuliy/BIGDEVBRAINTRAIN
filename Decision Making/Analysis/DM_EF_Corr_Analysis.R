# clears workspace
rm(list=ls())

library(dplyr)
library(ggplot2)
library(extrafont)
library(tidyr)
library(effectsize)
library(rstatix)
library(ggpubr)

#install.packages("ggiraphExtra")
#install.packages("ggiraph")
require(ggiraph)
require(ggiraphExtra)
require(plyr)
library(ggeffects)
library(ggplot2)
library(stats)

#library(confint)
library(boot)
library(lme4)
library(lmerTest)

#install.packages('stargazer')
library(stargazer)
library(ggeffects)
library(AICcmodavg)

# for missing data and such
library(mice)
library(VIM)
library(missMDA) # This loads also required package FactoMineR
library(nFactors)

library(dplyr)
library(Rmisc)
library(ggpubr)
library(ggplot2)

###########################################################
### - EF Corrs analysis at T0
###########################################################

load("data/Train_EF_Imp.RData")

### create same filter for the data
tempDataEF_T0 <- filter(tempDataEF, Session == 0)

max(tempDataEF_T0$data$Age_Frac_Imp)

tempDataEF_T0$data$Group <- as.character(tempDataEF_T0$data$Group)
tempDataEF_T0$data$Group[tempDataEF_T0$data$Group == '2'] <- '0'

tempDataEF_T0$imp$Group <- as.character(tempDataEF_T0$imp$Group)
tempDataEF_T0$imp$Group[tempDataEF_T0$data$Group == '2'] <- '0'

# name levels
levels(tempDataEF_T0$data$Group) <- c("Con","Exp")
levels(tempDataEF_T0$data$Online) <- c("Off","On")

levels(tempDataEF_T0$imp$Group) <- c("Con","Exp")
levels(tempDataEF_T0$imp$Online) <- c("Off","On")

names(tempDataEF_T0$imp)

tempDataEF_T0$imp$Perc_Delay_z <- scale(tempDataEF_T0$imp$Perc_Delay,center=TRUE,scale=TRUE)
tempDataEF_T0$imp$Perc_Delay_z <- as.numeric(tempDataEF_T0$imp$Perc_Delay_z)

# invert the shifting var
tempDataEF_T0$imp$S_I[tempDataEF_T0$imp$S] <- tempDataEF_T0$imp$S*-1

fit <- with(data = tempDataEF_T0, exp = lm(S ~ Age_Frac_Imp)) # sig and sig
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(S_I ~ Age_Frac_Imp)) # sig and sig
summary(pool(fit))



### ----------------------------------------- AGE
### DG coins
fit <- with(data = tempDataEF_T0, exp = lm(DG_Coins_Given ~ Age_Frac_Imp + Online)) # sig and sig
summary(pool(fit))

fit <- with(data = tempDataEF_T0, exp = lm(DG_Coins_Given_z ~ Age_Frac_Imp + Online)) # sig and sig
summary(pool(fit))

# dg rt
fit <- with(data = tempDataEF_T0, exp = lm(DG_RT_log ~ Age_Frac_Imp + Online)) # sig and sig
summary(pool(fit))

# ug coins
fit <- with(data = tempDataEF_T0, exp = lm(UG_Coins_Given ~ Age_Frac_Imp + Online)) # sig and sig
summary(pool(fit))

# ug rt
fit <- with(data = tempDataEF_T0, exp = lm(UG_RT_log ~ Age_Frac_Imp + Online)) # sig
summary(pool(fit))

ggplot(DF_imp_T0, aes(x = Age_Frac_Imp, y = DG_RT_log)) + 
  geom_line()

# ug dg diff
fit <- with(data = tempDataEF_T0, exp = lm(DG_UG_Diff ~ Age_Frac_Imp + Online)) # sig and sig
summary(pool(fit))

fit <- with(data = tempDataEF_T0, exp = lm(DG_UG_Diff_z ~ Age_Frac_Imp + Online)) # sig and sig
summary(pool(fit))

# UG accept
fit <- with(data = tempDataEF_T0, exp = lm(UG_Offer_Accept ~ Age_Frac_Imp + Online)) # NS
summary(pool(fit))

mod <- glm(UG_Offer_Accept ~ Age_Frac_Imp_z + Online_num, data = DF_imp_T0, family=binomial(link="logit"))
summary(mod) # NS

# UG accept decision time
fit <- with(data = tempDataEF_T0, exp = lm(UG_Decision_RT_log ~ Age_Frac_Imp + Online)) # NS
summary(pool(fit))

### temporal discounting
# perc delay
fit <- with(data = tempDataEF_T0, exp = lm(Perc_Delay ~ Age_Frac_Imp + Online)) # marginal
summary(pool(fit))

fit <- with(data = tempDataEF_T0, exp = lm(Perc_Delay_z ~ Age_Frac_Imp + Online)) # marginal
summary(pool(fit))

# td avg rt
fit <- with(data = tempDataEF_T0, exp = lm(TD_AvgRT_log ~ Age_Frac_Imp + Online)) # NS
summary(pool(fit))


# delay valuation
fit <- with(data = tempDataEF_T0, exp = lm(Delay_WMean ~ Age_Frac_Imp + Online)) # Sig
summary(pool(fit))

fit <- with(data = tempDataEF_T0, exp = lm(Delay_WMean_z ~ Age_Frac_Imp + Online)) # Sig
summary(pool(fit))

fit <- with(data = tempDataEF_T0, exp = lm(Corr_r_Exp_Delay ~ Age_Frac_Imp + Online)) # Sig
summary(pool(fit))

fit <- with(data = tempDataEF_T0, exp = lm(Corr_int_Exp_Delay ~ Age_Frac_Imp + Online)) # Sig
summary(pool(fit))

### reward valuation
fit <- with(data = tempDataEF_T0, exp = lm(Corr_r_Exp_Reward ~ Age_Frac_Imp + Online)) # Sig
summary(pool(fit))

fit <- with(data = tempDataEF_T0, exp = lm(Corr_int_Exp_Reward ~ Age_Frac_Imp + Online)) # Sig
summary(pool(fit))

fit <- with(data = tempDataEF_T0, exp = lm(Reward_WMean ~ Age_Frac_Imp + Online)) # NS
summary(pool(fit))

fit <- with(data = tempDataEF_T0, exp = lm(Reward_WMean_z ~ Age_Frac_Imp + Online)) # NS
summary(pool(fit))

### adjust p values
p <- c(0.002, )


### --- Inhibition factor first
fit <- with(data = tempDataEF_T0, exp = lm(DG_Coins_Given_z ~ I))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(UG_Coins_Given ~ I))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(DG_UG_Diff_z ~ I))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(UG_Offer_Accept ~ I))
summary(pool(fit))

fit <- with(data = tempDataEF_T0, exp = lm(DG_Coins_Given_z ~ I  + Age_Frac_Imp))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(UG_Coins_Given ~ I + Age_Frac_Imp))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(DG_UG_Diff_z ~ I + Age_Frac_Imp))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(UG_Offer_Accept ~ I + Age_Frac_Imp))
summary(pool(fit))

fit <- with(data = tempDataEF_T0, exp = lm(Perc_Delay_z ~ I))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(Delay_WMean_z ~ I))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(Reward_WMean_z ~ I))
summary(pool(fit))

fit <- with(data = tempDataEF_T0, exp = lm(Perc_Delay_z ~ I  + Age_Frac_Imp))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(Delay_WMean_z ~ I + Age_Frac_Imp))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(Reward_WMean_z ~ I + Age_Frac_Imp))
summary(pool(fit))

### --- cognitive flexibility factor first
fit <- with(data = tempDataEF_T0, exp = lm(DG_Coins_Given_z ~ S))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(UG_Coins_Given ~ S))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(DG_UG_Diff_z ~ S))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(UG_Offer_Accept ~ S))
summary(pool(fit))

fit <- with(data = tempDataEF_T0, exp = lm(DG_Coins_Given ~ S  + Age_Frac_Imp))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(UG_Coins_Given ~ S + Age_Frac_Imp))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(DG_UG_Diff_z ~ S + Age_Frac_Imp))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(UG_Offer_Accept ~ S + Age_Frac_Imp))
summary(pool(fit))

fit <- with(data = tempDataEF_T0, exp = lm(Perc_Delay_z ~ S))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(Delay_WMean_z ~ S))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(Reward_WMean_z ~ S))
summary(pool(fit))


fit <- with(data = tempDataEF_T0, exp = lm(Perc_Delay_z ~ S  + Age_Frac_Imp))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(Delay_WMean_z ~ S + Age_Frac_Imp))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(Reward_WMean_z ~ S + Age_Frac_Imp))
summary(pool(fit))


### --- Working Memory factor
fit <- with(data = tempDataEF_T0, exp = lm(DG_Coins_Given_z ~ M))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(UG_Coins_Given ~ M))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(DG_UG_Diff_z ~ M))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(UG_Offer_Accept ~ M))
summary(pool(fit))

fit <- with(data = tempDataEF_T0, exp = lm(DG_Coins_Given_z ~ M + Age_Frac_Imp))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(UG_Coins_Given ~ M + Age_Frac_Imp))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(DG_UG_Diff_z ~ M + Age_Frac_Imp))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(UG_Offer_Accept ~ M + Age_Frac_Imp))
summary(pool(fit))

fit <- with(data = tempDataEF_T0, exp = lm(Perc_Delay_z ~ M))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(Delay_WMean_z ~ M))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(Reward_WMean_z ~ M))
summary(pool(fit))


fit <- with(data = tempDataEF_T0, exp = lm(Perc_Delay ~ M + Age_Frac_Imp))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(Delay_WMean_z ~ M + Age_Frac_Imp))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(Reward_WMean ~ M + Age_Frac_Imp))
summary(pool(fit))

### ---SSRT separately
fit <- with(data = tempDataEF_T0, exp = lm(DG_Coins_Given ~ SSRT))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(UG_Coins_Given ~ SSRT))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(DG_UG_Diff ~ SSRT))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(UG_Offer_Accept ~ SSRT))
summary(pool(fit))

fit <- with(data = tempDataEF_T0, exp = lm(DG_Coins_Given ~ SSRT + Age_Frac_Imp))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(UG_Coins_Given ~ SSRT + Age_Frac_Imp))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(DG_UG_Diff ~ SSRT + Age_Frac_Imp))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(UG_Offer_Accept ~ SSRT + Age_Frac_Imp))
summary(pool(fit))

fit <- with(data = tempDataEF_T0, exp = lm(Perc_Delay ~ SSRT))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(Delay_WMean ~ SSRT))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(Reward_WMean ~ SSRT))
summary(pool(fit))


fit <- with(data = tempDataEF_T0, exp = lm(Perc_Delay ~ SSRT + Age_Frac_Imp))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(Delay_WMean ~ SSRT + Age_Frac_Imp))
summary(pool(fit))
fit <- with(data = tempDataEF_T0, exp = lm(Reward_WMean ~ SSRT + Age_Frac_Imp))
summary(pool(fit))
