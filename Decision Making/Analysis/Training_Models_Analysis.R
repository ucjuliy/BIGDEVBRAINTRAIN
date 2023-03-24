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

### for plotting
my_theme <-   theme(
  #legend.position = "none",
  plot.title = element_text(family="Arial",color="black", size=18 ,hjust = 0.5,margin=margin(0,0,10,0)),
  text = element_text(family="Arial", size=16),
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 14),
  axis.title.x = element_text(color="black", size=16, margin=margin(5,0,0,0)),
  axis.text.x = element_text(size = 14, margin=margin(5,0,0,0)),
  axis.text.y = element_text(size = 14, margin=margin(0,5,0,10))
)


######################################## PLOT RAW DATA OVERALL #####################################

# use this data instead of the other one!
# standardized imputed data
TD_imp <- read.csv("data/Train_Imp.csv")

# standardized imputed data
load("data/Train_Imp.RData")


names(TD_imp)

# 205 participants
A = length(unique(TD_imp$ID))
A
length(unique(tempData$data$ID))

# creating factors
TD_imp$ID <- as.factor(TD_imp$ID)
TD_imp$Group <- as.factor(TD_imp$Group)
TD_imp$Online <- as.factor(TD_imp$Online)
TD_imp$SessionFactor <- as.factor(TD_imp$Session)

# rename group - switching that 1 is con and 2 is exp to exp = 1 and con = 0
TD_imp$Group <- as.character(TD_imp$Group)
TD_imp$Group[TD_imp$Group == '2'] <- '0'

# name levels
levels(TD_imp$Group) <- c("Con","Exp")
levels(TD_imp$Online) <- c("Off","On")

T0 <- TD_imp[which(TD_imp$Session == 0),]

# invert this var? is this possible
T0$S_I <- T0$S*-1

cor.test(T0$S_I,T0$DG_UG_Diff)
cor.test(T0$S,T0$DG_UG_Diff)

cor.test(T0$S,T0$Age_Frac_Imp)
cor.test(T0$S_I,T0$Age_Frac_Imp)

# display T0 demographics
table(T0['Gender'])
table(T0['School'])
table(T0['Online'])
table(T0['SES'])

library(psych)
library(Rmisc)

################################################################################################
################################################################################################
### -------- IMMEDIATE TRAINING EFFECTS
################################################################################################
################################################################################################

### ---------------------------------- Models ----------------------------------

# let's do T0 and T1 first
unique(TD_imp$Session)

Exp <- TD_imp[which(TD_imp$Group == 1),]
Con <- TD_imp[which(TD_imp$Group == 0),]

Exp$Train_Coef_z <- scale(Exp$Train_Coef, center = TRUE, scale = TRUE)
Con$Train_Coef_z <- scale(Con$Train_Coef, center = TRUE, scale = TRUE)

TD_all <- rbind(Exp,Con)

TD_T0_T1 = TD_all[which (TD_all$Session != 2),]
unique(TD_T0_T1$Session)
names(TD_T0_T1)

length(unique(TD_T0_T1$ID))

### create same filter for the data

tempData_T0_T1 <- filter(tempData, Session != 2)


tempData_T0_T1$data$Group <- as.character(tempData_T0_T1$data$Group)
tempData_T0_T1$data$Group[tempData_T0_T1$data$Group == '2'] <- '0'

tempData_T0_T1$imp$Group <- as.character(tempData_T0_T1$imp$Group)
tempData_T0_T1$imp$Group[tempData_T0_T1$data$Group == '2'] <- '0'

# name levels
levels(tempData_T0_T1$data$Group) <- c("Con","Exp")
levels(tempData_T0_T1$data$Online) <- c("Off","On")

levels(tempData_T0_T1$imp$Group) <- c("Con","Exp")
levels(tempData_T0_T1$imp$Online) <- c("Off","On")


### prepping variables
names(TD_T0_T1)

# creating factors
TD_T0_T1$ID <- as.factor(TD_T0_T1$ID)
TD_T0_T1$Group <- as.factor(TD_T0_T1$Group)
TD_T0_T1$Online <- as.factor(TD_T0_T1$Online)
# dont make session a fator. treat it as a continous variable, assuming that
# there is a simple linear trend
#TD_T0_T1$Session <- as.numeric(TD_T0_T1$Session) # do this or not?

# name levels
levels(TD_T0_T1$Group) <- c("Con","Exp")
levels(TD_T0_T1$Online) <- c("Off","On")
#levels(TD_T0_T1$Session) <- c("T0","T1")

# z-score the variables (age already z-scored)
TD_T0_T1$Age_Frac_z <- scale(TD_T0_T1$Age_Frac, center = TRUE, scale = TRUE)
TD_T0_T1$DG_Coins_Given_z <- scale(TD_T0_T1$DG_Coins_Given, center = TRUE, scale = TRUE)
TD_T0_T1$UG_Coins_Given_z <- scale(TD_T0_T1$UG_Coins_Given, center = TRUE, scale = TRUE)
TD_T0_T1$DG_UG_Diff_z <- scale(TD_T0_T1$DG_UG_Diff, center = TRUE, scale = TRUE)
TD_T0_T1$DG_UG_RT_Diff_z <- scale(TD_T0_T1$DG_UG_RT_Diff, center = TRUE, scale = TRUE)
TD_T0_T1$TotalSessions_z <- scale(TD_T0_T1$TotalSessions, center = TRUE, scale = TRUE)
TD_T0_T1$PercentageBonusDone_z <- scale(TD_T0_T1$PercentageBonusDone, center = TRUE, scale = TRUE)

# split this up over groups
#TD_T0_T1$Train_Coef_z <- scale(TD_T0_T1$Train_Coef, center = TRUE, scale = TRUE)

range(TD_T0_T1$DG_Coins_Given_z)
range(TD_T0_T1$DG_UG_Diff_z)
range(TD_T0_T1$TotalSessions_z)
range(TD_T0_T1$Train_Coef_z)

########################################################################
########################################################################
########################################################################
########################################################################
# -------------------- TEMPORAL DISCOUNTING ----------------------------
########################################################################
########################################################################
########################################################################
########################################################################

names(TD_T0_T1)

##############################################################################
### ------------------ Total Delayed Percentage -----------------------------
##############################################################################

# -------------------------- normal models

# Session and group plus interaction + Online
mod1 <- lmer(Perc_Delay ~ Session * Group + Online + (1| ID), data = TD_T0_T1, REML = TRUE)

# so model 5 is best, but we interpret model 5 
summary(mod1)

fit <- with(data = tempData_T0_T1, exp = lmer(Perc_Delay ~ Session * Group + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# std
fit <- with(data = tempData_T0_T1, exp = lmer(Perc_Delay_z ~ Session * Group + Online + (1|ID), REML=TRUE))
summary(pool(fit))



### ------ BAYESIAN MODELS
#install.packages('future')
library(brms)
library(future)

# trying out a few things
brm(Perc_Delay ~ Session * Group + Online + (1|ID), data=TD_T0_T1 )

priors <- get_prior(Perc_Delay ~ Session * Group + Online + (1|ID),
                    data = TD_T0_T1, family = gaussian(), prior = priors)

priors


# actual way of running this
plan(multisession)
fit_imp1 <- brm_multiple(Perc_Delay ~ Session + Group + Online + (1|ID), 
                         data = tempData_T0_T1, chains = 1, save_all_pars=TRUE)

fit_imp2 <- brm_multiple(Perc_Delay ~ Session * Group + Online + (1|ID), 
                         data = tempData_T0_T1, chains = 1, save_all_pars=TRUE)
summary(fit_imp2)

bayes_factor(fit_imp1,fit_imp2)

# other way around
bayes_factor(fit_imp2,fit_imp1)

# -------------------------- Age

mod2 <- lmer(Perc_Delay ~ Session * Group + Age_Frac_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod2)

mod2 <- lmer(Perc_Delay ~ Session * Group * Age_Frac_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod2)

fit <- with(data = tempData_T0_T1, exp = lmer(Perc_Delay ~ Session * Group * Age_Frac_Imp + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# -------------------------- Training intensity

mod3 <- lmer(Perc_Delay ~ Session * Group * TotalSessions_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod3)

mod4 <- lmer(Perc_Delay ~ Session * Group * PercentageBonusDone_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod4)

mod5 <- lmer(Perc_Delay ~ Session * Group * Train_Coef_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod5)


##############################################################################
### ------------------ Reaction time for decisions  -----------------------------
##############################################################################

# -------------------------- normal models
min(TD_T0_T1$TD_AvgRT_log)

TD_T0_T1$TD_AvgRT_log[TD_T0_T1$TD_AvgRT_log < -5] <- NA

# Session and group plus interaction + Online
mod1 <- lmer(TD_AvgRT_log ~ Session * Group + Online + (1| ID), data = TD_T0_T1, REML = TRUE)

# so model 5 is best, but we interpret model 5 
summary(mod1)

fit <- with(data = tempData_T0_T1, exp = lm(TD_AvgRT_log ~ Session * Group + Online))
summary(pool(fit))

# std
fit <- with(data = tempData_T0_T1, exp = lmer(Perc_Delay_z ~ Session * Group + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# -------------------------- Age


mod2 <- lmer(TD_AvgRT_log ~ Session * Group + Age_Frac_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod2)

mod2 <- lmer(TD_AvgRT_log ~ Session * Group * Age_Frac_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod2)

# -------------------------- Training intensity

mod3 <- lmer(TD_AvgRT_log ~ Session * Group * TotalSessions_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod3)

mod4 <- lmer(TD_AvgRT_log ~ Session * Group * PercentageBonusDone_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod4)

mod5 <- lmer(TD_AvgRT_log ~ Session * Group * Train_Coef_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod5)


##############################################################################
### ------------------ Delay_WWMean  -----------------------------
##############################################################################

# -------------------------- normal models

# Session and group plus interaction + Online
mod1 <- lmer(Delay_WMean ~ Session * Group + Online + (1| ID), data = TD_T0_T1, REML = TRUE)

# so model 5 is best, but we interpret model 5 
summary(mod1)


fit <- with(data = tempData_T0_T1, exp = lmer(Delay_WMean ~ Session * Group + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# std
fit <- with(data = tempData_T0_T1, exp = lmer(Delay_WMean_z ~ Session * Group + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# actual way of running this
plan(multisession)
fit_imp1 <- brm_multiple(Delay_WMean ~ Session + Group + Online + (1|ID), 
                         data = tempData_T0_T1, chains = 1, save_all_pars=TRUE)

fit_imp2 <- brm_multiple(Delay_WMean ~ Session * Group + Online + (1|ID), 
                         data = tempData_T0_T1, chains = 1, save_all_pars=TRUE)
summary(fit_imp2)

bayes_factor(fit_imp1,fit_imp2)

# -------------------------- Age


mod2 <- lmer(Delay_WMean ~ Session * Group + Age_Frac_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod2)

mod2 <- lmer(Delay_WMean ~ Session * Group * Age_Frac_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod2)

fit <- with(data = tempData_T0_T1, exp = lmer(Delay_WMean ~ Session * Group * Age_Frac_Imp + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# -------------------------- Training intensity

mod3 <- lmer(Delay_WMean ~ Session * Group * TotalSessions_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod3)

mod4 <- lmer(Delay_WMean ~ Session * Group * PercentageBonusDone_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod4)

mod5 <- lmer(Delay_WMean ~ Session * Group * Train_Coef_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod5)

##############################################################################
### ------------------ Reward_WWMean  -----------------------------
##############################################################################

# -------------------------- normal models

# Session and group plus interaction + Online
mod1 <- lmer(Reward_WMean ~ Session * Group + Online + (1| ID), data = TD_T0_T1, REML = TRUE)

# so model 5 is best, but we interpret model 5 
summary(mod1)

fit <- with(data = tempData_T0_T1, exp = lmer(Reward_WMean ~ Session * Group + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# std
fit <- with(data = tempData_T0_T1, exp = lmer(Reward_WMean_z ~ Session * Group + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# actual way of running this
plan(multisession)
fit_imp1 <- brm_multiple(Reward_WMean ~ Session + Group + Online + (1|ID), 
                         data = tempData_T0_T1, chains = 1, save_all_pars=TRUE)

fit_imp2 <- brm_multiple(Reward_WMean ~ Session * Group + Online + (1|ID), 
                         data = tempData_T0_T1, chains = 1, save_all_pars=TRUE)
summary(fit_imp2)

bayes_factor(fit_imp1,fit_imp2)

# -------------------------- Age


mod2 <- lmer(Reward_WMean ~ Session * Group + Age_Frac_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod2)

mod2 <- lmer(Reward_WMean ~ Session * Group * Age_Frac_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod2)

fit <- with(data = tempData_T0_T1, exp = lmer(Reward_WMean ~ Session * Group * Age_Frac_Imp + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# -------------------------- Training intensity

mod3 <- lmer(Reward_WMean ~ Session * Group * TotalSessions_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod3)

mod4 <- lmer(Reward_WMean ~ Session * Group * PercentageBonusDone_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod4)

mod5 <- lmer(Reward_WMean ~ Session * Group * Train_Coef_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod5)


##############################################################################
### ------------------ DG_Coins_Given -----------------------------
##############################################################################

# -------------------------- normal models

# Session and group plus interaction + Online
mod1 <- lmer(DG_Coins_Given ~ Session * Group + Online + (1| ID), data = TD_T0_T1, REML = TRUE)

# so model 5 is best, but we interpret model 5 
summary(mod1)

fit <- with(data = tempData_T0_T1, exp = lmer(DG_Coins_Given ~ Session * Group + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# std
fit <- with(data = tempData_T0_T1, exp = lmer(DG_Coins_Given_z ~ Session * Group + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# actual way of running this
plan(multisession)
fit_imp1 <- brm_multiple(DG_Coins_Given ~ Session + Group + Online + (1|ID), 
                         data = tempData_T0_T1, chains = 1, save_all_pars=TRUE)

fit_imp2 <- brm_multiple(DG_Coins_Given ~ Session * Group + Online + (1|ID), 
                         data = tempData_T0_T1, chains = 1, save_all_pars=TRUE)
summary(fit_imp2)

bayes_factor(fit_imp1,fit_imp2)

# -------------------------- Age


mod2 <- lmer(DG_Coins_Given ~ Session * Group + Age_Frac_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod2)

mod2 <- lmer(DG_Coins_Given ~ Session * Group * Age_Frac_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod2)

fit <- with(data = tempData_T0_T1, exp = lmer(DG_Coins_Given ~ Session * Group * Age_Frac_Imp + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# -------------------------- Training intensity

mod3 <- lmer(DG_Coins_Given ~ Session * Group * TotalSessions_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod3)

mod4 <- lmer(DG_Coins_Given ~ Session * Group * PercentageBonusDone_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod4)

mod5 <- lmer(DG_Coins_Given ~ Session * Group * Train_Coef_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod5)

##############################################################################
### ------------------ UG_Coins_Given -----------------------------
##############################################################################

# -------------------------- normal models

# Session and group plus interaction + Online
mod1 <- lmer(UG_Coins_Given ~ Session * Group + Online + (1| ID), data = TD_T0_T1, REML = TRUE)

# so model 5 is best, but we interpret model 5 
summary(mod1)

fit <- with(data = tempData_T0_T1, exp = lmer(UG_Coins_Given ~ Session * Group + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# std
fit <- with(data = tempData_T0_T1, exp = lmer(UG_Coins_Given_z ~ Session * Group + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# actual way of running this
plan(multisession)
fit_imp1 <- brm_multiple(UG_Coins_Given ~ Session + Group + Online + (1|ID), 
                         data = tempData_T0_T1, chains = 1, save_all_pars=TRUE)

fit_imp2 <- brm_multiple(UG_Coins_Given ~ Session * Group + Online + (1|ID), 
                         data = tempData_T0_T1, chains = 1, save_all_pars=TRUE)
summary(fit_imp2)

bayes_factor(fit_imp1,fit_imp2)

# -------------------------- Age


mod2 <- lmer(UG_Coins_Given ~ Session * Group + Age_Frac_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod2)

mod2 <- lmer(UG_Coins_Given ~ Session * Group * Age_Frac_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod2)

fit <- with(data = tempData_T0_T1, exp = lmer(UG_Coins_Given ~ Session * Group * Age_Frac_Imp + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# -------------------------- Training intensity

mod3 <- lmer(UG_Coins_Given ~ Session * Group * TotalSessions_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod3)

### Plotting results
mydf <- ggpredict(mod3, se = TRUE, terms = c("Session","TotalSessions_z","Group"))
plot(mydf) +
  labs(
    x = "Session", 
    y = "Coins Given", 
    title = "Predicted Coins Given for the Ultimatum Proposer Game",
    colour = "Sessions (z)"
  )




mod4 <- lmer(UG_Coins_Given ~ Session * Group * PercentageBonusDone_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod4)

mod5 <- lmer(UG_Coins_Given ~ Session * Group * Train_Coef_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod5)

##############################################################################
### ------------------ DG UG Diff -----------------------------
##############################################################################

# -------------------------- normal models

# Session and group plus interaction + Online
mod1 <- lmer(DG_UG_Diff ~ Session * Group + Online + (1| ID), data = TD_T0_T1, REML = TRUE)

# so model 5 is best, but we interpret model 5 
summary(mod1)

fit <- with(data = tempData_T0_T1, exp = lmer(DG_UG_Diff ~ Session * Group + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# std
fit <- with(data = tempData_T0_T1, exp = lmer(DG_UG_Diff_z ~ Session * Group + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# actual way of running this
get_prior(DG_UG_Diff ~ Session + Group + Online + (1|ID), iter = 10000,
          data = TD_T0_T1)

bprior <- c(prior_string("normal(0,10)", class = "b"),
            prior(normal(1,2), class = b, coef = GroupExp),
            prior_(~cauchy(0,2), class = ~sd,
                   group = ~ID, coef = ~Intercept))

prior = prior(student_t(1, 0.1, 0.1), coef= Session)

make_stancode(DG_UG_Diff ~ Session + Group + Online + (1|ID),
              data = TD_T0_T1,
              prior = prior)

plan(multisession)
fit_imp1 <- brm_multiple(DG_UG_Diff ~ Session + Group + Online + (1|ID), 
                         data = tempData_T0_T1, chains = 1, save_all_pars=TRUE,
                         iter = 10000, prior = prior)

fit_imp2 <- brm_multiple(DG_UG_Diff ~ Session * Group + Online + (1|ID), iter = 10000,
                         data = tempData_T0_T1, chains = 1, save_all_pars=TRUE)
summary(fit_imp2)

bayes_factor(fit_imp1,fit_imp2)

# -------------------------- Age


mod2 <- lmer(DG_UG_Diff ~ Session * Group + Age_Frac_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod2)

mod2 <- lmer(DG_UG_Diff ~ Session * Group * Age_Frac_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod2)

fit <- with(data = tempData_T0_T1, exp = lmer(DG_UG_Diff ~ Session * Group * Age_Frac_Imp + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# -------------------------- Training intensity

mod3 <- lmer(DG_UG_Diff ~ Session * Group * TotalSessions_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod3)

mod4 <- lmer(DG_UG_Diff ~ Session * Group * PercentageBonusDone_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod4)

mod5 <- lmer(DG_UG_Diff ~ Session * Group * Train_Coef_z + Online + (1| ID), data = TD_T0_T1, REML = TRUE)
summary(mod5)


##############################################################################
### ------------------ UG_Offer_Accept -----------------------------
##############################################################################

# -------------------------- normal models

## THIS EFFECT IS BACK NOW

# Session and group plus interaction + Online
mod1 <- glmer(UG_Offer_Accept ~ Session * Group + Online + (1| ID), data = TD_T0_T1,
              family = binomial(link="logit"), control = glmerControl(optimizer = "bobyqa"))

# so model 5 is best, but we interpret model 5 
summary(mod1)

fit <- with(data = tempData_T0_T1, exp = glmer(UG_Offer_Accept ~ Session * Group + Online + (1|ID),
                                               family = binomial(link="logit"), control = glmerControl(optimizer = "bobyqa")))
summary(pool(fit))

# std
fit <- with(data = tempData_T0_T1, exp = glmer(UG_Offer_Accept ~ Session * Group + Online + (1|ID),
                                               family = binomial(link="logit"), control = glmerControl(optimizer = "bobyqa")))
summary(pool(fit))

### Plotting results
mydf <- ggpredict(mod1, se = TRUE, terms = c("Session","Group"))
plot(mydf)


# actual way of running this
plan(multisession)
fit_imp1 <- brm_multiple(UG_Offer_Accept ~ Session + Group + Online + (1|ID), 
                         data = tempData_T0_T1, chains = 1, save_all_pars=TRUE)

fit_imp2 <- brm_multiple(UG_Offer_Accept ~ Session * Group + Online + (1|ID), 
                         data = tempData_T0_T1, chains = 1, save_all_pars=TRUE)
summary(fit_imp2)

bayes_factor(fit_imp1,fit_imp2)


# -------------------------- Age


mod2 <- glmer(UG_Offer_Accept ~ Session * Group + Age_Frac_z + Online + (1| ID), data = TD_T0_T1,
              family = binomial(link="logit"), control = glmerControl(optimizer = "bobyqa"))
summary(mod2)

mod2 <- glmer(UG_Offer_Accept ~ Session * Group * Age_Frac_z + Online + (1| ID), data = TD_T0_T1,
              family = binomial(link="logit"), control = glmerControl(optimizer = "bobyqa"))
summary(mod2)

fit <- with(data = tempData_T0_T1, exp = glmer(UG_Offer_Accept ~ Session * Group * Age_Frac_Imp + Online + (1|ID),
                                               family = binomial(link="logit"), control = glmerControl(optimizer = "bobyqa")))
summary(pool(fit))


# -------------------------- Training intensity

mod3 <- glmer(UG_Offer_Accept ~ Session * Group * TotalSessions_z + Online + (1| ID), data = TD_T0_T1,
              family = binomial(link="logit"), control = glmerControl(optimizer = "bobyqa"))
summary(mod3)

mod4 <- glmer(UG_Offer_Accept ~ Session * Group * PercentageBonusDone_z + Online + (1| ID), data = TD_T0_T1,
              family = binomial(link="logit"), control = glmerControl(optimizer = "bobyqa"))
summary(mod4)

### Plotting results
mydf <- ggpredict(mod4, se = TRUE, terms = c("Session","PercentageBonusDone_z","Group"))
plot(mydf)

mod5 <- glmer(UG_Offer_Accept ~ Session * Group * Train_Coef_z + Online + (1| ID), data = TD_T0_T1,
              family = binomial(link="logit"), control = glmerControl(optimizer = "bobyqa"))
summary(mod5)

### Plotting results
mydf <- ggpredict(mod5, se = TRUE, terms = c("Group","Train_Coef_z"))
plot(mydf)


################################################################################################
################################################################################################
### -------- LONG TTERM TRAINING EFFECTS
################################################################################################
################################################################################################

### ---------------------------------- Models ----------------------------------

# let's do T0 and T1 first
unique(TD_imp$Session)
TD_T0_T2 = TD_imp[which (TD_imp$Session != 1),]
unique(TD_T0_T2$Session)
names(TD_T0_T2)

# rename group
TD_T0_T2$Group <- as.character(TD_T0_T2$Group)
TD_T0_T2$Group[TD_T0_T2$Group == '2'] <- '0'

#nrow(TD_T0_T2)
#rownames(TD_T0_T2) <- NULL

### prepping variables
names(TD_T0_T2)

# creating factors
TD_T0_T2$ID <- as.factor(TD_T0_T2$ID)
TD_T0_T2$Group <- as.factor(TD_T0_T2$Group)
TD_T0_T2$Online <- as.factor(TD_T0_T2$Online)
# dont make session a fator. treat it as a continous variable, assuming that
# there is a simple linear trend
#TD_T0_T2$Session <- as.numeric(TD_T0_T2$Session) # do this or not?

# name levels
levels(TD_T0_T2$Group) <- c("Con","Exp")
levels(TD_T0_T2$Online) <- c("Off","On")
#levels(TD_T0_T2$Session) <- c("T0","T1")

# z-score the variables (age already z-scored)
TD_T0_T2$Age_Frac_z <- scale(TD_T0_T2$Age_Frac, center = TRUE, scale = TRUE)
TD_T0_T2$DG_Coins_Given_z <- scale(TD_T0_T2$DG_Coins_Given, center = TRUE, scale = TRUE)
TD_T0_T2$UG_Coins_Given_z <- scale(TD_T0_T2$UG_Coins_Given, center = TRUE, scale = TRUE)
TD_T0_T2$DG_UG_Diff_z <- scale(TD_T0_T2$DG_UG_Diff, center = TRUE, scale = TRUE)
TD_T0_T2$DG_UG_RT_Diff_z <- scale(TD_T0_T2$DG_UG_RT_Diff, center = TRUE, scale = TRUE)
TD_T0_T2$TotalSessions_z <- scale(TD_T0_T2$TotalSessions, center = TRUE, scale = TRUE)
TD_T0_T2$PercentageBonusDone_z <- scale(TD_T0_T2$PercentageBonusDone, center = TRUE, scale = TRUE)
TD_T0_T2$Train_Coef_z <- scale(TD_T0_T2$Train_Coef, center = TRUE, scale = TRUE)

range(TD_T0_T2$DG_Coins_Given_z)
range(TD_T0_T2$DG_UG_Diff_z)
range(TD_T0_T2$TotalSessions_z)
range(TD_T0_T2$Train_Coef_z)

# reaction time orthogonalization
TD_T0_T2$AvgdelayRT[TD_T0_T2$AvgdelayRT == 0] <- NA
TD_T0_T2$AvgdelayRT_z <- (log(TD_T0_T2$AvgdelayRT) + log(TD_T0_T2$AvgdelayRT))/2

TD_T0_T2$AvgimmRT[TD_T0_T2$AvgimmRT == 0] <- NA
TD_T0_T2$AvgimmRT_z <- (log(TD_T0_T2$AvgimmRT) + log(TD_T0_T2$AvgimmRT))/2

TD_T0_T2$hi_lo_conf_RT_Diff[TD_T0_T2$hi_lo_conf_RT_Diff == 0] <- NA
TD_T0_T2$hi_lo_conf_RT_Diff_z <- (log(TD_T0_T2$hi_lo_conf_RT_Diff) + log(TD_T0_T2$hi_lo_conf_RT_Diff))/2

TD_T0_T2$imm_del_RT_Diff[TD_T0_T2$imm_del_RT_Diff == 0] <- NA
TD_T0_T2$imm_del_RT_Diff_z <- (log(TD_T0_T2$imm_del_RT_Diff) + log(TD_T0_T2$imm_del_RT_Diff))/2

### create same filter for the data

tempData_T0_T2 <- filter(tempData, Session != 1)


tempData_T0_T2$data$Group <- as.character(tempData_T0_T2$data$Group)
tempData_T0_T2$data$Group[tempData_T0_T2$data$Group == '2'] <- '0'

tempData_T0_T2$imp$Group <- as.character(tempData_T0_T2$imp$Group)
tempData_T0_T2$imp$Group[tempData_T0_T2$data$Group == '2'] <- '0'

# name levels
levels(tempData_T0_T2$data$Group) <- c("Con","Exp")
levels(tempData_T0_T2$data$Online) <- c("Off","On")

levels(tempData_T0_T2$imp$Group) <- c("Con","Exp")
levels(tempData_T0_T2$imp$Online) <- c("Off","On")


##############################################################################
### ------------------Dictator Game Coins Given  -----------------------------
##############################################################################

# -------------------------- normal models

# Session and group plus interaction + Online
mod1 <- lmer(DG_Coins_Given ~ Session * Group + Online + (1| ID), data = TD_T0_T2, REML = TRUE)

# so model 5 is best, but we interpret model 5 
summary(mod1)

fit <- with(data = tempData_T0_T2, exp = lmer(DG_Coins_Given ~ Session * Group + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# std
fit <- with(data = tempData_T0_T2, exp = lmer(DG_Coins_Given_z ~ Session * Group + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# -------------------------- Age

mod2 <- lmer(DG_Coins_Given ~ Session * Group + Age_Frac_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod2)

mod2 <- lmer(DG_Coins_Given ~ Session * Group * Age_Frac_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod2)

fit <- with(data = tempData_T0_T2, exp = lmer(DG_Coins_Given ~ Session * Group * Age_Frac_Imp + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# -------------------------- Training intensity

mod3 <- lmer(DG_Coins_Given ~ Session * Group * TotalSessions_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod3)

mod4 <- lmer(DG_Coins_Given ~ Session * Group * PercentageBonusDone_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod4)

mod5 <- lmer(DG_Coins_Given ~ Session * Group * Train_Coef_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod5)

##############################################################################
### ------------------ Ultimatum Game Coins Given  -----------------------------
##############################################################################

# -------------------------- normal models

# Session and group plus interaction + Online
mod1 <- lmer(UG_Coins_Given ~ Session * Group + Online + (1| ID), data = TD_T0_T2, REML = TRUE)

# so model 5 is best, but we interpret model 5 
summary(mod1)

fit <- with(data = tempData_T0_T2, exp = lmer(UG_Coins_Given ~ Session * Group + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# -------------------------- Age

mod2 <- lmer(UG_Coins_Given ~ Session * Group + Age_Frac_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod2)

mod2 <- lmer(UG_Coins_Given ~ Session * Group * Age_Frac_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod2)

fit <- with(data = tempData_T0_T2, exp = lmer(UG_Coins_Given ~ Session * Group * Age_Frac_Imp + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# -------------------------- Training intensity

mod3 <- lmer(UG_Coins_Given ~ Session * Group * TotalSessions_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod3)

mod4 <- lmer(UG_Coins_Given ~ Session * Group * PercentageBonusDone_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod4)

mod5 <- lmer(UG_Coins_Given ~ Session * Group * Train_Coef_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod5)

##############################################################################
### ------------------ Strategic decision making  -----------------------------
##############################################################################

# -------------------------- normal models

# Session and group plus interaction + Online
mod1 <- lmer(DG_UG_Diff ~ Session * Group + Online + (1| ID), data = TD_T0_T2, REML = TRUE)

# so model 5 is best, but we interpret model 5 
summary(mod1)

fit <- with(data = tempData_T0_T2, exp = lmer(DG_UG_Diff ~ Session * Group + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# std
fit <- with(data = tempData_T0_T2, exp = lmer(DG_UG_Diff_z ~ Session * Group + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# -------------------------- Age

mod2 <- lmer(DG_UG_Diff ~ Session * Group + Age_Frac_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod2)

mod2 <- lmer(DG_UG_Diff ~ Session * Group * Age_Frac_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod2)

fit <- with(data = tempData_T0_T2, exp = lmer(DG_UG_Diff ~ Session * Group * Age_Frac_Imp + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# -------------------------- Training intensity

mod3 <- lmer(DG_UG_Diff ~ Session * Group * TotalSessions_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod3)

mod4 <- lmer(DG_UG_Diff ~ Session * Group * PercentageBonusDone_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod4)

mod5 <- lmer(DG_UG_Diff ~ Session * Group * Train_Coef_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod5)


##############################################################################
### ------------------ UG_Offer_Accept -----------------------------
##############################################################################

# -------------------------- normal models

## THIS EFFECT IS BACK NOW

# Session and group plus interaction + Online
mod1 <- glmer(UG_Offer_Accept ~ Session * Group + Online + (1| ID), data = TD_T0_T2,
              family = binomial(link="logit"), control = glmerControl(optimizer = "bobyqa"))

# so model 5 is best, but we interpret model 5 
summary(mod1)

### Plotting results
mydf <- ggpredict(mod1, se = TRUE, terms = c("Session","Group"))
plot(mydf)

fit <- with(data = tempData_T0_T2, exp = glmer(UG_Offer_Accept ~ Session * Group + Online + (1|ID),
                                               family = binomial(link="logit"), control = glmerControl(optimizer = "bobyqa")))
summary(pool(fit))

# std
fit <- with(data = tempData_T0_T2, exp = glmer(UG_Offer_Accept ~ Session * Group + Online + (1|ID),
                                               family = binomial(link="logit"), control = glmerControl(optimizer = "bobyqa")))
summary(pool(fit))


# -------------------------- Age


mod2 <- glmer(UG_Offer_Accept ~ Session * Group + Age_Frac_z + Online + (1| ID), data = TD_T0_T2,
              family = binomial(link="logit"), control = glmerControl(optimizer = "bobyqa"))
summary(mod2)

mod2 <- glmer(UG_Offer_Accept ~ Session * Group * Age_Frac_z + Online + (1| ID), data = TD_T0_T2,
              family = binomial(link="logit"), control = glmerControl(optimizer = "bobyqa"))
summary(mod2)

fit <- with(data = tempData_T0_T2, exp = glmer(UG_Offer_Accept ~ Session * Group * Age_Frac_Imp + Online + (1|ID),
                                               family = binomial(link="logit"), control = glmerControl(optimizer = "bobyqa")))
summary(pool(fit))

# -------------------------- Training intensity

mod3 <- glmer(UG_Offer_Accept ~ Session * Group * TotalSessions_z + Online + (1| ID), data = TD_T0_T2,
              family = binomial(link="logit"), control = glmerControl(optimizer = "bobyqa"))
summary(mod3)

mod4 <- glmer(UG_Offer_Accept ~ Session * Group * PercentageBonusDone_z + Online + (1| ID), data = TD_T0_T2,
              family = binomial(link="logit"), control = glmerControl(optimizer = "bobyqa"))
summary(mod4)

### Plotting results
mydf <- ggpredict(mod4, se = TRUE, terms = c("Session","PercentageBonusDone_z","Group"))
plot(mydf)

mod5 <- glmer(UG_Offer_Accept ~ Session * Group * Train_Coef_z + Online + (1| ID), data = TD_T0_T2,
              family = binomial(link="logit"), control = glmerControl(optimizer = "bobyqa"))
summary(mod5)

### Plotting results
mydf <- ggpredict(mod5, se = TRUE, terms = c("Group","Train_Coef_z"))
plot(mydf)






##############################################################################
### ------------------ Total Delayed Percentage -----------------------------
##############################################################################

# -------------------------- normal models

# Session and group plus interaction + Online
mod1 <- lmer(Perc_Delay ~ Session * Group + Online + (1| ID), data = TD_T0_T2, REML = TRUE)

# so model 5 is best, but we interpret model 5 
summary(mod1)

fit <- with(data = tempData_T0_T2, exp = lmer(Perc_Delay ~ Session * Group + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# std
fit <- with(data = tempData_T0_T2, exp = lmer(Perc_Delay_z ~ Session * Group + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# -------------------------- Age

mod2 <- lmer(Perc_Delay ~ Session * Group + Age_Frac_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod2)

mod2 <- lmer(Perc_Delay ~ Session * Group * Age_Frac_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod2)

# -------------------------- Training intensity

mod3 <- lmer(Perc_Delay ~ Session * Group * TotalSessions_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod3)

mod4 <- lmer(Perc_Delay ~ Session * Group * PercentageBonusDone_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod4)

mod5 <- lmer(Perc_Delay ~ Session * Group * Train_Coef_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod5)


##############################################################################
### ------------------ RT For choices -----------------------------
##############################################################################

# -------------------------- normal models

# Session and group plus interaction + Online
mod1 <- lmer(TD_AvgRT_log ~ Session * Group + Online + (1| ID), data = TD_T0_T2, REML = TRUE)

# so model 5 is best, but we interpret model 5 
summary(mod1)

# -------------------------- Age

mod2 <- lmer(TD_AvgRT_log ~ Session * Group + Age_Frac_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod2)

mod2 <- lmer(TD_AvgRT_log ~ Session * Group * Age_Frac_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod2)

# -------------------------- Training intensity

mod3 <- lmer(TD_AvgRT_log ~ Session * Group * TotalSessions_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod3)

mod4 <- lmer(TD_AvgRT_log ~ Session * Group * PercentageBonusDone_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod4)

mod5 <- lmer(TD_AvgRT_log ~ Session * Group * Train_Coef_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod5)

##############################################################################
### ------------------ Delay_WMEan -----------------------------
##############################################################################

# -------------------------- normal models

# Session and group plus interaction + Online
mod1 <- lmer(Delay_WMean ~ Session * Group + Online + (1| ID), data = TD_T0_T2, REML = TRUE)

# so model 5 is best, but we interpret model 5 
summary(mod1)

fit <- with(data = tempData_T0_T2, exp = lmer(Delay_WMean ~ Session * Group + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# std
fit <- with(data = tempData_T0_T2, exp = lmer(Delay_WMean_z ~ Session * Group + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# -------------------------- Age

mod2 <- lmer(Delay_WMean ~ Session * Group + Age_Frac_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod2)

mod2 <- lmer(Delay_WMean ~ Session * Group * Age_Frac_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod2)

# -------------------------- Training intensity

mod3 <- lmer(Delay_WMean ~ Session * Group * TotalSessions_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod3)

mod4 <- lmer(Delay_WMean ~ Session * Group * PercentageBonusDone_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod4)

mod5 <- lmer(Delay_WMean ~ Session * Group * Train_Coef_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod5)


##############################################################################
### ------------------ Reward_WMEan -----------------------------
##############################################################################

# -------------------------- normal models

# Session and group plus interaction + Online
mod1 <- lmer(Reward_WMean ~ Session * Group + Online + (1| ID), data = TD_T0_T2, REML = TRUE)

# so model 5 is best, but we interpret model 5 
summary(mod1)

fit <- with(data = tempData_T0_T2, exp = lmer(Reward_WMean ~ Session * Group + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# std
fit <- with(data = tempData_T0_T2, exp = lmer(Reward_WMean_z ~ Session * Group + Online + (1|ID), REML=TRUE))
summary(pool(fit))

# -------------------------- Age

mod2 <- lmer(Reward_WMean ~ Session * Group + Age_Frac_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod2)

mod2 <- lmer(Reward_WMean ~ Session * Group * Age_Frac_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod2)

# -------------------------- Training intensity

mod3 <- lmer(Reward_WMean ~ Session * Group * TotalSessions_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod3)

mod4 <- lmer(Reward_WMean ~ Session * Group * PercentageBonusDone_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod4)

mod5 <- lmer(Reward_WMean ~ Session * Group * Train_Coef_z + Online + (1| ID), data = TD_T0_T2, REML = TRUE)
summary(mod5)



##############################################################################################################################
##############################################################################################################################
######### --- Plots (correcting for medium and without)
##############################################################################################################################
##############################################################################################################################

# T0
DF_imp_T0 = T0

DF_imp_T0$Age_round <- round(DF_imp_T0$Age_Frac_Imp)
unique(DF_imp_T0$Age_round)

DF_imp_T0$Age_round <- as.character(DF_imp_T0$Age_round)
unique(DF_imp_T0$Age_round)

DF_imp_T0$Age_round[DF_imp_T0$Age_round == "12"] <- "12+"
DF_imp_T0$Age_round[DF_imp_T0$Age_round == "13"] <- "12+"

DF_imp_T0$Age_round <- factor(DF_imp_T0$Age_round, levels = c("6","7","8","9","10","11","12+"))

### ---------------- DG COINS

medium_resid <- resid(lm(data=DF_imp_T0, DG_Coins_Given ~ Online))
age_resid <- resid(lm(data=DF_imp_T0, Age_Frac_Imp ~ Online))


# plotting the single w over age
MBplot <- ggplot(data = DF_imp_T0, fill = Age_round) +
  geom_jitter(data=DF_imp_T0,aes(x = age_resid, y = medium_resid, fill = Age_round),
              shape = 21,stroke = 1, size = 4,height = 0.25) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid), 
              color = "#c5c5c5", fill ="#c5c5c5", 
              alpha = 0.5, size = 1, linetype = "solid", method = "lm", 
              formula = y ~ x, se = T) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid),
              color = "black", size = 1.25,  
              linetype = "solid", method = "lm", formula = y ~ x, se = F) +
  scale_fill_brewer(palette = "Blues",name="Age (Years)") +
  #scale_x_continuous(breaks=seq(-3,,1), lim = c(6,13)) +
  #scale_y_continuous(breaks=seq(0,1,0.25), lim = c(0,1)) +
  guides(color = "none", fill = "none", shape = "none") +
  ggtitle('DG MUs Given over Age') +
  xlab('Age (residual)') +
  ylab('MUs (residual)') +
  theme_light() +
  my_theme

MBplot

# Saving plots with fixed dimensions, quality etc.
ggsave("DGCoins_Age_Medium_30Jun2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)

# plotting the single w over age
MBplot <- ggplot(data = DF_imp_T0, fill = Age_round) +
  geom_jitter(data=DF_imp_T0,aes(x = Age_Frac_Imp, y = DG_Coins_Given, fill = Age_round),
              shape = 21,stroke = 1, size = 4,height = 0.25) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = Age_Frac_Imp, y = DG_Coins_Given), 
              color = "#c5c5c5", fill ="#c5c5c5", 
              alpha = 0.5, size = 1, linetype = "solid", method = "lm", 
              formula = y ~ x, se = T) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = Age_Frac_Imp, y = DG_Coins_Given),
              color = "black", size = 1.25,  
              linetype = "solid", method = "lm", formula = y ~ x, se = F) +
  scale_fill_brewer(palette = "Blues",name="Age (Years)") +
  scale_x_continuous(breaks=seq(6,13), lim = c(6,13)) +
  #scale_y_continuous(breaks=seq(0,5,1), lim = c(-0.3,6)) +
  guides(color = "none", fill = "none", shape = "none") +
  ggtitle('DG MUs Given over Age') +
  xlab('Age (Years)') +
  ylab('MUs') +
  theme_light() +
  my_theme

MBplot

# Saving plots with fixed dimensions, quality etc.
ggsave("DGCoins_Age_30Jun2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)

### ---------------- DG RT Log

medium_resid <- resid(lm(data=DF_imp_T0, DG_RT_log ~ Online))
age_resid <- resid(lm(data=DF_imp_T0, Age_Frac_Imp ~ Online))


# plotting the single w over age
MBplot <- ggplot(data = DF_imp_T0, fill = Age_round) +
  geom_jitter(data=DF_imp_T0,aes(x = age_resid, y = medium_resid, fill = Age_round),
              shape = 21,stroke = 1, size = 4,height = 0.25) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid), 
              color = "#c5c5c5", fill ="#c5c5c5", 
              alpha = 0.5, size = 1, linetype = "solid", method = "lm", 
              formula = y ~ x, se = T) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid),
              color = "black", size = 1.25,  
              linetype = "solid", method = "lm", formula = y ~ x, se = F) +
  scale_fill_brewer(palette = "Blues",name="Age (Years)") +
  #scale_x_continuous(breaks=seq(-3,,1), lim = c(6,13)) +
  #scale_y_continuous(breaks=seq(0,1,0.25), lim = c(0,1)) +
  guides(color = "none", fill = "none", shape = "none") +
  ggtitle('DG RT Log over Age') +
  xlab('Age (residual)') +
  ylab('RT (Log, residual)') +
  theme_light() +
  my_theme

MBplot

# Saving plots with fixed dimensions, quality etc.
ggsave("DGRTLog_Age_Medium_30Jun2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)

# plotting the single w over age
MBplot <- ggplot(data = DF_imp_T0, fill = Age_round) +
  geom_jitter(data=DF_imp_T0,aes(x = Age_Frac_Imp, y = DG_RT_log, fill = Age_round),
              shape = 21,stroke = 1, size = 4,height = 0.25) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = Age_Frac_Imp, y = DG_RT_log), 
              color = "#c5c5c5", fill ="#c5c5c5", 
              alpha = 0.5, size = 1, linetype = "solid", method = "lm", 
              formula = y ~ x, se = T) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = Age_Frac_Imp, y = DG_RT_log),
              color = "black", size = 1.25,  
              linetype = "solid", method = "lm", formula = y ~ x, se = F) +
  scale_fill_brewer(palette = "Blues",name="Age (Years)") +
  scale_x_continuous(breaks=seq(6,13), lim = c(6,13)) +
  #scale_y_continuous(breaks=seq(0,5,1), lim = c(-0.3,6)) +
  guides(color = "none", fill = "none", shape = "none") +
  ggtitle('DG RT Log over Age') +
  xlab('Age (Years)') +
  ylab('RT (Log)') +
  theme_light() +
  my_theme

MBplot

# Saving plots with fixed dimensions, quality etc.
ggsave("DGRTLog_Age_30Jun2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)


### ---------------- UG Coins

medium_resid <- resid(lm(data=DF_imp_T0, UG_Coins_Given ~ Online))
age_resid <- resid(lm(data=DF_imp_T0, Age_Frac_Imp ~ Online))


# plotting the single w over age
MBplot <- ggplot(data = DF_imp_T0, fill = Age_round) +
  geom_jitter(data=DF_imp_T0,aes(x = age_resid, y = medium_resid, fill = Age_round),
              shape = 21,stroke = 1, size = 4,height = 0.25) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid), 
              color = "#c5c5c5", fill ="#c5c5c5", 
              alpha = 0.5, size = 1, linetype = "solid", method = "lm", 
              formula = y ~ x, se = T) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid),
              color = "black", size = 1.25,  
              linetype = "solid", method = "lm", formula = y ~ x, se = F) +
  scale_fill_brewer(palette = "Blues",name="Age (Years)") +
  #scale_x_continuous(breaks=seq(-3,,1), lim = c(6,13)) +
  #scale_y_continuous(breaks=seq(0,1,0.25), lim = c(0,1)) +
  guides(color = "none", fill = "none", shape = "none") +
  ggtitle('UG Proposer MUs Given over Age') +
  xlab('Age (residual)') +
  ylab('MUs (residual)') +
  theme_light() +
  my_theme

MBplot

# Saving plots with fixed dimensions, quality etc.
ggsave("UGCoins_Age_Medium_30Jun2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)

# plotting the single w over age
MBplot <- ggplot(data = DF_imp_T0, fill = Age_round) +
  geom_jitter(data=DF_imp_T0,aes(x = Age_Frac_Imp, y = UG_Coins_Given, fill = Age_round),
              shape = 21,stroke = 1, size = 4,height = 0.25) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = Age_Frac_Imp, y = UG_Coins_Given), 
              color = "#c5c5c5", fill ="#c5c5c5", 
              alpha = 0.5, size = 1, linetype = "solid", method = "lm", 
              formula = y ~ x, se = T) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = Age_Frac_Imp, y = UG_Coins_Given),
              color = "black", size = 1.25,  
              linetype = "solid", method = "lm", formula = y ~ x, se = F) +
  scale_fill_brewer(palette = "Blues",name="Age (Years)") +
  scale_x_continuous(breaks=seq(6,13), lim = c(6,13)) +
  #scale_y_continuous(breaks=seq(0,5,1), lim = c(-0.3,6)) +
  guides(color = "none", fill = "none", shape = "none") +
  ggtitle('UG Proposer MUs Given over Age') +
  xlab('Age (Years)') +
  ylab('MUs') +
  theme_light() +
  my_theme

MBplot

# Saving plots with fixed dimensions, quality etc.
ggsave("UGCoins_Age_30Jun2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)


### ---------------- UG RT Log

medium_resid <- resid(lm(data=DF_imp_T0, UG_RT_log ~ Online))
age_resid <- resid(lm(data=DF_imp_T0, Age_Frac_Imp ~ Online))


# plotting the single w over age
MBplot <- ggplot(data = DF_imp_T0, fill = Age_round) +
  geom_jitter(data=DF_imp_T0,aes(x = age_resid, y = medium_resid, fill = Age_round),
              shape = 21,stroke = 1, size = 4,height = 0.25) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid), 
              color = "#c5c5c5", fill ="#c5c5c5", 
              alpha = 0.5, size = 1, linetype = "solid", method = "lm", 
              formula = y ~ x, se = T) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid),
              color = "black", size = 1.25,  
              linetype = "solid", method = "lm", formula = y ~ x, se = F) +
  scale_fill_brewer(palette = "Blues",name="Age (Years)") +
  #scale_x_continuous(breaks=seq(-3,,1), lim = c(6,13)) +
  #scale_y_continuous(breaks=seq(0,1,0.25), lim = c(0,1)) +
  guides(color = "none", fill = "none", shape = "none") +
  ggtitle('UG Proposer RT Log over age') +
  xlab('Age (residual)') +
  ylab('RT (Log, residual)') +
  theme_light() +
  my_theme

MBplot

# Saving plots with fixed dimensions, quality etc.
ggsave("UGRTLog_Age_Medium_30Jun2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)

# plotting the single w over age
MBplot <- ggplot(data = DF_imp_T0, fill = Age_round) +
  geom_jitter(data=DF_imp_T0,aes(x = Age_Frac_Imp, y = UG_RT_log, fill = Age_round),
              shape = 21,stroke = 1, size = 4) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = Age_Frac_Imp, y = UG_RT_log), 
              color = "#c5c5c5", fill ="#c5c5c5", 
              alpha = 0.5, size = 1, linetype = "solid", method = "lm", 
              formula = y ~ x, se = T) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = Age_Frac_Imp, y = UG_RT_log),
              color = "black", size = 1.25,  
              linetype = "solid", method = "lm", formula = y ~ x, se = F) +
  scale_fill_brewer(palette = "Blues",name="Age (Years)") +
  scale_x_continuous(breaks=seq(6,13), lim = c(6,13)) +
  #scale_y_continuous(breaks=seq(0,5,1), lim = c(-0.3,6)) +
  guides(color = "none", fill = "none", shape = "none") +
  ggtitle('UG Proposer RT Log over age') +
  xlab('Age (Years)') +
  ylab('RT (Log)') +
  theme_light() +
  my_theme

MBplot

# Saving plots with fixed dimensions, quality etc.
ggsave("UGRTLog_Age_30Jun2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)


### ---------------- DG UG Coin Difference

medium_resid <- resid(lm(data=DF_imp_T0, DG_UG_Diff ~ Online))
age_resid <- resid(lm(data=DF_imp_T0, Age_Frac_Imp ~ Online))


# plotting the single w over age
MBplot <- ggplot(data = DF_imp_T0, fill = Age_round) +
  geom_jitter(data=DF_imp_T0,aes(x = age_resid, y = medium_resid, fill = Age_round),
              shape = 21,stroke = 1, size = 4, height = 0.25) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid), 
              color = "#c5c5c5", fill ="#c5c5c5", 
              alpha = 0.5, size = 1, linetype = "solid", method = "lm", 
              formula = y ~ x, se = T) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid),
              color = "black", size = 1.25,  
              linetype = "solid", method = "lm", formula = y ~ x, se = F) +
  scale_fill_brewer(palette = "Blues",name="Age (Years)") +
  #scale_x_continuous(breaks=seq(-3,,1), lim = c(6,13)) +
  #scale_y_continuous(breaks=seq(0,1,0.25), lim = c(0,1)) +
  guides(color = "none", fill = "none", shape = "none") +
  ggtitle('Difference in DG and UG MUs Given over age') +
  xlab('Age (residual)') +
  ylab('MU Difference (residual)') +
  theme_light() +
  my_theme

MBplot

# Saving plots with fixed dimensions, quality etc.
ggsave("DGUGDiff_Age_Medium_30Jun2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)

# plotting the single w over age
MBplot <- ggplot(data = DF_imp_T0, fill = Age_round) +
  geom_jitter(data=DF_imp_T0,aes(x = Age_Frac_Imp, y = DG_UG_Diff, fill = Age_round),
              shape = 21,stroke = 1, size = 4, height = 0.25) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = Age_Frac_Imp, y = DG_UG_Diff), 
              color = "#c5c5c5", fill ="#c5c5c5", 
              alpha = 0.5, size = 1, linetype = "solid", method = "lm", 
              formula = y ~ x, se = T) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = Age_Frac_Imp, y = DG_UG_Diff),
              color = "black", size = 1.25,  
              linetype = "solid", method = "lm", formula = y ~ x, se = F) +
  scale_fill_brewer(palette = "Blues",name="Age (Years)") +
  scale_x_continuous(breaks=seq(6,13), lim = c(6,13)) +
  #scale_y_continuous(breaks=seq(0,5,1), lim = c(-0.3,6)) +
  guides(color = "none", fill = "none", shape = "none") +
  ggtitle('UG MUs Given over age') +
  xlab('Age (Years)') +
  ylab('MU Difference') +
  theme_light() +
  my_theme

MBplot

# Saving plots with fixed dimensions, quality etc.
ggsave("DGUGDiff_Age_30Jun2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)


### ---------------- UG Offer Accept

medium_resid <- resid(lm(data=DF_imp_T0, UG_Offer_Accept ~ Online))
age_resid <- resid(lm(data=DF_imp_T0, Age_Frac_Imp ~ Online))


# plotting the single w over age
MBplot <- ggplot(data = DF_imp_T0, fill = Age_round) +
  #geom_boxplot(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid,fill = Age_round)) + 
  #geom_jitter(data=DF_imp_T0,aes(x = age_resid, y = medium_resid, fill = Age_round),
  #            shape = 21,stroke = 1, size = 4, height = 0.25) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid), 
              color = "#c5c5c5", fill ="#c5c5c5", 
              alpha = 0.5, size = 1, linetype = "solid", method = "lm", 
              formula = y ~ x, se = T) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid),
              color = "black", size = 1.25,  
              linetype = "solid", method = "lm", formula = y ~ x, se = F) +
  scale_fill_brewer(palette = "Blues",name="Age (Years)") +
  #scale_x_continuous(breaks=seq(-3,,1), lim = c(6,13)) +
  #scale_y_continuous(breaks=seq(-0.5,0.5,0.15), lim = c(-0.5,0.5)) +
  guides(color = "none", fill = "none", shape = "none") +
  ggtitle('Probability to Accept Unfair UG Offer over age') +
  xlab('Age (residual)') +
  ylab('Probability to Accept (residual)') +
  theme_light() +
  my_theme

MBplot

# Saving plots with fixed dimensions, quality etc.
ggsave("UGAccept_Age_Medium_30Jun2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)

# plotting the single w over age
MBplot <- ggplot(data = DF_imp_T0, fill = Age_round) +
  #geom_jitter(data=DF_imp_T0,aes(x = Age_Frac_Imp, y = DG_UG_Diff, fill = Age_round),
  #            shape = 21,stroke = 1, size = 4, height = 0.25) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = Age_Frac_Imp, y = UG_Offer_Accept), 
              color = "#c5c5c5", fill ="#c5c5c5", 
              alpha = 0.5, size = 1, linetype = "solid", method = "lm", 
              formula = y ~ x, se = T) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = Age_Frac_Imp, y = UG_Offer_Accept),
              color = "black", size = 1.25,  
              linetype = "solid", method = "lm", formula = y ~ x, se = F) +
  scale_fill_brewer(palette = "Blues",name="Age (Years)") +
  scale_x_continuous(breaks=seq(6,13), lim = c(6,13)) +
  #scale_y_continuous(breaks=seq(0,5,1), lim = c(-0.3,6)) +
  guides(color = "none", fill = "none", shape = "none") +
  ggtitle('Probability to Accept Unfair UG Offer over age') +
  xlab('Age (Years)') +
  ylab('Probability to Accept') +
  theme_light() +
  my_theme

MBplot

# Saving plots with fixed dimensions, quality etc.
ggsave("UGAccept_Age_30Jun2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)


### ---------------- UG Decision RT Log

medium_resid <- resid(lm(data=DF_imp_T0, UG_Decision_RT_log ~ Online))
age_resid <- resid(lm(data=DF_imp_T0, Age_Frac_Imp ~ Online))


# plotting the single w over age
MBplot <- ggplot(data = DF_imp_T0, fill = Age_round) +
  geom_jitter(data=DF_imp_T0,aes(x = age_resid, y = medium_resid, fill = Age_round),
              shape = 21,stroke = 1, size = 4,height = 0.25) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid), 
              color = "#c5c5c5", fill ="#c5c5c5", 
              alpha = 0.5, size = 1, linetype = "solid", method = "lm", 
              formula = y ~ x, se = T) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid),
              color = "black", size = 1.25,  
              linetype = "solid", method = "lm", formula = y ~ x, se = F) +
  scale_fill_brewer(palette = "Blues",name="Age (Years)") +
  #scale_x_continuous(breaks=seq(-3,,1), lim = c(6,13)) +
  #scale_y_continuous(breaks=seq(0,1,0.25), lim = c(0,1)) +
  guides(color = "none", fill = "none", shape = "none") +
  ggtitle('UG Responder Decision RT over age') +
  xlab('Age (residual)') +
  ylab('RT (Log, residual)') +
  theme_light() +
  my_theme

MBplot

# Saving plots with fixed dimensions, quality etc.
ggsave("UGRespRTLog_Age_Medium_30Jun2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)

# plotting the single w over age
MBplot <- ggplot(data = DF_imp_T0, fill = Age_round) +
  geom_jitter(data=DF_imp_T0,aes(x = Age_Frac_Imp, y = UG_Decision_RT_log, fill = Age_round),
              shape = 21,stroke = 1, size = 4) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = Age_Frac_Imp, y = UG_Decision_RT_log), 
              color = "#c5c5c5", fill ="#c5c5c5", 
              alpha = 0.5, size = 1, linetype = "solid", method = "lm", 
              formula = y ~ x, se = T) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = Age_Frac_Imp, y = UG_Decision_RT_log),
              color = "black", size = 1.25,  
              linetype = "solid", method = "lm", formula = y ~ x, se = F) +
  scale_fill_brewer(palette = "Blues",name="Age (Years)") +
  scale_x_continuous(breaks=seq(6,13), lim = c(6,13)) +
  #scale_y_continuous(breaks=seq(0,5,1), lim = c(-0.3,6)) +
  guides(color = "none", fill = "none", shape = "none") +
  ggtitle('UG Responder Decision RT over age') +
  xlab('Age (Years)') +
  ylab('RT (Log)') +
  theme_light() +
  my_theme

MBplot

# Saving plots with fixed dimensions, quality etc.
ggsave("UGRespRTLog_Age_30Jun2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)



################################################################################

### ----- Temporal discounting

################################################################################

DF_imp_T0$Age_round <- round(DF_imp_T0$Age_Frac_Imp)
unique(DF_imp_T0$Age_round)

DF_imp_T0$Age_round <- as.character(DF_imp_T0$Age_round)
unique(DF_imp_T0$Age_round)

DF_imp_T0$Age_round[DF_imp_T0$Age_round == "12"] <- "12+"
DF_imp_T0$Age_round[DF_imp_T0$Age_round == "13"] <- "12+"

DF_imp_T0$Age_round <- factor(DF_imp_T0$Age_round, levels = c("6","7","8","9","10","11","12+"))

### ---------------- Percentage delayed choices

medium_resid <- resid(lm(data=DF_imp_T0, Perc_Delay ~ Online))
age_resid <- resid(lm(data=DF_imp_T0, Age_Frac_Imp ~ Online))


# plotting the single w over age
MBplot <- ggplot(data = DF_imp_T0, fill = Age_round) +
  geom_jitter(data=DF_imp_T0,aes(x = age_resid, y = medium_resid, fill = Age_round),
              shape = 21,stroke = 1, size = 4,height = 0.25) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid), 
              color = "#c5c5c5", fill ="#c5c5c5", 
              alpha = 0.5, size = 1, linetype = "solid", method = "lm", 
              formula = y ~ x, se = T) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid),
              color = "black", size = 1.25,  
              linetype = "solid", method = "lm", formula = y ~ x, se = F) +
  scale_fill_brewer(palette = "Blues",name="Age (Years)") +
  #scale_x_continuous(breaks=seq(-3,,1), lim = c(6,13)) +
  #scale_y_continuous(breaks=seq(0,1,0.25), lim = c(0,1)) +
  guides(color = "none", fill = "none", shape = "none") +
  ggtitle('Percentage Delayed Choices over Age') +
  xlab('Age (residual)') +
  ylab('Percentage (residual)') +
  theme_light() +
  my_theme

MBplot

# Saving plots with fixed dimensions, quality etc.
ggsave("PCDelay_Age_Medium_30Jun2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)

# plotting the single w over age
MBplot <- ggplot(data = DF_imp_T0, fill = Age_round) +
  geom_jitter(data=DF_imp_T0,aes(x = Age_Frac_Imp, y = Perc_Delay, fill = Age_round),
              shape = 21,stroke = 1, size = 4,height = 0.25) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = Age_Frac_Imp, y = Perc_Delay), 
              color = "#c5c5c5", fill ="#c5c5c5", 
              alpha = 0.5, size = 1, linetype = "solid", method = "lm", 
              formula = y ~ x, se = T) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = Age_Frac_Imp, y = Perc_Delay),
              color = "black", size = 1.25,  
              linetype = "solid", method = "lm", formula = y ~ x, se = F) +
  scale_fill_brewer(palette = "Blues",name="Age (Years)") +
  scale_x_continuous(breaks=seq(6,13), lim = c(6,13)) +
  #scale_y_continuous(breaks=seq(0,5,1), lim = c(-0.3,6)) +
  guides(color = "none", fill = "none", shape = "none") +
  ggtitle('Percentage Delayed Choices over Age') +
  xlab('Age (Years)') +
  ylab('Percentage (z-score)') +
  theme_light() +
  my_theme

MBplot

# Saving plots with fixed dimensions, quality etc.
ggsave("DPCDelay_Age_30Jun2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)

### ---------------- TD Avg RT

min(DF_imp_T0$TD_AvgRT_log)

DF_imp_T0$TD_AvgRT_log[DF_imp_T0$TD_AvgRT_log < -5] <- NA

medium_resid <- resid(lm(data=DF_imp_T0, TD_AvgRT_log ~ Online))
age_resid <- resid(lm(data=DF_imp_T0, Age_Frac_Imp ~ Online))


# plotting the single w over age
MBplot <- ggplot(data = DF_imp_T0, fill = Age_round) +
  geom_jitter(data=DF_imp_T0,aes(x = age_resid, y = medium_resid, fill = Age_round),
              shape = 21,stroke = 1, size = 4,height = 0.25) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid), 
              color = "#c5c5c5", fill ="#c5c5c5", 
              alpha = 0.5, size = 1, linetype = "solid", method = "lm", 
              formula = y ~ x, se = T) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid),
              color = "black", size = 1.25,  
              linetype = "solid", method = "lm", formula = y ~ x, se = F) +
  scale_fill_brewer(palette = "Blues",name="Age (Years)") +
  #scale_x_continuous(breaks=seq(-3,,1), lim = c(6,13)) +
  #scale_y_continuous(breaks=seq(0,1,0.25), lim = c(0,1)) +
  guides(color = "none", fill = "none", shape = "none") +
  ggtitle('Average Reaction Time for Choices over Age') +
  xlab('Age (residual)') +
  ylab('RT (Log, residual)') +
  theme_light() +
  my_theme

MBplot

# Saving plots with fixed dimensions, quality etc.
ggsave("TD_Choice_RT_Medium_23Aug2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)

# plotting the single w over age
MBplot <- ggplot(data = DF_imp_T0, fill = Age_round) +
  geom_jitter(data=DF_imp_T0,aes(x = Age_Frac_Imp, y = TD_AvgRT_log, fill = Age_round),
              shape = 21,stroke = 1, size = 4,height = 0.25) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = Age_Frac_Imp, y = TD_AvgRT_log), 
              color = "#c5c5c5", fill ="#c5c5c5", 
              alpha = 0.5, size = 1, linetype = "solid", method = "lm", 
              formula = y ~ x, se = T) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = Age_Frac_Imp, y = TD_AvgRT_log),
              color = "black", size = 1.25,  
              linetype = "solid", method = "lm", formula = y ~ x, se = F) +
  scale_fill_brewer(palette = "Blues",name="Age (Years)") +
  scale_x_continuous(breaks=seq(6,13), lim = c(6,13)) +
  #scale_y_continuous(breaks=seq(0,5,1), lim = c(-0.3,6)) +
  guides(color = "none", fill = "none", shape = "none") +
  ggtitle('Average Reaction Time for Choices over Age') +
  xlab('Age (Years)') +
  ylab('RT (Log)') +
  theme_light() +
  my_theme

MBplot

# Saving plots with fixed dimensions, quality etc.
ggsave("TD_Choice_RT_Age_25Jul2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)

### ---------------- Delay valuation

medium_resid <- resid(lm(data=DF_imp_T0, Delay_WMean ~ Online))
age_resid <- resid(lm(data=DF_imp_T0, Age_Frac_Imp ~ Online))


# plotting the single w over age
MBplot <- ggplot(data = DF_imp_T0, fill = Age_round) +
  geom_jitter(data=DF_imp_T0,aes(x = age_resid, y = medium_resid, fill = Age_round),
              shape = 21,stroke = 1, size = 4,height = 0.25) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid), 
              color = "#c5c5c5", fill ="#c5c5c5", 
              alpha = 0.5, size = 1, linetype = "solid", method = "lm", 
              formula = y ~ x, se = T) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid),
              color = "black", size = 1.25,  
              linetype = "solid", method = "lm", formula = y ~ x, se = F) +
  scale_fill_brewer(palette = "Blues",name="Age (Years)") +
  #scale_x_continuous(breaks=seq(-3,,1), lim = c(6,13)) +
  #scale_y_continuous(breaks=seq(0,1,0.25), lim = c(0,1)) +
  guides(color = "none", fill = "none", shape = "none") +
  ggtitle('Delay Valuation over Age') +
  xlab('Age (residual)') +
  ylab('Delay valuation (residual)') +
  theme_light() +
  my_theme

MBplot

# Saving plots with fixed dimensions, quality etc.
ggsave("DelayValuation_Medium_11Aug2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)


### ---------------- Reward Valuation

medium_resid <- resid(lm(data=DF_imp_T0, Reward_WMean ~ Online))
age_resid <- resid(lm(data=DF_imp_T0, Age_Frac_Imp ~ Online))


# plotting the single w over age
MBplot <- ggplot(data = DF_imp_T0, fill = Age_round) +
  geom_jitter(data=DF_imp_T0,aes(x = age_resid, y = medium_resid, fill = Age_round),
              shape = 21,stroke = 1, size = 4,height = 0.25) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid), 
              color = "#c5c5c5", fill ="#c5c5c5", 
              alpha = 0.5, size = 1, linetype = "solid", method = "lm", 
              formula = y ~ x, se = T) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid),
              color = "black", size = 1.25,  
              linetype = "solid", method = "lm", formula = y ~ x, se = F) +
  scale_fill_brewer(palette = "Blues",name="Age (Years)") +
  #scale_x_continuous(breaks=seq(-3,,1), lim = c(6,13)) +
  #scale_y_continuous(breaks=seq(0,1,0.25), lim = c(0,1)) +
  guides(color = "none", fill = "none", shape = "none") +
  ggtitle('Reward Valuation over Age') +
  xlab('Age (residual)') +
  ylab('Reward Valuation (residual)') +
  theme_light() +
  my_theme

MBplot

# Saving plots with fixed dimensions, quality etc.
ggsave("RewardValuation_Medium_25Jul2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)


### ---------------- Corr r delay valuation

medium_resid <- resid(lm(data=DF_imp_T0, Corr_r_Exp_Delay ~ Online))
age_resid <- resid(lm(data=DF_imp_T0, Age_Frac_Imp ~ Online))


# plotting the single w over age
MBplot <- ggplot(data = DF_imp_T0, fill = Age_round) +
  geom_jitter(data=DF_imp_T0,aes(x = age_resid, y = medium_resid, fill = Age_round),
              shape = 21,stroke = 1, size = 4,height = 0.25) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid), 
              color = "#c5c5c5", fill ="#c5c5c5", 
              alpha = 0.5, size = 1, linetype = "solid", method = "lm", 
              formula = y ~ x, se = T) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid),
              color = "black", size = 1.25,  
              linetype = "solid", method = "lm", formula = y ~ x, se = F) +
  scale_fill_brewer(palette = "Blues",name="Age (Years)") +
  #scale_x_continuous(breaks=seq(-3,,1), lim = c(6,13)) +
  #scale_y_continuous(breaks=seq(0,1,0.25), lim = c(0,1)) +
  guides(color = "none", fill = "none", shape = "none") +
  ggtitle('Delay Valuation Coefficient over Age') +
  xlab('Age (residual)') +
  ylab('Delay Valuation Coefficient (residual)') +
  theme_light() +
  my_theme

MBplot

# Saving plots with fixed dimensions, quality etc.
ggsave("DelayValuationCoeff_Medium_25Jul2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)

### ---------------- Corr int delay valuation

medium_resid <- resid(lm(data=DF_imp_T0, Corr_int_Exp_Delay ~ Online))
age_resid <- resid(lm(data=DF_imp_T0, Age_Frac_Imp ~ Online))


# plotting the single w over age
MBplot <- ggplot(data = DF_imp_T0, fill = Age_round) +
  geom_jitter(data=DF_imp_T0,aes(x = age_resid, y = medium_resid, fill = Age_round),
              shape = 21,stroke = 1, size = 4,height = 0.25) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid), 
              color = "#c5c5c5", fill ="#c5c5c5", 
              alpha = 0.5, size = 1, linetype = "solid", method = "lm", 
              formula = y ~ x, se = T) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid),
              color = "black", size = 1.25,  
              linetype = "solid", method = "lm", formula = y ~ x, se = F) +
  scale_fill_brewer(palette = "Blues",name="Age (Years)") +
  #scale_x_continuous(breaks=seq(-3,,1), lim = c(6,13)) +
  #scale_y_continuous(breaks=seq(0,1,0.25), lim = c(0,1)) +
  guides(color = "none", fill = "none", shape = "none") +
  ggtitle('Delay Valuation Intercept over Age') +
  xlab('Age (residual)') +
  ylab('Delay Valuation Intercept (residual)') +
  theme_light() +
  my_theme

MBplot

# Saving plots with fixed dimensions, quality etc.
ggsave("DelayValuationItc_Medium_25Jul2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)



### ---------------- Reward Valuation coeff

medium_resid <- resid(lm(data=DF_imp_T0, Corr_r_Exp_Reward ~ Online))
age_resid <- resid(lm(data=DF_imp_T0, Age_Frac_Imp ~ Online))


# plotting the single w over age
MBplot <- ggplot(data = DF_imp_T0, fill = Age_round) +
  geom_jitter(data=DF_imp_T0,aes(x = age_resid, y = medium_resid, fill = Age_round),
              shape = 21,stroke = 1, size = 4,height = 0.25) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid), 
              color = "#c5c5c5", fill ="#c5c5c5", 
              alpha = 0.5, size = 1, linetype = "solid", method = "lm", 
              formula = y ~ x, se = T) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid),
              color = "black", size = 1.25,  
              linetype = "solid", method = "lm", formula = y ~ x, se = F) +
  scale_fill_brewer(palette = "Blues",name="Age (Years)") +
  #scale_x_continuous(breaks=seq(-3,,1), lim = c(6,13)) +
  #scale_y_continuous(breaks=seq(0,1,0.25), lim = c(0,1)) +
  guides(color = "none", fill = "none", shape = "none") +
  ggtitle('Reward Valuation Coefficient over Age') +
  xlab('Age (residual)') +
  ylab('Reward Valuation Coefficient (residual)') +
  theme_light() +
  my_theme

MBplot

# Saving plots with fixed dimensions, quality etc.
ggsave("RewardValuationCoeff_Medium_25Jul2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)

### ---------------- Reward Valuation ITC

medium_resid <- resid(lm(data=DF_imp_T0, Corr_int_Exp_Reward ~ Online))
age_resid <- resid(lm(data=DF_imp_T0, Age_Frac_Imp ~ Online))


# plotting the single w over age
MBplot <- ggplot(data = DF_imp_T0, fill = Age_round) +
  geom_jitter(data=DF_imp_T0,aes(x = age_resid, y = medium_resid, fill = Age_round),
              shape = 21,stroke = 1, size = 4,height = 0.25) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid), 
              color = "#c5c5c5", fill ="#c5c5c5", 
              alpha = 0.5, size = 1, linetype = "solid", method = "lm", 
              formula = y ~ x, se = T) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = age_resid, y = medium_resid),
              color = "black", size = 1.25,  
              linetype = "solid", method = "lm", formula = y ~ x, se = F) +
  scale_fill_brewer(palette = "Blues",name="Age (Years)") +
  #scale_x_continuous(breaks=seq(-3,,1), lim = c(6,13)) +
  #scale_y_continuous(breaks=seq(0,1,0.25), lim = c(0,1)) +
  guides(color = "none", fill = "none", shape = "none") +
  ggtitle('Reward Valuation Intercept over Age') +
  xlab('Age (residual)') +
  ylab('Reward Valuation Intercept (residual)') +
  theme_light() +
  my_theme

MBplot

# Saving plots with fixed dimensions, quality etc.
ggsave("RewardValuationIntc_Medium_25Jul2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)




### ---------------- Reward Valuation ITC

# plotting the single w over age
MBplot <- ggplot(data = DF_imp_T0, fill = Age_round) +
  geom_jitter(data=DF_imp_T0,aes(x = S, y = DG_UG_Diff, fill = Age_round),
              shape = 21,stroke = 1, size = 4,height = 0.25) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = S, y = DG_UG_Diff), 
              color = "#c5c5c5", fill ="#c5c5c5", 
              alpha = 0.5, size = 1, linetype = "solid", method = "lm", 
              formula = y ~ x, se = T) +
  stat_smooth(data = DF_imp_T0, inherit.aes = F, aes(x = S, y = DG_UG_Diff),
              color = "black", size = 1.25,  
              linetype = "solid", method = "lm", formula = y ~ x, se = F) +
  scale_fill_brewer(palette = "Blues",name="Age (Years)") +
  #scale_x_continuous(breaks=seq(-3,,1), lim = c(6,13)) +
  #scale_y_continuous(breaks=seq(0,1,0.25), lim = c(0,1)) +
  guides(color = "none", fill = "none", shape = "none") +
  #ggtitle('Reward Valuation Intercept over Age') +
  #xlab('Age (residual)') +
  #ylab('Reward Valuation Intercept (residual)') +
  theme_light() +
  my_theme

MBplot

# Saving plots with fixed dimensions, quality etc.
ggsave("RewardValuationIntc_Medium_25Jul2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)


######### PLOT FOR UG ACCEPT
### TO - T1 

accept_sum = summarySE(data = TD_T0_T1, measurevar = "UG_Offer_Accept", groupvars = c("SessionFactor","Group"))


ggplot(data = TD_T0_T1, aes(x = SessionFactor, y = UG_Offer_Accept, fill = Group)) +
  geom_violin(alpha = 0.3,color="white") +
  geom_errorbar(data = accept_sum, aes(x = SessionFactor, ymin=UG_Offer_Accept-ci,ymax=UG_Offer_Accept+ci),
                position="dodge",lwd=1) +
  geom_point(data = accept_sum, aes(x = SessionFactor, y = UG_Offer_Accept, fill = Group),
             size = 4, shape = 21,position=position_dodge(width=0.9)) +
  scale_color_manual(name= "Group", labels = c("Con","Exp"), values=my_colors) +
  scale_fill_manual(name= "Group", labels = c("Con","Exp"), values=my_colors) +
  scale_x_discrete(name = "Session",labels = c("Pre", "Post")) +
  scale_y_continuous(name="Probability to accept") +
  theme_light() +
  my_theme


# Saving plots with fixed dimensions, quality etc.
ggsave("UG_Offer_Accept_prepost_23Jun2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)


accept_sum = summarySE(data = TD_T0_T2, measurevar = "UG_Offer_Accept", groupvars = c("SessionFactor","Group"))


ggplot(data = TD_T0_T2, aes(x = SessionFactor, y = UG_Offer_Accept, fill = Group)) +
  geom_violin(alpha = 0.3,color="white") +
  geom_errorbar(data = accept_sum, aes(x = SessionFactor, ymin=UG_Offer_Accept-ci,ymax=UG_Offer_Accept+ci),
                position="dodge",lwd=1) +
  geom_point(data = accept_sum, aes(x = SessionFactor, y = UG_Offer_Accept, fill = Group),
             size = 4, shape = 21,position=position_dodge(width=0.9)) +
  scale_color_manual(name= "Group", labels = c("Con","Exp"), values=my_colors) +
  scale_fill_manual(name= "Group", labels = c("Con","Exp"), values=my_colors) +
  scale_x_discrete(name = "Session",labels = c("Pre-training", "Follow-up")) +
  scale_y_continuous(name="Probability to accept") +
  theme_light() +
  my_theme


# Saving plots with fixed dimensions, quality etc.
ggsave("UG_Offer_Accept_pre_followup_23Jun2022.png", plot = last_plot(), path = plots_folder,
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)
