##KG FINAL MIXED MODEL when variables are defined

b<-lmer(mean_SSRT2 ~ Tot_session + (1 | Participant),REML = FALSE,  data=Exp_SSRT2)
anova(b)
b<-lmer(mean_all_goRT ~ Tot_session + (1 | Participant),REML = FALSE,  data=Con_train)
anova(b)

###### MAIN STUDY TRAINING DATA ANALYSIS 

# Date: 12/05/2021
# Author: Claire R. Smid

# paper to follow
# https://www.jneurosci.org/content/34/1/149
# multilevel model specification: http://www.rensenieuwenhuis.nl/r-sessions-16-multilevel-model-specification-lme4/
# multivariate analysis: https://mac-theobio.github.io/QMEE/MultivariateMixed.html

# This analysis will:
# - work with the cleaned training data from the Matlab scripts
# - make the graphs as they are in the paper
# - run the mixed models as they are reported in the paper.

# things to do in this script

# 1 - make graphs
# 2. some basic stats:
#   2.1 - compare first - last training sessions. correlate this with pre-post changes
#   2.2 - check number of sessions (per participant per group?)
#   2.3 - how many bonus games completed?
# 3. mixed models


### REMAINING TO DO'S:
# control group analyses (multilevel models for go reaction time, corr reaction time, stimdur, covvar of RT)
# export individual regression slopes
# repeat experimental group analyses with the latest model / data 


# clears workspace
# rm(list=ls())

# get necessary packages
library(ggplot2)
library(lattice)
library(reshape2)
library(ggrepel)
library(dplyr)
library(tidyr)
library(rlang)

#install.packages("cowplot")
library(gridExtra)
library(cowplot)

# for mlm 
library(lme4)
library(mlmRev)

# for stats
#install.packages('psych')
library(psych)
library(Rmisc)

#install.packages("extrafont")
library(extrafont)
#font_import()
#loadfonts(device="win")       #Register fonts for Windows bitmap output
#loadfonts(device="pdf")       #Register fonts for Windows bitmap output
fonts()           


###############################################################################################
### 9 July 2020 ---- IMPORT TRAINING DATA
# import data (new method of cleaning - logged in Matlab files)

## EXPERIMENTAL
Exp_train <- read.csv('Clean_EXP_SSRT_V1.csv')

# CONTROL
Con_train <- read.csv('Clean_FINAL_CON_output_M2.csv')

# probability of false alarm
# percentage of correct go trials
# some motivation measures

Exp_train <- Exp_train[Exp_train$Participant != 9999, ]

mean(Exp_train$mean_SSRT0,na.rm=T)
mean(Exp_train$mean_SSRT1,na.rm=T)
mean(Exp_train$mean_SSRT2,na.rm=T)
mean(Exp_train$mean_SSRT3,na.rm=T)

# define groups / factors
Exp_train <- within(Exp_train,{
  Participant <- as.factor(Participant)
  #Tot_session <- as.factor(Tot_session)
})

Exp_train <- data.frame(Exp_train)

Exp_SSRT2 <- Exp_train[!is.na(Exp_train$mean_SSRT2), ]
Exp_SSRT2 <- Exp_SSRT2[Exp_SSRT2$mean_SSRT2 > 0, ]


# clean for control too
Con_train <- Con_train[Con_train$Participant != 9999, ]

mean(Con_train$mean_all_goRT,na.rm=T)
mean(Con_train$mean_all_goRT_2SD,na.rm=T)
mean(Con_train$mean_Corr_goRT,na.rm=T)
mean(Con_train$mean_Corr_goRT_2SD,na.rm=T)
mean(Con_train$CovVar_Corr_goRT,na.rm=T)
mean(Con_train$CovVar_Corr_goRT_2SD,na.rm=T)
mean(Con_train$mean_Stim_dur,na.rm=T)

# define groups / factors
Con_train <- within(Con_train,{
  Participant <- as.factor(Participant)
  #Tot_session <- as.factor(Tot_session)
})

Con_train <- data.frame(Con_train)

#

Exp_SSRT2 <- Exp_train[!is.na(Exp_train$mean_SSRT2), ]
Exp_SSRT2 <- Exp_SSRT2[Exp_SSRT2$mean_SSRT2 > 0, ]

########################################### 
########################################### SSRT 2 WITH INDIVIDUAL TIME LINES OVERLAY
# create means per session over the games
# HERE KEERTANA

Exp_SSRT2 <- Exp_train[!is.na(Exp_train$mean_SSRT2), ]
Exp_SSRT2 <- Exp_SSRT2[Exp_SSRT2$mean_SSRT2 > 0, ]

#longsesh <- Exp_SSRT1[Exp_SSRT1$Tot_session > 25, ]
#longsesh <- longsesh[complete.cases(longsesh),]

Exp_M2 <- summarySE(data = Exp_SSRT2, measurevar = "mean_SSRT2", groupvars = c("Tot_session"))
Exp_ID2 <- summarySE(data = Exp_SSRT2, measurevar = "mean_SSRT2", groupvars = c("Participant","Tot_session"))

# a <- lm(mean_SSRT2 ~ Tot_session, data = Exp_M2)
a<-lmer(mean_SSRT2 ~ Tot_session + (1 | Participant),REML = FALSE,  data=Exp_M2)
summary(a)

Exp_M2$Tot_session <- as.numeric(Exp_M2$Tot_session)
Exp_ID2$Tot_session <- as.numeric(Exp_ID2$Tot_session)

# check if NAs
max(Exp_ID2$mean_SSRT2)
min(Exp_ID2$mean_SSRT2)

# check mean
max(Exp_ID2$mean_SSRT2,na.rm = T)
min(Exp_ID2$mean_SSRT2,na.rm = T)

# check if NAs
max(Exp_M2$mean_SSRT2)
min(Exp_M2$mean_SSRT2)

## for EXP M 
max(Exp_M2$mean_SSRT2,na.rm = T)
min(Exp_M2$mean_SSRT2,na.rm = T)


### plot for the mean values over session, rather than per ID
EXP_SSRT2 <- ggplot(data = Exp_M2, aes(x = Tot_session, y = mean_SSRT2)) +
  geom_line(data = Exp_ID2, aes(x = Tot_session, y = mean_SSRT2, 
                               group = Participant),linetype = 'dotted',colour = 'black',lwd=0.5, alpha = 0.5) +
  stat_smooth(data = Exp_M2, aes(x = Tot_session, y = mean_SSRT2),size = 1, alpha = 0.4,
              color = "black", linetype = "solid", method = "lm", formula = y ~ x, se = T) +
  geom_point(data = Exp_M2, aes(x = Tot_session, y = mean_SSRT2),size = 5, stroke = 1.5, shape = 1) +
  #geom_line(stat="identity") +
  guides(color = FALSE, linetype = FALSE) +
  scale_y_continuous(breaks = seq(0,1250,250),lim = c(0,1250)) +
  #coord_cartesian(ylim = c(0,600)) +
  scale_x_continuous(breaks = seq(0,32,2), lim = c(0,32)) +
  ggtitle('Mean SSRT - Integration (with replacement)') +
  #ggtitle('Mean SSRT per session and per participant for the\nresponse inhibition group') +
  xlab('Sessions') +
  ylab('SSRT (ms)') +
  theme_light() +
  graph_theme

EXP_SSRT2

# Saving plots with fixed dimensions, quality etc.
ggsave("Exp_SSRT2_Time_Means.png", plot = last_plot(), path = '/Users/claire.smid/Documents/Main_STUDY/TrainingData/',
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)

########################################### 
########################################### SSD WITH INDIVIDUAL TIME LINES OVERLAY

# create means per session over the games
# UPDATE used Exp_SSRT2 here instead of EXP_train - because I think it's better?

Exp_SSD_M <- summarySE(data = Exp_SSRT2, measurevar = "mean_SSD", groupvars = c("Tot_session"))
Exp_SSD_ID <- summarySE(data = Exp_SSRT2, measurevar = "mean_SSD", groupvars = c("Participant","Tot_session"))

Exp_SSD_M$Tot_session <- as.numeric(Exp_SSD_M$Tot_session)
Exp_SSD_ID$Tot_session <- as.numeric(Exp_SSD_ID$Tot_session)

### plot for the mean values over session, rather than per ID
EXP_SSD <- ggplot(data = Exp_SSD_ID, aes(x = Tot_session, y = mean_SSD)) +
  geom_line(data = Exp_SSD_ID, aes(x = Tot_session, y = mean_SSD, 
                                group = Participant),linetype = 'dotted',colour = 'black',lwd=0.5, alpha = 0.5) +
  stat_smooth(data = Exp_SSD_ID, aes(x = Tot_session, y = mean_SSD),size = 1, alpha = 0.4,
              color = "black", linetype = "solid", method = "lm", formula = y ~ x, se = T) +
  geom_point(data = Exp_SSD_M, aes(x = Tot_session, y = mean_SSD),size = 5, stroke = 1.5, shape = 1) +
  #geom_line(stat="identity") +
  guides(color = FALSE, linetype = FALSE) +
  #scale_y_continuous(breaks = seq(0,1000,250),lim = c(0,1000)) +
  #coord_cartesian(ylim = c(0,600)) +
  scale_x_continuous(breaks = seq(0,32,2), lim = c(0,32)) +
  ggtitle('SSD') +
  #ggtitle('Mean SSRT per session and per participant for the\nresponse inhibition group') +
  xlab('Sessions') +
  ylab('SSD (ms)') +
  theme_light() +
  graph_theme

EXP_SSD 

# Saving plots with fixed dimensions, quality etc.
ggsave("EXP_SSD_ID_Means.png", plot = last_plot(), path = '/Users/claire.smid/Documents/Main_STUDY/TrainingData/',
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)

#### OVER WEEKS
# create means per session over the games
Exp_SSD_M <- summarySE(data = Exp_SSRT2, measurevar = "mean_SSD", groupvars = c("Weeks"))
Exp_SSD_ID <- summarySE(data = Exp_SSRT2, measurevar = "mean_SSD", groupvars = c("Participant","Weeks"))

Exp_SSD_M$Weeks <- as.numeric(Exp_SSD_M$Weeks)
Exp_SSD_ID$Weeks <- as.numeric(Exp_SSD_ID$Weeks)

### plot for the mean values over session, rather than per ID
EXP_SSD <- ggplot(data = Exp_SSD_ID, aes(x = Weeks, y = mean_SSD)) +
  geom_line(data = Exp_SSD_ID, aes(x = Weeks, y = mean_SSD, 
                                   group = Participant),linetype = 'dotted',colour = 'black',lwd=0.5, alpha = 0.5) +
  stat_smooth(data = Exp_SSD_ID, aes(x = Weeks, y = mean_SSD),size = 1, alpha = 0.4,
              color = "black", linetype = "solid", method = "lm", formula = y ~ x, se = T) +
  geom_point(data = Exp_SSD_M, aes(x = Weeks, y = mean_SSD),size = 5, stroke = 1.5, shape = 1) +
  #geom_line(stat="identity") +
  guides(color = FALSE, linetype = FALSE) +
  #scale_y_continuous(breaks = seq(0,1000,250),lim = c(0,1000)) +
  #coord_cartesian(ylim = c(0,600)) +
  #scale_x_continuous(breaks = seq(0,32,2), lim = c(0,32)) +
  ggtitle('SSD') +
  #ggtitle('Mean SSRT per session and per participant for the\nresponse inhibition group') +
  xlab('Sessions') +
  ylab('SSD (ms)') +
  theme_light() +
  graph_theme

EXP_SSD 

# Saving plots with fixed dimensions, quality etc.
ggsave("EXP_SSD_ID_Weeks_Means.png", plot = last_plot(), path = '/Users/claire.smid/Documents/Main_STUDY/TrainingData/',
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)





########################################### 
########################################### go reaction times

# create means per session over the games
Exp_goRT <- summarySE(data = Exp_SSRT2, measurevar = "mean_all_goRT", groupvars = c("Tot_session"))
Exp_goRT_ID <- summarySE(data = Exp_SSRT2, measurevar = "mean_all_goRT", groupvars = c("Participant","Tot_session"))

Exp_goRT$Tot_session <- as.numeric(Exp_goRT$Tot_session)
Exp_goRT_ID$Tot_session <- as.numeric(Exp_goRT_ID$Tot_session)

### plot for the mean values over session, rather than per ID
EXP_goRT <- ggplot(data = Exp_goRT_ID, aes(x = Tot_session, y = mean_all_goRT)) +
  geom_line(data = Exp_goRT_ID, aes(x = Tot_session, y = mean_all_goRT, 
                                   group = Participant),linetype = 'dotted',colour = 'black',lwd=0.5, alpha = 0.5) +
  stat_smooth(data = Exp_goRT, aes(x = Tot_session, y = mean_all_goRT),size = 1, alpha = 0.4,
              color = "black", linetype = "solid", method = "lm", formula = y ~ x, se = T) +
  geom_point(data = Exp_goRT, aes(x = Tot_session, y = mean_all_goRT),size = 5, stroke = 1.5, shape = 1) +
  #geom_line(stat="identity") +
  guides(color = FALSE, linetype = FALSE) +
  #scale_y_continuous(breaks = seq(0,1000,250),lim = c(0,1000)) +
  #coord_cartesian(ylim = c(0,600)) +
  scale_x_continuous(breaks = seq(0,32,2), lim = c(0,32)) +
  ggtitle('Mean go reaction time (all trials - including go-errors and premature resp)') +
  #ggtitle('Mean SSRT per session and per participant for the\nresponse inhibition group') +
  xlab('Sessions') +
  ylab('Reaction Time (ms)') +
  theme_light() +
  graph_theme

EXP_goRT 

# Saving plots with fixed dimensions, quality etc.
ggsave("EXP_goRT_Time_Means.png", plot = last_plot(), path = '/Users/claire.smid/Documents/Main_STUDY/TrainingData/',
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)


### plot for the mean values over session, rather than per ID
EXP_SSRT2 <- ggplot(data = Exp_M2, aes(x = Tot_session, y = mean_SSRT2)) +
  geom_point(data = Exp_M2, aes(x = Tot_session, y = mean_SSRT2),size = 2, stroke = 1.5, shape = 1) +
  geom_point(data = Exp_M2, aes(x = Tot_session, y = predYsvm),color = 'red',size = 1, stroke = 1.5, shape = 1) +
  #geom_line(stat="identity") +
  guides(color = FALSE, linetype = FALSE) +
  scale_y_continuous(breaks = seq(0,1250,250),lim = c(0,1250)) +
  #coord_cartesian(ylim = c(0,600)) +
  scale_x_continuous(breaks = seq(0,32,2), lim = c(0,32)) +
  ggtitle('Mean SSRT - Integration (with replacement)') +
  #ggtitle('Mean SSRT per session and per participant for the\nresponse inhibition group') +
  xlab('Sessions') +
  ylab('SSRT (ms)') +
  theme_light() +
  graph_theme

EXP_SSRT2

# Saving plots with fixed dimensions, quality etc.
ggsave("EXP_SVM_Time_Means_Example.png", plot = last_plot(), path = '/Users/claire.smid/Documents/Main_STUDY/TrainingData/',
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)


### INDIVIDUAL LINES INCLUDED
EXP_SSRT2 <- ggplot(data = Exp_M2, aes(x = Tot_session, y = mean_SSRT2)) +
  geom_line(data = Exp_ID2, aes(x = Tot_session, y = mean_SSRT2, 
                                group = Participant),linetype = 'dotted',colour = 'black',lwd=0.5, alpha = 0.5) +
  geom_point(data = Exp_M2, aes(x = Tot_session, y = mean_SSRT2),size = 2, stroke = 1.5, shape = 1) +
  geom_point(data = Exp_M2, aes(x = Tot_session, y = predYsvm),color = 'red',size = 1, stroke = 1.5, shape = 1) +
  #geom_line(stat="identity") +
  guides(color = FALSE, linetype = FALSE) +
  scale_y_continuous(breaks = seq(0,1250,250),lim = c(0,1250)) +
  #coord_cartesian(ylim = c(0,600)) +
  scale_x_continuous(breaks = seq(0,32,2), lim = c(0,32)) +
  ggtitle('Mean SSRT - Integration (with replacement)') +
  #ggtitle('Mean SSRT per session and per participant for the\nresponse inhibition group') +
  xlab('Sessions') +
  ylab('SSRT (ms)') +
  theme_light() +
  graph_theme

EXP_SSRT2

# Saving plots with fixed dimensions, quality etc.
ggsave("EXP_SVM_Time_Means_andID_Example.png", plot = last_plot(), path = '/Users/claire.smid/Documents/Main_STUDY/TrainingData/',
       scale = 1, width = 20, height = 16, units = "cm",
       dpi = 300)



# #Regression with SVM - this is not working per ID yet - need to add
# modelsvm = svm(mean_SSRT2~Tot_session,Exp_ID2)
# 
# #Predict using SVM regression
# predYsvm = predict(modelsvm, Exp_ID2)
# 
# ### plot for the mean values over session, rather than per ID
# EXP_SSRT2 <- ggplot(data = Exp_ID2, aes(x = Tot_session, y = mean_SSRT2)) +
#   geom_line(data = Exp_ID2, aes(x = Tot_session, y = mean_SSRT2, 
#                                 group = Participant),linetype = 'dotted',colour = 'black',lwd=0.5, alpha = 0.5) +
#   #stat_smooth(data = Exp_ID2, aes(x = Tot_session, y = mean_SSRT2),size = 1, alpha = 0.4,
#   #            color = "black", linetype = "solid", method = "lm", formula = y ~ x, se = T) +
#   geom_point(data = Exp_ID2, aes(x = Tot_session, y = mean_SSRT2),size = 5, stroke = 1.5, shape = 1) +
#   geom_point(data = Exp_ID2, aes(x = Tot_session, y = predYsvm),color = 'red',size = 1, stroke = 1.5, shape = 1) +
#   #geom_line(stat="identity") +
#   guides(color = FALSE, linetype = FALSE) +
#   scale_y_continuous(breaks = seq(0,1250,250),lim = c(0,1250)) +
#   #coord_cartesian(ylim = c(0,600)) +
#   scale_x_continuous(breaks = seq(0,32,2), lim = c(0,32)) +
#   ggtitle('Mean SSRT - Integration (with replacement)') +
#   #ggtitle('Mean SSRT per session and per participant for the\nresponse inhibition group') +
#   xlab('Sessions') +
#   ylab('SSRT (ms)') +
#   theme_light() +
#   graph_theme
# 
# EXP_SSRT2
# 
# # Saving plots with fixed dimensions, quality etc.
# ggsave("EXP_SVM_ID_Means_Example.png", plot = last_plot(), path = '/Users/claire.smid/Documents/Main_STUDY/TrainingData/',
#        scale = 1, width = 20, height = 16, units = "cm",
#        dpi = 300)
