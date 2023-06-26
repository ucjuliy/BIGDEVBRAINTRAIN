#interaction training and covid
#SQD & AES

setwd("~/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/training study data/WASI")

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
library(sjPlot)

#load file
data <- read.csv("wasi.csv")
data <- data[data$Session %in% c("0", "1"), ]


#as factor
data$ID <- factor(data$ID)
data$Session <- factor(data$Session)
data$Group <- factor(data$Group)
data$Gender <- factor(data$Gender)
data$Age_YM <- factor(data$Age_YM)


#WASI
formula <- T_ScoreSum ~ Session*Group + Age_YM + Gender + (1|ID)
anova(lmer( formula, data=data, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data, REML=TRUE)
print(emmeans(model, pairwise~Session*Group, adjust = "bonferroni"))


#plot
my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data, measurevar="T_ScoreSum", groupvars=c("Session","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)
WASI <- ggplot(data, aes(x = Session, y = T_ScoreSum, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = Session, y =T_ScoreSum, colour = Group, ), show.legend = TRUE,
             position = position_jitter(width = .01), size = .5, shape = 10)+ scale_y_continuous(limits = c(40, 200)) +
  geom_line(data = tgc, aes(x = Session, y = T_ScoreSum, group = Group, colour = Group), linetype = 3, size = 1)+
  geom_point(data = tgc, aes(x = Session, y = T_ScoreSum, group = Group, colour = Group), shape = 17, size = 5) +
  geom_errorbar(data = tgc, aes(x = Session, y = T_ScoreSum, group = Group, colour = Group, ymin = T_ScoreSum-se, ymax = T_ScoreSum+se), width = .1)+
  theme_classic() + 
  xlab("Session") + 
  ylab("WASI Total") +
  scale_x_discrete(labels=c("0" = "pre", "1" = "post"))+
  scale_colour_manual(labels=c("1" = "experimental", "2" = "control"), values = c("#CC6666","#9999CC"))+
  scale_fill_manual(labels=c("1" = "experimental", "2" = "control"), values = c("#CC6666","#9999CC"))+ theme(text = element_text(size = 30))+
  ggtitle("")+
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.5)) +theme(text = element_text(size=30))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 20))
WASI
ggsave('plot_WASI.jpg', width = 10, height = 8)




#FSIQ
formula <- FSIQ ~ Session*Group + (1|ID)
anova(lmer( formula, data=data, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data, REML=TRUE)
print(emmeans(model, pairwise~Session*Group, adjust = "bonferroni"))


#plot
my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data, measurevar="FSIQ", groupvars=c("Session","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)
FSIQ <- ggplot(data, aes(x = Session, y = FSIQ, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = Session, y =FSIQ, colour = Group, ), show.legend = TRUE,
             position = position_jitter(width = .01), size = .5, shape = 10)+ scale_y_continuous(limits = c(40, 200)) +
  geom_line(data = tgc, aes(x = Session, y = FSIQ, group = Group, colour = Group), linetype = 3, size = 1)+
  geom_point(data = tgc, aes(x = Session, y = FSIQ, group = Group, colour = Group), shape = 17, size = 5) +
  geom_errorbar(data = tgc, aes(x = Session, y = FSIQ, group = Group, colour = Group, ymin = FSIQ-se, ymax = FSIQ+se), width = .1)+
  theme_classic() + 
  xlab("Session") + 
  ylab("FSIQ Total") +
  scale_x_discrete(labels=c("0" = "pre", "1" = "post"))+
  scale_colour_manual(labels=c("1" = "experimental", "2" = "control"), values = c("#CC6666","#9999CC"))+
  scale_fill_manual(labels=c("1" = "experimental", "2" = "control"), values = c("#CC6666","#9999CC"))+ theme(text = element_text(size = 30))+
  ggtitle("")+
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.5)) +theme(text = element_text(size=30))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 20))
FSIQ
ggsave('plot_FSIQ.jpg', width = 10, height = 8)

