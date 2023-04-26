# This script analyses RSFC within-network data

# Set working directory
#setwd("Z:/TrainingData/talk_niko")

# Load packages
library(tidyverse)
library(readxl)
library(emmeans)
library(lme4)
library(lmerTest)
library(MuMIn)
library(effectsize)
library(ggeffects)
library(psych)
library(afex)
library(meta)
library(lm.beta)
library(ggplot2)

# Turn off scientific notation
options(scipen = 999)

# Read in dataset
data <- read.csv("motivation_new.csv")
data$ID <- as.factor(data$ID)
data$Session <- as.factor(data$Session)
data$Week <- factor(data$Session, levels = c( "2","3","4","5","6","7","8"))
data = subset(data, select = -c(Score_raw))

#do partial eta squared
motivationEffect <- lmer(Score ~ as.factor(Groups)*as.factor(Week) + (1 | ID), data = data, REML = F ) #define & run model
anova(motivationEffect) #test effect of bigger model sequentially, start from empty model and keep adding variables (order variables may be important!)
eta_squared(motivationEffect, partial = TRUE)
            
library(tidyverse)
# creating 'week 9' which is average so I can plot it with rest of the weeks 
k<-data %>% 
  group_by(ID) %>% 
  summarise(Score = mean(Score))
k$Week<-'9'
k$Session<-'9'
data<-rbind(data,k)
data$Week <- factor(data$Week, levels = c("2","3","4","5","6","7","8", "9"))
dat<-read.csv("motivation.csv")
dat$Groups <- factor(dat$Groups, levels = c("Experimental", "Control"))
dat = subset(dat, select = -c(Score))
# dat <- dat[dat$Session %in% c("2"), ]
dat = subset(dat, select = -c(Session)) 
data <- merge(data,dat,by=c( "ID"))
data<-data[!duplicated(data), ]
describe(data)
data$Session <- as.factor(data$Session)


# Stats & plot motivation for each groups
# my_palette = (c("#404080","#69b3a2"))
# t.test(data$Score~data$Groups)
# p0<-ggplot(data, aes(x=Groups, y=Score, fill=Groups)) + # fill=name allow to automatically dedicate a color for each group
#   geom_violin(width=0.5, color=NA) +
#   geom_boxplot(width=0.1, color="black", alpha=0.2)+theme_classic()+scale_y_continuous(limits = c(1,7), breaks=c(1,2,3,4,5,6,7), expand = c(0,0))+
#   theme(text = element_text(size=15))+scale_fill_manual(values = my_palette)+ggtitle("a")
# #ggsave("motivation_groups_new.png")

my_palette = (c("#404080","#69b3a2"))

# Stats & plot motivation for each group & week

emmeans(motivationEffect, list(pairwise ~ Week), adjust = "bonferroni") #post-hoc main effect
# p1<-ggplot(data, aes(x=Week, y=Score, fill=Groups))+ # fill=name allow to automatically dedicate a color for each group
#   geom_violin(width=0.5, color=NA, position="dodge")+
#   geom_boxplot(width=0.5, color="black", alpha=0.2)+
# theme_classic()+scale_y_continuous(limits = c(1,7), breaks=c(1,2,3,4,5,6,7), expand = c(0,0))+
#   theme(text = element_text(size=15))+scale_fill_manual(values = my_palette)+ggtitle("a")+
#   ylab("Motivation scores")+
# scale_x_discrete(labels=c("2" = "2", "3" = "3", "4" ="4", "5" = "5", "6"="6", "7"="7", "8"="8", "9"= "mean"))
# #ggsave("motivation_groups_weeks_new.png")


# total sessions comparison 
## EXPERIMENTAL
data_sess<-read.csv(file = "C:/Users/keertana/Documents/Latent Score Modelling/RecombinedIMPUTED_T2addedtooriginal.csv")
# data_sess$group <- factor(data_sess$group, levels = c("Experimental", "Control"))
# only need one datapoint
data_sess <- data_sess[data_sess$session %in% c("t0"), ]
data_sess<-data_sess[complete.cases(data_sess[ ,4]),]
data_sess<-data_sess[complete.cases(data_sess[ ,6]),]

my_palette = (c("#404080","#69b3a2"))
t.test(data_sess$totalsessions~data_sess$group)
ttestBF(formula = totalsessions~group, data = data_sess)
ttestBF(formula = totalsessions ~ group, data = data_sess)

data_sess$Group = factor(data_sess$group, levels = c("Exp", "Con"), labels = c('Experimental', 'Control'))

# p0<-ggplot(data_sess, aes(x=Group, y=totalsessions, fill=Group)) + # fill=name allow to automatically dedicate a color for each group
#   geom_violin(width=0.5, color=NA) +
#   geom_boxplot(width=0.1, color="black", alpha=0.2)+theme_classic()+scale_y_continuous(limits = c(0,35), expand = c(0,0),breaks=seq(0,35,by=5))+theme_classic()+
#   theme(text = element_text(size=15))+scale_fill_manual(values = my_palette)+ 
#   ylab("Total sessions")+ggtitle("b")
# #ggsave("motivation_groups_new.png")
# p0
# 
# p1 <- ggplot(data, aes(x = Session, y = Score, fill = Groups)) +
#   geom_flat_violin(aes(fill = Groups),position = position_nudge(x = .1,
#                                                                 y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
#   geom_point(aes(x = as.numeric(Session)-.15, y = Score, colour = Groups), show.legend = TRUE,
#              position = position_jitter(width = .05), size = 2, shape = 20)+ scale_y_continuous(limits = c(1, 7), expand = c(0,0))+
#   geom_boxplot(aes(x = Session, y = Score, fill = Groups),outlier.shape= NA, alpha = .5, width = .1, colour = "black")+
#   theme_classic() + 
#   xlab("Session") + 
#   ylab("Motivation scores ") +
#   scale_x_discrete(labels=c("2" = "2", "3" = "3", "4" ="4", "5" = "5", "6"="6", "7"="7", "8"="8", "9"= "mean"))+
#   scale_colour_manual(values = my_palette)+
#   scale_fill_manual(values = my_palette)+ theme(text = element_text(size = 25))+ 
#   ggtitle("a")
#ggsave('Raincloudplot.png', width = w, height = h)
#coord_flip()+
p0 <- ggplot(data_sess, aes(x=Group, y=totalsessions, fill=Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = TRUE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(Group)-.15, y =totalsessions, colour = Group), show.legend = TRUE,
             position = position_jitter(width = .05), size = 2, shape = 20)+ scale_y_continuous(limits = c(0, 40), expand = c(0,0))+
  geom_boxplot(aes(x = Group, y = totalsessions, fill = Group),outlier.shape= NA, alpha = .5, width = .1, colour = "black")+
  theme_classic() + 
  xlab("Group") + 
  ylab("Sessions") +
  # scale_x_discrete(labels=c("0" = "pre-test", "1" = "post-test", "2" = "1-year-follow-up"))+
  scale_colour_manual(values = my_palette)+
  scale_fill_manual(values = my_palette)+ theme(text = element_text(size = 25))+
  ggtitle("b")
#ggsave('Raincloudplot.png', width = w, height = h)
#coord_flip()+
p0
# ggsave('Raincloudplot_sessions.jpg', width = 15, height = 10)

p1 <- ggplot(data, aes(x = Session, y = Score, fill = Groups)) +
  geom_flat_violin(aes(fill = Groups),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(Session)-.15, y = Score, colour = Groups), show.legend = TRUE,
             position = position_jitter(width = .05), size = 2, shape = 20)+ scale_y_continuous(limits = c(1, 7), expand = c(0,0))+
  geom_boxplot(aes(x = Session, y = Score, fill = Groups),outlier.shape= NA, alpha = .5, width = .15, colour = "black")+
  theme_classic() + 
  xlab("Session") + 
  ylab("Motivation scores ") +
  scale_x_discrete(labels=c("2" = "2", "3" = "3", "4" ="4", "5" = "5", "6"="6", "7"="7", "8"="8", "9"= "mean"))+
  scale_colour_manual(values = my_palette)+
  scale_fill_manual(values = my_palette)+ theme(text = element_text(size = 25))+ 
  ggtitle("a")
#ggsave('Raincloudplot.png', width = w, height = h)
#coord_flip()+
p1
# ggsave('Raincloudplot_motivation.jpg', width = 15, height = 10)

ggarrange(p1, p0, common.legend = TRUE, legend="bottom", ncol=2,nrow=1, widths = c(1, .3))
ggsave('Motivation_NEWER.jpg', width = 20, height = 10)




###initial motivation 
p<-filter(data, Week == "2")
t.test(p$Score~p$Groups)
ttestBF(formula = Score ~ Groups, data = p)

