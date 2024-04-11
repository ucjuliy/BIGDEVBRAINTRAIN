#interaction training and covid
#SQD & AES

setwd("/Users/zoeli/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/training study/covid data")
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
#aes <- read.csv("aes.csv")
#sdq <- read.csv("sdq.csv")
#covariates <- read.csv("covariates_of_interest.csv")
#data.q <- merge(aes, sdq, by = c("id","timepoint","cov"),
#                all.x = TRUE, all.y = TRUE)
#data.q <- merge(data.q, covariates, by = c("id"),
#                all.x = TRUE, all.y = TRUE)
#write.csv(data.q, "data_aessdq.csv")
data.cov <- read.csv("data_aessdq.csv")
#casi <- read.csv("casi.csv")
#data.q <- merge(data.cov, casi, by = c("id","timepoint","cov"),
#                all.x = TRUE, all.y = TRUE)
#data.q <-subset(data.q, timepoint != "NA" )
#write.csv(data.q, "data_whole.csv")
data <- read.csv("data_whole.csv")

#covid.data <- read.csv("StressCovid.csv")
#covariates <- read.csv("covariates_of_interest.csv")
#stress.cov <- merge(covid.data, covariates, by = c("id"),
#                all.x = TRUE, all.y = TRUE)
#write.csv(stress.cov, "StressCovid.csv")



#perceived stress covid
stress.cov <- read.csv("StressCovid.csv")
stress.cov$id <- factor(stress.cov$id)
stress.cov$timepoint <- factor(stress.cov$timepoint)
stress.cov$cov <- factor(stress.cov$cov)
stress.cov$Group <- factor(stress.cov$Group)

formula <- perceivedstress ~ cov*Group + Age + Gender + (1|id)
anova(lmer( formula, data=stress.cov, REML=TRUE))  #check if is significant
model<-lmer( formula, data=stress.cov, REML=TRUE)
eta_squared(model, partial = TRUE)
print(emmeans(model, pairwise~Group, adjust = "bonferroni"))
stress.cov <- subset(stress.cov, perceivedstress != "NA" )
bf = anovaBF(perceivedstress ~ cov*Group, data=stress.cov, whichRandom = "id")
bf[4] / bf[3]




#as factor
data$id <- factor(data$id)
data$timepoint <- factor(data$timepoint)
data$cov <- factor(data$cov)
data$Gender <- factor(data$Gender)
data$Group <- factor(data$Group)
data.q$id <- factor(data.q$id)
data.q$timepoint <- factor(data.q$timepoint)
data.q$cov <- factor(data.q$cov)
data.q$Gender <- factor(data.q$Gender)
data.q$Group <- factor(data.q$Group)



#effect of intervention
formula <- total_aes ~ cov*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data, REML=TRUE)
eta_squared(model, partial = TRUE)
print(emmeans(model, pairwise~cov*Group, adjust = "bonferroni"))
data.q <- subset(data.q, total_aes != "NA" )
bf = anovaBF(total_aes ~ cov*Group, data=data.q, whichRandom = "id")
bf[4] / bf[3]

#plot
my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data.q, measurevar="total_aes", groupvars=c("cov","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)
aes_cov <- ggplot(data.q, aes(x = cov, y = total_aes, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = cov, y =total_aes, colour = Group, ), show.legend = TRUE,
             position = position_jitter(width = .01), size = .5, shape = 10)+ scale_y_continuous(limits = c(10, 80)) +
  geom_line(data = tgc, aes(x = cov, y = total_aes, group = Group, colour = Group), linetype = 3, size = 1)+
  geom_point(data = tgc, aes(x = cov, y = total_aes, group = Group, colour = Group), shape = 17, size = 5) +
  geom_errorbar(data = tgc, aes(x = cov, y = total_aes, group = Group, colour = Group, ymin = total_aes-se, ymax = total_aes+se), width = .1)+
  theme_classic() + 
  xlab("covid-19") + 
  ylab("Apathy scores total") +
  scale_x_discrete(labels=c("0" = "before", "1" = "after"))+
  scale_colour_manual(labels=c("1" = "experimental", "2" = "control"), values = c("#CC6666","#9999CC"))+
  scale_fill_manual(labels=c("1" = "experimental", "2" = "control"), values = c("#CC6666","#9999CC"))+ theme(text = element_text(size = 30))+
  ggtitle("")+
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.5)) +theme(text = element_text(size=30))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 20))+
  ggtitle("a") + theme(plot.title = element_text(hjust = -0.15, vjust = 2))
aes_cov
ggsave('plot_total_aes.jpg', width = 10, height = 8)

#compare group differences
compare_means(total_aes ~ cov, data = data.q, group.by = "Group", method = 't.test')
aes <- ggplot(data.q, aes(x=Group, y=total_aes, fill=cov)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 75)) +
  theme_classic() + 
    labs(x="Group", y="Apathy score total", fill = "covid-19")+
  scale_x_discrete(labels=c("1" = "experimental", "2" = "control"))+
  scale_colour_manual(labels=c("0" = "before", "1" = "after"), values = c("#CC6666","#9999CC"))+
  scale_fill_manual(labels=c("0" = "before", "1" = "after"), values = c("#CC6666","#9999CC"))+ theme(text = element_text(size = 30))+
  ggtitle("")+
  stat_compare_means(aes(label = after_stat(p.signif)),
                     method = "t.test", label.y = c(68,68), size = 10)
aes

ggsave('aes_group_difference.jpg', width = 10, height = 8)


#sdq
#effect of intervention
formula <- total_sdq ~ cov*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data, REML=TRUE)
eta_squared(model, partial = TRUE)
print(emmeans(model, pairwise~cov*Group, adjust = "bonferroni"))
data.q <- subset(data.q, total_sdq != "NA" )
bf = anovaBF(total_sdq ~ cov*Group, data=data.q, whichRandom = "id")
bf[4] / bf[3]

#plot
my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data.cov, measurevar="total_sdq", groupvars=c("cov","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)
sdq_cov <- ggplot(data.cov, aes(x = cov, y = total_sdq, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = cov, y =total_sdq, colour = Group, ), show.legend = TRUE,
             position = position_jitter(width = .01), size = .5, shape = 10)+ scale_y_continuous(limits = c(-10, 40)) +
  geom_line(data = tgc, aes(x = cov, y = total_sdq, group = Group, colour = Group), linetype = 3, size = 1)+
  geom_point(data = tgc, aes(x = cov, y = total_sdq, group = Group, colour = Group), shape = 17, size = 5) +
  geom_errorbar(data = tgc, aes(x = cov, y = total_sdq, group = Group, colour = Group, ymin = total_sdq-se, ymax = total_sdq+se), width = .1)+
  theme_classic() + 
  xlab("covid-19") + 
  ylab("Strength & Difficulty scores total") +
  scale_x_discrete(labels=c("0" = "before", "1" = "after"))+
  scale_colour_manual(labels=c("1" = "experimental", "2" = "control"), values = c("#CC6666","#9999CC"))+
  scale_fill_manual(labels=c("1" = "experimental", "2" = "control"), values = c("#CC6666","#9999CC"))+ theme(text = element_text(size = 30))+
  ggtitle("")+
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.5)) +theme(text = element_text(size=30))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 20))+
  ggtitle("b") + theme(plot.title = element_text(hjust = -0.15, vjust = 2))
ggsave('plot_total_sdq.jpg', width = 10, height = 8)

#compare group differences
compare_means(total_sdq ~ cov, data = data.q, group.by = "Group")
sdq <- ggplot(data.q, aes(x=Group, y=total_sdq, fill=cov)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(0, 40)) +
  theme_classic() + 
  labs(x="Group", y="Strength & Difficulty scores total", fill = "covid-19")+
  scale_x_discrete(labels=c("1" = "experimental", "2" = "control"))+
  scale_colour_manual(labels=c("0" = "before", "1" = "after"), values = c("#CC6666","#9999CC"))+
  scale_fill_manual(labels=c("0" = "before", "1" = "after"), values = c("#CC6666","#9999CC"))+ theme(text = element_text(size = 30))+
  ggtitle("")+
  stat_compare_means(aes(label = after_stat(p.signif)),
                     method = "t.test", label.y = c(35,35), size = 10)
sdq

ggsave('sdq_group_difference.jpg', width = 10, height = 8)


mainplot_behaviour <- ggarrange(aes_cov, sdq_cov,
                                ncol=2, nrow=1, common.legend = TRUE, legend="right")
ggsave("cov.png",
       width = 5000, height = 2500,units = c("px"))



#CASI
#effect of intervention
formula <- casi_total ~ cov*Group + Age + Gender + (1|id)
anova(lmer( formula, data=data, REML=TRUE))  #check if is significant
model<-lmer( formula, data=data, REML=TRUE)
print(emmeans(model, pairwise~cov*Group, adjust = "bonferroni"))
data.q <- subset(data.q, casi_total != "NA" )
bf = anovaBF(casi_total ~ cov*Group, data=data, whichRandom = "id")
bf[4] / bf[3]

#plot
tgc <- summarySE(data, measurevar="casi_total", groupvars=c("cov","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)
casi_total <- ggplot(data, aes(x = cov, y = casi_total, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = cov, y =casi_total, colour = Group, ), show.legend = TRUE,
             position = position_jitter(width = .01), size = .5, shape = 10)+ scale_y_continuous(limits = c(-30, 175)) +
  geom_line(data = tgc, aes(x = cov, y = casi_total, group = Group, colour = Group), linetype = 3, size = 1)+
  geom_point(data = tgc, aes(x = cov, y = casi_total, group = Group, colour = Group), shape = 17, size = 5) +
  geom_errorbar(data = tgc, aes(x = cov, y = casi_total, group = Group, colour = Group, ymin = casi_total-se, ymax = casi_total+se), width = .1)+
  theme_classic() + 
  xlab("covid-19") + 
  ylab("CASI total") +
  scale_x_discrete(labels=c("0" = "before", "1" = "after"))+
  scale_colour_manual(labels=c("1" = "experimental", "2" = "control"), values = c("#CC6666","#9999CC"))+
  scale_fill_manual(labels=c("1" = "experimental", "2" = "control"), values = c("#CC6666","#9999CC"))+ theme(text = element_text(size = 30))+
  ggtitle("")+
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.5)) +theme(text = element_text(size=30))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 20))+
ggsave('plot_casi_total.jpg', width = 10, height = 8)

