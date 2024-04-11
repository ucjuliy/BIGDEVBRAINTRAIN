#plots training effect T0T1
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
library(raincloudplots)


#load file
data.imp <- read.csv("training_data.csv")
#as factor
data.imp$id <- factor(data.imp$id)
data.imp$timepoint <- factor(data.imp$timepoint)
data.imp$Group <- factor(data.imp$Group,levels = c('1', '2'), 
                         labels = c('1', '2'))
data.imp$gender <- factor(data.imp$Gender)
data_T0T1 <- data.imp[data.imp$timepoint %in% c("0", "1"), ]



#raincloud plot

#EF short transfer
my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T1, measurevar="prob_stopsig", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

prob_stopsig <- ggplot(data_T0T1, aes(x = timepoint, y = prob_stopsig, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(timepoint)-.15, y =prob_stopsig, colour = Group), show.legend = TRUE,
             position = position_jitter(width = .05), size = 3, shape = 20)+ scale_y_continuous(limits = c(0.2,1)) +
  geom_line(data = tgc, aes(x = as.numeric(timepoint)+.1, y = prob_stopsig, group = Group, colour = Group), linetype = 1, size = 1)+
  geom_point(data = tgc, aes(x = as.numeric(timepoint)+.1, y = prob_stopsig, group = Group, colour = Group), shape = 18, size = 4) +
  geom_errorbar(data = tgc, aes(x = as.numeric(timepoint)+.1, y = prob_stopsig, group = Group, colour = Group, ymin = prob_stopsig-se, ymax = prob_stopsig+se), width = .1)+
  theme_classic() + 
  xlab("session") + 
  ylab("% correct stop") +
  scale_x_discrete(labels=c("0" = "pre", "1" = "post"))+
  scale_colour_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+
  scale_fill_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+ theme(text = element_text(size = 30))+
  ggtitle("")+
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.5)) +theme(text = element_text(size=30))+
  theme(legend.title = element_text(size = 30), legend.text = element_text(size = 25))+
  ggtitle("a") + theme(plot.title = element_text(hjust = -0.15, vjust = 2))
#ggsave('Raincloudplot.png', width = w, height = h)
#coord_flip()+
prob_stopsig
ggsave('Raincloudplot_prob_stopsig.jpg', width = 16, height = 10)

my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T1, measurevar="meango_corrrtnr_t", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

meango_corrrtnr_t <- ggplot(data_T0T1, aes(x = timepoint, y = meango_corrrtnr_t, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(timepoint)-.15, y =meango_corrrtnr_t, colour = Group), show.legend = TRUE,
             position = position_jitter(width = .05), size = 3, shape = 20)+ scale_y_continuous(limits = c(200,1000)) +
  geom_line(data = tgc, aes(x = as.numeric(timepoint)+.1, y = meango_corrrtnr_t, group = Group, colour = Group), linetype = 1, size = 1)+
  geom_point(data = tgc, aes(x = as.numeric(timepoint)+.1, y = meango_corrrtnr_t, group = Group, colour = Group), shape = 18, size = 4) +
  geom_errorbar(data = tgc, aes(x = as.numeric(timepoint)+.1, y = meango_corrrtnr_t, group = Group, colour = Group, ymin = meango_corrrtnr_t-se, ymax = meango_corrrtnr_t+se), width = .1)+
  theme_classic() + 
  xlab("session") + 
  ylab("Go RT (ms)") +
  scale_x_discrete(labels=c("0" = "pre", "1" = "post"))+
  scale_colour_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+
  scale_fill_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+ theme(text = element_text(size = 30))+
  ggtitle("")+
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.5)) +theme(text = element_text(size=30))+
  theme(legend.title = element_text(size = 30), legend.text = element_text(size = 25))+
  ggtitle("b") + theme(plot.title = element_text(hjust = -0.15, vjust = 2))
#ggsave('Raincloudplot.png', width = w, height = h)
#coord_flip()+
meango_corrrtnr_t
ggsave('Raincloudplot_meango_corrrtnr_t.jpg', width = 16, height = 10)


mainplot<-ggarrange(prob_stopsig, meango_corrrtnr_t, 
                    ncol=2, nrow=1, common.legend = TRUE, legend="right")
ggsave("raincloud_shortterm_EF_shorttransfer.png",
       width = 5000, height = 2500,units = c("px"))


#EF 3 factors
my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T1, measurevar="EF_I_error", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

EF_I_error <- ggplot(data_T0T1, aes(x = timepoint, y = EF_I_error, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(timepoint)-.15, y =EF_I_error, colour = Group), show.legend = TRUE,
             position = position_jitter(width = .05), size = 3, shape = 20)+ scale_y_continuous(limits = c(-0.6,0.6)) +
  geom_line(data = tgc, aes(x = as.numeric(timepoint)+.1, y = EF_I_error, group = Group, colour = Group), linetype = 1, size = 1)+
  geom_point(data = tgc, aes(x = as.numeric(timepoint)+.1, y = EF_I_error, group = Group, colour = Group), shape = 18, size = 4) +
  geom_errorbar(data = tgc, aes(x = as.numeric(timepoint)+.1, y = EF_I_error, group = Group, colour = Group, ymin = EF_I_error-se, ymax = EF_I_error+se), width = .1)+
  theme_classic() + 
  xlab("session") + 
  ylab("Error rate inhibition / shifting") +
  scale_x_discrete(labels=c("0" = "pre", "1" = "post"))+
  scale_colour_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+
  scale_fill_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+ theme(text = element_text(size = 30))+
  ggtitle("")+
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.5)) +theme(text = element_text(size=30))+
  theme(legend.title = element_text(size = 30), legend.text = element_text(size = 25))+
  ggtitle("a") + theme(plot.title = element_text(hjust = -0.15, vjust = 2))
#ggsave('Raincloudplot.png', width = w, height = h)
#coord_flip()+
EF_I_error
ggsave('Raincloudplot_EF_I_error.jpg', width = 16, height = 10)


my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T1, measurevar="EF_M_error", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

EF_M_error <- ggplot(data_T0T1, aes(x = timepoint, y = EF_M_error, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(timepoint)-.15, y =EF_M_error, colour = Group), show.legend = TRUE,
             position = position_jitter(width = .05), size = 3, shape = 20)+ scale_y_continuous(limits = c(-0.2, 0.2)) +
  geom_line(data = tgc, aes(x = as.numeric(timepoint)+.1, y = EF_M_error, group = Group, colour = Group), linetype = 1, size = 1)+
  geom_point(data = tgc, aes(x = as.numeric(timepoint)+.1, y = EF_M_error, group = Group, colour = Group), shape = 18, size = 4) +
  geom_errorbar(data = tgc, aes(x = as.numeric(timepoint)+.1, y = EF_M_error, group = Group, colour = Group, ymin = EF_M_error-se, ymax = EF_M_error+se), width = .1)+
  theme_classic() + 
  xlab("session") + 
  ylab("Error rate memory") +
  scale_x_discrete(labels=c("0" = "pre", "1" = "post"))+
  scale_colour_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+
  scale_fill_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+ theme(text = element_text(size = 30))+
  ggtitle("")+
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.5)) +theme(text = element_text(size=30))+
  theme(legend.title = element_text(size = 30), legend.text = element_text(size = 25))+
  ggtitle("b") + theme(plot.title = element_text(hjust = -0.15, vjust = 2))
#ggsave('Raincloudplot.png', width = w, height = h)
#coord_flip()+
EF_M_error
ggsave('Raincloudplot_EF_M_error.jpg', width = 16, height = 10)

my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T1, measurevar="EFRTFACTOR", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

EFRTFACTOR <- ggplot(data_T0T1, aes(x = timepoint, y = EFRTFACTOR, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(timepoint)-.15, y =EFRTFACTOR, colour = Group), show.legend = TRUE,
             position = position_jitter(width = .05), size = 3, shape = 20)+ scale_y_continuous(limits = c(-30,20)) +
  geom_line(data = tgc, aes(x = as.numeric(timepoint)+.1, y = EFRTFACTOR, group = Group, colour = Group), linetype = 1, size = 1)+
  geom_point(data = tgc, aes(x = as.numeric(timepoint)+.1, y = EFRTFACTOR, group = Group, colour = Group), shape = 18, size = 4) +
  geom_errorbar(data = tgc, aes(x = as.numeric(timepoint)+.1, y = EFRTFACTOR, group = Group, colour = Group, ymin = EFRTFACTOR-se, ymax = EFRTFACTOR+se), width = .1)+
  theme_classic() + 
  xlab("session") + 
  ylab("Reaction time Cognitive Control") +
  scale_x_discrete(labels=c("0" = "pre", "1" = "post"))+
  scale_colour_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+
  scale_fill_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+ theme(text = element_text(size = 30))+
  ggtitle("")+
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.5)) +theme(text = element_text(size=30))+
  theme(legend.title = element_text(size = 30), legend.text = element_text(size = 25))+
  ggtitle("c") + theme(plot.title = element_text(hjust = -0.15, vjust = 2))
#ggsave('Raincloudplot.png', width = w, height = h)
#coord_flip()+
EFRTFACTOR
ggsave('Raincloudplot_EFRTFACTOR.jpg', width = 16, height = 10)


mainplot1<-ggarrange(EF_I_error, EF_M_error, EFRTFACTOR,
                    ncol=5, nrow=1, common.legend = TRUE, legend="right")
ggsave("raincloud_shortterm_fartransfer_EF_3factors.png",
       width = 7000, height = 2500,units = c("px"))

#decision making
my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T1, measurevar="DG_Coins_Given", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

DG_Coins_Given <- ggplot(data_T0T1, aes(x = timepoint, y = DG_Coins_Given, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(timepoint)-.15, y =DG_Coins_Given, colour = Group), show.legend = TRUE,
             position = position_jitter(width = .05), size = 3, shape = 20)+ scale_y_continuous(limits = c(-2.5,8.5)) +
  geom_line(data = tgc, aes(x = as.numeric(timepoint)+.1, y = DG_Coins_Given, group = Group, colour = Group), linetype = 1, size = 1)+
  geom_point(data = tgc, aes(x = as.numeric(timepoint)+.1, y = DG_Coins_Given, group = Group, colour = Group), shape = 18, size = 4) +
  geom_errorbar(data = tgc, aes(x = as.numeric(timepoint)+.1, y = DG_Coins_Given, group = Group, colour = Group, ymin = DG_Coins_Given-se, ymax = DG_Coins_Given+se), width = .1)+
  theme_classic() + 
  xlab("session") + 
  ylab("DG Offer") +
  scale_x_discrete(labels=c("0" = "pre", "1" = "post"))+
  scale_colour_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+
  scale_fill_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+ theme(text = element_text(size = 30))+
  ggtitle("")+
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.5)) +theme(text = element_text(size=30))+
  theme(legend.title = element_text(size = 30), legend.text = element_text(size = 25))+
  ggtitle("d") + theme(plot.title = element_text(hjust = -0.1, vjust = 2))

#ggsave('Raincloudplot.png', width = w, height = h)
#coord_flip()+
DG_Coins_Given
ggsave('Raincloudplot_DG_Coins_Given.jpg', width = 16, height = 10)

my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T1, measurevar="UG_Offer_Accept", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)
UG_Offer_Accept <- ggplot(data_T0T1, aes(x = timepoint, y = UG_Offer_Accept, fill = Group)) +
  geom_line(data = tgc, aes(x = timepoint, y = UG_Offer_Accept, group = Group, colour = Group), linetype = 1, size = 1)+
  geom_point(data = tgc, aes(x = timepoint, y = UG_Offer_Accept, group = Group, colour = Group), shape = 18, size = 6) +
  geom_errorbar(data = tgc, aes(x = timepoint, y = UG_Offer_Accept, group = Group, colour = Group, ymin = UG_Offer_Accept-se, ymax = UG_Offer_Accept+se), width = .1)+
  theme_classic() + 
  xlab("session") + 
  ylab("Unfair offer acceptance") +
  scale_x_discrete(labels=c("0" = "pre", "1" = "post"))+
  scale_colour_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+
  scale_fill_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+ theme(text = element_text(size = 30))+
  ggtitle("")+
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.5)) +theme(text = element_text(size=30))+
  theme(legend.title = element_text(size = 30), legend.text = element_text(size = 25))+
  ggtitle("e") + theme(plot.title = element_text(hjust = -0.15, vjust = 2))
#ggsave('Raincloudplot.png', width = w, height = h)
#coord_flip()+
UG_Offer_Accept
ggsave('Raincloudplot_UG_Offer_Accept.jpg', width = 16, height = 10)

my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T1, measurevar="tot_delay_perc", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

tot_delay_perc <- ggplot(data_T0T1, aes(x = timepoint, y = tot_delay_perc, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(timepoint)-.15, y =tot_delay_perc, colour = Group), show.legend = TRUE,
             position = position_jitter(width = .05), size = 3, shape = 20)+ scale_y_continuous(limits = c(-50,150)) +
  geom_line(data = tgc, aes(x = as.numeric(timepoint)+.1, y = tot_delay_perc, group = Group, colour = Group), linetype = 1, size = 1)+
  geom_point(data = tgc, aes(x = as.numeric(timepoint)+.1, y = tot_delay_perc, group = Group, colour = Group), shape = 18, size = 4) +
  geom_errorbar(data = tgc, aes(x = as.numeric(timepoint)+.1, y = tot_delay_perc, group = Group, colour = Group, ymin = tot_delay_perc-se, ymax = tot_delay_perc+se), width = .1)+
  theme_classic() + 
  xlab("session") + 
  ylab("percentage of delayed choice (%)") +
  scale_x_discrete(labels=c("0" = "pre", "1" = "post"))+
  scale_colour_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+
  scale_fill_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+ theme(text = element_text(size = 30))+
  ggtitle("")+
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.5)) +theme(text = element_text(size=30))+
  theme(legend.title = element_text(size = 30), legend.text = element_text(size = 25))+
  ggtitle("f") + theme(plot.title = element_text(hjust = -0.15, vjust = 2))
#ggsave('Raincloudplot.png', width = w, height = h)
#coord_flip()+
tot_delay_perc
ggsave('Raincloudplot_tot_delay_perc.jpg', width = 16, height = 10)

mainplot1<-ggarrange(EF_I_error, EF_M_error, EFRTFACTOR,
                     DG_Coins_Given, UG_Offer_Accept, tot_delay_perc,
                    ncol=6, nrow=1, common.legend = TRUE, legend="right")
ggsave("raincloud_shortterm_fartransfer_decisionmaking.png",
       width = 7000, height = 2500,units = c("px"))


#academic score
my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T1, measurevar="academic_score", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

academic_score <- ggplot(data_T0T1, aes(x = timepoint, y = academic_score, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(timepoint)-.15, y =academic_score, colour = Group), show.legend = TRUE,
             position = position_jitter(width = .05), size = 3, shape = 20)+ scale_y_continuous(limits = c(60,160)) +
  geom_line(data = tgc, aes(x = as.numeric(timepoint)+.1, y = academic_score, group = Group, colour = Group), linetype = 1, size = 1)+
  geom_point(data = tgc, aes(x = as.numeric(timepoint)+.1, y = academic_score, group = Group, colour = Group), shape = 18, size = 4) +
  geom_errorbar(data = tgc, aes(x = as.numeric(timepoint)+.1, y = academic_score, group = Group, colour = Group, ymin = academic_score-se, ymax = academic_score+se), width = .1)+
  theme_classic() + 
  xlab("session") + 
  ylab("Reading / Maths") +
  scale_x_discrete(labels=c("0" = "pre", "1" = "post"))+
  scale_colour_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+
  scale_fill_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+ theme(text = element_text(size = 30))+
  ggtitle("")+
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.5)) +theme(text = element_text(size=30))+
  theme(legend.title = element_text(size = 30), legend.text = element_text(size = 25))+
  ggtitle("g") + theme(plot.title = element_text(hjust = -0.15, vjust = 2))
#ggsave('Raincloudplot.png', width = w, height = h)
#coord_flip()+
academic_score
ggsave('Raincloudplot_AcademicScore.jpg', width = 16, height = 10)

#creativity
my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T1, measurevar="creativity", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

creativity <- ggplot(data_T0T1, aes(x = timepoint, y = creativity, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(timepoint)-.15, y =creativity, colour = Group), show.legend = TRUE,
             position = position_jitter(width = .05), size = 3, shape = 20)+ scale_y_continuous(limits = c(-20,60)) +
  geom_line(data = tgc, aes(x = as.numeric(timepoint)+.1, y = creativity, group = Group, colour = Group), linetype = 1, size = 1)+
  geom_point(data = tgc, aes(x = as.numeric(timepoint)+.1, y = creativity, group = Group, colour = Group), shape = 18, size = 4) +
  geom_errorbar(data = tgc, aes(x = as.numeric(timepoint)+.1, y = creativity, group = Group, colour = Group, ymin = creativity-se, ymax = creativity+se), width = .1)+
  theme_classic() + 
  xlab("session") + 
  ylab("Creativity Scores") +
  scale_x_discrete(labels=c("0" = "pre", "1" = "post"))+
  scale_colour_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+
  scale_fill_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+ theme(text = element_text(size = 30))+
  ggtitle("")+
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.5)) +theme(text = element_text(size=30))+
  theme(legend.title = element_text(size = 30), legend.text = element_text(size = 25))+
  ggtitle("k") + theme(plot.title = element_text(hjust = -0.15, vjust = 2))
#ggsave('Raincloudplot.png', width = w, height = h)
#coord_flip()+
creativity
ggsave('Raincloudplot_creativity.jpg', width = 16, height = 10)

#WASI
my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T1, measurevar="wasi", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

wasi <- ggplot(data_T0T1, aes(x = timepoint, y = wasi, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(timepoint)-.15, y =wasi, colour = Group), show.legend = TRUE,
             position = position_jitter(width = .05), size = 3, shape = 20)+ scale_y_continuous(limits = c(40,200)) +
  geom_line(data = tgc, aes(x = as.numeric(timepoint)+.1, y = wasi, group = Group, colour = Group), linetype = 1, size = 1)+
  geom_point(data = tgc, aes(x = as.numeric(timepoint)+.1, y = wasi, group = Group, colour = Group), shape = 18, size = 4) +
  geom_errorbar(data = tgc, aes(x = as.numeric(timepoint)+.1, y = wasi, group = Group, colour = Group, ymin = wasi-se, ymax = wasi+se), width = .1)+
  theme_classic() + 
  xlab("session") + 
  ylab("WASI Intelligence Scores") +
  scale_x_discrete(labels=c("0" = "pre", "1" = "post"))+
  scale_colour_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+
  scale_fill_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+ theme(text = element_text(size = 30))+
  ggtitle("")+
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.5)) +theme(text = element_text(size=30))+
  theme(legend.title = element_text(size = 30), legend.text = element_text(size = 25))+
  ggtitle("h") + theme(plot.title = element_text(hjust = -0.15, vjust = 2))
#ggsave('Raincloudplot.png', width = w, height = h)
#coord_flip()+
wasi
ggsave('Raincloudplot_WASI.jpg', width = 16, height = 10)

#mental health
my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T1, measurevar="internalising_factor", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

internalising_factor <- ggplot(data_T0T1, aes(x = timepoint, y = internalising_factor, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(timepoint)-.15, y =internalising_factor, colour = Group), show.legend = TRUE,
             position = position_jitter(width = .05), size = 3, shape = 20)+ scale_y_continuous(limits = c(-4,6)) +
  geom_line(data = tgc, aes(x = as.numeric(timepoint)+.1, y = internalising_factor, group = Group, colour = Group), linetype = 1, size = 1)+
  geom_point(data = tgc, aes(x = as.numeric(timepoint)+.1, y = internalising_factor, group = Group, colour = Group), shape = 18, size = 4) +
  geom_errorbar(data = tgc, aes(x = as.numeric(timepoint)+.1, y = internalising_factor, group = Group, colour = Group, ymin = internalising_factor-se, ymax = internalising_factor+se), width = .1)+
  theme_classic() + 
  xlab("session") + 
  ylab("Internalising problems") +
  scale_x_discrete(labels=c("0" = "pre", "1" = "post"))+
  scale_colour_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+
  scale_fill_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+ theme(text = element_text(size = 30))+
  ggtitle("")+
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.5)) +theme(text = element_text(size=30))+
  theme(legend.title = element_text(size = 30), legend.text = element_text(size = 25))+
  ggtitle("i") + theme(plot.title = element_text(hjust = -0.08, vjust = 2))
#ggsave('Raincloudplot.png', width = w, height = h)
#coord_flip()+
internalising_factor
ggsave('Raincloudplot_internalising_factor.jpg', width = 16, height = 10)

my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T1, measurevar="externalising_factor", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

externalising_factor <- ggplot(data_T0T1, aes(x = timepoint, y = externalising_factor, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(timepoint)-.15, y =externalising_factor, colour = Group), show.legend = TRUE,
             position = position_jitter(width = .05), size = 3, shape = 20)+ scale_y_continuous(limits = c(-4,6)) +
  geom_line(data = tgc, aes(x = as.numeric(timepoint)+.1, y = externalising_factor, group = Group, colour = Group), linetype = 1, size = 1)+
  geom_point(data = tgc, aes(x = as.numeric(timepoint)+.1, y = externalising_factor, group = Group, colour = Group), shape = 18, size = 4) +
  geom_errorbar(data = tgc, aes(x = as.numeric(timepoint)+.1, y = externalising_factor, group = Group, colour = Group, ymin = externalising_factor-se, ymax = externalising_factor+se), width = .1)+
  theme_classic() + 
  xlab("session") + 
  ylab("Externalising problems") +
  scale_x_discrete(labels=c("0" = "pre", "1" = "post"))+
  scale_colour_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+
  scale_fill_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+ theme(text = element_text(size = 30))+
  ggtitle("")+
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.5)) +theme(text = element_text(size=30))+
  theme(legend.title = element_text(size = 30), legend.text = element_text(size = 25))+
  ggtitle("j") + theme(plot.title = element_text(hjust = -0.1, vjust = 2))
#ggsave('Raincloudplot.png', width = w, height = h)
#coord_flip()+
externalising_factor
ggsave('Raincloudplot_externalising_factor.jpg', width = 16, height = 10)


mainplot_behaviour<-ggarrange(EF_I_error, EF_M_error, EFRTFACTOR,
                              DG_Coins_Given, UG_Offer_Accept, tot_delay_perc,
                              academic_score, wasi, 
                              internalising_factor, externalising_factor, creativity,
                              ncol=3, nrow=4, common.legend = TRUE, legend="right")
ggsave("raincloud_shortterm_fartransfer_behaviour.png",
       width = 7500, height = 9500,units = c("px"))



#brain rIFG
my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T1, measurevar="HavardOxford_Cortl", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

HavardOxford_Cortl <- ggplot(data_T0T1, aes(x = timepoint, y = HavardOxford_Cortl, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(timepoint)-.15, y =HavardOxford_Cortl, colour = Group), show.legend = TRUE,
             position = position_jitter(width = .05), size = 3, shape = 20)+ scale_y_continuous(limits = c(-10,10)) +
  geom_line(data = tgc, aes(x = as.numeric(timepoint)+.1, y = HavardOxford_Cortl, group = Group, colour = Group), linetype = 1, size = 1)+
  geom_point(data = tgc, aes(x = as.numeric(timepoint)+.1, y = HavardOxford_Cortl, group = Group, colour = Group), shape = 18, size = 4) +
  geom_errorbar(data = tgc, aes(x = as.numeric(timepoint)+.1, y = HavardOxford_Cortl, group = Group, colour = Group, ymin = HavardOxford_Cortl-se, ymax = HavardOxford_Cortl+se), width = .1)+
  theme_classic() + 
  xlab("session") + 
  ylab("Activation rIFG") +
  scale_x_discrete(labels=c("0" = "pre", "1" = "post"))+
  scale_colour_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+
  scale_fill_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+ theme(text = element_text(size = 30))+
  ggtitle("")+
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.5)) +theme(text = element_text(size=25))+
  theme(legend.title = element_text(size = 30), legend.text = element_text(size = 25))+
  ggtitle("a") + theme(plot.title = element_text(hjust = -0.15, vjust = 2))
#ggsave('Raincloudplot.png', width = w, height = h)
#coord_flip()+
HavardOxford_Cortl
ggsave('Raincloudplot_HavardOxford_Cortl.jpg', width = 16, height = 10)

my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T1, measurevar="CorticalThickness", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

CorticalThickness <- ggplot(data_T0T1, aes(x = timepoint, y = CorticalThickness, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(timepoint)-.15, y =CorticalThickness, colour = Group), show.legend = TRUE,
             position = position_jitter(width = .05), size = 3, shape = 20)+ scale_y_continuous(limits = c(2,3.5)) +
  geom_line(data = tgc, aes(x = as.numeric(timepoint)+.1, y = CorticalThickness, group = Group, colour = Group), linetype = 1, size = 1)+
  geom_point(data = tgc, aes(x = as.numeric(timepoint)+.1, y = CorticalThickness, group = Group, colour = Group), shape = 18, size = 4) +
  geom_errorbar(data = tgc, aes(x = as.numeric(timepoint)+.1, y = CorticalThickness, group = Group, colour = Group, ymin = CorticalThickness-se, ymax = CorticalThickness+se), width = .1)+
  theme_classic() + 
  xlab("session") + 
  ylab("rIFG Cortical thickness  (mm)") +
  scale_x_discrete(labels=c("0" = "pre", "1" = "post"))+
  scale_colour_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+
  scale_fill_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+ theme(text = element_text(size = 30))+
  ggtitle("")+
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.5)) +theme(text = element_text(size=25))+
  theme(legend.title = element_text(size = 30), legend.text = element_text(size = 25))+
  ggtitle("b") + theme(plot.title = element_text(hjust = -0.15, vjust = 2))
#ggsave('Raincloudplot.png', width = w, height = h)
#coord_flip()+
CorticalThickness
ggsave('Raincloudplot_CorticalThickness.jpg', width = 16, height = 10)


#brain network
my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T1, measurevar="CON", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

CON <- ggplot(data_T0T1, aes(x = timepoint, y = CON, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(timepoint)-.15, y =CON, colour = Group), show.legend = TRUE,
             position = position_jitter(width = .05), size = 3, shape = 20)+ scale_y_continuous(limits = c(0,0.8)) +
  geom_line(data = tgc, aes(x = as.numeric(timepoint)+.1, y = CON, group = Group, colour = Group), linetype = 1, size = 1)+
  geom_point(data = tgc, aes(x = as.numeric(timepoint)+.1, y = CON, group = Group, colour = Group), shape = 18, size = 4) +
  geom_errorbar(data = tgc, aes(x = as.numeric(timepoint)+.1, y = CON, group = Group, colour = Group, ymin = CON-se, ymax = CON+se), width = .1)+
  theme_classic() + 
  xlab("session") + 
  ylab("Connectivity from Cingulo-opercular Network") +
  scale_x_discrete(labels=c("0" = "pre", "1" = "post"))+
  scale_colour_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+
  scale_fill_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+ theme(text = element_text(size = 30))+
  ggtitle("")+
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.5)) +theme(text = element_text(size=25))+
  theme(legend.title = element_text(size = 30), legend.text = element_text(size = 25))+
  ggtitle("c") + theme(plot.title = element_text(hjust = -0.15, vjust = 2.5))
#ggsave('Raincloudplot.png', width = w, height = h)
#coord_flip()+
CON
ggsave('Raincloudplot_CON.jpg', width = 16, height = 10)


my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T1, measurevar="FPN", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

FPN <- ggplot(data_T0T1, aes(x = timepoint, y = FPN, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(timepoint)-.15, y =FPN, colour = Group), show.legend = TRUE,
             position = position_jitter(width = .05), size = 3, shape = 20)+ scale_y_continuous(limits = c(0, 0.8)) +
  geom_line(data = tgc, aes(x = as.numeric(timepoint)+.1, y = FPN, group = Group, colour = Group), linetype = 1, size = 1)+
  geom_point(data = tgc, aes(x = as.numeric(timepoint)+.1, y = FPN, group = Group, colour = Group), shape = 18, size = 4) +
  geom_errorbar(data = tgc, aes(x = as.numeric(timepoint)+.1, y = FPN, group = Group, colour = Group, ymin = FPN-se, ymax = FPN+se), width = .1)+
  theme_classic() + 
  xlab("session") + 
  ylab("Connectivity from Frontoparetial Network") +
  scale_x_discrete(labels=c("0" = "pre", "1" = "post"))+
  scale_colour_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+
  scale_fill_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+ theme(text = element_text(size = 30))+
  ggtitle("")+
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.5)) +theme(text = element_text(size=25))+
  theme(legend.title = element_text(size = 30), legend.text = element_text(size = 25))+
  ggtitle("d") + theme(plot.title = element_text(hjust = -0.15, vjust = 2.5))
#ggsave('Raincloudplot.png', width = w, height = h)
#coord_flip()+
FPN
ggsave('Raincloudplot_FPN.jpg', width = 16, height = 10)


#DTI
my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T1, measurevar="Rputa.frontal_FA", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

Rputa.frontal_FA <- ggplot(data_T0T1, aes(x = timepoint, y = Rputa.frontal_FA, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(timepoint)-.15, y =Rputa.frontal_FA, colour = Group), show.legend = TRUE,
             position = position_jitter(width = .05), size = 3, shape = 20)+ scale_y_continuous(limits = c(0.3,0.6)) +
  geom_line(data = tgc, aes(x = as.numeric(timepoint)+.1, y = Rputa.frontal_FA, group = Group, colour = Group), linetype = 1, size = 1)+
  geom_point(data = tgc, aes(x = as.numeric(timepoint)+.1, y = Rputa.frontal_FA, group = Group, colour = Group), shape = 18, size = 4) +
  geom_errorbar(data = tgc, aes(x = as.numeric(timepoint)+.1, y = Rputa.frontal_FA, group = Group, colour = Group, ymin = Rputa.frontal_FA-se, ymax = Rputa.frontal_FA+se), width = .1)+
  theme_classic() + 
  xlab("session") + 
  ylab("Right fronto-striatal tract putamen (FA)") +
  scale_x_discrete(labels=c("0" = "pre", "1" = "post"))+
  scale_colour_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+
  scale_fill_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+ theme(text = element_text(size = 30))+
  ggtitle("")+
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.5)) +theme(text = element_text(size=25))+
  theme(legend.title = element_text(size = 30), legend.text = element_text(size = 25))+
  ggtitle("e") + theme(plot.title = element_text(hjust = -0.15, vjust = 2))
#ggsave('Raincloudplot.png', width = w, height = h)
#coord_flip()+
Rputa.frontal_FA
ggsave('Raincloudplot_Rputa.frontal_FA.jpg', width = 16, height = 10)


my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T1, measurevar="Rputa.frontal_MD", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

Rputa.frontal_MD <- ggplot(data_T0T1, aes(x = timepoint, y = Rputa.frontal_MD, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(timepoint)-.15, y =Rputa.frontal_MD, colour = Group), show.legend = TRUE,
             position = position_jitter(width = .05), size = 3, shape = 20)+ scale_y_continuous(limits = c(0.00075, 0.001)) +
  geom_line(data = tgc, aes(x = as.numeric(timepoint)+.1, y = Rputa.frontal_MD, group = Group, colour = Group), linetype = 1, size = 1)+
  geom_point(data = tgc, aes(x = as.numeric(timepoint)+.1, y = Rputa.frontal_MD, group = Group, colour = Group), shape = 18, size = 4) +
  geom_errorbar(data = tgc, aes(x = as.numeric(timepoint)+.1, y = Rputa.frontal_MD, group = Group, colour = Group, ymin = Rputa.frontal_MD-se, ymax = Rputa.frontal_MD+se), width = .1)+
  theme_classic() + 
  xlab("session") + 
  ylab("Right fronto-striatal tract putamen (MD)") +
  scale_x_discrete(labels=c("0" = "pre", "1" = "post"))+
  scale_colour_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+
  scale_fill_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+ theme(text = element_text(size = 30))+
  ggtitle("")+
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.5)) +theme(text = element_text(size=25))+
  theme(legend.title = element_text(size = 30), legend.text = element_text(size = 25))+
  ggtitle("f") + theme(plot.title = element_text(hjust = -0.25, vjust = 2))
#ggsave('Raincloudplot.png', width = w, height = h)
#coord_flip()+
Rputa.frontal_MD
ggsave('Raincloudplot_Rputa.frontal_MD.jpg', width = 16, height = 10)


mainplot<-ggarrange(HavardOxford_Cortl, CorticalThickness,
                    CON,FPN,
                    Rputa.frontal_FA, Rputa.frontal_MD,
                    ncol=2, nrow=3, common.legend = TRUE, legend="right")
ggsave("raincloud_shortterm_fartransfer_brain.png",
       width = 5000, height = 7000,units = c("px"))

