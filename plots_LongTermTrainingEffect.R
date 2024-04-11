#plots training effect T0T2
#long term near/far transfer 
#training effect t0 vs. t2
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
data_T0T2["timepoint"][data_T0T2["timepoint"] == "2"] <- "1"


#EF short transfer
my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T2, measurevar="prob_stopsig", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

prob_stopsig <- ggplot(data_T0T2, aes(x = timepoint, y = prob_stopsig, fill = Group)) +
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
  scale_x_discrete(labels=c("0" = "pre", "1" = "one-year-post"))+
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
tgc <- summarySE(data_T0T2, measurevar="meango_corrrtnr_t", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

meango_corrrtnr_t <- ggplot(data_T0T2, aes(x = timepoint, y = meango_corrrtnr_t, fill = Group)) +
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
  scale_x_discrete(labels=c("0" = "pre", "1" = "one-year-post"))+
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
ggsave("raincloud_longterm_fartransfer_EF_shorttransfer.png",
       width = 5000, height = 2500,units = c("px"))



#EF task measures
my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T2, measurevar="corsi_max_wm_t", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

corsi_max_wm_t <- ggplot(data_T0T2, aes(x = timepoint, y = corsi_max_wm_t, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(timepoint)-.15, y =corsi_max_wm_t, colour = Group), show.legend = TRUE,
             position = position_jitter(width = .05), size = 3, shape = 20)+ scale_y_continuous(limits = c(2,10)) +
  geom_line(data = tgc, aes(x = as.numeric(timepoint)+.1, y = corsi_max_wm_t, group = Group, colour = Group), linetype = 1, size = 1)+
  geom_point(data = tgc, aes(x = as.numeric(timepoint)+.1, y = corsi_max_wm_t, group = Group, colour = Group), shape = 18, size = 4) +
  geom_errorbar(data = tgc, aes(x = as.numeric(timepoint)+.1, y = corsi_max_wm_t, group = Group, colour = Group, ymin = corsi_max_wm_t-se, ymax = corsi_max_wm_t+se), width = .1)+
  theme_classic() + 
  xlab("session") + 
  ylab("Memory span") +
  scale_x_discrete(labels=c("0" = "pre", "1" = "one-year-post"))+
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
corsi_max_wm_t
ggsave('Raincloudplot_corsi_max_wm_t.jpg', width = 16, height = 10)


my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T2, measurevar="PBI_t", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

PBI_t <- ggplot(data_T0T2, aes(x = timepoint, y = PBI_t, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(timepoint)-.15, y =PBI_t, colour = Group), show.legend = TRUE,
             position = position_jitter(width = .05), size = 3, shape = 20)+ scale_y_continuous(limits = c(-2.5, 2.5)) +
  geom_line(data = tgc, aes(x = as.numeric(timepoint)+.1, y = PBI_t, group = Group, colour = Group), linetype = 1, size = 1)+
  geom_point(data = tgc, aes(x = as.numeric(timepoint)+.1, y = PBI_t, group = Group, colour = Group), shape = 18, size = 4) +
  geom_errorbar(data = tgc, aes(x = as.numeric(timepoint)+.1, y = PBI_t, group = Group, colour = Group, ymin = PBI_t-se, ymax = PBI_t+se), width = .1)+
  theme_classic() + 
  xlab("session") + 
  ylab("PBI scores") +
  scale_x_discrete(labels=c("0" = "pre", "1" = "one-year-post"))+
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
PBI_t
ggsave('Raincloudplot_PBI_t.jpg', width = 16, height = 10)

my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T2, measurevar="cogflex_t", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

cogflex_t <- ggplot(data_T0T2, aes(x = timepoint, y = cogflex_t, fill = Group)) +
  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
  geom_point(aes(x = as.numeric(timepoint)-.15, y =cogflex_t, colour = Group), show.legend = TRUE,
             position = position_jitter(width = .05), size = 3, shape = 20)+ scale_y_continuous(limits = c(-2.5,2.5)) +
  geom_line(data = tgc, aes(x = as.numeric(timepoint)+.1, y = cogflex_t, group = Group, colour = Group), linetype = 1, size = 1)+
  geom_point(data = tgc, aes(x = as.numeric(timepoint)+.1, y = cogflex_t, group = Group, colour = Group), shape = 18, size = 4) +
  geom_errorbar(data = tgc, aes(x = as.numeric(timepoint)+.1, y = cogflex_t, group = Group, colour = Group, ymin = cogflex_t-se, ymax = cogflex_t+se), width = .1)+
  theme_classic() + 
  xlab("session") + 
  ylab("Cognitive Flexibility scores") +
  scale_x_discrete(labels=c("0" = "pre", "1" = "one-year-post"))+
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
cogflex_t
ggsave('Raincloudplot_cogflex_t.jpg', width = 16, height = 10)



#decision making
my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T2, measurevar="DG_Coins_Given", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

DG_Coins_Given <- ggplot(data_T0T2, aes(x = timepoint, y = DG_Coins_Given, fill = Group)) +
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
  scale_x_discrete(labels=c("0" = "pre", "1" = "one-year-post"))+
  scale_colour_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+
  scale_fill_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+ theme(text = element_text(size = 30))+
  ggtitle("")+
  theme(
    axis.title.y = element_text(vjust = +1),
    axis.title.x = element_text(vjust = -0.5)) +theme(text = element_text(size=30))+
  theme(legend.title = element_text(size = 30), legend.text = element_text(size = 25))+
  ggtitle("d") + theme(plot.title = element_text(hjust = -0.15, vjust = 2))
#ggsave('Raincloudplot.png', width = w, height = h)
#coord_flip()+
DG_Coins_Given
ggsave('Raincloudplot_DG_Coins_Given.jpg', width = 16, height = 10)


my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T2, measurevar="UG_Offer_Accept", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)
UG_Offer_Accept <- ggplot(data_T0T2, aes(x = timepoint, y = UG_Offer_Accept, fill = Group)) +
  geom_line(data = tgc, aes(x = timepoint, y = UG_Offer_Accept, group = Group, colour = Group), linetype = 1, size = 1)+
  geom_point(data = tgc, aes(x = timepoint, y = UG_Offer_Accept, group = Group, colour = Group), shape = 18, size = 6) +
  geom_errorbar(data = tgc, aes(x = timepoint, y = UG_Offer_Accept, group = Group, colour = Group, ymin = UG_Offer_Accept-se, ymax = UG_Offer_Accept+se), width = .1)+
  theme_classic() + 
  xlab("session") + 
  ylab("Unfair offer acceptance") +
  scale_x_discrete(labels=c("0" = "pre", "1" = "one-year-post"))+
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
tgc <- summarySE(data_T0T2, measurevar="tot_delay_perc", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

tot_delay_perc <- ggplot(data_T0T2, aes(x = timepoint, y = tot_delay_perc, fill = Group)) +
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
  scale_x_discrete(labels=c("0" = "pre", "1" = "one-year-post"))+
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

#mental health
my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T2, measurevar="internalising_factor", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

internalising_factor <- ggplot(data_T0T2, aes(x = timepoint, y = internalising_factor, fill = Group)) +
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
  scale_x_discrete(labels=c("0" = "pre", "1" = "one-year-post"))+
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
internalising_factor
ggsave('Raincloudplot_internalising_factor.jpg', width = 16, height = 10)

my_palette = (c("#404080","#69b3a2"))
tgc <- summarySE(data_T0T2, measurevar="externalising_factor", groupvars=c("timepoint","Group"),na.rm=TRUE)
tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)

externalising_factor <- ggplot(data_T0T2, aes(x = timepoint, y = externalising_factor, fill = Group)) +
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
  scale_x_discrete(labels=c("0" = "pre", "1" = "one-year-post"))+
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
externalising_factor
ggsave('Raincloudplot_externalising_factor.jpg', width = 16, height = 10)


mainplot1<-ggarrange(corsi_max_wm_t, PBI_t, cogflex_t,
                    ncol=3, nrow=1, common.legend = TRUE, legend="none")
mainplot2<-ggarrange(DG_Coins_Given, UG_Offer_Accept, tot_delay_perc,
                      ncol=3, nrow=1, common.legend = TRUE, legend="none")
mainplot3<-ggarrange(internalising_factor, externalising_factor,
                     ncol=2, nrow=1, common.legend = TRUE, legend="none")
mainplot_behaviour<-ggarrange(corsi_max_wm_t, PBI_t, cogflex_t,
                              DG_Coins_Given, UG_Offer_Accept, tot_delay_perc,
                              internalising_factor, externalising_factor,
                              ncol=3, nrow=3, common.legend = TRUE, legend="right")
ggsave("raincloud_longterm_fartransfer_behaviour.png",
       width = 7000, height = 7000,units = c("px"))


#raincloudplot
# code just to plot barchart for training effects
#my_palette = (c("#404080","#69b3a2"))
#tgc <- summarySE(data.imp, measurevar="DG_Coins_Given", groupvars=c("timepoint","Group"),na.rm=TRUE)
#tgc$Group = factor(tgc$Group, levels = c("1", "2"), ordered = TRUE)
#data.imp$timepoint<-factor(data.imp$timepoint, levels = c('0', '1', '2'), labels = c('0', '1','2'))
#
#DG_Coins_Given <- ggplot(data.imp, aes(x = timepoint, y = DG_Coins_Given, fill = Group)) +
#  geom_flat_violin(aes(fill = Group),position = position_nudge(x = .1,
#                                                               y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
#  geom_point(aes(x = as.numeric(timepoint)-.15, y =DG_Coins_Given, colour = Group), show.legend = TRUE,
#             position = position_jitter(width = .05), size = 2, shape = 20)+ scale_y_continuous(limits = c(-2,5)) +
#  geom_line(data = tgc, aes(x = as.numeric(timepoint)+.1, y = DG_Coins_Given, group = Group, colour = Group), linetype = 3, size = 1)+
#  geom_point(data = tgc, aes(x = as.numeric(timepoint)+.1, y = DG_Coins_Given, group = Group, colour = Group), shape = 18, size = 4) +
#  geom_errorbar(data = tgc, aes(x = as.numeric(timepoint)+.1, y = DG_Coins_Given, group = Group, colour = Group, ymin = DG_Coins_Given-se, ymax = DG_Coins_Given+se), width = .1)+
#  theme_classic() + 
#  xlab("session") + 
#  ylab("DG_Coins_Given") +
#  scale_x_discrete(labels=c("0" = "pre-test", "1" = "post-test", "2" = "one-year-post"))+
#  scale_colour_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+
#  scale_fill_manual(labels=c("1" = "experimental", "2" = "control"), values = my_palette)+ theme(text = element_text(size = 20))+
#  ggtitle("")+
#  theme(
#    axis.title.y = element_text(vjust = +1),
#    axis.title.x = element_text(vjust = -0.7)) 
#ggsave('Raincloudplot.png', width = w, height = h)
#coord_flip()+
#DG_Coins_Given
#ggsave('Raincloudplot_DG_Coins_Given.jpg', width = 16, height = 10)


