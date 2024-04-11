#correlation analysis of inhibition and other domains at T0

setwd("~/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/training study data/mixed model")

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
data.imp$Gender <- factor(data.imp$Gender)

# Correlations - at T0
data_baseline <- data.imp[data.imp$timepoint %in% c("0"), ]
# academic score
x<-cor.test(data_baseline$prob_stopsig, data_baseline$academic_score,  method = "pearson", use = "complete.obs")
AcademicScore_df<-as.numeric(x["parameter"])
AcademicScore_t<-as.numeric(x["statistic"])
AcademicScore_p<-as.numeric(x["p.value"])

# creativity 
x<-cor.test(data_baseline$prob_stopsig, data_baseline$creativity,  method = "pearson", use = "complete.obs")
creativity_df<-as.numeric(x["parameter"])
creativity_t<-as.numeric(x["statistic"])
creativity_p<-as.numeric(x["p.value"])


# decision making - Dictator & Ultimatum game
x<-cor.test(data_baseline$prob_stopsig, data_baseline$UG_Offer_Accept,  method = "pearson", use = "complete.obs")
UG_Offer_Accept_df<-as.numeric(x["parameter"])
UG_Offer_Accept_t<-as.numeric(x["statistic"])
UG_Offer_Accept_p<-as.numeric(x["p.value"])


x<-cor.test(data_baseline$prob_stopsig, data_baseline$DG_Coins_Given,  method = "pearson", use = "complete.obs")
DG_Coins_Given_df<-as.numeric(x["parameter"])
DG_Coins_Given_t<-as.numeric(x["statistic"])
DG_Coins_Given_p<-as.numeric(x["p.value"])


x<-cor.test(data_baseline$prob_stopsig, data_baseline$DG_UG_Diff,  method = "pearson", use = "complete.obs")
DG_UG_Diff_df<-as.numeric(x["parameter"])
DG_UG_Diff_t<-as.numeric(x["statistic"])
DG_UG_Diff_p<-as.numeric(x["p.value"])

# decision making - Temporal discounting
x<-cor.test(data_baseline$prob_stopsig, data_baseline$tot_delay_perc,  method = "pearson", use = "complete.obs")
tot_delay_perc_df<-as.numeric(x["parameter"])
tot_delay_perc_t<-as.numeric(x["statistic"])
tot_delay_perc_p<-as.numeric(x["p.value"])


# mental health 
x<-cor.test(data_baseline$prob_stopsig, data_baseline$externalising_factor,  method = "pearson", use = "complete.obs")
externalising_factor_df<-as.numeric(x["parameter"])
externalising_factor_t<-as.numeric(x["statistic"])
externalising_factor_p<-as.numeric(x["p.value"])

x<-cor.test(data_baseline$prob_stopsig, data_baseline$internalising_factor,  method = "pearson", use = "complete.obs")
internalising_factor_df<-as.numeric(x["parameter"])
internalising_factor_t<-as.numeric(x["statistic"])
internalising_factor_p<-as.numeric(x["p.value"])


# Executive Function
x<-cor.test(data_baseline$prob_stopsig, data_baseline$shifting_T0,  method = "pearson", use = "complete.obs")
shifting_T0_df<-as.numeric(x["parameter"])
shifting_T0_t<-as.numeric(x["statistic"])
shifting_T0_p<-as.numeric(x["p.value"])

x<-cor.test(data_baseline$prob_stopsig, data_baseline$memory_T0,  method = "pearson", use = "complete.obs")
memory_T0_df<-as.numeric(x["parameter"])
memory_T0_t<-as.numeric(x["statistic"])
memory_T0_p<-as.numeric(x["p.value"])


# Brain Measures fMRI Task Based
x<-cor.test(data_baseline$prob_stopsig, data_baseline$HavardOxford_Cortl,  method = "pearson", use = "complete.obs")
HavardOxford_Cortl_df<-as.numeric(x["parameter"])
HavardOxford_Cortl_t<-as.numeric(x["statistic"])
HavardOxford_Cortl_p<-as.numeric(x["p.value"])

# Brain Measures Resting States
x<-cor.test(data_baseline$prob_stopsig, data_baseline$DMN,  method = "pearson", use = "complete.obs")
DMN_df<-as.numeric(x["parameter"])
DMN_t<-as.numeric(x["statistic"])
DMN_p<-as.numeric(x["p.value"])

x<-cor.test(data_baseline$prob_stopsig, data_baseline$FPN,  method = "pearson", use = "complete.obs")
FPN_df<-as.numeric(x["parameter"])
FPN_t<-as.numeric(x["statistic"])
FPN_p<-as.numeric(x["p.value"])

x<-cor.test(data_baseline$prob_stopsig, data_baseline$CON,  method = "pearson", use = "complete.obs")
CON_df<-as.numeric(x["parameter"])
CON_t<-as.numeric(x["statistic"])
CON_p<-as.numeric(x["p.value"])

# Brain Measures Structural
x<-cor.test(data_baseline$prob_stopsig, data_baseline$CorticalThickness,  method = "pearson", use = "complete.obs")
CorticalThickness_df<-as.numeric(x["parameter"])
CorticalThickness_t<-as.numeric(x["statistic"])
CorticalThickness_p<-as.numeric(x["p.value"])

# Brain Measures DTI
x<-cor.test(data_baseline$prob_stopsig, data_baseline$Rputa.frontal_FA,  method = "pearson", use = "complete.obs")
Rputa.frontal_FA_df<-as.numeric(x["parameter"])
Rputa.frontal_FA_t<-as.numeric(x["statistic"])
Rputa.frontal_FA_p<-as.numeric(x["p.value"])

x<-cor.test(data_baseline$prob_stopsig, data_baseline$Rputa.frontal_MD,  method = "pearson", use = "complete.obs")
Rputa.frontal_MD_df<-as.numeric(x["parameter"])
Rputa.frontal_MD_t<-as.numeric(x["statistic"])
Rputa.frontal_MD_p<-as.numeric(x["p.value"])



#save values
domain<-c('AcademicScore', 'creativity', 
          'UG_Offer_Accept', 'DG_Coins_Given', 'DG_UG_Diff', 'tot_delay_perc',
          'externalising_factor', 'internalising_factor', 
          'shifting_T0','memory_T0',
          'HavardOxford_Cortl', 'DMN', 'FPN', 'CON', 'CorticalThickness', 
          'Rputa.frontal_FA', 'Rputa.frontal_MD')
df <- as.numeric(list(AcademicScore_df, creativity_df,
                      UG_Offer_Accept_df, DG_Coins_Given_df, DG_UG_Diff_df, tot_delay_perc_df,
                      externalising_factor_df, internalising_factor_df, 
                      shifting_T0_df, memory_T0_df,
                      HavardOxford_Cortl_df, DMN_df, FPN_df, CON_df, CorticalThickness_df,
                      Rputa.frontal_FA_df, Rputa.frontal_MD_df))
t.score <- as.numeric(list(AcademicScore_t, creativity_t,
                           UG_Offer_Accept_t, DG_Coins_Given_t, DG_UG_Diff_t, tot_delay_perc_t,
                           externalising_factor_t, internalising_factor_t, 
                           shifting_T0_t, memory_T0_t,
                           HavardOxford_Cortl_t, DMN_t, FPN_t, CON_t, CorticalThickness_t,
                           Rputa.frontal_FA_t, Rputa.frontal_MD_t))
P_values <-as.numeric(list(AcademicScore_p, creativity_p,
                          UG_Offer_Accept_p, DG_Coins_Given_p, DG_UG_Diff_p, tot_delay_perc_p,
                          externalising_factor_p, internalising_factor_p, 
                          shifting_T0_p, memory_T0_p,
                          HavardOxford_Cortl_p, DMN_p, FPN_p, CON_p, CorticalThickness_p,
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
k$t.score <- t.score
write.csv(k, "correlation_T0_pStop.csv")


#scatter plot use inhibition 
#only use T0 

#decision making 
DG_Coins_Given_scatter <-ggplot(data_baseline, aes(x=inhibition_T0, y=DG_Coins_Given)) + 
  geom_point(color='#9999CC') + 
  geom_smooth(method=lm, color="#69b3a2")+
  theme_classic()+
  labs(#title="Academic Performance and Inhibition Factor",
    x="Inhibition", y = "DG Offer")+theme(text = element_text(size=20))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 20))+ggtitle("a")
DG_Coins_Given_scatter
ggsave('scatterplot_DG_Coins_Given.jpg', width = 16, height = 10)

DG_UG_Diff_scatter <-ggplot(data_baseline, aes(x=inhibition_T0, y=DG_UG_Diff)) + 
  geom_point(color='#9999CC') + 
  geom_smooth(method=lm, color="#69b3a2")+
  theme_classic()+
  labs(#title="Academic Performance and Inhibition Factor",
    x="Inhibition", y = "UG-DG Offer")+theme(text = element_text(size=20))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 20))+ggtitle("b")
DG_UG_Diff_scatter
ggsave('scatterplot_DG_UG_Diff.jpg', width = 16, height = 10)


UG_Offer_Accept_scatter <-ggplot(data_baseline, aes(x=inhibition_T0, y=UG_Offer_Accept)) + 
  geom_point(color='#9999CC') + 
  geom_smooth(method=lm, color="#69b3a2")+
  theme_classic()+
  labs(#title="Academic Performance and Inhibition Factor",
    x="Inhibition", y = "Unfair offer acceptance")+theme(text = element_text(size=20))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 20))+ggtitle("c")
UG_Offer_Accept_scatter
ggsave('scatterplot_UG_Offer_Accept.jpg', width = 16, height = 10)

tot_delay_perc_scatter <-ggplot(data_baseline, aes(x=inhibition_T0, y=tot_delay_perc)) + 
  geom_point(color='#9999CC') + 
  geom_smooth(method=lm, color="#69b3a2")+
  theme_classic()+
  labs(#title="Academic Performance and Inhibition Factor",
    x="Inhibition", y = "Percentage of Delayed Choice (%)")+theme(text = element_text(size=18))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 20))+ggtitle("d")
tot_delay_perc_scatter
ggsave('scatterplot_tot_delay_perc.jpg', width = 16, height = 10)


mainplot<-ggarrange(DG_Coins_Given_scatter, DG_UG_Diff_scatter, UG_Offer_Accept_scatter, tot_delay_perc_scatter, 
                    ncol=2, nrow=2, common.legend = TRUE, legend="right")
ggsave("correaltion_decisionmaking.png",
       width = 3500, height = 2500,units = c("px"))

#EF other factor 
shifting_T0_scatter <-ggplot(data_baseline, aes(x=inhibition_T0, y=shifting_T0)) + 
  geom_point(color='#9999CC') + 
  geom_smooth(method=lm, color="#69b3a2")+
  theme_classic()+
  labs(#title="Academic Performance and Inhibition Factor",
    x="Inhibition", y = "Shifting")+theme(text = element_text(size=20))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 20))+ggtitle("a"))
shifting_T0_scatter
ggsave('scatterplot_shifting_T0.jpg', width = 16, height = 10)

memory_T0_scatter <-ggplot(data_baseline, aes(x=inhibition_T0, y=memory_T0)) + 
  geom_point(color='#9999CC') + 
  geom_smooth(method=lm, color="#69b3a2")+
  theme_classic()+
  labs(#title="Academic Performance and Inhibition Factor",
    x="Inhibition", y = "Memory")+theme(text = element_text(size=20))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 20))+ggtitle("b")
memory_T0_scatter
ggsave('scatterplot_memory_T0.jpg', width = 16, height = 10)


mainplot<-ggarrange(shifting_T0_scatter, memory_T0_scatter, 
                    ncol=2, nrow=1, common.legend = TRUE, legend="right")
ggsave("correlationT0_EF_OtherFactors.png",
       width = 3500, height = 2000,units = c("px"))

#academic score
AcademicScore_scatter <-ggplot(data_baseline, aes(x=inhibition_T0, y=academic_score)) + 
  geom_point(color='#9999CC') + 
  geom_smooth(method=lm, color="#69b3a2")+
  theme_classic()+
  labs(#title="Academic Performance and Inhibition Factor",
    x="Inhibition", y = "Reading / Maths")+theme(text = element_text(size=20))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 20))+ggtitle("a")
AcademicScore_scatter
ggsave('scatterplot_AcademicScore.jpg', width = 16, height = 10)

#creativity
creativity_scatter <-ggplot(data_baseline, aes(x=inhibition_T0, y=creativity)) + 
  geom_point(color='#9999CC') + 
  geom_smooth(method=lm, color="#69b3a2")+
  theme_classic()+
  labs(#title="Academic Performance and Inhibition Factor",
    x="Inhibition", y = "Creativity Scores")+theme(text = element_text(size=20))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 20))+ggtitle("b")
creativity_scatter
ggsave('scatterplot_creativity.jpg', width = 16, height = 10)

mainplot<-ggarrange(AcademicScore_scatter, creativity_scatter, 
                    ncol=2, nrow=1, common.legend = TRUE, legend="right")
ggsave("correlationT0_academic_creativity.png",
       width = 3500, height = 2000,units = c("px"))


#mental health 
internalising_factor_scatter <-ggplot(data_baseline, aes(x=inhibition_T0, y=internalising_factor)) + 
  geom_point(color='#9999CC') + 
  geom_smooth(method=lm, color="#69b3a2")+
  theme_classic()+
  labs(#title="Academic Performance and Inhibition Factor",
    x="Inhibition", y = "Internalising problems")+theme(text = element_text(size=20))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 20))+ggtitle("a")
internalising_factor_scatter
ggsave('scatterplot_internalising_factor.jpg', width = 16, height = 10)

externalising_factor_scatter <-ggplot(data_baseline, aes(x=inhibition_T0, y=externalising_factor)) + 
  geom_point(color='#9999CC') + 
  geom_smooth(method=lm, color="#69b3a2")+
  theme_classic()+
  labs(#title="Academic Performance and Inhibition Factor",
    x="Inhibition", y = "Externalising problems")+theme(text = element_text(size=20))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 20))+ggtitle("b")
externalising_factor_scatter
ggsave('scatterplot_externalising_factor.jpg', width = 16, height = 10)

mainplot<-ggarrange(internalising_factor_scatter, externalising_factor_scatter, 
                    ncol=2, nrow=1, common.legend = TRUE, legend="right")
ggsave("correlationT0_mentalhealth.png",
       width = 3500, height = 2000,units = c("px"))

#brain fMRI task based
HavardOxford_Cortl_scatter <-ggplot(data_baseline, aes(x=inhibition_T0, y=HavardOxford_Cortl)) + 
  geom_point(color='#9999CC') + 
  geom_smooth(method=lm, color="#69b3a2")+
  theme_classic()+
  labs(#title="Academic Performance and Inhibition Factor",
    x="Inhibition", y = "Activation rIFG")+theme(text = element_text(size=20))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 20))+ggtitle("a")
HavardOxford_Cortl_scatter
ggsave('scatterplot_HavardOxford_Cortl.jpg', width = 16, height = 10)

#brain structural 
CorticalThickness_scatter <-ggplot(data_baseline, aes(x=inhibition_T0, y=CorticalThickness)) + 
  geom_point(color='#9999CC') + 
  geom_smooth(method=lm, color="#69b3a2")+
  theme_classic()+
  labs(#title="Academic Performance and Inhibition Factor",
    x="Inhibition", y = "rIFG Cortical thickness (mm)")+theme(text = element_text(size=20))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 20))+ggtitle("b")
CorticalThickness_scatter
ggsave('scatterplot_CorticalThickness.jpg', width = 16, height = 10)

mainplot<-ggarrange(HavardOxford_Cortl_scatter, CorticalThickness_scatter, 
                    ncol=2, nrow=1, common.legend = TRUE, legend="right")
ggsave("correlationT0_rIFG.png",
       width = 3500, height = 2000,units = c("px"))


#brain fMRI resting state
CON_scatter <-ggplot(data_baseline, aes(x=inhibition_T0, y=CON)) + 
  geom_point(color='#9999CC') + 
  geom_smooth(method=lm, color="#69b3a2")+
  theme_classic()+
  labs(#title="Academic Performance and Inhibition Factor",
    x="Inhibition", y = "Connectivity from Cingulo-opercular Network")+theme(text = element_text(size=20))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 20))+ggtitle("a")
CON_scatter
ggsave('scatterplot_CON.jpg', width = 16, height = 10)

FPN_scatter <-ggplot(data_baseline, aes(x=inhibition_T0, y=FPN)) + 
  geom_point(color='#9999CC') + 
  geom_smooth(method=lm, color="#69b3a2")+
  theme_classic()+
  labs(#title="Academic Performance and Inhibition Factor",
    x="Inhibition", y = "Connectivity from Frontoparetial Network")+theme(text = element_text(size=20))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 20))+ggtitle("b")
FPN_scatter
ggsave('scatterplot_FPN.jpg', width = 16, height = 10)

DMN_scatter <-ggplot(data_baseline, aes(x=inhibition_T0, y=DMN)) + 
  geom_point(color='#9999CC') + 
  geom_smooth(method=lm, color="#69b3a2")+
  theme_classic()+
  labs(#title="Academic Performance and Inhibition Factor",
    x="Inhibition", y = "Connectivity from Default Mode Network ")+theme(text = element_text(size=20))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 20))+ggtitle("c")
DMN_scatter
ggsave('scatterplot_DMN.jpg', width = 16, height = 10)


mainplot<-ggarrange(CON_scatter, FPN_scatter, DMN_scatter, 
                    ncol=3, nrow=1, common.legend = TRUE, legend="right")
ggsave("correlation_brain_network.png",
       width = 5500, height = 2500,units = c("px"))

#DTI
Rputa.frontal_FA_scatter <-ggplot(data_baseline, aes(x=inhibition_T0, y=Rputa.frontal_FA)) + 
  geom_point(color='#9999CC') + 
  geom_smooth(method=lm, color="#69b3a2")+
  theme_classic()+
  labs(#title="Academic Performance and Inhibition Factor",
    x="Inhibition", y = "Right fronto-striatal tract putamen (FA)")+theme(text = element_text(size=20))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 20))+ggtitle("a")
Rputa.frontal_FA_scatter
ggsave('scatterplot_Rputa.frontal_FA.jpg', width = 16, height = 10)

Rputa.frontal_MD_scatter <-ggplot(data_baseline, aes(x=inhibition_T0, y=Rputa.frontal_MD)) + 
  geom_point(color='#9999CC') + 
  geom_smooth(method=lm, color="#69b3a2")+
  theme_classic()+
  labs(#title="Academic Performance and Inhibition Factor",
    x="Inhibition", y = "Right fronto-striatal tract putamen (MD)")+theme(text = element_text(size=20))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 20))+ggtitle("b")
Rputa.frontal_MD_scatter
ggsave('scatterplot_Rputa.frontal_MD.jpg', width = 16, height = 10)


Rputa.frontal_RD_scatter <-ggplot(data_baseline, aes(x=inhibition_T0, y=Rputa.frontal_RD)) + 
  geom_point(color='#9999CC') + 
  geom_smooth(method=lm, color="#69b3a2")+
  theme_classic()+
  labs(#title="Academic Performance and Inhibition Factor",
    x="Inhibition", y = "Right fronto-striatal tract putamen (RD)")+theme(text = element_text(size=20))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 20))+ggtitle("c")
Rputa.frontal_RD_scatter
ggsave('scatterplot_Rputa.frontal_RD.jpg', width = 16, height = 10)

Rcaud.frontal_FA_scatter <-ggplot(data_baseline, aes(x=inhibition_T0, y=Rcaud.frontal_FA)) + 
  geom_point(color='#9999CC') + 
  geom_smooth(method=lm, color="#69b3a2")+
  theme_classic()+
  labs(#title="Academic Performance and Inhibition Factor",
    x="Inhibition", y = "Right fronto-striatal tract caudate (FA)")+theme(text = element_text(size=20))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 20))+ggtitle("d")
Rcaud.frontal_FA_scatter
ggsave('scatterplot_Rcaud.frontal_FA.jpg', width = 16, height = 10)


Rcaud.frontal_MD_scatter <-ggplot(data_baseline, aes(x=inhibition_T0, y=Rcaud.frontal_MD)) + 
  geom_point(color='#9999CC') + 
  geom_smooth(method=lm, color="#69b3a2")+
  theme_classic()+
  labs(#title="Academic Performance and Inhibition Factor",
    x="Inhibition", y = "Right fronto-striatal tract caudate (MD)")+theme(text = element_text(size=20))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 20))+ggtitle("e")
Rcaud.frontal_MD_scatter
ggsave('scatterplot_Rcaud.frontal_MD.jpg', width = 16, height = 10)

Rcaud.frontal_RD_scatter <-ggplot(data_baseline, aes(x=inhibition_T0, y=Rcaud.frontal_RD)) + 
  geom_point(color='#9999CC') + 
  geom_smooth(method=lm, color="#69b3a2")+
  theme_classic()+
  labs(#title="Academic Performance and Inhibition Factor",
    x="Inhibition", y = "Right fronto-striatal tract caudate (RD)")+theme(text = element_text(size=20))+
  theme(legend.title = element_text(size = 18), legend.text = element_text(size = 20))+ggtitle("f")
Rcaud.frontal_RD_scatter
ggsave('scatterplot_Rcaud.frontal_RD.jpg', width = 16, height = 10)

mainplot<-ggarrange(Rputa.frontal_FA_scatter, Rputa.frontal_MD_scatter, Rputa.frontal_RD_scatter,
                    Rcaud.frontal_FA_scatter, Rcaud.frontal_MD_scatter, Rcaud.frontal_RD_scatter, 
                    ncol=3, nrow=2, common.legend = TRUE, legend="right")
ggsave("correlation_DTI.png",
       width = 6000, height = 4000,units = c("px"))
