# This script analyses IDEAS data

# Set working directory
setwd("/Users/zoeli/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/training study/WASI")

# Load packages
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

source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

# Turn off scientific notation
options(scipen = 999)
options(max.print=1000000)

# Read in dataset
#load data here
#########################IMPUTATION FOR Fssrt FIRST#############################################

data_excel<-read.csv("wasi_data.csv")
data <- as.data.frame(data_excel)
attach(data) #access variables in data

# Specify factors
data$id <- factor(data$id)
data$timepoint <- factor(data$timepoint)
data$Group <- factor(data$Group,levels = c('1', '2'), 
                     labels = c('1', '2'))
data$Gender <- factor(data$Gender)


# Select only T0 & T1 data
#data <- data[data$session %in% c("T0", "T1"), ]

# Summary data
summary(data)

# Define measures for loops
measures <- c('wasi')
tags <- c('A')

# Check for outliers & replace by NA
for(n in 1:length(measures)){
  outlier_values <- boxplot.stats(eval(parse(text=paste("data","$",measures[[n]],sep=""))), coef = 2)$out # outlier values.
  assign(paste("outlier_rowID_",measures[[n]],sep=""),which(eval(parse(text=paste("data","$",measures[[n]],sep=""))) %in% c(outlier_values)))
  boxplot(eval(parse(text=paste("data","$",measures[[n]],sep=""))), main=measures[n], boxwex=0.1)
  mtext(paste("Outliers: ", paste(eval(parse(text=paste("outlier_rowID_",measures[[n]],sep=""))), collapse=", ")), cex=0.6)
  eval(parse(text=paste("data","$",measures[[n]],'[outlier_rowID_',measures[[n]],']<-NA',sep="")))
}

## Percentage missing data
sink("check_missing_data_percentage.txt")
### Whole dataset
print("Percentage missing data for whole dataset")
print(sum(is.na(data))/(nrow(data)*ncol(data)))
### Measure-wise
for(n in 1:length(measures)){
  print(paste("Percentage missing data for ",measures[[n]],sep=""))
  print(sum(is.na(eval(parse(text=paste("data","$",measures[[n]],sep="")))))/(nrow(data)))
}
sink()
## Pattern of missing data
missing_plot(data)
ggsave(paste("check_missing_data_map.png",sep=""),
       width = 7,height = 7)


# Impute data
# Define measures for loops
measures <- c('wasi')
set.seed(123)
ini <- mice(data, maxit=0, pri=F) #get predictor matrix
pred <- ini$predictorMatrix
pred[,c("id","TotalSessions","Gender")] <- 0 #don't use as predictor
meth <- ini$method
meth[c("id","timepoint","Group", "TotalSessions", "Gender",
       "Age", "trainingslope")] <- "" #don't impute these variables, use only as predictors
imp <- mice(data, m=100, 
            maxit=20, printFlag=TRUE, 
            predictorMatrix=pred, method=meth) #impute data with 100 imputations and 20 iterations


# Select a single dataset for post-hoc tests
implist <- mids2mitml.list(imp) #create a list of completed data sets
data.imp <- implist[[1]]
write_csv(data.imp,"wasi_imputated.csv") #save imputed dataset to use for analyses


# Checks on missing/imputed data: check distribution of imputed data (red) is similar to observed data (blue)
## create function
fortify.mids <- function(x){
  imps <- do.call(rbind, lapply(seq_len(x$m), function(i){
    data.frame(complete(x, i), Imputation = i, Data = "Imputed")
  }))
  orig <- cbind(x$data, Imputation = NA, Data = "Observed")
  rbind(imps, orig)
}
## pooled: plot & save
plotList <- list()
for(n in 1:length(measures)){
  plotName <- paste( 'p', n, sep = '' )
  (plotList[[plotName]] <- local({
    n <- n
    ggplot(fortify.mids(imp), aes(x = eval(parse(text=measures[[n]])), colour = Data, group = Imputation)) +
      geom_density(size = 1) +
      scale_colour_manual(values = c(Imputed = "lightsalmon2", Observed = "dodgerblue1")) +
      labs(x=measures[[n]]) +
      theme_grey(base_size=10) +
      theme(legend.position="none")
  }))
}
ggarrange(plotList$p1, plotList$p2, plotList$p3, plotList$p4, plotList$p5, plotList$p6, plotList$p7, plotList$p8, plotList$p9,
          plotList$p10, plotList$p11, plotList$p12, plotList$p13, plotList$p14, plotList$p15, plotList$p16, plotList$p17, plotList$p18,
          plotList$p19, plotList$p20, plotList$p21, plotList$p22, plotList$p23, plotList$p24, plotList$p25, plotList$p26, plotList$p27,
          ncol=6, nrow=5)
ggsave("check_imputed_data_densplot.png",
       width = 3500, height = 2000,units = c("px"))
## single: plot & save
plotList <- list()
for(n in 1:length(measures)){
  plotName <- paste( 'p', n, sep = '' )
  (plotList[[plotName]] <- local({
    n <- n
    ggplot() +
      geom_density(aes(x = eval(parse(text=measures[[n]])), colour = "data1"), alpha = .2, size = 1, data = data.imp) +
      geom_density(aes(x = eval(parse(text=measures[[n]])), colour = "data2"), , alpha = .2, size = 1, data = data) +
      scale_colour_manual(values = c(data1 = "lightsalmon2", data2 = "dodgerblue1")) +
      labs(x=measures[[n]]) +
      theme_grey(base_size=10) +
      theme(legend.position="none")
  }))
}
ggarrange(plotList$p1, plotList$p2, plotList$p3, plotList$p4, plotList$p5, plotList$p6, plotList$p7, plotList$p8, plotList$p9,
          plotList$p10, plotList$p11, plotList$p12, plotList$p13, plotList$p14, plotList$p15, plotList$p16, plotList$p17, plotList$p18,
          plotList$p19, plotList$p20, plotList$p21, plotList$p22, plotList$p23, plotList$p24, plotList$p25, plotList$p26, plotList$p27,
          ncol=6, nrow=5)
ggsave("check_imputed_data_densplot_single.png",
       width = 3500, height = 2000,units = c("px"))







##mixed models##
#data.imp$p_id <- factor(data.imp$p_id)
#data.imp$group<-as.factor(data.imp$group)
#data.imp$session<-as.factor(data.imp$session)
data.imp<-read.csv("EF_imputed_twotimepts.csv")
temp<-data.imp
# Select only T0 & T1 data
temp <- temp[temp$session %in% c("t0", "t1"), ]

#data.imp<-read.csv("SSRT-FUNC-mentalhealth_imputedT2.csv")


### comparing non-imputed data - call 'data'### 

#formula <- REzssrtnr_t ~ session*group + (1|p_id)
#anova(lmer( formula, data=data, REML=TRUE))
#anova(lmer( formula, data=data.imp, REML=TRUE))
#formula <- corsi_max_wm_t ~ session*group + (1|p_id)
#anova(lmer( formula, data=data, REML=TRUE))
#anova(lmer( formula, data=data.imp, REML=TRUE))
#formula <- REflankerinh_t ~ session*group + (1|p_id)
#formula <- scaled ~ session*group + (1|p_id)
# anova(lmer( formula, data=data, REML=TRUE))
#anova(lmer( formula, data=data.imp, REML=TRUE))

#formula <- PBI_t ~ session*group + (1|p_id)
#anova(lmer( formula, data=data, REML=TRUE))
#anova(lmer( formula, data=data.imp, REML=TRUE))


###difference in groups####
#formula <- REflankerswitch_t ~ session*group + (1|p_id)
#anova(lmer( formula, data=data, REML=TRUE))
#anova(lmer( formula, data=data.imp, REML=TRUE))


#formula <- cf_switchrt_t ~ session*group + (1|p_id)
# anova(lmer( formula, data=data, REML=TRUE))
#anova(lmer( formula, data=data.imp, REML=TRUE))

#formula <- cogflex_t ~ session*group + (1|p_id)
# anova(lmer( formula, data=data, REML=TRUE))
#anova(lmer( formula, data=data.imp, REML=TRUE))


###difference in groups with imputation###
#formula <- dprimeONEBACK_t ~ session*group + (1|p_id)
#anova(lmer( formula, data=data, REML=TRUE))
#anova(lmer( formula, data=data.imp, REML=TRUE))

###difference in groups with imputation###
#formula <- dprimeTWOBACK_t ~ session*group + (1|p_id)
#formula <- scaled ~ session*group + (1|p_id)
#anova(lmer( formula, data=data, REML=TRUE))
#anova(lmer( formula, data=data.imp, REML=TRUE))


#formula <- stroop_t ~ session*group + (1|p_id)
#formula <- scaled ~ session*group + (1|p_id)
# anova(lmer( formula, data=data, REML=TRUE))
#anova(lmer( formula, data=data.imp, REML=TRUE))
#model<-lmer( formula, data=data.imp, REML=TRUE)
#print(emmeans(model, pairwise~session*group))





f#ormula <- Fssrtnr_t ~ session*group + (1|p_id)
#models<-list()
#M <- length(implist)
#for (mm in 1:M){
#  models[[mm]] <- lme4::lmer( formula, data=implist[[mm]], REML=FALSE)
#}

#res1 <- miceadds::lmer_pool(models)
#summary(res1)


##bind both files 

#x <- read.csv("SSRT-FUNC-mentalhealth_imputedT2.csv")
#y<-read.csv("SSRT-FUNC-mentalhealth_imputed.csv")
##select functional data## 
#y <- data.frame(matrix(ncol = 0, nrow = 526))

##select T2 data from x##

# x <- x[x$session %in% c("t2"),]
# ##add empty inhibition variable for x##
# x$inhibitionnet_t<-NA
# total <- rbind(y, x)
# write_csv(total,"RecombinedIMPUTED_T2addedtooriginal.csv")
# total <- read.csv("RecombinedIMPUTED_T2addedtooriginal.csv")
# data.wide<-reshape(total, idvar = "p_id", timevar = "session", direction = "wide")
# write_csv(data.wide,"WIDERecombinedIMPUTED_T2addedtooriginal.csv")
# y.wide<-reshape(y, idvar = "p_id", timevar = "session", direction = "wide")
# dats.wide<-reshape(data.imp, idvar = "p_id", timevar = "session", direction = "wide")
# total <- merge(dats.wide,y.wide,by=c("p_id"))


tgc <- summarySE(data.imp, measurevar="REflankerinh_t", groupvars=c("session","group"),na.rm=TRUE)
tgc$group = factor(tgc$group, levels = c("Exp", "Con"), ordered = TRUE)

jpeg("C:/Users/keertana/Desktop/MAINSTUDY/T1/final output/Graphs/Training_Bars/EF/REFLANK.jpg")

# Error bars represent standard error of the mean
ggplot(tgc, aes(x=group, y=REflankerinh_t, fill=session)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=REflankerinh_t-se, ymax=REflankerinh_t+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +theme_classic() + xlab('Group') +
  ylab('reflank') +   scale_fill_discrete(name="Session", labels=c( "pre-training", "post-training")) +scale_x_discrete(labels=c("1" = "Experimental", "2" = "Control"))+theme(text = element_text(size=20))+ #+coord_cartesian(ylim=c(100,300))+
  theme(legend.title = element_text(size = 15), 
        legend.text = element_text(size = 12))


dev.off()

tgc <- summarySE(x, measurevar="meango_corrrtnr_t", groupvars=c("session","group"),na.rm=TRUE)
tgc$group = factor(tgc$group, levels = c("Exp", "Con"), ordered = TRUE)


jpeg("C:/Users/keertana/Desktop/MAINSTUDY/T1/final output/Graphs/Training_Bars/Imputed_T2/GoRT_inputed.jpg")

# Error bars represent standard error of the mean
ggplot(tgc, aes(x=group, y=meango_corrrtnr_t, fill=session)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=meango_corrrtnr_t-se, ymax=meango_corrrtnr_t+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +theme_classic() + xlab('Group') +
  ylab('Go_RT (ms)') +   scale_fill_discrete(name="Session", labels=c( "pre-training", "post-training", "1-year follow-up")) +scale_x_discrete(labels=c("1" = "Experimental", "2" = "Control"))+theme(text = element_text(size=20)) +coord_cartesian(ylim=c(100,800))+
  theme(legend.title = element_text(size = 15), 
        legend.text = element_text(size = 12))


dev.off()


tgc <- summarySE(data.imp, measurevar="adhd_tot_casi_t", groupvars=c("session","group"),na.rm=TRUE)
tgc$group = factor(tgc$group, levels = c("Exp", "Con"), ordered = TRUE)

ggplot(tgc, aes(x=group, y=adhd_tot_casi_t, fill=session)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=adhd_tot_casi_t-se, ymax=adhd_tot_casi_t+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) 

#remove if no trainingslope

data.wide<-data.wide[complete.cases(data.wide[ ,6]),]


data.wide$changetotCASI2 <- data.wide$adhd_tot_casi_t.t2 - data.wide$adhd_tot_casi_t.t0
data.wide$changegort2 <- data.wide$meango_corrrtnr_t.t2 - data.wide$meango_corrrtnr_t.t0
data.wide$changehypCASI2 <- data.wide$adhd_hyp_casi_t.t2 - data.wide$adhd_hyp_casi_t.t0
data.wide$changeinCASI2 <- data.wide$adhd_in_casi_t.t2 - data.wide$adhd_in_casi_t.t0
data.wide$changeSSRT2<-data.wide$Fssrtnr_t.t2-data.wide$Fssrtnr_t.t0
data.wide$changeinhnet2<-data.wide$inhibitionnet_t.t2-data.wide$inhibitionnet_t.t0

Exp <- data.wide[data.wide$group.t0 %in% c("Exp"), ]
Con <- data.wide[data.wide$group.t0 %in% c("Con"), ]


t.test(changeSSRT2 ~ group.t0, data = data.wide)

t.test(Exp$Fssrtnr_t.t0, Exp$Fssrtnr_t.t1, paired = TRUE, alternative = "two.sided")
t.test(Exp$Fssrtnr_t.t0, Exp$Fssrtnr_t.t2, paired = TRUE, alternative = "two.sided")
t.test(Con$Fssrtnr_t.t0, Con$Fssrtnr_t.t1, paired = TRUE, alternative = "two.sided")
t.test(Con$Fssrtnr_t.t0, Con$Fssrtnr_t.t2, paired = TRUE, alternative = "two.sided")

mean(Exp$Fssrtnr_t.t0, na.rm = TRUE)
mean(Exp$Fssrtnr_t.t1, na.rm = TRUE)
mean(Exp$Fssrtnr_t.t2, na.rm = TRUE)

mean(Con$Fssrtnr_t.t0, na.rm = TRUE)
mean(Con$Fssrtnr_t.t1, na.rm = TRUE)
mean(Con$Fssrtnr_t.t2, na.rm = TRUE)


t.test(Exp$meango_corrrtnr_t.t0, Exp$meango_corrrtnr_t.t1, paired = TRUE, alternative = "two.sided")
t.test(Exp$meango_corrrtnr_t.t0, Exp$meango_corrrtnr_t.t2, paired = TRUE, alternative = "two.sided")
t.test(Con$meango_corrrtnr_t.t0, Con$meango_corrrtnr_t.t1, paired = TRUE, alternative = "two.sided")
t.test(Con$meango_corrrtnr_t.t1, Con$meango_corrrtnr_t.t2, paired = TRUE, alternative = "two.sided")


t.test(data.wide$inhibitionnet_t.t0, data.wide$inhibitionnet_t.t1, paired = TRUE, alternative = "two.sided")

t.test(Exp$adhd_tot_casi_t.t0, Exp$adhd_tot_casi_t.t1, paired = TRUE, alternative = "two.sided")
t.test(Exp$adhd_tot_casi_t.t0, Exp$adhd_tot_casi_t.t2, paired = TRUE, alternative = "two.sided")


