library(Rcpp)

library(tidyverse)

library(readxl)

library(writexl)

library(emmeans)

library(lme4)

library(lmerTest)

library(MuMIn)

library(effectsize)

library(ggeffects)

library(broom.mixed)

library(brms)

library(mice)

library(reshape2)

library(summarytools)

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

## Load training data 
#note: 
data_excel<-read.csv(file = "/Users/grace.revill/Desktop/PhD/Rotation2/TrainingData.csv")
data <- as.data.frame(data_excel)


# Summary data
summary(data)

# Define measures for loops
measures <- c('t1_adhd_in_casi','t1_adhd_hyp_casi', 't1_adhd_tot_casi', 't1_cd_casi', 't1_gad_casi',
              't1_mdd_total_casi', 't1_mdd_casi1', 't1_mdd_casi2','t1_sepanx_casi', 't1_socphob_casi',
              't1_emo_sdq', 't1_cd_sdq', 't1_hy_sdq', 't1_pro_sdq', 't1_ext_sdq', 't1_int_sdq')



# Checks on missing data
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
set.seed(123)
ini <- mice(data, maxit=0, pri=F) #get predictor matrix
pred <- ini$predictorMatrix
pred[,c("ID")] <- 0 #don't use as predictor
meth <- ini$method
meth[c("Group", "Age_frac_T0", "TotalSessions")] <- "" #don't impute these variables, use only as predictors
imp <- mice(data, m=100, maxit=20, printFlag=TRUE, predictorMatrix=pred, method=meth) #impute data with 100 imputations and 20 iterations

# Select a single dataset for post-hoc tests
implist <- mids2mitml.list(imp) #create a list of completed data sets
data.imp <- implist[[1]]
write_csv(data.imp,"/Users/grace.revill/Desktop/PhD/Rotation2/mentalhealth_imputed.csv") #save imputed dataset to use for analyses

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
#temp<-data.imp
# Select only T0 & T1 data
#temp <- data[temp$session %in% c("t0", "t1"), ]

#reminder of measures#
#'_t','meango_corrrtnr_t', 'inhibitionnet_t'
#adhd_tot_casi_t', 'adhd_hyp_casi_t', 'adhd_in_casi_t',
#'total_sdq_t', 'ext_sdq_t', 'hy_sdq_t'

#shapiro.test(data.imp$adhd_tot_casi_t)
##not normally distributed
#qqnorm(data.imp$adhd_tot_casi_t, pch = 1, frame = FALSE)
#qqline(data.imp$adhd_tot_casi_t, col = "steelblue", lwd = 2)

res <- var.test(adhd_tot_casi_t ~ group, data = data.imp)
res
##equal variances between the groups 

formula <- adhd_tot_casi_t ~ session*group + (1|p_id)
#formula <- scaled ~ session*group + (1|p_id)
anova(lmer( formula, data=data.imp, REML=TRUE))

formula <- adhd_hyp_casi_t ~ session*group + (1|p_id)
anova(lmer( formula, data=data.imp, REML=FALSE))


formula <- adhd_in_casi_t ~ session*group + (1|p_id)
anova(lmer( formula, data=data.imp, REML=FALSE))


formula <- Fssrtnr_t ~ session*group + (1|p_id)
anova(lmer( formula, data=data.imp, REML=FALSE))

models<-list()
M <- length(implist)
for (mm in 1:M){
  models[[mm]] <- lme4::lmer( formula, data=implist[[mm]], REML=FALSE)
}

res1 <- miceadds::lmer_pool(models)
summary(res1)

data.imp<-data.imp[complete.cases(data.imp[ ,4]),]
##follow up sig group*time interactions
##long to wide##
tgc <- summarySE(data.imp, measurevar="adhd_tot_casi_t", groupvars=c("session","group"),na.rm=TRUE)
tgc <- within(tgc,{
  #p_id <- as.factor(p_id)
  session <- as.factor(session)
  group <- as.factor(group)
})

tgc <- within(tgc,{
  group <- factor(group)
})

jpeg("C:/Users/keertana/Desktop/MAINSTUDY/T1/final output/Graphs/Training_Bars/ADHDtotal_inputed.jpg")
# Error bars represent standard error of the mean
ggplot(tgc, aes(x=group, y=adhd_tot_casi_t, fill=session)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=adhd_tot_casi_t-se, ymax=adhd_tot_casi_t+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +theme_classic() + xlab('Group') +
  ylab('ADHD_total') +   scale_fill_discrete(name="Session", labels=c( "pre-training", "post-training", "1-year follow-up")) +scale_x_discrete(labels=c("1" = "Experimental", "2" = "Control"))+theme(text = element_text(size=20)) +coord_cartesian(ylim=c(1,20))+
  theme(legend.title = element_text(size = 15), 
        legend.text = element_text(size = 12))

dev.off()


tgc <- summarySE(data.imp, measurevar="adhd_in_casi_t", groupvars=c("session","group"),na.rm=TRUE)
jpeg("C:/Users/keertana/Desktop/MAINSTUDY/T1/final output/Graphs/Training_Bars/ADHDIN_inputed.jpg")
ggplot(tgc, aes(x=group, y=adhd_in_casi_t, fill=session)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=adhd_in_casi_t-se, ymax=adhd_in_casi_t+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +theme_classic() + xlab('Group') +
  ylab('ADHD_inhibition') +   scale_fill_discrete(name="Session", labels=c( "pre-training", "post-training", "1-year follow-up")) +scale_x_discrete(labels=c("1" = "Experimental", "2" = "Control"))+theme(text = element_text(size=20)) +coord_cartesian(ylim=c(1,10))+
  theme(legend.title = element_text(size = 15), 
        legend.text = element_text(size = 12))
dev.off()

tgc <- summarySE(data.imp, measurevar="adhd_hyp_casi_t", groupvars=c("session","group"),na.rm=TRUE)
jpeg("C:/Users/keertana/Desktop/MAINSTUDY/T1/final output/Graphs/Training_Bars/ADHDHYP_inputed.jpg")
ggplot(tgc, aes(x=group, y=adhd_hyp_casi_t, fill=session)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=adhd_hyp_casi_t-se, ymax=adhd_hyp_casi_t+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +theme_classic() + xlab('Group') +
  ylab('ADHD_hyperactivity') +   scale_fill_discrete(name="Session", labels=c( "pre-training", "post-training", "1-year follow-up")) +scale_x_discrete(labels=c("1" = "Experimental", "2" = "Control"))+theme(text = element_text(size=20)) +coord_cartesian(ylim=c(1,10))+
  theme(legend.title = element_text(size = 15), 
        legend.text = element_text(size = 12))
dev.off()

data.wide<-reshape(data.imp, idvar = "p_id", timevar = "session", direction = "wide")

##first tot casi ADHD 
Exp <- data.wide[data.wide$group.t0 %in% c("Exp"), ]
Con <- data.wide[data.wide$group.t0 %in% c("Con"), ]


##delete any NA rows##
Exp[complete.cases(Exp[ , 10,20]),]
Con[complete.cases(Con[ , 10:20]),]

##total##
mean(data.wide$adhd_tot_casi_t.t0, na.rm = TRUE)

t.test(Exp$adhd_tot_casi_t.t0, Exp$adhd_tot_casi_t.t1, paired = TRUE, alternative = "two.sided")
mean(Exp$adhd_tot_casi_t.t0, na.rm = TRUE)
mean(Exp$adhd_tot_casi_t.t1, na.rm = TRUE)
t.test(Con$adhd_tot_casi_t.t0, Con$adhd_tot_casi_t.t1, paired = TRUE, alternative = "two.sided")
mean(Con$adhd_tot_casi_t.t0, na.rm = TRUE)
mean(Con$adhd_tot_casi_t.t1, na.rm = TRUE)

hist(Exp$adhd_tot_casi_t.t0)
shapiro.test(Exp$adhd_tot_casi_t.t0)
hist(Exp$adhd_tot_casi_t.t1)
shapiro.test(Exp$adhd_tot_casi_t.t1)
hist(Con$adhd_tot_casi_t.t0)
shapiro.test(Con$adhd_tot_casi_t.t0)
hist(Con$adhd_tot_casi_t.t1)
shapiro.test(Con$adhd_tot_casi_t.t1)

##let's median split the Exp group
Exp<-Exp %>% mutate(GroupSTART =
                            case_when(adhd_tot_casi_t.t0 < 15.96552  ~ "1", 
                                      adhd_tot_casi_t.t0 > 15.96552  ~ "2",
                                      adhd_tot_casi_t.t0 == "NA" ~ "NA")
)

Exp <- within(Exp,{
  #p_id <- as.factor(p_id)
  session <- as.factor(session)
  group <- as.factor(GroupSTART)
})

# delete rows if lowstart
HighStartExp<-Exp[!(Exp$GroupSTART=="1"),]
LowStartExp<-Exp[!(Exp$GroupSTART=="2"),]

t.test(HighStartExp$adhd_tot_casi_t.t0, HighStartExp$adhd_tot_casi_t.t1, paired = TRUE, alternative = "two.sided")
t.test(LowStartExp$adhd_tot_casi_t.t0, LowStartExp$adhd_tot_casi_t.t1, paired = TRUE, alternative = "two.sided")
mean(HighStartExp$adhd_tot_casi_t.t0, na.rm = TRUE)
mean(HighStartExp$adhd_tot_casi_t.t1, na.rm = TRUE)
mean(LowStartExp$adhd_tot_casi_t.t0, na.rm = TRUE)
mean(LowStartExp$adhd_tot_casi_t.t1, na.rm = TRUE)

##is there a difference in starting values?
##yes there is
t.test(Con$adhd_tot_casi_t.t0, Exp$adhd_tot_casi_t.t0, paired = FALSE, alternative = "two.sided")


###CHANGE SCORES####
data.wide$changetotCASI <- data.wide$adhd_tot_casi_t.t1 - data.wide$adhd_tot_casi_t.t0
data.wide$changegort <- data.wide$meango_corrrtnr_t.t1 - data.wide$meango_corrrtnr_t.t0
data.wide$changehypCASI <- data.wide$adhd_hyp_casi_t.t1 - data.wide$adhd_hyp_casi_t.t0
data.wide$changeinCASI <- data.wide$adhd_in_casi_t.t1 - data.wide$adhd_in_casi_t.t0
data.wide$changeSSRT<-data.wide$Fssrtnr_t.t1-data.wide$Fssrtnr_t.t0
data.wide$changeinhnet<-data.wide$inhibitionnet_t.t1-data.wide$inhibitionnet_t.t0
data.wide <- within(data.wide,{
  group.t0 <- factor(group.t0)
})

# ggplot(data.wide, aes(x=changetotCASI, y=changegort, shape=group.t0)) +
#   geom_point()+
#   geom_smooth(method=lm)+theme_classic()+
# #   labs(#title="Academic Performance and Proactive Control changes",
# #     x="changes in Proactive Control", y = "changes in Academic Performance")+theme(text = element_text(size=20))
# # 
# # jpeg(file="ACAD-PROAC.jpg", res=600, width=6000, height=4800, pointsize=10,
# #      type="windows", antialias="cleartype")

Exp$changetotCASI <- Exp$adhd_tot_casi_t.t1 - Exp$adhd_tot_casi_t.t0
Exp$changegort <- Exp$meango_corrrtnr_t.t1 - Exp$meango_corrrtnr_t.t0
Exp$changehypCASI <- Exp$adhd_hyp_casi_t.t1 - Exp$adhd_hyp_casi_t.t0
Exp$changeinCASI <- Exp$adhd_in_casi_t.t1 - Exp$adhd_in_casi_t.t0
Exp$changeSSRT<-Exp$Fssrtnr_t.t1-Exp$Fssrtnr_t.t0
Exp$changeinhnet<-Exp$inhibitionnet_t.t1-Exp$inhibitionnet_t.t0

Exp$changetotCASI <- Exp$adhd_tot_casi_t.t1 - Exp$adhd_tot_casi_t.t0
Exp$changegort <- Exp$meango_corrrtnr_t.t1 - Exp$meango_corrrtnr_t.t0
Exp$changehypCASI <- Exp$adhd_hyp_casi_t.t1 - Exp$adhd_hyp_casi_t.t0
Exp$changeinCASI <- Exp$adhd_in_casi_t.t1 - Exp$adhd_in_casi_t.t0
Exp$changeSSRT<-Exp$Fssrtnr_t.t1-Exp$Fssrtnr_t.t0
Exp$changeinhnet<-Exp$inhibitionnet_t.t1-Exp$inhibitionnet_t.t0

cor.test(Exp$changetotCASI, Exp$changegort, method=c("pearson", "kendall", "spearman"))
cor.test(Exp$changehypCASI, Exp$changegort, method=c("pearson", "kendall", "spearman"))
cor.test(Exp$changeinCASI, Exp$changegort, method=c("pearson", "kendall", "spearman"))

cor.test(Exp$changetotCASI, Exp$changeSSRT, method=c("pearson", "kendall", "spearman"))
cor.test(Exp$changehypCASI, Exp$changeSSRT, method=c("pearson", "kendall", "spearman"))
cor.test(Exp$changeinCASI, Exp$changeSSRT, method=c("pearson", "kendall", "spearman"))

cor.test(Exp$changetotCASI, Exp$changeinhnet, method=c("pearson", "kendall", "spearman"))
cor.test(Exp$changehypCASI, Exp$changeinhnet, method=c("pearson", "kendall", "spearman"))
cor.test(Exp$changeinCASI, Exp$changeinhnet, method=c("pearson", "kendall", "spearman"))

cor.test(Exp$changegort, Exp$changeinhnet, method=c("pearson", "kendall", "spearman"))
cor.test(Exp$changeSSRT, Exp$changeinhnet, method=c("pearson", "kendall", "spearman"))


cor.test(Exp$changetotCASI, Exp$trainingslope.t0, method=c("pearson", "kendall", "spearman"))
cor.test(Exp$changehypCASI, Exp$trainingslope.t0, method=c("pearson", "kendall", "spearman"))
cor.test(Exp$changeinCASI, Exp$trainingslope.t0, method=c("pearson", "kendall", "spearman"))


ExpNAremoved<-Exp[complete.cases(Exp),]

pcor.test( Exp$trainingslope.t0)
pcor.test(Exp$changetotCASI, Exp$changegort, Exp$totalsessions.t0, semi = FALSE, conf.level = 0.95, nrep = 1000,
          method = c("pearson", "spearman"))

partial.r(data=Exp, x=c("changetotCASI","changegort"), y=c("trainingslope.t0", "age.t0", "totalsessions.t0"),method="pearson")

##median split by whether they improve or not on CASI 
Exp<-Exp %>% mutate(CASIimprove =
                      case_when(changetotCASI < 0  ~ "1", 
                                changetotCASI > 0  ~ "2",
                                changetotCASI == 0 ~ "2")
)

Exp<-Exp %>% mutate(goRTimprove =
                      case_when(changegort < 0  ~ "2", 
                                changegort > 0  ~ "1",
                                changegort == 0 ~ "2")
)

t.test(changegort ~ CASIimprove, data = Exp)

##in##
t.test(Exp$adhd_in_casi_t.t0, Exp$adhd_in_casi_t.t1, paired = TRUE, alternative = "two.sided")
mean(Exp$adhd_in_casi_t.t0, na.rm = TRUE)
mean(Exp$adhd_in_casi_t.t1, na.rm = TRUE)
t.test(Con$adhd_in_casi_t.t0, Con$adhd_in_casi_t.t1, paired = TRUE, alternative = "two.sided")
mean(Con$adhd_in_casi_t.t0, na.rm = TRUE)
mean(Con$adhd_in_casi_t.t1, na.rm = TRUE)

##is there a difference in starting values?
##yes there is
t.test(Con$adhd_in_casi_t.t0, Exp$adhd_in_casi_t.t0, paired = FALSE, alternative = "two.sided")

##hyp##
t.test(Exp$adhd_hyp_casi_t.t0, Exp$adhd_hyp_casi_t.t1, paired = TRUE, alternative = "two.sided")
t.test(Exp$adhd_hyp_casi_t.t2, Exp$adhd_hyp_casi_t.t1, paired = TRUE, alternative = "two.sided")
mean(Exp$adhd_hyp_casi_t.t0, na.rm = TRUE)
mean(Exp$adhd_hyp_casi_t.t1, na.rm = TRUE)
t.test(Con$adhd_hyp_casi_t.t0, Con$adhd_hyp_casi_t.t1, paired = TRUE, alternative = "two.sided")
mean(Con$adhd_hyp_casi_t.t0, na.rm = TRUE)
mean(Con$adhd_hyp_casi_t.t1, na.rm = TRUE)

##is there a difference in starting values?
##yes there is
t.test(Con$adhd_hyp_casi_t.t0, Exp$adhd_hyp_casi_t.t0, paired = FALSE, alternative = "two.sided")



models<-list()
M <- length(implist)
for (mm in 1:M){
  models[[mm]] <- lme4::lmer( formula, data=implist[[mm]], REML=FALSE)
}

res1 <- miceadds::lmer_pool(models)
summary(res1)
