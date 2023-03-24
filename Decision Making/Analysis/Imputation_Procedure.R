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

plots_folder = 'analysis/'
reg_plt_width = 20
reg_plt_height = 15

library(RColorBrewer)
imp_colors <- RColorBrewer::brewer.pal(12, "Paired")[9:10]
my_colors = c("#404080","#69b3a2")


### load all data
DF <- read.csv('data/TrainingPaper_AllData.csv')
names(DF)
dim(DF)

# check if these are in the same position
names(DF)[6] <- "Online"
names(DF)[42] <- "Perc_Delay" # tot_delay_perc
names(DF)[48] <- "Perc_HiConf_Delay" # tot_delay_perc
names(DF)[49] <- "Perc_LoConf_Delay" # tot_delay_perc
names(DF)[50] <- "Perc_Conflict_Del_Diff" # hi_lo_conf_del_diff

names(DF)[53] <- "TD_AvgRT" # this was capped by 3 SD
names(DF)[64] <- "TD_Conflict_RT_Diff" #hi_lo_conf_RT_3SD_Diff this was capped by 3 SD

names(DF)[88] <- "Corr_r_Exp_Delay" # tot_delay_perc
names(DF)[89] <- "Corr_int_Exp_Delay" # tot_delay_perc
names(DF)[90] <- "Corr_rsq_Exp_Delay" # tot_delay_perc
names(DF)[91] <- "Corr_r_Exp_Reward" # tot_delay_perc
names(DF)[92] <- "Corr_int_Exp_Reward" # tot_delay_perc
names(DF)[93] <- "Corr_rsq_Exp_Reward" # tot_delay_perc

names(DF)[123] <- "Corsi_WM_Span" # corsi_max_wm_t
names(DF)[126] <- "AXCPT_t" # PBI_t
names(DF)[127] <- "CogFlex_SwitchRT_t" # cf_switchrt_t
names(DF)[128] <- "CogFlex_t" # cogflex_t
names(DF)[129] <- "SSRT" # REzssrtnr_t
names(DF)[130] <- "FlankerSwitch_t" # REflankerswitch_t
names(DF)[131] <- "FlankerInhib_t" # REflankerinh_t
names(DF)[132] <- "Stroop_t" # stroop
names(DF)[133] <- "OneBack_WM_t" # dprimeONEBACK_t
names(DF)[134] <- "TwoBack_WM_t" # dprimeTWOBACK_t

DF$ID_no <- as.integer(DF$ID)

# remove the unknown participant numbers
DF = DF[which (DF$ID_no < 266),]

# total NAs
sum(is.na(DF$Online))


#### only add these when imputing with medium(?) ######
# add some sessions in here - clean this up
DF$Online[which(DF$Session == 2)] <- 1
sum(is.na(DF$Online))

DF$Online[is.na(DF$Online) & DF$ID < 135] <- 0
sum(is.na(DF$Online))

DF$Online[is.na(DF$Online) & DF$ID > 134] <- 1
sum(is.na(DF$Online))
#####

### checl session
sum(is.na(DF$Session))
# missing session is for 232 for session 1
#DF$Session[is.na(DF$Session)] <- 1
sum(is.na(DF$Session))

## check if group missing
sum(is.na(DF$Group))

names(DF)

## removes all non measures of interest # edited to include Date 
drops <- c("ID_title","ID_no","Time","OS","UG_DataRows","DOB","DG_DataRows",
           "Train_ID","Age","Testing_Date","randtype","Score",
           "Training_Version","Online_Test_Group","DG_Coins_Kept","UG_Coins_Kept",
           "UG_Attempts","total_randb","auc_tot_val","auc_indiff",
           "auc_prob","auc_indiff_all","corr_r_pob","corr_int_pob",
           "corr_r_indiff_all","corr_int_indiff_all","corr_r_tot_val",
           "corr_int_tot_val","Exp_delay_trials","Exp_reward_trials",
           "UG_Total_RT","tot_loconf","tot_hiconf","tot_delay","AvgRT",
           "RT_STD","avg_del_prob",
           "Age_Frac_Imp","total_scores","time",
           "Age_YrsMths",'time',
           "UG_Accept","twodelays","fourdelays","sixdelays",
           "AvgimmRT","imm_del_RT_Diff","Avg_hiconfRT","Avg_loconfRT",
           "avg_indiff","Exp_delay_avg_rating",
           "Exp_reward_unique_ratings","Exp_reward_avg_rating",
           "Exp_delay_unique_ratings","Exp_reward_avg_rating",
           "Online_y","Date_y","AvgdelayRT","Exp_reward_auc","Exp_delay_auc",
           "Raw_Vocab","Raw_Matrix",
           # no model-based stuff
           "it_P6","lr_P6","eg_P6","st_P6","repst_P6","it","lr","eg","st",
           "repst","w_lo","w_hi","w_diff","w_P6",
           "maths_scores","english_scores","total_scores",
           "Perc_Rank","FSIQ.2","T_ScoreSum",
           "trainingslope","totalsessions",
           "Perc_HiConf_Delay","Perc_LoConf_Delay","Perc_Conflict_Del_Diff",
           "hi_lo_conf_RT_3SD_Diff","Corr_rsq_Exp_Delay","Corr_rsq_Exp_Reward",
           "Avg_hiconfRT_3SD","Avg_loconfRT_3SD","TD_Conflict_RT_Diff") # "AvgRT_3SD","hi_lo_conf_RT_Diff",

DF <- DF[, !(names(DF) %in% drops)]
names(DF)

# check how many pps in each phase
DF_T0 <- DF[which (DF$Session == 0),]
DF_T0_all <- DF_T0[which (!is.na(DF_T0$DG_Coins_Given)),]
nrow(DF_T0_all)

DF_T1 <- DF[which (DF$Session == 1),]
DF_T1_all <- DF_T1[which (!is.na(DF_T1$DG_Coins_Given)),]
nrow(DF_T1_all)

DF_T2 <- DF[which (DF$Session == 2),]
DF_T2_all <- DF_T2[which (!is.na(DF_T2$DG_Coins_Given)),]
nrow(DF_T2_all)

length(unique(DF$ID))

# list the participants we want (the subset who did the training)
Train_IDs <- array(DF %>%
                     filter(!is.na(DF$TotalSessions)) %>%
                     distinct(ID))

Train_IDs <- as.numeric(unlist(Train_IDs))
length(Train_IDs) # 69 max participants
Train_IDs

# extract the participants we want
# participants who (a) had at least one session worth of DM data, (b) have completed
# at least one valid session of training
DFT <- DF[DF$ID %in% Train_IDs, ]

dim(DFT)
length(unique(DFT$ID))
names(DFT)

DFT_T0 <- DFT[which (DFT$Session == 0),]

nrow(DFT_T0)

# check participants here
DF_T0 <- DFT[which (DFT$Session == 0),]
DF_T0_all <- DF_T0[which (!is.na(DF_T0$DG_Coins_Given)),]
nrow(DF_T0_all)

DF_T1 <- DFT[which (DFT$Session == 1),]
DF_T1_all <- DF_T1[which (!is.na(DF_T1$DG_Coins_Given)),]
nrow(DF_T1_all)

DF_T2 <- DFT[which (DFT$Session == 2),]
DF_T2_all <- DF_T2[which (!is.na(DF_T2$DG_Coins_Given)),]
nrow(DF_T2_all)

length(unique(DF$ID))

# display T0 demographics
table(DFT_T0['Gender'])
table(DFT_T0['School'])
table(DFT_T0['Online'])
table(DFT_T0['SES'])

min(DFT_T0$Age_Frac, na.rm=TRUE)
max(DFT_T0$Age_Frac, na.rm=TRUE)
mean(DFT_T0$Age_Frac, na.rm=TRUE)
sd(DFT_T0$Age_Frac, na.rm=TRUE)

# cannot be imputed across all timepoints:
# - MB measures
# - academic achievement

excl <- c("ID","Session","Date","Online","TrainingWeeks","TotalSessions","TotalBonusGames",
          "PercentageBonusDone","Group_Name","Training_Start","randtype","Training_End",
          "Gender","Group","Age_YM","ComboEmploy","ComboEdu","SES","School",
          "imm_del_RT_3SD_Diff","Train_Coef","Age_Frac","FlankerSwitch_t",
          "FlankerInhib_t","OneBack_WM_t","Stroop_t","TwoBack_WM_t","CogFlex_t",
          "CogFlex_SwitchRT_t","T_Matrix","Corsi_WM_Span","SSRT","AXCPT_t",
          "T_Vocab","AvgimmRT_3SD","AvgdelayRT_3SD","Age_Frac_Imp","SES_inv_z")

DFT_T0_plt <- DFT_T0[, !(names(DFT_T0) %in% excl)]
names(DFT_T0_plt)

nrow(DFT_T0_plt)

aggr_plot<-aggr(DFT_T0_plt,col=c('navyblue','red'),numbers=T, sortVars=T,
                labels=T,cex.axis=0.99,oma=c(10,5,5,3),title(main="Miss"))
title(main="Missingness Plot Pre-training")


#### visualise missing data
DFT_T1 <- DFT[which (DFT$Session == 1),]
DFT_T1_plt <- DFT_T1[, !(names(DFT_T1) %in% excl)]
names(DFT_T1_plt)

nrow(DFT_T1_plt)

aggr_plot<-aggr(DFT_T1_plt,col=c('navyblue','red'),numbers=T, sortVars=T,
                labels=T,cex.axis=0.99,oma=c(10,5,5,3),title(main="Miss"))
title(main="Missingness Plot Post-training")


#### visualise missing data
DFT_T2 <- DFT[which (DFT$Session == 2),]
DFT_T2_plt <- DFT_T2[, !(names(DFT_T2) %in% excl)]
names(DFT_T2_plt)

nrow(DFT_T2_plt)

aggr_plot<-aggr(DFT_T2_plt,col=c('navyblue','red'),numbers=T, sortVars=T,
                labels=T,cex.axis=0.99,oma=c(10,5,5,3),title(main="Miss"))
title(main="Missingness Plot Follow-up")

##### ------------- IMPUTE AGAIN IF NECESSARY -----------------------
# For this chapter, we impute everyone who has data. but for Chapter 4, we need 
# to impute only the participants who have completed the training, I think. Yes, 
# because else we don't know what group they're in. So we need different data for 
# Chapter 3 and 4

# should be like 76 here
novars = dim(DFT)[2]
novars

names(DFT)

### process some vars etc
DFT$Age_Frac_Imp <- DFT$Age_Frac

# log transform some reaction times
DFT$DG_RT_log <- log(1/DFT$DG_RT)
DFT$UG_RT_log <- log(1/DFT$UG_RT)
DFT$UG_Decision_RT_log <- log(1/DFT$UG_Decision_RT)
DFT$DG_UG_RT_Diff_log <- log(1/DFT$DG_UG_RT_Diff)

DFT$TD_AvgRT_log <- log(1/DFT$TD_AvgRT)

DFT$DG_UG_RT_Diff_z <- scale(DFT$DG_UG_RT_Diff, center = TRUE, scale = TRUE)
DFT$DG_UG_RT_Diff_z <- as.numeric(DFT$DG_UG_RT_Diff_z)

# SES score
# first invert SES
DFT$SES_inv = ((DFT$SES - max(DFT$SES,na.rm=T)) * -1) + min(DFT$SES,na.rm=T)
DFT$SES_inv_z <- scale(DFT$SES_inv, center = TRUE, scale = TRUE)
DFT$SES_inv_z <- as.numeric(DFT$SES_inv_z)

# weighted averaging
DFT$Reward_WeightCoef = DFT$Corr_r_Exp_Reward*2
DFT$Delay_WeightCoef = DFT$Corr_r_Exp_Delay*2

DFT$Reward_WMean = as.numeric(rowMeans(DFT[,c("Corr_int_Exp_Reward","Reward_WeightCoef")]))
DFT$Delay_WMean = as.numeric(rowMeans(DFT[,c("Corr_int_Exp_Delay","Delay_WeightCoef")]))

# inverse it
DFT$Reward_WMean = DFT$Reward_WMean*-1

# scale all vars here to help with interpretation of the Bayes factor
DFT$Perc_Delay_z <- scale(DFT$Perc_Delay, center = TRUE, scale = TRUE)
DFT$Perc_Delay_z <- as.numeric(DFT$Perc_Delay_z)

DFT$Delay_WMean_z <- scale(DFT$Delay_WMean, center = TRUE, scale = TRUE)
DFT$Delay_WMean_z <- as.numeric(DFT$Delay_WMean_z)

DFT$Reward_WMean_z <- scale(DFT$Reward_WMean, center = TRUE, scale = TRUE)
DFT$Reward_WMean_z <- as.numeric(DFT$Reward_WMean_z)

DFT$Corr_r_Exp_Delay_z <- scale(DFT$Corr_r_Exp_Delay, center = TRUE, scale = TRUE)
DFT$Corr_r_Exp_Delay_z <- as.numeric(DFT$Corr_r_Exp_Delay_z)

DFT$Corr_int_Exp_Delay_z <- scale(DFT$Corr_int_Exp_Delay, center = TRUE, scale = TRUE)
DFT$Corr_int_Exp_Delay_z <- as.numeric(DFT$Corr_int_Exp_Delay_z)

DFT$Corr_r_Exp_Reward_z <- scale(DFT$Corr_r_Exp_Reward, center = TRUE, scale = TRUE)
DFT$Corr_r_Exp_Reward_z <- as.numeric(DFT$Corr_r_Exp_Reward_z)

DFT$Corr_int_Exp_Reward_z <- scale(DFT$Corr_int_Exp_Reward, center = TRUE, scale = TRUE)
DFT$Corr_int_Exp_Reward_z <- as.numeric(DFT$Corr_int_Exp_Reward_z)

# social
DFT$DG_Coins_Given_z <- scale(DFT$DG_Coins_Given, center = TRUE, scale = TRUE)
DFT$DG_Coins_Given_z <- as.numeric(DFT$DG_Coins_Given_z)

DFT$UG_Coins_Given_z <- scale(DFT$UG_Coins_Given, center = TRUE, scale = TRUE)
DFT$UG_Coins_Given_z <- as.numeric(DFT$UG_Coins_Given_z)

DFT$DG_UG_Diff_z <- scale(DFT$DG_UG_Diff, center = TRUE, scale = TRUE)
DFT$DG_UG_Diff_z <- as.numeric(DFT$DG_UG_Diff_z)

# EFs & Others
DFT$T_Matrix_z <- scale(DFT$T_Matrix, center = TRUE, scale = TRUE)
DFT$T_Matrix_z <- as.numeric(DFT$T_Matrix_z)

DFT$T_Vocab_z <- scale(DFT$T_Vocab, center = TRUE, scale = TRUE)
DFT$T_Vocab_z <- as.numeric(DFT$T_Vocab_z)

DFT$Corsi_WM_Span_z <- scale(DFT$Corsi_WM_Span, center = TRUE, scale = TRUE)
DFT$Corsi_WM_Span_z <- as.numeric(DFT$Corsi_WM_Span_z)


# we also impute SES and age, maybe keep in school too.
## Take out measures not to be used in imputing
# took out School - now used in imputation also
Demdrops <- c("ID","Date","Training_Start","Training_End","Gender","Group",
              "Group_Name","Testing_Sessions","SES_inv",
              "Session","Age_Frac","Age_YrsMths","TrainingWeeks","TotalSessions",
              "TotalBonusGames","PercentageBonusDone","Online","Age_YM") 

Impvars <- c("ID","School","Session","SES_inv_z","Train_Coef","Age_Frac_Imp","Online","Group",
             "DG_Coins_Given","UG_Offer_Accept","UG_Coins_Given",
             "DG_UG_Diff","DG_UG_RT_Diff_z","DG_RT_log","UG_RT_log","UG_Decision_RT_log",
             "Perc_Delay","TD_AvgRT_log","Corr_r_Exp_Delay","Corr_int_Exp_Delay",
             "Corr_r_Exp_Reward","Corr_int_Exp_Reward","Reward_WMean","Delay_WMean",
             "T_Vocab","T_Matrix","Corsi_WM_Span","AXCPT_t","CogFlex_t","SSRT",
             "FlankerSwitch_t","FlankerInhib_t","Stroop_t","OneBack_WM_t",
             "TwoBack_WM_t") 

Impvars_std <- c("ID","School","Session","SES_inv_z","Train_Coef","Age_Frac_Imp","Online","Group",
                 "DG_Coins_Given_z","UG_Offer_Accept","UG_Coins_Given_z",
                 "DG_UG_Diff_z","DG_UG_RT_Diff_z","DG_RT_log","UG_RT_log","UG_Decision_RT_log",
                 "Perc_Delay_z","TD_AvgRT_log","Corr_r_Exp_Delay_z","Corr_int_Exp_Delay_z",
                 "Corr_r_Exp_Reward_z","Corr_int_Exp_Reward_z","Reward_WMean_z","Delay_WMean_z",
                 "T_Vocab_z","T_Matrix_z","Corsi_WM_Span_z","AXCPT_t","CogFlex_t","SSRT",
                 "FlankerSwitch_t","FlankerInhib_t","Stroop_t","OneBack_WM_t",
                 "TwoBack_WM_t") 

# so training coefficient and online included in the imputation
testdata <- DFT[, (names(DFT) %in% Impvars)]
testdata <- DFT[, (names(DFT) %in% Impvars_std)]
names(testdata)
length(unique(testdata$ID))


#### visualise missing data
aggr_plot<-aggr(testdata,col=c('navyblue','red'),numbers=T, sortVars=T,
                labels=T,cex.axis=0.8,oma=c(10,5,5,3))

library(purrr)
library(tidyr)


testdata %>% 
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = 'free') +
  #geom_histogram()
  geom_density()

names(testdata)

# quick predictor?
inlist <- c("ID","Session","Group","Age_Frac_Imp","Online",
            "DG_Coins_Given","UG_Coins_Given","UG_Offer_Accept",
            "DG_UG_Diff","Perc_Delay","Corr_r_Exp_Delay","Corr_int_Exp_Delay",
            "Corr_r_Exp_Reward","Corr_int_Exp_Reward")

# quick predictor_std
inlist_std <- c("ID","Session","Group","Age_Frac_Imp","Online",
                "DG_Coins_Given_z","UG_Coins_Given_z","UG_Offer_Accept",
                "DG_UG_Diff_z","Perc_Delay_z","Corr_r_Exp_Delay_z","Corr_int_Exp_Delay_z",
                "Corr_r_Exp_Reward_z","Corr_int_Exp_Reward_z")

pred <- quickpred(testdata, minpuc=0.5,include=inlist)
pred_std <- quickpred(testdata, minpuc=0.5,include=inlist_std)
table(rowSums(pred))
table(rowSums(pred_std))

# actually impute
tempData <- mice(testdata, m = 50, maxit = 50, meth = 'pmm', seed = 500,
                 pred = pred, exclude = "ID")

# actually impute with std data
tempData <- mice(testdata, m = 50, maxit = 50, meth = 'pmm', seed = 500,
                 pred_std = pred, exclude = "ID")
summary(tempData)

# add the columns back in
extra_cols <- DFT[, (names(DFT) %in% Demdrops)]
#extra_cols <- extra_cols[, !(names(extra_cols) %in% "School")]
names(extra_cols)

# which one is the best tempdata? 1 or another?
TD_imp <- cbind(extra_cols,complete(tempData,2))
names(TD_imp)

## save imputed output
write.csv(TD_imp,"data/Train_Imp.csv",row.names=FALSE)
save(tempData,file="data/Train_Imp.RData")

###############################################################################################################
# --- do this for EFs too....
###############################################################################################################

EFs <- read.csv('data/NewThreeFactor_t0.csv')
names(EFs)
names(EFs)[4] <- "ID"
EFs$Session <- 0

TD_EFs <- left_join(DFT,EFs)
names(TD_EFs)

# we also impute SES and age, maybe keep in school too.
## Take out measures not to be used in imputing
# took out School - now used in imputation also
Demdrops <- c("ID","Date","Training_Start","Training_End","Gender","Group",
              "Group_Name","Testing_Sessions","SES_inv",
              "Session","Age_Frac","Age_YrsMths","TrainingWeeks","TotalSessions",
              "TotalBonusGames","PercentageBonusDone","Online","Age_YM") 

Impvars <- c("ID","School","Session","SES_inv_z","Train_Coef","Age_Frac_Imp","Online","Group",
             "DG_Coins_Given","UG_Offer_Accept","UG_Coins_Given",
             "DG_UG_Diff","DG_UG_RT_Diff_z","DG_RT_log","UG_RT_log","UG_Decision_RT_log",
             "Perc_Delay","TD_AvgRT_log","Corr_r_Exp_Delay","Corr_int_Exp_Delay",
             "Corr_r_Exp_Reward","Corr_int_Exp_Reward","Reward_WMean","Delay_WMean",
             "T_Vocab","T_Matrix","Corsi_WM_Span","AXCPT_t","CogFlex_t","SSRT",
             "FlankerSwitch_t","FlankerInhib_t","Stroop_t","OneBack_WM_t",
             "TwoBack_WM_t","I","S","M") 

Impvars_std <- c("ID","School","Session","SES_inv_z","Train_Coef","Age_Frac_Imp","Online","Group",
                 "DG_Coins_Given_z","UG_Offer_Accept","UG_Coins_Given_z",
                 "DG_UG_Diff_z","DG_UG_RT_Diff_z","DG_RT_log","UG_RT_log","UG_Decision_RT_log",
                 "Perc_Delay_z","TD_AvgRT_log","Corr_r_Exp_Delay_z","Corr_int_Exp_Delay_z",
                 "Corr_r_Exp_Reward_z","Corr_int_Exp_Reward_z","Reward_WMean_z","Delay_WMean_z",
                 "T_Vocab_z","T_Matrix_z","Corsi_WM_Span_z","AXCPT_t","CogFlex_t","SSRT",
                 "FlankerSwitch_t","FlankerInhib_t","Stroop_t","OneBack_WM_t",
                 "TwoBack_WM_t","I","S","M") 

# so training coefficient and online included in the imputation
testdata <- TD_EFs[, (names(TD_EFs) %in% Impvars)]
testdata <- TD_EFs[, (names(TD_EFs) %in% Impvars_std)]
names(testdata)
length(unique(testdata$ID))



# visualise missing data
aggr_plot<-aggr(testdata,col=c('navyblue','red'),numbers=T, sortVars=T,
                labels=T,cex.axis=0.8,oma=c(10,5,5,3))

library(purrr)
library(tidyr)

testdata %>% 
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = 'free') +
  #geom_histogram()
  geom_density()

names(testdata)

# quick predictor?
inlist <- c("ID","Session","Group","Age_Frac_Imp","Online",
            "DG_Coins_Given","UG_Coins_Given","UG_Offer_Accept",
            "DG_UG_Diff","Perc_Delay","Corr_r_Exp_Delay","Corr_int_Exp_Delay",
            "Corr_r_Exp_Reward","Corr_int_Exp_Reward","I","S","M")

# quick predictor_std
inlist_std <- c("ID","Session","Group","Age_Frac_Imp","Online",
                "DG_Coins_Given_z","UG_Coins_Given_z","UG_Offer_Accept",
                "DG_UG_Diff_z","Perc_Delay_z","Corr_r_Exp_Delay_z","Corr_int_Exp_Delay_z",
                "Corr_r_Exp_Reward_z","Corr_int_Exp_Reward_z")

pred <- quickpred(testdata, minpuc=0.5,include=inlist)
table(rowSums(pred))

pred_std <- quickpred(testdata, minpuc=0.5,include=inlist_std)
table(rowSums(pred))

# actually impute
tempDataEF <- mice(testdata, m = 50, maxit = 50, meth = 'pmm', seed = 500,
                   pred = pred, exclude = "ID")
summary(tempDataEF)

# actually impute - std
tempDataEF <- mice(testdata, m = 50, maxit = 50, meth = 'pmm', seed = 500,
                   pred = pred_std, exclude = "ID")
summary(tempDataEF)

# add the columns back in
extra_cols <- TD_EFs[, (names(TD_EFs) %in% Demdrops)]
#extra_cols <- extra_cols[, !(names(extra_cols) %in% "School")]
names(extra_cols)

# which one is the best tempdata? 1 or another?
TD_EFs_imp <- cbind(extra_cols,complete(tempDataEF,2))
names(TD_EFs_imp)

## save the imputed data with the EF factors
write.csv(TD_EFs_imp,"data/Train_EF_Imp.csv",row.names=FALSE)
save(tempDataEF,file="data/Train_EF_Imp.RData")

