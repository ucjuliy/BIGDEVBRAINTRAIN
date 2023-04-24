##Decision making script - edited by KG NEW

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
# my_theme <-   theme(
#   #legend.position = "none",
#   plot.title = element_text(family="Arial",color="black", size=18 ,hjust = 0.5,margin=margin(0,0,10,0)),
#   text = element_text(family="Arial", size=16),
#   legend.title = element_text(size = 16),
#   legend.text = element_text(size = 14),
#   axis.title.x = element_text(color="black", size=16, margin=margin(5,0,0,0)),
#   axis.text.x = element_text(size = 14, margin=margin(5,0,0,0)),
#   axis.text.y = element_text(size = 14, margin=margin(0,5,0,10))
# )
# 
# plots_folder = 'analysis/'
# reg_plt_width = 20
# reg_plt_height = 15
# 
# library(RColorBrewer)
# imp_colors <- RColorBrewer::brewer.pal(12, "Paired")[9:10]
# my_colors = c("#404080","#69b3a2")
# 

### load all data
DF <- read.csv('TrainingPaper_AllData.csv')
names(DF)
dim(DF)

# # check if these are in the same position
# names(DF)[6] <- "Online"
# names(DF)[42] <- "Perc_Delay" # tot_delay_perc
# names(DF)[48] <- "Perc_HiConf_Delay" # tot_delay_perc
# names(DF)[49] <- "Perc_LoConf_Delay" # tot_delay_perc
# names(DF)[50] <- "Perc_Conflict_Del_Diff" # hi_lo_conf_del_diff
# 
# names(DF)[53] <- "TD_AvgRT" # this was capped by 3 SD
# names(DF)[64] <- "TD_Conflict_RT_Diff" #hi_lo_conf_RT_3SD_Diff this was capped by 3 SD
# 
# names(DF)[88] <- "Corr_r_Exp_Delay" # tot_delay_perc
# names(DF)[89] <- "Corr_int_Exp_Delay" # tot_delay_perc
# names(DF)[90] <- "Corr_rsq_Exp_Delay" # tot_delay_perc
# names(DF)[91] <- "Corr_r_Exp_Reward" # tot_delay_perc
# names(DF)[92] <- "Corr_int_Exp_Reward" # tot_delay_perc
# names(DF)[93] <- "Corr_rsq_Exp_Reward" # tot_delay_perc
# 
# names(DF)[123] <- "Corsi_WM_Span" # corsi_max_wm_t
# names(DF)[126] <- "AXCPT_t" # PBI_t
# names(DF)[127] <- "CogFlex_SwitchRT_t" # cf_switchrt_t
# names(DF)[128] <- "CogFlex_t" # cogflex_t
# names(DF)[129] <- "SSRT" # REzssrtnr_t
# names(DF)[130] <- "FlankerSwitch_t" # REflankerswitch_t
# names(DF)[131] <- "FlankerInhib_t" # REflankerinh_t
# names(DF)[132] <- "Stroop_t" # stroop
# names(DF)[133] <- "OneBack_WM_t" # dprimeONEBACK_t
# names(DF)[134] <- "TwoBack_WM_t" # dprimeTWOBACK_t

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

##drop all EF tasks + IQ tasks
DF<-DF[,c(1:40)]


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

##FLAG KEERTANA/KG HAS CHANGED THIS TO THOSE WHO ARE ASSIGNED GROUP
# list the participants we want (the subset who did the training)
Train_IDs <- array(DF %>%
                     filter(!is.na(DF$Group_Name)) %>%
                     distinct(ID))

Train_IDs <- as.numeric(unlist(Train_IDs))
length(Train_IDs) # 69 max participants
Train_IDs

# extract the participants we want
# participants who (a) had at least one session worth of DM data, (b) have completed
# at least one valid session of training
DFT <- DF[DF$ID %in% Train_IDs, ]
# extract the participants we want
# participants who (a) had at least one session worth of DM data, (b) have completed
# at least one valid session of training
##FLAG HAVE REMOVED THIS - KEERTANA/KG
#DFT <- DF[DF$ID %in% Train_IDs, ]

# dim(DFT)
# length(unique(DFT$ID))
# names(DFT)

#KG - deleted anything that was time specfifc (separating out per timepoint)## 

##KG removing a few more variables to have sheet be cleaner 

## removes all non measures of interest # edited to include Date 
drops <- c("Online","Online.1","TrainingWeeks",
           "TotalSessions","TotalBonusGames","PercentageBonusDone",
           "SES","ComboEdu","ComboEmploy") # "AvgRT_3SD","hi_lo_conf_RT_Diff",

DFT <- DFT[, !(names(DFT) %in% drops)]

##### ------------- IMPUTE AGAIN IF NECESSARY -----------------------

# should be like 76 here
# novars = dim(DFT)[2]
# novars



# KG - dropping all variables that arent being used 
names(DFT)
keeps <- c("ID","Session", "Group", "Train_Coef", "UG_Offer_Accept", "tot_delay_perc", "DG_UG_Diff", "DG_Coins_Given", "Age_Frac")
data = DFT[keeps]

# ### process some vars etc
# DFT$Age_Frac_Imp <- DFT$Age_YM
# 
# # log transform some reaction times
# DFT$DG_RT_log <- log(1/DFT$DG_RT)
# DFT$UG_RT_log <- log(1/DFT$UG_RT)
# DFT$UG_Decision_RT_log <- log(1/DFT$UG_Decision_RT)
# DFT$DG_UG_RT_Diff_log <- log(1/DFT$DG_UG_RT_Diff)
# 
# DFT$TD_AvgRT_log <- log(1/DFT$TD_AvgRT)
# 
# DFT$DG_UG_RT_Diff_z <- scale(DFT$DG_UG_RT_Diff, center = TRUE, scale = TRUE)
# DFT$DG_UG_RT_Diff_z <- as.numeric(DFT$DG_UG_RT_Diff_z)

# SES score - uncomment
# first invert SES
# DFT$SES_inv = ((DFT$SES - max(DFT$SES,na.rm=T)) * -1) + min(DFT$SES,na.rm=T)
# DFT$SES_inv_z <- scale(DFT$SES_inv, center = TRUE, scale = TRUE)
# DFT$SES_inv_z <- as.numeric(DFT$SES_inv_z)

# # weighted averaging
# DFT$Reward_WeightCoef = DFT$corr_r_exp_reward*2
# DFT$Delay_WeightCoef = DFT$corr_r_exp_delay*2
# 
# DFT$Reward_WMean = as.numeric(rowMeans(DFT[,c("corr_r_exp_reward","Reward_WeightCoef")]))
# DFT$Delay_WMean = as.numeric(rowMeans(DFT[,c("corr_r_exp_delay","Delay_WeightCoef")]))
# 
# # inverse it
# DFT$Reward_WMean = DFT$Reward_WMean*-1
# 
# # scale all vars here to help with interpretation of the Bayes factor
# DFT$Perc_Delay_z <- scale(DFT$Perc_Delay, center = TRUE, scale = TRUE)
# DFT$Perc_Delay_z <- as.numeric(DFT$Perc_Delay_z)
# 
# DFT$Delay_WMean_z <- scale(DFT$Delay_WMean, center = TRUE, scale = TRUE)
# DFT$Delay_WMean_z <- as.numeric(DFT$Delay_WMean_z)
# 
# DFT$Reward_WMean_z <- scale(DFT$Reward_WMean, center = TRUE, scale = TRUE)
# DFT$Reward_WMean_z <- as.numeric(DFT$Reward_WMean_z)
# 
# DFT$Corr_r_Exp_Delay_z <- scale(DFT$corr_r_exp_delay, center = TRUE, scale = TRUE)
# DFT$Corr_r_Exp_Delay_z <- as.numeric(DFT$Corr_r_Exp_Delay_z)
# 
# DFT$Corr_int_Exp_Delay_z <- scale(DFT$corr_int_exp_delay, center = TRUE, scale = TRUE)
# DFT$Corr_int_Exp_Delay_z <- as.numeric(DFT$Corr_int_Exp_Delay_z)
# 
# DFT$Corr_r_Exp_Reward_z <- scale(DFT$corr_r_exp_reward, center = TRUE, scale = TRUE)
# DFT$Corr_r_Exp_Reward_z <- as.numeric(DFT$Corr_r_Exp_Reward_z)
# 
# DFT$Corr_int_Exp_Reward_z <- scale(DFT$corr_int_exp_reward, center = TRUE, scale = TRUE)
# DFT$Corr_int_Exp_Reward_z <- as.numeric(DFT$Corr_int_Exp_Reward_z)
# 
# # social
# DFT$DG_Coins_Given_z <- scale(DFT$DG_Coins_Given, center = TRUE, scale = TRUE)
# DFT$DG_Coins_Given_z <- as.numeric(DFT$DG_Coins_Given_z)
# 
# DFT$UG_Coins_Given_z <- scale(DFT$UG_Coins_Given, center = TRUE, scale = TRUE)
# DFT$UG_Coins_Given_z <- as.numeric(DFT$UG_Coins_Given_z)
# 
# DFT$DG_UG_Diff_z <- scale(DFT$DG_UG_Diff, center = TRUE, scale = TRUE)
# DFT$DG_UG_Diff_z <- as.numeric(DFT$DG_UG_Diff_z)

# # EFs & Others
# DFT$T_Matrix_z <- scale(DFT$T_Matrix, center = TRUE, scale = TRUE)
# DFT$T_Matrix_z <- as.numeric(DFT$T_Matrix_z)
# 
# DFT$T_Vocab_z <- scale(DFT$T_Vocab, center = TRUE, scale = TRUE)
# DFT$T_Vocab_z <- as.numeric(DFT$T_Vocab_z)
# 
# DFT$Corsi_WM_Span_z <- scale(DFT$Corsi_WM_Span, center = TRUE, scale = TRUE)
# DFT$Corsi_WM_Span_z <- as.numeric(DFT$Corsi_WM_Span_z)


# we also impute SES and age, maybe keep in school too.
## Take out measures not to be used in imputing
# took out School - now used in imputation also

# KG EDITED 

data$Group<-factor(data$Group, levels = c('1', '2'), labels = c('Exp', 'Con'))
data$Session<-factor(data$Session, levels = c('0', '1', '2'), labels = c('t0', 't1', 't2'))
measures <- c("UG_Offer_Accept", "tot_delay_perc", "DG_UG_Diff", "DG_Coins_Given")

sink("check_missing_data_percentage_DECISIONMAKING.txt")
### Whole dataset
print("Percentage missing data for whole dataset")
print(sum(is.na(data))/(nrow(data)*ncol(data)))
### Measure-wise
for(n in 1:length(measures)){
  print(paste("Percentage missing data for ",measures[[n]],sep=""))
  print(sum(is.na(eval(parse(text=paste("data","$",measures[[n]],sep="")))))/(nrow(data)))
}
sink()


missing_plot(data)
ggsave(paste("check_missing_data_map_DECISIONMAKING.png",sep=""),
       width = 7,height = 7)
# dependent <- c('Fssrt','muFssrt','sigmaFssrt','tauFssrt','muGoRT','sigmaGoRT','tauGoRT',
#                'academic_x.wide_scores',
#                'DMN','VIS','FPN','CON','MOT','AUD',
#                'IN','rIFG','rCAU','rPUT','rTHAL','rSTN','lPSMA','rCAL')
# explanatory <- c('id','age','gender','session','group','training_slope','training_sessions','pHit','SSD','academic_time','FD')
# missing_pairs(data,dependent,explanatory)

# Impute data
set.seed(123)
ini <- mice(data, maxit=0, pri=F) #get predictor matrix
pred <- ini$predictorMatrix
pred[,c("ID" )] <- 0 #don't use as predictor
meth <- ini$method
meth[c("ID","Session","Group", "Age_Frac", "Train_Coef")] <- "" #don't impute these variables, use only as predictors
imp <- mice(data, m=100, maxit=20, printFlag=TRUE, predictorMatrix=pred, method=meth) #impute data with 100 imputations and 20 iterations

# Select a single dataset for post-hoc tests
implist <- mids2mitml.list(imp) #create a list of completed data sets
data.imp <- implist[[1]]
write_csv(data.imp,"DECISIONMAKING_imputed.csv") #save imputed dataset to use for analyses
write_csv(DFT,"DECISIONMAKING_preprocessed_nonimputed.csv") #save imputed dataset to use for analyses

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
ggsave("check_imputed_data_densplot_DECISIONMAKING.png",
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
ggsave("check_imputed_data_densplot_single_DECISIONMAKING.png",
       width = 3500, height = 2000,units = c("px"))

