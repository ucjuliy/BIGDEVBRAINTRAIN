#test-retest reliability analyses: tasks#
setwd("~/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/training study/reliability test/task variables T0T1")

library(tidyverse)
library(rstatix)
library(reshape)
library(dplyr)
library(ggpubr)
library(plyr)
library(datarium)
library(optimx)
library(afex)
library(DHARMa)
library(haven)
library(psych)
library(irr)

#fMRI
fMRI_T0 <- read.csv("fMRI_T0.csv")
fMRI_T1 <- read.csv("fMRI_T1.csv")

fMRI_T0_outliers <- boxplot(fMRI_T0$HavardOxford_Cortl, plot=FALSE)$out
fMRI_T0 <- fMRI_T0[-which(fMRI_T0$HavardOxford_Cortl %in% fMRI_T0_outliers),]

fMRI_T1_outliers <- boxplot(fMRI_T1$HavardOxford_Cortl, plot=FALSE)$out
fMRI_T1 <- fMRI_T1[-which(fMRI_T1$HavardOxford_Cortl %in% fMRI_T1_outliers),]

fMRI_T0T1 <- merge(fMRI_T0, fMRI_T1, all.x = TRUE)
fMRI_T0T1 <- fMRI_T0T1 %>%
  dplyr::select(-c(1))
ICC(fMRI_T0T1)



fMRI <- read.csv("fMRI.csv")
fMRI <- reshape(fMRI, idvar = "id", v.name = c("HavardOxford_Cortl"),
                timevar= "timepoint", direction = "wide")

fMRI_T0 <- fMRI %>%
  dplyr::select(c("id", "HavardOxford_Cortl.0"))
fMRI_T0_outliers <- boxplot(fMRI_T0$HavardOxford_Cortl.0, plot=FALSE)$out
fMRI_T0 <- fMRI_T0[-which(fMRI$HavardOxford_Cortl.0 %in% fMRI_T0_outliers),]
fMRI_T0 <- na.omit(fMRI_T0)

fMRI_T1 <- fMRI %>%
  dplyr::select(c("id", "HavardOxford_Cortl.1"))
fMRI_T1_outliers <- boxplot(fMRI_T1$HavardOxford_Cortl.1, plot=FALSE)$out
fMRI_T1 <- fMRI_T1[-which(fMRI_T1$HavardOxford_Cortl.1 %in% fMRI_T1_outliers),]
fMRI_T1 <- na.omit(fMRI_T1)


fMRI_T0T1 <- merge(fMRI_T0, fMRI_T1, all.x = TRUE)
fMRI_T0T1 <- fMRI_T0T1 %>%
  dplyr::select(-c(1))
ICC(fMRI_T0T1)




#stroop task
stroop_T0 <- read.csv("stroop_T0.csv")
stroop_T1 <- read.csv("stroop_T1.csv")
stroop_T0_accuracy <- stroop_T0 %>%
  dplyr::select(c("id", "trial.p", "accuracy"))
stroop_T0_accuracy$accuracy <- as.numeric(stroop_T0_accuracy$accuracy)
stroop_T0_accuracy_wide <- reshape(stroop_T0_accuracy, idvar= "id", v.names= c("accuracy"),
                      timevar= "trial.p", direction = "wide")
stroop_T0_accuracy_wide$accuracy <- rowMeans(stroop_T0_accuracy_wide[,2:73], na.rm = TRUE)
stroop_T0_accuracy_mean <- stroop_T0_accuracy_wide %>%
  dplyr::select(c("id", "accuracy"))
stroop_T1_accuracy <- stroop_T1 %>%
  dplyr::select(c("id", "trial.p", "accuracy"))
stroop_T1_accuracy$accuracy <- as.numeric(stroop_T1_accuracy$accuracy)
stroop_T1_accuracy_wide <- reshape(stroop_T1_accuracy, idvar= "id", v.names= c("accuracy"),
                                   timevar= "trial.p", direction = "wide")
stroop_T1_accuracy_wide$accuracy <- rowMeans(stroop_T1_accuracy_wide[,2:73], na.rm = TRUE)
stroop_T1_accuracy_mean <- stroop_T1_accuracy_wide %>%
  dplyr::select(c("id", "accuracy"))

stroop_accuracy_T0T1 <- merge(stroop_T0_accuracy_mean, stroop_T1_accuracy_mean, by = c("id"))
stroop_accuracy_T0T1 <- stroop_accuracy_T0T1 %>%
  dplyr::select(-c(1))
ICC(stroop_accuracy_T0T1)

stroop_T0_RT <- stroop_T0 %>%
  dplyr::select(c("id", "trial.p", "accuracy", "RT"))
stroop_T0_RT <- stroop_T0_RT[stroop_T0_RT$accuracy %in% c("1"),]
stroop_T0_RT <- stroop_T0_RT %>%
  dplyr::select(c("id", "trial.p", "RT"))
stroop_T0_RT_wide <- reshape(stroop_T0_RT, idvar= "id", v.names= c("RT"),
                                   timevar= "trial.p", direction = "wide")
stroop_T0_RT_wide$RT <- rowMeans(stroop_T0_RT_wide[,2:73], na.rm = TRUE)
stroop_T0_RT_mean <- stroop_T0_RT_wide %>%
  dplyr::select(c("id", "RT"))
stroop_T1_RT <- stroop_T1 %>%
  dplyr::select(c("id", "trial.p", "accuracy", "RT"))
stroop_T1_RT <- stroop_T1_RT[stroop_T1_RT$accuracy %in% c("1"),]
stroop_T1_RT <- stroop_T1_RT %>%
  dplyr::select(c("id", "trial.p", "RT"))
stroop_T1_RT_wide <- reshape(stroop_T1_RT, idvar= "id", v.names= c("RT"),
                             timevar= "trial.p", direction = "wide")
stroop_T1_RT_wide$RT <- rowMeans(stroop_T1_RT_wide[,2:73], na.rm = TRUE)
stroop_T1_RT_mean <- stroop_T1_RT_wide %>%
  dplyr::select(c("id", "RT"))
stroop_RT_T0T1 <- merge(stroop_T0_RT_mean, stroop_T1_RT_mean, by = c("id"))
stroop_RT_T0T1 <- stroop_RT_T0T1 %>%
  dplyr::select(-c(1))
ICC(stroop_RT_T0T1)





#axcpt
AXCPT_T0 <- read.csv("AXCPT_T0.csv")
AXCPT_T1 <- read.csv("AXCPT_T1.csv")
AXCPT_summary_T0T1 <- read.csv("AXCPT_T0T1.csv")

AXCTP_accuracy <- AXCPT_summary_T0T1 %>%
  dplyr::select(c("accuracy_T0", "accuracy_T1"))
ICC(AXCTP_accuracy)

AXCPT_T0_RT <- AXCPT_T0 %>%
  dplyr::select(c("id", "trial.p", "accuracy", "RT"))
AXCPT_T0_RT <- AXCPT_T0_RT[AXCPT_T0_RT$accuracy %in% c("1"),]
AXCPT_T0_RT <- AXCPT_T0_RT %>%
  dplyr::select(c("id", "trial.p", "RT"))
AXCPT_T0_RT_wide <- reshape(AXCPT_T0_RT, idvar= "id", v.names= c("RT"),
                             timevar= "trial.p", direction = "wide")
AXCPT_T0_RT_wide$RT <- rowMeans(AXCPT_T0_RT_wide[,2:73], na.rm = TRUE)
AXCPT_T0_RT_mean <- AXCPT_T0_RT_wide %>%
  dplyr::select(c("id", "RT"))
AXCPT_T1_RT <- AXCPT_T1 %>%
  dplyr::select(c("id", "trial.p", "accuracy", "RT"))
AXCPT_T1_RT <- AXCPT_T1_RT[AXCPT_T1_RT$accuracy %in% c("1"),]
AXCPT_T1_RT <- AXCPT_T1_RT %>%
  dplyr::select(c("id", "trial.p", "RT"))
AXCPT_T1_RT_wide <- reshape(AXCPT_T1_RT, idvar= "id", v.names= c("RT"),
                             timevar= "trial.p", direction = "wide")
AXCPT_T1_RT_wide$RT <- rowMeans(AXCPT_T1_RT_wide[,2:73], na.rm = TRUE)
AXCPT_T1_RT_mean <- AXCPT_T1_RT_wide %>%
  dplyr::select(c("id", "RT"))
AXCPT_RT_T0T1 <- merge(AXCPT_T0_RT_mean, AXCPT_T1_RT_mean, by = c("id"))
AXCPT_RT_T0T1 <- AXCPT_RT_T0T1 %>%
  dplyr::select(-c(1))
ICC(AXCPT_RT_T0T1)



#flanker_inhibition_T0 <- read.csv("flanker_inhibition_summary_T0.csv")
#flanker_inhibition_T1 <- read.csv("flanker_inhibition_summary_T1.csv")
#flanker_inhibition_T0T1 <- merge(flanker_inhibition_T0, flanker_inhibition_T1, by = c("ID", "Compatibility"))
#names(flanker_inhibition_T0T1)[names(flanker_inhibition_T0T1) == "Accuracy.x"] <- "accuracy_T0"
#names(flanker_inhibition_T0T1)[names(flanker_inhibition_T0T1) == "Accuracy.y"] <- "accuracy_T1"
#names(flanker_inhibition_T0T1)[names(flanker_inhibition_T0T1) == "Avg.RT.x"] <- "AvgRT_T0"
#names(flanker_inhibition_T0T1)[names(flanker_inhibition_T0T1) == "Avg.RT.y"] <- "AvgRT_T1"
#write.csv(flanker_inhibition_T0T1, "flanker_inhibition_T0T1.csv", row.names = F)


#flanker_shifting_T0 <- read.csv("flanker_shifting_summary_T0.csv")
#flanker_shifting_T1 <- read.csv("flanker_shifting_summary_T1.csv")
#flanker_shifting_T0T1 <- merge(flanker_shifting_T0, flanker_shifting_T1, by = c("ID", "Compatibility"))
#names(flanker_shifting_T0T1)[names(flanker_shifting_T0T1) == "Accuracy.x"] <- "accuracy_T0"
#names(flanker_shifting_T0T1)[names(flanker_shifting_T0T1) == "Accuracy.y"] <- "accuracy_T1"
#names(flanker_shifting_T0T1)[names(flanker_shifting_T0T1) == "Avg.RT.x"] <- "AvgRT_T0"
#names(flanker_shifting_T0T1)[names(flanker_shifting_T0T1) == "Avg.RT.y"] <- "AvgRT_T1"
#write.csv(flanker_shifting_T0T1, "flanker_shifting_T0T1.csv", row.names = F)

flanker_inhibition_T0T1_accuracy <- flanker_inhibition_T0T1 %>%
  select(c("accuracy_T0", "accuracy_T1"))
ICC(flanker_inhibition_T0T1_accuracy)
flanker_inhibition_T0T1_Avg.RT <- flanker_inhibition_T0T1 %>%
  select(c("AvgRT_T0", "AvgRT_T1"))
ICC(flanker_inhibition_T0T1_Avg.RT)

flanker_shifting_T0T1_accuracy <- flanker_shifting_T0T1 %>%
  select(c("accuracy_T0", "accuracy_T1"))
ICC(flanker_shifting_T0T1_accuracy)
flanker_shifting_T0T1_Avg.RT <- flanker_shifting_T0T1 %>%
  select(c("AvgRT_T0", "AvgRT_T1"))
ICC(flanker_shifting_T0T1_Avg.RT)




#pstop 
pstop_T0 <- read.csv("pstop_T0.csv")
pstop_T1 <- read.csv("pstop_T1.csv")
pstop_T2 <- read.csv("pstop_T2.csv")


pstop_T0_accuracy <- pstop_T0 %>%
  dplyr::select(c("id", "trial.p", "accuracy"))
pstop_T0_accuracy_wide <- reshape(pstop_T0_accuracy, idvar= "id", v.names= c("accuracy"),
                            timevar= "trial.p", direction = "wide")
pstop_T0_accuracy_wide$accuracy <- rowMeans(pstop_T0_accuracy_wide[,2:29], na.rm = TRUE)
pstop_mean_T0 <- pstop_T0_accuracy_wide %>%
  dplyr::select(c("id", "accuracy"))
pstop_T1_accuracy <- pstop_T1 %>%
  dplyr::select(c("id", "trial.p", "accuracy"))
pstop_T1_accuracy_wide <- reshape(pstop_T1_accuracy, idvar= "id", v.names= c("accuracy"),
                                  timevar= "trial.p", direction = "wide")
pstop_T1_accuracy_wide$accuracy <- rowMeans(pstop_T1_accuracy_wide[,2:29], na.rm = TRUE)
pstop_mean_T1 <- pstop_T1_accuracy_wide %>%
  dplyr::select(c("id", "accuracy"))
pstop_T2_accuracy <- pstop_T2 %>%
  dplyr::select(c("id", "trial.p", "accuracy"))
pstop_T2_accuracy_wide <- reshape(pstop_T2_accuracy, idvar= "id", v.names= c("accuracy"),
                                  timevar= "trial.p", direction = "wide")
pstop_T2_accuracy_wide$accuracy <- rowMeans(pstop_T2_accuracy_wide[,2:21], na.rm = TRUE)
pstop_mean_T2 <- pstop_T2_accuracy_wide %>%
  dplyr::select(c("id", "accuracy"))


pstop_T0_outliers <- boxplot(pstop_mean_T0$accuracy, plot=FALSE)$out
pstop_mean_T0 <- pstop_mean_T0[-which(pstop_mean_T0$accuracy %in% pstop_T0_outliers),]
pstop_T1_outliers <- boxplot(pstop_mean_T1$accuracy, plot=FALSE)$out
pstop_mean_T1 <- pstop_mean_T1[-which(pstop_mean_T1$accuracy %in% pstop_T1_outliers),]
pstop_T2_outliers <- boxplot(pstop_mean_T2$accuracy, plot=FALSE)$out
pstop_mean_T2 <- pstop_mean_T2[-which(pstop_mean_T2$accuracy %in% pstop_T2_outliers),]



pstop_T0T1 <- merge(pstop_mean_T0, pstop_mean_T1, by = c("id"))
pstop_T0T1T2_1 <- merge(pstop_T0T1, pstop_mean_T2, by = c("id"))
write.csv(pstop_T0T1T2_1, "pstop_T0T1T2.csv",row.names = F)

pstop_T0T1T2_1 <- pstop_T0T1T2_1 %>%
  dplyr::select(-c(1))
ICC(pstop_T0T1T2_1)



#goRT
go_rt_T0 <- read.csv("Go_RT_T0.csv")
go_rt_T1 <- read.csv("Go_RT_T1.csv")
go_rt_T2 <- read.csv("Go_RT_T2.csv")



GoRT_T0_RT <- go_rt_T0 %>%
  dplyr::select(c("id", "trial.p", "RT"))
GoRT_T0_RT_wide <- reshape(GoRT_T0_RT, idvar= "id", v.names= c("RT"),
                            timevar= "trial.p", direction = "wide")
GoRT_T0_RT_wide$RT <- rowMeans(GoRT_T0_RT_wide[,2:85], na.rm = TRUE)
GoRT_T0_RT_mean <- GoRT_T0_RT_wide %>%
  dplyr::select(c("id", "RT"))

GoRT_T1_RT <- go_rt_T1 %>%
  dplyr::select(c("id", "trial.p", "RT"))
GoRT_T1_RT_wide <- reshape(GoRT_T1_RT, idvar= "id", v.names= c("RT"),
                            timevar= "trial.p", direction = "wide")
GoRT_T1_RT_wide$RT <- rowMeans(GoRT_T1_RT_wide[,2:85], na.rm = TRUE)
GoRT_T1_RT_mean <- GoRT_T1_RT_wide %>%
  dplyr::select(c("id", "RT"))

GoRT_T2_RT <- go_rt_T2 %>%
  dplyr::select(c("id", "trial.p", "RT"))
GoRT_T2_RT_wide <- reshape(GoRT_T2_RT, idvar= "id", v.names= c("RT"),
                           timevar= "trial.p", direction = "wide")
GoRT_T2_RT_wide$RT <- rowMeans(GoRT_T2_RT_wide[,2:61], na.rm = TRUE)
GoRT_T2_RT_mean <- GoRT_T1_RT_wide %>%
  dplyr::select(c("id", "RT"))

GoRT_T0T1 <- merge(GoRT_T0_RT_mean, GoRT_T1_RT_mean, by = c("id"))
GoRT_T0T1T2 <- merge(GoRT_T0T1, GoRT_T2_RT_mean, by = c("id"))
write.csv(GoRT_T0T1T2, "GoRT_T0T1T2.csv", row.names = F)
GoRT_T0T1T2 <- GoRT_T0T1T2 %>%
  dplyr::select(-c(1))
ICC(GoRT_T0T1T2)

#TempDisc_T0 <- read.csv("TempDiscounting_T0.csv")
#TempDisc_T1 <- read.csv("TempDiscounting_T1.csv")
#TempDisc_T0$Timing[TempDisc_T0$Timing == "Immediate"] <- 1
#TempDisc_T0$Timing[TempDisc_T0$Timing == "Delayed"] <- 0
#TempDisc_T0$Timing[TempDisc_T0$Timing == "None"] <- 0
#TempDisc_T0$Timing <- as.numeric(TempDisc_T0$Timing)
#
#TempDisc_T0_wide <- reshape(TempDisc_T0, idvar= "id", v.names= c("Timing"),
#                      timevar= "trial", direction = "wide")
#TempDisc_T0_wide$PercImm <- rowMeans(TempDisc_T0_wide[,2:19], na.rm = TRUE)
#TempDisc_T0_wide<-mutate(TempDisc_T0_wide, PercDelayed = 1 - PercImm)
#
#TempDisc_T1$Timing[TempDisc_T1$Timing == "Immediate"] <- 1
#TempDisc_T1$Timing[TempDisc_T1$Timing == "Delayed"] <- 0
#TempDisc_T1$Timing[TempDisc_T1$Timing == "None"] <- 0
#TempDisc_T1$Timing <- as.numeric(TempDisc_T1$Timing)
#
#TempDisc_T1_wide <- reshape(TempDisc_T1, idvar= "id", v.names= c("Timing"),
#                            timevar= "trial", direction = "wide")
#TempDisc_T1_wide$PercImm <- rowMeans(TempDisc_T1_wide[,2:19], na.rm = TRUE)
#TempDisc_T1_wide<-mutate(TempDisc_T1_wide, PercDelayed = 1 - PercImm)
#
#TempDisc_PercDelayed_T0 <- TempDisc_T0_wide %>%
#  select(,c("id", "PercDelayed"))
#TempDisc_PercDelayed_T0$timepoint <- rep(0, 172)
#
#TempDisc_PercDelayed_T1 <- TempDisc_T1_wide %>%
#  select(,c("id", "PercDelayed"))
#TempDisc_PercDelayed_T1$timepoint <- rep(1, 84)
#
#TempDisc_PercDelayed_T0T1 <- rbind(TempDisc_PercDelayed_T0, TempDisc_PercDelayed_T1)
#write.csv(TempDisc_PercDelayed_T0T1, "TempDisc_PercDelayed_T0T1.csv")





#DG
DG_T0 <- read.csv("DictatorGame_T0.csv")
DG_T1 <- read.csv("DictatorGame_T1.csv")
data <- read.csv("training_data.csv")

DG_coins <- data %>%
  dplyr::select(c("id", "timepoint", "DG_Coins_Given"))
DG_coins_T0T1 <- reshape(DG_coins, idvar= "id", v.names= c("DG_Coins_Given"),
                           timevar= "timepoint", direction = "wide")
DG_coins_T0T1 <- DG_coins_T0T1 %>%
  dplyr::select(c(2,3))
ICC(DG_coins_T0T1)

UG_Offer <- data %>%
  dplyr::select(c("id", "timepoint", "UG_Offer_Accept"))
UG_Offer_T0T1 <- reshape(UG_Offer, idvar= "id", v.names= c("UG_Offer_Accept"),
                         timevar= "timepoint", direction = "wide")
UG_Offer_T0T1 <- UG_Offer_T0T1 %>%
  dplyr::select(c(2,3))
ICC(UG_Offer_T0T1)


DG_CoinsGive_T0 <- DG_T0 %>% 
  filter(trial == 6) %>%
  dplyr::select(c("id", "boxcount2"))
names(DG_CoinsGive_T0)[names(DG_CoinsGive_T0) == "boxcount2"] <- "Coins"
DG_CoinsGive_T0$Coins <- as.numeric(DG_CoinsGive_T0$Coins)

DG_CoinsGive_T1 <- DG_T1 %>% 
  filter(trial == 6) %>%
  dplyr::select(c("id", "boxcount2"))
names(DG_CoinsGive_T1)[names(DG_CoinsGive_T1) == "boxcount2"] <- "Coins"
DG_CoinsGive_T1$Coins <- as.numeric(DG_CoinsGive_T1$Coins)

DG_CoinsGive_T0T1 <- merge(DG_CoinsGive_T0, DG_CoinsGive_T1, by = "id")
DG_CoinsGive_T0T1 <- DG_CoinsGive_T0T1 %>%
  dplyr::select(,c(2,3))
ICC(DG_CoinsGive_T0T1)

write.csv(DG_CoinsGive_T0T1, "DG_CoinsGive_T0T1.csv")



#cogflex
cogflex_T0 <- read.csv("cogflex_T0.csv")
cogflex_T1 <- read.csv("cogflex_T1.csv")
cogflex_T2 <- read.csv("cogflex_T2.csv")

cogflex_accuracy_T0 <- cogflex_T0 %>%
  dplyr::select(c("id","accuracy", "trial.p"))
cogflex_accuracy_T0 <- reshape(cogflex_accuracy_T0, idvar= "id", v.names= c("accuracy"),
                               timevar= "trial.p", direction = "wide")
cogflex_accuracy_T0$accuracy <- rowMeans(cogflex_accuracy_T0[,2:41], na.rm = TRUE)
cogflex_accuracy_T0 <- cogflex_accuracy_T0 %>%
  dplyr::select(c("id","accuracy"))

cogflex_accuracy_T1 <- cogflex_T1 %>%
  dplyr::select(c("id","accuracy", "trial.p"))
cogflex_accuracy_T1 <- reshape(cogflex_accuracy_T1, idvar= "id", v.names= c("accuracy"),
                               timevar= "trial.p", direction = "wide")
cogflex_accuracy_T1$accuracy <- rowMeans(cogflex_accuracy_T1[,2:41], na.rm = TRUE)
cogflex_accuracy_T1 <- cogflex_accuracy_T1 %>%
  dplyr::select(c("id","accuracy"))

cogflex_accuracy_T2 <- cogflex_T2 %>%
  dplyr::select(c("id","accuracy", "trial.p"))
cogflex_accuracy_T2 <- reshape(cogflex_accuracy_T2, idvar= "id", v.names= c("accuracy"),
                               timevar= "trial.p", direction = "wide")
cogflex_accuracy_T2$accuracy <- rowMeans(cogflex_accuracy_T2[,2:31], na.rm = TRUE)
cogflex_accuracy_T2 <- cogflex_accuracy_T2 %>%
  dplyr::select(c("id","accuracy"))

cogflex_accuracy_T0T1 <- merge(cogflex_accuracy_T0, cogflex_accuracy_T1, by = "id")
cogflex_accuracy_T0T1T2 <- merge(cogflex_accuracy_T0T1, cogflex_accuracy_T2, by = "id")

cogflex_accuracy_T0T1 <- cogflex_accuracy_T0T1 %>%
  dplyr::select(-c(1))
ICC(cogflex_accuracy_T0T1)

cogflex_accuracy_T0T1T2 <- cogflex_accuracy_T0T1T2 %>%
  dplyr::select(-c(1))
ICC(cogflex_accuracy_T0T1T2)





cogflex_T0_RT <- cogflex_T0 %>%
  dplyr::select(c("id", "trial.p", "accuracy", "RT"))
cogflex_T0_RT <- cogflex_T0_RT[cogflex_T0_RT$accuracy %in% c("1"),]
cogflex_T0_RT <- cogflex_T0_RT %>%
  dplyr::select(c("id", "trial.p", "RT"))
cogflex_T0_RT <- reshape(cogflex_T0_RT, idvar= "id", v.names= c("RT"),
                               timevar= "trial.p", direction = "wide")
cogflex_T0_RT$Avg.RT <- rowMeans(cogflex_T0_RT[,2:41], na.rm = TRUE)
cogflex_T0_RT <- cogflex_T0_RT %>%
  dplyr::select(c("id","Avg.RT"))

cogflex_T1_RT <- cogflex_T1 %>%
  dplyr::select(c("id", "trial.p", "accuracy", "RT"))
cogflex_T1_RT <- cogflex_T1_RT[cogflex_T1_RT$accuracy %in% c("1"),]
cogflex_T1_RT <- cogflex_T1_RT %>%
  dplyr::select(c("id", "trial.p", "RT"))
cogflex_T1_RT <- reshape(cogflex_T1_RT, idvar= "id", v.names= c("RT"),
                         timevar= "trial.p", direction = "wide")
cogflex_T1_RT$Avg.RT <- rowMeans(cogflex_T1_RT[,2:41], na.rm = TRUE)
cogflex_T1_RT <- cogflex_T1_RT %>%
  dplyr::select(c("id","Avg.RT"))

cogflex_T2_RT <- cogflex_T2 %>%
  dplyr::select(c("id", "trial.p", "accuracy", "RT"))
cogflex_T2_RT <- cogflex_T2_RT[cogflex_T2_RT$accuracy %in% c("1"),]
cogflex_T2_RT <- cogflex_T2_RT %>%
  dplyr::select(c("id", "trial.p", "RT"))
cogflex_T2_RT <- reshape(cogflex_T2_RT, idvar= "id", v.names= c("RT"),
                         timevar= "trial.p", direction = "wide")
cogflex_T2_RT$Avg.RT <- rowMeans(cogflex_T2_RT[,2:31], na.rm = TRUE)
cogflex_T2_RT <- cogflex_T2_RT %>%
  dplyr::select(c("id","Avg.RT"))

cogflex_RT_T0T1 <- merge(cogflex_T0_RT, cogflex_T1_RT, by = "id")
cogflex_RT_T0T1T2 <- merge(cogflex_RT_T0T1, cogflex_T2_RT, by = "id")

cogflex_RT_T0T1 <- cogflex_RT_T0T1 %>%
  dplyr::select(-c(1))
ICC(cogflex_RT_T0T1)

cogflex_RT_T0T1T2 <- cogflex_RT_T0T1T2 %>%
  dplyr::select(-c(1))
ICC(cogflex_RT_T0T1T2)
write.csv(cogflex_RT_T0T1T2, "cogflex_RT_T0T1T2.csv", row.names = F)
write.csv(cogflex_accuracy_T0T1T2, "cogflex_accuracy_T0T1T2.csv", row.names = F)




#corsi
corsi_T0 <- read.csv("corsi_T0.csv")
corsi_T1 <- read.csv("corsi_T1.csv")
corsi_T2 <- read.csv("corsi_T2.csv")

corsi_T0 <- corsi_T0 %>%
  dplyr::select("id", "span", "accuracy","trial")
corsi_T0 <- corsi_T0[corsi_T0$accuracy %in% c("1"),]
corsi_T0 <- reshape(corsi_T0, idvar= "id", v.names= c("span"),
                         timevar= "trial", direction = "wide")
corsi_T0$Avg.Span <- rowMeans(corsi_T0[,3:16], na.rm = TRUE)
corsi_T0 <- corsi_T0 %>%
  dplyr::select("id", "Avg.Span")

corsi_T1 <- corsi_T1 %>%
  dplyr::select("id", "span", "accuracy","trial")
corsi_T1 <- corsi_T1[corsi_T1$accuracy %in% c("1"),]
corsi_T1 <- reshape(corsi_T1, idvar= "id", v.names= c("span"),
                    timevar= "trial", direction = "wide")
corsi_T1$Avg.Span <- rowMeans(corsi_T1[,3:16], na.rm = TRUE)
corsi_T1 <- corsi_T1 %>%
  dplyr::select("id", "Avg.Span")

corsi_T2 <- corsi_T2 %>%
  dplyr::select("id", "span", "accuracy","trial")
corsi_T2 <- corsi_T2[corsi_T2$accuracy %in% c("1"),]
corsi_T2 <- reshape(corsi_T2, idvar= "id", v.names= c("span"),
                    timevar= "trial", direction = "wide")
corsi_T2$Avg.Span <- rowMeans(corsi_T2[,3:16], na.rm = TRUE)
corsi_T2 <- corsi_T2 %>%
  dplyr::select("id", "Avg.Span")

corsi_T0T1 <- merge(corsi_T0, corsi_T1, by = "id")
corsi_T0T1T2 <- merge(corsi_T0T1, corsi_T2, by = "id")

corsi_T0T1 <- corsi_T0T1 %>%
  dplyr::select(-c(1))
write.csv(corsi_T0T1, "corsi_T0T1.csv", row.names = F)
ICC(corsi_T0T1)

corsi_T0T1T2 <- corsi_T0T1T2 %>%
  dplyr::select(-c(1))
write.csv(corsi_T0T1T2, "corsi_T0T1T2.csv", row.names = F)
ICC(corsi_T0T1T2)



#nback
nback_T0 <- read.csv("nback_T0.csv")
nback_T1 <- read.csv("nback_T1.csv")


nback_accuracy_T0 <- nback_T0 %>%
  dplyr::select("id","accuracy","trial")
nback_accuracy_T0 <- reshape(nback_accuracy_T0, idvar= "id", v.names= c("accuracy"),
                    timevar= "trial", direction = "wide")
nback_accuracy_T0$accuracy <- rowMeans(nback_accuracy_T0[,2:41], na.rm = TRUE)
nback_accuracy_T0 <- nback_accuracy_T0 %>%
  dplyr::select("id", "accuracy")

nback_accuracy_T1 <- nback_T1 %>%
  dplyr::select("id","accuracy","trial")
nback_accuracy_T1 <- reshape(nback_accuracy_T1, idvar= "id", v.names= c("accuracy"),
                             timevar= "trial", direction = "wide")
nback_accuracy_T1$accuracy <- rowMeans(nback_accuracy_T1[,2:41], na.rm = TRUE)
nback_accuracy_T1 <- nback_accuracy_T1 %>%
  dplyr::select("id", "accuracy")

nback_accuracy_T0T1 <- merge(nback_accuracy_T0, nback_accuracy_T1, by = "id")
nback_accuracy_T0T1 <- nback_accuracy_T0T1 %>%
  dplyr::select(-c(1))
ICC(nback_accuracy_T0T1)
write.csv(nback_accuracy_T0T1, "nback_accuracy_T0T1.csv", row.names = F)

nback_RT_T0<- nback_T0 %>%
  dplyr::select(c("id", "trial", "accuracy", "RT"))
nback_RT_T0 <- nback_RT_T0[nback_RT_T0$accuracy %in% c("1"),]
nback_RT_T0[nback_RT_T0 == 0.000] <- NA
nback_RT_T0 <- nback_RT_T0 %>%
  na.omit()
nback_RT_T0 <- reshape(nback_RT_T0, idvar= "id", v.names= c("RT"),
                             timevar= "trial", direction = "wide")
nback_RT_T0$RT <- rowMeans(nback_RT_T0[,2:14], na.rm = TRUE)
nback_RT_T0 <- nback_RT_T0 %>%
  dplyr::select("id", "RT")

nback_RT_T1<- nback_T1 %>%
  dplyr::select(c("id", "trial", "accuracy", "RT"))
nback_RT_T1 <- nback_RT_T1[nback_RT_T1$accuracy %in% c("1"),]
nback_RT_T1[nback_RT_T1 == 0.000] <- NA
nback_RT_T1 <- nback_RT_T1 %>%
  na.omit()
nback_RT_T1 <- reshape(nback_RT_T1, idvar= "id", v.names= c("RT"),
                       timevar= "trial", direction = "wide")
nback_RT_T1$RT <- rowMeans(nback_RT_T1[,2:14], na.rm = TRUE)
nback_RT_T1 <- nback_RT_T1 %>%
  dplyr::select("id", "RT")

nback_RT_T0T1 <- merge(nback_RT_T0, nback_RT_T1, by = "id")
nback_RT_T0T1 <- nback_RT_T0T1 %>%
  dplyr::select(-c(1))
ICC(nback_RT_T0T1)
write.csv(nback_RT_T0T1, "nback_RT_T0T1.csv", row.names = F)



#test-retest reliability - academic score/WASI/creativity
academic_score <- read.csv("academic_score_T0T1.csv")
creativity <- read.csv("creativity_T0T1.csv")
wasi <- read.csv("WASI_T0T1.csv")
cogflex_accuracy_T0T1T2 <- read.csv("cogflex_accuracy_T0T1T2.csv")
cogflex_RT_T0T1T2 <- read.csv("cogflex_RT_T0T1T2.csv")
corsi_T0T1 <- read.csv("corsi_T0T1.csv")



academic_score_T0T1 <- reshape(academic_score, idvar= "id", v.names= c("academic_total_scores"),
                             timevar= "timepoint", direction = "wide")
academic_score_T0T1 <- academic_score_T0T1 %>%
  dplyr::select(-c(1))
ICC(academic_score_T0T1)

wasi_T0T1 <- reshape(wasi, idvar= "id", v.names= c("wasi"),
                               timevar= "timepoint", direction = "wide")
wasi_T0T1 <- wasi_T0T1 %>%
  dplyr::select(-c(1))
ICC(wasi_T0T1)

creativity_T0T1 <- reshape(creativity, idvar= "id", v.names= c("creativity"),
                     timevar= "timepoint", direction = "wide")
creativity_T0T1 <- creativity_T0T1 %>%
  dplyr::select(-c(1))
ICC(creativity_T0T1)




data <- read.csv("brain_measures.csv")

HavardOxford_Cortl <- data %>%
  dplyr::select("id", "timepoint", "HavardOxford_Cortl")
HavardOxford_Cortl <- reshape(HavardOxford_Cortl, idvar= "id", v.names= c("HavardOxford_Cortl"),
                           timevar= "timepoint", direction = "wide")
HavardOxford_Cortl <- HavardOxford_Cortl %>%
  dplyr::select(c(2,3))
ICC(HavardOxford_Cortl)


FPN <- data %>%
  dplyr::select("id", "timepoint", "FPN")
FPN <- reshape(FPN, idvar= "id", v.names= c("FPN"),
                              timevar= "timepoint", direction = "wide")
FPN <- FPN %>%
  dplyr::select(c(2,3))
ICC(FPN)

CON <- data %>%
  dplyr::select("id", "timepoint", "CON")
CON <- reshape(CON, idvar= "id", v.names= c("CON"),
               timevar= "timepoint", direction = "wide")
CON <- CON %>%
  dplyr::select(c(2,3))
ICC(CON)


CorticalThickness <- data %>%
  dplyr::select("id", "timepoint", "CorticalThickness")
CorticalThickness <- reshape(CorticalThickness, idvar= "id", v.names= c("CorticalThickness"),
               timevar= "timepoint", direction = "wide")
CorticalThickness <- CorticalThickness %>%
  dplyr::select(c(2,3))
ICC(CorticalThickness)


Rputa.frontal_FA <- data %>%
  dplyr::select("id", "timepoint", "Rputa.frontal_FA")
Rputa.frontal_FA <- reshape(Rputa.frontal_FA, idvar= "id", v.names= c("Rputa.frontal_FA"),
                             timevar= "timepoint", direction = "wide")
Rputa.frontal_FA <- Rputa.frontal_FA %>%
  dplyr::select(c(2,3))
ICC(Rputa.frontal_FA)


Rputa.frontal_MD <- data %>%
  dplyr::select("id", "timepoint", "Rputa.frontal_MD")
Rputa.frontal_MD <- reshape(Rputa.frontal_MD, idvar= "id", v.names= c("Rputa.frontal_MD"),
                            timevar= "timepoint", direction = "wide")
Rputa.frontal_MD <- Rputa.frontal_MD %>%
  dplyr::select(c(2,3))
ICC(Rputa.frontal_MD)






