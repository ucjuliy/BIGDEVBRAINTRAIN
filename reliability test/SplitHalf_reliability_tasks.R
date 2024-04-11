#reliability analyses: tasks#
setwd("~/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/training study/reliability test/task variables")

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
library(splithalf)
library(MASS)
library(splithalfr)


#stroop task 
stroop_T0 <- read.csv("stroop_T0.csv")
stroop_T1 <- read.csv("stroop_T1.csv")

stroop_T0_RT<- stroop_T0 %>%
  dplyr::select(c("id", "trial.p", "RT"))

stroop_T0_mean_RT <- function (stroop_T0_RT) {
  return (mean(stroop_T0_RT$RT))
}
by(
  stroop_T0_RT,
  stroop_T0_RT$id,
  stroop_T0_mean_RT
)
stroop_T0_RT_split <- by_split(
  stroop_T0_RT,
  stroop_T0_RT$id,
  stroop_T0_mean_RT,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
stroop_rt_t0 <- mean(split_coefs(stroop_T0_RT_split, spearman_brown))


stroop_T0_accuracy <- stroop_T0 %>%
  dplyr::select(c("id", "trial.p", "accuracy"))
stroop_T0_accuracy$accuracy <- as.numeric(stroop_T0_accuracy$accuracy)
stroop_T0_mean_accuracy <- function (stroop_T0_accuracy) {
  return (mean(stroop_T0_accuracy$accuracy))
}
by(
  stroop_T0_accuracy,
  stroop_T0_accuracy$id,
  stroop_T0_mean_accuracy
)
stroop_T0_accuracy_split <- by_split(
  stroop_T0_accuracy,
  stroop_T0_accuracy$id,
  stroop_T0_mean_accuracy,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
stroop_accuracy_t0 <- mean(split_coefs(stroop_T0_accuracy_split, spearman_brown))

stroop_T1_RT<- stroop_T1 %>%
  dplyr::select(c("id", "trial.p", "RT"))

stroop_T1_mean_RT <- function (stroop_T1_RT) {
  return (mean(stroop_T1_RT$RT))
}
by(
  stroop_T1_RT,
  stroop_T1_RT$id,
  stroop_T1_mean_RT
)
stroop_T1_RT_split <- by_split(
  stroop_T1_RT,
  stroop_T1_RT$id,
  stroop_T1_mean_RT,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
stroop_rt_t1 <- mean(split_coefs(stroop_T1_RT_split, spearman_brown))


stroop_T1_accuracy <- stroop_T1 %>%
  dplyr::select(c("id", "trial.p", "accuracy"))
stroop_T1_accuracy$accuracy <- as.numeric(stroop_T1_accuracy$accuracy)
stroop_T1_mean_accuracy <- function (stroop_T1_accuracy) {
  return (mean(stroop_T1_accuracy$accuracy))
}
by(
  stroop_T1_accuracy,
  stroop_T1_accuracy$id,
  stroop_T1_mean_accuracy
)
stroop_T1_accuracy_split <- by_split(
  stroop_T1_accuracy,
  stroop_T1_accuracy$id,
  stroop_T1_mean_accuracy,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
stroop_accuracy_t1 <- mean(split_coefs(stroop_T1_accuracy_split, spearman_brown))








#AX-CPT
AXCPT_T0 <- read.csv("AXCPT_T0.csv")
AXCPT_T1 <- read.csv("AXCPT_T1.csv")
AXCPT_T2 <- read.csv("AXCPT_T2.csv")


AXCPT_T0_RT<- AXCPT_T0 %>%
  dplyr::select(c("id", "trial.p", "RT"))
AXCPT_T0_RT <- AXCPT_T0_RT %>%
  na.omit()
AXCPT_T0_mean_RT <- function (AXCPT_T0_RT) {
  return (mean(AXCPT_T0_RT$RT))
}
by(
  AXCPT_T0_RT,
  AXCPT_T0_RT$id,
  AXCPT_T0_mean_RT
)
AXCPT_T0_RT_split <- by_split(
  AXCPT_T0_RT,
  AXCPT_T0_RT$id,
  AXCPT_T0_mean_RT,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
AXCPT_rt_t0 <- mean(split_coefs(AXCPT_T0_RT_split, spearman_brown))


AXCPT_T0_accuracy <- AXCPT_T0 %>%
  dplyr::select(c("id", "trial.p", "accuracy"))
AXCPT_T0_accuracy$accuracy <- as.numeric(AXCPT_T0_accuracy$accuracy)
AXCPT_T0_mean_accuracy <- function (AXCPT_T0_accuracy) {
  return (mean(AXCPT_T0_accuracy$accuracy))
}
by(
  AXCPT_T0_accuracy,
  AXCPT_T0_accuracy$id,
  AXCPT_T0_mean_accuracy
)
AXCPT_T0_accuracy_split <- by_split(
  AXCPT_T0_accuracy,
  AXCPT_T0_accuracy$id,
  AXCPT_T0_mean_accuracy,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
AXCPT_accuracy_t0 <- mean(split_coefs(AXCPT_T0_accuracy_split, spearman_brown))


AXCPT_T1_RT<- AXCPT_T1 %>%
  dplyr::select(c("id", "trial.p", "RT"))
AXCPT_T1_RT <- AXCPT_T1_RT %>%
  na.omit()
AXCPT_T1_mean_RT <- function (AXCPT_T1_RT) {
  return (mean(AXCPT_T1_RT$RT))
}
by(
  AXCPT_T1_RT,
  AXCPT_T1_RT$id,
  AXCPT_T1_mean_RT
)
AXCPT_T1_RT_split <- by_split(
  AXCPT_T1_RT,
  AXCPT_T1_RT$id,
  AXCPT_T1_mean_RT,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
AXCPT_rt_t1 <- mean(split_coefs(AXCPT_T1_RT_split, spearman_brown))


AXCPT_T1_accuracy <- AXCPT_T1 %>%
  dplyr::select(c("id", "trial.p", "accuracy"))
AXCPT_T1_accuracy$accuracy <- as.numeric(AXCPT_T1_accuracy$accuracy)
AXCPT_T1_mean_accuracy <- function (AXCPT_T1_accuracy) {
  return (mean(AXCPT_T1_accuracy$accuracy))
}
by(
  AXCPT_T1_accuracy,
  AXCPT_T1_accuracy$id,
  AXCPT_T1_mean_accuracy
)
AXCPT_T1_accuracy_split <- by_split(
  AXCPT_T1_accuracy,
  AXCPT_T1_accuracy$id,
  AXCPT_T1_mean_accuracy,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
AXCPT_accuracy_t1 <- mean(split_coefs(AXCPT_T1_accuracy_split, spearman_brown))



AXCPT_T2_RT<- AXCPT_T2 %>%
  dplyr::select(c("id", "trial", "RT"))
AXCPT_T2_RT <- AXCPT_T2_RT %>%
  na.omit()
AXCPT_T2_mean_RT <- function (AXCPT_T2_RT) {
  return (mean(AXCPT_T2_RT$RT))
}
by(
  AXCPT_T2_RT,
  AXCPT_T2_RT$id,
  AXCPT_T2_mean_RT
)
AXCPT_T2_RT_split <- by_split(
  AXCPT_T2_RT,
  AXCPT_T2_RT$id,
  AXCPT_T2_mean_RT,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
AXCPT_rt_t2 <- mean(split_coefs(AXCPT_T2_RT_split, spearman_brown))


AXCPT_T2_accuracy <- AXCPT_T2 %>%
  dplyr::select(c("id", "trial", "accuracy"))
AXCPT_T2_accuracy$accuracy <- as.numeric(AXCPT_T2_accuracy$accuracy)
AXCPT_T2_mean_accuracy <- function (AXCPT_T2_accuracy) {
  return (mean(AXCPT_T2_accuracy$accuracy))
}
by(
  AXCPT_T2_accuracy,
  AXCPT_T2_accuracy$id,
  AXCPT_T2_mean_accuracy
)
AXCPT_T2_accuracy_split <- by_split(
  AXCPT_T2_accuracy,
  AXCPT_T2_accuracy$id,
  AXCPT_T2_mean_accuracy,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
AXCPT_accuracy_t2 <- mean(split_coefs(AXCPT_T2_accuracy_split, spearman_brown))





#cogflex
cogflex_T0 <- read.csv("cogflex_T0.csv")
cogflex_T1 <- read.csv("cogflex_T1.csv")
cogflex_T2 <- read.csv("cogflex_T2.csv")

cogflex_T0_RT<- cogflex_T0 %>%
  dplyr::select(c("id", "trial.p", "RT"))
cogflex_T0_RT <- cogflex_T0_RT %>%
  na.omit()
cogflex_T0_mean_RT <- function (cogflex_T0_RT) {
  return (mean(cogflex_T0_RT$RT))
}
by(
  cogflex_T0_RT,
  cogflex_T0_RT$id,
  cogflex_T0_mean_RT
)
cogflex_T0_RT_split <- by_split(
  cogflex_T0_RT,
  cogflex_T0_RT$id,
  cogflex_T0_mean_RT,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
cogflex_rt_t0 <- mean(split_coefs(cogflex_T0_RT_split, spearman_brown))


cogflex_T0_accuracy <- cogflex_T0 %>%
  dplyr::select(c("id", "trial.p", "accuracy"))
cogflex_T0_accuracy$accuracy <- as.numeric(cogflex_T0_accuracy$accuracy)
cogflex_T0_mean_accuracy <- function (cogflex_T0_accuracy) {
  return (mean(cogflex_T0_accuracy$accuracy))
}
by(
  cogflex_T0_accuracy,
  cogflex_T0_accuracy$id,
  cogflex_T0_mean_accuracy
)
cogflex_T0_accuracy_split <- by_split(
  cogflex_T0_accuracy,
  cogflex_T0_accuracy$id,
  cogflex_T0_mean_accuracy,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
cogflex_accuracy_t0 <- mean(split_coefs(cogflex_T0_accuracy_split, spearman_brown))


cogflex_T1_RT<- cogflex_T1 %>%
  dplyr::select(c("id", "trial.p", "RT"))
cogflex_T1_RT <- cogflex_T1_RT %>%
  na.omit()
cogflex_T1_mean_RT <- function (cogflex_T1_RT) {
  return (mean(cogflex_T1_RT$RT))
}
by(
  cogflex_T1_RT,
  cogflex_T1_RT$id,
  cogflex_T1_mean_RT
)
cogflex_T1_RT_split <- by_split(
  cogflex_T1_RT,
  cogflex_T1_RT$id,
  cogflex_T1_mean_RT,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
cogflex_rt_t1 <- mean(split_coefs(cogflex_T1_RT_split, spearman_brown))


cogflex_T1_accuracy <- cogflex_T1 %>%
  dplyr::select(c("id", "trial.p", "accuracy"))
cogflex_T1_accuracy$accuracy <- as.numeric(cogflex_T1_accuracy$accuracy)
cogflex_T1_mean_accuracy <- function (cogflex_T1_accuracy) {
  return (mean(cogflex_T1_accuracy$accuracy))
}
by(
  cogflex_T1_accuracy,
  cogflex_T1_accuracy$id,
  cogflex_T1_mean_accuracy
)
cogflex_T1_accuracy_split <- by_split(
  cogflex_T1_accuracy,
  cogflex_T1_accuracy$id,
  cogflex_T1_mean_accuracy,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
cogflex_accuracy_t1 <- mean(split_coefs(cogflex_T1_accuracy_split, spearman_brown))

cogflex_T2_RT<- cogflex_T2 %>%
  dplyr::select(c("id", "trial.p", "RT"))
cogflex_T2_RT <- cogflex_T2_RT %>%
  na.omit()
cogflex_T2_mean_RT <- function (cogflex_T2_RT) {
  return (mean(cogflex_T2_RT$RT))
}
by(
  cogflex_T2_RT,
  cogflex_T2_RT$id,
  cogflex_T2_mean_RT
)
cogflex_T2_RT_split <- by_split(
  cogflex_T2_RT,
  cogflex_T2_RT$id,
  cogflex_T2_mean_RT,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
cogflex_rt_t2 <- mean(split_coefs(cogflex_T2_RT_split, spearman_brown))


cogflex_T2_accuracy <- cogflex_T2 %>%
  dplyr::select(c("id", "trial.p", "accuracy"))
cogflex_T2_accuracy$accuracy <- as.numeric(cogflex_T2_accuracy$accuracy)
cogflex_T2_mean_accuracy <- function (cogflex_T2_accuracy) {
  return (mean(cogflex_T2_accuracy$accuracy))
}
by(
  cogflex_T2_accuracy,
  cogflex_T2_accuracy$id,
  cogflex_T2_mean_accuracy
)
cogflex_T2_accuracy_split <- by_split(
  cogflex_T2_accuracy,
  cogflex_T2_accuracy$id,
  cogflex_T2_mean_accuracy,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
cogflex_accuracy_t2 <- mean(split_coefs(cogflex_T2_accuracy_split, spearman_brown))






#dictator game
DG_T0 <- read.csv("DictatorGame_T0.csv")
DG_T1 <- read.csv("DictatorGame_T1.csv")

DG_T0_coins <- DG_T0 %>%
  dplyr::select(c("id", "trial", "coins_given"))
DG_T0_coins$coins_given <- as.numeric(DG_T0_coins$coins_given)
DG_T0_sum_coins <- function (DG_T0_coins) {
  return (sum(DG_T0_coins$coins_given))
}
by(
  DG_T0_coins,
  DG_T0_coins$id,
  DG_T0_sum_coins
)
DG_T0_coins_split <- by_split(
  DG_T0_coins,
  DG_T0_coins$id,
  DG_T0_sum_coins,
  method = c("random"),
  ncores = 1,
  match_participants = F
)
DG_coins_t0 <- mean(split_coefs(DG_T0_coins_split, spearman_brown))

DG_T1_coins <- DG_T1 %>%
  dplyr::select(c("id", "trial", "coins_given"))
DG_T1_coins$coins_given <- as.numeric(DG_T1_coins$coins_given)
DG_T1_sum_coins <- function (DG_T1_coins) {
  return (sum(DG_T1_coins$coins_given))
}
by(
  DG_T1_coins,
  DG_T1_coins$id,
  DG_T1_sum_coins
)
DG_T1_coins_split <- by_split(
  DG_T1_coins,
  DG_T1_coins$id,
  DG_T1_sum_coins,
  method = c("random"),
  ncores = 1,
  match_participants = F
)
DG_coins_t1 <- mean(split_coefs(DG_T1_coins_split, spearman_brown))






#flanker inhibition
inhibition_T0 <- read.csv("flanker_inhibition_T0.csv")
inhibition_T1 <- read.csv("flanker_inhibition_T1.csv")

inhibition_T0_RT<- inhibition_T0 %>%
  dplyr::select(c("id", "trial", "RT"))
inhibition_T0_RT <- inhibition_T0_RT %>%
  na.omit()
inhibition_T0_mean_RT <- function (inhibition_T0_RT) {
  return (mean(inhibition_T0_RT$RT))
}
by(
  inhibition_T0_RT,
  inhibition_T0_RT$id,
  inhibition_T0_mean_RT
)
inhibition_T0_RT_split <- by_split(
  inhibition_T0_RT,
  inhibition_T0_RT$id,
  inhibition_T0_mean_RT,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
flanker_inhibition_rt_t0 <- mean(split_coefs(inhibition_T0_RT_split, spearman_brown))


inhibition_T0_accuracy <- inhibition_T0 %>%
  dplyr::select(c("id", "trial", "accuracy"))
inhibition_T0_accuracy$accuracy <- as.numeric(inhibition_T0_accuracy$accuracy)
inhibition_T0_mean_accuracy <- function (inhibition_T0_accuracy) {
  return (mean(inhibition_T0_accuracy$accuracy))
}
by(
  inhibition_T0_accuracy,
  inhibition_T0_accuracy$id,
  inhibition_T0_mean_accuracy
)
inhibition_T0_accuracy_split <- by_split(
  inhibition_T0_accuracy,
  inhibition_T0_accuracy$id,
  inhibition_T0_mean_accuracy,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
flanker_inhibition_accuracy_t0 <- mean(split_coefs(inhibition_T0_accuracy_split, spearman_brown))


inhibition_T1_RT<- inhibition_T1 %>%
  dplyr::select(c("id", "trial", "RT"))
inhibition_T1_RT <- inhibition_T1_RT %>%
  na.omit()
inhibition_T1_mean_RT <- function (inhibition_T1_RT) {
  return (mean(inhibition_T1_RT$RT))
}
by(
  inhibition_T1_RT,
  inhibition_T1_RT$id,
  inhibition_T1_mean_RT
)
inhibition_T1_RT_split <- by_split(
  inhibition_T1_RT,
  inhibition_T1_RT$id,
  inhibition_T1_mean_RT,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
flanker_inhibition_rt_t1 <- mean(split_coefs(inhibition_T1_RT_split, spearman_brown))


inhibition_T1_accuracy <- inhibition_T1 %>%
  dplyr::select(c("id", "trial", "accuracy"))
inhibition_T1_accuracy$accuracy <- as.numeric(inhibition_T1_accuracy$accuracy)
inhibition_T1_mean_accuracy <- function (inhibition_T1_accuracy) {
  return (mean(inhibition_T1_accuracy$accuracy))
}
by(
  inhibition_T1_accuracy,
  inhibition_T1_accuracy$id,
  inhibition_T1_mean_accuracy
)
inhibition_T1_accuracy_split <- by_split(
  inhibition_T1_accuracy,
  inhibition_T1_accuracy$id,
  inhibition_T1_mean_accuracy,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
flanker_inhibition_accuracy_t1 <- mean(split_coefs(inhibition_T1_accuracy_split, spearman_brown))






#flanker shifting
shifting_T0 <- read.csv("flanker_shifting_T0.csv")
shifting_T1 <- read.csv("flanker_shifting_T1.csv")


shifting_T0_RT<- shifting_T0 %>%
  dplyr::select(c("id", "trial.p", "RT"))
shifting_T0_RT <- shifting_T0_RT %>%
  na.omit()
shifting_T0_mean_RT <- function (shifting_T0_RT) {
  return (mean(shifting_T0_RT$RT))
}
by(
  shifting_T0_RT,
  shifting_T0_RT$id,
  shifting_T0_mean_RT
)
shifting_T0_RT_split <- by_split(
  shifting_T0_RT,
  shifting_T0_RT$id,
  shifting_T0_mean_RT,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
flanker_shifting_rt_t0 <- mean(split_coefs(shifting_T0_RT_split, spearman_brown))


shifting_T0_accuracy <- shifting_T0 %>%
  dplyr::select(c("id", "trial.p", "accuracy"))
shifting_T0_accuracy$accuracy <- as.numeric(shifting_T0_accuracy$accuracy)
shifting_T0_mean_accuracy <- function (shifting_T0_accuracy) {
  return (mean(shifting_T0_accuracy$accuracy))
}
by(
  shifting_T0_accuracy,
  shifting_T0_accuracy$id,
  shifting_T0_mean_accuracy
)
shifting_T0_accuracy_split <- by_split(
  shifting_T0_accuracy,
  shifting_T0_accuracy$id,
  shifting_T0_mean_accuracy,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
flanker_shifting_accuracy_t0 <- mean(split_coefs(shifting_T0_accuracy_split, spearman_brown))


shifting_T1_RT<- shifting_T1 %>%
  dplyr::select(c("id", "trial.p", "RT"))
shifting_T1_RT <- shifting_T1_RT %>%
  na.omit()
shifting_T1_mean_RT <- function (shifting_T1_RT) {
  return (mean(shifting_T1_RT$RT))
}
by(
  shifting_T1_RT,
  shifting_T1_RT$id,
  shifting_T1_mean_RT
)
shifting_T1_RT_split <- by_split(
  shifting_T1_RT,
  shifting_T1_RT$id,
  shifting_T1_mean_RT,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
flanker_shifting_rt_t1 <- mean(split_coefs(shifting_T1_RT_split, spearman_brown))


shifting_T1_accuracy <- shifting_T1 %>%
  dplyr::select(c("id", "trial.p", "accuracy"))
shifting_T1_accuracy$accuracy <- as.numeric(shifting_T1_accuracy$accuracy)
shifting_T1_mean_accuracy <- function (shifting_T1_accuracy) {
  return (mean(shifting_T1_accuracy$accuracy))
}
by(
  shifting_T1_accuracy,
  shifting_T1_accuracy$id,
  shifting_T1_mean_accuracy
)
shifting_T1_accuracy_split <- by_split(
  shifting_T1_accuracy,
  shifting_T1_accuracy$id,
  shifting_T1_mean_accuracy,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
flanker_shifting_accuracy_t1 <- mean(split_coefs(shifting_T1_accuracy_split, spearman_brown))







#temporal discounting
#delayed = 1; immediate = 0
tempdisc_T0 <- read.csv("TempDiscounting_T0.csv")
tempdisc_T1 <- read.csv("TempDiscounting_T1.csv")

tempdisc_T0_timing <- tempdisc_T0 %>%
  dplyr::select(c("id", "trial", "Timing"))
tempdisc_T0_timing$Timing <- as.numeric(tempdisc_T0_timing$Timing)
tempdisc_T0_mean_timing <- function (tempdisc_T0_timing) {
  return (mean(tempdisc_T0_timing$Timing))
}
by(
  tempdisc_T0_timing,
  tempdisc_T0_timing$id,
  tempdisc_T0_mean_timing
)
tempdisc_T0_timing_split <- by_split(
  tempdisc_T0_timing,
  tempdisc_T0_timing$id,
  tempdisc_T0_mean_timing,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
tempdisc_delayedperc_t0 <- mean(split_coefs(tempdisc_T0_timing_split, spearman_brown))


tempdisc_T1_timing <- tempdisc_T1 %>%
  dplyr::select(c("id", "trial", "Timing"))
tempdisc_T1_timing$Timing <- as.numeric(tempdisc_T1_timing$Timing)
tempdisc_T1_mean_timing <- function (tempdisc_T1_timing) {
  return (mean(tempdisc_T1_timing$Timing))
}
by(
  tempdisc_T1_timing,
  tempdisc_T1_timing$id,
  tempdisc_T1_mean_timing
)
tempdisc_T1_timing_split <- by_split(
  tempdisc_T1_timing,
  tempdisc_T1_timing$id,
  tempdisc_T1_mean_timing,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
tempdisc_delayedperc_t1 <- mean(split_coefs(tempdisc_T1_timing_split, spearman_brown))



#nback 
nback_T0 <- read.csv("Nback_T0.csv")
nback_T1 <- read.csv("Nback_T1.csv")

nback_T0_RT<- nback_T0 %>%
  dplyr::select(c("id", "trial", "accuracy", "RT"))
nback_T0_RT <- nback_T0_RT[nback_T0_RT$accuracy %in% c("1"),]
nback_T0_RT <- nback_T0_RT %>%
  na.omit()
nback_T0_mean_RT <- function (nback_T0_RT) {
  return (mean(nback_T0_RT$RT))
}
by(
  nback_T0_RT,
  nback_T0_RT$id,
  nback_T0_mean_RT
)
nback_T0_RT_split <- by_split(
  nback_T0_RT,
  nback_T0_RT$id,
  nback_T0_mean_RT,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
nback_rt_t0 <- mean(split_coefs(nback_T0_RT_split, spearman_brown))


nback_T0_accuracy <- nback_T0 %>%
  dplyr::select(c("id", "trial", "accuracy"))
nback_T0_accuracy$accuracy <- as.numeric(nback_T0_accuracy$accuracy)
nback_T0_mean_accuracy <- function (nback_T0_accuracy) {
  return (mean(nback_T0_accuracy$accuracy))
}
by(
  nback_T0_accuracy,
  nback_T0_accuracy$id,
  nback_T0_mean_accuracy
)
nback_T0_accuracy_split <- by_split(
  nback_T0_accuracy,
  nback_T0_accuracy$id,
  nback_T0_mean_accuracy,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
nback_accuracy_t0 <- mean(split_coefs(nback_T0_accuracy_split, spearman_brown))

nback_T1_RT<- nback_T1 %>%
  dplyr::select(c("id", "trial", "accuracy", "RT"))
nback_T1_RT <- nback_T1_RT[nback_T1_RT$accuracy %in% c("1"),]
nback_T1_RT <- nback_T1_RT %>%
  na.omit()
nback_T1_mean_RT <- function (nback_T1_RT) {
  return (mean(nback_T1_RT$RT))
}
by(
  nback_T1_RT,
  nback_T1_RT$id,
  nback_T1_mean_RT
)
nback_T1_RT_split <- by_split(
  nback_T1_RT,
  nback_T1_RT$id,
  nback_T1_mean_RT,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
nback_rt_t1 <- mean(split_coefs(nback_T1_RT_split, spearman_brown))


nback_T1_accuracy <- nback_T1 %>%
  dplyr::select(c("id", "trial", "accuracy"))
nback_T1_accuracy$accuracy <- as.numeric(nback_T1_accuracy$accuracy)
nback_T1_mean_accuracy <- function (nback_T1_accuracy) {
  return (mean(nback_T1_accuracy$accuracy))
}
by(
  nback_T1_accuracy,
  nback_T1_accuracy$id,
  nback_T1_mean_accuracy
)
nback_T1_accuracy_split <- by_split(
  nback_T1_accuracy,
  nback_T1_accuracy$id,
  nback_T1_mean_accuracy,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
nback_accuracy_t1 <- mean(split_coefs(nback_T1_accuracy_split, spearman_brown))






#pstop - accuracy 1: successfully stopped; accuracy 0: unsuccessful 
pstop_T0 <- read.csv("pstop_T0.csv")
pstop_T1 <- read.csv("pstop_T1.csv")
pstop_T2 <- read.csv("pstop_T2.csv")

pstop_T0_accuracy <- pstop_T0 %>%
  dplyr::select(c("id", "trial.p", "accuracy"))
pstop_T0_accuracy_wide <- reshape(pstop_T0_accuracy, idvar= "id", v.names= c("accuracy"),
                                  timevar= "trial.p", direction = "wide")

pstop_T0_accuracy_wide$accuracy_firsthalf <- rowMeans(pstop_T0_accuracy_wide[,2:15], na.rm = TRUE)
pstop_T0_accuracy_wide$accuracy_secondhalf <- rowMeans(pstop_T0_accuracy_wide[,16:29], na.rm = TRUE)
spearman_brown(pstop_T0_accuracy_wide$accuracy_firsthalf, pstop_T0_accuracy_wide$accuracy_secondhalf)


pstop_T0_accuracy_wide$accuracy_oddhalf <- rowMeans(pstop_T0_accuracy_wide[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28)], na.rm = TRUE)
pstop_T0_accuracy_wide$accuracy_evenhalf <- rowMeans(pstop_T0_accuracy_wide[c(3,5,7,9,11,13,15,17,19,21,23,25,27,29)], na.rm = TRUE)

spearman_brown(pstop_T0_accuracy_wide$accuracy_oddhalf, pstop_T0_accuracy_wide$accuracy_evenhalf)


pstop_T1_accuracy <- pstop_T1 %>%
  dplyr::select(c("id", "trial.p", "accuracy"))
pstop_T1_accuracy_wide <- reshape(pstop_T1_accuracy, idvar= "id", v.names= c("accuracy"),
                                  timevar= "trial.p", direction = "wide")
pstop_T1_accuracy_wide$accuracy_firsthalf <- rowMeans(pstop_T1_accuracy_wide[,2:15], na.rm = TRUE)
pstop_T1_accuracy_wide$accuracy_secondhalf <- rowMeans(pstop_T1_accuracy_wide[,16:29], na.rm = TRUE)
spearman_brown(pstop_T1_accuracy_wide$accuracy_firsthalf, pstop_T1_accuracy_wide$accuracy_secondhalf)

pstop_T1_accuracy_wide$accuracy_oddhalf <- rowMeans(pstop_T1_accuracy_wide[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28)], na.rm = TRUE)
pstop_T1_accuracy_wide$accuracy_evenhalf <- rowMeans(pstop_T1_accuracy_wide[c(3,5,7,9,11,13,15,17,19,21,23,25,27,29)], na.rm = TRUE)
spearman_brown(pstop_T1_accuracy_wide$accuracy_oddhalf, pstop_T1_accuracy_wide$accuracy_evenhalf)


pstop_T2_accuracy <- pstop_T2 %>%
  dplyr::select(c("id", "trial.p", "accuracy"))
pstop_T2_accuracy_wide <- reshape(pstop_T2_accuracy, idvar= "id", v.names= c("accuracy"),
                                  timevar= "trial.p", direction = "wide")
pstop_T2_accuracy_wide$accuracy_firsthalf <- rowMeans(pstop_T2_accuracy_wide[,2:11], na.rm = TRUE)
pstop_T2_accuracy_wide$accuracy_secondhalf <- rowMeans(pstop_T2_accuracy_wide[,12:21], na.rm = TRUE)
spearman_brown(pstop_T2_accuracy_wide$accuracy_firsthalf, pstop_T2_accuracy_wide$accuracy_secondhalf)

pstop_T2_accuracy_wide$accuracy_oddhalf <- rowMeans(pstop_T2_accuracy_wide[c(2,4,6,8,10,12,14,16,18,20)], na.rm = TRUE)
pstop_T2_accuracy_wide$accuracy_evenhalf <- rowMeans(pstop_T2_accuracy_wide[c(3,5,7,9,11,13,15,17,19,21)], na.rm = TRUE)
spearman_brown(pstop_T2_accuracy_wide$accuracy_oddhalf, pstop_T2_accuracy_wide$accuracy_evenhalf)



pstop_T0_accuracy <- pstop_T0 %>%
  dplyr::select(c("id", "trial.p", "accuracy"))
pstop_T0_accuracy$accuracy <- as.numeric(pstop_T0_accuracy$accuracy)
pstop_T0_mean_accuracy <- function (pstop_T0_accuracy) {
  return (mean(pstop_T0_accuracy$accuracy))
}
by(
  pstop_T0_accuracy,
  pstop_T0_accuracy$id,
  pstop_T0_mean_accuracy
)
pstop_T0_accuracy_split <- by_split(
  pstop_T0_accuracy,
  pstop_T0_accuracy$id,
  pstop_T0_mean_accuracy,
  method = c("random"),
  ncores = 1,
  match_participants = F
)
pstop_t0 <- mean(split_coefs(pstop_T0_accuracy_split, spearman_brown))


pstop_T1_accuracy <- pstop_T1 %>%
  dplyr::select(c("id", "trial.p", "accuracy"))
pstop_T1_accuracy$accuracy <- as.numeric(pstop_T1_accuracy$accuracy)
pstop_T1_mean_accuracy <- function (pstop_T1_accuracy) {
  return (mean(pstop_T1_accuracy$accuracy))
}
by(
  pstop_T1_accuracy,
  pstop_T1_accuracy$id,
  pstop_T1_mean_accuracy
)
pstop_T1_accuracy_split <- by_split(
  pstop_T1_accuracy,
  pstop_T1_accuracy$id,
  pstop_T1_mean_accuracy,
  method = c("even_odd"),
  ncores = 1,
  match_participants = F
)
pstop_t1 <- mean(split_coefs(pstop_T1_accuracy_split, spearman_brown))


pstop_T2_accuracy <- pstop_T2 %>%
  dplyr::select(c("id", "trial.p", "accuracy"))
pstop_T2_accuracy$accuracy <- as.numeric(pstop_T2_accuracy$accuracy)
pstop_T2_mean_accuracy <- function (pstop_T2_accuracy) {
  return (mean(pstop_T2_accuracy$accuracy))
}
by(
  pstop_T2_accuracy,
  pstop_T2_accuracy$id,
  pstop_T2_mean_accuracy
)
pstop_T2_accuracy_split <- by_split(
  pstop_T2_accuracy,
  pstop_T2_accuracy$id,
  pstop_T2_mean_accuracy,
  method = c("even_odd"),
  ncores = 1,
  match_participants = F
)
pstop_t2 <- mean(split_coefs(pstop_T2_accuracy_split, spearman_brown))





#Go RT
GoRT_T0 <- read.csv("Go_RT_T0.csv")
GoRT_T1 <- read.csv("Go_RT_T1.csv")
GoRT_T2 <- read.csv("Go_RT_T2.csv")


GoRT_T0_RT<- GoRT_T0 %>%
  dplyr::select(c("id", "trial", "RT"))
GoRT_T0_mean_RT <- function (GoRT_T0_RT) {
  return (mean(GoRT_T0_RT$RT))
}
by(
  GoRT_T0_RT,
  GoRT_T0_RT$id,
  GoRT_T0_mean_RT
)
GoRT_T0_RT_split <- by_split(
  GoRT_T0_RT,
  GoRT_T0_RT$id,
  GoRT_T0_mean_RT,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
Go_RT_t0 <- mean(split_coefs(GoRT_T0_RT_split, spearman_brown))

GoRT_T1_RT<- GoRT_T1 %>%
  dplyr::select(c("id", "trial", "RT"))
GoRT_T1_mean_RT <- function (GoRT_T1_RT) {
  return (mean(GoRT_T1_RT$RT))
}
by(
  GoRT_T1_RT,
  GoRT_T1_RT$id,
  GoRT_T1_mean_RT
)
GoRT_T1_RT_split <- by_split(
  GoRT_T1_RT,
  GoRT_T1_RT$id,
  GoRT_T1_mean_RT,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
Go_RT_t1 <- mean(split_coefs(GoRT_T1_RT_split, spearman_brown))

GoRT_T2_RT<- GoRT_T2 %>%
  dplyr::select(c("id", "trial", "RT"))
GoRT_T2_mean_RT <- function (GoRT_T2_RT) {
  return (mean(GoRT_T2_RT$RT))
}
by(
  GoRT_T2_RT,
  GoRT_T2_RT$id,
  GoRT_T2_mean_RT
)
GoRT_T2_RT_split <- by_split(
  GoRT_T2_RT,
  GoRT_T2_RT$id,
  GoRT_T2_mean_RT,
  method = c("first_second"),
  ncores = 1,
  match_participants = F
)
Go_RT_t2 <- mean(split_coefs(GoRT_T2_RT_split, spearman_brown))









#fMRI
fMRI_T0_R1 <- read.csv("fMRI_T0_R1.csv")
fMRI_T0_R2 <- read.csv("fMRI_T0_R2.csv")
fMRI_T1_R1 <- read.csv("fMRI_T1_R1.csv")
fMRI_T1_R2 <- read.csv("fMRI_T1_R2.csv")


fMRI_T0_R1_outliers <- boxplot(fMRI_T0_R1$HavardOxford_Cortl, plot=FALSE)$out
fMRI_T0_R1 <- fMRI_T0_R1[-which(fMRI_T0_R1$HavardOxford_Cortl %in% fMRI_T0_R1_outliers),]

fMRI_T0_R2_outliers <- boxplot(fMRI_T0_R2$HavardOxford_Cortl, plot=FALSE)$out
fMRI_T0_R2 <- fMRI_T0_R2[-which(fMRI_T0_R2$HavardOxford_Cortl %in% fMRI_T0_R2_outliers),]

fMRI_T1_R1_outliers <- boxplot(fMRI_T1_R1$HavardOxford_Cortl, plot=FALSE)$out
fMRI_T1_R1 <- fMRI_T1_R1[-which(fMRI_T1_R1$HavardOxford_Cortl %in% fMRI_T1_R1_outliers),]

fMRI_T1_R2_outliers <- boxplot(fMRI_T1_R2$HavardOxford_Cortl, plot=FALSE)$out
fMRI_T1_R2 <- fMRI_T1_R2[-which(fMRI_T1_R2$HavardOxford_Cortl %in% fMRI_T1_R2_outliers),]




fMRI_T0_excludedoutliers <- merge(fMRI_T0_R1, fMRI_T0_R2, by = "id", all.x = TRUE)
write.csv(fMRI_T0_excludedoutliers, "fMRI_T0_excludedoutliers.csv")
fMRI_T0 <- merge(fMRI_T0_R1, fMRI_T0_R2, by = "id")
spearman_brown(fMRI_T0$HavardOxford_Cortl.x, fMRI_T0$HavardOxford_Cortl.y, 
               fn_cor=cor)
fMRI_T0 <- fMRI_T0 %>%
  dplyr::select(-c(1))
ICC(fMRI_T0)

fMRI_T1_excludedoutliers <- merge(fMRI_T1_R1, fMRI_T1_R2, by = "id", all.x = T)
write.csv(fMRI_T1_excludedoutliers, "fMRI_T1_excludedoutliers.csv")
fMRI_T1 <- merge(fMRI_T1_R1, fMRI_T1_R2, by = "id")
spearman_brown(fMRI_T1$HavardOxford_Cortl.x, fMRI_T1$HavardOxford_Cortl.y, 
               fn_cor=cor)
fMRI_T1 <- fMRI_T1 %>%
  dplyr::select(-c(1))
icc(fMRI_T1)
