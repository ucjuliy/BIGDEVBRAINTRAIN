#test-retest reliability analyses: tasks - separate groups#
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


#pstop
pstop <- read.csv("pStop_T0T1T2.csv")
Go_RT <- read.csv("GoRT_T0T1T2.csv")
group <- read.csv("group.csv")

pstop_both <- merge(pstop, group, by = "id")

pstop_exp <- pstop_both[pstop_both$group %in% c("1"),]
pstop_exp <- pstop_exp %>%
  dplyr::select(c(2,3,4))
ICC(pstop_exp)

pstop_con <- pstop_both[pstop_both$group %in% c("2"),]
pstop_con <- pstop_con %>%
  dplyr::select(c(2,3,4))
ICC(pstop_con)


pstop <- pstop %>%
  dplyr::select(c(2,3,4))
ICC(pstop)


GoRT_both <- merge(Go_RT, group, by = "id")

GoRT_exp <- GoRT_both[GoRT_both$group %in% c("1"),]
GoRT_exp <- GoRT_exp %>%
  dplyr::select(c(2,3,4))
ICC(GoRT_exp)

GoRT_con <- GoRT_both[GoRT_both$group %in% c("2"),]
GoRT_con <- GoRT_con %>%
  dplyr::select(c(2,3,4))
ICC(GoRT_con)

Go_RT <- Go_RT %>%
  dplyr::select(c(2,3,4))
ICC(Go_RT)




