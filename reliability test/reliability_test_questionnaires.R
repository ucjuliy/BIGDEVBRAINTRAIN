#reliability analyses: AES, SDQ, CASI#
setwd("~/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/training study/reliability test")

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


#load data
AES <- read.csv("aes.csv")
SDQ <- read.csv("sdq.csv")
CASI <- read.csv("casi.csv")


#test-retest reliability AES
AES_total <- AES%>%
  select("ID", "timepoint", "total_aes")
AES_total <- reshape(AES_total, idvar= "ID", v.names= c("total_aes"),
                     timevar= "timepoint", direction = "wide")
AES_total <- AES_total %>% 
  select(-c(1))
ICC(AES_total)

#test-retest reliability SDQ/internalising/externalising problems
SDQ_total <- SDQ%>%
  select("id", "timepoint", "total_sdq")
SDQ_total <- reshape(SDQ_total, idvar= "id", v.names= c("total_sdq"),
                     timevar= "timepoint", direction = "wide")
SDQ_total <- SDQ_total %>% 
  select(-c(1))
ICC(SDQ_total)

SDQ_internalising <- SDQ %>%
  select(,c("id", "timepoint", 
            "SDQ_Q1","SDQ_Q6","SDQ_Q8","SDQ_Q11","SDQ_Q13","SDQ_Q14",
            "SDQ_Q16","SDQ_Q19","SDQ_Q23","SDQ_Q24"))
SDQ_internalising$internalising_total <- rowSums( SDQ_internalising[,3:12] )

SDQ_internalising_total <- SDQ_internalising %>%
  select("id", "timepoint", "internalising_total")
SDQ_internalising_total <- reshape(SDQ_internalising_total, idvar= "id", v.names= c("internalising_total"),
                     timevar= "timepoint", direction = "wide")
SDQ_internalising_total <- SDQ_internalising_total %>% 
  select(-c(1))
ICC(SDQ_internalising_total)

SDQ_externalising <- SDQ %>%
  select(,c("id", "timepoint", 
            "SDQ_Q2","SDQ_Q5","SDQ_Q7","SDQ_Q10","SDQ_Q12","SDQ_Q15",
            "SDQ_Q18","SDQ_Q21","SDQ_Q22","SDQ_Q25"))
SDQ_externalising$externalsing_total <- rowSums( SDQ_externalising[,3:12] )

SDQ_externalising_total <- SDQ_externalising %>%
  select("id", "timepoint", "externalsing_total")
SDQ_externalising_total <- reshape(SDQ_externalising_total, idvar= "id", v.names= c("externalsing_total"),
                                   timevar= "timepoint", direction = "wide")
SDQ_externalising_total <- SDQ_externalising_total %>% 
  select(-c(1))
ICC(SDQ_externalising_total)


#test-retest reliability CASI ADHD (externalising), 
#CASI-social phobia, and CASI-separation anxiety and -depression (internalsing)
CASI_total <- CASI %>%
  select("id", "timepoint", "casi_total")
CASI_total <- reshape(CASI_total, idvar= "id", v.names= c("casi_total"),
                     timevar= "timepoint", direction = "wide")
CASI_total <- CASI_total %>% 
  select(-c(1))
ICC(CASI_total)

CASI_ADHD <- CASI %>%
  select("id", "timepoint", "adhd_tot_casi")
CASI_ADHD <- reshape(CASI_ADHD, idvar= "id", v.names= c("adhd_tot_casi"),
                     timevar= "timepoint", direction = "wide")
CASI_ADHD <- CASI_ADHD %>% 
  select(-c(1))
ICC(CASI_ADHD)

CASI_MDD <- CASI %>%
  select("id", "timepoint", "mdd_total_casi")
CASI_MDD <- reshape(CASI_MDD, idvar= "id", v.names= c("mdd_total_casi"),
                     timevar= "timepoint", direction = "wide")
CASI_MDD <- CASI_MDD %>% 
  select(-c(1))
ICC(CASI_MDD)

CASI_sepanx <- CASI %>%
  select("id", "timepoint", "sepanx_casi")
CASI_sepanx <- reshape(CASI_sepanx, idvar= "id", v.names= c("sepanx_casi"),
                    timevar= "timepoint", direction = "wide")
CASI_sepanx <- CASI_sepanx %>% 
  select(-c(1))
ICC(CASI_sepanx)

CASI_socphob <- CASI %>%
  select("id", "timepoint", "socphob_casi")
CASI_socphob <- reshape(CASI_socphob, idvar= "id", v.names= c("socphob_casi"),
                       timevar= "timepoint", direction = "wide")
CASI_socphob <- CASI_socphob %>% 
  select(-c(1))
ICC(CASI_socphob)




#split half test internal reliability AES
AES_T0 <- AES[AES$timepoint %in% c("0"), ]
psych::alpha(AES_T0[,c("AES_C1",	"AES_C3",	"AES_C4",	
                "AES_C5",	"AES_B6",	"AES_E7",	
                "AES_C8",	"AES_B9",	"AES_B10",
                "AES_C11",	"AES_B12",	"AES_C13",
                "AES_E14",	"AES_O15",	"AES_C16",
                "AES_O17",	"AES_O18")])

AES_T1 <- AES[AES$timepoint %in% c("1"), ]
psych::alpha(AES_T1[,c("AES_C1",	"AES_C3",	"AES_C4",	
                       "AES_C5",	"AES_B6",	"AES_E7",	
                       "AES_C8",	"AES_B9",	"AES_B10",
                       "AES_C11",	"AES_B12",	"AES_C13",
                       "AES_E14",	"AES_O15",	"AES_C16",
                       "AES_O17",	"AES_O18")])

AES_T2 <- AES[AES$timepoint %in% c("2"), ]
psych::alpha(AES_T2[,c("AES_C1",	"AES_C3",	"AES_C4",	
                       "AES_C5",	"AES_B6",	"AES_E7",	
                       "AES_C8",	"AES_B9",	"AES_B10",
                       "AES_C11",	"AES_B12",	"AES_C13",
                       "AES_E14",	"AES_O15",	"AES_C16",
                       "AES_O17",	"AES_O18")])


#internal reliability test SDQ
#reverse coding item: SDQ_Q7, SDQ_Q21, SDQ_Q25, SDQ_Q11, SDQ_Q14
#subscale: Emotional problems scale (3,8,13,16,24),
#          Conduct problems Scale (5,7,12,18,22), 
#          Hyperactivity scale (2,10,15,21,25),
#          Peer problems scale (6,11,14,19,23), 
#          Prosocial scale (1,4,9,17,20)
#Internalising: sum of the emotional and peer problems scales (3,8,13,16,24,6,11,14,19,23)
#Externalising: sum of the conduct and hyperactivity scales (5,7,12,18,22,2,10,15,21,25)
#check SDQEnglishUK4-17scoring-1.PDF for more information 
SDQ_T0 <- SDQ[SDQ$timepoint %in% c("0"), ]
psych::alpha(SDQ_T0[,c("SDQ_Q1","SDQ_Q2","SDQ_Q3",
                       "SDQ_Q4","SDQ_Q5","SDQ_Q6",
                       "SDQ_Q7","SDQ_Q8","SDQ_Q9",
                       "SDQ_Q10","SDQ_Q11","SDQ_Q12",
                       "SDQ_Q13","SDQ_Q14","SDQ_Q15",
                       "SDQ_Q16","SDQ_Q17","SDQ_Q18",
                       "SDQ_Q19","SDQ_Q20","SDQ_Q21",
                       "SDQ_Q22","SDQ_Q23","SDQ_Q24","SDQ_Q25")])

SDQ_T1 <- SDQ[SDQ$timepoint %in% c("1"), ]
psych::alpha(SDQ_T1[,c("SDQ_Q1","SDQ_Q2","SDQ_Q3",
                       "SDQ_Q4","SDQ_Q5","SDQ_Q6",
                       "SDQ_Q7","SDQ_Q8","SDQ_Q9",
                       "SDQ_Q10","SDQ_Q11","SDQ_Q12",
                       "SDQ_Q13","SDQ_Q14","SDQ_Q15",
                       "SDQ_Q16","SDQ_Q17","SDQ_Q18",
                       "SDQ_Q19","SDQ_Q20","SDQ_Q21",
                       "SDQ_Q22","SDQ_Q23","SDQ_Q24","SDQ_Q25")])

SDQ_T2 <- SDQ[SDQ$timepoint %in% c("2"), ]
psych::alpha(SDQ_T2[,c("SDQ_Q1","SDQ_Q2","SDQ_Q3",
                       "SDQ_Q4","SDQ_Q5","SDQ_Q6",
                       "SDQ_Q7","SDQ_Q8","SDQ_Q9",
                       "SDQ_Q10","SDQ_Q11","SDQ_Q12",
                       "SDQ_Q13","SDQ_Q14","SDQ_Q15",
                       "SDQ_Q16","SDQ_Q17","SDQ_Q18",
                       "SDQ_Q19","SDQ_Q20","SDQ_Q21",
                       "SDQ_Q22","SDQ_Q23","SDQ_Q24","SDQ_Q25")])

SDQ_internalising_T0 <- SDQ_internalising[SDQ_internalising$timepoint %in% c("0"), ]
psych::alpha(SDQ_internalising_T0[,c("SDQ_Q1","SDQ_Q6","SDQ_Q8","SDQ_Q11","SDQ_Q13","SDQ_Q14",
                                     "SDQ_Q16","SDQ_Q19","SDQ_Q23","SDQ_Q24")])

SDQ_internalising_T1 <- SDQ_internalising[SDQ_internalising$timepoint %in% c("1"), ]
psych::alpha(SDQ_internalising_T1[,c("SDQ_Q1","SDQ_Q6","SDQ_Q8","SDQ_Q11","SDQ_Q13","SDQ_Q14",
                                     "SDQ_Q16","SDQ_Q19","SDQ_Q23","SDQ_Q24")])

SDQ_internalising_T2 <- SDQ_internalising[SDQ_internalising$timepoint %in% c("2"), ]
psych::alpha(SDQ_internalising_T2[,c("SDQ_Q1","SDQ_Q6","SDQ_Q8","SDQ_Q11","SDQ_Q13","SDQ_Q14",
                                     "SDQ_Q16","SDQ_Q19","SDQ_Q23","SDQ_Q24")])

SDQ_externalising_T0 <- SDQ_externalising[SDQ_externalising$timepoint %in% c("0"), ]
psych::alpha(SDQ_externalising_T0[,c("SDQ_Q2","SDQ_Q5","SDQ_Q7","SDQ_Q10","SDQ_Q12","SDQ_Q15",
                                     "SDQ_Q18","SDQ_Q21","SDQ_Q22","SDQ_Q25")])

SDQ_externalising_T1 <- SDQ_externalising[SDQ_externalising$timepoint %in% c("1"), ]
psych::alpha(SDQ_externalising_T1[,c("SDQ_Q2","SDQ_Q5","SDQ_Q7","SDQ_Q10","SDQ_Q12","SDQ_Q15",
                                     "SDQ_Q18","SDQ_Q21","SDQ_Q22","SDQ_Q25")])

SDQ_externalising_T2 <- SDQ_externalising[SDQ_externalising$timepoint %in% c("2"), ]
psych::alpha(SDQ_externalising_T2[,c("SDQ_Q2","SDQ_Q5","SDQ_Q7","SDQ_Q10","SDQ_Q12","SDQ_Q15",
                                     "SDQ_Q18","SDQ_Q21","SDQ_Q22","SDQ_Q25")])


#internal reliability test CASI 
#CASI ADHD: CASI_ADHD_1 - CASI_ADHD_18
#CASI MDD: CASI_Depr_1 - CASI_Depr_7 and CASI_Depr2_1 - CASI_Depr2_7
#CASI seperation anxiety: CASI_SepAnx_1 - CASI_SepAnx_9
#CASI social phobia: CASI_SP_1 - CASI_SP_5
CASI_ADHD_T0 <- CASI[CASI$timepoint %in% c("0"), ]
psych::alpha(CASI_ADHD_T0[,c("CASI_ADHD_1",	"CASI_ADHD_2",	"CASI_ADHD_3",	
                       "CASI_ADHD_4",	"CASI_ADHD_5",	"CASI_ADHD_6",	
                       "CASI_ADHD_7",	"CASI_ADHD_8",	"CASI_ADHD_9",
                       "CASI_ADHD_10",	"CASI_ADHD_11",	"CASI_ADHD_12a","CASI_ADHD_12b",
                       "CASI_ADHD_13",	"CASI_ADHD_14",	"CASI_ADHD_15",
                       "CASI_ADHD_16",	"CASI_ADHD_17", "CASI_ADHD_18")])

CASI_ADHD_T1 <- CASI[CASI$timepoint %in% c("1"), ]
psych::alpha(CASI_ADHD_T1[,c("CASI_ADHD_1",	"CASI_ADHD_2",	"CASI_ADHD_3",	
                             "CASI_ADHD_4",	"CASI_ADHD_5",	"CASI_ADHD_6",	
                             "CASI_ADHD_7",	"CASI_ADHD_8",	"CASI_ADHD_9",
                             "CASI_ADHD_10",	"CASI_ADHD_11",	"CASI_ADHD_12a","CASI_ADHD_12b",
                             "CASI_ADHD_13",	"CASI_ADHD_14",	"CASI_ADHD_15",
                             "CASI_ADHD_16",	"CASI_ADHD_17", "CASI_ADHD_18")])

CASI_ADHD_T2 <- CASI[CASI$timepoint %in% c("2"), ]
psych::alpha(CASI_ADHD_T2[,c("CASI_ADHD_1",	"CASI_ADHD_2",	"CASI_ADHD_3",	
                             "CASI_ADHD_4",	"CASI_ADHD_5",	"CASI_ADHD_6",	
                             "CASI_ADHD_7",	"CASI_ADHD_8",	"CASI_ADHD_9",
                             "CASI_ADHD_10",	"CASI_ADHD_11",	"CASI_ADHD_12a","CASI_ADHD_12b",
                             "CASI_ADHD_13",	"CASI_ADHD_14",	"CASI_ADHD_15",
                             "CASI_ADHD_16",	"CASI_ADHD_17", "CASI_ADHD_18")])

CASI_MDD_T0 <- CASI[CASI$timepoint %in% c("0"), ]
psych::alpha(CASI_MDD_T0[,c("CASI_Depr_1",	"CASI_Depr_2",	"CASI_Depr_3",	
                             "CASI_Depr_4",	"CASI_Depr_5",	"CASI_Depr_6", "CASI_Depr_7",	
                             "CASI_Depr2_1",	"CASI_Depr2_2", "CASI_Depr2_3",
                             "CASI_Depr2_4",	"CASI_Depr2_5","CASI_Depr2_6", "CASI_Depr2_7")])

CASI_MDD_T1 <- CASI[CASI$timepoint %in% c("1"), ]
psych::alpha(CASI_MDD_T1[,c("CASI_Depr_1",	"CASI_Depr_2",	"CASI_Depr_3",	
                            "CASI_Depr_4",	"CASI_Depr_5",	"CASI_Depr_6", "CASI_Depr_7",	
                            "CASI_Depr2_1",	"CASI_Depr2_2", "CASI_Depr2_3",
                            "CASI_Depr2_4",	"CASI_Depr2_5","CASI_Depr2_6", "CASI_Depr2_7")])

CASI_MDD_T2 <- CASI[CASI$timepoint %in% c("2"), ]
psych::alpha(CASI_MDD_T1[,c("CASI_Depr_1",	"CASI_Depr_2",	"CASI_Depr_3",	
                            "CASI_Depr_4",	"CASI_Depr_5",	"CASI_Depr_6", "CASI_Depr_7",	
                            "CASI_Depr2_1",	"CASI_Depr2_2", "CASI_Depr2_3",
                            "CASI_Depr2_4",	"CASI_Depr2_5","CASI_Depr2_6", "CASI_Depr2_7")])

CASI_sepanx_T0 <- CASI[CASI$timepoint %in% c("0"), ]
psych::alpha(CASI_sepanx_T0[,c("CASI_SepAnx_1",	"CASI_SepAnx_2",	"CASI_SepAnx_3",	
                             "CASI_SepAnx_4",	"CASI_SepAnx_5",	"CASI_SepAnx_6",	
                             "CASI_SepAnx_7",	"CASI_SepAnx_8",	"CASI_SepAnx_9")])

CASI_sepanx_T1 <- CASI[CASI$timepoint %in% c("1"), ]
psych::alpha(CASI_sepanx_T1[,c("CASI_SepAnx_1",	"CASI_SepAnx_2",	"CASI_SepAnx_3",	
                               "CASI_SepAnx_4",	"CASI_SepAnx_5",	"CASI_SepAnx_6",	
                               "CASI_SepAnx_7",	"CASI_SepAnx_8",	"CASI_SepAnx_9")])

CASI_sepanx_T2 <- CASI[CASI$timepoint %in% c("2"), ]
psych::alpha(CASI_sepanx_T2[,c("CASI_SepAnx_1",	"CASI_SepAnx_2",	"CASI_SepAnx_3",	
                               "CASI_SepAnx_4",	"CASI_SepAnx_5",	"CASI_SepAnx_6",	
                               "CASI_SepAnx_7",	"CASI_SepAnx_8",	"CASI_SepAnx_9")])

CASI_socphob_T0 <- CASI[CASI$timepoint %in% c("0"), ]
psych::alpha(CASI_socphob_T0[,c("CASI_SP_1",	"CASI_SP_2",	"CASI_SP_3",	
                               "CASI_SP_4",	"CASI_SP_5")])

CASI_socphob_T1 <- CASI[CASI$timepoint %in% c("1"), ]
psych::alpha(CASI_socphob_T1[,c("CASI_SP_1",	"CASI_SP_2",	"CASI_SP_3",	
                                "CASI_SP_4",	"CASI_SP_5")])

CASI_socphob_T2 <- CASI[CASI$timepoint %in% c("2"), ]
psych::alpha(CASI_socphob_T2[,c("CASI_SP_1",	"CASI_SP_2",	"CASI_SP_3",	
                                "CASI_SP_4",	"CASI_SP_5")])
