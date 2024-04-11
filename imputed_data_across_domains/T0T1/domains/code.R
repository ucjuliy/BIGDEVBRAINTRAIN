#melt/reshape data
setwd("~/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/training study data/mixed model/imputed_data_across_domains/T0T1/domains")



T0T1_academic_score <- read.csv("T0T1_academicscore.csv")
T0T1_creativity <- read.csv("T0T1_creativity.csv")
T0T1_cogflex_t <- read.csv("T0T1_cogflex_t.csv")
T0T1_CON <- read.csv("T0T1_CON.csv")
T0T1_corsi_max_wm_t <- read.csv("T0T1_corsi_max_wm_t.csv")
T0T1_CorticalThickness <- read.csv("T0T1_CorticalThickness.csv")
T0T1_DG_Coins_Given <- read.csv("T0T1_DG_Coins_Given.csv")
T0T1_DG_UG_Diff <- read.csv("T0T1_DG_UG_Diff.csv")
T0T1_DMN <- read.csv("T0T1_DMN.csv")
T0T1_EF_I_error <- read.csv("T0T1_EF_I_error.csv")
T0T1_EF_M_error <- read.csv("T0T1_EF_M_error.csv")
T0T1_EFRTFACTOR <- read.csv("T0T1_EFRTFACTOR.csv")
T0T1_externalising_factor <- read.csv("T0T1_externalising_factor.csv")
T0T1_FPN <- read.csv("T0T1_FPN.csv")
T0T1_HavardOxford_Cortl <- read.csv("T0T1_HavardOxford_Cortl.csv")
T0T1_internalising_factor <- read.csv("T0T1_internalising_factor.csv")
T0T1_PBI_t <- read.csv("T0T1_PBI_t.csv")
T0T1_Rcaud.frontal_FA <- read.csv("T0T1_Rcaud.frontal_FA.csv")
T0T1_Rcaud.frontal_MD <- read.csv("T0T1_Rcaud.frontal_MD.csv")
T0T1_Rcaud.frontal_RD <- read.csv("T0T1_Rcaud.frontal_RD.csv")
T0T1_REzssrtnr_t <- read.csv("T0T1_REzssrtnr_t.csv")
T0T1_Rputa.frontal_FA <- read.csv("T0T1_Rputa.frontal_FA.csv")
T0T1_Rputa.frontal_MD <- read.csv("T0T1_Rputa.frontal_MD.csv")
T0T1_Rputa.frontal_RD <- read.csv("T0T1_Rputa.frontal_RD.csv")
T0T1_tot_delay_perc <- read.csv("T0T1_tot_delay_perc.csv")
T0T1_UG_offer_accept <- read.csv("T0T1_UG_offer_accept.csv")

T0T1_prob_stopsig <- read.csv("T0T1_prob_stopsig.csv")
T0T1_prob_stopsig <- reshape(T0T1_prob_stopsig, idvar = "id", timevar = "timepoint", direction = "wide")
write.csv(T0T1_prob_stopsig, "T0T1_prob_stopsig.csv")

change_T0T1 <- read.csv("change_T0T1.csv")
change_T0T1 <- merge(change_T0T1, T0T1_academic_score, by = c("id"),
                       all.x=TRUE, all.y = TRUE)
change_T0T1 <- merge(change_T0T1, T0T1_creativity, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
change_T0T1 <- merge(change_T0T1, T0T1_cogflex_t, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
change_T0T1 <- merge(change_T0T1, T0T1_CON, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
change_T0T1 <- merge(change_T0T1, T0T1_corsi_max_wm_t, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
change_T0T1 <- merge(change_T0T1, T0T1_CorticalThickness, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
change_T0T1 <- merge(change_T0T1, T0T1_DG_Coins_Given, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
change_T0T1 <- merge(change_T0T1, T0T1_DG_UG_Diff, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
change_T0T1 <- merge(change_T0T1, T0T1_DMN, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
change_T0T1 <- merge(change_T0T1, T0T1_EF_I_error, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
change_T0T1 <- merge(change_T0T1, T0T1_EF_M_error, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
change_T0T1 <- merge(change_T0T1, T0T1_EFRTFACTOR, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
change_T0T1 <- merge(change_T0T1, T0T1_externalising_factor, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
change_T0T1 <- merge(change_T0T1, T0T1_FPN, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
change_T0T1 <- merge(change_T0T1, T0T1_HavardOxford_Cortl, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
change_T0T1 <- merge(change_T0T1, T0T1_internalising_factor, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
change_T0T1 <- merge(change_T0T1, T0T1_PBI_t, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
change_T0T1 <- merge(change_T0T1, T0T1_Rcaud.frontal_FA, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
change_T0T1 <- merge(change_T0T1, T0T1_Rcaud.frontal_MD, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
change_T0T1 <- merge(change_T0T1, T0T1_Rcaud.frontal_RD, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
change_T0T1 <- merge(change_T0T1, T0T1_REzssrtnr_t, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
change_T0T1 <- merge(change_T0T1, T0T1_Rputa.frontal_FA, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
change_T0T1 <- merge(change_T0T1, T0T1_Rputa.frontal_MD, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
change_T0T1 <- merge(change_T0T1, T0T1_Rputa.frontal_RD, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
change_T0T1 <- merge(change_T0T1, T0T1_tot_delay_perc, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
change_T0T1 <- merge(change_T0T1, T0T1_UG_offer_accept, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
change_T0T1 <- merge(change_T0T1, T0T1_fssrt_t, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
write.csv(change_T0T1, "change_T0T1.csv")

setwd("~/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/training study data/mixed model/imputed_data_across_domains/T0T1")
change_T0T1 <- read.csv("change_T0T1.csv")
change_T0T1 <- merge(change_T0T1, T0T1_prob_stopsig, by = c("id"),
                     all.x=TRUE, all.y = TRUE)
write.csv(change_T0T1, "change_T0T1.csv")

