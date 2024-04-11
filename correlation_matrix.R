setwd("/Users/zoeli/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/training study/mixed model")
library("Hmisc")
library("corrplot")
library("PerformanceAnalytics")
library(tidyverse)
library(dplyr)
library(dbplyr)
library(corrr)
require(corrplot)

#load file
data.imp <- read.csv("training_data.csv")
data_baseline <- data.imp[data.imp$timepoint %in% c("0"), ]
data_baseline <- data_baseline[c('EFFactor_T0', 'academic_score', 'creativity', 'wasi',
                                 'UG_Offer_Accept', 'DG_Coins_Given', 'tot_delay_perc',
                                 'externalising_factor', 'internalising_factor', 
                                 'HavardOxford_Cortl', 'FPN', 'CON', 'CorticalThickness', 
                                 'Rputa.frontal_FA', 'Rputa.frontal_MD')]
data_baseline <- data_baseline %>% replace(.=="NULL", NA)

names(data_baseline)[names(data_baseline) == "EFFactor_T0"] <- "Cognitive Control"
names(data_baseline)[names(data_baseline) == "academic_score"] <- "Academic Performance"
names(data_baseline)[names(data_baseline) == "creativity"] <- "Creativity"
names(data_baseline)[names(data_baseline) == "wasi"] <- "WASI"
names(data_baseline)[names(data_baseline) == "UG_Offer_Accept"] <- "UG Offer Accept"
names(data_baseline)[names(data_baseline) == "DG_Coins_Given"] <- "DG Coins Given"
names(data_baseline)[names(data_baseline) == "tot_delay_perc"] <- "Percentage Total Delay"
names(data_baseline)[names(data_baseline) == "externalising_factor"] <- "Externalising Factor"
names(data_baseline)[names(data_baseline) == "internalising_factor"] <- "Internalising Factor"
names(data_baseline)[names(data_baseline) == "HavardOxford_Cortl"] <- "rIFG Activation"
names(data_baseline)[names(data_baseline) == "CorticalThickness"] <- "rIFG Cortical Thickness"
names(data_baseline)[names(data_baseline) == "FPN"] <- "FPN connection"
names(data_baseline)[names(data_baseline) == "CON"] <- "CON connection"
names(data_baseline)[names(data_baseline) == "Rputa.frontal_FA"] <- "Fronto-Putamen fractional anisotropy"
names(data_baseline)[names(data_baseline) == "Rputa.frontal_MD"] <- "Fronto-Putamen mean difusivity"






res <- cor(data_baseline[,1:15], use = "complete.obs")[1,1:15, drop=FALSE]
testRes = cor.mtest(data_baseline, conf.level = 0.95)
p = testRes$p[1,1:15]
p <- matrix(p,nrow=1,ncol=15,byrow=TRUE)
colnames(p) <- c('Cognitive Control', 'Academic Performance', 'Creativity', 'WASI',
                  'UG Offer Accept', 'DG Coins Given', 'Percentage Total Delay',
                  'Externalising Factor', 'Internalising Factor', 
                  'rIFG Activation', 'FPN connection', 'CON connection', 'rIFG Cortical Thickness', 
                  'Rputa.frontal FA', 'Rputa.frontal MD')
rownames(p) <-c('Cognitive Control')


png("corr_matrix_1.png", width = 3000,height = 1000)
corrplot(cor(data_baseline[,1:15], use = "complete.obs")[1,1:15, drop=FALSE], method = 'square', 
         tl.col = 'black', tl.srt = 70, tl.cex = 2, addCoef.col = 'black',number.cex = 2.5, 
         #p.mat = p, sig.level = c(0.001, 0.01, 0.05), pch.cex = 2,
         insig = 'label_sig', pch.col = 'grey20', 
         col = COL2('RdYlBu', 10), cl.pos = 'n',
         mar = c(0,0,0,3))
colorlegend(colbar = COL2('RdYlBu', 10), -1:1, col = 'black', align = 'c', cex=2.5,
            xlim = c(0.5, 15.5), ylim = c(-0.5, 0), vertical = FALSE,
           )
dev.off()





chart.Correlation(data_baseline, histogram=TRUE, pch=19)

corrplot(res, p.mat = res2$P, method = 'square', type="lower", order="FPC", tl.col = 'black', tl.srt = 45,
         sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9,
         insig = 'label_sig', pch.col = 'grey20', col = COL2('RdYlBu', 10))
