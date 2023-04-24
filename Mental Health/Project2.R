# Load R packages
library(dplyr)
library(foreign)
library(ggplot2)
library(lme4)
library(performance)
library(Matrix)
library(sjPlot)
library(table1)
library(lavaan)
library(semPlot) 

#NB: Data imputation was performed in another script (imputation_Grace.R)

# Load data
fulldata <- "/Users/Grace/Desktop/PhD/Rotation2/mentalhealth_imputed.csv" 
trainingdata <- read.csv(fulldata, stringsAsFactors=FALSE, na.strings=c("","NA")) 


analysis_a <- "/Users/grace/Desktop/main_a_covariates.csv" 
trainingdata2 <- read.csv(analysis_a, stringsAsFactors=FALSE, na.strings=c("","NA")) 
partialdata <- "/Users/grace/Desktop/TrainingData.csv" 
trainingdata3 <- read.csv(partialdata, stringsAsFactors=FALSE, na.strings=c("","NA")) 



# Check the variable type 
str(analysis_a)
trainingdata$Gender <- as.factor(trainingdata$Gender)
trainingdata$School <- as.factor(trainingdata$School)
trainingdata$ID <- as.factor(trainingdata$ID)
trainingdata$cov <- as.factor(trainingdata$cov)
trainingdata$Group <- as.factor(trainingdata$Group)

trainingdata$timepoint <- as.factor(trainingdata$timepoint)
trainingdata$Test.Condition.Belief <- as.factor(trainingdata$Test.Condition.Belief)

trainingdata2$Group <- as.factor(trainingdata2$Group)
trainingdata2$Group <- as.numeric(trainingdata2$Group)


# Merge 1 & 1_2 timepoint 
trainingdata2$timepoint[trainingdata2$timepoint==0] <- 0  
trainingdata2$timepoint[trainingdata2$timepoint==1] <- 1  
trainingdata2$timepoint[trainingdata2$timepoint=='1_2'] <- 1  
trainingdata2$timepoint[trainingdata2$timepoint==2] <- 2  


# Descriptive analyses (& testing data import has worked) 

mean_age0 <- mean(trainingdata$Age_frac_T0, na.rm = TRUE)
mean_age1 <- mean(trainingdata$Age_frac_T1, na.rm = TRUE)
mean_age2 <- mean(trainingdata$Age_frac_T2, na.rm = TRUE)
mean_training_sessions <- mean(trainingdata$TotalSessions, na.rm = TRUE)
xtabs(~ Gender, data=trainingdata)
xtabs(~ timepoint, data=trainingdata2)
xtabs(~ Group, data=trainingdata)
xtabs(~ Group, data=trainingdata2)
mean_SES <- mean(trainingdata$SES, na.rm = TRUE)
xtabs(~ Gender + Group, data=trainingdata)
bar <- xtabs(~ TotalSessions + Group, data=trainingdata)
xtabs(~ timepoint, data=trainingdata2)


#Descriptive boxplots 

#Total Sessions
bar <- ggplot(trainingdata, aes(x=TotalSessions)) + geom_bar()
bar

#Total Sessions between groups 
g1 <- ggplot(trainingdata, aes(x=TotalSessions, y=Group)) + 
  geom_boxplot() + coord_flip()
g1


#SES between groups 
g1 <- ggplot(trainingdata, aes(x=Group, y=SES)) + 
  geom_boxplot(aes(group = Group)) 
g1


#Age between groups at different timepoints 
g1 <- ggplot(trainingdata, aes(x=Age_frac_T0, y=Group)) + 
  geom_boxplot() + coord_flip()
g1

g1 <- ggplot(trainingdata, aes(x=Age_frac_T1, y=Group)) + 
  geom_boxplot() + coord_flip()
g1

g1 <- ggplot(trainingdata, aes(x=Age_frac_T2, y=Group)) + 
  geom_boxplot() + coord_flip()
g1



#Descriptive Table 
table1(~ Age_frac_T0 + Gender + Group + TotalSessions, data=trainingdata, overall="Total")






# Box plots of mental health outcomes 

# CASI
### Across CASI the scoring is as follows:
# "Never" = "0",
# "Sometimes" = "1",
# "Often" = "2",
# "Very Often" = "3"

# SDQ
# General scoring is:                            
# "Not true" = "0",
# "Somewhat true" = "1",
# "Certainly true" = "2"
# *except* for items SDQ_7, SDQ_11, SDQ_14, SDQ_21, SDQ_25 (these are reverse coded)
# *and* question SDQ_26 (and subquestions) and the 'SDQ impact supplement' which is scored differently according to the
# following:
# "Not at all" = "0",
# "Only a little" = "0",
# "A medium amount" = "1"
# "A great deal" = "2"


#AES
### Across AES the scoring is as follows:
# "Not at all" = "1",
# "Slightly" = "2",
# "Somewhat" = "3",
# "A lot" = "4"



#Boxplots for CASI, SDQ and AES variables (timepoint & Group)

# CASI  - ADHD hyp 
# Boxplot by timepoint & mean points 


g2 <- ggplot(trainingdata2, aes(x=adhd_hyp_casi, y=timepoint)) + 
  geom_boxplot() + coord_flip()
g2
g2 + stat_summary(fun=mean, geom="point", shape=23, size=4) + coord_flip()


# Timepoint & Group

p <- ggplot(trainingdata2, aes(x=adhd_in_casi, y=timepoint, fill=Group)) +
  geom_boxplot(position=position_dodge(1)) + coord_flip()
p


# CASI  - ADHD in 
# Boxplot by timepoint & mean points

g2 <- ggplot(trainingdata2, aes(x=adhd_in_casi, y=timepoint)) + 
  geom_boxplot() + coord_flip()
g2
g2 + stat_summary(fun=mean, geom="point", shape=23, size=4) + coord_flip()


# Timepoint & Group

p <- ggplot(trainingdata2, aes(x=adhd_hyp_casi, y=timepoint, fill=Group)) +
  geom_boxplot(position=position_dodge(1)) + coord_flip()
p


# CASI  - ADHD total
# Boxplot by timepoint & mean points

g2 <- ggplot(trainingdata2, aes(x=adhd_tot_casi, y=timepoint)) + 
  geom_boxplot() + coord_flip()
g2 
g2 + stat_summary(fun=mean, geom="point", shape=23, size=4) + coord_flip()

# Timepoint & Group

p <- ggplot(trainingdata2, aes(x=adhd_tot_casi, y=timepoint, fill=Group)) +
  geom_boxplot(position=position_dodge(1)) + coord_flip()
p


# CASI  - CD
# Boxplot by timepoint & mean points

g2 <- ggplot(trainingdata2, aes(x=cd_casi, y=timepoint)) + 
  geom_boxplot() + coord_flip()
g2 
g2 + stat_summary(fun=mean, geom="point", shape=23, size=4) + coord_flip()


# Timepoint & Group

p <- ggplot(trainingdata2, aes(x=cd_casi, y=timepoint, fill=Group)) +
  geom_boxplot(position=position_dodge(1)) + coord_flip()
p


# CASI  - GAD
# Boxplot by timepoint & mean points

g2 <- ggplot(trainingdata2, aes(x=gad_casi, y=timepoint)) + 
  geom_boxplot() + coord_flip()
g2 
g2 + stat_summary(fun=mean, geom="point", shape=23, size=4) + coord_flip()


# Timepoint & Group

p <- ggplot(trainingdata2, aes(x=gad_casi, y=timepoint, fill=Group)) +
  geom_boxplot(position=position_dodge(1)) + coord_flip()
p


# CASI  - MDD1 
# Boxplot by timepoint & mean points

g2 <- ggplot(trainingdata2, aes(x=mdd_casi1, y=timepoint)) + 
  geom_boxplot() + coord_flip()
g2 
g2 + stat_summary(fun=mean, geom="point", shape=23, size=4) + coord_flip()


# Timepoint & Group

p <- ggplot(trainingdata2, aes(x=mdd_casi1, y=timepoint, fill=Group)) +
  geom_boxplot(position=position_dodge(1)) + coord_flip()
p


# CASI  - MDD2
# Boxplot by timepoint & mean points

g2 <- ggplot(trainingdata2, aes(x=mdd_casi2, y=timepoint)) + 
  geom_boxplot() + coord_flip()
g2 
g2 + stat_summary(fun=mean, geom="point", shape=23, size=4) + coord_flip()

# Timepoint & Group

p <- ggplot(trainingdata2, aes(x=mdd_casi2, y=timepoint, fill=Group)) +
  geom_boxplot(position=position_dodge(1)) + coord_flip()
p


# CASI  - MDD total
# Boxplot by timepoint & mean points

g2 <- ggplot(trainingdata2, aes(x=mdd_total_casi, y=timepoint)) + 
  geom_boxplot() + coord_flip()
g2 
g2 + stat_summary(fun=mean, geom="point", shape=23, size=4) + coord_flip()


# Timepoint & Group

p <- ggplot(trainingdata2, aes(x=mdd_total_casi, y=timepoint, fill=Group)) +
  geom_boxplot(position=position_dodge(1)) + coord_flip()
p


# CASI  - separation anxiety
# Boxplot by timepoint & mean points


g2 <- ggplot(trainingdata2, aes(x=sepanx_casi, y=timepoint)) + 
  geom_boxplot() + coord_flip()
g2 
g2 + stat_summary(fun=mean, geom="point", shape=23, size=4) + coord_flip()

# Timepoint & Group

p <- ggplot(trainingdata2, aes(x=sepanx_casi, y=timepoint, fill=Group)) +
  geom_boxplot(position=position_dodge(1)) + coord_flip()
p


# CASI  - social phobia
# Boxplot by timepoint & mean points

g2 <- ggplot(trainingdata2, aes(x=socphob_casi, y=timepoint)) + 
  geom_boxplot() + coord_flip()
g2 
g2 + stat_summary(fun=mean, geom="point", shape=23, size=4) + coord_flip()


# Timepoint & Group

p <- ggplot(trainingdata2, aes(x=socphob_casi, y=timepoint, fill=Group)) +
  geom_boxplot(position=position_dodge(1)) + coord_flip()
p


# SDQ - emotional 
# Boxplot by timepoint & mean points

g2 <- ggplot(trainingdata2, aes(x=emo_sdq, y=timepoint)) + 
  geom_boxplot() + coord_flip()
g2 
g2 + stat_summary(fun=mean, geom="point", shape=23, size=4) + coord_flip()


# Timepoint & Group

p <- ggplot(trainingdata2, aes(x=emo_sdq, y=timepoint, fill=Group)) +
  geom_boxplot(position=position_dodge(1)) + coord_flip()
p



# SDQ - CD 
# Boxplot by timepoint & mean points 

g2 <- ggplot(trainingdata2, aes(x=cd_sdq, y=timepoint)) + 
  geom_boxplot() + coord_flip()
g2
g2 + stat_summary(fun=mean, geom="point", shape=23, size=4) + coord_flip()


# Timepoint & Group

p <- ggplot(trainingdata2, aes(x=cd_sdq, y=timepoint, fill=Group)) +
  geom_boxplot(position=position_dodge(1)) + coord_flip()
p


# SDQ - hyperactivity 
# Boxplot by timepoint & mean points 

g2 <- ggplot(trainingdata2, aes(x=hy_sdq, y=timepoint)) + 
  geom_boxplot() + coord_flip()
g2 
g2 + stat_summary(fun=mean, geom="point", shape=23, size=4) + coord_flip()


# Timepoint & Group

p <- ggplot(trainingdata2, aes(x=hy_sdq, y=timepoint, fill=Group)) +
  geom_boxplot(position=position_dodge(1)) + coord_flip()
p



# SDQ - peer 
# Boxplot by timepoint & mean points 

g2 <- ggplot(trainingdata2, aes(x=peer_sdq, y=timepoint)) + 
  geom_boxplot() + coord_flip()
g2 
g2 + stat_summary(fun=mean, geom="point", shape=23, size=4) + coord_flip()

# Timepoint & Group

p <- ggplot(trainingdata2, aes(x=peer_sdq, y=timepoint, fill=Group)) +
  geom_boxplot(position=position_dodge(1)) + coord_flip()
p


# SDQ - prosociability  
# Boxplot by timepoint & mean points 
g2 <- ggplot(trainingdata2, aes(x=pro_sdq, y=timepoint)) + 
  geom_boxplot() + coord_flip()
g2
g2 + stat_summary(fun=mean, geom="point", shape=23, size=4) + coord_flip()

# Timepoint & Group

p <- ggplot(trainingdata2, aes(x=pro_sdq, y=timepoint, fill=Group)) +
  geom_boxplot(position=position_dodge(1)) + coord_flip()
p



# SDQ - externalising   
# Boxplot by timepoint & mean points

g2 <- ggplot(trainingdata2, aes(x=ext_sdq, y=timepoint)) + 
  geom_boxplot() + coord_flip()
g2
g2 + stat_summary(fun=mean, geom="point", shape=23, size=4) + coord_flip()

# Timepoint & Group

p <- ggplot(trainingdata2, aes(x=ext_sdq, y=timepoint, fill=Group)) +
  geom_boxplot(position=position_dodge(1)) + coord_flip()
p



# SDQ - internalising  
# Boxplot by timepoint & mean points

g2 <- ggplot(trainingdata2, aes(x=int_sdq, y=timepoint)) + 
  geom_boxplot() + coord_flip()
g2
g2 + stat_summary(fun=mean, geom="point", shape=23, size=4) + coord_flip()


# Timepoint & Group

p <- ggplot(trainingdata2, aes(x=int_sdq, y=timepoint, fill=Group)) +
  geom_boxplot(position=position_dodge(1)) + coord_flip()
p


# SDQ - total
# Boxplot by timepoint & mean points

g2 <- ggplot(trainingdata2, aes(x=total_sdq, y=timepoint)) + 
  geom_boxplot() + coord_flip()
g2
g2 + stat_summary(fun=mean, geom="point", shape=23, size=4) + coord_flip()


# Timepoint & Group

p <- ggplot(trainingdata2, aes(x=total_sdq, y=timepoint, fill=Group)) +
  geom_boxplot(position=position_dodge(1)) + coord_flip()
p




# AES - cognitive 
# Boxplot by timepoint & mean points 


g2 <- ggplot(trainingdata2, aes(x=aes_cog, y=timepoint)) + 
  geom_boxplot() + coord_flip()
g2 
g2 + stat_summary(fun=mean, geom="point", shape=23, size=4) + coord_flip()


# Timepoint & Group

p <- ggplot(trainingdata2, aes(x=aes_cog, y=timepoint, fill=Group)) +
  geom_boxplot(position=position_dodge(1)) + coord_flip()
p


# AES - behavioural
# Boxplot by timepoint & mean points 

g2 <- ggplot(trainingdata2, aes(x=aes_beh, y=timepoint)) + 
  geom_boxplot() + coord_flip()
g2 
g2 + stat_summary(fun=mean, geom="point", shape=23, size=4) + coord_flip()


# Timepoint & Group

p <- ggplot(trainingdata2, aes(x=aes_beh, y=timepoint, fill=Group)) +
  geom_boxplot(position=position_dodge(1)) + coord_flip()
p


# AES - emotional 
# Boxplot by timepoint & mean points 


g2 <- ggplot(trainingdata2, aes(x=aes_emo, y=timepoint)) + 
  geom_boxplot() + coord_flip()
g2 
g2 + stat_summary(fun=mean, geom="point", shape=23, size=4) + coord_flip()

# Timepoint & Group

p <- ggplot(trainingdata2, aes(x=aes_emo, y=timepoint, fill=Group)) +
  geom_boxplot(position=position_dodge(1)) + coord_flip()
p


# AES - other  
# Boxplot by timepoint & mean points 

g2 <- ggplot(trainingdata2, aes(x=aes_other, y=timepoint)) + 
  geom_boxplot() + coord_flip()
g2 
g2 + stat_summary(fun=mean, geom="point", shape=23, size=4) + coord_flip()

# Timepoint & Group

p <- ggplot(trainingdata2, aes(x=aes_other, y=timepoint, fill=Group)) +
  geom_boxplot(position=position_dodge(1)) + coord_flip()
p


# AES - total   
# Boxplot by timepoint & mean points 
g2 <- ggplot(trainingdata2, aes(x=total_aes, y=timepoint)) + 
  geom_boxplot() + coord_flip()
g2
g2 + stat_summary(fun=mean, geom="point", shape=23, size=4) + coord_flip()

# Timepoint & Group

p <- ggplot(trainingdata2, aes(x=total_aes, y=timepoint, fill=Group)) +
  geom_boxplot(position=position_dodge(1)) + coord_flip()
p


# Effects of training (CASI, SDQ and AES questionnaire variables)


# adhd_in_casi - training effect
lmer1 <- lmer(adhd_in_casi ~ timepoint*Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(adhd_in_casi ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="ADHD Inattentive Type")


# adhd_hyp_casi - training effect
lmer1 <- lmer(adhd_hyp_casi ~ timepoint*Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(adhd_hyp_casi ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="ADHD Hyperactive Type")


# adhd_tot_casi - training effect
lmer1 <- lmer(adhd_tot_casi ~ timepoint*Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(adhd_tot_casi ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2)  
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="ADHD Total Score") 



# cd_casi - training effect
lmer1 <- lmer(cd_casi ~ timepoint*Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(cd_casi ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2)
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Conduct Disorder")



# gad_casi - training effect
lmer1 <- lmer(gad_casi ~ timepoint*Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(gad_casi ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Generalised Anxiety Disorder") 


# mdd_casi1 - training effect
lmer1 <- lmer(mdd_casi1 ~ timepoint*Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(mdd_casi1 ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Major Depressive Disorder I") 

# mdd_casi2 - training effect
lmer1 <- lmer(mdd_casi2 ~ timepoint*Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(mdd_casi2 ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2)
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Major Depressive Disorder II") 


# mdd_total_casi - training effect
lmer1 <- lmer(mdd_total_casi ~ timepoint*Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(mdd_total_casi ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Major Depressive Disorder Total") 


# sepanx_casi - training effect
lmer1 <- lmer(sepanx_casi ~ timepoint*Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(sepanx_casi ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Separation Anxiety") 


# socphob_casi - training effect
lmer1 <- lmer(socphob_casi ~ timepoint*Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(socphob_casi ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Social Phobia") 




# emo_sdq - training effect
lmer1 <- lmer(emo_sdq ~ timepoint*Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(emo_sdq ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Emotional Problems") 


# cd_sdq - training effect
lmer1 <- lmer(cd_sdq ~ timepoint*Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(cd_sdq ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Conduct Disorder") 


# hy_sdq - training effect
lmer1 <- lmer(hy_sdq ~ timepoint*Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(hy_sdq ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2)  
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Hyperactivity") 


# peer_sdq - training effect
lmer1 <- lmer(peer_sdq ~ timepoint*Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(peer_sdq ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Peer Problems") 

# pro_sdq - training effect
lmer1 <- lmer(pro_sdq ~ timepoint*Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(pro_sdq ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2)
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Prosociality")


# ext_sdq - training effect
lmer1 <- lmer(ext_sdq ~ timepoint*Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(ext_sdq ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Externalising Symptoms") 

# int_sdq - training effect
lmer1 <- lmer(int_sdq ~ timepoint*Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(int_sdq ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Internalising Symptoms") 

# SDQ total - training effect
lmer1 <- lmer(total_sdq ~ timepoint*Group + (1|ID) + TotalSessions + Age_frac_T0 + SES + Gender, data = trainingdata2, REML=FALSE)
summary(lmer2)
lmer2 <- lmer(total_sdq ~ timepoint + Group + (1|ID) + TotalSessions + Age_frac_T0 + SES + Gender, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Total SDQ Score") 



# aes_cog - training effect
lmer1 <- lmer(aes_cog ~ timepoint*Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(aes_cog ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Cognitive") 



# aes_beh - training effect
lmer1 <- lmer(aes_beh ~ timepoint*Group + (1|ID) + TotalSessions + Age_frac_T0 + Gender, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(aes_beh ~ timepoint + Group + (1|ID) + TotalSessions+ Age_frac_T0 + Gender, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2)
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Behavioural")


# aes_emo - training effect
lmer1 <- lmer(aes_emo ~ timepoint*Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(aes_emo ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2)  
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Emotional")



# aes_other - training effect
lmer1 <- lmer(aes_other ~ timepoint*Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(aes_other ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2)
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Other") 


# total_aes - training effect
lmer1 <- lmer(total_aes ~ timepoint*Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(total_aes ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Total Score")




#Time effects (CASI, SDQ and AES questionnaire variables)

# on adhd_tot_casi - main effect of time
lmer1 <- lmer(adhd_tot_casi ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(adhd_tot_casi ~ Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint"), title="ADHD Total") 


# on adhd_in_casi - main effect of time
lmer1 <- lmer(adhd_in_casi ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(adhd_in_casi ~ Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint"), title="ADHD Inattentive Type") 

# on adhd_hy_casi - main effect of time
lmer1 <- lmer(adhd_hyp_casi ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(adhd_hyp_casi ~ Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint"), title="ADHD Hyperactive Type") 

# on cd_casi - main effect of time
lmer1 <- lmer(cd_casi ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(cd_casi ~ Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint"), title="Conduct Disorder") 

# on gad_casi - main effect of time
lmer1 <- lmer(gad_casi ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(gad_casi ~ Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint"), title="Generalised Anxiety Disorder") 


# on mdd1_casi - main effect of time
lmer1 <- lmer(mdd_casi1 ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(mdd_casi1 ~ Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint"), title="Major Depressive Disorder I") 


# on mdd2_casi - main effect of time
lmer1 <- lmer(mdd_casi2 ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(mdd_casi2 ~ Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint"), title="Major Depressive Disorder II") 


# on mdd_total_casi - main effect of time
lmer1 <- lmer(mdd_total_casi ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(mdd_total_casi ~ Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint"), title="Major Depressive Disorder Total") 



# CASI Sep Anx - main effect of time
lmer1 <- lmer(sepanx_casi ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(sepanx_casi ~ Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint"), title="Separation Anxiety") 


# CASI SOC PHOB - main effect of time
lmer1 <- lmer(socphob_casi ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(socphob_casi ~ Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint"), title="Social Phobia") 


# SDQ Emo - main effect of time
lmer1 <- lmer(emo_sdq ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(emo_sdq ~ Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint"), title="Emotional Problems") 


# SDQ CD - main effect of time
lmer1 <- lmer(cd_sdq ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(cd_sdq ~ Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint"), title="Conduct Disorder")

# SDQ HY - main effect of time
lmer1 <- lmer(hy_sdq ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(hy_sdq ~ Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint"), title="Hyperactivity") 


# SDQ PEER - main effect of time
lmer1 <- lmer(peer_sdq ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(peer_sdq ~ Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint"), title="Peer Problems") 


# SDQ PRO - main effect of time
lmer1 <- lmer(pro_sdq ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(pro_sdq ~ Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2)
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint"), title="Prosociality")


# SDQ EXT - main effect of time
lmer1 <- lmer(ext_sdq ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(ext_sdq ~ Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint"), title="Externalising Symptoms") 


# SDQ INT - main effect of time
lmer1 <- lmer(int_sdq ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(int_sdq ~ Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint"), title="Internalising Symptoms")


# SDQ TOTAL - main effect of time
lmer1 <- lmer(total_sdq ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(total_sdq ~ Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint"), title="Total SDQ Score") 

# AES COG - main effect of time
lmer1 <- lmer(aes_cog ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(aes_cog ~ Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint"), title="Cognitive")


# AES BEH - main effect of time
lmer1 <- lmer(aes_beh ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(aes_beh ~ Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint"), title="Behavioural") 


# AES EMO - main effect of time
lmer1 <- lmer(aes_emo ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(aes_emo ~ Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint"), title="Emotional") 


# AES OTHER - main effect of time
lmer1 <- lmer(aes_other ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(aes_other ~ Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint"), title="Other") 



# AES TOTAL - main effect of time
lmer1 <- lmer(total_aes ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(total_aes ~ Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint"), title="Total AES Score") 






























# Factor analysis 

##Timepoint 0

#Tried fitting models using the imputed and non imputed data
##Overall comparing both did not change the model fit much at this timepoint 

#CFA - imputed data 
dat <- data.frame(matrix(ncol = 0, nrow = 233))
dat$t0_adhd_in_casi<-trainingdata$t0_adhd_in_casi
dat$t0_adhd_hyp_casi<-trainingdata$t0_adhd_hyp_casi
dat$t0_adhd_tot_casi<-trainingdata$t0_adhd_tot_casi
dat$t0_cd_casi<-trainingdata$t0_cd_casi
dat$t0_gad_casi<-trainingdata$t0_gad_casi
dat$t0_mdd_casi1<-trainingdata$t0_mdd_casi1
dat$t0_mdd_casi2<-trainingdata$t0_mdd_casi2
dat$t0_mdd_total_casi<-trainingdata$t0_mdd_total_casi
dat$t0_sepanx_casi<-trainingdata$t0_sepanx_casi
dat$t0_socphob_casi<-trainingdata$t0_socphob_casi
dat$t0_emo_sdq<-trainingdata$t0_emo_sdq
dat$t0_cd_sdq<-trainingdata$t0_cd_sdq
dat$t0_hy_sdq<-trainingdata$t0_hy_sdq
dat$t0_peer_sdq<-trainingdata$t0_peer_sdq
dat$t0_pro_sdq<-trainingdata$t0_pro_sdq
dat$t0_ext_sdq<-trainingdata$t0_ext_sdq
dat$t0_int_sdq<-trainingdata$t0_int_sdq
dat$t0_aes_cog<-trainingdata$t0_aes_cog
dat$t0_aes_beh<-trainingdata$t0_aes_beh
dat$t0_aes_emo<-trainingdata$t0_aes_emo
dat$t0_aes_other<-trainingdata$t0_aes_other
dat$t0_total_aes<-trainingdata$t0_total_aes


#CFA - non-imputed data 
dat0alt <- data.frame(matrix(ncol = 0, nrow = 852))
dat0alt$t0_adhd_in_casi<-trainingdata3$t0_adhd_in_casi
dat0alt$t0_adhd_hyp_casi<-trainingdata3$t0_adhd_hyp_casi
dat0alt$t0_adhd_tot_casi<-trainingdata3$t0_adhd_tot_casi
dat0alt$t0_cd_casi<-trainingdata3$t0_cd_casi
dat0alt$t0_gad_casi<-trainingdata3$t0_gad_casi
dat0alt$t0_mdd_casi1<-trainingdata3$t0_mdd_casi1
dat0alt$t0_mdd_casi2<-trainingdata3$t0_mdd_casi2
dat0alt$t0_mdd_total_casi<-trainingdata3$t0_mdd_total_casi
dat0alt$t0_sepanx_casi<-trainingdata3$t0_sepanx_casi
dat0alt$t0_socphob_casi<-trainingdata3$t0_socphob_casi
dat0alt$t0_emo_sdq<-trainingdata3$t0_emo_sdq
dat0alt$t0_cd_sdq<-trainingdata3$t0_cd_sdq
dat0alt$t0_hy_sdq<-trainingdata3$t0_hy_sdq
dat0alt$t0_peer_sdq<-trainingdata3$t0_peer_sdq
dat0alt$t0_pro_sdq<-trainingdata3$t0_pro_sdq
dat0alt$t0_ext_sdq<-trainingdata3$t0_ext_sdq
dat0alt$t0_int_sdq<-trainingdata3$t0_int_sdq



#CFA - non-imputed data 
dat0long <- data.frame(matrix(ncol = 0, nrow = 479))
dat0long$t0_adhd_in_casi<-trainingdata2$adhd_in_casi
dat0long$t0_adhd_hyp_casi<-trainingdata2$tadhd_hyp_casi
dat0long$t0_adhd_tot_casi<-trainingdata2$tadhd_tot_casi
dat0long$t0_cd_casi<-trainingdata2$cd_casi
dat0long$t0_gad_casi<-trainingdata2$gad_casi
dat0long$t0_mdd_casi1<-trainingdata2$mdd_casi1
dat0long$t0_mdd_casi2<-trainingdata2$mdd_casi2
dat0long$t0_mdd_total_casi<-trainingdata2$mdd_total_casi
dat0long$t0_sepanx_casi<-trainingdata2$sepanx_casi
dat0long$t0_socphob_casi<-trainingdata2$socphob_casi
dat0long$t0_emo_sdq<-trainingdata2$emo_sdq
dat0long$t0_cd_sdq<-trainingdata2$cd_sdq
dat0long$t0_hy_sdq<-trainingdata2$hy_sdq
dat0long$t0_peer_sdq<-trainingdata2$peer_sdq
dat0long$t0_pro_sdq<-trainingdata2$pro_sdq
dat0long$t0_ext_sdq<-trainingdata2$ext_sdq
dat0long$t0_int_sdq<-trainingdata2$int_sdq



#Model fit assessed as follows (have checked and this is the general consensus)
##Factor loadings on graph: >0.5 (preferably 0.6) 
##RMSEA <0.05 = close fit, <0.08 reasonable fit
##CFI: >0.95 
##TLI: >0.90


#Models tested: 
##Correlated-factors model
##Common manifestation model
##Bifactor model
##higher-order factor model 



#This model just used to test out the process 
#single factor - imputed data 

HS.model <- 'I=~ t0_adhd_in_casi + t0_adhd_hyp_casi + t0_adhd_tot_casi +
t0_cd_casi + t0_gad_casi + t0_mdd_casi2 + t0_mdd_casi1 +
t0_mdd_total_casi + t0_sepanx_casi + t0_socphob_casi + t0_emo_sdq + t0_cd_sdq +
t0_hy_sdq + t0_peer_sdq + t0_pro_sdq'


fit <- cfa(HS.model, data = dat0alt, missing='fiml')
summary(fit, fit.measures = TRUE)


semPaths(fit,"std")


#Does not fit 



#Specify a two factor - using all variables to look at factor loadings 
HS.model2 <- 'I=~ t0_gad_casi + t0_mdd_casi2 + t0_mdd_casi1 +
+ t0_mdd_total_casi + t0_sepanx_casi + t0_socphob_casi + t0_emo_sdq + t0_peer_sdq + t0_pro_sdq
E =~ t0_adhd_in_casi + t0_adhd_hyp_casi + t0_adhd_tot_casi + 
t0_hy_sdq + t0_cd_sdq + t0_cd_casi'


##Fit model 
fit1 <- cfa(HS.model2, data = dat)
summary(fit1, fit.measures = TRUE)

#Graphical representation type 1
semPaths(fit1, residuals=F,sizeMan=7,"std",
         posCol=c("skyblue4", "red"),
         #edge.color="skyblue4",
         edge.label.cex=1.2,layout="circle2")

#Graphical representation type 2
semPaths(fit1,"std")

#Does not fit 







# CORRELATED FACTORS MODELS

#Correlated factors model 1- imputed data 
HS.model3 <- 'E=~ t0_adhd_in_casi + t0_adhd_hyp_casi + t0_hy_sdq 
 I=~ t0_gad_casi + t0_mdd_total_casi + t0_socphob_casi + t0_sepanx_casi'

##Fit model 
fit2 <- cfa(HS.model3, data = dat)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")


#Does  not fit 



#Correlated factors model 1 - non-imputed data  
HS.model3 <- 'E=~ t0_adhd_in_casi + t0_adhd_hyp_casi + t0_hy_sdq 
 I=~ t0_gad_casi + t0_mdd_total_casi + t0_socphob_casi + t0_sepanx_casi'

##Fit model
fit2 <- cfa(HS.model3, data = dat0alt, missing = 'fiml')
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")


#Does  not fit 


#Correlated factors model 2 - imputed data 
## Using social aspects as a third higher order domain 
HS.model3 <- 'E=~ t0_adhd_in_casi + t0_adhd_hyp_casi + t0_hy_sdq 
 I=~ t0_gad_casi + t0_mdd_total_casi + t0_mdd_casi1
S=~ t0_sepanx_casi + t0_socphob_casi + t0_emo_sdq'

##Fit model
fit2 <- cfa(HS.model3, data = dat)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")


#Does  not fit 



#Correlated factors model 2 - non-imputed data 
## Using social aspects as a third higher order domain 
HS.model3 <- 'E=~ t0_adhd_in_casi + t0_adhd_hyp_casi + t0_hy_sdq 
 I=~ t0_gad_casi + t0_mdd_total_casi + t0_mdd_casi1
S=~ t0_sepanx_casi + t0_socphob_casi + t0_emo_sdq'

##Fit model
fit2 <- cfa(HS.model3, data = dat0alt, missing = 'fiml')
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")

#Does  not fit 





#Correlated factors model 3 - imputed data 
HS.model3 <- 'E=~ t0_adhd_in_casi + t0_adhd_hyp_casi + t0_hy_sdq 
 I=~ t0_gad_casi + t0_mdd_total_casi + t0_emo_sdq'

##Fit model
fit2 <- cfa(HS.model3, data = dat)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")


#RMSEA too high, not a good fit 




#Correlated factors model 3 - non-imputed data 
HS.model3 <- 'E=~ t0_adhd_in_casi + t0_adhd_hyp_casi + t0_hy_sdq 
 I=~ t0_gad_casi + t0_mdd_total_casi + t0_emo_sdq'

##Fit model
fit2 <- cfa(HS.model3, data = dat0alt, missing = 'fiml')
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")


#RMSEA too high, not a good fit 




##common manifestation using 4 separate domains 
HS.model4 <- 'I=~ t0_gad_casi + t0_mdd_total_casi 
E =~ t0_adhd_in_casi + t0_adhd_hyp_casi
D =~ t0_cd_sdq + t0_cd_casi  
S =~ t0_sepanx_casi'


##CFA
fit3 <- cfa(HS.model4, data = dat)
summary(fit3, fit.measures = TRUE)

#Graphical representation
semPaths(fit3, residuals=F,sizeMan=7,"std",
         posCol=c("skyblue4", "red"),
         #edge.color="skyblue4",
         edge.label.cex=1.2,layout="circle2")

semPaths(fit3,"std")


#RMSEA = 0.076
#CFI = 0.961
#TLI = 0.909

# Model fits well 


baseline1 <- measEq.syntax(configural.model = HS.model4,
                           data = dat, 
                           parameterization = "delta",
                           group = "session", missing = 'fiml',
                           group.equal = c( "loadings", "thresholds"))

model.baseline1 <- as.character(baseline1)
both1 <- cfa(model.baseline1, data = dat, group = 'session', missing = 'fiml')
summary(both1, fit.measures = TRUE, standardized = TRUE)








#  COMMON MANIFESTATION MODELS 

#Common manifestation model 1 - imputed data  
HS.model3 <- 'P=~ t0_gad_casi + t0_mdd_total_casi +
t0_mdd_casi1 +  t0_cd_casi + t0_adhd_in_casi + t0_adhd_hyp_casi + 
t0_int_sdq + t0_hy_sdq + t0_cd_sdq'

##Fit model
fit2 <- cfa(HS.model3, data = dat)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")


#Does  not fit 



#Common manifestation model 1 - non-imputed data  
HS.model3 <- 'P=~ t0_gad_casi + t0_mdd_total_casi +
t0_mdd_casi1 +  t0_cd_casi + t0_adhd_in_casi + t0_adhd_hyp_casi + 
t0_int_sdq + t0_hy_sdq + t0_cd_sdq'

##Fit model
fit2 <- cfa(HS.model3, data = dat0alt, missing = 'fiml' )
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")


#Does  not fit 




#Higher order factor model (using sum of SDQ int and ext variables) - imputed data 
HS.model3 <- 'E=~ t0_ext_sdq + t0_adhd_in_casi + t0_adhd_hyp_casi 
I=~ t0_int_sdq + t0_socphob_casi + t0_sepanx_casi + t0_mdd_total_casi + t0_gad_casi
P=~ E + I '

##Fit model
fit2 <- cfa(HS.model3, data = dat)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")



#RMSEA: 0.095
#CFI: 0.943
#TLI: 0.911
##Good fit 

#Higher order factor model (using sum of SDQ int and ext variables) - imputed data  
HS.model3 <- 'E=~ t0_ext_sdq + t0_adhd_in_casi + t0_adhd_hyp_casi 
I=~ t0_int_sdq + t0_socphob_casi + t0_sepanx_casi + t0_mdd_total_casi 
P=~ E + I '

#Fit model
fit2 <- cfa(HS.model3, data = dat0alt, missing = 'fiml')
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")



#RMSEA:0.056
#CFI: 0.984
#TLI: 0.972








##Higher order factor model - imputed data 

HS.model1 <- 'I=~ t0_gad_casi + t0_mdd_total_casi + t0_mdd_casi1
E =~ t0_adhd_hyp_casi +  t0_hy_sdq + t0_adhd_tot_casi 
S =~ t0_sepanx_casi + t0_socphob_casi + t0_emo_sdq 
P =~ I + E + S'

fit2 <- cfa(HS.model1, data = dat)
summary(fit2, fit.measures = TRUE, standardized = TRUE)

#Graphical representation 
semPaths(fit2,"std", shapeMan = "rectangle", nCharNodes = 7, sizeMan = 8, sizeMan2 = 5)

semPaths(fit2, "par", weighted = FALSE, nCharNodes = 7, shapeMan = "rectangle",
         sizeMan = 8, sizeMan2 = 5)



##Higher order factor model - non-imputed data  

HS.model1 <- 'I=~ t0_gad_casi + t0_mdd_total_casi + t0_mdd_casi1
E =~ t0_adhd_hyp_casi +  t0_hy_sdq + t0_adhd_tot_casi 
S =~ t0_sepanx_casi + t0_socphob_casi + t0_emo_sdq 
P =~ I + E + S'

fit2 <- cfa(HS.model1, data = dat0alt)
summary(fit2, fit.measures = TRUE, standardized = TRUE)

#Graphical representation 
semPaths(fit2,"std", shapeMan = "rectangle", nCharNodes = 7, sizeMan = 8, sizeMan2 = 5)






##Higher order factor model2 - imputed data 

HS.model20 <- 'I=~ t0_mdd_casi2 + t0_mdd_casi1 + t0_mdd_total_casi 
E =~ t0_adhd_in_casi + t0_adhd_hyp_casi + t0_adhd_tot_casi 
P =~ I + E'

fit20 <- cfa(HS.model20, data = dat)
summary(fit20, fit.measures = TRUE)

#Graphical representation 
semPaths(fit20,"std")





##Higher order factor model2 - imputed data 

HS.model20 <- 'I=~ t0_mdd_casi2 + t0_mdd_casi1 + t0_mdd_total_casi 
E =~ t0_adhd_in_casi + t0_adhd_hyp_casi + t0_adhd_tot_casi 
P =~ I + E'

fit20 <- cfa(HS.model20, data = dat)
summary(fit20, fit.measures = TRUE)

#Graphical representation 
semPaths(fit20,"std")



##Higher order factor model2 - non-imputed data 

HS.model2 <- 'I=~ t0_mdd_casi2 + t0_mdd_casi1 + t0_mdd_total_casi + t0_gad_casi
E =~ t0_adhd_in_casi + t0_adhd_hyp_casi + t0_adhd_tot_casi 
P =~ I + E'

fit2 <- cfa(HS.model2, data = dat0alt)
summary(fit2, fit.measures = TRUE)

#Graphical representation 
semPaths(fit2,"std")




#USE THIS ONE BELOW !!!!!!!!!!!!!

#~Correlated factors  - using int and ext - non imputed data 

HS.model3 <- 'E =~ t0_ext_sdq + t0_adhd_in_casi + t0_adhd_hyp_casi 
I =~ t0_int_sdq + t0_socphob_casi + t0_sepanx_casi + t0_mdd_total_casi'

##Fit model
fit2 <- cfa(HS.model3, data = dat0long)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")
predict(fit2)



baseline1 <- measEq.syntax(configural.model = HS.model3,
                           data = dat0alt, 
                           parameterization = "delta",
                           group = "session", missing = 'fiml',
                           group.equal = c( "loadings", "thresholds"))

model.baseline1 <- as.character(baseline1)
both1 <- cfa(model.baseline1, data = dat, group = 'session', missing = 'fiml')
summary(both1, fit.measures = TRUE, standardized = TRUE)







#Higher order - using int and ext - non imputed data 
HS.model3 <- 'E=~ t0_ext_sdq + t0_adhd_in_casi + t0_adhd_hyp_casi 
I=~ t0_int_sdq + t0_socphob_casi + t0_sepanx_casi + t0_mdd_total_casi 
P=~ E + I '

##Fit model
fit2 <- cfa(HS.model3, data = dat0alt)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")
predict(fit2)



#Higher order - using int and ext - non imputed data 
HS.model3 <- 'E=~ t1_ext_sdq + t1_adhd_in_casi + t1_adhd_hyp_casi 
I=~ t1_int_sdq + t1_socphob_casi + t1_sepanx_casi + t1_mdd_total_casi 
P=~ E + I '

##Fit model
fit2 <- cfa(HS.model3, data = dat1alt)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")
predict(fit2)



#Higher order - using int and ext - non imputed data 
HS.model3 <- 'E=~ t2_ext_sdq + t2_adhd_in_casi + t2_adhd_hyp_casi 
I=~ t2_int_sdq + t2_socphob_casi + t2_sepanx_casi + t2_mdd_total_casi 
P=~ E + I '

##Fit model
fit2 <- cfa(HS.model3, data = dat2alt)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")
predict(fit2)






##Bifactor model - imputed data 


HS.model1 <- "P =~ t0_mdd_total_casi + t0_int_sdq + t0_socphob_casi + t0_sepanx_casi + t0_adhd_in_casi + t0_adhd_hyp_casi +  t0_ext_sdq
I   =~ t0_mdd_total_casi + t0_int_sdq + t0_socphob_casi + t0_sepanx_casi
               E =~ t0_adhd_in_casi + t0_adhd_hyp_casi +  t0_ext_sdq 
P ~~ 0*I
P ~~ 0*E
I ~~ 0*E"

fit2 <- cfa(HS.model1, data = dat)
summary(fit2, fit.measures = TRUE)

#Graphical representation 
semPaths(fit2,"std")



##Bifactor model 


HS.modelG <- "P =~ t1_mdd_total_casi + t1_int_sdq + t1_socphob_casi + 
t1_sepanx_casi + t1_adhd_in_casi + t1_adhd_hyp_casi +  t1_ext_sdq
I   =~ t1_mdd_total_casi + t1_int_sdq + t1_socphob_casi + t1_sepanx_casi
               E =~ t1_adhd_in_casi + t1_adhd_hyp_casi +  t1_ext_sdq "

fitG <- cfa(HS.modelG, data = dat)
summary(fitG, fit.measures = TRUE)

#Graphical representation 
semPaths(fitG,"std")




HS.model1 <- 'I=~ t0_mdd_total_casi + t0_int_sdq + t0_socphob_casi + t0_sepanx_casi
E =~ t0_adhd_in_casi + t0_adhd_hyp_casi +  t0_ext_sdq
P =~ t0_mdd_total_casi + t0_int_sdq + t0_socphob_casi + t0_sepanx_casi + t0_adhd_in_casi + t0_adhd_hyp_casi +  t0_ext_sdq'

fit2 <- cfa(HS.model1, data = dat, orthogonal=TRUE)
summary(fit2, fit.measures = TRUE)

#Graphical representation 
semPaths(fit2,"std")



HS.model1 <- 'I=~ t0_mdd_total_casi + t0_int_sdq + t0_socphob_casi + t0_sepanx_casi
E =~ t0_adhd_in_casi + t0_adhd_hyp_casi +  t0_ext_sdq
P =~ t0_mdd_total_casi + t0_int_sdq + t0_socphob_casi + t0_sepanx_casi + t0_adhd_in_casi + t0_adhd_hyp_casi +  t0_ext_sdq'

fit2 <- cfa(HS.model1, data = dat0alt, orthogonal=TRUE)
summary(fit2, fit.measures = TRUE)

#Graphical representation 
semPaths(fit2,"std")











#Common manifestation model 

#Common manifestation shared by disorders
HS.model1 <-'P =~ t0_mdd_casi2 + t0_mdd_casi1 +  t0_mdd_total_casi
+ t0_adhd_in_casi + t0_adhd_hyp_casi + t0_adhd_tot_casi + t0_sepanx_casi + t0_socphob_casi + t0_emo_sdq + t0_cd_sdq +
t0_hy_sdq + t0_peer_sdq + t0_pro_sdq'

fit2 <- cfa(HS.model1, data = dat)
summary(fit2, fit.measures = TRUE)

#Graphical representation 
semPaths(fit2,"std")






#Timepoint1 


#Tried fitting models using the imputed and non imputed data
##Overall comparing both did not change the model fit much at this timepoint 



#CFA - imputed data 
dat1 <- data.frame(matrix(ncol = 0, nrow = 233))
dat1$t1_adhd_in_casi<-trainingdata$t1_adhd_in_casi
dat1$t1_adhd_hyp_casi<-trainingdata$t1_adhd_hyp_casi
dat1$t1_adhd_tot_casi<-trainingdata$t1_adhd_tot_casi
dat1$t1_cd_casi<-trainingdata$t1_cd_casi
dat1$t1_gad_casi<-trainingdata$t1_gad_casi
dat1$t1_mdd_casi1<-trainingdata$t1_mdd_casi1
dat1$t1_mdd_casi2<-trainingdata$t1_mdd_casi2
dat1$t1_mdd_total_casi<-trainingdata$t1_mdd_total_casi
dat1$t1_sepanx_casi<-trainingdata$t1_sepanx_casi
dat1$t1_socphob_casi<-trainingdata$t1_socphob_casi
dat1$t1_emo_sdq<-trainingdata$t1_emo_sdq
dat1$t1_cd_sdq<-trainingdata$t1_cd_sdq
dat1$t1_hy_sdq<-trainingdata$t1_hy_sdq
dat1$t1_peer_sdq<-trainingdata$t1_peer_sdq
dat1$t1_pro_sdq<-trainingdata$t1_pro_sdq
dat1$t1_int_sdq<-trainingdata$t1_int_sdq
dat1$t1_ext_sdq<-trainingdata$t1_ext_sdq



#Alternative data - non imputed data 
dat1alt <- data.frame(matrix(ncol = 0, nrow = 233))
dat1alt$t1_adhd_in_casi<-trainingdata3$t1_adhd_in_casi
dat1alt$t1_adhd_hyp_casi<-trainingdata3$t1_adhd_hyp_casi
dat1alt$t1_adhd_tot_casi<-trainingdata3$t1_adhd_tot_casi
dat1alt$t1_cd_casi<-trainingdata3$t1_cd_casi
dat1alt$t1_gad_casi<-trainingdata3$t1_gad_casi
dat1alt$t1_mdd_casi1<-trainingdata3$t1_mdd_casi1
dat1alt$t1_mdd_casi2<-trainingdata3$t1_mdd_casi2
dat1alt$t1_mdd_total_casi<-trainingdata3$t1_mdd_total_casi
dat1alt$t1_sepanx_casi<-trainingdata3$t1_sepanx_casi
dat1alt$t1_socphob_casi<-trainingdata3$t1_socphob_casi
dat1alt$t1_emo_sdq<-trainingdata3$t1_emo_sdq
dat1alt$t1_cd_sdq<-trainingdata3$t1_cd_sdq
dat1alt$t1_hy_sdq<-trainingdata3$t1_hy_sdq
dat1alt$t1_peer_sdq<-trainingdata3$t1_peer_sdq
dat1alt$t1_pro_sdq<-trainingdata3$t1_pro_sdq
dat1alt$t1_int_sdq<-trainingdata3$t1_int_sdq
dat1alt$t1_ext_sdq<-trainingdata3$t1_ext_sdq




#Correlated factors model - imputed data 
HS.model3 <- 'E=~ t1_adhd_in_casi + t1_adhd_hyp_casi + t1_hy_sdq 
 I=~ t1_gad_casi + t1_mdd_total_casi + t1_emo_sdq'

##Fit model
fit2 <- cfa(HS.model3, data = dat1)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")


#Correlated factors model - imputed data 
HS.model3 <- 'E=~ t1_adhd_in_casi + t1_adhd_hyp_casi + t1_hy_sdq 
 I=~ t1_gad_casi + t1_mdd_total_casi + t1_mdd_casi1
S=~ t1_sepanx_casi + t1_socphob_casi + t1_emo_sdq'

##Fit model
fit2 <- cfa(HS.model3, data = dat1)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")


#Correlated factors model - non imputed data 
HS.model3 <- 'E=~ t1_adhd_in_casi + t1_adhd_hyp_casi + t1_hy_sdq 
 I=~ t1_gad_casi + t1_mdd_total_casi + t1_mdd_casi1
S=~ t1_sepanx_casi + t1_socphob_casi + t1_emo_sdq'

##Fit model
fit2 <- cfa(HS.model3, data = dat1alt)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")



#Correlated factors model - non imputed data 
HS.model3 <- 'E=~ t1_adhd_in_casi + t1_adhd_hyp_casi + t1_hy_sdq 
 I=~ t1_gad_casi + t1_mdd_total_casi + t1_emo_sdq'

##Fit model
fit2 <- cfa(HS.model3, data = dat1alt)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")






##Bifactor model - non imputed data 


HS.model1 <- "P =~ t1_mdd_total_casi + t1_int_sdq + t1_socphob_casi + t1_sepanx_casi + 
t1_adhd_in_casi + t1_adhd_hyp_casi +  t1_ext_sdq
I   =~ t1_mdd_total_casi + t1_int_sdq + t1_socphob_casi + t1_sepanx_casi
               E =~ t1_adhd_in_casi + t1_adhd_hyp_casi +  t1_ext_sdq 
P ~~ 0*I
P ~~ 0*E
I ~~ 0*E"

fit2 <- cfa(HS.model1, data = dat1alt)
summary(fit2, fit.measures = TRUE)

#Graphical representation 
semPaths(fit2,"std")







##Higher order factor model - imputed data

HS.model1 <- 'I=~ t1_gad_casi + t1_mdd_total_casi + t1_emo_sdq
E =~ t1_adhd_in_casi + t1_adhd_hyp_casi + t1_hy_sdq  
P =~ I + E'

fit2 <- cfa(HS.model1, data = dat1)
summary(fit2, fit.measures = TRUE, standardized = TRUE)

#Graphical representation 
semPaths(fit2,"std", shapeMan = "rectangle", nCharNodes = 7, sizeMan = 8, sizeMan2 = 5)




#Higher order - using int and ext 

HS.model3 <- 'E =~ t1_ext_sdq + t1_adhd_in_casi + t1_adhd_hyp_casi 
I =~ t1_int_sdq + t1_socphob_casi + t1_sepanx_casi + t1_mdd_total_casi 
P =~ E + I '

##Fit model
fit2 <- cfa(HS.model3, data = dat1)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")

#Common manifestation model 
HS.model3 <- 'P=~ t1_gad_casi + t1_mdd_total_casi +
t1_mdd_casi1 + 
t1_int_sdq'

##Fit model
fit2 <- cfa(HS.model3, data = dat1alt)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")



#Higher order - using int and ext (alternative) 

HS.model3 <- 'E=~ t1_ext_sdq + t1_adhd_in_casi + t1_adhd_hyp_casi 
I=~ t1_int_sdq + t1_socphob_casi + t1_sepanx_casi + t1_mdd_total_casi 
P=~ E + I '

##Fit model
fit2 <- cfa(HS.model3, data = dat1alt)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")









##Specifying factors##
HS.model5 <- 'I=~ t1_mdd_total_casi 
E =~ t1_adhd_in_casi + t1_adhd_hyp_casi
P =~ t1_cd_sdq + t1_cd_casi'

##CFA
fit4 <- cfa(HS.model5, data = dat1)
summary(fit4, fit.measures = TRUE)



#Graphical representation
semPaths(fit4, residuals=F,sizeMan=7,"std",
         posCol=c("skyblue4", "red"),
         #edge.color="skyblue4",
         edge.label.cex=1.2,layout="circle2")



#Correlated factors model 
HS.model3 <- 'E=~ t1_adhd_in_casi + t1_adhd_hyp_casi + t1_hy_sdq 
 I=~ t1_gad_casi + t1_mdd_total_casi + t1_socphob_casi + t1_sepanx_casi'

#Fit model
fit2 <- cfa(HS.model3, data = dat1)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")


#~Correlated factors  - using int and ext 

HS.model3 <- 'E =~ t1_ext_sdq + t1_adhd_in_casi + t1_adhd_hyp_casi 
I =~ t1_int_sdq + t1_socphob_casi + t1_sepanx_casi + t1_mdd_total_casi'

##Fit model
fit2 <- cfa(HS.model3, data = dat1alt)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")



#Correlated factors model - alternative 
HS.model3 <- 'E=~ t1_adhd_in_casi + t1_adhd_hyp_casi + t1_hy_sdq 
 I=~ t1_gad_casi + t1_mdd_total_casi + t1_socphob_casi + t1_sepanx_casi'

##Fit model
fit2 <- cfa(HS.model3, data = dat1alt)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")




#Higher order factor model 
HS.model1 <- 'I=~ t1_gad_casi + t1_mdd_total_casi + t1_mdd_casi1
E =~ t1_adhd_hyp_casi +  t1_hy_sdq + t1_adhd_tot_casi 
S =~ t1_sepanx_casi + t1_socphob_casi + t1_emo_sdq 
P =~ I + E + S'

fit2 <- cfa(HS.model1, data = dat1alt)
summary(fit2, fit.measures = TRUE, standardized = TRUE)

#Graphical representation 
semPaths(fit2,"std", shapeMan = "rectangle", nCharNodes = 7, sizeMan = 8, sizeMan2 = 5)



#Bifactor model 
HS.modelG <- "P =~ t1_mdd_total_casi + t1_int_sdq + t1_socphob_casi + t1_sepanx_casi + 
t1_adhd_in_casi + t1_adhd_hyp_casi +  t1_ext_sdq
I   =~ t1_mdd_total_casi + t1_int_sdq + t1_socphob_casi + t1_sepanx_casi
               E =~ t1_adhd_in_casi + t1_adhd_hyp_casi +  t1_ext_sdq "

fitG <- cfa(HS.modelG, data = dat1)
summary(fitG, fit.measures = TRUE)

fitG <- cfa(HS.modelG, data = dat1, orthogonal=TRUE)
summary(fitG, fit.measures = TRUE)

#Graphical representation 
semPaths(fitG,"std")

#Graphical representation 
semPaths(fitG,"std")


#Bifactor2 
HS.modelG <- "P =~ t1_mdd_total_casi + t1_int_sdq + 
t1_adhd_in_casi + t1_adhd_hyp_casi +  t1_ext_sdq
I   =~ t1_mdd_total_casi + t1_int_sdq 
               E =~ t1_adhd_in_casi + t1_adhd_hyp_casi +  t1_ext_sdq"


fitG <- cfa(HS.modelG, data = dat1, orthogonal=TRUE)
summary(fitG, fit.measures = TRUE)

#Graphical representation 
semPaths(fitG,"std")




#Timepoint2

#CFA
dat2 <- data.frame(matrix(ncol = 0, nrow = 233))
dat2$t2_adhd_in_casi<-trainingdata$t2_adhd_in_casi
dat2$t2_adhd_hyp_casi<-trainingdata$t2_adhd_hyp_casi
dat2$t2_adhd_tot_casi<-trainingdata$t2_adhd_tot_casi
dat2$t2_cd_casi<-trainingdata$t2_cd_casi
dat2$t2_gad_casi<-trainingdata$t2_gad_casi
dat2$t2_mdd_casi1<-trainingdata$t2_mdd_casi1
dat2$t2_mdd_casi2<-trainingdata$t2_mdd_casi2
dat2$t2_mdd_total_casi<-trainingdata$t2_mdd_total_casi
dat2$t2_sepanx_casi<-trainingdata$t2_sepanx_casi
dat2$t2_socphob_casi<-trainingdata$t2_socphob_casi
dat2$t2_emo_sdq<-trainingdata$t2_emo_sdq
dat2$t2_cd_sdq<-trainingdata$t2_cd_sdq
dat2$t2_hy_sdq<-trainingdata$t2_hy_sdq
dat2$t2_peer_sdq<-trainingdata$t2_peer_sdq
dat2$t2_pro_sdq<-trainingdata$t2_pro_sdq
dat2$t2_int_sdq<-trainingdata$t2_int_sdq
dat2$t2_ext_sdq<-trainingdata$t2_ext_sdq

#Alternative data 
dat2alt <- data.frame(matrix(ncol = 0, nrow = 233))
dat2alt$t2_adhd_in_casi<-trainingdata3$t2_adhd_in_casi
dat2alt$t2_adhd_hyp_casi<-trainingdata3$t2_adhd_hyp_casi
dat2alt$t2_adhd_tot_casi<-trainingdata3$t2_adhd_tot_casi
dat2alt$t2_cd_casi<-trainingdata3$t2_cd_casi
dat2alt$t2_gad_casi<-trainingdata3$t2_gad_casi
dat2alt$t2_mdd_casi1<-trainingdata3$t2_mdd_casi1
dat2alt$t2_mdd_casi2<-trainingdata3$t2_mdd_casi2
dat2alt$t2_mdd_total_casi<-trainingdata3$t2_mdd_total_casi
dat2alt$t2_sepanx_casi<-trainingdata3$t2_sepanx_casi
dat2alt$t2_socphob_casi<-trainingdata3$t2_socphob_casi
dat2alt$t2_emo_sdq<-trainingdata3$t2_emo_sdq
dat2alt$t2_cd_sdq<-trainingdata3$t2_cd_sdq
dat2alt$t2_hy_sdq<-trainingdata3$t2_hy_sdq
dat2alt$t2_peer_sdq<-trainingdata3$t2_peer_sdq
dat2alt$t2_pro_sdq<-trainingdata3$t2_pro_sdq
dat2alt$t2_int_sdq<-trainingdata3$t2_int_sdq
dat2alt$t2_ext_sdq<-trainingdata3$t2_ext_sdq




#Correlated factors model 
HS.model3 <- 'E=~ t2_adhd_in_casi + t2_adhd_hyp_casi + t2_hy_sdq 
 I=~ t2_gad_casi + t2_mdd_total_casi + t2_socphob_casi + t2_sepanx_casi'

##Fit model
fit2 <- cfa(HS.model3, data = dat2)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")





#Correlated factors model 
HS.model3 <- 'E=~ t2_adhd_in_casi + t2_adhd_hyp_casi + t2_hy_sdq 
 I=~ t2_gad_casi + t2_mdd_total_casi + t2_emo_sdq'

##Fit model
fit2 <- cfa(HS.model3, data = dat2)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")



##Bifactor model 


HS.model1 <- 'P =~ t2_mdd_total_casi + t2_int_sdq + t2_socphob_casi + t2_sepanx_casi + 
t2_adhd_in_casi + t2_adhd_hyp_casi +  t2_ext_sdq
I =~ t2_mdd_total_casi + t2_int_sdq + t2_socphob_casi + t2_sepanx_casi
E =~ t2_adhd_in_casi + t2_adhd_hyp_casi +  t2_ext_sdq 
P ~~ 0*I
P ~~ 0*E
I ~~ 0*E'

fit2 <- cfa(HS.model1, data = dat2alt)
summary(fit2, fit.measures = TRUE)

#Graphical representation 
semPaths(fit2,"std")





#Correlated factors model 
HS.model3 <- 'E=~ t2_adhd_in_casi + t2_adhd_hyp_casi + t2_hy_sdq 
 I=~ t2_gad_casi + t2_mdd_total_casi + t2_mdd_casi1
S=~ t2_sepanx_casi + t2_socphob_casi + t2_emo_sdq'

##Fit model
fit2 <- cfa(HS.model3, data = dat2)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")



#Correlated factors model - alternative 
HS.model3 <- 'E=~ t2_adhd_in_casi + t2_adhd_hyp_casi + t2_hy_sdq 
 I=~ t2_gad_casi + t2_mdd_total_casi + t2_socphob_casi + t2_sepanx_casi'

##Fit model
fit2 <- cfa(HS.model3, data = dat2alt)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")




#Higher order factor model 
HS.model1 <- 'I=~ t2_gad_casi + t2_mdd_total_casi + t2_mdd_casi1
E =~ t2_adhd_hyp_casi +  t2_hy_sdq + t2_adhd_tot_casi 
P =~ I + E'

fit2 <- cfa(HS.model1, data = dat2)
summary(fit2, fit.measures = TRUE, standardized = TRUE)

#Graphical representation 
semPaths(fit2,"std", shapeMan = "rectangle", nCharNodes = 7, sizeMan = 8, sizeMan2 = 5)




#Correlated factors model - alternative 
HS.model3 <- 'E=~ t2_adhd_in_casi + t2_adhd_hyp_casi + t2_hy_sdq 
 I=~ t2_gad_casi + t2_mdd_total_casi + t2_emo_sdq'

##Fit model
fit2 <- cfa(HS.model3, data = dat2alt)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")


#Higher order factor model 
HS.model1 <- 'I=~ t2_gad_casi + t2_mdd_total_casi + t2_mdd_casi1
E =~ t2_adhd_hyp_casi +  t2_hy_sdq + t2_adhd_tot_casi 
S =~ t2_sepanx_casi + t2_socphob_casi + t2_emo_sdq 
P =~ I + E + S'

fit2 <- cfa(HS.model1, data = dat2)
summary(fit2, fit.measures = TRUE, standardized = TRUE)

#Graphical representation 
semPaths(fit2,"std", shapeMan = "rectangle", nCharNodes = 7, sizeMan = 8, sizeMan2 = 5)




#Higher order factor model - alt 
HS.model1 <- 'I=~ t2_gad_casi + t2_mdd_total_casi + t2_mdd_casi1
E =~ t2_adhd_hyp_casi +  t2_hy_sdq + t2_adhd_tot_casi 
S =~ t2_sepanx_casi + t2_socphob_casi + t2_emo_sdq 
P =~ I + E + S'

fit2 <- cfa(HS.model1, data = dat2alt)
summary(fit2, fit.measures = TRUE, standardized = TRUE)

#Graphical representation 
semPaths(fit2,"std", shapeMan = "rectangle", nCharNodes = 7, sizeMan = 8, sizeMan2 = 5)





#Higher order - using int and ext 

HS.model3 <- 'E =~ t2_ext_sdq + t2_adhd_in_casi + t2_adhd_hyp_casi 
I =~ t2_int_sdq + t2_socphob_casi + t2_sepanx_casi + t2_mdd_total_casi 
P =~ E + I '

##Fit model
fit2 <- cfa(HS.model3, data = dat2alt)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")



#Higher order - using int and ext 

HS.model3 <- 'E =~ t2_ext_sdq + t2_adhd_in_casi + t2_adhd_hyp_casi 
I =~ t2_int_sdq + t2_socphob_casi + t2_sepanx_casi + t2_mdd_total_casi 
P =~ E + I '

##Fit model
fit2 <- cfa(HS.model3, data = dat2alt)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")



#~Correlated factors  - using int and ext 

HS.model3 <- 'E =~ t2_ext_sdq + t2_adhd_in_casi + t2_adhd_hyp_casi 
I =~ t2_int_sdq + t2_socphob_casi + t2_sepanx_casi + t2_mdd_total_casi'

##Fit model
fit2 <- cfa(HS.model3, data = dat2alt)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")



#Correlated factors model - alternative 
HS.model3 <- 'E=~ t2_adhd_in_casi + t2_adhd_hyp_casi + t2_hy_sdq 
 I=~ t2_gad_casi + t2_mdd_total_casi + t2_mdd_casi1
S=~ t2_sepanx_casi + t2_socphob_casi + t2_emo_sdq'

##Fit model
fit2 <- cfa(HS.model3, data = dat2alt)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")





##Specifying factors##
HS.model6 <- 'I=~ t2_gad_casi + t2_mdd_total_casi 
E =~ t2_adhd_in_casi + t2_adhd_hyp_casi'

##CFA
fit5 <- cfa(HS.model6, data = dat2)
summary(fit5, fit.measures = TRUE)



#Graphical representation
semPaths(fit, residuals=F,sizeMan=7,"std",
         posCol=c("skyblue4", "red"),
         #edge.color="skyblue4",
         edge.label.cex=1.2,layout="circle2")


##Specifying factors##
HS.model7 <- 'I=~ t2_mdd_total_casi 
E =~ t2_adhd_in_casi + t2_adhd_hyp_casi
P =~ t2_cd_sdq + t2_cd_casi'

##CFA
fit6 <- cfa(HS.model7, data = dat2)
summary(fit6, fit.measures = TRUE)




#Extracted p scores  

#
g1 <- ggplot(trainingdata3, aes(x=Group, y=P_T0)) + 
  geom_boxplot(aes(group = Group)) 
g1

#SES between groups 
g1 <- ggplot(trainingdata3, aes(x=Group, y=P_T1)) + 
  geom_boxplot(aes(group = Group)) 
g1

#SES between groups 
g1 <- ggplot(trainingdata3, aes(x=Group, y=P_T2)) + 
  geom_boxplot(aes(group = Group)) 
g1



g1 <- ggplot(trainingdata3, aes(x=Group, y=P_T0)) + 
  geom_boxplot() + coord_flip()
g1



# mdd_casi2 - training effect
lmer1 <- lmer(P_T0 ~ Group + (1|ID) + TotalSessions, data = trainingdata3, REML=FALSE)
summary(lmer1)

lmer2 <- lmer(P_T1 ~ Group + (1|ID) + TotalSessions, data = trainingdata3, REML=FALSE)
summary(lmer2)

lmer3 <- lmer(P_T2 ~ Group + (1|ID) + TotalSessions, data = trainingdata3, REML=FALSE)
summary(lmer3)

anova(lmer1, lmer2, lmer3)


lmer2 <- lmer(mdd_casi2 ~ timepoint + Group + (1|ID) + TotalSessions, data = trainingdata2, REML=FALSE)
anova(lmer1, lmer2) # no significant effect 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Major Depressive Disorder II") # plot this to visualise



