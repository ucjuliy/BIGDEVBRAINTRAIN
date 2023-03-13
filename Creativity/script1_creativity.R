### 12/07/2022
# Does training lead to changes in creativity?
library("readxl")
library(dplyr)

t0 <- read.csv("./TorranceRatings-Rose_T0.csv")
t1 <- read.csv("./TorranceRatings-Rose_T1.csv")

# demographic information
demo <- read.csv("./Demographic_Information.csv")

# replace "#N/A" with NA
t0[t0=="#N/A"] <- NA
t1[t1=="#N/A"] <- NA

# check the variable type
str(t0) # factors, so these need to be changed
str(t1) # factors, so these need to be changed

t0 = t0 %>% mutate(across(.cols=2:7, .fns=as.numeric)) 
t1 = t1 %>% mutate(across(.cols=2:7, .fns=as.numeric)) 

# write the data out for use in other situations
write.csv(t0, file="./creativityt0_db.csv")
write.csv(t1, file="./creativityt1_db.csv")

# is there is a significant difference between T0-T1 on any of the creativity measures? 
t.test(t0$Fluency_T0, t1$Fluency_T1) # the data is non-normal so makes sense to to a wilcox
wilcox.test(t0$Fluency_T0 , t1$Fluency_T1) 
boxplot(t0$Fluency_T0)
boxplot(t1$Fluency_T1)

# so fluency decreases at T1
t.test(t0$Originality_T0, t1$Originality_T1) # the data is non-normal so makes sense to to a wilcox
wilcox.test(t0$Originality_T0, t1$Originality_T1) # originality decreases at T1

t.test(t0$Title_T0, t1$Title_T1) # title is no different
wilcox.test(t0$Title_T0, t1$Title_T1) #

t.test(t0$Elaboration_T0, t1$Elaboration_T1) # elaboration is no different

t.test(t0$Closure_T0, t1$Closure_T1) # closure is no different

t.test(t0$Total_T0, t1$Total_T1) # total is very different
wilcox.test(t0$Total_T0, t1$Total_T1) #

# my first thoughts are: 
# 1) this might just be caused by the difference in N, since there is a much smaller N at T1?
# 2) this could just be age-related
# 3) what about training, specifically?
# 4) also look at correlations with SSRT?

# let's look at training now
training_t0 <- t0
training_t1 <- t1

# Is there an impact of training?  ----------------------------------------

colnames(training_t0)[2] <- "Fluency"
colnames(training_t0)[3] <- "Originality"
colnames(training_t0)[4] <- "Title"
colnames(training_t0)[5] <- "Elaboration"
colnames(training_t0)[6] <- "Closure"
colnames(training_t0)[7] <- "Total"

colnames(training_t1)[2] <- "Fluency"
colnames(training_t1)[3] <- "Originality"
colnames(training_t1)[4] <- "Title"
colnames(training_t1)[5] <- "Elaboration"
colnames(training_t1)[6] <- "Closure"
colnames(training_t1)[7] <- "Total"

training_db <- rbind(training_t0, training_t1)
# merge with demographic information
db <- merge(training_db, demo, by="ID", all.x=TRUE) # n_distinct(db$ID) = 251

# check the type of variables
str(db)
db$ID <- as.factor(db$ID)
db$Group <- as.factor(db$Group)
db$timepoint <- as.factor(db$timepoint)

# center the predictors
db <- transform(db,
                       age_cs = scale(Age_frac_T0))

library(lme4)
library(ggplot2)

# # ADHD_IN_CASI training effect
# lmer1 <- lmer(adhd_in_casi ~ timepoint*Group + (1|ID) + totaltraining_cs, data = analysis_a, REML=FALSE)
# summary(lmer1)
# lmer2 <- lmer(adhd_in_casi ~ timepoint + Group + (1|ID) + totaltraining_cs, data = analysis_a, REML=FALSE)
# anova(lmer1, lmer2) # no significant effect 
# sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="ADHD In CASI") # plot this to visualise
# ggsave("./plots/script5/analysis_a/group_casi_adhdin.jpg", width = 8, height = 6, dpi = 300) # save the plot


# Is there a significant effect of training?  -----------------------------

# is there a significant effect of training? 
lmer1 <- lmer(Fluency ~ timepoint*Group + (1|ID), data = db, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(Fluency ~ timepoint + Group + (1|ID), data = db, REML=FALSE)
anova(lmer1, lmer2) # no significant effect of training 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Fluency") # plot this to visualise
ggsave("./plots/fluency_training.jpg", width = 8, height = 6, dpi = 300) # save the plot

# is there a significant effect of training? 
lmer1 <- lmer(Originality ~ timepoint*Group + (1|ID), data = db, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(Originality ~ timepoint + Group + (1|ID), data = db, REML=FALSE)
anova(lmer1, lmer2) # no significant effect of training 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Originality") # plot this to visualise
ggsave("./plots/originality_training.jpg", width = 8, height = 6, dpi = 300) # save the plot

# is there a significant effect of training? 
lmer1 <- lmer(Title ~ timepoint*Group + (1|ID), data = db, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(Title ~ timepoint + Group + (1|ID), data = db, REML=FALSE)
anova(lmer1, lmer2) # no significant effect of training 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Title") # plot this to visualise
ggsave("./plots/title_training.jpg", width = 8, height = 6, dpi = 300) # save the plot

# is there a significant effect of training? 
lmer1 <- lmer(Elaboration~ timepoint*Group + (1|ID), data = db, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(Elaboration ~ timepoint + Group + (1|ID), data = db, REML=FALSE)
anova(lmer1, lmer2) # no significant effect of training 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Elaboration") # plot this to visualise
ggsave("./plots/elaboration_training.jpg", width = 8, height = 6, dpi = 300) # save the plot

# is there a significant effect of training? 
lmer1 <- lmer(Closure ~ timepoint*Group + (1|ID), data = db, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(Closure ~ timepoint + Group + (1|ID), data = db, REML=FALSE)
anova(lmer1, lmer2) # no significant effect of training 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Closure") # plot this to visualise
ggsave("./plots/closure_training.jpg", width = 8, height = 6, dpi = 300) # save the plot

# is there a significant effect of training? 
lmer1 <- lmer(Total ~ timepoint*Group + (1|ID), data = db, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(Total ~ timepoint + Group + (1|ID), data = db, REML=FALSE)
anova(lmer1, lmer2) # no significant effect of training 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Total") # plot this to visualise
ggsave("./plots/total_training.jpg", width = 8, height = 6, dpi = 300) # save the plot


# With age controlled -----------------------------------------------------


# is there a significant effect of training? 
lmer1 <- lmer(Fluency ~ timepoint*Group + (1|ID) + age_cs, data = db, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(Fluency ~ timepoint + Group + (1|ID) + age_cs, data = db, REML=FALSE)
anova(lmer1, lmer2) # no significant effect of training 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Fluency") # plot this to visualise
ggsave("./plots/fluency_training.jpg", width = 8, height = 6, dpi = 300) # save the plot

# is there a significant effect of training? 
lmer1 <- lmer(Originality ~ timepoint*Group + (1|ID) + age_cs, data = db, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(Originality ~ timepoint + Group + (1|ID) + age_cs, data = db, REML=FALSE)
anova(lmer1, lmer2) # no significant effect of training 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Originality") # plot this to visualise
ggsave("./plots/originality_training.jpg", width = 8, height = 6, dpi = 300) # save the plot

# is there a significant effect of training? 
lmer1 <- lmer(Title ~ timepoint*Group + (1|ID) + age_cs, data = db, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(Title ~ timepoint + Group + (1|ID) + age_cs, data = db, REML=FALSE)
anova(lmer1, lmer2) # no significant effect of training 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Title") # plot this to visualise
ggsave("./plots/title_training.jpg", width = 8, height = 6, dpi = 300) # save the plot

# is there a significant effect of training? 
lmer1 <- lmer(Elaboration~ timepoint*Group + (1|ID) + age_cs, data = db, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(Elaboration ~ timepoint + Group + (1|ID) + age_cs, data = db, REML=FALSE)
anova(lmer1, lmer2) # no significant effect of training 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Elaboration") # plot this to visualise
ggsave("./plots/elaboration_training.jpg", width = 8, height = 6, dpi = 300) # save the plot

# is there a significant effect of training? 
lmer1 <- lmer(Closure ~ timepoint*Group + (1|ID) + age_cs, data = db, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(Closure ~ timepoint + Group + (1|ID) + age_cs, data = db, REML=FALSE)
anova(lmer1, lmer2) # no significant effect of training 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Closure") # plot this to visualise
ggsave("./plots/closure_training.jpg", width = 8, height = 6, dpi = 300) # save the plot

# is there a significant effect of training? 
lmer1 <- lmer(Total ~ timepoint*Group + (1|ID) + age_cs, data = db, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(Total ~ timepoint + Group + (1|ID) + age_cs, data = db, REML=FALSE)
anova(lmer1, lmer2) # no significant effect of training 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Total") # plot this to visualise
ggsave("./plots/total_training.jpg", width = 8, height = 6, dpi = 300) # save the plot


# Is there a significant effect of age?  ----------------------------------

lmer1 <- lmer(Fluency ~ timepoint + Group + (1|ID) + age_cs, data = db, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(Fluency ~ timepoint + Group + (1|ID), data = db, REML=FALSE)
anova(lmer1, lmer2) # no significant effect of training 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Fluency") # plot this to visualise
# this approaches significance 
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
# lmer2    5 1283.5 1301.4 -636.73   1273.5                       
# lmer1    6 1281.9 1303.4 -634.93   1269.9 3.5883  1    0.05819 .

lmer1 <- lmer(Originality ~ timepoint + Group + (1|ID) + age_cs, data = db, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(Originality ~ timepoint + Group + (1|ID), data = db, REML=FALSE)
anova(lmer1, lmer2) # no significant effect of training 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Originality") # plot this to visualise

lmer1 <- lmer(Title ~ timepoint + Group + (1|ID) + age_cs, data = db, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(Title ~ timepoint + Group + (1|ID), data = db, REML=FALSE)
anova(lmer1, lmer2) # no significant effect of training 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Title") # plot this to visualise
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
# lmer2    5 1692.5 1710.5 -841.27   1682.5                       
# lmer1    6 1691.2 1712.8 -839.63   1679.2 3.2819  1    0.07005

lmer1 <- lmer(Elaboration~ timepoint + Group + (1|ID) + age_cs, data = db, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(Elaboration ~ timepoint + Group + (1|ID), data = db, REML=FALSE)
anova(lmer1, lmer2) # no significant effect of training 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Elaboration") # plot this to visualise
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)   
# lmer2    5 587.32 605.24 -288.66   577.32                        
# lmer1    6 580.83 602.33 -284.42   568.83 8.4905  1    0.00357 **

lmer1 <- lmer(Closure ~ timepoint*Group + (1|ID) + age_cs, data = db, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(Closure ~ timepoint + Group + (1|ID), data = db, REML=FALSE)
anova(lmer1, lmer2) # no significant effect of training 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Closure") # plot this to visualise

# is there a significant effect of training? 
lmer1 <- lmer(Total ~ timepoint + Group + (1|ID) + age_cs, data = db, REML=FALSE)
summary(lmer1)
lmer2 <- lmer(Total ~ timepoint + Group + (1|ID), data = db, REML=FALSE)
anova(lmer1, lmer2) # no significant effect of training 
sjPlot::plot_model(lmer1, type = "eff", terms = c("timepoint","Group"), title="Total") # plot this to visualise
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
# lmer2    4 1858.4 1872.7 -925.20   1850.4                       
# lmer1    5 1857.5 1875.4 -923.75   1847.5 2.8888  1     0.0892 

# Correlations with SSRT performance --------------------------------------

ssrt <- read.csv("./EFLong_Abi.csv")
colnames(ssrt)[1] <- "ID"
ssrt_t0 <- filter(ssrt, session == 0)
ssrt_t1 <- filter(ssrt, session == 1)

t0_data <- merge(t0, ssrt_t0, by='ID', all.x=TRUE)
t1_data <- merge(t1, ssrt_t1, by='ID', all.x=TRUE)

# correlations at T0
cor.test(t0_data$Fluency_T0, t0_data$Fssrtnr_t)
cor.test(t0_data$Originality_T0, t0_data$Fssrtnr_t)
cor.test(t0_data$Title_T0, t0_data$Fssrtnr_t)
cor.test(t0_data$Elaboration_T0, t0_data$Fssrtnr_t)
cor.test(t0_data$Closure_T0, t0_data$Fssrtnr_t)
cor.test(t0_data$Total_T0, t0_data$Fssrtnr_t)

# correlations at T1
cor.test(t1_data$Fluency_T1, t1_data$Fssrtnr_t)
cor.test(t1_data$Originality_T1, t1_data$Fssrtnr_t)
cor.test(t1_data$Title_T1, t1_data$Fssrtnr_t)
cor.test(t1_data$Elaboration_T1, t1_data$Fssrtnr_t)
cor.test(t1_data$Closure_T1, t1_data$Fssrtnr_t)
cor.test(t1_data$Total_T1, t1_data$Fssrtnr_t)

# Does creativity correlate with 'within network' connectivity? (at T0) -----------

conn <- read.csv("./within_network_conn_allPpts_T0.csv")
colnames(conn)[1] <- "ID"
conn_db <- merge(t0, conn, by="ID") # 122 observations - this is how many have connectivity data

# output for spss
write.csv(conn_db, file="./connectivitydata_db.csv", na="")

