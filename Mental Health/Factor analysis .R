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
library(semTools)
library(emmeans)
library(BayesFactor)
library(brms)


rm(list=ls())


#NB: Data imputation was performed in another script (imputation_Grace.R)

# Load data 

#Imputed (didn't use)
fulldata <- "/Users/Grace/Desktop/PhD/Rotation2/mentalhealth_imputed.csv" 
trainingdata <- read.csv(fulldata, stringsAsFactors=FALSE, na.strings=c("","NA")) 


#Non-imputed data (long format)
analysis_a <- "/Users/grace/Desktop/Stats/main_a_covariates.csv" 
trainingdata2 <- read.csv(analysis_a, stringsAsFactors=FALSE, na.strings=c("","NA")) 

#Non-imputed data (wide format)
partialdata <- "/Users/grace/Desktop/Stats/TrainingData.csv" 
trainingdata3 <- read.csv(partialdata, stringsAsFactors=FALSE, na.strings=c("","NA")) 


# Merge 1 & 1_2 timepoint 
trainingdata2$timepoint[trainingdata2$timepoint==0] <- 0  
trainingdata2$timepoint[trainingdata2$timepoint==1] <- 1  
trainingdata2$timepoint[trainingdata2$timepoint=='1_2'] <- 1  
trainingdata2$timepoint[trainingdata2$timepoint==2] <- 2  





# Factor analysis 

#~Correlated factors  - using int and ext - non imputed data 

HS.model1 <- 'E =~ ext_sdq + adhd_in_casi + adhd_hyp_casi 
I =~ int_sdq + socphob_casi + sepanx_casi + mdd_total_casi'

##Fit model
fit1 <- cfa(HS.model1, data = trainingdata2)
summary(fit1, fit.measures = TRUE)

#Graphical representation
semPaths(fit1,"std")
predict(fit1)


#Constraining across timepoints 
baseline1 <- measEq.syntax(configural.model = HS.model1,
                           data = trainingdata2, 
                           parameterization = "delta",
                           group = "timepoint", missing = 'fiml',
                           group.equal = c( "loadings", "thresholds"))

model.baseline1 <- as.character(baseline1)
both1 <- cfa(model.baseline1, data = trainingdata2, group = 'timepoint', missing = 'fiml')
summary(both1, fit.measures = TRUE, standardized = TRUE)



#Not finished this section 

xyz <- predict(both1)
 as.data.frame(xyz)

total <- merge(trainingdata2, xyz)

t0 <- total %>%
  subset(timepoint == 0) 
t1 <- total %>% 
  subset(timepoint == 1) 
t2 <- total %>% 
  subset(timepoint == 2) 
             

formula <- I ~ timepoint*Group + (1|ID) +Age_frac_T0
aov(lmer( formula, data=total, REML=TRUE))
model<-lmer( formula, data=total, REML=TRUE)
print(emmeans(model, pairwise~timepoint*Group))


bf <- lmBF(adhd_in_casi ~ timepoint*Group, data=trainingdata2, whichRandom = "ID")
bf
bf[4] / bf[3]





#Higher order - using int and ext - non imputed data


HS.model2 <- 'E=~ ext_sdq + adhd_in_casi + adhd_hyp_casi 
I=~ int_sdq + socphob_casi + sepanx_casi + mdd_total_casi 
P=~ E + I '

##Fit model
fit2 <- cfa(HS.model2, data = trainingdata2)
summary(fit2, fit.measures = TRUE)

#Graphical representation
semPaths(fit2,"std")
predict(fit2)


baseline1 <- measEq.syntax(configural.model = HS.model2,
                           data = trainingdata2, 
                           parameterization = "delta",
                           group = "timepoint", missing = 'fiml',
                           group.equal = c( "loadings", "thresholds"))

model.baseline1 <- as.character(baseline1)
both2 <- cfa(model.baseline1, data = trainingdata2, group = 'timepoint', missing = 'fiml')
summary(both2, fit.measures = TRUE, standardized = TRUE)





anova(both1, both2)





