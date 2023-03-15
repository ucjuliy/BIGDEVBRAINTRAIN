# This code runs LCS with Academic and EF data

# Load packages
library(lavaan)
library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)

# Load data here
setwd("Z:/Roser/analyses_20220621_AcEF")
data_excel <- read_excel("data_factor_T0-T1_wide.xlsx")
data <- as.data.frame(data_excel)

# Change working directory to output folder
setwd("Z:/Roser/analyses_20220621_AcEF/LCS")

# Specify factors
data$group<-factor(data$group,
                        labels = c("control","experimental"),
                        levels = c("control", "experimental"))
data$gender <- factor(data$gender)

# Define measures for loops
measures <- c('academic_total_scores_T0','academic_total_scores_T1',
                   'inhibition_factor_T0','switch_factor_T0','memory_factor_T0')

# Check for outliers & replace by NaN
## Loop through measures
for(n in 1:length(measures)){
  outlier_values <- boxplot.stats(eval(parse(text=paste("data","$",measures[[n]],sep=""))))$out # outlier values.
  assign(paste("outlier_rowID_",measures[[n]],sep=""),which(eval(parse(text=paste("data","$",measures[[n]],sep=""))) %in% c(outlier_values)))
  boxplot(eval(parse(text=paste("data","$",measures[[n]],sep=""))), main=measures[n], boxwex=0.1)
  mtext(paste("Outliers: ", paste(eval(parse(text=paste("outlier_rowID_",measures[[n]],sep=""))), collapse=", ")), cex=0.6)
  eval(parse(text=paste("data","$",measures[[n]],'[outlier_rowID_',measures[[n]],']<-NA',sep="")))
}

# Run LCS: covariance

## academic_total_scores - inhibition_factor

ULCS<-'

academic_total_scores_T1 ~ 1*academic_total_scores_T0     # Fixed regression of COG_T1 on COG_T0
dacad01 =~ 1*academic_total_scores_T1                     # Fixed regression of dCOG01 on COG_T1
academic_total_scores_T1 ~ 0*1                            # This line constrains the intercept of COG_T1 to 0
academic_total_scores_T1 ~~ 0*academic_total_scores_T1    # This fixes the variance of the COG_T1 to 0

inhibition_factor_T0 ~ 1
inhibition_factor_T0 ~~ inhibition_factor_T0

dacad01 ~ 1                                               # This estimates the intercept of the change scores
academic_total_scores_T0 ~  1                             # This estimates the intercept of COG_T0
dacad01 ~~  dacad01                                       # This estimates the variance of the change scores
academic_total_scores_T0 ~~ academic_total_scores_T0      # This estimates the variance of COG_T0
dacad01 ~ academic_total_scores_T0+inhibition_factor_T0   # This estimates the self-feedback parameter: ~ is for regression, ~~ is for covariance (i.e. control for T0 change)

'

### Fit LCS: specify model, data, group for group comparisons, and deal with missing data with FIML
ULCSfit <- lavaan(ULCS, data=data, estimator='mlr', fixed.x=FALSE, group ='group', missing='fiml')

### Show results from LCS fit: CHANGE RESULT FILE!!!
sink("lcs-regression_acad-inh.txt")
summary(ULCSfit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
sink()
# Note about output: if there's a dot before a variable name means it's dependent/endogenous
# (predicted by the latent variables): the value for the variance that is printed in the output is an estimate of
# the residual variance: the left-over variance that is not explained by the predictor(s).
# By contrast, there is no dot before the latent variable names, because they are exogenous variables in this model
# (there are no single-headed arrows pointing to them). The values for the variances here are the estimated total
# variances of the latent variables

## academic_total_scores - switch_factor

ULCS<-'

academic_total_scores_T1 ~ 1*academic_total_scores_T0     # Fixed regression of COG_T1 on COG_T0
dacad01 =~ 1*academic_total_scores_T1                     # Fixed regression of dCOG01 on COG_T1
academic_total_scores_T1 ~ 0*1                            # This line constrains the intercept of COG_T1 to 0
academic_total_scores_T1 ~~ 0*academic_total_scores_T1    # This fixes the variance of the COG_T1 to 0

switch_factor_T0 ~ 1
switch_factor_T0 ~~ switch_factor_T0

dacad01 ~ 1                                               # This estimates the intercept of the change scores
academic_total_scores_T0 ~  1                             # This estimates the intercept of COG_T0
dacad01 ~~  dacad01                                       # This estimates the variance of the change scores
academic_total_scores_T0 ~~ academic_total_scores_T0      # This estimates the variance of COG_T0
dacad01 ~ academic_total_scores_T0+switch_factor_T0      # This estimates the self-feedback parameter: ~ is for regression, ~~ is for covariance (i.e. control for T0 change)

'

### Fit LCS: specify model, data, group for group comparisons, and deal with missing data with FIML
ULCSfit <- lavaan(ULCS, data=data, estimator='mlr', fixed.x=FALSE, group ='group', missing='fiml')

### Show results from LCS fit: CHANGE RESULT FILE!!!
sink("lcs-regression_acad-switch.txt")
summary(ULCSfit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
sink()
# Note about output: if there's a dot before a variable name means it's dependent/endogenous
# (predicted by the latent variables): the value for the variance that is printed in the output is an estimate of
# the residual variance: the left-over variance that is not explained by the predictor(s).
# By contrast, there is no dot before the latent variable names, because they are exogenous variables in this model
# (there are no single-headed arrows pointing to them). The values for the variances here are the estimated total
# variances of the latent variables

## academic_total_scores - memory_factor

ULCS<-'

academic_total_scores_T1 ~ 1*academic_total_scores_T0     # Fixed regression of COG_T1 on COG_T0
dacad01 =~ 1*academic_total_scores_T1                     # Fixed regression of dCOG01 on COG_T1
academic_total_scores_T1 ~ 0*1                            # This line constrains the intercept of COG_T1 to 0
academic_total_scores_T1 ~~ 0*academic_total_scores_T1    # This fixes the variance of the COG_T1 to 0

memory_factor_T0 ~ 1
memory_factor_T0 ~~ memory_factor_T0

dacad01 ~ 1                                               # This estimates the intercept of the change scores
academic_total_scores_T0 ~  1                             # This estimates the intercept of COG_T0
dacad01 ~~  dacad01                                       # This estimates the variance of the change scores
academic_total_scores_T0 ~~ academic_total_scores_T0      # This estimates the variance of COG_T0
dacad01 ~ academic_total_scores_T0+memory_factor_T0      # This estimates the self-feedback parameter: ~ is for regression, ~~ is for covariance (i.e. control for T0 change)

'

### Fit LCS: specify model, data, group for group comparisons, and deal with missing data with FIML
ULCSfit <- lavaan(ULCS, data=data, estimator='mlr', fixed.x=FALSE, group ='group', missing='fiml')

### Show results from LCS fit: CHANGE RESULT FILE!!!
sink("lcs-regression_acad-mem.txt")
summary(ULCSfit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
sink()
# Note about output: if there's a dot before a variable name means it's dependent/endogenous
# (predicted by the latent variables): the value for the variance that is printed in the output is an estimate of
# the residual variance: the left-over variance that is not explained by the predictor(s).
# By contrast, there is no dot before the latent variable names, because they are exogenous variables in this model
# (there are no single-headed arrows pointing to them). The values for the variances here are the estimated total
# variances of the latent variables
