setwd("/Users/zoeli/Library/CloudStorage/OneDrive-UniversityCollegeLondon/Desktop/training study/mixed model/imputed_data_across_domains")

library(fitdistrplus)

EF <- read.csv("EF_imputed_3timepointmeasures.csv")
EF_goRT <- EF[, c("p_id", "session", "meango_corrrtnr_t")]
EF_goRT <- reshape(EF_goRT, idvar = "p_id", timevar = "session", direction = "wide")

data.imp <- read.csv("data_imp.csv")
EF_factor_T0 <- read.csv("EF_NewThreeFactor_t0.csv")
training_data <- merge(data.imp, EF_factor_T0, by = c("id"),
                       all.x=TRUE, all.y = TRUE)
write.csv(training_data, "training_data.csv") 

training_data <- read.csv("training_data.csv")
EF <- read.csv("EF_imputed_3timepointmeasures_modified.csv")
training_data <- merge(training_data, EF, by = c("id", "timepoint"),
                                                 all.x=TRUE, all.y=TRUE)
training_data <-subset(training_data, Group != "NA" )
write.csv(training_data, "training_data.csv") 


EF_factor_T0 <- read.csv("SingleEFfactor_t0.csv")
training_data <- read.csv("training_data.csv")
ses <- read.csv("ses.csv")
wasi <- read.csv ("wasi_imputated.csv")
training_data <- merge(training_data, ses, by = c("id"),
                       all.x=TRUE, all.y=TRUE)
training_data <- merge(training_data, wasi, by = c("id", "timepoint"),
                       all.x=TRUE, all.y=TRUE)
write.csv(training_data, "training_data.csv") 

training_data <-subset(training_data, SES != "NA" )
training_data <- training_data[training_data$timepoint %in% c("0"), ]

quantile(training_data$SES,probs = c(0.25, 0.5, 0.75, 1))
unname(round(quantile(training_data$SES,probs = c(0.25, 0.5, 0.75, 1))))
summary(training_data$SES)
table(cut(training_data$SES, quantile(training_data$SES, 0:4 /4)))

descdist(training_data$SES, discrete = FALSE)
normal_dist <- fitdist(training_data$SES, "norm")
plot(normal_dist)

hist(training_data$SES)
lines(density(training_data$SES))
shapiro.test(training_data$SES)

