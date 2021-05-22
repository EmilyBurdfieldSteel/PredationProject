predation_march <- read.csv("experiment_march.csv", header=TRUE, sep=";")

### Transform to factor
predation_march$MothColour<-factor(predation_march$MothColour)
predation_march$Block<-factor(predation_march$Block)
predation_march$Status<-factor(predation_march$Status) #Can't do this to get the coxph funtion to work
predation_march$Status<-as.numeric(predation_march$Status)

  
str(predation_march)

boxplot(Status~Block*MothColour, data=predation_march)

### Coxme analysis
install.packages("coxme")
library(coxme)
library(survival)
modela<-coxph(Surv(SurvMin, Status) ~ MothColour, 
              na.action=na.omit, data=predation_march)
modelb<-coxph(Surv(SurvMin, Status)~ MothColour * Block, 
              na.action=na.omit, data=predation_march)
modelc<-coxph(Surv(SurvMin, Status) ~ Block, na.action=na.omit,
              data=predation_march)
modeld<-coxph(Surv(SurvMin, Status) ~ MothColour + Block, na.action=na.omit,
              data=predation_march)

summary(modela)
summary(modelb)
summary(modelc)
summary(modeld)

##You can use AIC to see which model is best fit (smaller AIC=better fit)##
AIC(modeld,modelb,modelc)
##in this case it seems the block effect is huge so adding moth colour doesn't improve it at all!##

##you can change which of the colours is taken as the baseline in your model like this##
relevel(predation_march$MothColour, 2)

cox_fit <- survfit(modela)
cox_fit2 <- survfit(modelb)
cox_fit3 <- survfit(modelc)

plot(cox_fit)
plot(cox_fit2)
plot(cox_fit3)

library(ggplot2)
library(survminer)
library(car)

### Best plots so far
predation_march$MothColour<-recode(predation_march$MothColour, 
                                   '1'="Orange",'2'="Brown",'3'= "Green")
fit <- survfit(Surv(SurvMin, Status) ~ MothColour, na.action=na.omit, data = predation_march)
ggsurvplot(fit, legend.labs = levels(predation_march$MothColour),
           pval = TRUE)

fit2 <- survfit(Surv(SurvMin, Status) ~ Block, na.action=na.omit, data = predation_march) # Coloud * Block interaction not possible
ggsurvplot(fit2, legend.labs = levels(predation_march$Block), 
           pval = TRUE)



###Pairwise comparisons using Log-Rank test 
res <- pairwise_survdiff(Surv(SurvMin, Status) ~ MothColour,
                         data = predation_march)
res

res2 <- pairwise_survdiff(Surv(SurvMin, Status) ~  Block,
                          data = predation_march)
res2