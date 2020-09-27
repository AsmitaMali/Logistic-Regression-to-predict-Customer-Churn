


#Let us first set the working directory path and import the data

setwd('D:/Personal/Predictive modelling/group')
getwd()

#reading the data
cellphone  <- read.csv("cellphone.csv", header=TRUE)

summary(cellphone)
#no missing data

str(cellphone)

#datatype change required
cellphone$Churn <- as.factor(cellphone$Churn)
cellphone$ContractRenewal <- as.factor(cellphone$ContractRenewal)
cellphone$DataPlan <- as.factor(cellphone$DataPlan)

#missing values
library(DataExplorer)
plot_missing(cellphone)

?plot_missing

#understadning distribution 
library(funModeling)
plot_num(cellphone)



attach(cellphone)


#CHECK OULIERS


boxplot(AccountWeeks)
boxplot(DataUsage)
boxplot(DayMins)
boxplot(DayCalls)
boxplot(MonthlyCharge)
boxplot(OverageFee)
boxplot(RoamMins)


#all variables has outliers

#treat outliers with Winsorize


library(DescTools)
?Winsorize

cellphone$AccountWeeks <- Winsorize(cellphone$AccountWeeks)
cellphone$DataUsage <- Winsorize(cellphone$DataUsage)
cellphone$DayMins <- Winsorize(cellphone$DayMins)
cellphone$DayCalls <- Winsorize(cellphone$DayCalls)
cellphone$MonthlyCharge <- Winsorize(cellphone$MonthlyCharge)
cellphone$OverageFee <- Winsorize(cellphone$OverageFee)
cellphone$RoamMins <- Winsorize(cellphone$RoamMins)

#if we now plot the box plot we see there is  no outlier
boxplot(cellphone$AccountWeeks)
boxplot(cellphone$DataUsage)
boxplot(cellphone$DayMins)
boxplot(cellphone$DayCalls)
boxplot(cellphone$MonthlyCharge)
boxplot(cellphone$OverageFee)
boxplot(cellphone$RoamMins)



#check the how each variable impacts Churn

# barplot(DayCalls~AccountWeeks)
boxplot(cellphone$AccountWeeks~cellphone$Churn)
t.test(cellphone$AccountWeeks[cellphone$Churn==1],cellphone$AccountWeeks[cellphone$Churn==0])
#individually no impact


boxplot(cellphone$DataUsage~cellphone$Churn)
t.test(cellphone$DataUsage[cellphone$Churn==1],cellphone$DataUsage[cellphone$Churn==0])
#impacts



boxplot(DayMins~Churn)
t.test(DayMins[Churn==1],DayMins[Churn==0])
#impacts


boxplot(DayCalls~Churn)
t.test(DayCalls[Churn==1],DayCalls[Churn==0])
#noimpact


boxplot(MonthlyCharge~Churn)
t.test(MonthlyCharge[Churn==1],MonthlyCharge[Churn==0])
#impact

boxplot(OverageFee~Churn)
t.test(OverageFee[Churn==1],OverageFee[Churn==0])
#impact

boxplot(RoamMins~Churn)
t.test(RoamMins[Churn==1],RoamMins[Churn==0])
#impact

chisq.test(table(ContractRenewal,Churn))
chisq.test(table(DataPlan,Churn))
#iMPACT


#DayCalls #AccountWeeks does not impact

library(GGally)
?ggcorr

#removing no-numeric columns
ggcorr(cellphone[,-c(1,3,4,6)])

#Correlated DataUsage with MonthlyCharge
#Correlated DayMins with MonthlyCharge


#test and train

#two sample 70 :30 training and testing
#total 3333 obs 70% = 2333

set.seed(1212)
s <- sample(c(1:3333), size = 2333)
cellphone.train <- cellphone[s,]
cellphone.test <- cellphone[-s,]

#to see if the split has happened properly
nrow(cellphone.train)
nrow(cellphone.test)

model.1 <- glm(Churn~., family = "binomial", data = cellphone.train)
summary(model.1)
library(car)
?vif
vif(model.1)

#removed insignificant variables
#AccountWeeks, DataPlan1, DayCalls            

#removed highly correlated variable, this can be seen from correlated plot as well
#MonthlyCharge


model.2 <- glm(Churn~., family = "binomial", data = cellphone.train[,c(1,3,5,6,7,10,11)])
summary(model.2)

#vif model 2
vif(model.2)

#all signifincant variables, vif all small

# assigning  probablities
cellphone.train$fittedvalue <-model.2$fitted.values


# The distribution of the estimated probabilities
hist(cellphone.train$fittedvalue[cellphone.train$Churn==0])
hist(cellphone.train$fittedvalue[cellphone.train$Churn==1])


#Response Rate
nrow(cellphone.train[cellphone.train$Churn=="1",])/nrow(cellphone.train)
nrow(cellphone.test[cellphone.test$Churn=="1",])/nrow(cellphone.test)

#the threshold used on probabilities is 15% as this was probability prior to buidling the model
cellphone.train$prediction <- ifelse(cellphone.train$fittedvalue >0.15, 1, 0)

#confusion matrix
library(caret)
library(e1071)
confusionMatrix( as.factor(cellphone.train$prediction),as.factor(cellphone.train$Churn))

# Other Model Performance Measures 
library(ROCR)
library(ineq)
pred <- ROCR::prediction(cellphone.train$fittedvalue, cellphone.train$Churn)
perf <- performance(pred, "tpr", "fpr")
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
gini = ineq(cellphone.train$fittedvalue, type="Gini")

with( cellphone.train, table(Churn, as.factor(prediction)  ))
auc
KS
gini

gini = 2 * auc - 1



#testing

cellphone.test$fittedvalue <-predict(model.2, newdata=cellphone.test, type = "response")

## Assgining 0 / 1 class based on certain threshold
cellphone.test$prediction <- ifelse(cellphone.test$fittedvalue >0.15, 1, 0)


#Confusion Matrix testing
confusionMatrix( as.factor(cellphone.test$prediction),as.factor(cellphone.test$Churn))


# Other Model Performance Measures  - testing
pred <- ROCR::prediction(cellphone.test$fittedvalue, cellphone.test$Churn)
perf <- performance(pred, "tpr", "fpr")
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
gini = ineq(cellphone.test$fittedvalue, type="Gini")

with( cellphone.test, table(Churn, as.factor(prediction)  ))
auc
KS
gini

gini = 2 * auc - 1



#training and testing model perfomance comparable
#Which model performed the best - testing model



#Actionable Insights and Recommendations

#Execute the model every month, filter customers which are predicated as risky for churn, offer incentives alternative data plans so that they don't switch.
#model will help to focus on high risky customers instead of all customers, help to reduce the churn

