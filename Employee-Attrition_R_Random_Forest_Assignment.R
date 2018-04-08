## Author: Salil Agarwal
## Let us import the data that we intend to use for modeling
## Building the model using Random Forest

RFHR.dev <- read.table("C:/Users/Owner/Documents/Dev_HR_Employee_Attrition_Data.csv", sep = ",", header = T)
RFHR.holdout <- read.table("C:/Users/Owner/Documents/Holdout_HR_Employee_Attrition_Data.csv", sep = ",", header = T)
c(nrow(RFHR.dev), nrow(RFHR.holdout))

install.packages("randomForest")
library(randomForest)
?randomForest

## Calling syntax to build the Random Forest for DEV Sample
RF <- randomForest(as.factor(Target) ~ ., data = RFDF.dev[,-1], 
                   ntree=500, mtry = 3, nodesize = 10,
                   importance=TRUE)
print(RF)

plot(RF, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest RFDF.dev")

RF$err.rate

## Tuning Random Forest
tRF <- tuneRF(x = RFHR.dev[,-c(2)], 
              y=as.factor(RFHR.dev$Attrition),
              mtryStart = 3, 
              ntreeTry=100, 
              stepFactor = 1.5, 
              improve = 0.001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 10, 
              importance=TRUE
              )
library(rattle)
rattle()

## Scoring syntax
RFHR.dev$predict.class <- predict(tRF, RFHR.dev, type="class")
RFHR.dev$predict.score <- predict(tRF, RFHR.dev, type="prob")
head(RFHR.dev)

## deciling code
decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}


## deciling
RFHR.dev$deciles <- decile(RFHR.dev$predict.score[,2])

attach(RFHR.dev)
## Ranking code for HR data
library(data.table)
tmp_DT = data.table(RFHR.dev)
rank <- tmp_DT[, list(
  att = length(Attrition), 
  att_yes = sum(Attrition == "Yes"), 
  att_no = sum(Attrition == "No")) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$att_yes * 100 / rank$att,2);
rank$cum_att <- cumsum(rank$att_yes)
rank$cum_non_att <- cumsum(rank$att_no)
rank$cum_rel_att <- round(rank$cum_att / sum(rank$att_yes),2);
rank$cum_rel_non_att <- round(rank$cum_non_att / sum(rank$att_no),2);
rank$ks <- abs(rank$cum_rel_att - rank$cum_rel_non_att);
View(rank)

library(ROCR)
pred <- prediction(RFHR.dev$predict.score[,2], RFHR.dev$Attrition)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

library(ineq)
gini = ineq(RFHR.dev$predict.score[,2], type="Gini")

with(RFHR.dev, table(Attrition, predict.class))
auc
KS
gini

## Scoring syntax of Attrition class for Holdout Sample
RFHR.holdout$predict.class <- predict(tRF, RFHR.holdout, type="class")
RFHR.holdout$predict.score <- predict(tRF, RFHR.holdout, type="prob")
with(RFHR.holdout, table(Attrition, predict.class))

RFHR.holdout$deciles <- decile(RFHR.holdout$predict.score[,2])
tmp_DT = data.table(RFHR.holdout)
rank <- tmp_DT[, list(
  att = length(Attrition), 
  att_yes = sum(Attrition == "Yes"), 
  att_no = sum(Attrition == "No")) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$att_yes * 100 / rank$att,2);
rank$cum_att <- cumsum(rank$att_yes)
rank$cum_non_att <- cumsum(rank$att_no)
rank$cum_rel_att <- round(rank$cum_att / sum(rank$att_yes),2);
rank$cum_rel_non_att <- round(rank$cum_non_att / sum(rank$att_no),2);
rank$ks <- abs(rank$cum_rel_att - rank$cum_rel_non_att);
View(rank)

library(ROCR)
pred <- prediction(RFHR.holdout$predict.score[,2], RFHR.holdout$Attrition)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

library(ineq)
gini = ineq(RFHR.holdout$predict.score[,2], type="Gini")

with(RFHR.holdout, table(Attrition, predict.class))
auc
KS
gini


## C50
##install.packages("C50")
library(C50)
C50DF.dev <- read.table("C:/Users/Owner/Documents/Dev_HR_Employee_Attrition_Data.csv", sep = ",", header = T)
C50model <- C5.0(x = C50DF.dev[,-c(2)], 
                 y=as.factor(C50DF.dev$Attrition))
summary(C50model)
plot(C50model)

predict.class <- predict(C50model, C50DF.dev, type="class")
table(C50DF.dev$Target, predict.class)

