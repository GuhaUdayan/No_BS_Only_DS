## Author: Salil Agarwal

nn.dev <- read.table("C:/Users/Owner/Documents/Dev_HR_Employee_Attrition_Data.csv", sep = ",", header = T)
nn.holdout <- read.table("C:/Users/Owner/Documents/Holdout_HR_Employee_Attrition_Data.csv", sep = ",", header = T)

c(nrow(nn.dev), nrow(nn.holdout))
str(nn.dev)

## Installing the Neural Net package; 
## If already installed do not run the below step
install.packages("neuralnet")

##Converting the Dev Sample categorical variables to numeric data type for Neural Network Model
nn.dev$Attrition.f = as.numeric(factor(nn.dev$Attrition, levels=c("Yes","No")))
nn.dev$OverTime.f = as.numeric(factor(nn.dev$OverTime, levels=c("Yes","No")))
nn.dev$JobRole.f = as.numeric(factor(nn.dev$JobRole, levels=c("Healthcare Representative",
                                                              "Human Resources",
                                                              "Laboratory Technician",
                                                              "Manager",
                                                              "Manufacturing Director",
                                                              "Research Director",
                                                              "Research Scientist",
                                                              "Sales Executive",
                                                              "Sales Representative")))

nn.dev$Attrition.f <- recode(nn.dev$Attrition.f,"'1'=1;'2'=0")
nn.dev$OverTime.f <- recode(nn.dev$OverTime.f,"'1'=1;'2'=0")

str(nn.dev)
summary(nn.dev)

library(car)

##Dropping columns from dataset
##nn.dev = subset(nn.dev,select = -c(37))

library(neuralnet)
nn1$startweights
##nn.dev$Balance_Scaled = nn.dev$Balance/10000 

##Building Neural Network model for Dev sample
nn1 <- neuralnet(formula = Attrition.f ~ OverTime.f+JobLevel+JobRole.f, 
                 data = nn.dev, 
                 hidden = 2,
                 err.fct = "sse",
                 linear.output = FALSE,
                 lifesign = "full",
                 lifesign.step = 10,
                 threshold = 0.1,
                 stepmax = 2000
)

plot (nn1)

quantile(nn1$net.result[[1]], c(0,1,5,10,25,50,75,90,95,99,100)/100)
## The distribution of the estimated results

misClassTable = data.frame(Attrition = nn.dev$Attrition.f, Prediction = nn1$net.result[[1]] )
misClassTable$Classification = ifelse(misClassTable$Prediction>0.50,1,0)
with(misClassTable, table(Attrition, Classification))

## We can use the confusionMatrix function of the caret package 
##install.packages("caret")
##library(caret)
confusionMatrix(misClassTable$Attrition, misClassTable$Classification)

##Validating the Holdout sample

nn.holdout$Attrition.f = as.numeric(factor(nn.holdout$Attrition, levels=c("Yes","No")))
nn.holdout$OverTime.f = as.numeric(factor(nn.holdout$OverTime, levels=c("Yes","No")))
nn.holdout$JobRole.f = as.numeric(factor(nn.holdout$JobRole, levels=c("Healthcare Representative",
                                                              "Human Resources",
                                                              "Laboratory Technician",
                                                              "Manager",
                                                              "Manufacturing Director",
                                                              "Research Director",
                                                              "Research Scientist",
                                                              "Sales Executive",
                                                              "Sales Representative")))

nn.holdout$Attrition.f <- recode(nn.holdout$Attrition.f,"'1'=1;'2'=0")
nn.holdout$OverTime.f <- recode(nn.holdout$OverTime.f,"'1'=1;'2'=0")

str(nn.holdout)

nn2 <- neuralnet(formula = Attrition.f ~ OverTime.f+JobLevel+JobRole.f, 
                 data = nn.holdout, 
                 hidden = 2,
                 err.fct = "sse",
                 linear.output = FALSE,
                 lifesign = "full",
                 lifesign.step = 10,
                 threshold = 0.1,
                 stepmax = 2000
)

plot (nn2)

quantile(nn2$net.result[[1]], c(0,1,5,10,25,50,75,90,95,99,100)/100)
## The distribution of the estimated results

misClassTable1 = data.frame(Attrition = nn.holdout$Attrition.f, Prediction = nn2$net.result[[1]] )
misClassTable1$Classification = ifelse(misClassTable1$Prediction>0.50,1,0)
with(misClassTable1, table(Attrition, Classification))

## We can use the confusionMatrix function of the caret package 
##install.packages("caret")
##library(caret)
confusionMatrix(misClassTable1$Attrition, misClassTable1$Classification)

## Scoring holdout dataset using the Neural Net Model Object
## To score we will use the compute function
x <- subset(nn.holdout, 
            select = c("OverTime.f","JobLevel", "JobRole.f")
)
##x.scaled <- scale(x)
compute.output = compute(nn1, x)
nn.holdout$Predict.score = compute.output$net.result
View(nn.holdout)
quantile(nn.holdout$Predict.score, c(0,1,5,10,25,50,75,90,95,99,100)/100)

misClassTable2 = data.frame(Attrition = nn.holdout$Attrition.f, Prediction = nn.holdout$Predict.score )
misClassTable2$Classification = ifelse(misClassTable2$Prediction>0.50,1,0)
with(misClassTable2, table(Attrition, Classification))

## We can use the confusionMatrix function of the caret package 
##install.packages("caret")
##library(caret)
confusionMatrix(misClassTable2$Attrition, misClassTable2$Classification)