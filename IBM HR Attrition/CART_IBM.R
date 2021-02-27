# CART Model
library(tidyverse)
library(caret)
library(rpart)
library(rattle)
library(MLmetrics)

df <- fread('cleaned_IBM_HR.csv', na.strings = c("NA", "missing", "N/A", -99, "", "na", "."), stringsAsFactors = T)
# only 96 terminations, just exclude 
df <- df[Attrition!="Termination"]

# convert 10 ordered factors
df$BusinessTravel <- factor(df$BusinessTravel, ordered=T, levels=c('Non-Travel','Travel_Rarely','Travel_Frequently'))
df$Education <- factor(df$Education, ordered=T, levels =c(1,2,3,4,5))
df$EnvironmentSatisfaction <- factor(df$EnvironmentSatisfaction, ordered=T, levels=c(1,2,3,4))
df$JobInvolvement <- factor(df$JobInvolvement, ordered=T, levels=c(1,2,3,4))
df$JobLevel <- factor(df$JobLevel, ordered=T, levels=c(1,2,3,4,5))
df$JobSatisfaction <- factor(df$JobSatisfaction, ordered=T, levels=c(1,2,3,4))
df$PerformanceRating <- factor(df$PerformanceRating, ordered=T, levels=c(3,4))
df$RelationshipSatisfaction <- factor(df$RelationshipSatisfaction, ordered=T, levels=c(1,2,3,4))
df$StockOptionLevel <- factor(df$StockOptionLevel, ordered=T, levels=c(0,1,2,3))
df$WorkLifeBalance <- factor(df$WorkLifeBalance, ordered=T, levels=c(1,2,3,4))

# Omit NA for now
df <- na.omit(df)

# Rename Employee Source
df <- subset(df[,EmployeeSource:=`Employee Source`], select=-c(`Employee Source`))

# Reset Attrition to 2 level factor
df$Attrition <- factor(df$Attrition)

# Seed for reproducible results
set.seed(123)

# Split dataset 85-15 since larger, test set of 3000+ still OK
# Stratified sampling to ensure same proportion of resignation ppl 
sample <- sample.split(df$Attrition, SplitRatio = .85) 
train <- subset(df, sample == TRUE)
test <- subset(df, sample == FALSE)
dim(train)
dim(test)

model1 <- train(Attrition~.,method="rpart",data=train) 
# Plot the trees
fancyRpartPlot(model1$finalModel)

# Evaluate Model Performance
# Train set
train.cart <- predict(model1)
table(train.cart, as.matrix(train$Attrition))
# Misclassification rate is 172+2735 / 19723 = 0.1474
F1_Score(as.matrix(train[,2]), train.cart) 
# F1 score: 0.9187558

# Test set
pred.cart <-predict(model1,newdata=test[,-2])
table(pred.cart,test$Attrition)
# Misclassif rate is 35+506 / 3481 = 0.1554 
F1_Score(as.matrix(test[,2]), pred.cart) 
# F1 score: 0.9145745

# Tune model further if needed ... 