# Random Forest
library(MLmetrics)
library(caTools)
library(data.table)
library(randomForest)
library(pROC)
library(ROCR)
install.packages("reprtree")

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

# Using random forest model 
# bigger dataset size -> need more trees (ntree) to sample entire dataset
# more features -> sample more variables (mtry)
rf <- randomForest(Attrition ~ ., data=train, 
                   ntree=400, mtry=8)
rf
plot(rf) 

# Get F1 score for train set results
pred <- predict(rf)
# Confusion matrix for train set
table(as.matrix(train[,2]), pred)
# Misclassification rate: 42+8/19723 = 0.0025
F1_Score(as.matrix(train[,2]), pred) # 0.9984963

# Get AUC for train set
rf_p_train <- predict(rf, type="prob")[,2]
rf_pr_train <- prediction(rf_p_train, train$Attrition)
r_auc_train1 <- performance(rf_pr_train, measure = "auc")@y.values[[1]] 
r_auc_train1  # 0.9999226

# Predict on test set, exclude Attrition column
pred <- predict(rf, newdata=test[,-2])
# Confusion matrix for test set
table(as.matrix(test[,2]), pred) 
# Misclassification rate: 6/3481 = 0.0017
# Get F1 score for test set
F1_Score(as.matrix(test[,2]), pred) # 0.9989775

# Get AUC for test set
rf_p_test <- predict(rf, type="prob", newdata=test[,-2])[,2]
rf_pr_test <- prediction(rf_p_test, test$Attrition)
r_auc_test1 <- performance(rf_pr_test, measure = "auc")@y.values[[1]] 
r_auc_test1  # 0.9998663

# Visualize a tree
# Install reprtree package
# install.packages("devtools")
# install.packages("installr")
# library(devtools)
# library(installr)
# devtools::install_github('skinner927/reprtree')

library(reprtree)
reprtree:::plot.getTree(rf) # Full tree is too messy
reprtree:::plot.getTree(rf, depth=4)
# By depth 4, can already classify some samples

# Getting importance of variables
impt <- importance(rf)
# Rank based on decreasing importance, can use in feature selection
# Least important features are performance rating, gender
impt[order(impt[,1],decreasing=TRUE),]
barplot(impt[order(impt[,1],decreasing=TRUE),])

# Try using only top 12 features, see if performance improves
# ie: Age, DailyRate, HourlyRate, DistanceFromHome, EmployeeSource, MonthlyIncome, JobRole, MonthlyRate, TotalWorkingYears, PercentSalaryHike, YearsAtCompany, Education

df_subset <- select(df, c(Age, Attrition, DailyRate, HourlyRate, DistanceFromHome, EmployeeSource, MonthlyIncome, JobRole, MonthlyRate, TotalWorkingYears, PercentSalaryHike, YearsAtCompany, Education))

sample2 <- sample.split(df_subset$Attrition, SplitRatio = .85) 
train2 <- subset(df_subset, sample2 == TRUE)
test2 <- subset(df_subset, sample2 == FALSE)

rf2 <- randomForest(Attrition ~ ., data=train2, 
                   ntree=400, mtry=8)

# Get F1 score for train set with reduced features
pred2 <- predict(rf2)
# Confusion matrix for train set
table(as.matrix(train2[,2]), pred2)
# Misclassification rate: 41+4/19723 = 0.0023 slightly better than all features
F1_Score(as.matrix(train2[,2]), pred2) # 0.9986468 slightly better

# Get AUC for train set with reduced features
rf_p_train2 <- predict(rf2, type="prob")[,2]
rf_pr_train2 <- prediction(rf_p_train2, train2$Attrition)
r_auc_train2 <- performance(rf_pr_train2, measure = "auc")@y.values[[1]] 
r_auc_train2  # 0.9999436 slightly better

# Predict on test set with reduced features
pred2 <- predict(rf2, newdata=test2[,-2])
# Confusion matrix for test set
table(as.matrix(test2[,2]), pred2) 
# Misclassification rate: 8/3481 = 0.0023 slightly worse than all features
# Get F1 score for test set with reduced features
F1_Score(as.matrix(test2[,2]), pred2) # 0.9986371 slightly worse

# Get AUC for test set with reduced features
rf_p_test2 <- predict(rf2, type="prob", newdata=test2[,-2])[,2]
rf_pr_test2 <- prediction(rf_p_test2, test2$Attrition)
r_auc_test2 <- performance(rf_pr_test2, measure = "auc")@y.values[[1]] 
r_auc_test2  # 0.9997916 slightly worse

# Reducing X from 31 to 12 features improves train perf, but worsens test

# Tune model further if needed ... 
