# CART Model
library(tidyverse)
library(caret)
library(rpart)
library(rattle)
library(MLmetrics)

df <- fread('cleaned_IBM_HR.csv', na.strings = c("NA", "missing", "N/A", -99, "", "na", "."), stringsAsFactors = T)
# only 96 terminations, just exclude 
df <- df[Attrition!="Termination"]

# using ordered factors gives var.L, var.Q, var.C instead of categories
# So, use unordered factors instead 
df$BusinessTravel <- factor(df$BusinessTravel, ordered=F, levels=c('Non-Travel','Travel_Rarely','Travel_Frequently'))
df$Education <- factor(df$Education, ordered=F, levels =c(1,2,3,4,5))
df$EnvironmentSatisfaction <- factor(df$EnvironmentSatisfaction, ordered=F, levels=c(1,2,3,4))
df$JobInvolvement <- factor(df$JobInvolvement, ordered=F, levels=c(1,2,3,4))
df$JobLevel <- factor(df$JobLevel, ordered=F, levels=c(1,2,3,4,5))
df$JobSatisfaction <- factor(df$JobSatisfaction, ordered=F, levels=c(1,2,3,4))
df$PerformanceRating <- factor(df$PerformanceRating, ordered=F, levels=c(3,4))
df$RelationshipSatisfaction <- factor(df$RelationshipSatisfaction, ordered=F, levels=c(1,2,3,4))
df$StockOptionLevel <- factor(df$StockOptionLevel, ordered=F, levels=c(0,1,2,3))
df$WorkLifeBalance <- factor(df$WorkLifeBalance, ordered=F, levels=c(1,2,3,4))

# Omit NA for now, to standardize dataset used in all models
df <- na.omit(df)

# Rename Employee Source
df <- subset(df[,EmployeeSource:=`Employee Source`], select=-c(`Employee Source`))

# Reset Attrition to 2 level factor
df$Attrition <- factor(df$Attrition)

# Seed for reproducible results
set.seed(123)

# Simple train-test split for comparison to other models
# Split dataset 85-15 since larger, test set of 3000+ still OK
# Stratified sampling to ensure same proportion of resignation ppl 
sample <- sample.split(df$Attrition, SplitRatio = .85) 
train <- subset(df, sample == TRUE)
test <- subset(df, sample == FALSE)
dim(train)
dim(test)

# Train basic CART model
cartmodel <- rpart(Attrition~., data = train,
                   method = 'class', 
                   control = rpart.control(cp = 0.0001))
plotcp(cartmodel)
# Retrieve of optimal cp value based on cross-validated error
index <- which.min(cartmodel$cptable[, "xerror"])
cp_optimal <- cartmodel$cptable[index, "CP"]
cp_optimal
# Prune with cp value that contains lowest xerror
model1 <- prune(cartmodel, cp=cp_optimal)

# Plot the trees (quite unintelligible, but a lot of features to consider, so kinda expected)
fancyRpartPlot(model1)

# Evaluate Model Performance
# Train set
train.cart <- predict(model1, type='class')
confusionMatrix(data = train.cart,       
                reference = train$Attrition)
table(train.cart, as.matrix(train$Attrition))
# Misclassification rate = 1-0.9825 = 0.0175
F1_Score(as.matrix(train[,2]), train.cart) 
# F1 score: 0.9896368

# Test set
pred.cart <- predict(model1,newdata=test[,-2], )
confusionMatrix(data = pred.cart,       
                reference = test$Attrition)
# Misclassif rate = 1-0.9698 = 0.0302 
F1_Score(as.matrix(test[,2]), pred.cart) 
# F1 score: 0.9820727

# Maybe Tune model hyperparams further: 
# Minsplit, Maxdepth, Complexity param (cp)

# If chosen to focus on CART over other models
# Can use K-fold cross validation to optimize CART model

