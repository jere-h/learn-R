# Logistic Regressiom
library(MLmetrics)
library(caTools)
library(data.table)

df <- fread('cleaned_IBM_HR.csv', na.strings = c("NA", "missing", "N/A", -99, "", "na", "."), stringsAsFactors = T)
# only 96 terminations, just exclude 
df <- df[Attrition!="Termination"]

# For GLM, using ordered factors gives var.L, var.Q, var.C instead of categories
# So, use unordered factors instead for glm, but set levels
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

levels(train$Attrition) # Baseline = "Current employee" 

lrmodel <- glm(Attrition ~ ., family=binomial, data=train)

# Check significance (***)
summary(lrmodel) # Can consider removing insignificant feature cols
# Eg: PerformanceRating, MonthlyRate, MonthlyIncome, 
# TotalWorkingYears, YearsWithCurrManager, EmployeeSource

# Already uses lowest level of factor variables as baseline, and excludes it
# Eg: GenderFemale not included (OR set to=1)
OR <- exp(coef(lrmodel)) 

# Most are categorical variables, so OR relative to base level (eg. OverTime="No" is base)
sort(OR, decreasing=T) # OverTime=Yes 2.5x more likely than "No" to resign?
levels(train$BusinessTravel) # BizTravel=frequently 4x more likely than "non-travel" to resign?

# conf interval
OR.CI <- exp(confint(lrmodel))
OR.CI 
# If CI includes 1, that category is insignificant (minimal effect on Y)
# CI includes 1: DepartmentSales, Education3, Education5, EducationField... 

# Output logistic function probabilities
prob <- predict(lrmodel, type='response')

# Set threshold for predicting Y
# One choice is to use proportion of resignations (around 15%)
threshold <- sum(train$Attrition == "Voluntary Resignation")/length(train$Attrition)

# If probability > threshold, predict Y=1
trainpred <- ifelse(prob > threshold, "Voluntary Resignation", "Current employee")

# Confusion matrix: Diagonals are correct predictions
table(train$Attrition, trainpred)
prop.table(table(train$Attrition, trainpred)) # proportions

# Train set accuracy
mean(trainpred == train$Attrition) # 69.3%

# Predict on test set
testprob <- predict(lrmodel, newdata=test[,-2])
testpred <- ifelse(testprob > threshold, "Voluntary Resignation", "Current employee")

table(test$Attrition, testpred)
prop.table(table(test$Attrition, testpred)) # proportions

# Test set accuracy
mean(testpred == test$Attrition) # 85.2%
