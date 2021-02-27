# Data Cleaning (IBM HR Data)
# Deal with duplicates, missing values, errors, convert datatypes

library(tidyverse)
library(data.table)
library(mice)

df <- fread("IBM HR Data.csv", na.strings = c("NA", "missing", "N/A", -99, "", "na", "."))
glimpse(df)
summary(df)

sum(duplicated(df)) # 14 duplicates, drop them
df <- df[!duplicated(df)]
sum(duplicated(df)) # now 0

# Check percentage of data missing 
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(df,2,pMiss) # in each column, less than 0.1% NA

# understand the missing value pattern
md.pattern(df, rotate.names=T) 
# 23299 out of 23532 not missing -> less than 1% problematic rows 
# Can ignore and proceed first, if performance unsatisfactory then impute

# Check for errors in each column
summary(df$Age)
densityplot(df$Age) # Nothing out of the ordinary
class(df$Age)

table(df$Attrition)
# categorical variable, so convert to factor
df$Attrition <- factor(df$Attrition)

table(df$BusinessTravel)
# Ordinal categorical, so convert to ordered factor
df$BusinessTravel <- factor(df$BusinessTravel, ordered=T, levels=c('Non-Travel','Travel_Rarely','Travel_Frequently'))
class(df$BusinessTravel)

hist(df$DailyRate) # quite evenly distributed, shouldnt it be more skewed?
# Plot some bivariate distributions
df %>% 
  ggplot(aes(x=YearsAtCompany, y=DailyRate)) +
  geom_point() +
  geom_smooth(method="lm")

df %>% 
  ggplot(aes(x=MonthlyRate, y=DailyRate)) +
  geom_point() +
  geom_smooth(method="lm")
# seems to be random distribution since data is fictional, ignore for now
# can use backward elimination later

table(df$Department) # what is 1296??
# Get row number to query row specifically
which(df$Department=="1296")
# check row 3794
df[3794] # values shifted 1 column from DailyRate onwards, Last col missing

# Fix the values
df[3794, c(4:36):=df[3794,c(5:37)]]
# Still need to fix last col, replace with most common value
table(df[,c(37)]) # Company Website
df[3794, c(37):="Company Website"]
# Lastly, Change Department to Factor since categorical
df$Department <- factor(df$Department)

# Check next col DistanceFromHome
summary(df$DistanceFromHome) # character type
table(df$DistanceFromHome) # all numeric
# Change to numeric
df$DistanceFromHome <- as.numeric(df$DistanceFromHome)
densityplot(df$DistanceFromHome) # not sure what units, could be km, doesnt matter

table(df$Education) # ordinal categorical, 5 is probably the highest edu
# Convert to ordered factor
df$Education <- factor(df$Education, ordered=T, levels =c(1,2,3,4,5))
class(df$Education)

table(df$EducationField) # What is Test? 
which(df$EducationField=="Test") # 16727
df[16727] # not a swapped field, so just replace with NA
df[16727, EducationField := NA]
# Categorical, so convert to factor
df$EducationField <- factor(df$EducationField)
class(df$EducationField)

summary(df$EmployeeCount) # character type
table(df$EmployeeCount) # 23513 out of 23518 are 1, other 5 seem to be NA
df[is.na(EmployeeCount), .N] # yup, NA
# No value for analysis, Drop Employee Count column
df <- df[, -EmployeeCount]

summary(df$EmployeeNumber) # character type
table(df$EmployeeNumber) # is an ID column, limited value for analysis
# Drop Employee Number column
df <- df[, -EmployeeNumber]

summary(df$`Application ID`) # character type
table(df$`Application ID`) # is an ID column, similarly, drop it
# Drop column
df <- df[, -`Application ID`]

summary(df$EnvironmentSatisfaction) # seems to have outliers
table(df$EnvironmentSatisfaction) # 1 outlier, check row
df[EnvironmentSatisfaction>4] # Many values jumbled up, just delete that row lol
df <- df[-which(EnvironmentSatisfaction>4)] 
# Ordinal Categorical, convert to ordered factor
df$EnvironmentSatisfaction <- factor(df$EnvironmentSatisfaction, ordered=T, levels=c(1,2,3,4))

table(df$Gender)
# Categorical, convert to factor
df$Gender <- factor(df$Gender)

summary(df$HourlyRate) # character type
table(df$HourlyRate) # numerical values, no characters
# Change to numeric
df$HourlyRate <- as.numeric(df$HourlyRate)
densityplot(df$HourlyRate) # even distrib, seems useless

summary(df$JobInvolvement)
table(df$JobInvolvement) # Likert scale 
# Convert to ordered factor
df$JobInvolvement <- factor(df$JobInvolvement, ordered=T, levels=c(1,2,3,4))

summary(df$JobLevel)
table(df$JobLevel) # Likert Scale
# Convert to ordered factor
df$JobLevel <- factor(df$JobLevel, ordered=T, levels=c(1,2,3,4,5))

table(df$JobRole) # Categorical
# Convert to factor
df$JobRole <- factor(df$JobRole)

summary(df$JobSatisfaction) # Character
table(df$JobSatisfaction) # Likert Scale
# Convert to ordered factor
df$JobSatisfaction <- factor(df$JobSatisfaction, ordered=T, levels=c(1,2,3,4))

table(df$MaritalStatus)
# Convert to factor
df$MaritalStatus <- factor(df$MaritalStatus)

summary(df$MonthlyIncome) # Character, but numeric
df$MonthlyIncome <- as.numeric(df$MonthlyIncome)
densityplot(df$MonthlyIncome) # more realistic skewed distribution, can keep

summary(df$MonthlyRate)
densityplot(df$MonthlyRate) # evenly distributed, seems useless

table(df$NumCompaniesWorked) 
class(df$NumCompaniesWorked)

table(df$Over18) # uniform values, can drop this column
df <- df[, -Over18]

table(df$OverTime) 
# Convert to factor
df$OverTime <- factor(df$OverTime)

summary(df$PercentSalaryHike) # Character, should be numeric
df$PercentSalaryHike <- as.numeric(df$PercentSalaryHike)
summary(df$PercentSalaryHike)
densityplot(df$PercentSalaryHike) # Long tailed distrib looks usable

table(df$PerformanceRating) # only 2 ratings lol still usable i guess
class(df$PerformanceRating) 
# Convert to ordered factor
df$PerformanceRating <- factor(df$PerformanceRating, ordered=T, levels=c(3,4))

table(df$RelationshipSatisfaction) # Likert Scale
# Convert to ordered factor
df$RelationshipSatisfaction <- factor(df$RelationshipSatisfaction, ordered=T, levels=c(1,2,3,4))

table(df$StandardHours) # All 80.. useless column
# Drop this column
df <- df[, -StandardHours]

table(df$StockOptionLevel) # Ordinal categorical
# Convert to ordered factor
df$StockOptionLevel <- factor(df$StockOptionLevel, ordered=T, levels=c(0,1,2,3))

table(df$TotalWorkingYears)
class(df$TotalWorkingYears)
densityplot(df$TotalWorkingYears) # looks usable

table(df$TrainingTimesLastYear)
class(df$TrainingTimesLastYear)
hist(df$TrainingTimesLastYear) # looks usable

table(df$WorkLifeBalance) # Looks like likert scale
# Convert to ordered factor
df$WorkLifeBalance <- factor(df$WorkLifeBalance, ordered=T, levels=c(1,2,3,4))

summary(df$YearsAtCompany)
densityplot(df$YearsAtCompany) # looks usable

summary(df$YearsInCurrentRole)
densityplot(df$YearsInCurrentRole) # looks usable

summary(df$YearsSinceLastPromotion)
densityplot(df$YearsSinceLastPromotion) # looks usable

summary(df$YearsWithCurrManager)
densityplot(df$YearsWithCurrManager) # looks usable

table(df$`Employee Source`) # 1 entry of "Test", check that row
df[`Employee Source`=="Test"] # No other obvious errors in the row
# Replace with NA
df[`Employee Source`=="Test", `Employee Source`:=NA]

# First pass through all the columns done
# Next, do Exploratory Data Analysis
# Might come across anomalies when doing multivariate plots
# Can perform ad-hoc data cleaning on the fly

# Export the cleaned dataset
finaldf <- df
# write.csv(finaldf, "cleaned_IBM_HR.csv", row.names=FALSE)
# Factor columns must be re-converted when loading csv 