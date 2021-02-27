# Dimensionality Reduction to observe correlation
# Multiple Correspondence Analysis
library(data.table)
library(FactoMineR)
library(factoextra)

df <- fread('cleaned_IBM_HR.csv', na.strings = c("NA", "missing", "N/A", -99, "", "na", "."), stringsAsFactors = T)

# only 96 terminations, just exclude 
df <- df[Attrition!="Termination"]

# 10 ordered factors
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

# For MCA, only use categorical variables
# For now, just omit non-categorical, in future can bin into categories
df1 <- df[, which(sapply(df,is.factor)), with=FALSE]
str(df1)

# Qualitative supplementary variable is target var applied after MCA
df.mca <- MCA(na.omit(df1),
               quali.sup=c(1),
               graph=FALSE)

# Plot MCA results
fviz_mca_var(df.mca, repel=TRUE)
# Observe variables near to target variable (Voluntary Resignation)
# Eg: Jora (employee source), Education_1, StockOptionLevel_0, 
# Indicating that these 3 attributes are correlated with Resignation

# Try PCA with numeric variables
# Select only numeric variables
df2 <- df[, which(sapply(df,is.numeric)), with=FALSE]
str(df2)

df.pca <- PCA(df2,
              scale.unit = TRUE,
              graph=FALSE)

# Plot variance explained 
fviz_eig(df.pca, addlabels = TRUE, ylim = c(0, 40))
summary(df.pca) # 9 out of 14 dimensions will explain 85.9% of variance
# Can choose to retain first 9 principal components for further models

# Plot correlation circle
fviz_pca_var(df.pca, repel=TRUE)
# Positively correlated variables are grouped together
# Negatively correlated variables are oppositely positioned
# Distance from origin is quality 
