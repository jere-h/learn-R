# Exploratory Data Analysis of IBM HR Data
library(tidyverse)
library(data.table)
library(ggpubr)

df <- fread('cleaned_IBM_HR.csv', na.strings = c("NA", "missing", "N/A", -99, "", "na", "."), stringsAsFactors=T)

table(df$Attrition) # only 96 terminations, just exclude for now
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

# Make a numerical column to study attrition correlation?
df[,num_attrition := ifelse(Attrition=="Voluntary Resignation", 1, 0)]

# Plot Correlation heat map to check any unexpectedly low/high
# Only works with numeric values, so exclude categorical
df1 <- df[, which(sapply(df,is.numeric)), with=FALSE]
cormat <- round(cor(df1[complete.cases(df1)]),3)
# Get upper triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)


reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 2) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.5, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

# Weak correlations all around except for a few columns, zoom in
df1 <- df1 %>% select(JobLevel,MonthlyIncome,YearsSinceLastPromotion,YearsInCurrentRole,YearsAtCompany,YearsWithCurrManager,Age)
# Replot with selected columns
cormat <- round(cor(df1[complete.cases(df1)]),3)
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.5, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

# Years___ columns have expected high correlations with each other
# Monthly Income has decent correlation with YearsAtCompany
# JobLevel has expected correlations with income, years
# Age has surprisingly low correlation with joblevel, income
  
# Basic Plots against Attrition, using first few columns
# First drop rows with Attrition=NA
df <- df[!is.na(Attrition)]

plot1 <- df %>% 
  ggplot(aes(x=Age, color=Attrition)) +
  geom_density()

plot2 <- df %>%
  ggplot(aes(x=Age, y=Attrition)) +
  geom_jitter(size=1,alpha=0.2)

plot3 <- df[!is.na(BusinessTravel)] %>%
  ggplot(aes(x=BusinessTravel, y=Attrition)) +
  geom_count() +
  scale_size(range = c(4, 20))

plot4 <- df[!is.na(Department)] %>%
  ggplot(aes(x=Department, y=Attrition)) +
  geom_count() +
  scale_size(range = c(4, 20)) +
  scale_x_discrete(labels=c('HR','R&D','Sales'))

ggarrange(plot1, plot2, plot3, plot4,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow=2)

# From the above, we observe certain age ranges at risk of leaving
# Attrition also more likely in bigger departments eg R&D

# More exploratory plots to understand variables, since there is no documentation on what each column means
# Eg: Confirm if JobLevel is related to JobRole
df %>%
  ggplot(aes(x=JobRole, y=JobLevel)) +
  geom_count()
# JobLevel refers to the level in that role