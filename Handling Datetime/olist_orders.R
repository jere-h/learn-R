library(tidyverse)
library(data.table)

df <- fread('olist_orders_dataset.csv', na.strings=c("na","NA"))
# big dataset, make it smaller to experiment with
df <- df[1:9999]

str(df)

date_cols <- c(4:8)
setDT(df)[, (date_cols) := lapply(.SD, as.POSIXct, tz="UTC", format="%Y-%m-%d %H:%M:%S"), .SDcols = date_cols]

str(df)

df[, purchase_to_delivery := df[,7] - df[,4]]
df$purchase_to_delivery <- as.double(df$purchase_to_delivery, units="days")
str(df)

df[purchase_to_delivery<=0] # no rows are negative

summary(df$purchase_to_delivery)

df[which(is.na(df$purchase_to_delivery))] # look at some NA rows

df[purchase_to_delivery<=20, .N] / df[!is.na(purchase_to_delivery),.N]
# 84.9% of customers received order in 20 days or less

df[, purchase_mth := month(order_purchase_timestamp)]

df %>%
  ggplot(aes(x=purchase_mth,y=purchase_to_delivery)) +
  geom_bar(stat="summary", fun="mean") 

sum(duplicated(df))
