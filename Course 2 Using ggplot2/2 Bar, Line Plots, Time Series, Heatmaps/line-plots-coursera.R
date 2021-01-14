library(tidyverse)

####create a sequence of years

years <- seq(from=2001,to=2020,by=1)

####create "fake" data for price (note, your values will be different)

price <- rnorm(20,mean=15,sd=5)

####put years and price together

fig_data <- tibble("year"=years,"stock_price"=price)

ggplot(fig_data,(aes(x=years,y=price)))+
  geom_line()

####make data for the first of two stocks

fig_data$stock_id=rep("Stock_1",20)

stock_1_time_series <- fig_data

#####create data for the second company
########same approach as with the last company
stock_id <- rep("Stock_2",20)

years <- seq(from=2001,to=2020,by=1)

price <- rnorm(20,mean=10,sd=3)

stock_2_time_series <- tibble("stock_id"=stock_id,"year"=years,"stock_price"=price)

####combine with bind_rows()

all_stocks_time_series <- bind_rows(stock_1_time_series,stock_2_time_series)

View(all_stocks_time_series)

####make the plot, setting group to stock_id

ggplot(all_stocks_time_series,(aes(x=year,y=stock_price,group=stock_id)))+
  geom_line()

####modify group, linetype, color, and add facet_wrap()

ggplot(all_stocks_time_series,(aes(x=year,y=stock_price,group=stock_id,linetype=stock_id,color=stock_id)))+
  geom_line()+
  facet_wrap(~stock_id)

#####Practice with another data set

cel <- read_csv(url("https://www.dropbox.com/s/4ebgnkdhhxo5rac/cel_volden_wiseman%20_coursera.csv?raw=1"))

cel$Party <- recode(cel$dem,`1`="Democrat",`0`="Republican")

fig_data <- cel %>% 
  group_by(Party,year) %>% 
  summarize("Ideology"=mean(dwnom1,na.rm=T))
  
View(fig_data)

ggplot(fig_data,(aes(x=year,y=Ideology,group=Party,color=Party)))+
  geom_line()+
  scale_color_manual(values=c("blue","red"))
  

