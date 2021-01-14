library(tidyverse)

########make a scatter plot of lifetime high scores in tetris against cumulative hours playing tetris
############likely to see positive correlation

kid <- c("Nick","Jessica","Justin","Brandi","Kelly","Enrique")
time_spent <- c(40,35,25,20,10,5)
high_score <- c(100,75,85,50,25,30)

tetris <- tibble(kid,time_spent,high_score)

ggplot(tetris,aes(x=time_spent,y=high_score))+
  geom_point()

ggplot(tetris,aes(x=time_spent,y=high_score))+
  geom_point()+
  geom_text(aes(label=kid))
     
ggplot(tetris,aes(x=time_spent,y=high_score))+
  #geom_point()+
  geom_text(aes(label=kid))

####look for help
?geom_text()

####push the text away from the point
ggplot(tetris,aes(x=time_spent,y=high_score))+
  geom_point()+
  geom_text(aes(label=kid),nudge_y=5)

######make a plot using Congress data

######plot political ideology and how many bills a member passes

#####dwnom1 on the x axis, all_pass on the y axis

cel<-drop_na(read_csv(url("https://www.dropbox.com/s/4ebgnkdhhxo5rac/cel_volden_wiseman%20_coursera.csv?raw=1")))

#####using piping here to feed the data to ggplot
cel %>% filter(congress==115) %>% 
  ggplot(aes(x=dwnom1,y=all_pass,label=thomas_name))+
  geom_point()+
  geom_text()

cel %>% filter(congress==115) %>% 
  ggplot(aes(x=dwnom1,y=all_pass,label=thomas_name))+
  geom_point()+
  ######specify here that you only want the geom_text to apply to a subset of the data
  geom_text(data=filter(cel,congress==115 & all_pass>8))

###install.packages("ggrepel")
library(ggrepel)

cel %>% filter(congress==115) %>% 
  ggplot(aes(x=dwnom1,y=all_pass))+
  geom_point()+
  geom_text_repel(data=filter(cel,congress==115 & all_pass>8),mapping=aes(x=dwnom1,y=all_pass,label=thomas_name))

cel %>% filter(congress==115) %>% 
  ggplot(aes(x=dwnom1,y=all_pass))+
  geom_point()+
  geom_text_repel(filter(cel,congress==115 & all_pass>8),mapping=aes(x=dwnom1,y=all_pass,label=thomas_name))+
  annotate("rect",xmin=.05,xmax=.4,ymin=13,ymax=15,alpha=.2,fill="red")+
  annotate("text",x=.6,y=14,label="Most Passed",color="red")


