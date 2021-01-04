library(gganimate)
library(ggplot2)
library(dplyr)
library(reshape2)
library(gifski)
library(ggthemes)
library(foreach)

setwd("C:/R/AnimateFish")
df=read.csv("FishKattegatt_CatchPerc_CPUE.csv", header=TRUE, sep=',', encoding = "Win-1252")

names(df) = c("Species", "Swe_Species", "Year", "CPUE", "PresencePercent", "Family")
#df2 = df[which(df$Year==2019),]

graph1 = df %>% 
  ggplot(aes(x=PresencePercent, y=log2(CPUE))) +
  geom_point(size = 8, colour = "white", alpha = 0.7)+
  geom_point(shape = 21, size = 9, colour = "black", stroke = 1.5, alpha = 0.7)+
  geom_text(aes(label=Swe_Species), hjust=0.5, vjust=1.9, size=5)+
  scale_size(range=c(2,12), guide="none")+   #Change the range of size for points, and then guide something?...
  theme_fivethirtyeight(13)+
  labs(title = "Kattegatt fish populations winter 1999-2019", 
       x = "Distrubution homogenity",
       y = "Normalized catch per unit effort")+
  #color = "Species"
  theme(axis.title = element_text(),
        legend.text = element_text(size = 10))+
  scale_color_brewer(palette = "Set2")


graph1.animation = graph1 +
  transition_time(Year)+
  labs(subtitle = "Year:{frame_time}")+
  theme(plot.subtitle=element_text(size=19,color="#424543", face="bold", hjust = 1))
#+
#shadow_wake(wake_length = 0.08)

#animate(graph1.animation, height = 800, width = 1600,  fps = 30, duration = 54, res = 100,end_pause = 210, start_pause = 210) #Create a longer more well paced graph, try it once the quick seems good. 
animate(graph1.animation, height = 800, width = 1600,  fps = 10, duration = 5, res = 100, start_pause = 20) #Create quick animation to check result