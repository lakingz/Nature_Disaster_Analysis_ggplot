library('readr')
library("lubridate")
library('rgl')
library('ggplot2')
dator <- read_csv("C:/Users/ankai/Desktop/important saves/disaster analysis/data_pop_ud.csv")

dat <- dator[dator$`Province/Territory`=="MB",]
datplot <- data.frame(x=as.Date(dat$`EVENT START DATE`),y=dat$Population)

ggplot()+geom_point(data=datplot,aes(x,y),stat="identity")+
  labs(x="year" , y="population",title="Population in MB over Year")

