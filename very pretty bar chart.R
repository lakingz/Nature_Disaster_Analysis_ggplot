#
# very pretty bar chart
#
#

library('readr')
library("lubridate")
library('rgl')
library('ggplot2')
dator <- read_csv("C:/Users/ankai/Desktop/important saves/disaster analysis/data_pop_ud.csv")
dator$`EVENT START DATE`[131] <- "2018-01-01"

datplot <- dator[dator$`Province/Territory`=="ON",]
datplot <- data.frame(x=datplot$`EVENT TYPE`,y=datplot$`EVENT START DATE`)
ggplot(datplot, aes(x)) + geom_bar(width=0.8,aes(fill = as.character(strftime(datplot$y,"%m"))))+
  labs(x='Disaster in ON.(province)',y='count',fill="month")

datplot <- dator[dator$`EVENT TYPE`=="Wildfire",]
datplot <- data.frame(x=datplot$`Province/Territory`,y=datplot$`EVENT START DATE`)
ggplot(datplot, aes(x)) + geom_bar(width=0.8,aes(fill = as.character(strftime(datplot$y,"%m"))))+
  labs(x='Wildfire VS. Province',y='count',fill="month")
#
# because for hist it do not except character x-veriable. so I only ploted month
# 
# if ew can find a way to converge character into numeric then we can generate more nicer plots
#
datplot_on <- dator[dator$`Province/Territory`=="ON",]
datplot_mb <- dator[dator$`Province/Territory`=="MB",]
p1 <- hist(as.numeric(strftime(datplot_on$`EVENT START DATE`,"%m")),breaks=12)
p2 <- hist(as.numeric(strftime(datplot_mb$`EVENT START DATE`,"%m")),breaks=12)
plot( p1, col=rgb(0,0,1,1/4), xlab="Month",main= "Event Starting Month in ON VS. MB")
plot( p2, col=rgb(1,0,0,1/4), add=T)
legend('topright',c("ON","MB"),fill = c(rgb(0,0,1,1/4),rgb(1,0,0,1/4)),bty='n', border = NA)


#
# Wildfire VS. Flood in time series 
#
unique(dator$`Province/Territory`)
dat <- dator               
datplot <- data.frame(x=as.Date(dat$`EVENT START DATE`),
                      y=(dat$`INJURED/INFECTED`+dat$FATALITIES+dat$EVACUATED)/(dat$Population),
                      type=dat$`EVENT TYPE`,
                      month=as.numeric(strftime(dat$`EVENT START DATE`,"%m")))
datplot <- datplot[datplot$y < 0.001,]

ggplot(datplot,aes(x,y))+ geom_point(aes(color=as.character(datplot$type)))+
  labs(x='Year',y='Human factors',color="Event type")


unique(dator$`Province/Territory`)
dat <- dator[dator$`Province/Territory`=="MB", ] 

ggplot(datplot,aes(x,type))+ geom_point(aes(size=y,color=as.character(month)))+
  labs(x='Year',y='dasister type',size="Human factor",color="month")

dat <- dator[dator$`Province/Territory`=="NL",]
datplot <- data.frame(x=dat$`EVENT TYPE`,y=dat$FATALITIES,z=dat$Population,zz=dat$`EVENT START DATE`)
ggplot(datplot,aes(zz,y))+ geom_point(aes(color=as.character(x)))+
  labs(x='Year',y='dasister type',color="Event type")

dat <- dator[dator$FATALITIES<10000 & dator$FATALITIES>=1,]
datplot <- data.frame(x=dat$`EVENT TYPE`,y=dat$FATALITIES,z=dat$Population,zz=dat$`EVENT START DATE`)
ggplot(datplot,aes(zz,y))+ geom_point(aes(color=as.character(x)))+
  labs(x='Year',y='dasister type',color="Event type")
test <- dat[dat$FATALITIES>100,]
test

unique(dator$`Province/Territory`)
test <- dator[dator$FATALITIES>1000,]
test <- dator[dator$`Province/Territory`=="Across Canada",]
test


dat_on <- dator[dator$`Province/Territory`=="NL" & as.numeric(strftime(dator$`EVENT START DATE`,"%Y"))<=1982,]
dat_mb <- dator[dator$`Province/Territory`=="NL" & as.numeric(strftime(dator$`EVENT START DATE`,"%Y"))>1982,]
datplot_on <- data.frame(x=c(dat_on$EVACUATED,dat_on$FATALITIES),y=c(rep("E",nrow(dat_on)),rep("F",nrow(dat_on))))
datplot_mb <- data.frame(x=c(dat_mb$EVACUATED,dat_mb$FATALITIES),y=c(rep("E",nrow(dat_mb)),rep("F",nrow(dat_mb))))

datbreak <- dator[dator$`Province/Territory`=="NL" ,]
x_axis <- unique(datbreak$FATALITIES,datbreak$EVACUATED)
sort(x_axis)
c(sort(x_axis))
p1 <- hist(datplot_on$x, breaks=seq(0,2000,250))
p2 <- hist(datplot_mb$x, breaks=seq(0,2000,250))
plot( p2, col=rgb(1,0,0,1/4), xlab="Month",main= "Event Starting Month in ON VS. MB")
plot( p1, col=rgb(0,0,1,1/4), add=T)
legend('topright',c("<1982",">1982"),fill = c(rgb(0,0,1,1/4),rgb(1,0,0,1/4)),bty='n', border = NA)

#
# test
#
df <- read.table(header=TRUE, text='
 cond yval
                 A 2
                 B 2.5
                 C 1.6
                 ')
ggplot(df, aes(x=cond, y=yval)) + geom_bar(stat="identity") 

+ 
  scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99"))
