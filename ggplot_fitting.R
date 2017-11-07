library('readr')
library("lubridate")
library('rgl')
library('ggplot2')
dator <- read_csv("C:/Users/ankai/Desktop/important saves/disaster analysis/data_pop_ud.csv")



c(unique(dat$`EVENT TYPE`))

wf <- dat[(dat$`EVENT TYPE` == 'Wildfire') & (dat$Population > 0),] 
wfyear<-data.frame(x=as.Date(wf$`EVENT START DATE`),y=(wf$`INJURED/INFECTED`+wf$FATALITIES+wf$EVACUATED)/(wf$Population))


#
# two ways to fiting data
# first one is built in and easier to use
#
ggplot(wfyear,aes(x=wfyear$x,y=wfyear$y))+ geom_point(shape=1)+
  geom_smooth(aes(colour = "spline"), method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE)+ 
  geom_smooth(aes(colour = "lm"), method = "lm", se = FALSE)+ 
  labs(colour = "Method")
#
# this one we got control of the fiting
#
wffit <- data.frame(x=as.numeric(wfyear$x),y1= wfyear$y,y2=wfyear$y^2)
fitA <- lm(y1+y2~x, data = wffit)
s_t <- data.frame(y=predict(fitA))
ggplot(wfyear,aes(x=wfyear$x,y=wfyear$y))+ geom_point()+
                                            geom_line(data=s_t,aes(x=wfyear$x,y))
#
#  fitting with step function at 1995
#
dat <- dator[(dator$`EVENT TYPE` == 'Wildfire') & dator$Population > 0,]
datplot <- data.frame(x=as.Date(dat$`EVENT START DATE`),y=(dat$`INJURED/INFECTED`+dat$FATALITIES+dat$EVACUATED)/(dat$Population))
datplot <- datplot[datplot$y <0.01,]
nrow(datplot)
step<-nrow(datplot[datplot$x<as.Date("1jan1990", "%d%b%Y"),])
#
# notice in the following y2 is the step function
#
datfit <- data.frame(x=as.numeric(datplot$x),y1= datplot$y,y2=c(rep(0.005,nrow(datplot)-step),rep(0,step)))
fitB <- lm(y1~x+y2, data = datfit)
s_tB <- data.frame(y=predict(fitB))
ggplot(datplot,aes(x=datplot$x,y=datplot$y))+ geom_point()+
  geom_line(data=s_tB,aes(x=datplot$x,y))
#
# 
#
c(unique(dator$`EVENT TYPE`))
c(unique(dator$`Province/Territory`))


#
# here we can pull out different set of data
#
dat <- dator[dator$Population > 0,]
dat <- dator[(dator$`Province/Territory` == 'ON') & dator$Population > 0,]
dat <- dator[(dator$`EVENT TYPE` == 'Wildfire') & dator$Population > 0,]
dat <- dator[(dator$`Province/Territory` == 'MB') & (dator$`EVENT TYPE` == 'Wildfire') & dator$Population > 0,]
datplot <- data.frame(x=as.Date(dat$`EVENT START DATE`),y=(dat$`INJURED/INFECTED`+dat$FATALITIES+dat$EVACUATED)/(dat$Population))

#
# we can use it to elimated the outlier
#
datplot <- datplot[datplot$y <0.01,]
#
# here is the time step
#
step<-as.Date("1jan1980", "%d%b%Y")
#
# here is the fitting 
#
datplotA <- datplot[datplot$x<=step,]
datplotB <- datplot[datplot$x>step,]
#
# we can fit different type of function
#
datfitA <- data.frame(x=as.numeric(datplotA$x),y1= datplotA$y)
fitA <- lm(y1~x, data = datfitA)

s_tA <- data.frame(y=predict(fitA))
datfitB <- data.frame(x=as.numeric(datplotB$x),y1= datplotB$y)
fitB <- lm(y1~x, data = datfitB)
s_tB <- data.frame(y=predict(fitB))
#
# paste fitting together
#
datfit <- data.frame(x=datplot$x,y=c(s_tA$y,s_tB$y))
#
# plot generater
#
ggplot(datplot,aes(x=datplot$x,y=datplot$y))+ geom_point()+
  geom_line(data=datfit,aes(x=datplot$x,y))

