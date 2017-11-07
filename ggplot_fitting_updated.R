library('readr')
library("lubridate")
library('rgl')
library('ggplot2')
dator <- read_csv("C:/Users/ankai/Desktop/important saves/disaster analysis/data_pop_ud.csv")
dator$`EVENT START DATE`[131] <- "2018-01-01"
#
# 
#
c(unique(dator$`EVENT TYPE`))
c(unique(dator$`Province/Territory`))

#
# here we can pull out different set of data
#
dat <- dator[!(dator$`EVENT TYPE` != 'Wildfire'& dator$`EVENT TYPE` != 'Flood') & dator$Population > 0,]
dat <- dat[as.numeric(strftime(dat$`EVENT START DATE`,"%Y"))>=2000,]
dat <- dat[as.numeric(strftime(dat$`EVENT START DATE`,"%Y"))<=2005,]
daplot <- data.frame(x=as.Date(dat$`EVENT START DATE`),
                     y=(dat$EVACUATED)/(dat$Population),
                     z=dat$`EVENT TYPE`,
                     month=as.numeric(strftime(dat$`EVENT START DATE`,"%m")),
                     year=as.numeric(strftime(dat$`EVENT START DATE`,"%Y")))

daplot <- data.frame(x=as.Date(dat$`EVENT START DATE`),y=(dat$EVACUATED),z=dat$`EVENT TYPE`)



datplot <- daplot[daplot$y <0.05 & daplot$y>=0,]
step<-as.Date("1jan1970", "%d%b%Y")
datplotA <- datplot[datplot$x<=step,]
datplotB <- datplot[datplot$x>step,]
datfitA <- data.frame(x=as.numeric(datplotA$x),y1= datplotA$y)
fitA <- lm(y1~x+x*x+x*x*x, data = datfitA)
s_tA <- data.frame(y=predict(fitA))

datfitB <- data.frame(x=as.numeric(datplotB$x),y1= datplotB$y)
fitB <- lm(y1~x+x*x+x*x*x, data = datfitB)
s_tB <- data.frame(y=predict(fitB))


cols <- c("Flood" = alpha("royalblue",0.7) , "Wildfire" = alpha("firebrick1",0.5),"Drought"="goldenrod1", "Storms and Severe Thunderstorms"="palegreen3")
datfit <- data.frame(x=datplot$x,y=c(s_tA$y,s_tB$y))
ggplot(datplot,aes(x,y,color=as.character(datplot$z)))+ geom_point(size=2)+
  geom_line(data=datfit,aes(x,y),color="black",size=0.7)+ 
  scale_colour_manual(values = cols,labels=c("Flood","Wildfire"))  +
  labs(x='EVENT YEAR',y='Normalized Human Factor',subtitle='??(event type) in ?? (province)',color="Event Type") +
  geom_vline(xintercept = as.numeric(step))

#
# very pretty bar chart
#
datplot <- dator[dator$`Province/Territory`=="ON",]
datplot <- data.frame(x=datplot$`EVENT TYPE`,y=datplot$`EVENT START DATE`)
ggplot(datplot, aes(x)) + geom_bar(width=0.8,aes(fill = as.character(strftime(datplot$y,"%m"))))+
  labs(x='disaster in ON(province)',y='count',fill="month")

datplot <- dator[dator$`EVENT TYPE`=="Wildfire",]
datplot <- data.frame(x=datplot$`Province/Territory`,y=datplot$`EVENT START DATE`)
ggplot(datplot, aes(x)) + geom_bar(width=0.8,aes(fill = as.character(strftime(datplot$y,"%m"))))+
  labs(x='Wildfire VS. province',y='count',fill="month")


unique(dator$`EVENT TYPE`)

library(fitdistrplus)
dat<- dator[dator$Population > 0,]
dat <- dator[(dator$`EVENT TYPE` == 'Wildfire') & dator$Population > 0,]
datplot <- data.frame(x=as.Date(dat$`EVENT START DATE`),y=(dat$`INJURED/INFECTED`+dat$FATALITIES+dat$EVACUATED)/(dat$Population))
my_data <- datplot$y
hist(my_data)

plotdist(my_data, histo = TRUE, demp = TRUE)
descdist(my_data, discrete=FALSE, boot=500)

fit_g  <- fitdist(my_data, "exp")
denscomp(fit_g, legendtext = "exp")
cdfcomp (fit_g, legendtext = "exp")
qqcomp  (fit_g, legendtext = "exp")
ppcomp  (fit_g, legendtext = "exp")

#
# here we are going to look at fitting a distribution in a hist plot
# not sure what that means but it do gives nice plots
#
# here we have fitting of hist plot with three kinds of distribution 
# from João Neto (2015) (http://www.di.fc.ul.pt/~jpn/r/distributions/fitting.html)
#
#

library(fitdistrplus)

dator <- read_csv("C:/Users/ankai/Desktop/important saves/disaster analysis/data_pop_ud.csv")
datplot <- dator[(dator$`Province/Territory` == 'ON'),]
#
# the 131 row have some problem with its start date
#
dator$`EVENT START DATE`[131] <- "2018-01-01"
dator$`EVENT START DATE`[131]
datplot <- dator

my_data <- as.numeric(strftime(datplot$`EVENT START DATE`,"%m"))
hist(my_data)

plotdist(my_data, histo = TRUE, demp = TRUE)
descdist(my_data, discrete=FALSE, boot=500)

fit_w  <- fitdist(my_data, "weibull")
fit_g  <- fitdist(my_data, "gamma")
fit_ln <- fitdist(my_data, "lnorm")
summary(fit_ln)
plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fit_w, fit_g, fit_ln), legendtext = plot.legend)
cdfcomp (list(fit_w, fit_g, fit_ln), legendtext = plot.legend)
qqcomp  (list(fit_w, fit_g, fit_ln), legendtext = plot.legend)
ppcomp  (list(fit_w, fit_g, fit_ln), legendtext = plot.legend)

