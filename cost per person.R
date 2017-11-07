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
c(unique(dat$`Province/Territory`))

dat <- dator[dator$`NORMALIZED TOTAL COST`>0 & dator$Population >0 ,]
daplot <- data.frame(x=as.Date(dat$`EVENT START DATE`), 
                     y=log(dat$`ESTIMATED TOTAL COST`/dat$Population),
                     type=dat$`EVENT TYPE`,
                     month=as.numeric(strftime(dat$`EVENT START DATE`,"%m")),
                     year=as.numeric(strftime(dat$`EVENT START DATE`,"%Y")),
                     z=log(dat$`NORMALIZED TOTAL COST`))

region <- rep(0,nrow(dat))
for (i in 1:nrow(dat)) {
  if (grepl("AB", dat$`Province/Territory`[i], fixed = TRUE)==TRUE) {region[i] <- 2}
  if (grepl("MB", dat$`Province/Territory`[i], fixed = TRUE)==TRUE) {region[i] <- 2}
  if (grepl("NB", dat$`Province/Territory`[i], fixed = TRUE)==TRUE) {region[i] <- 4}
  if (grepl("NL", dat$`Province/Territory`[i], fixed = TRUE)==TRUE) {region[i] <- 4}
  if (grepl("QC", dat$`Province/Territory`[i], fixed = TRUE)==TRUE) {region[i] <- 3}
  if (grepl("YT", dat$`Province/Territory`[i], fixed = TRUE)==TRUE) {region[i] <- 1}
  if (grepl("NS", dat$`Province/Territory`[i], fixed = TRUE)==TRUE) {region[i] <- 4}
  if (grepl("BC", dat$`Province/Territory`[i], fixed = TRUE)==TRUE) {region[i] <- 2}
  if (grepl("ON", dat$`Province/Territory`[i], fixed = TRUE)==TRUE) {region[i] <- 3}
  if (grepl("SK", dat$`Province/Territory`[i], fixed = TRUE)==TRUE) {region[i] <- 2}
  if (grepl("Eastern Canada", dat$`Province/Territory`[i], fixed = TRUE)==TRUE) {region[i] <- 4}
  if (grepl("Maritimes", dat$`Province/Territory`[i], fixed = TRUE)==TRUE) {region[i] <- 4}
  if (grepl("Prairies", dat$`Province/Territory`[i], fixed = TRUE)==TRUE) {region[i] <- 2}
  if (grepl("Across Canada", dat$`Province/Territory`[i], fixed = TRUE)==TRUE) {region[i] <- 5}
  if (grepl("PE", dat$`Province/Territory`[i], fixed = TRUE)==TRUE) {region[i] <- 4}
  if (grepl("NU", dat$`Province/Territory`[i], fixed = TRUE)==TRUE) {region[i] <- 1}
  if (grepl("NT", dat$`Province/Territory`[i], fixed = TRUE)==TRUE) {region[i] <- 1}
}
daplot <- data.frame(x=as.Date(dat$`EVENT START DATE`), 
                     y=log(dat$`ESTIMATED TOTAL COST`/dat$Population),
                     type=dat$`EVENT TYPE`,
                     month=as.numeric(strftime(dat$`EVENT START DATE`,"%m")),
                     year=as.numeric(strftime(dat$`EVENT START DATE`,"%Y")),
                     z=log(dat$`NORMALIZED TOTAL COST`),
                     region=as.character(region))

#test <- data.frame(dat$`Province/Territory`,region)
#test[test$region==1,]
#
#
#
#
#
#
perio <-10
daplot <- daplot[ daplot$region!=5 ,]
datplot <- data.frame(daplot,tf=as.character((daplot$year-1970)%%perio))
#
#
ggplot(datplot,aes(x,y))+geom_point(aes(color=region))+ 
  geom_smooth(method="loess", span=0.6) +
  labs(x="Event Year", y="Costs Per Person (log)")+ 
  coord_cartesian( ylim=c(-8, 6)) + 
  geom_vline(xintercept = seq(as.numeric(as.Date("1970-01-01")),
                              as.numeric(as.Date("2020-01-01")),perio*365), linetype=4) + 
  scale_color_discrete(label=c("North","West","Mid","East","Across Canada"))


my.smooth <- lm(y ~ x, data = datplot)
summary(my.smooth)
my.data <- data.frame(x = datplot$x)                                           
my.data$y <- predict(my.smooth, newdata = my.data) 

g1 <- ggplot(datplot,aes(x,y))+geom_point(aes(color=region))+ 
  geom_smooth(data=my.data,size=0.5) +
  labs(x="Event Year", y="Costs Per Person (log)")+ 
  coord_cartesian( ylim=c(-8, 6)) + 
  geom_vline(xintercept = seq(as.numeric(as.Date("1970-01-01")),
                              as.numeric(as.Date("2020-01-01")),perio*365), linetype=4) + 
  scale_color_discrete(label=c("North","West","Mid","East","Across Canada"))+
  facet_grid(region~.)  
g1 
svg("ankai_cost_per_person_1.svg", width = 10.75, height = 8)
plot(g1)
dev.off() 



#
# test 
#

c(unique(dat$`Province/Territory`))
containsNB <- grepl("NB", dat$`Province/Territory`, fixed = TRUE)
as.character(c(1,2,3))
#
#
#
eg <- data.frame(x = c(1:50, 50:1),  
                 y = c(1:50, 1:50) + rnorm(100),  
                 g = rep(c("a","b"), each=50)) 
my.smooth <- lm(y ~ x, data = eg)
my.data <- data.frame(x = 1:50)                                           
my.data$y <- predict(my.smooth, newdata = my.data) 

qplot(x, y, data = eg) + 
  facet_wrap(~ g) + 
  geom_smooth() + 
  geom_smooth(data = my.data)
