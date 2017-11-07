---
title: "test1"
output:
  pdf_document: default
  html_document: default
---
    
```{r_setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
``    

## R Markdown


```{r cars}
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
barplot(prop.table(table(dator$`EVENT TYPE`)))
table(dator$`EVENT TYPE`)
dat <- dator[!(dator$`EVENT TYPE` != 'Wildfire'& dator$`EVENT TYPE` != 'Flood')& dator$Population > 0,]
daplot <- data.frame(x=as.Date(dat$`EVENT START DATE`), y=(dat$EVACUATED)/(dat$Population),
                     type=dat$`EVENT TYPE`,yp=(dat$EVACUATED),
                     month=as.numeric(strftime(dat$`EVENT START DATE`,"%m")),
                     year=as.numeric(strftime(dat$`EVENT START DATE`,"%Y")),
                     z=(dat$FATALITIES+dat$`INJURED/INFECTED`))

datplot <- daplot[!(daplot$yp==0 & daplot$z==0),]
#
#
#aes(size=y)
cols <- c("Flood"= alpha("royalblue",0.7) , "Wildfire"=alpha("firebrick1",0.7))
g1 <- ggplot(datplot,aes(x,z,color=as.character(type)))+ geom_point(aes(size=y*100))+ 
  labs(x='EVENT YEAR',y='Fatality',subtitle='Month',size="Eva/Pop(%)", color="Event Type") +
  facet_grid(type~month) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 0.9, vjust = 0.5))+ 
  geom_vline(xintercept = as.numeric(as.Date("1970-01-01")), linetype=4) + 
  scale_colour_manual(values = cols,labels=c("Flood","Wildfire"))

svg("ankai_monthly.svg", width = 10.75, height = 8)
plot(g1)
dev.off()


## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

test_frame <- data.frame(x=rnorm(100),y=1:100)
m <- cor(test_frame)
varimax(m)
coe <- c(2,3)
test_frame %*% coe


