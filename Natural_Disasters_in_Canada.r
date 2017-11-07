
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
library(readr)
library("lubridate")
dat <- read_csv("~/Poster presentation/eventStartDate.csv")
```

The following plots show the starting dates of natural disasters of the indicated type in Canada from 1905-2014 and their estimated total cost in Canadian dollars. 

```{r EventType vs Total cost plots}

datEstimatedTotalCost <- dat[!is.na(dat$`ESTIMATED TOTAL COST`) & (dat$`ESTIMATED TOTAL COST` != 0), ]

eventTypes <- c(unique(dat$`EVENT TYPE`))
sortedEventTypes <- sort(eventTypes)

testVector <- vector(length = 18, mode = "list")

for (i in 1:18) # for loop to create a plot for each of the 18 event types 
{
  testVector[[i]] <-dat[dat$`EVENT TYPE` == sortedEventTypes[i] & dat$`ESTIMATED TOTAL COST` > 0, c("EVENT START DATE", "ESTIMATED TOTAL COST")]
  if(dim(testVector[[i]])[1] > 1) {
    if(is.finite(max(testVector[[i]]$`ESTIMATED TOTAL COST`, na.rm = TRUE))) {
  plot(as.Date(testVector[[i]]$`EVENT START DATE`), testVector[[i]]$`ESTIMATED TOTAL COST`, xlab = "Event Start Date", ylab = "Estimated Total Cost (CAD)", main = sortedEventTypes[i])
    }
  }
}
```

The following plot shows the event start dates of various types of natural disasters in Canada from 1905-2014 and the amount in Canadian dollars paid out by insurance companies. 

```{r Insurance Payments Plot}

datInsurancePayments <- dat[!is.na(dat$`INSURANCE PAYMENTS`) & (dat$`INSURANCE PAYMENTS` != 0), ]

plot(as.Date(datInsurancePayments$`EVENT START DATE`), datInsurancePayments$`INSURANCE PAYMENTS`, xlab = "Event Start Date", ylab = "Insurance Payments (CAD)", main = "Natural Disasters in Canada vs. their associated insurance payments")
```

The following plot show the event start dates of various types of natural disaster in Canada from 1905-2014 and the number of people who lost power during the relevant event. 

```{r Utilities Plot}

datUtiities <- dat[!is.na(dat$`UTILITY/PEOPLE AFFECTED`) & (dat$`UTILITY/PEOPLE AFFECTED` != 0),]

plot(as.Date(datUtiities$`EVENT START DATE`), datUtiities$`UTILITY/PEOPLE AFFECTED`, xlab = "Event Start Date", ylab = "Number of People whose Utilities were affected", main = "Natural Disasters in Canada vs. Utilities affected")
```

The following two plots show the event start date of various types of natural disasters in Canada from 1905-2014 and the associated number of fatalities. The first plot includes the outlier in 1918, and the second plot does not. 

```{r Fatalities Plot}

datFatalies1 <- dat[!is.na(dat$FATALITIES) & (dat$FATALITIES != 0) ,]

plot(as.Date(datFatalies1$`EVENT START DATE`), datFatalies1$`FATALITIES`, xlab = "Event Start Date", ylab = "Number of Fatalities", main = "Natural Disasters in Canada vs. Number of Associated Fatalities")

datFatalies2 <- dat[!is.na(dat$FATALITIES) & (dat$FATALITIES != 0) & (dat$FATALITIES != 50000),]

plot(as.Date(datFatalies2$`EVENT START DATE`), datFatalies2$`FATALITIES`, xlab = "Event Start Date", ylab = "Number of Fatalities", main = "Natural Disasters in Canada vs. Number of Associated Fatalities")

```

The following plot shows the event start date of various types of natural disasters in Canada from 1905-2014 and the associated number of injured/infected people. 

```{r Injured/Infected Plot}

datInjured <- dat[!is.na(dat$`INJURED/INFECTED`) & (dat$`INJURED/INFECTED` != 0) ,]

plot(as.Date(datInjured$`EVENT START DATE`), datInjured$`FATALITIES`, xlab = "Event Start Date", ylab = "Number of Fatalities", main = "Natural Disasters in Canada vs. Number of Associated Injuries and Infections", cex.main = 0.8)

```

The following plot shows the event start date of various types of natural disasters in Canada from 1905-2014 and the associated number of people evacuated.

```{r Evacuated Plot}

datEvacuated <- dat[!is.na(dat$EVACUATED) & (dat$EVACUATED != 0) ,]

plot(as.Date(datEvacuated$`EVENT START DATE`), datEvacuated$`EVACUATED`, xlab = "Event Start Date", ylab = "Number of Evacuations", main = "Natural Disasters in Canada vs. Number of Associated Evacuations")

```

The following plot shows the event start date of various types of natural disasters in Canada from 1905-2014 and the associated ratio of fatalities to injuries/infections. 

```{r Ratio of Fatalities to Injuries/Infections Plot}

datFatalityRatio <- dat[!is.na(dat$FATALITIES) & (dat$FATALITIES != 0) & (!is.na(dat$`INJURED/INFECTED`)) & (dat$`INJURED/INFECTED` != 0),]

plot(as.Date(datFatalityRatio$`EVENT START DATE`), datFatalityRatio$FATALITIES/datFatalityRatio$`INJURED/INFECTED`, xlab = "Event Start Date", ylab = "Ratio of Fatalities to Injuries/Infections", main = "Natural Disasters in Canada and their ratios of deaths to injuries/infections", cex.main = 0.8)
```


```{r Province Subsets}
containsAB <- grepl("AB", dat$`Province/Territory`, fixed = TRUE)

containsSK <- grepl("SK", dat$`Province/Territory`, fixed = TRUE)

containsON <- grepl("ON", dat$`Province/Territory`, fixed = TRUE)

containsMB <- grepl("MB", dat$`Province/Territory`, fixed = TRUE)

containsBC <- grepl("BC", dat$`Province/Territory`, fixed = TRUE)

containsNB <- grepl("NB", dat$`Province/Territory`, fixed = TRUE)

containsNL <- grepl("NL", dat$`Province/Territory`, fixed = TRUE)

containsNS <- grepl("NS", dat$`Province/Territory`, fixed = TRUE)

containsNT <- grepl("NT", dat$`Province/Territory`, fixed = TRUE)

containsPE <- grepl("PE", dat$`Province/Territory`, fixed = TRUE)

containsQC <- grepl("QC", dat$`Province/Territory`, fixed = TRUE)

containsYT <- grepl("YT", dat$`Province/Territory`, fixed = TRUE)

containsNU <- grepl("NU", dat$`Province/Territory`, fixed = TRUE)

containsPrairies <- grepl("Praries", dat$`Province/Territory`, fixed = TRUE)

containsMaritimes <- grepl("Maritimes", dat$`Province/Territory`, fixed = TRUE)

datAB <- dat[containsAB,]

datBC <- dat[containsBC,]
  
datNB <- dat[containsNB,]

datNL <- dat[containsNL,]

datNS <- dat[containsNS,]

datNT <- dat[containsNT,]

datPE <- dat[containsPE,]
  
datQC <- dat[containsQC,]
  
datYT <- dat[containsYT,]
  
datNU <- dat[containsNU,]
  
datSK <- dat[containsSK,]
  
datON <- dat[containsON,]

datMB <- dat[containsMB,]

datPrairies <- dat[containsPrairies, ]

datMaritimes <- dat[containsMaritimes, ]

#store them all in a list 

datVector <- list(datAB, datSK, datON, datMB, datBC, datNB, datNL, datNS, datNT, datPE, datQC, datYT, datNU, datPrairies, datMaritimes)

```

The following plot shows the total amount of natural disaster events that have taken place in each province over the years 1905-2014. 

```{r Events by province}

provinceNames <- c("Alberta", "Saskatchewan", "Ontario", "Manitoba", "British Columbia", "New Brunswick", "Newfoundland", "Nova Scotia", "Northwest Territories", "PEI", "Quebec", "Yukon Territories", "Nunavut", "Prairies", "Maritimes")

totalEvents <- vector(length = 15)
for (i in 1:15)
{
  totalEvents[i] <- length(datVector[[i]]$`EVENT START DATE`)
}

par(las = 2)
barplot(totalEvents, names.arg = provinceNames, ylab = "Total Number of Events from 1905-2014", main = "Total Number of Natural Disasters in each Province/Region of Canada", cex.names = 0.6)
```
 
```{r Year subsets}
datYears <- list(length = 110) # Create data subsets for each year in 1905-2014 
for (j in 1905:2014)
{
  datYears[[j]] <- dat[grepl(j, dat$`EVENT START DATE`, fixed = TRUE),]
}
```