library('readr')
library("lubridate")
library(readxl)
library('rgl')
dat <- read_csv("C:/Users/ankai/Desktop/important saves/disaster analysis/case_study_1_cdd_database_final_0.csv")
dat_pop <- read_csv("C:/Users/ankai/Desktop/important saves/disaster analysis/datWithPopulations2.csv")
POP <- read_csv("C:/Users/ankai/Desktop/important saves/disaster analysis/1971-2014.csv")

c(unique(dat_pop$`Province/Territory`))
str(POP)
POP
names(POP) <- c('YEAR', 'NL' , 'PE' , 'NS' , 'NB' ,'QC' , 'ON' , 'MB' , 'SK' , 'AB' , 'BC' , 'YT' , 'NT NU' , 'NT' , 'NU')
names(POP)[-1]
POP <- data.frame(POP, prairies=rowSums(POP[,c("AB" , "MB" , "SK")], na.rm=T), maritimes=rowSums(POP[,c("NB", "NS" , "PE")], na.rm=T),
                  estern=rowSums(POP[,c("NB", "NL" , "NS" , "ON" , "PE" , "QC")], na.rm=T),
                  western=rowSums(POP[,c("AB", "BC" , "MB" , "SK")], na.rm=T),
                  corss_canada=rowSums(POP[,names(POP)[-1]], na.rm=T),
                  BYN=rowSums(POP[,c("BC", "YT" , "NT")], na.rm=T),
                  QNNPN=rowSums(POP[,c("QC", "NB" , "NS" , "PE" , "NL")], na.rm=T),
                  NN=rowSums(POP[,c("NB", "NS")], na.rm=T),
                  AS=rowSums(POP[,c("AB", "SK")], na.rm=T),
                  QN=rowSums(POP[,c("QC", "NB")], na.rm=T),
                  OQ=rowSums(POP[,c("ON", "QC")], na.rm=T),
                  AM=rowSums(POP[,c("AB", "MB")], na.rm=T),
                  NSP=rowSums(POP[,c("NS", "PE")], na.rm=T),
                  BA=rowSums(POP[,c("BC", "AB")], na.rm=T),
                  OQN=rowSums(POP[,c("ON", "QC" , "NB")], na.rm=T),
                  NBP=rowSums(POP[,c("NB", "PE")], na.rm=T),
                  BASMO=rowSums(POP[,c("BC" , "AB" , "SK" , "MB" , "ON")], na.rm=T),
                  MO=rowSums(POP[,c("MB", "ON")], na.rm=T),
                  NPN=rowSums(POP[,c("NS" , "PE" , "NL")], na.rm=T),
                  SM=rowSums(POP[,c("MB" , "SK")], na.rm=T)
                  )

for (i in 1:length(dat_pop$X1)) {
  if(!is.na(dat_pop$`Province/Territory`[i]))
  if(as.numeric(strftime(dat_pop$`EVENT START DATE`[i],"%Y")) >= 1971)
  if(as.numeric(strftime(dat_pop$`EVENT START DATE`[i],"%Y")) <= 2014)
    { 
    if (dat_pop$`Province/Territory`[i]=="Prairies") {dat_pop$Population[i] <- POP$prairies[as.numeric(strftime(dat_pop$`EVENT START DATE`[i],"%Y"))-1971+1]}
    if (dat_pop$`Province/Territory`[i]=="Maritimes") {dat_pop$Population[i] <- POP$maritimes[as.numeric(strftime(dat_pop$`EVENT START DATE`[i],"%Y"))-1971+1]}
    if (dat_pop$`Province/Territory`[i]=="Eastern Canada") {dat_pop$Population[i] <- POP$estern[as.numeric(strftime(dat_pop$`EVENT START DATE`[i],"%Y"))-1971+1]}
    if (dat_pop$`Province/Territory`[i]=="Western Canada") {dat_pop$Population[i] <- POP$western[as.numeric(strftime(dat_pop$`EVENT START DATE`[i],"%Y"))-1971+1]}
    if (dat_pop$`Province/Territory`[i]=="Across Canada") {dat_pop$Population[i] <- POP$corss_canada[as.numeric(strftime(dat_pop$`EVENT START DATE`[i],"%Y"))-1971+1]}
    if (dat_pop$`Province/Territory`[i]=="BC YT NT") {dat_pop$Population[i] <- POP$BYN[as.numeric(strftime(dat_pop$`EVENT START DATE`[i],"%Y"))-1971+1]}
    if (dat_pop$`Province/Territory`[i]=="QC NB NS PE NL") {dat_pop$Population[i] <- POP$QNNPN[as.numeric(strftime(dat_pop$`EVENT START DATE`[i],"%Y"))-1971+1]}
    if (dat_pop$`Province/Territory`[i]=="NB NS") {dat_pop$Population[i] <- POP$NN[as.numeric(strftime(dat_pop$`EVENT START DATE`[i],"%Y"))-1971+1]}
    if (dat_pop$`Province/Territory`[i]=="AB SK") {dat_pop$Population[i] <- POP$AS[as.numeric(strftime(dat_pop$`EVENT START DATE`[i],"%Y"))-1971+1]}
    if (dat_pop$`Province/Territory`[i]=="QC NB") {dat_pop$Population[i] <- POP$QN[as.numeric(strftime(dat_pop$`EVENT START DATE`[i],"%Y"))-1971+1]}
    if (dat_pop$`Province/Territory`[i]=="ON QC") {dat_pop$Population[i] <- POP$OQ[as.numeric(strftime(dat_pop$`EVENT START DATE`[i],"%Y"))-1971+1]}
    if (dat_pop$`Province/Territory`[i]=="AB MB") {dat_pop$Population[i] <- POP$AM[as.numeric(strftime(dat_pop$`EVENT START DATE`[i],"%Y"))-1971+1]}
    if (dat_pop$`Province/Territory`[i]=="NS PE") {dat_pop$Population[i] <- POP$NSP[as.numeric(strftime(dat_pop$`EVENT START DATE`[i],"%Y"))-1971+1]}
    if (dat_pop$`Province/Territory`[i]=="BC AB") {dat_pop$Population[i] <- POP$BA[as.numeric(strftime(dat_pop$`EVENT START DATE`[i],"%Y"))-1971+1]}
    if (dat_pop$`Province/Territory`[i]=="ON QC NB") {dat_pop$Population[i] <- POP$OQN[as.numeric(strftime(dat_pop$`EVENT START DATE`[i],"%Y"))-1971+1]}
    if (dat_pop$`Province/Territory`[i]=="NB PE") {dat_pop$Population[i] <- POP$NBP[as.numeric(strftime(dat_pop$`EVENT START DATE`[i],"%Y"))-1971+1]}
    if (dat_pop$`Province/Territory`[i]=="BC AB SK MB ON") {dat_pop$Population[i] <- POP$BASMO[as.numeric(strftime(dat_pop$`EVENT START DATE`[i],"%Y"))-1971+1]}
    if (dat_pop$`Province/Territory`[i]=="MB ON") {dat_pop$Population[i] <- POP$MO[as.numeric(strftime(dat_pop$`EVENT START DATE`[i],"%Y"))-1971+1]}
    if (dat_pop$`Province/Territory`[i]=="NS PE NL") {dat_pop$Population[i] <- POP$NPN[as.numeric(strftime(dat_pop$`EVENT START DATE`[i],"%Y"))-1971+1]}
    if (dat_pop$`Province/Territory`[i]=="SK MB") {dat_pop$Population[i] <- POP$SM[as.numeric(strftime(dat_pop$`EVENT START DATE`[i],"%Y"))-1971+1]}
    }
}
test <- data.frame(dat_pop$`EVENT START DATE`, dat_pop$`Province/Territory`,dat_pop$Population)

write.csv(dat_pop, file = "data_pop 1")
data_pop
