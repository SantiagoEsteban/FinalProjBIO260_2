###############
#PACKAGES
###############
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(stringr)
library(ggplot2)
library(readxl)
library(ggmap)
library(RNetCDF)
library(xlsx)

#####################
#Vap - vapor pressure
#####################
vap.full <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.vap.dat.nc', write=FALSE)
vap.var <- var.get.nc(vap.full, "vap")
lon <- var.get.nc(vap.full, "lon")
lat <- var.get.nc(vap.full, "lat")
#Bomi
Vap_Bomi_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                      Month=rep(seq(1,12,1), 4), day=1,
                                      Vap=vap.var[which(lon==10.75),which(lat==6.75),1:48]))
Vap_Bomi_monthly$Location <- 'Bomi'
Vap_Bomi_monthly$date <- ymd(paste(Vap_Bomi_monthly$Year, Vap_Bomi_monthly$Month, Vap_Bomi_monthly$day, sep="-"))
Vap_Bomi_monthly_2015 <- select(Vap_Bomi_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Bomi_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Bomi_monthly_2015a$Location <- 'Bomi'
Vap_Bomi_monthly_2015a$date <- ymd(paste(Vap_Bomi_monthly_2015a$Year, Vap_Bomi_monthly_2015a$Month, Vap_Bomi_monthly_2015a$day, sep="-"))
Vap_Bomi_monthly_2015a <- select(Vap_Bomi_monthly_2015a, date, Year, Month, day, Location)
Vap_Bomi_monthly_2015 <- full_join(Vap_Bomi_monthly_2015a, Vap_Bomi_monthly_2015)
Vap_Bomi_monthly <- rbind(select(Vap_Bomi_monthly,date, Year, Month, day, Location, Vap), Vap_Bomi_monthly_2015)
rm(Vap_Bomi_monthly_2015, Vap_Bomi_monthly_2015a)
Vap_Bomi_monthly$measurement <- "Vap"
Vap_Bomi_monthly <- rename(Vap_Bomi_monthly, Value=Vap)

#Bong
Vap_Bong_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        Vap1=vap.var[which(lon==9.75),which(lat==7.25),1:48],
                                        Vap2=vap.var[which(lon==9.25),which(lat==7.25),1:48],
                                        Vap3=vap.var[which(lon==10.25),which(lat==6.75),1:48],
                                        Vap4=vap.var[which(lon==9.75),which(lat==6.75),1:48],
                                        Vap5=vap.var[which(lon==9.25),which(lat==6.75),1:48]))
Vap_Bong_monthly$Vap <- rowMeans(select(Vap_Bong_monthly, Vap1, Vap2, Vap3, Vap4, Vap5))
Vap_Bong_monthly$Location <- 'Bong'
Vap_Bong_monthly$date <- ymd(paste(Vap_Bong_monthly$Year, Vap_Bong_monthly$Month, Vap_Bong_monthly$day, sep="-"))
Vap_Bong_monthly_2015 <- select(Vap_Bong_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Bong_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Bong_monthly_2015a$Location <- 'Bong'
Vap_Bong_monthly_2015a$date <- ymd(paste(Vap_Bong_monthly_2015a$Year, Vap_Bong_monthly_2015a$Month, Vap_Bong_monthly_2015a$day, sep="-"))
Vap_Bong_monthly_2015a <- select(Vap_Bong_monthly_2015a, date, Year, Month, day, Location)
Vap_Bong_monthly_2015 <- full_join(Vap_Bong_monthly_2015a, Vap_Bong_monthly_2015)
Vap_Bong_monthly <- rbind(select(Vap_Bong_monthly,date, Year, Month, day, Location, Vap), Vap_Bong_monthly_2015)
rm(Vap_Bong_monthly_2015, Vap_Bong_monthly_2015a)
Vap_Bong_monthly$measurement <- "Vap"
Vap_Bong_monthly <- rename(Vap_Bong_monthly, Value=Vap)

#Gbarpolu
Vap_Gbarpolu_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        Vap1=vap.var[which(lon==10.25),which(lat==7.75),1:48],
                                        Vap2=vap.var[which(lon==10.25),which(lat==7.25),1:48],
                                        Vap3=vap.var[which(lon==9.75),which(lat==7.25),1:48]))
Vap_Gbarpolu_monthly$Vap <- rowMeans(select(Vap_Gbarpolu_monthly, Vap1, Vap2, Vap3))
Vap_Gbarpolu_monthly$Location <- 'Gbarpolu'
Vap_Gbarpolu_monthly$date <- ymd(paste(Vap_Gbarpolu_monthly$Year, Vap_Gbarpolu_monthly$Month, Vap_Gbarpolu_monthly$day, sep="-"))
Vap_Gbarpolu_monthly_2015 <- select(Vap_Gbarpolu_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Gbarpolu_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Gbarpolu_monthly_2015a$Location <- 'Gbarpolu'
Vap_Gbarpolu_monthly_2015a$date <- ymd(paste(Vap_Gbarpolu_monthly_2015a$Year, Vap_Gbarpolu_monthly_2015a$Month, Vap_Gbarpolu_monthly_2015a$day, sep="-"))
Vap_Gbarpolu_monthly_2015a <- select(Vap_Gbarpolu_monthly_2015a, date, Year, Month, day, Location)
Vap_Gbarpolu_monthly_2015 <- full_join(Vap_Gbarpolu_monthly_2015a, Vap_Gbarpolu_monthly_2015)
Vap_Gbarpolu_monthly <- rbind(select(Vap_Gbarpolu_monthly,date, Year, Month, day, Location, Vap), Vap_Gbarpolu_monthly_2015)
rm(Vap_Gbarpolu_monthly_2015, Vap_Gbarpolu_monthly_2015a)
Vap_Gbarpolu_monthly$measurement <- "Vap"
Vap_Gbarpolu_monthly <- rename(Vap_Gbarpolu_monthly, Value=Vap)

#Grand_Bassa
Vap_Grand_Bassa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            Vap1=vap.var[which(lon==10.25),which(lat==6.25),1:48],
                                            Vap2=vap.var[which(lon==9.75),which(lat==6.25),1:48]))
Vap_Grand_Bassa_monthly$Vap <- rowMeans(select(Vap_Grand_Bassa_monthly, Vap1, Vap2))
Vap_Grand_Bassa_monthly$Location <- 'Grand_Bassa'
Vap_Grand_Bassa_monthly$date <- ymd(paste(Vap_Grand_Bassa_monthly$Year, Vap_Grand_Bassa_monthly$Month, Vap_Grand_Bassa_monthly$day, sep="-"))
Vap_Grand_Bassa_monthly_2015 <- select(Vap_Grand_Bassa_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Grand_Bassa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Grand_Bassa_monthly_2015a$Location <- 'Grand_Bassa'
Vap_Grand_Bassa_monthly_2015a$date <- ymd(paste(Vap_Grand_Bassa_monthly_2015a$Year, Vap_Grand_Bassa_monthly_2015a$Month, Vap_Grand_Bassa_monthly_2015a$day, sep="-"))
Vap_Grand_Bassa_monthly_2015a <- select(Vap_Grand_Bassa_monthly_2015a, date, Year, Month, day, Location)
Vap_Grand_Bassa_monthly_2015 <- full_join(Vap_Grand_Bassa_monthly_2015a, Vap_Grand_Bassa_monthly_2015)
Vap_Grand_Bassa_monthly <- rbind(select(Vap_Grand_Bassa_monthly,date, Year, Month, day, Location, Vap), Vap_Grand_Bassa_monthly_2015)
rm(Vap_Grand_Bassa_monthly_2015, Vap_Grand_Bassa_monthly_2015a)
Vap_Grand_Bassa_monthly$measurement <- "Vap"
Vap_Grand_Bassa_monthly <- rename(Vap_Grand_Bassa_monthly, Value=Vap)

#Grand_Cape_Mount
Vap_Grand_Cape_Mount_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        Vap1=vap.var[which(lon==11.25),which(lat==7.25),1:48],
                                        Vap2=vap.var[which(lon==10.75),which(lat==7.25),1:48],
                                        Vap3=vap.var[which(lon==11.25),which(lat==6.75),1:48]))
Vap_Grand_Cape_Mount_monthly$Vap <- rowMeans(select(Vap_Grand_Cape_Mount_monthly, Vap1, Vap2, Vap3))
Vap_Grand_Cape_Mount_monthly$Location <- 'Grand_Cape_Mount'
Vap_Grand_Cape_Mount_monthly$date <- ymd(paste(Vap_Grand_Cape_Mount_monthly$Year, Vap_Grand_Cape_Mount_monthly$Month, Vap_Grand_Cape_Mount_monthly$day, sep="-"))
Vap_Grand_Cape_Mount_monthly_2015 <- select(Vap_Grand_Cape_Mount_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Grand_Cape_Mount_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Grand_Cape_Mount_monthly_2015a$Location <- 'Grand_Cape_Mount'
Vap_Grand_Cape_Mount_monthly_2015a$date <- ymd(paste(Vap_Grand_Cape_Mount_monthly_2015a$Year, Vap_Grand_Cape_Mount_monthly_2015a$Month, Vap_Grand_Cape_Mount_monthly_2015a$day, sep="-"))
Vap_Grand_Cape_Mount_monthly_2015a <- select(Vap_Grand_Cape_Mount_monthly_2015a, date, Year, Month, day, Location)
Vap_Grand_Cape_Mount_monthly_2015 <- full_join(Vap_Grand_Cape_Mount_monthly_2015a, Vap_Grand_Cape_Mount_monthly_2015)
Vap_Grand_Cape_Mount_monthly <- rbind(select(Vap_Grand_Cape_Mount_monthly,date, Year, Month, day, Location, Vap), Vap_Grand_Cape_Mount_monthly_2015)
rm(Vap_Grand_Cape_Mount_monthly_2015, Vap_Grand_Cape_Mount_monthly_2015a)
Vap_Grand_Cape_Mount_monthly$measurement <- "Vap"
Vap_Grand_Cape_Mount_monthly <- rename(Vap_Grand_Cape_Mount_monthly, Value=Vap)

#Grand_Gedeh
Vap_Grand_Gedeh_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        Vap1=vap.var[which(lon==8.75),which(lat==6.25),1:48],
                                        Vap2=vap.var[which(lon==8.25),which(lat==6.25),1:48],
                                        Vap3=vap.var[which(lon==8.75),which(lat==5.75),1:48],
                                        Vap4=vap.var[which(lon==8.25),which(lat==5.75),1:48],
                                        Vap5=vap.var[which(lon==7.75),which(lat==5.75),1:48]))
Vap_Grand_Gedeh_monthly$Vap <- rowMeans(select(Vap_Grand_Gedeh_monthly, Vap1, Vap2, Vap3, Vap4, Vap5))
Vap_Grand_Gedeh_monthly$Location <- 'Grand_Gedeh'
Vap_Grand_Gedeh_monthly$date <- ymd(paste(Vap_Grand_Gedeh_monthly$Year, Vap_Grand_Gedeh_monthly$Month, Vap_Grand_Gedeh_monthly$day, sep="-"))
Vap_Grand_Gedeh_monthly_2015 <- select(Vap_Grand_Gedeh_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Grand_Gedeh_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Grand_Gedeh_monthly_2015a$Location <- 'Grand_Gedeh'
Vap_Grand_Gedeh_monthly_2015a$date <- ymd(paste(Vap_Grand_Gedeh_monthly_2015a$Year, Vap_Grand_Gedeh_monthly_2015a$Month, Vap_Grand_Gedeh_monthly_2015a$day, sep="-"))
Vap_Grand_Gedeh_monthly_2015a <- select(Vap_Grand_Gedeh_monthly_2015a, date, Year, Month, day, Location)
Vap_Grand_Gedeh_monthly_2015 <- full_join(Vap_Grand_Gedeh_monthly_2015a, Vap_Grand_Gedeh_monthly_2015)
Vap_Grand_Gedeh_monthly <- rbind(select(Vap_Grand_Gedeh_monthly,date, Year, Month, day, Location, Vap), Vap_Grand_Gedeh_monthly_2015)
rm(Vap_Grand_Gedeh_monthly_2015, Vap_Grand_Gedeh_monthly_2015a)
Vap_Grand_Gedeh_monthly$measurement <- "Vap"
Vap_Grand_Gedeh_monthly <- rename(Vap_Grand_Gedeh_monthly, Value=Vap)

#Grand_Kru
Vap_Grand_Kru_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        Vap=vap.var[which(lon==8.25),which(lat==4.75),1:48]))
Vap_Grand_Kru_monthly$Location <- 'Grand_Kru'
Vap_Grand_Kru_monthly$date <- ymd(paste(Vap_Grand_Kru_monthly$Year, Vap_Grand_Kru_monthly$Month, Vap_Grand_Kru_monthly$day, sep="-"))
Vap_Grand_Kru_monthly_2015 <- select(Vap_Grand_Kru_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Grand_Kru_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Grand_Kru_monthly_2015a$Location <- 'Grand_Kru'
Vap_Grand_Kru_monthly_2015a$date <- ymd(paste(Vap_Grand_Kru_monthly_2015a$Year, Vap_Grand_Kru_monthly_2015a$Month, Vap_Grand_Kru_monthly_2015a$day, sep="-"))
Vap_Grand_Kru_monthly_2015a <- select(Vap_Grand_Kru_monthly_2015a, date, Year, Month, day, Location)
Vap_Grand_Kru_monthly_2015 <- full_join(Vap_Grand_Kru_monthly_2015a, Vap_Grand_Kru_monthly_2015)
Vap_Grand_Kru_monthly <- rbind(select(Vap_Grand_Kru_monthly,date, Year, Month, day, Location, Vap), Vap_Grand_Kru_monthly_2015)
rm(Vap_Grand_Kru_monthly_2015, Vap_Grand_Kru_monthly_2015a)
Vap_Grand_Kru_monthly$measurement <- "Vap"
Vap_Grand_Kru_monthly <- rename(Vap_Grand_Kru_monthly, Value=Vap)

#Lofa
Vap_Lofa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        Vap1=vap.var[which(lon==10.25),which(lat==8.25),1:48],
                                        Vap2=vap.var[which(lon==9.75),which(lat==8.25),1:48],
                                        Vap3=vap.var[which(lon==9.75),which(lat==7.75),1:48]))
Vap_Lofa_monthly$Vap <- rowMeans(select(Vap_Lofa_monthly, Vap1, Vap2, Vap3))
Vap_Lofa_monthly$Location <- 'Lofa'
Vap_Lofa_monthly$date <- ymd(paste(Vap_Lofa_monthly$Year, Vap_Lofa_monthly$Month, Vap_Lofa_monthly$day, sep="-"))
Vap_Lofa_monthly_2015 <- select(Vap_Lofa_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Lofa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Lofa_monthly_2015a$Location <- 'Lofa'
Vap_Lofa_monthly_2015a$date <- ymd(paste(Vap_Lofa_monthly_2015a$Year, Vap_Lofa_monthly_2015a$Month, Vap_Lofa_monthly_2015a$day, sep="-"))
Vap_Lofa_monthly_2015a <- select(Vap_Lofa_monthly_2015a, date, Year, Month, day, Location)
Vap_Lofa_monthly_2015 <- full_join(Vap_Lofa_monthly_2015a, Vap_Lofa_monthly_2015)
Vap_Lofa_monthly <- rbind(select(Vap_Lofa_monthly,date, Year, Month, day, Location, Vap), Vap_Lofa_monthly_2015)
rm(Vap_Lofa_monthly_2015, Vap_Lofa_monthly_2015a)
Vap_Lofa_monthly$measurement <- "Vap"
Vap_Lofa_monthly <- rename(Vap_Lofa_monthly, Value=Vap)

#Margibi
Vap_Margibi_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        Vap1=vap.var[which(lon==10.25),which(lat==6.75),1:48],
                                        Vap2=vap.var[which(lon==10.25),which(lat==6.25),1:48]))
Vap_Margibi_monthly$Vap <- rowMeans(select(Vap_Margibi_monthly, Vap1, Vap2))
Vap_Margibi_monthly$Location <- 'Margibi'
Vap_Margibi_monthly$date <- ymd(paste(Vap_Margibi_monthly$Year, Vap_Margibi_monthly$Month, Vap_Margibi_monthly$day, sep="-"))
Vap_Margibi_monthly_2015 <- select(Vap_Margibi_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Margibi_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Margibi_monthly_2015a$Location <- 'Margibi'
Vap_Margibi_monthly_2015a$date <- ymd(paste(Vap_Margibi_monthly_2015a$Year, Vap_Margibi_monthly_2015a$Month, Vap_Margibi_monthly_2015a$day, sep="-"))
Vap_Margibi_monthly_2015a <- select(Vap_Margibi_monthly_2015a, date, Year, Month, day, Location)
Vap_Margibi_monthly_2015 <- full_join(Vap_Margibi_monthly_2015a, Vap_Margibi_monthly_2015)
Vap_Margibi_monthly <- rbind(select(Vap_Margibi_monthly,date, Year, Month, day, Location, Vap), Vap_Margibi_monthly_2015)
rm(Vap_Margibi_monthly_2015, Vap_Margibi_monthly_2015a)
Vap_Margibi_monthly$measurement <- "Vap"
Vap_Margibi_monthly <- rename(Vap_Margibi_monthly, Value=Vap)

#Maryland
Vap_Maryland_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        Vap=vap.var[which(lon==7.75),which(lat==4.75),1:48]))
Vap_Maryland_monthly$Location <- 'Maryland'
Vap_Maryland_monthly$date <- ymd(paste(Vap_Maryland_monthly$Year, Vap_Maryland_monthly$Month, Vap_Maryland_monthly$day, sep="-"))
Vap_Maryland_monthly_2015 <- select(Vap_Maryland_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Maryland_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Maryland_monthly_2015a$Location <- 'Maryland'
Vap_Maryland_monthly_2015a$date <- ymd(paste(Vap_Maryland_monthly_2015a$Year, Vap_Maryland_monthly_2015a$Month, Vap_Maryland_monthly_2015a$day, sep="-"))
Vap_Maryland_monthly_2015a <- select(Vap_Maryland_monthly_2015a, date, Year, Month, day, Location)
Vap_Maryland_monthly_2015 <- full_join(Vap_Maryland_monthly_2015a, Vap_Maryland_monthly_2015)
Vap_Maryland_monthly <- rbind(select(Vap_Maryland_monthly,date, Year, Month, day, Location, Vap), Vap_Maryland_monthly_2015)
rm(Vap_Maryland_monthly_2015, Vap_Maryland_monthly_2015a)
Vap_Maryland_monthly$measurement <- "Vap"
Vap_Maryland_monthly <- rename(Vap_Maryland_monthly, Value=Vap)

#Montserrado
Vap_Montserrado_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        Vap=vap.var[which(lon==10.75),which(lat==6.25),1:48]))
Vap_Montserrado_monthly$Location <- 'Montserrado'
Vap_Montserrado_monthly$date <- ymd(paste(Vap_Montserrado_monthly$Year, Vap_Montserrado_monthly$Month, Vap_Montserrado_monthly$day, sep="-"))
Vap_Montserrado_monthly_2015 <- select(Vap_Montserrado_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Montserrado_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Montserrado_monthly_2015a$Location <- 'Montserrado'
Vap_Montserrado_monthly_2015a$date <- ymd(paste(Vap_Montserrado_monthly_2015a$Year, Vap_Montserrado_monthly_2015a$Month, Vap_Montserrado_monthly_2015a$day, sep="-"))
Vap_Montserrado_monthly_2015a <- select(Vap_Montserrado_monthly_2015a, date, Year, Month, day, Location)
Vap_Montserrado_monthly_2015 <- full_join(Vap_Montserrado_monthly_2015a, Vap_Montserrado_monthly_2015)
Vap_Montserrado_monthly <- rbind(select(Vap_Montserrado_monthly,date, Year, Month, day, Location, Vap), Vap_Montserrado_monthly_2015)
rm(Vap_Montserrado_monthly_2015, Vap_Montserrado_monthly_2015a)
Vap_Montserrado_monthly$measurement <- "Vap"
Vap_Montserrado_monthly <- rename(Vap_Montserrado_monthly, Value=Vap)

#Nimba
Vap_Nimba_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        Vap1=vap.var[which(lon==8.75),which(lat==7.25),1:48],
                                        Vap2=vap.var[which(lon==8.75),which(lat==6.75),1:48],
                                        Vap3=vap.var[which(lon==8.75),which(lat==6.25),1:48]))
Vap_Nimba_monthly$Vap <- rowMeans(select(Vap_Nimba_monthly, Vap1, Vap2, Vap3))
Vap_Nimba_monthly$Location <- 'Nimba'
Vap_Nimba_monthly$date <- ymd(paste(Vap_Nimba_monthly$Year, Vap_Nimba_monthly$Month, Vap_Nimba_monthly$day, sep="-"))
Vap_Nimba_monthly_2015 <- select(Vap_Nimba_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Nimba_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Nimba_monthly_2015a$Location <- 'Nimba'
Vap_Nimba_monthly_2015a$date <- ymd(paste(Vap_Nimba_monthly_2015a$Year, Vap_Nimba_monthly_2015a$Month, Vap_Nimba_monthly_2015a$day, sep="-"))
Vap_Nimba_monthly_2015a <- select(Vap_Nimba_monthly_2015a, date, Year, Month, day, Location)
Vap_Nimba_monthly_2015 <- full_join(Vap_Nimba_monthly_2015a, Vap_Nimba_monthly_2015)
Vap_Nimba_monthly <- rbind(select(Vap_Nimba_monthly,date, Year, Month, day, Location, Vap), Vap_Nimba_monthly_2015)
rm(Vap_Nimba_monthly_2015, Vap_Nimba_monthly_2015a)
Vap_Nimba_monthly$measurement <- "Vap"
Vap_Nimba_monthly <- rename(Vap_Nimba_monthly, Value=Vap)

#River_Gee
Vap_River_Gee_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        Vap1=vap.var[which(lon==8.25),which(lat==5.25),1:48],
                                        Vap2=vap.var[which(lon==7.75),which(lat==5.25),1:48]))
Vap_River_Gee_monthly$Vap <- rowMeans(select(Vap_River_Gee_monthly, Vap1, Vap2))
Vap_River_Gee_monthly$Location <- 'River_Gee'
Vap_River_Gee_monthly$date <- ymd(paste(Vap_River_Gee_monthly$Year, Vap_River_Gee_monthly$Month, Vap_River_Gee_monthly$day, sep="-"))
Vap_River_Gee_monthly_2015 <- select(Vap_River_Gee_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_River_Gee_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_River_Gee_monthly_2015a$Location <- 'River_Gee'
Vap_River_Gee_monthly_2015a$date <- ymd(paste(Vap_River_Gee_monthly_2015a$Year, Vap_River_Gee_monthly_2015a$Month, Vap_River_Gee_monthly_2015a$day, sep="-"))
Vap_River_Gee_monthly_2015a <- select(Vap_River_Gee_monthly_2015a, date, Year, Month, day, Location)
Vap_River_Gee_monthly_2015 <- full_join(Vap_River_Gee_monthly_2015a, Vap_River_Gee_monthly_2015)
Vap_River_Gee_monthly <- rbind(select(Vap_River_Gee_monthly,date, Year, Month, day, Location, Vap), Vap_River_Gee_monthly_2015)
rm(Vap_River_Gee_monthly_2015, Vap_River_Gee_monthly_2015a)
Vap_River_Gee_monthly$measurement <- "Vap"
Vap_River_Gee_monthly <- rename(Vap_River_Gee_monthly, Value=Vap)

#Rivercess
Vap_River_Cess_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        Vap1=vap.var[which(lon==9.25),which(lat==6.25),1:48],
                                        Vap2=vap.var[which(lon==9.25),which(lat==5.75),1:48],
                                        Vap3=vap.var[which(lon==9.75),which(lat==5.25),1:48]))
Vap_River_Cess_monthly$Vap <- rowMeans(select(Vap_River_Cess_monthly, Vap1, Vap2, Vap3))
Vap_River_Cess_monthly$Location <- 'River_Cess'
Vap_River_Cess_monthly$date <- ymd(paste(Vap_River_Cess_monthly$Year, Vap_River_Cess_monthly$Month, Vap_River_Cess_monthly$day, sep="-"))
Vap_River_Cess_monthly_2015 <- select(Vap_River_Cess_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_River_Cess_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_River_Cess_monthly_2015a$Location <- 'River_Cess'
Vap_River_Cess_monthly_2015a$date <- ymd(paste(Vap_River_Cess_monthly_2015a$Year, Vap_River_Cess_monthly_2015a$Month, Vap_River_Cess_monthly_2015a$day, sep="-"))
Vap_River_Cess_monthly_2015a <- select(Vap_River_Cess_monthly_2015a, date, Year, Month, day, Location)
Vap_River_Cess_monthly_2015 <- full_join(Vap_River_Cess_monthly_2015a, Vap_River_Cess_monthly_2015)
Vap_River_Cess_monthly <- rbind(select(Vap_River_Cess_monthly,date, Year, Month, day, Location, Vap), Vap_River_Cess_monthly_2015)
rm(Vap_River_Cess_monthly_2015, Vap_River_Cess_monthly_2015a)
Vap_River_Cess_monthly$measurement <- "Vap"
Vap_River_Cess_monthly <- rename(Vap_River_Cess_monthly, Value=Vap)

#Sinoe
Vap_Sinoe_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        Vap1=vap.var[which(lon==8.75),which(lat==5.75),1:48],
                                        Vap2=vap.var[which(lon==9.25),which(lat==5.25),1:48],
                                        Vap3=vap.var[which(lon==8.75),which(lat==5.25),1:48],
                                        Vap4=vap.var[which(lon==8.75),which(lat==4.75),1:48]))
Vap_Sinoe_monthly$Vap <- rowMeans(select(Vap_Sinoe_monthly, Vap1, Vap2, Vap3, Vap4))
Vap_Sinoe_monthly$Location <- 'Sinoe'
Vap_Sinoe_monthly$date <- ymd(paste(Vap_Sinoe_monthly$Year, Vap_Sinoe_monthly$Month, Vap_Sinoe_monthly$day, sep="-"))
Vap_Sinoe_monthly_2015 <- select(Vap_Sinoe_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Sinoe_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Sinoe_monthly_2015a$Location <- 'Sinoe'
Vap_Sinoe_monthly_2015a$date <- ymd(paste(Vap_Sinoe_monthly_2015a$Year, Vap_Sinoe_monthly_2015a$Month, Vap_Sinoe_monthly_2015a$day, sep="-"))
Vap_Sinoe_monthly_2015a <- select(Vap_Sinoe_monthly_2015a, date, Year, Month, day, Location)
Vap_Sinoe_monthly_2015 <- full_join(Vap_Sinoe_monthly_2015a, Vap_Sinoe_monthly_2015)
Vap_Sinoe_monthly <- rbind(select(Vap_Sinoe_monthly,date, Year, Month, day, Location, Vap), Vap_Sinoe_monthly_2015)
rm(Vap_Sinoe_monthly_2015, Vap_Sinoe_monthly_2015a)
Vap_Sinoe_monthly$measurement <- "Vap"
Vap_Sinoe_monthly <- rename(Vap_Sinoe_monthly, Value=Vap)

#Merging in long format
VapLiberiamonthly_district <- rbind(Vap_Bomi_monthly, Vap_Bong_monthly, Vap_Gbarpolu_monthly,
                                 Vap_Grand_Bassa_monthly, Vap_Grand_Cape_Mount_monthly, Vap_Grand_Gedeh_monthly,
                                 Vap_Grand_Kru_monthly, Vap_Lofa_monthly, Vap_Margibi_monthly,
                                 Vap_Maryland_monthly, Vap_Montserrado_monthly, Vap_Nimba_monthly,
                                 Vap_River_Gee_monthly, Vap_River_Cess_monthly, Vap_Sinoe_monthly)

#####################
#wet - wet days
#####################
wet.full <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.01.2011.2014.wet.dat.nc', write=FALSE)
wet.var <- var.get.nc(wet.full, "wet")
#Bomi
wet_Bomi_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        wet=wet.var[which(lon==10.75),which(lat==6.75),1:48]))
wet_Bomi_monthly$Location <- 'Bomi'
wet_Bomi_monthly$date <- ymd(paste(wet_Bomi_monthly$Year, wet_Bomi_monthly$Month, wet_Bomi_monthly$day, sep="-"))
wet_Bomi_monthly_2015 <- select(wet_Bomi_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Bomi_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Bomi_monthly_2015a$Location <- 'Bomi'
wet_Bomi_monthly_2015a$date <- ymd(paste(wet_Bomi_monthly_2015a$Year, wet_Bomi_monthly_2015a$Month, wet_Bomi_monthly_2015a$day, sep="-"))
wet_Bomi_monthly_2015a <- select(wet_Bomi_monthly_2015a, date, Year, Month, day, Location)
wet_Bomi_monthly_2015 <- full_join(wet_Bomi_monthly_2015a, wet_Bomi_monthly_2015)
wet_Bomi_monthly <- rbind(select(wet_Bomi_monthly,date, Year, Month, day, Location, wet), wet_Bomi_monthly_2015)
rm(wet_Bomi_monthly_2015, wet_Bomi_monthly_2015a)
wet_Bomi_monthly$measurement <- "wet"
wet_Bomi_monthly <- rename(wet_Bomi_monthly, Value=wet)

#Bong
wet_Bong_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        wet1=wet.var[which(lon==9.75),which(lat==7.25),1:48],
                                        wet2=wet.var[which(lon==9.25),which(lat==7.25),1:48],
                                        wet3=wet.var[which(lon==10.25),which(lat==6.75),1:48],
                                        wet4=wet.var[which(lon==9.75),which(lat==6.75),1:48],
                                        wet5=wet.var[which(lon==9.25),which(lat==6.75),1:48]))
wet_Bong_monthly$wet <- rowMeans(select(wet_Bong_monthly, wet1, wet2, wet3, wet4, wet5))
wet_Bong_monthly$Location <- 'Bong'
wet_Bong_monthly$date <- ymd(paste(wet_Bong_monthly$Year, wet_Bong_monthly$Month, wet_Bong_monthly$day, sep="-"))
wet_Bong_monthly_2015 <- select(wet_Bong_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Bong_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Bong_monthly_2015a$Location <- 'Bong'
wet_Bong_monthly_2015a$date <- ymd(paste(wet_Bong_monthly_2015a$Year, wet_Bong_monthly_2015a$Month, wet_Bong_monthly_2015a$day, sep="-"))
wet_Bong_monthly_2015a <- select(wet_Bong_monthly_2015a, date, Year, Month, day, Location)
wet_Bong_monthly_2015 <- full_join(wet_Bong_monthly_2015a, wet_Bong_monthly_2015)
wet_Bong_monthly <- rbind(select(wet_Bong_monthly,date, Year, Month, day, Location, wet), wet_Bong_monthly_2015)
rm(wet_Bong_monthly_2015, wet_Bong_monthly_2015a)
wet_Bong_monthly$measurement <- "wet"
wet_Bong_monthly <- rename(wet_Bong_monthly, Value=wet)

#Gbarpolu
wet_Gbarpolu_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            wet1=wet.var[which(lon==10.25),which(lat==7.75),1:48],
                                            wet2=wet.var[which(lon==10.25),which(lat==7.25),1:48],
                                            wet3=wet.var[which(lon==9.75),which(lat==7.25),1:48]))
wet_Gbarpolu_monthly$wet <- rowMeans(select(wet_Gbarpolu_monthly, wet1, wet2, wet3))
wet_Gbarpolu_monthly$Location <- 'Gbarpolu'
wet_Gbarpolu_monthly$date <- ymd(paste(wet_Gbarpolu_monthly$Year, wet_Gbarpolu_monthly$Month, wet_Gbarpolu_monthly$day, sep="-"))
wet_Gbarpolu_monthly_2015 <- select(wet_Gbarpolu_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Gbarpolu_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Gbarpolu_monthly_2015a$Location <- 'Gbarpolu'
wet_Gbarpolu_monthly_2015a$date <- ymd(paste(wet_Gbarpolu_monthly_2015a$Year, wet_Gbarpolu_monthly_2015a$Month, wet_Gbarpolu_monthly_2015a$day, sep="-"))
wet_Gbarpolu_monthly_2015a <- select(wet_Gbarpolu_monthly_2015a, date, Year, Month, day, Location)
wet_Gbarpolu_monthly_2015 <- full_join(wet_Gbarpolu_monthly_2015a, wet_Gbarpolu_monthly_2015)
wet_Gbarpolu_monthly <- rbind(select(wet_Gbarpolu_monthly,date, Year, Month, day, Location, wet), wet_Gbarpolu_monthly_2015)
rm(wet_Gbarpolu_monthly_2015, wet_Gbarpolu_monthly_2015a)
wet_Gbarpolu_monthly$measurement <- "wet"
wet_Gbarpolu_monthly <- rename(wet_Gbarpolu_monthly, Value=wet)

#Grand_Bassa
wet_Grand_Bassa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               wet1=wet.var[which(lon==10.25),which(lat==6.25),1:48],
                                               wet2=wet.var[which(lon==9.75),which(lat==6.25),1:48]))
wet_Grand_Bassa_monthly$wet <- rowMeans(select(wet_Grand_Bassa_monthly, wet1, wet2))
wet_Grand_Bassa_monthly$Location <- 'Grand_Bassa'
wet_Grand_Bassa_monthly$date <- ymd(paste(wet_Grand_Bassa_monthly$Year, wet_Grand_Bassa_monthly$Month, wet_Grand_Bassa_monthly$day, sep="-"))
wet_Grand_Bassa_monthly_2015 <- select(wet_Grand_Bassa_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Grand_Bassa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Grand_Bassa_monthly_2015a$Location <- 'Grand_Bassa'
wet_Grand_Bassa_monthly_2015a$date <- ymd(paste(wet_Grand_Bassa_monthly_2015a$Year, wet_Grand_Bassa_monthly_2015a$Month, wet_Grand_Bassa_monthly_2015a$day, sep="-"))
wet_Grand_Bassa_monthly_2015a <- select(wet_Grand_Bassa_monthly_2015a, date, Year, Month, day, Location)
wet_Grand_Bassa_monthly_2015 <- full_join(wet_Grand_Bassa_monthly_2015a, wet_Grand_Bassa_monthly_2015)
wet_Grand_Bassa_monthly <- rbind(select(wet_Grand_Bassa_monthly,date, Year, Month, day, Location, wet), wet_Grand_Bassa_monthly_2015)
rm(wet_Grand_Bassa_monthly_2015, wet_Grand_Bassa_monthly_2015a)
wet_Grand_Bassa_monthly$measurement <- "wet"
wet_Grand_Bassa_monthly <- rename(wet_Grand_Bassa_monthly, Value=wet)

#Grand_Cape_Mount
wet_Grand_Cape_Mount_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                                    Month=rep(seq(1,12,1), 4), day=1,
                                                    wet1=wet.var[which(lon==11.25),which(lat==7.25),1:48],
                                                    wet2=wet.var[which(lon==10.75),which(lat==7.25),1:48],
                                                    wet3=wet.var[which(lon==11.25),which(lat==6.75),1:48]))
wet_Grand_Cape_Mount_monthly$wet <- rowMeans(select(wet_Grand_Cape_Mount_monthly, wet1, wet2, wet3))
wet_Grand_Cape_Mount_monthly$Location <- 'Grand_Cape_Mount'
wet_Grand_Cape_Mount_monthly$date <- ymd(paste(wet_Grand_Cape_Mount_monthly$Year, wet_Grand_Cape_Mount_monthly$Month, wet_Grand_Cape_Mount_monthly$day, sep="-"))
wet_Grand_Cape_Mount_monthly_2015 <- select(wet_Grand_Cape_Mount_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Grand_Cape_Mount_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Grand_Cape_Mount_monthly_2015a$Location <- 'Grand_Cape_Mount'
wet_Grand_Cape_Mount_monthly_2015a$date <- ymd(paste(wet_Grand_Cape_Mount_monthly_2015a$Year, wet_Grand_Cape_Mount_monthly_2015a$Month, wet_Grand_Cape_Mount_monthly_2015a$day, sep="-"))
wet_Grand_Cape_Mount_monthly_2015a <- select(wet_Grand_Cape_Mount_monthly_2015a, date, Year, Month, day, Location)
wet_Grand_Cape_Mount_monthly_2015 <- full_join(wet_Grand_Cape_Mount_monthly_2015a, wet_Grand_Cape_Mount_monthly_2015)
wet_Grand_Cape_Mount_monthly <- rbind(select(wet_Grand_Cape_Mount_monthly,date, Year, Month, day, Location, wet), wet_Grand_Cape_Mount_monthly_2015)
rm(wet_Grand_Cape_Mount_monthly_2015, wet_Grand_Cape_Mount_monthly_2015a)
wet_Grand_Cape_Mount_monthly$measurement <- "wet"
wet_Grand_Cape_Mount_monthly <- rename(wet_Grand_Cape_Mount_monthly, Value=wet)

#Grand_Gedeh
wet_Grand_Gedeh_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               wet1=wet.var[which(lon==8.75),which(lat==6.25),1:48],
                                               wet2=wet.var[which(lon==8.25),which(lat==6.25),1:48],
                                               wet3=wet.var[which(lon==8.75),which(lat==5.75),1:48],
                                               wet4=wet.var[which(lon==8.25),which(lat==5.75),1:48],
                                               wet5=wet.var[which(lon==7.75),which(lat==5.75),1:48]))
wet_Grand_Gedeh_monthly$wet <- rowMeans(select(wet_Grand_Gedeh_monthly, wet1, wet2, wet3, wet4, wet5))
wet_Grand_Gedeh_monthly$Location <- 'Grand_Gedeh'
wet_Grand_Gedeh_monthly$date <- ymd(paste(wet_Grand_Gedeh_monthly$Year, wet_Grand_Gedeh_monthly$Month, wet_Grand_Gedeh_monthly$day, sep="-"))
wet_Grand_Gedeh_monthly_2015 <- select(wet_Grand_Gedeh_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Grand_Gedeh_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Grand_Gedeh_monthly_2015a$Location <- 'Grand_Gedeh'
wet_Grand_Gedeh_monthly_2015a$date <- ymd(paste(wet_Grand_Gedeh_monthly_2015a$Year, wet_Grand_Gedeh_monthly_2015a$Month, wet_Grand_Gedeh_monthly_2015a$day, sep="-"))
wet_Grand_Gedeh_monthly_2015a <- select(wet_Grand_Gedeh_monthly_2015a, date, Year, Month, day, Location)
wet_Grand_Gedeh_monthly_2015 <- full_join(wet_Grand_Gedeh_monthly_2015a, wet_Grand_Gedeh_monthly_2015)
wet_Grand_Gedeh_monthly <- rbind(select(wet_Grand_Gedeh_monthly,date, Year, Month, day, Location, wet), wet_Grand_Gedeh_monthly_2015)
rm(wet_Grand_Gedeh_monthly_2015, wet_Grand_Gedeh_monthly_2015a)
wet_Grand_Gedeh_monthly$measurement <- "wet"
wet_Grand_Gedeh_monthly <- rename(wet_Grand_Gedeh_monthly, Value=wet)

#Grand_Kru
wet_Grand_Kru_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             wet=wet.var[which(lon==8.25),which(lat==4.75),1:48]))
wet_Grand_Kru_monthly$Location <- 'Grand_Kru'
wet_Grand_Kru_monthly$date <- ymd(paste(wet_Grand_Kru_monthly$Year, wet_Grand_Kru_monthly$Month, wet_Grand_Kru_monthly$day, sep="-"))
wet_Grand_Kru_monthly_2015 <- select(wet_Grand_Kru_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Grand_Kru_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Grand_Kru_monthly_2015a$Location <- 'Grand_Kru'
wet_Grand_Kru_monthly_2015a$date <- ymd(paste(wet_Grand_Kru_monthly_2015a$Year, wet_Grand_Kru_monthly_2015a$Month, wet_Grand_Kru_monthly_2015a$day, sep="-"))
wet_Grand_Kru_monthly_2015a <- select(wet_Grand_Kru_monthly_2015a, date, Year, Month, day, Location)
wet_Grand_Kru_monthly_2015 <- full_join(wet_Grand_Kru_monthly_2015a, wet_Grand_Kru_monthly_2015)
wet_Grand_Kru_monthly <- rbind(select(wet_Grand_Kru_monthly,date, Year, Month, day, Location, wet), wet_Grand_Kru_monthly_2015)
rm(wet_Grand_Kru_monthly_2015, wet_Grand_Kru_monthly_2015a)
wet_Grand_Kru_monthly$measurement <- "wet"
wet_Grand_Kru_monthly <- rename(wet_Grand_Kru_monthly, Value=wet)

#Lofa
wet_Lofa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        wet1=wet.var[which(lon==10.25),which(lat==8.25),1:48],
                                        wet2=wet.var[which(lon==9.75),which(lat==8.25),1:48],
                                        wet3=wet.var[which(lon==9.75),which(lat==7.75),1:48]))
wet_Lofa_monthly$wet <- rowMeans(select(wet_Lofa_monthly, wet1, wet2, wet3))
wet_Lofa_monthly$Location <- 'Lofa'
wet_Lofa_monthly$date <- ymd(paste(wet_Lofa_monthly$Year, wet_Lofa_monthly$Month, wet_Lofa_monthly$day, sep="-"))
wet_Lofa_monthly_2015 <- select(wet_Lofa_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Lofa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Lofa_monthly_2015a$Location <- 'Lofa'
wet_Lofa_monthly_2015a$date <- ymd(paste(wet_Lofa_monthly_2015a$Year, wet_Lofa_monthly_2015a$Month, wet_Lofa_monthly_2015a$day, sep="-"))
wet_Lofa_monthly_2015a <- select(wet_Lofa_monthly_2015a, date, Year, Month, day, Location)
wet_Lofa_monthly_2015 <- full_join(wet_Lofa_monthly_2015a, wet_Lofa_monthly_2015)
wet_Lofa_monthly <- rbind(select(wet_Lofa_monthly,date, Year, Month, day, Location, wet), wet_Lofa_monthly_2015)
rm(wet_Lofa_monthly_2015, wet_Lofa_monthly_2015a)
wet_Lofa_monthly$measurement <- "wet"
wet_Lofa_monthly <- rename(wet_Lofa_monthly, Value=wet)

#Margibi
wet_Margibi_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           wet1=wet.var[which(lon==10.25),which(lat==6.75),1:48],
                                           wet2=wet.var[which(lon==10.25),which(lat==6.25),1:48]))
wet_Margibi_monthly$wet <- rowMeans(select(wet_Margibi_monthly, wet1, wet2))
wet_Margibi_monthly$Location <- 'Margibi'
wet_Margibi_monthly$date <- ymd(paste(wet_Margibi_monthly$Year, wet_Margibi_monthly$Month, wet_Margibi_monthly$day, sep="-"))
wet_Margibi_monthly_2015 <- select(wet_Margibi_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Margibi_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Margibi_monthly_2015a$Location <- 'Margibi'
wet_Margibi_monthly_2015a$date <- ymd(paste(wet_Margibi_monthly_2015a$Year, wet_Margibi_monthly_2015a$Month, wet_Margibi_monthly_2015a$day, sep="-"))
wet_Margibi_monthly_2015a <- select(wet_Margibi_monthly_2015a, date, Year, Month, day, Location)
wet_Margibi_monthly_2015 <- full_join(wet_Margibi_monthly_2015a, wet_Margibi_monthly_2015)
wet_Margibi_monthly <- rbind(select(wet_Margibi_monthly,date, Year, Month, day, Location, wet), wet_Margibi_monthly_2015)
rm(wet_Margibi_monthly_2015, wet_Margibi_monthly_2015a)
wet_Margibi_monthly$measurement <- "wet"
wet_Margibi_monthly <- rename(wet_Margibi_monthly, Value=wet)

#Maryland
wet_Maryland_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            wet=wet.var[which(lon==7.75),which(lat==4.75),1:48]))
wet_Maryland_monthly$Location <- 'Maryland'
wet_Maryland_monthly$date <- ymd(paste(wet_Maryland_monthly$Year, wet_Maryland_monthly$Month, wet_Maryland_monthly$day, sep="-"))
wet_Maryland_monthly_2015 <- select(wet_Maryland_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Maryland_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Maryland_monthly_2015a$Location <- 'Maryland'
wet_Maryland_monthly_2015a$date <- ymd(paste(wet_Maryland_monthly_2015a$Year, wet_Maryland_monthly_2015a$Month, wet_Maryland_monthly_2015a$day, sep="-"))
wet_Maryland_monthly_2015a <- select(wet_Maryland_monthly_2015a, date, Year, Month, day, Location)
wet_Maryland_monthly_2015 <- full_join(wet_Maryland_monthly_2015a, wet_Maryland_monthly_2015)
wet_Maryland_monthly <- rbind(select(wet_Maryland_monthly,date, Year, Month, day, Location, wet), wet_Maryland_monthly_2015)
rm(wet_Maryland_monthly_2015, wet_Maryland_monthly_2015a)
wet_Maryland_monthly$measurement <- "wet"
wet_Maryland_monthly <- rename(wet_Maryland_monthly, Value=wet)

#Montserrado
wet_Montserrado_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               wet=wet.var[which(lon==10.75),which(lat==6.25),1:48]))
wet_Montserrado_monthly$Location <- 'Montserrado'
wet_Montserrado_monthly$date <- ymd(paste(wet_Montserrado_monthly$Year, wet_Montserrado_monthly$Month, wet_Montserrado_monthly$day, sep="-"))
wet_Montserrado_monthly_2015 <- select(wet_Montserrado_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Montserrado_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Montserrado_monthly_2015a$Location <- 'Montserrado'
wet_Montserrado_monthly_2015a$date <- ymd(paste(wet_Montserrado_monthly_2015a$Year, wet_Montserrado_monthly_2015a$Month, wet_Montserrado_monthly_2015a$day, sep="-"))
wet_Montserrado_monthly_2015a <- select(wet_Montserrado_monthly_2015a, date, Year, Month, day, Location)
wet_Montserrado_monthly_2015 <- full_join(wet_Montserrado_monthly_2015a, wet_Montserrado_monthly_2015)
wet_Montserrado_monthly <- rbind(select(wet_Montserrado_monthly,date, Year, Month, day, Location, wet), wet_Montserrado_monthly_2015)
rm(wet_Montserrado_monthly_2015, wet_Montserrado_monthly_2015a)
wet_Montserrado_monthly$measurement <- "wet"
wet_Montserrado_monthly <- rename(wet_Montserrado_monthly, Value=wet)

#Nimba
wet_Nimba_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         wet1=wet.var[which(lon==8.75),which(lat==7.25),1:48],
                                         wet2=wet.var[which(lon==8.75),which(lat==6.75),1:48],
                                         wet3=wet.var[which(lon==8.75),which(lat==6.25),1:48]))
wet_Nimba_monthly$wet <- rowMeans(select(wet_Nimba_monthly, wet1, wet2, wet3))
wet_Nimba_monthly$Location <- 'Nimba'
wet_Nimba_monthly$date <- ymd(paste(wet_Nimba_monthly$Year, wet_Nimba_monthly$Month, wet_Nimba_monthly$day, sep="-"))
wet_Nimba_monthly_2015 <- select(wet_Nimba_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Nimba_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Nimba_monthly_2015a$Location <- 'Nimba'
wet_Nimba_monthly_2015a$date <- ymd(paste(wet_Nimba_monthly_2015a$Year, wet_Nimba_monthly_2015a$Month, wet_Nimba_monthly_2015a$day, sep="-"))
wet_Nimba_monthly_2015a <- select(wet_Nimba_monthly_2015a, date, Year, Month, day, Location)
wet_Nimba_monthly_2015 <- full_join(wet_Nimba_monthly_2015a, wet_Nimba_monthly_2015)
wet_Nimba_monthly <- rbind(select(wet_Nimba_monthly,date, Year, Month, day, Location, wet), wet_Nimba_monthly_2015)
rm(wet_Nimba_monthly_2015, wet_Nimba_monthly_2015a)
wet_Nimba_monthly$measurement <- "wet"
wet_Nimba_monthly <- rename(wet_Nimba_monthly, Value=wet)

#River_Gee
wet_River_Gee_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             wet1=wet.var[which(lon==8.25),which(lat==5.25),1:48],
                                             wet2=wet.var[which(lon==7.75),which(lat==5.25),1:48]))
wet_River_Gee_monthly$wet <- rowMeans(select(wet_River_Gee_monthly, wet1, wet2))
wet_River_Gee_monthly$Location <- 'River_Gee'
wet_River_Gee_monthly$date <- ymd(paste(wet_River_Gee_monthly$Year, wet_River_Gee_monthly$Month, wet_River_Gee_monthly$day, sep="-"))
wet_River_Gee_monthly_2015 <- select(wet_River_Gee_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_River_Gee_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_River_Gee_monthly_2015a$Location <- 'River_Gee'
wet_River_Gee_monthly_2015a$date <- ymd(paste(wet_River_Gee_monthly_2015a$Year, wet_River_Gee_monthly_2015a$Month, wet_River_Gee_monthly_2015a$day, sep="-"))
wet_River_Gee_monthly_2015a <- select(wet_River_Gee_monthly_2015a, date, Year, Month, day, Location)
wet_River_Gee_monthly_2015 <- full_join(wet_River_Gee_monthly_2015a, wet_River_Gee_monthly_2015)
wet_River_Gee_monthly <- rbind(select(wet_River_Gee_monthly,date, Year, Month, day, Location, wet), wet_River_Gee_monthly_2015)
rm(wet_River_Gee_monthly_2015, wet_River_Gee_monthly_2015a)
wet_River_Gee_monthly$measurement <- "wet"
wet_River_Gee_monthly <- rename(wet_River_Gee_monthly, Value=wet)

#Rivercess
wet_River_Cess_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                              Month=rep(seq(1,12,1), 4), day=1,
                                              wet1=wet.var[which(lon==9.25),which(lat==6.25),1:48],
                                              wet2=wet.var[which(lon==9.25),which(lat==5.75),1:48],
                                              wet3=wet.var[which(lon==9.75),which(lat==5.25),1:48]))
wet_River_Cess_monthly$wet <- rowMeans(select(wet_River_Cess_monthly, wet1, wet2, wet3))
wet_River_Cess_monthly$Location <- 'River_Cess'
wet_River_Cess_monthly$date <- ymd(paste(wet_River_Cess_monthly$Year, wet_River_Cess_monthly$Month, wet_River_Cess_monthly$day, sep="-"))
wet_River_Cess_monthly_2015 <- select(wet_River_Cess_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_River_Cess_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_River_Cess_monthly_2015a$Location <- 'River_Cess'
wet_River_Cess_monthly_2015a$date <- ymd(paste(wet_River_Cess_monthly_2015a$Year, wet_River_Cess_monthly_2015a$Month, wet_River_Cess_monthly_2015a$day, sep="-"))
wet_River_Cess_monthly_2015a <- select(wet_River_Cess_monthly_2015a, date, Year, Month, day, Location)
wet_River_Cess_monthly_2015 <- full_join(wet_River_Cess_monthly_2015a, wet_River_Cess_monthly_2015)
wet_River_Cess_monthly <- rbind(select(wet_River_Cess_monthly,date, Year, Month, day, Location, wet), wet_River_Cess_monthly_2015)
rm(wet_River_Cess_monthly_2015, wet_River_Cess_monthly_2015a)
wet_River_Cess_monthly$measurement <- "wet"
wet_River_Cess_monthly <- rename(wet_River_Cess_monthly, Value=wet)

#Sinoe
wet_Sinoe_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         wet1=wet.var[which(lon==8.75),which(lat==5.75),1:48],
                                         wet2=wet.var[which(lon==9.25),which(lat==5.25),1:48],
                                         wet3=wet.var[which(lon==8.75),which(lat==5.25),1:48],
                                         wet4=wet.var[which(lon==8.75),which(lat==4.75),1:48]))
wet_Sinoe_monthly$wet <- rowMeans(select(wet_Sinoe_monthly, wet1, wet2, wet3, wet4))
wet_Sinoe_monthly$Location <- 'Sinoe'
wet_Sinoe_monthly$date <- ymd(paste(wet_Sinoe_monthly$Year, wet_Sinoe_monthly$Month, wet_Sinoe_monthly$day, sep="-"))
wet_Sinoe_monthly_2015 <- select(wet_Sinoe_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Sinoe_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Sinoe_monthly_2015a$Location <- 'Sinoe'
wet_Sinoe_monthly_2015a$date <- ymd(paste(wet_Sinoe_monthly_2015a$Year, wet_Sinoe_monthly_2015a$Month, wet_Sinoe_monthly_2015a$day, sep="-"))
wet_Sinoe_monthly_2015a <- select(wet_Sinoe_monthly_2015a, date, Year, Month, day, Location)
wet_Sinoe_monthly_2015 <- full_join(wet_Sinoe_monthly_2015a, wet_Sinoe_monthly_2015)
wet_Sinoe_monthly <- rbind(select(wet_Sinoe_monthly,date, Year, Month, day, Location, wet), wet_Sinoe_monthly_2015)
rm(wet_Sinoe_monthly_2015, wet_Sinoe_monthly_2015a)
wet_Sinoe_monthly$measurement <- "wet"
wet_Sinoe_monthly <- rename(wet_Sinoe_monthly, Value=wet)

#Merging in long format
wetLiberiamonthly_district <- rbind(wet_Bomi_monthly, wet_Bong_monthly, wet_Gbarpolu_monthly,
                                 wet_Grand_Bassa_monthly, wet_Grand_Cape_Mount_monthly, wet_Grand_Gedeh_monthly,
                                 wet_Grand_Kru_monthly, wet_Lofa_monthly, wet_Margibi_monthly,
                                 wet_Maryland_monthly, wet_Montserrado_monthly, wet_Nimba_monthly,
                                 wet_River_Gee_monthly, wet_River_Cess_monthly, wet_Sinoe_monthly)

#####################
#dtr - daily temp range
#####################
dtr.full <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.dtr.dat.nc', write=FALSE)
dtr.var <- var.get.nc(dtr.full, "dtr")

#Bomi
dtr_Bomi_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        dtr=dtr.var[which(lon==10.75),which(lat==6.75),1:48]))
dtr_Bomi_monthly$Location <- 'Bomi'
dtr_Bomi_monthly$date <- ymd(paste(dtr_Bomi_monthly$Year, dtr_Bomi_monthly$Month, dtr_Bomi_monthly$day, sep="-"))
dtr_Bomi_monthly_2015 <- select(dtr_Bomi_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Bomi_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Bomi_monthly_2015a$Location <- 'Bomi'
dtr_Bomi_monthly_2015a$date <- ymd(paste(dtr_Bomi_monthly_2015a$Year, dtr_Bomi_monthly_2015a$Month, dtr_Bomi_monthly_2015a$day, sep="-"))
dtr_Bomi_monthly_2015a <- select(dtr_Bomi_monthly_2015a, date, Year, Month, day, Location)
dtr_Bomi_monthly_2015 <- full_join(dtr_Bomi_monthly_2015a, dtr_Bomi_monthly_2015)
dtr_Bomi_monthly <- rbind(select(dtr_Bomi_monthly,date, Year, Month, day, Location, dtr), dtr_Bomi_monthly_2015)
rm(dtr_Bomi_monthly_2015, dtr_Bomi_monthly_2015a)
dtr_Bomi_monthly$measurement <- "dtr"
dtr_Bomi_monthly <- rename(dtr_Bomi_monthly, Value=dtr)

#Bong
dtr_Bong_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        dtr1=dtr.var[which(lon==9.75),which(lat==7.25),1:48],
                                        dtr2=dtr.var[which(lon==9.25),which(lat==7.25),1:48],
                                        dtr3=dtr.var[which(lon==10.25),which(lat==6.75),1:48],
                                        dtr4=dtr.var[which(lon==9.75),which(lat==6.75),1:48],
                                        dtr5=dtr.var[which(lon==9.25),which(lat==6.75),1:48]))
dtr_Bong_monthly$dtr <- rowMeans(select(dtr_Bong_monthly, dtr1, dtr2, dtr3, dtr4, dtr5))
dtr_Bong_monthly$Location <- 'Bong'
dtr_Bong_monthly$date <- ymd(paste(dtr_Bong_monthly$Year, dtr_Bong_monthly$Month, dtr_Bong_monthly$day, sep="-"))
dtr_Bong_monthly_2015 <- select(dtr_Bong_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Bong_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Bong_monthly_2015a$Location <- 'Bong'
dtr_Bong_monthly_2015a$date <- ymd(paste(dtr_Bong_monthly_2015a$Year, dtr_Bong_monthly_2015a$Month, dtr_Bong_monthly_2015a$day, sep="-"))
dtr_Bong_monthly_2015a <- select(dtr_Bong_monthly_2015a, date, Year, Month, day, Location)
dtr_Bong_monthly_2015 <- full_join(dtr_Bong_monthly_2015a, dtr_Bong_monthly_2015)
dtr_Bong_monthly <- rbind(select(dtr_Bong_monthly,date, Year, Month, day, Location, dtr), dtr_Bong_monthly_2015)
rm(dtr_Bong_monthly_2015, dtr_Bong_monthly_2015a)
dtr_Bong_monthly$measurement <- "dtr"
dtr_Bong_monthly <- rename(dtr_Bong_monthly, Value=dtr)

#Gbarpolu
dtr_Gbarpolu_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            dtr1=dtr.var[which(lon==10.25),which(lat==7.75),1:48],
                                            dtr2=dtr.var[which(lon==10.25),which(lat==7.25),1:48],
                                            dtr3=dtr.var[which(lon==9.75),which(lat==7.25),1:48]))
dtr_Gbarpolu_monthly$dtr <- rowMeans(select(dtr_Gbarpolu_monthly, dtr1, dtr2, dtr3))
dtr_Gbarpolu_monthly$Location <- 'Gbarpolu'
dtr_Gbarpolu_monthly$date <- ymd(paste(dtr_Gbarpolu_monthly$Year, dtr_Gbarpolu_monthly$Month, dtr_Gbarpolu_monthly$day, sep="-"))
dtr_Gbarpolu_monthly_2015 <- select(dtr_Gbarpolu_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Gbarpolu_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Gbarpolu_monthly_2015a$Location <- 'Gbarpolu'
dtr_Gbarpolu_monthly_2015a$date <- ymd(paste(dtr_Gbarpolu_monthly_2015a$Year, dtr_Gbarpolu_monthly_2015a$Month, dtr_Gbarpolu_monthly_2015a$day, sep="-"))
dtr_Gbarpolu_monthly_2015a <- select(dtr_Gbarpolu_monthly_2015a, date, Year, Month, day, Location)
dtr_Gbarpolu_monthly_2015 <- full_join(dtr_Gbarpolu_monthly_2015a, dtr_Gbarpolu_monthly_2015)
dtr_Gbarpolu_monthly <- rbind(select(dtr_Gbarpolu_monthly,date, Year, Month, day, Location, dtr), dtr_Gbarpolu_monthly_2015)
rm(dtr_Gbarpolu_monthly_2015, dtr_Gbarpolu_monthly_2015a)
dtr_Gbarpolu_monthly$measurement <- "dtr"
dtr_Gbarpolu_monthly <- rename(dtr_Gbarpolu_monthly, Value=dtr)

#Grand_Bassa
dtr_Grand_Bassa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               dtr1=dtr.var[which(lon==10.25),which(lat==6.25),1:48],
                                               dtr2=dtr.var[which(lon==9.75),which(lat==6.25),1:48]))
dtr_Grand_Bassa_monthly$dtr <- rowMeans(select(dtr_Grand_Bassa_monthly, dtr1, dtr2))
dtr_Grand_Bassa_monthly$Location <- 'Grand_Bassa'
dtr_Grand_Bassa_monthly$date <- ymd(paste(dtr_Grand_Bassa_monthly$Year, dtr_Grand_Bassa_monthly$Month, dtr_Grand_Bassa_monthly$day, sep="-"))
dtr_Grand_Bassa_monthly_2015 <- select(dtr_Grand_Bassa_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Grand_Bassa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Grand_Bassa_monthly_2015a$Location <- 'Grand_Bassa'
dtr_Grand_Bassa_monthly_2015a$date <- ymd(paste(dtr_Grand_Bassa_monthly_2015a$Year, dtr_Grand_Bassa_monthly_2015a$Month, dtr_Grand_Bassa_monthly_2015a$day, sep="-"))
dtr_Grand_Bassa_monthly_2015a <- select(dtr_Grand_Bassa_monthly_2015a, date, Year, Month, day, Location)
dtr_Grand_Bassa_monthly_2015 <- full_join(dtr_Grand_Bassa_monthly_2015a, dtr_Grand_Bassa_monthly_2015)
dtr_Grand_Bassa_monthly <- rbind(select(dtr_Grand_Bassa_monthly,date, Year, Month, day, Location, dtr), dtr_Grand_Bassa_monthly_2015)
rm(dtr_Grand_Bassa_monthly_2015, dtr_Grand_Bassa_monthly_2015a)
dtr_Grand_Bassa_monthly$measurement <- "dtr"
dtr_Grand_Bassa_monthly <- rename(dtr_Grand_Bassa_monthly, Value=dtr)

#Grand_Cape_Mount
dtr_Grand_Cape_Mount_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                                    Month=rep(seq(1,12,1), 4), day=1,
                                                    dtr1=dtr.var[which(lon==11.25),which(lat==7.25),1:48],
                                                    dtr2=dtr.var[which(lon==10.75),which(lat==7.25),1:48],
                                                    dtr3=dtr.var[which(lon==11.25),which(lat==6.75),1:48]))
dtr_Grand_Cape_Mount_monthly$dtr <- rowMeans(select(dtr_Grand_Cape_Mount_monthly, dtr1, dtr2, dtr3))
dtr_Grand_Cape_Mount_monthly$Location <- 'Grand_Cape_Mount'
dtr_Grand_Cape_Mount_monthly$date <- ymd(paste(dtr_Grand_Cape_Mount_monthly$Year, dtr_Grand_Cape_Mount_monthly$Month, dtr_Grand_Cape_Mount_monthly$day, sep="-"))
dtr_Grand_Cape_Mount_monthly_2015 <- select(dtr_Grand_Cape_Mount_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Grand_Cape_Mount_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Grand_Cape_Mount_monthly_2015a$Location <- 'Grand_Cape_Mount'
dtr_Grand_Cape_Mount_monthly_2015a$date <- ymd(paste(dtr_Grand_Cape_Mount_monthly_2015a$Year, dtr_Grand_Cape_Mount_monthly_2015a$Month, dtr_Grand_Cape_Mount_monthly_2015a$day, sep="-"))
dtr_Grand_Cape_Mount_monthly_2015a <- select(dtr_Grand_Cape_Mount_monthly_2015a, date, Year, Month, day, Location)
dtr_Grand_Cape_Mount_monthly_2015 <- full_join(dtr_Grand_Cape_Mount_monthly_2015a, dtr_Grand_Cape_Mount_monthly_2015)
dtr_Grand_Cape_Mount_monthly <- rbind(select(dtr_Grand_Cape_Mount_monthly,date, Year, Month, day, Location, dtr), dtr_Grand_Cape_Mount_monthly_2015)
rm(dtr_Grand_Cape_Mount_monthly_2015, dtr_Grand_Cape_Mount_monthly_2015a)
dtr_Grand_Cape_Mount_monthly$measurement <- "dtr"
dtr_Grand_Cape_Mount_monthly <- rename(dtr_Grand_Cape_Mount_monthly, Value=dtr)

#Grand_Gedeh
dtr_Grand_Gedeh_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               dtr1=dtr.var[which(lon==8.75),which(lat==6.25),1:48],
                                               dtr2=dtr.var[which(lon==8.25),which(lat==6.25),1:48],
                                               dtr3=dtr.var[which(lon==8.75),which(lat==5.75),1:48],
                                               dtr4=dtr.var[which(lon==8.25),which(lat==5.75),1:48],
                                               dtr5=dtr.var[which(lon==7.75),which(lat==5.75),1:48]))
dtr_Grand_Gedeh_monthly$dtr <- rowMeans(select(dtr_Grand_Gedeh_monthly, dtr1, dtr2, dtr3, dtr4, dtr5))
dtr_Grand_Gedeh_monthly$Location <- 'Grand_Gedeh'
dtr_Grand_Gedeh_monthly$date <- ymd(paste(dtr_Grand_Gedeh_monthly$Year, dtr_Grand_Gedeh_monthly$Month, dtr_Grand_Gedeh_monthly$day, sep="-"))
dtr_Grand_Gedeh_monthly_2015 <- select(dtr_Grand_Gedeh_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Grand_Gedeh_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Grand_Gedeh_monthly_2015a$Location <- 'Grand_Gedeh'
dtr_Grand_Gedeh_monthly_2015a$date <- ymd(paste(dtr_Grand_Gedeh_monthly_2015a$Year, dtr_Grand_Gedeh_monthly_2015a$Month, dtr_Grand_Gedeh_monthly_2015a$day, sep="-"))
dtr_Grand_Gedeh_monthly_2015a <- select(dtr_Grand_Gedeh_monthly_2015a, date, Year, Month, day, Location)
dtr_Grand_Gedeh_monthly_2015 <- full_join(dtr_Grand_Gedeh_monthly_2015a, dtr_Grand_Gedeh_monthly_2015)
dtr_Grand_Gedeh_monthly <- rbind(select(dtr_Grand_Gedeh_monthly,date, Year, Month, day, Location, dtr), dtr_Grand_Gedeh_monthly_2015)
rm(dtr_Grand_Gedeh_monthly_2015, dtr_Grand_Gedeh_monthly_2015a)
dtr_Grand_Gedeh_monthly$measurement <- "dtr"
dtr_Grand_Gedeh_monthly <- rename(dtr_Grand_Gedeh_monthly, Value=dtr)

#Grand_Kru
dtr_Grand_Kru_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             dtr=dtr.var[which(lon==8.25),which(lat==4.75),1:48]))
dtr_Grand_Kru_monthly$Location <- 'Grand_Kru'
dtr_Grand_Kru_monthly$date <- ymd(paste(dtr_Grand_Kru_monthly$Year, dtr_Grand_Kru_monthly$Month, dtr_Grand_Kru_monthly$day, sep="-"))
dtr_Grand_Kru_monthly_2015 <- select(dtr_Grand_Kru_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Grand_Kru_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Grand_Kru_monthly_2015a$Location <- 'Grand_Kru'
dtr_Grand_Kru_monthly_2015a$date <- ymd(paste(dtr_Grand_Kru_monthly_2015a$Year, dtr_Grand_Kru_monthly_2015a$Month, dtr_Grand_Kru_monthly_2015a$day, sep="-"))
dtr_Grand_Kru_monthly_2015a <- select(dtr_Grand_Kru_monthly_2015a, date, Year, Month, day, Location)
dtr_Grand_Kru_monthly_2015 <- full_join(dtr_Grand_Kru_monthly_2015a, dtr_Grand_Kru_monthly_2015)
dtr_Grand_Kru_monthly <- rbind(select(dtr_Grand_Kru_monthly,date, Year, Month, day, Location, dtr), dtr_Grand_Kru_monthly_2015)
rm(dtr_Grand_Kru_monthly_2015, dtr_Grand_Kru_monthly_2015a)
dtr_Grand_Kru_monthly$measurement <- "dtr"
dtr_Grand_Kru_monthly <- rename(dtr_Grand_Kru_monthly, Value=dtr)

#Lofa
dtr_Lofa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        dtr1=dtr.var[which(lon==10.25),which(lat==8.25),1:48],
                                        dtr2=dtr.var[which(lon==9.75),which(lat==8.25),1:48],
                                        dtr3=dtr.var[which(lon==9.75),which(lat==7.75),1:48]))
dtr_Lofa_monthly$dtr <- rowMeans(select(dtr_Lofa_monthly, dtr1, dtr2, dtr3))
dtr_Lofa_monthly$Location <- 'Lofa'
dtr_Lofa_monthly$date <- ymd(paste(dtr_Lofa_monthly$Year, dtr_Lofa_monthly$Month, dtr_Lofa_monthly$day, sep="-"))
dtr_Lofa_monthly_2015 <- select(dtr_Lofa_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Lofa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Lofa_monthly_2015a$Location <- 'Lofa'
dtr_Lofa_monthly_2015a$date <- ymd(paste(dtr_Lofa_monthly_2015a$Year, dtr_Lofa_monthly_2015a$Month, dtr_Lofa_monthly_2015a$day, sep="-"))
dtr_Lofa_monthly_2015a <- select(dtr_Lofa_monthly_2015a, date, Year, Month, day, Location)
dtr_Lofa_monthly_2015 <- full_join(dtr_Lofa_monthly_2015a, dtr_Lofa_monthly_2015)
dtr_Lofa_monthly <- rbind(select(dtr_Lofa_monthly,date, Year, Month, day, Location, dtr), dtr_Lofa_monthly_2015)
rm(dtr_Lofa_monthly_2015, dtr_Lofa_monthly_2015a)
dtr_Lofa_monthly$measurement <- "dtr"
dtr_Lofa_monthly <- rename(dtr_Lofa_monthly, Value=dtr)

#Margibi
dtr_Margibi_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           dtr1=dtr.var[which(lon==10.25),which(lat==6.75),1:48],
                                           dtr2=dtr.var[which(lon==10.25),which(lat==6.25),1:48]))
dtr_Margibi_monthly$dtr <- rowMeans(select(dtr_Margibi_monthly, dtr1, dtr2))
dtr_Margibi_monthly$Location <- 'Margibi'
dtr_Margibi_monthly$date <- ymd(paste(dtr_Margibi_monthly$Year, dtr_Margibi_monthly$Month, dtr_Margibi_monthly$day, sep="-"))
dtr_Margibi_monthly_2015 <- select(dtr_Margibi_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Margibi_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Margibi_monthly_2015a$Location <- 'Margibi'
dtr_Margibi_monthly_2015a$date <- ymd(paste(dtr_Margibi_monthly_2015a$Year, dtr_Margibi_monthly_2015a$Month, dtr_Margibi_monthly_2015a$day, sep="-"))
dtr_Margibi_monthly_2015a <- select(dtr_Margibi_monthly_2015a, date, Year, Month, day, Location)
dtr_Margibi_monthly_2015 <- full_join(dtr_Margibi_monthly_2015a, dtr_Margibi_monthly_2015)
dtr_Margibi_monthly <- rbind(select(dtr_Margibi_monthly,date, Year, Month, day, Location, dtr), dtr_Margibi_monthly_2015)
rm(dtr_Margibi_monthly_2015, dtr_Margibi_monthly_2015a)
dtr_Margibi_monthly$measurement <- "dtr"
dtr_Margibi_monthly <- rename(dtr_Margibi_monthly, Value=dtr)

#Maryland
dtr_Maryland_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            dtr=dtr.var[which(lon==7.75),which(lat==4.75),1:48]))
dtr_Maryland_monthly$Location <- 'Maryland'
dtr_Maryland_monthly$date <- ymd(paste(dtr_Maryland_monthly$Year, dtr_Maryland_monthly$Month, dtr_Maryland_monthly$day, sep="-"))
dtr_Maryland_monthly_2015 <- select(dtr_Maryland_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Maryland_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Maryland_monthly_2015a$Location <- 'Maryland'
dtr_Maryland_monthly_2015a$date <- ymd(paste(dtr_Maryland_monthly_2015a$Year, dtr_Maryland_monthly_2015a$Month, dtr_Maryland_monthly_2015a$day, sep="-"))
dtr_Maryland_monthly_2015a <- select(dtr_Maryland_monthly_2015a, date, Year, Month, day, Location)
dtr_Maryland_monthly_2015 <- full_join(dtr_Maryland_monthly_2015a, dtr_Maryland_monthly_2015)
dtr_Maryland_monthly <- rbind(select(dtr_Maryland_monthly,date, Year, Month, day, Location, dtr), dtr_Maryland_monthly_2015)
rm(dtr_Maryland_monthly_2015, dtr_Maryland_monthly_2015a)
dtr_Maryland_monthly$measurement <- "dtr"
dtr_Maryland_monthly <- rename(dtr_Maryland_monthly, Value=dtr)

#Montserrado
dtr_Montserrado_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               dtr=dtr.var[which(lon==10.75),which(lat==6.25),1:48]))
dtr_Montserrado_monthly$Location <- 'Montserrado'
dtr_Montserrado_monthly$date <- ymd(paste(dtr_Montserrado_monthly$Year, dtr_Montserrado_monthly$Month, dtr_Montserrado_monthly$day, sep="-"))
dtr_Montserrado_monthly_2015 <- select(dtr_Montserrado_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Montserrado_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Montserrado_monthly_2015a$Location <- 'Montserrado'
dtr_Montserrado_monthly_2015a$date <- ymd(paste(dtr_Montserrado_monthly_2015a$Year, dtr_Montserrado_monthly_2015a$Month, dtr_Montserrado_monthly_2015a$day, sep="-"))
dtr_Montserrado_monthly_2015a <- select(dtr_Montserrado_monthly_2015a, date, Year, Month, day, Location)
dtr_Montserrado_monthly_2015 <- full_join(dtr_Montserrado_monthly_2015a, dtr_Montserrado_monthly_2015)
dtr_Montserrado_monthly <- rbind(select(dtr_Montserrado_monthly,date, Year, Month, day, Location, dtr), dtr_Montserrado_monthly_2015)
rm(dtr_Montserrado_monthly_2015, dtr_Montserrado_monthly_2015a)
dtr_Montserrado_monthly$measurement <- "dtr"
dtr_Montserrado_monthly <- rename(dtr_Montserrado_monthly, Value=dtr)

#Nimba
dtr_Nimba_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         dtr1=dtr.var[which(lon==8.75),which(lat==7.25),1:48],
                                         dtr2=dtr.var[which(lon==8.75),which(lat==6.75),1:48],
                                         dtr3=dtr.var[which(lon==8.75),which(lat==6.25),1:48]))
dtr_Nimba_monthly$dtr <- rowMeans(select(dtr_Nimba_monthly, dtr1, dtr2, dtr3))
dtr_Nimba_monthly$Location <- 'Nimba'
dtr_Nimba_monthly$date <- ymd(paste(dtr_Nimba_monthly$Year, dtr_Nimba_monthly$Month, dtr_Nimba_monthly$day, sep="-"))
dtr_Nimba_monthly_2015 <- select(dtr_Nimba_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Nimba_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Nimba_monthly_2015a$Location <- 'Nimba'
dtr_Nimba_monthly_2015a$date <- ymd(paste(dtr_Nimba_monthly_2015a$Year, dtr_Nimba_monthly_2015a$Month, dtr_Nimba_monthly_2015a$day, sep="-"))
dtr_Nimba_monthly_2015a <- select(dtr_Nimba_monthly_2015a, date, Year, Month, day, Location)
dtr_Nimba_monthly_2015 <- full_join(dtr_Nimba_monthly_2015a, dtr_Nimba_monthly_2015)
dtr_Nimba_monthly <- rbind(select(dtr_Nimba_monthly,date, Year, Month, day, Location, dtr), dtr_Nimba_monthly_2015)
rm(dtr_Nimba_monthly_2015, dtr_Nimba_monthly_2015a)
dtr_Nimba_monthly$measurement <- "dtr"
dtr_Nimba_monthly <- rename(dtr_Nimba_monthly, Value=dtr)

#River_Gee
dtr_River_Gee_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             dtr1=dtr.var[which(lon==8.25),which(lat==5.25),1:48],
                                             dtr2=dtr.var[which(lon==7.75),which(lat==5.25),1:48]))
dtr_River_Gee_monthly$dtr <- rowMeans(select(dtr_River_Gee_monthly, dtr1, dtr2))
dtr_River_Gee_monthly$Location <- 'River_Gee'
dtr_River_Gee_monthly$date <- ymd(paste(dtr_River_Gee_monthly$Year, dtr_River_Gee_monthly$Month, dtr_River_Gee_monthly$day, sep="-"))
dtr_River_Gee_monthly_2015 <- select(dtr_River_Gee_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_River_Gee_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_River_Gee_monthly_2015a$Location <- 'River_Gee'
dtr_River_Gee_monthly_2015a$date <- ymd(paste(dtr_River_Gee_monthly_2015a$Year, dtr_River_Gee_monthly_2015a$Month, dtr_River_Gee_monthly_2015a$day, sep="-"))
dtr_River_Gee_monthly_2015a <- select(dtr_River_Gee_monthly_2015a, date, Year, Month, day, Location)
dtr_River_Gee_monthly_2015 <- full_join(dtr_River_Gee_monthly_2015a, dtr_River_Gee_monthly_2015)
dtr_River_Gee_monthly <- rbind(select(dtr_River_Gee_monthly,date, Year, Month, day, Location, dtr), dtr_River_Gee_monthly_2015)
rm(dtr_River_Gee_monthly_2015, dtr_River_Gee_monthly_2015a)
dtr_River_Gee_monthly$measurement <- "dtr"
dtr_River_Gee_monthly <- rename(dtr_River_Gee_monthly, Value=dtr)

#Rivercess
dtr_River_Cess_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                              Month=rep(seq(1,12,1), 4), day=1,
                                              dtr1=dtr.var[which(lon==9.25),which(lat==6.25),1:48],
                                              dtr2=dtr.var[which(lon==9.25),which(lat==5.75),1:48],
                                              dtr3=dtr.var[which(lon==9.75),which(lat==5.25),1:48]))
dtr_River_Cess_monthly$dtr <- rowMeans(select(dtr_River_Cess_monthly, dtr1, dtr2, dtr3))
dtr_River_Cess_monthly$Location <- 'River_Cess'
dtr_River_Cess_monthly$date <- ymd(paste(dtr_River_Cess_monthly$Year, dtr_River_Cess_monthly$Month, dtr_River_Cess_monthly$day, sep="-"))
dtr_River_Cess_monthly_2015 <- select(dtr_River_Cess_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_River_Cess_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_River_Cess_monthly_2015a$Location <- 'River_Cess'
dtr_River_Cess_monthly_2015a$date <- ymd(paste(dtr_River_Cess_monthly_2015a$Year, dtr_River_Cess_monthly_2015a$Month, dtr_River_Cess_monthly_2015a$day, sep="-"))
dtr_River_Cess_monthly_2015a <- select(dtr_River_Cess_monthly_2015a, date, Year, Month, day, Location)
dtr_River_Cess_monthly_2015 <- full_join(dtr_River_Cess_monthly_2015a, dtr_River_Cess_monthly_2015)
dtr_River_Cess_monthly <- rbind(select(dtr_River_Cess_monthly,date, Year, Month, day, Location, dtr), dtr_River_Cess_monthly_2015)
rm(dtr_River_Cess_monthly_2015, dtr_River_Cess_monthly_2015a)
dtr_River_Cess_monthly$measurement <- "dtr"
dtr_River_Cess_monthly <- rename(dtr_River_Cess_monthly, Value=dtr)

#Sinoe
dtr_Sinoe_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         dtr1=dtr.var[which(lon==8.75),which(lat==5.75),1:48],
                                         dtr2=dtr.var[which(lon==9.25),which(lat==5.25),1:48],
                                         dtr3=dtr.var[which(lon==8.75),which(lat==5.25),1:48],
                                         dtr4=dtr.var[which(lon==8.75),which(lat==4.75),1:48]))
dtr_Sinoe_monthly$dtr <- rowMeans(select(dtr_Sinoe_monthly, dtr1, dtr2, dtr3, dtr4))
dtr_Sinoe_monthly$Location <- 'Sinoe'
dtr_Sinoe_monthly$date <- ymd(paste(dtr_Sinoe_monthly$Year, dtr_Sinoe_monthly$Month, dtr_Sinoe_monthly$day, sep="-"))
dtr_Sinoe_monthly_2015 <- select(dtr_Sinoe_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Sinoe_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Sinoe_monthly_2015a$Location <- 'Sinoe'
dtr_Sinoe_monthly_2015a$date <- ymd(paste(dtr_Sinoe_monthly_2015a$Year, dtr_Sinoe_monthly_2015a$Month, dtr_Sinoe_monthly_2015a$day, sep="-"))
dtr_Sinoe_monthly_2015a <- select(dtr_Sinoe_monthly_2015a, date, Year, Month, day, Location)
dtr_Sinoe_monthly_2015 <- full_join(dtr_Sinoe_monthly_2015a, dtr_Sinoe_monthly_2015)
dtr_Sinoe_monthly <- rbind(select(dtr_Sinoe_monthly,date, Year, Month, day, Location, dtr), dtr_Sinoe_monthly_2015)
rm(dtr_Sinoe_monthly_2015, dtr_Sinoe_monthly_2015a)
dtr_Sinoe_monthly$measurement <- "dtr"
dtr_Sinoe_monthly <- rename(dtr_Sinoe_monthly, Value=dtr)

#Merging in long format
dtrLiberiamonthly_district <- rbind(dtr_Bomi_monthly, dtr_Bong_monthly, dtr_Gbarpolu_monthly,
                                 dtr_Grand_Bassa_monthly, dtr_Grand_Cape_Mount_monthly, dtr_Grand_Gedeh_monthly,
                                 dtr_Grand_Kru_monthly, dtr_Lofa_monthly, dtr_Margibi_monthly,
                                 dtr_Maryland_monthly, dtr_Montserrado_monthly, dtr_Nimba_monthly,
                                 dtr_River_Gee_monthly, dtr_River_Cess_monthly, dtr_Sinoe_monthly)

#####################
#pet - potential evapotraonspiration
#####################
pet.full <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.pet.dat.nc', write=FALSE)
pet.var <- var.get.nc(pet.full, "pet")

#Bomi
pet_Bomi_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        pet=pet.var[which(lon==10.75),which(lat==6.75),1:48]))
pet_Bomi_monthly$Location <- 'Bomi'
pet_Bomi_monthly$date <- ymd(paste(pet_Bomi_monthly$Year, pet_Bomi_monthly$Month, pet_Bomi_monthly$day, sep="-"))
pet_Bomi_monthly_2015 <- select(pet_Bomi_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Bomi_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Bomi_monthly_2015a$Location <- 'Bomi'
pet_Bomi_monthly_2015a$date <- ymd(paste(pet_Bomi_monthly_2015a$Year, pet_Bomi_monthly_2015a$Month, pet_Bomi_monthly_2015a$day, sep="-"))
pet_Bomi_monthly_2015a <- select(pet_Bomi_monthly_2015a, date, Year, Month, day, Location)
pet_Bomi_monthly_2015 <- full_join(pet_Bomi_monthly_2015a, pet_Bomi_monthly_2015)
pet_Bomi_monthly <- rbind(select(pet_Bomi_monthly,date, Year, Month, day, Location, pet), pet_Bomi_monthly_2015)
rm(pet_Bomi_monthly_2015, pet_Bomi_monthly_2015a)
pet_Bomi_monthly$measurement <- "pet"
pet_Bomi_monthly <- rename(pet_Bomi_monthly, Value=pet)

#Bong
pet_Bong_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        pet1=pet.var[which(lon==9.75),which(lat==7.25),1:48],
                                        pet2=pet.var[which(lon==9.25),which(lat==7.25),1:48],
                                        pet3=pet.var[which(lon==10.25),which(lat==6.75),1:48],
                                        pet4=pet.var[which(lon==9.75),which(lat==6.75),1:48],
                                        pet5=pet.var[which(lon==9.25),which(lat==6.75),1:48]))
pet_Bong_monthly$pet <- rowMeans(select(pet_Bong_monthly, pet1, pet2, pet3, pet4, pet5))
pet_Bong_monthly$Location <- 'Bong'
pet_Bong_monthly$date <- ymd(paste(pet_Bong_monthly$Year, pet_Bong_monthly$Month, pet_Bong_monthly$day, sep="-"))
pet_Bong_monthly_2015 <- select(pet_Bong_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Bong_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Bong_monthly_2015a$Location <- 'Bong'
pet_Bong_monthly_2015a$date <- ymd(paste(pet_Bong_monthly_2015a$Year, pet_Bong_monthly_2015a$Month, pet_Bong_monthly_2015a$day, sep="-"))
pet_Bong_monthly_2015a <- select(pet_Bong_monthly_2015a, date, Year, Month, day, Location)
pet_Bong_monthly_2015 <- full_join(pet_Bong_monthly_2015a, pet_Bong_monthly_2015)
pet_Bong_monthly <- rbind(select(pet_Bong_monthly,date, Year, Month, day, Location, pet), pet_Bong_monthly_2015)
rm(pet_Bong_monthly_2015, pet_Bong_monthly_2015a)
pet_Bong_monthly$measurement <- "pet"
pet_Bong_monthly <- rename(pet_Bong_monthly, Value=pet)

#Gbarpolu
pet_Gbarpolu_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            pet1=pet.var[which(lon==10.25),which(lat==7.75),1:48],
                                            pet2=pet.var[which(lon==10.25),which(lat==7.25),1:48],
                                            pet3=pet.var[which(lon==9.75),which(lat==7.25),1:48]))
pet_Gbarpolu_monthly$pet <- rowMeans(select(pet_Gbarpolu_monthly, pet1, pet2, pet3))
pet_Gbarpolu_monthly$Location <- 'Gbarpolu'
pet_Gbarpolu_monthly$date <- ymd(paste(pet_Gbarpolu_monthly$Year, pet_Gbarpolu_monthly$Month, pet_Gbarpolu_monthly$day, sep="-"))
pet_Gbarpolu_monthly_2015 <- select(pet_Gbarpolu_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Gbarpolu_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Gbarpolu_monthly_2015a$Location <- 'Gbarpolu'
pet_Gbarpolu_monthly_2015a$date <- ymd(paste(pet_Gbarpolu_monthly_2015a$Year, pet_Gbarpolu_monthly_2015a$Month, pet_Gbarpolu_monthly_2015a$day, sep="-"))
pet_Gbarpolu_monthly_2015a <- select(pet_Gbarpolu_monthly_2015a, date, Year, Month, day, Location)
pet_Gbarpolu_monthly_2015 <- full_join(pet_Gbarpolu_monthly_2015a, pet_Gbarpolu_monthly_2015)
pet_Gbarpolu_monthly <- rbind(select(pet_Gbarpolu_monthly,date, Year, Month, day, Location, pet), pet_Gbarpolu_monthly_2015)
rm(pet_Gbarpolu_monthly_2015, pet_Gbarpolu_monthly_2015a)
pet_Gbarpolu_monthly$measurement <- "pet"
pet_Gbarpolu_monthly <- rename(pet_Gbarpolu_monthly, Value=pet)

#Grand_Bassa
pet_Grand_Bassa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               pet1=pet.var[which(lon==10.25),which(lat==6.25),1:48],
                                               pet2=pet.var[which(lon==9.75),which(lat==6.25),1:48]))
pet_Grand_Bassa_monthly$pet <- rowMeans(select(pet_Grand_Bassa_monthly, pet1, pet2))
pet_Grand_Bassa_monthly$Location <- 'Grand_Bassa'
pet_Grand_Bassa_monthly$date <- ymd(paste(pet_Grand_Bassa_monthly$Year, pet_Grand_Bassa_monthly$Month, pet_Grand_Bassa_monthly$day, sep="-"))
pet_Grand_Bassa_monthly_2015 <- select(pet_Grand_Bassa_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Grand_Bassa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Grand_Bassa_monthly_2015a$Location <- 'Grand_Bassa'
pet_Grand_Bassa_monthly_2015a$date <- ymd(paste(pet_Grand_Bassa_monthly_2015a$Year, pet_Grand_Bassa_monthly_2015a$Month, pet_Grand_Bassa_monthly_2015a$day, sep="-"))
pet_Grand_Bassa_monthly_2015a <- select(pet_Grand_Bassa_monthly_2015a, date, Year, Month, day, Location)
pet_Grand_Bassa_monthly_2015 <- full_join(pet_Grand_Bassa_monthly_2015a, pet_Grand_Bassa_monthly_2015)
pet_Grand_Bassa_monthly <- rbind(select(pet_Grand_Bassa_monthly,date, Year, Month, day, Location, pet), pet_Grand_Bassa_monthly_2015)
rm(pet_Grand_Bassa_monthly_2015, pet_Grand_Bassa_monthly_2015a)
pet_Grand_Bassa_monthly$measurement <- "pet"
pet_Grand_Bassa_monthly <- rename(pet_Grand_Bassa_monthly, Value=pet)

#Grand_Cape_Mount
pet_Grand_Cape_Mount_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                                    Month=rep(seq(1,12,1), 4), day=1,
                                                    pet1=pet.var[which(lon==11.25),which(lat==7.25),1:48],
                                                    pet2=pet.var[which(lon==10.75),which(lat==7.25),1:48],
                                                    pet3=pet.var[which(lon==11.25),which(lat==6.75),1:48]))
pet_Grand_Cape_Mount_monthly$pet <- rowMeans(select(pet_Grand_Cape_Mount_monthly, pet1, pet2, pet3))
pet_Grand_Cape_Mount_monthly$Location <- 'Grand_Cape_Mount'
pet_Grand_Cape_Mount_monthly$date <- ymd(paste(pet_Grand_Cape_Mount_monthly$Year, pet_Grand_Cape_Mount_monthly$Month, pet_Grand_Cape_Mount_monthly$day, sep="-"))
pet_Grand_Cape_Mount_monthly_2015 <- select(pet_Grand_Cape_Mount_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Grand_Cape_Mount_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Grand_Cape_Mount_monthly_2015a$Location <- 'Grand_Cape_Mount'
pet_Grand_Cape_Mount_monthly_2015a$date <- ymd(paste(pet_Grand_Cape_Mount_monthly_2015a$Year, pet_Grand_Cape_Mount_monthly_2015a$Month, pet_Grand_Cape_Mount_monthly_2015a$day, sep="-"))
pet_Grand_Cape_Mount_monthly_2015a <- select(pet_Grand_Cape_Mount_monthly_2015a, date, Year, Month, day, Location)
pet_Grand_Cape_Mount_monthly_2015 <- full_join(pet_Grand_Cape_Mount_monthly_2015a, pet_Grand_Cape_Mount_monthly_2015)
pet_Grand_Cape_Mount_monthly <- rbind(select(pet_Grand_Cape_Mount_monthly,date, Year, Month, day, Location, pet), pet_Grand_Cape_Mount_monthly_2015)
rm(pet_Grand_Cape_Mount_monthly_2015, pet_Grand_Cape_Mount_monthly_2015a)
pet_Grand_Cape_Mount_monthly$measurement <- "pet"
pet_Grand_Cape_Mount_monthly <- rename(pet_Grand_Cape_Mount_monthly, Value=pet)

#Grand_Gedeh
pet_Grand_Gedeh_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               pet1=pet.var[which(lon==8.75),which(lat==6.25),1:48],
                                               pet2=pet.var[which(lon==8.25),which(lat==6.25),1:48],
                                               pet3=pet.var[which(lon==8.75),which(lat==5.75),1:48],
                                               pet4=pet.var[which(lon==8.25),which(lat==5.75),1:48],
                                               pet5=pet.var[which(lon==7.75),which(lat==5.75),1:48]))
pet_Grand_Gedeh_monthly$pet <- rowMeans(select(pet_Grand_Gedeh_monthly, pet1, pet2, pet3, pet4, pet5))
pet_Grand_Gedeh_monthly$Location <- 'Grand_Gedeh'
pet_Grand_Gedeh_monthly$date <- ymd(paste(pet_Grand_Gedeh_monthly$Year, pet_Grand_Gedeh_monthly$Month, pet_Grand_Gedeh_monthly$day, sep="-"))
pet_Grand_Gedeh_monthly_2015 <- select(pet_Grand_Gedeh_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Grand_Gedeh_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Grand_Gedeh_monthly_2015a$Location <- 'Grand_Gedeh'
pet_Grand_Gedeh_monthly_2015a$date <- ymd(paste(pet_Grand_Gedeh_monthly_2015a$Year, pet_Grand_Gedeh_monthly_2015a$Month, pet_Grand_Gedeh_monthly_2015a$day, sep="-"))
pet_Grand_Gedeh_monthly_2015a <- select(pet_Grand_Gedeh_monthly_2015a, date, Year, Month, day, Location)
pet_Grand_Gedeh_monthly_2015 <- full_join(pet_Grand_Gedeh_monthly_2015a, pet_Grand_Gedeh_monthly_2015)
pet_Grand_Gedeh_monthly <- rbind(select(pet_Grand_Gedeh_monthly,date, Year, Month, day, Location, pet), pet_Grand_Gedeh_monthly_2015)
rm(pet_Grand_Gedeh_monthly_2015, pet_Grand_Gedeh_monthly_2015a)
pet_Grand_Gedeh_monthly$measurement <- "pet"
pet_Grand_Gedeh_monthly <- rename(pet_Grand_Gedeh_monthly, Value=pet)

#Grand_Kru
pet_Grand_Kru_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             pet=pet.var[which(lon==8.25),which(lat==4.75),1:48]))
pet_Grand_Kru_monthly$Location <- 'Grand_Kru'
pet_Grand_Kru_monthly$date <- ymd(paste(pet_Grand_Kru_monthly$Year, pet_Grand_Kru_monthly$Month, pet_Grand_Kru_monthly$day, sep="-"))
pet_Grand_Kru_monthly_2015 <- select(pet_Grand_Kru_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Grand_Kru_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Grand_Kru_monthly_2015a$Location <- 'Grand_Kru'
pet_Grand_Kru_monthly_2015a$date <- ymd(paste(pet_Grand_Kru_monthly_2015a$Year, pet_Grand_Kru_monthly_2015a$Month, pet_Grand_Kru_monthly_2015a$day, sep="-"))
pet_Grand_Kru_monthly_2015a <- select(pet_Grand_Kru_monthly_2015a, date, Year, Month, day, Location)
pet_Grand_Kru_monthly_2015 <- full_join(pet_Grand_Kru_monthly_2015a, pet_Grand_Kru_monthly_2015)
pet_Grand_Kru_monthly <- rbind(select(pet_Grand_Kru_monthly,date, Year, Month, day, Location, pet), pet_Grand_Kru_monthly_2015)
rm(pet_Grand_Kru_monthly_2015, pet_Grand_Kru_monthly_2015a)
pet_Grand_Kru_monthly$measurement <- "pet"
pet_Grand_Kru_monthly <- rename(pet_Grand_Kru_monthly, Value=pet)

#Lofa
pet_Lofa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        pet1=pet.var[which(lon==10.25),which(lat==8.25),1:48],
                                        pet2=pet.var[which(lon==9.75),which(lat==8.25),1:48],
                                        pet3=pet.var[which(lon==9.75),which(lat==7.75),1:48]))
pet_Lofa_monthly$pet <- rowMeans(select(pet_Lofa_monthly, pet1, pet2, pet3))
pet_Lofa_monthly$Location <- 'Lofa'
pet_Lofa_monthly$date <- ymd(paste(pet_Lofa_monthly$Year, pet_Lofa_monthly$Month, pet_Lofa_monthly$day, sep="-"))
pet_Lofa_monthly_2015 <- select(pet_Lofa_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Lofa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Lofa_monthly_2015a$Location <- 'Lofa'
pet_Lofa_monthly_2015a$date <- ymd(paste(pet_Lofa_monthly_2015a$Year, pet_Lofa_monthly_2015a$Month, pet_Lofa_monthly_2015a$day, sep="-"))
pet_Lofa_monthly_2015a <- select(pet_Lofa_monthly_2015a, date, Year, Month, day, Location)
pet_Lofa_monthly_2015 <- full_join(pet_Lofa_monthly_2015a, pet_Lofa_monthly_2015)
pet_Lofa_monthly <- rbind(select(pet_Lofa_monthly,date, Year, Month, day, Location, pet), pet_Lofa_monthly_2015)
rm(pet_Lofa_monthly_2015, pet_Lofa_monthly_2015a)
pet_Lofa_monthly$measurement <- "pet"
pet_Lofa_monthly <- rename(pet_Lofa_monthly, Value=pet)

#Margibi
pet_Margibi_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           pet1=pet.var[which(lon==10.25),which(lat==6.75),1:48],
                                           pet2=pet.var[which(lon==10.25),which(lat==6.25),1:48]))
pet_Margibi_monthly$pet <- rowMeans(select(pet_Margibi_monthly, pet1, pet2))
pet_Margibi_monthly$Location <- 'Margibi'
pet_Margibi_monthly$date <- ymd(paste(pet_Margibi_monthly$Year, pet_Margibi_monthly$Month, pet_Margibi_monthly$day, sep="-"))
pet_Margibi_monthly_2015 <- select(pet_Margibi_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Margibi_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Margibi_monthly_2015a$Location <- 'Margibi'
pet_Margibi_monthly_2015a$date <- ymd(paste(pet_Margibi_monthly_2015a$Year, pet_Margibi_monthly_2015a$Month, pet_Margibi_monthly_2015a$day, sep="-"))
pet_Margibi_monthly_2015a <- select(pet_Margibi_monthly_2015a, date, Year, Month, day, Location)
pet_Margibi_monthly_2015 <- full_join(pet_Margibi_monthly_2015a, pet_Margibi_monthly_2015)
pet_Margibi_monthly <- rbind(select(pet_Margibi_monthly,date, Year, Month, day, Location, pet), pet_Margibi_monthly_2015)
rm(pet_Margibi_monthly_2015, pet_Margibi_monthly_2015a)
pet_Margibi_monthly$measurement <- "pet"
pet_Margibi_monthly <- rename(pet_Margibi_monthly, Value=pet)

#Maryland
pet_Maryland_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            pet=pet.var[which(lon==7.75),which(lat==4.75),1:48]))
pet_Maryland_monthly$Location <- 'Maryland'
pet_Maryland_monthly$date <- ymd(paste(pet_Maryland_monthly$Year, pet_Maryland_monthly$Month, pet_Maryland_monthly$day, sep="-"))
pet_Maryland_monthly_2015 <- select(pet_Maryland_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Maryland_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Maryland_monthly_2015a$Location <- 'Maryland'
pet_Maryland_monthly_2015a$date <- ymd(paste(pet_Maryland_monthly_2015a$Year, pet_Maryland_monthly_2015a$Month, pet_Maryland_monthly_2015a$day, sep="-"))
pet_Maryland_monthly_2015a <- select(pet_Maryland_monthly_2015a, date, Year, Month, day, Location)
pet_Maryland_monthly_2015 <- full_join(pet_Maryland_monthly_2015a, pet_Maryland_monthly_2015)
pet_Maryland_monthly <- rbind(select(pet_Maryland_monthly,date, Year, Month, day, Location, pet), pet_Maryland_monthly_2015)
rm(pet_Maryland_monthly_2015, pet_Maryland_monthly_2015a)
pet_Maryland_monthly$measurement <- "pet"
pet_Maryland_monthly <- rename(pet_Maryland_monthly, Value=pet)

#Montserrado
pet_Montserrado_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               pet=pet.var[which(lon==10.75),which(lat==6.25),1:48]))
pet_Montserrado_monthly$Location <- 'Montserrado'
pet_Montserrado_monthly$date <- ymd(paste(pet_Montserrado_monthly$Year, pet_Montserrado_monthly$Month, pet_Montserrado_monthly$day, sep="-"))
pet_Montserrado_monthly_2015 <- select(pet_Montserrado_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Montserrado_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Montserrado_monthly_2015a$Location <- 'Montserrado'
pet_Montserrado_monthly_2015a$date <- ymd(paste(pet_Montserrado_monthly_2015a$Year, pet_Montserrado_monthly_2015a$Month, pet_Montserrado_monthly_2015a$day, sep="-"))
pet_Montserrado_monthly_2015a <- select(pet_Montserrado_monthly_2015a, date, Year, Month, day, Location)
pet_Montserrado_monthly_2015 <- full_join(pet_Montserrado_monthly_2015a, pet_Montserrado_monthly_2015)
pet_Montserrado_monthly <- rbind(select(pet_Montserrado_monthly,date, Year, Month, day, Location, pet), pet_Montserrado_monthly_2015)
rm(pet_Montserrado_monthly_2015, pet_Montserrado_monthly_2015a)
pet_Montserrado_monthly$measurement <- "pet"
pet_Montserrado_monthly <- rename(pet_Montserrado_monthly, Value=pet)

#Nimba
pet_Nimba_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         pet1=pet.var[which(lon==8.75),which(lat==7.25),1:48],
                                         pet2=pet.var[which(lon==8.75),which(lat==6.75),1:48],
                                         pet3=pet.var[which(lon==8.75),which(lat==6.25),1:48]))
pet_Nimba_monthly$pet <- rowMeans(select(pet_Nimba_monthly, pet1, pet2, pet3))
pet_Nimba_monthly$Location <- 'Nimba'
pet_Nimba_monthly$date <- ymd(paste(pet_Nimba_monthly$Year, pet_Nimba_monthly$Month, pet_Nimba_monthly$day, sep="-"))
pet_Nimba_monthly_2015 <- select(pet_Nimba_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Nimba_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Nimba_monthly_2015a$Location <- 'Nimba'
pet_Nimba_monthly_2015a$date <- ymd(paste(pet_Nimba_monthly_2015a$Year, pet_Nimba_monthly_2015a$Month, pet_Nimba_monthly_2015a$day, sep="-"))
pet_Nimba_monthly_2015a <- select(pet_Nimba_monthly_2015a, date, Year, Month, day, Location)
pet_Nimba_monthly_2015 <- full_join(pet_Nimba_monthly_2015a, pet_Nimba_monthly_2015)
pet_Nimba_monthly <- rbind(select(pet_Nimba_monthly,date, Year, Month, day, Location, pet), pet_Nimba_monthly_2015)
rm(pet_Nimba_monthly_2015, pet_Nimba_monthly_2015a)
pet_Nimba_monthly$measurement <- "pet"
pet_Nimba_monthly <- rename(pet_Nimba_monthly, Value=pet)

#River_Gee
pet_River_Gee_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             pet1=pet.var[which(lon==8.25),which(lat==5.25),1:48],
                                             pet2=pet.var[which(lon==7.75),which(lat==5.25),1:48]))
pet_River_Gee_monthly$pet <- rowMeans(select(pet_River_Gee_monthly, pet1, pet2))
pet_River_Gee_monthly$Location <- 'River_Gee'
pet_River_Gee_monthly$date <- ymd(paste(pet_River_Gee_monthly$Year, pet_River_Gee_monthly$Month, pet_River_Gee_monthly$day, sep="-"))
pet_River_Gee_monthly_2015 <- select(pet_River_Gee_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_River_Gee_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_River_Gee_monthly_2015a$Location <- 'River_Gee'
pet_River_Gee_monthly_2015a$date <- ymd(paste(pet_River_Gee_monthly_2015a$Year, pet_River_Gee_monthly_2015a$Month, pet_River_Gee_monthly_2015a$day, sep="-"))
pet_River_Gee_monthly_2015a <- select(pet_River_Gee_monthly_2015a, date, Year, Month, day, Location)
pet_River_Gee_monthly_2015 <- full_join(pet_River_Gee_monthly_2015a, pet_River_Gee_monthly_2015)
pet_River_Gee_monthly <- rbind(select(pet_River_Gee_monthly,date, Year, Month, day, Location, pet), pet_River_Gee_monthly_2015)
rm(pet_River_Gee_monthly_2015, pet_River_Gee_monthly_2015a)
pet_River_Gee_monthly$measurement <- "pet"
pet_River_Gee_monthly <- rename(pet_River_Gee_monthly, Value=pet)

#Rivercess
pet_River_Cess_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                              Month=rep(seq(1,12,1), 4), day=1,
                                              pet1=pet.var[which(lon==9.25),which(lat==6.25),1:48],
                                              pet2=pet.var[which(lon==9.25),which(lat==5.75),1:48],
                                              pet3=pet.var[which(lon==9.75),which(lat==5.25),1:48]))
pet_River_Cess_monthly$pet <- rowMeans(select(pet_River_Cess_monthly, pet1, pet2, pet3))
pet_River_Cess_monthly$Location <- 'River_Cess'
pet_River_Cess_monthly$date <- ymd(paste(pet_River_Cess_monthly$Year, pet_River_Cess_monthly$Month, pet_River_Cess_monthly$day, sep="-"))
pet_River_Cess_monthly_2015 <- select(pet_River_Cess_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_River_Cess_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_River_Cess_monthly_2015a$Location <- 'River_Cess'
pet_River_Cess_monthly_2015a$date <- ymd(paste(pet_River_Cess_monthly_2015a$Year, pet_River_Cess_monthly_2015a$Month, pet_River_Cess_monthly_2015a$day, sep="-"))
pet_River_Cess_monthly_2015a <- select(pet_River_Cess_monthly_2015a, date, Year, Month, day, Location)
pet_River_Cess_monthly_2015 <- full_join(pet_River_Cess_monthly_2015a, pet_River_Cess_monthly_2015)
pet_River_Cess_monthly <- rbind(select(pet_River_Cess_monthly,date, Year, Month, day, Location, pet), pet_River_Cess_monthly_2015)
rm(pet_River_Cess_monthly_2015, pet_River_Cess_monthly_2015a)
pet_River_Cess_monthly$measurement <- "pet"
pet_River_Cess_monthly <- rename(pet_River_Cess_monthly, Value=pet)

#Sinoe
pet_Sinoe_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         pet1=pet.var[which(lon==8.75),which(lat==5.75),1:48],
                                         pet2=pet.var[which(lon==9.25),which(lat==5.25),1:48],
                                         pet3=pet.var[which(lon==8.75),which(lat==5.25),1:48],
                                         pet4=pet.var[which(lon==8.75),which(lat==4.75),1:48]))
pet_Sinoe_monthly$pet <- rowMeans(select(pet_Sinoe_monthly, pet1, pet2, pet3, pet4))
pet_Sinoe_monthly$Location <- 'Sinoe'
pet_Sinoe_monthly$date <- ymd(paste(pet_Sinoe_monthly$Year, pet_Sinoe_monthly$Month, pet_Sinoe_monthly$day, sep="-"))
pet_Sinoe_monthly_2015 <- select(pet_Sinoe_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Sinoe_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Sinoe_monthly_2015a$Location <- 'Sinoe'
pet_Sinoe_monthly_2015a$date <- ymd(paste(pet_Sinoe_monthly_2015a$Year, pet_Sinoe_monthly_2015a$Month, pet_Sinoe_monthly_2015a$day, sep="-"))
pet_Sinoe_monthly_2015a <- select(pet_Sinoe_monthly_2015a, date, Year, Month, day, Location)
pet_Sinoe_monthly_2015 <- full_join(pet_Sinoe_monthly_2015a, pet_Sinoe_monthly_2015)
pet_Sinoe_monthly <- rbind(select(pet_Sinoe_monthly,date, Year, Month, day, Location, pet), pet_Sinoe_monthly_2015)
rm(pet_Sinoe_monthly_2015, pet_Sinoe_monthly_2015a)
pet_Sinoe_monthly$measurement <- "pet"
pet_Sinoe_monthly <- rename(pet_Sinoe_monthly, Value=pet)

#Merging in long format
petLiberiamonthly_district <- rbind(pet_Bomi_monthly, pet_Bong_monthly, pet_Gbarpolu_monthly,
                                 pet_Grand_Bassa_monthly, pet_Grand_Cape_Mount_monthly, pet_Grand_Gedeh_monthly,
                                 pet_Grand_Kru_monthly, pet_Lofa_monthly, pet_Margibi_monthly,
                                 pet_Maryland_monthly, pet_Montserrado_monthly, pet_Nimba_monthly,
                                 pet_River_Gee_monthly, pet_River_Cess_monthly, pet_Sinoe_monthly)

#####################
#pre - preocipitation
#####################
pre.full <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.pre.dat.nc', write=FALSE)
pre.var <- var.get.nc(pre.full, "pre")

#Bomi
pre_Bomi_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        pre=pre.var[which(lon==10.75),which(lat==6.75),1:48]))
pre_Bomi_monthly$Location <- 'Bomi'
pre_Bomi_monthly$date <- ymd(paste(pre_Bomi_monthly$Year, pre_Bomi_monthly$Month, pre_Bomi_monthly$day, sep="-"))
pre_Bomi_monthly_2015 <- select(pre_Bomi_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Bomi_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Bomi_monthly_2015a$Location <- 'Bomi'
pre_Bomi_monthly_2015a$date <- ymd(paste(pre_Bomi_monthly_2015a$Year, pre_Bomi_monthly_2015a$Month, pre_Bomi_monthly_2015a$day, sep="-"))
pre_Bomi_monthly_2015a <- select(pre_Bomi_monthly_2015a, date, Year, Month, day, Location)
pre_Bomi_monthly_2015 <- full_join(pre_Bomi_monthly_2015a, pre_Bomi_monthly_2015)
pre_Bomi_monthly <- rbind(select(pre_Bomi_monthly,date, Year, Month, day, Location, pre), pre_Bomi_monthly_2015)
rm(pre_Bomi_monthly_2015, pre_Bomi_monthly_2015a)
pre_Bomi_monthly$measurement <- "pre"
pre_Bomi_monthly <- rename(pre_Bomi_monthly, Value=pre)

#Bong
pre_Bong_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        pre1=pre.var[which(lon==9.75),which(lat==7.25),1:48],
                                        pre2=pre.var[which(lon==9.25),which(lat==7.25),1:48],
                                        pre3=pre.var[which(lon==10.25),which(lat==6.75),1:48],
                                        pre4=pre.var[which(lon==9.75),which(lat==6.75),1:48],
                                        pre5=pre.var[which(lon==9.25),which(lat==6.75),1:48]))
pre_Bong_monthly$pre <- rowMeans(select(pre_Bong_monthly, pre1, pre2, pre3, pre4, pre5))
pre_Bong_monthly$Location <- 'Bong'
pre_Bong_monthly$date <- ymd(paste(pre_Bong_monthly$Year, pre_Bong_monthly$Month, pre_Bong_monthly$day, sep="-"))
pre_Bong_monthly_2015 <- select(pre_Bong_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Bong_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Bong_monthly_2015a$Location <- 'Bong'
pre_Bong_monthly_2015a$date <- ymd(paste(pre_Bong_monthly_2015a$Year, pre_Bong_monthly_2015a$Month, pre_Bong_monthly_2015a$day, sep="-"))
pre_Bong_monthly_2015a <- select(pre_Bong_monthly_2015a, date, Year, Month, day, Location)
pre_Bong_monthly_2015 <- full_join(pre_Bong_monthly_2015a, pre_Bong_monthly_2015)
pre_Bong_monthly <- rbind(select(pre_Bong_monthly,date, Year, Month, day, Location, pre), pre_Bong_monthly_2015)
rm(pre_Bong_monthly_2015, pre_Bong_monthly_2015a)
pre_Bong_monthly$measurement <- "pre"
pre_Bong_monthly <- rename(pre_Bong_monthly, Value=pre)

#Gbarpolu
pre_Gbarpolu_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            pre1=pre.var[which(lon==10.25),which(lat==7.75),1:48],
                                            pre2=pre.var[which(lon==10.25),which(lat==7.25),1:48],
                                            pre3=pre.var[which(lon==9.75),which(lat==7.25),1:48]))
pre_Gbarpolu_monthly$pre <- rowMeans(select(pre_Gbarpolu_monthly, pre1, pre2, pre3))
pre_Gbarpolu_monthly$Location <- 'Gbarpolu'
pre_Gbarpolu_monthly$date <- ymd(paste(pre_Gbarpolu_monthly$Year, pre_Gbarpolu_monthly$Month, pre_Gbarpolu_monthly$day, sep="-"))
pre_Gbarpolu_monthly_2015 <- select(pre_Gbarpolu_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Gbarpolu_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Gbarpolu_monthly_2015a$Location <- 'Gbarpolu'
pre_Gbarpolu_monthly_2015a$date <- ymd(paste(pre_Gbarpolu_monthly_2015a$Year, pre_Gbarpolu_monthly_2015a$Month, pre_Gbarpolu_monthly_2015a$day, sep="-"))
pre_Gbarpolu_monthly_2015a <- select(pre_Gbarpolu_monthly_2015a, date, Year, Month, day, Location)
pre_Gbarpolu_monthly_2015 <- full_join(pre_Gbarpolu_monthly_2015a, pre_Gbarpolu_monthly_2015)
pre_Gbarpolu_monthly <- rbind(select(pre_Gbarpolu_monthly,date, Year, Month, day, Location, pre), pre_Gbarpolu_monthly_2015)
rm(pre_Gbarpolu_monthly_2015, pre_Gbarpolu_monthly_2015a)
pre_Gbarpolu_monthly$measurement <- "pre"
pre_Gbarpolu_monthly <- rename(pre_Gbarpolu_monthly, Value=pre)

#Grand_Bassa
pre_Grand_Bassa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               pre1=pre.var[which(lon==10.25),which(lat==6.25),1:48],
                                               pre2=pre.var[which(lon==9.75),which(lat==6.25),1:48]))
pre_Grand_Bassa_monthly$pre <- rowMeans(select(pre_Grand_Bassa_monthly, pre1, pre2))
pre_Grand_Bassa_monthly$Location <- 'Grand_Bassa'
pre_Grand_Bassa_monthly$date <- ymd(paste(pre_Grand_Bassa_monthly$Year, pre_Grand_Bassa_monthly$Month, pre_Grand_Bassa_monthly$day, sep="-"))
pre_Grand_Bassa_monthly_2015 <- select(pre_Grand_Bassa_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Grand_Bassa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Grand_Bassa_monthly_2015a$Location <- 'Grand_Bassa'
pre_Grand_Bassa_monthly_2015a$date <- ymd(paste(pre_Grand_Bassa_monthly_2015a$Year, pre_Grand_Bassa_monthly_2015a$Month, pre_Grand_Bassa_monthly_2015a$day, sep="-"))
pre_Grand_Bassa_monthly_2015a <- select(pre_Grand_Bassa_monthly_2015a, date, Year, Month, day, Location)
pre_Grand_Bassa_monthly_2015 <- full_join(pre_Grand_Bassa_monthly_2015a, pre_Grand_Bassa_monthly_2015)
pre_Grand_Bassa_monthly <- rbind(select(pre_Grand_Bassa_monthly,date, Year, Month, day, Location, pre), pre_Grand_Bassa_monthly_2015)
rm(pre_Grand_Bassa_monthly_2015, pre_Grand_Bassa_monthly_2015a)
pre_Grand_Bassa_monthly$measurement <- "pre"
pre_Grand_Bassa_monthly <- rename(pre_Grand_Bassa_monthly, Value=pre)

#Grand_Cape_Mount
pre_Grand_Cape_Mount_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                                    Month=rep(seq(1,12,1), 4), day=1,
                                                    pre1=pre.var[which(lon==11.25),which(lat==7.25),1:48],
                                                    pre2=pre.var[which(lon==10.75),which(lat==7.25),1:48],
                                                    pre3=pre.var[which(lon==11.25),which(lat==6.75),1:48]))
pre_Grand_Cape_Mount_monthly$pre <- rowMeans(select(pre_Grand_Cape_Mount_monthly, pre1, pre2, pre3))
pre_Grand_Cape_Mount_monthly$Location <- 'Grand_Cape_Mount'
pre_Grand_Cape_Mount_monthly$date <- ymd(paste(pre_Grand_Cape_Mount_monthly$Year, pre_Grand_Cape_Mount_monthly$Month, pre_Grand_Cape_Mount_monthly$day, sep="-"))
pre_Grand_Cape_Mount_monthly_2015 <- select(pre_Grand_Cape_Mount_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Grand_Cape_Mount_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Grand_Cape_Mount_monthly_2015a$Location <- 'Grand_Cape_Mount'
pre_Grand_Cape_Mount_monthly_2015a$date <- ymd(paste(pre_Grand_Cape_Mount_monthly_2015a$Year, pre_Grand_Cape_Mount_monthly_2015a$Month, pre_Grand_Cape_Mount_monthly_2015a$day, sep="-"))
pre_Grand_Cape_Mount_monthly_2015a <- select(pre_Grand_Cape_Mount_monthly_2015a, date, Year, Month, day, Location)
pre_Grand_Cape_Mount_monthly_2015 <- full_join(pre_Grand_Cape_Mount_monthly_2015a, pre_Grand_Cape_Mount_monthly_2015)
pre_Grand_Cape_Mount_monthly <- rbind(select(pre_Grand_Cape_Mount_monthly,date, Year, Month, day, Location, pre), pre_Grand_Cape_Mount_monthly_2015)
rm(pre_Grand_Cape_Mount_monthly_2015, pre_Grand_Cape_Mount_monthly_2015a)
pre_Grand_Cape_Mount_monthly$measurement <- "pre"
pre_Grand_Cape_Mount_monthly <- rename(pre_Grand_Cape_Mount_monthly, Value=pre)

#Grand_Gedeh
pre_Grand_Gedeh_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               pre1=pre.var[which(lon==8.75),which(lat==6.25),1:48],
                                               pre2=pre.var[which(lon==8.25),which(lat==6.25),1:48],
                                               pre3=pre.var[which(lon==8.75),which(lat==5.75),1:48],
                                               pre4=pre.var[which(lon==8.25),which(lat==5.75),1:48],
                                               pre5=pre.var[which(lon==7.75),which(lat==5.75),1:48]))
pre_Grand_Gedeh_monthly$pre <- rowMeans(select(pre_Grand_Gedeh_monthly, pre1, pre2, pre3, pre4, pre5))
pre_Grand_Gedeh_monthly$Location <- 'Grand_Gedeh'
pre_Grand_Gedeh_monthly$date <- ymd(paste(pre_Grand_Gedeh_monthly$Year, pre_Grand_Gedeh_monthly$Month, pre_Grand_Gedeh_monthly$day, sep="-"))
pre_Grand_Gedeh_monthly_2015 <- select(pre_Grand_Gedeh_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Grand_Gedeh_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Grand_Gedeh_monthly_2015a$Location <- 'Grand_Gedeh'
pre_Grand_Gedeh_monthly_2015a$date <- ymd(paste(pre_Grand_Gedeh_monthly_2015a$Year, pre_Grand_Gedeh_monthly_2015a$Month, pre_Grand_Gedeh_monthly_2015a$day, sep="-"))
pre_Grand_Gedeh_monthly_2015a <- select(pre_Grand_Gedeh_monthly_2015a, date, Year, Month, day, Location)
pre_Grand_Gedeh_monthly_2015 <- full_join(pre_Grand_Gedeh_monthly_2015a, pre_Grand_Gedeh_monthly_2015)
pre_Grand_Gedeh_monthly <- rbind(select(pre_Grand_Gedeh_monthly,date, Year, Month, day, Location, pre), pre_Grand_Gedeh_monthly_2015)
rm(pre_Grand_Gedeh_monthly_2015, pre_Grand_Gedeh_monthly_2015a)
pre_Grand_Gedeh_monthly$measurement <- "pre"
pre_Grand_Gedeh_monthly <- rename(pre_Grand_Gedeh_monthly, Value=pre)

#Grand_Kru
pre_Grand_Kru_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             pre=pre.var[which(lon==8.25),which(lat==4.75),1:48]))
pre_Grand_Kru_monthly$Location <- 'Grand_Kru'
pre_Grand_Kru_monthly$date <- ymd(paste(pre_Grand_Kru_monthly$Year, pre_Grand_Kru_monthly$Month, pre_Grand_Kru_monthly$day, sep="-"))
pre_Grand_Kru_monthly_2015 <- select(pre_Grand_Kru_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Grand_Kru_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Grand_Kru_monthly_2015a$Location <- 'Grand_Kru'
pre_Grand_Kru_monthly_2015a$date <- ymd(paste(pre_Grand_Kru_monthly_2015a$Year, pre_Grand_Kru_monthly_2015a$Month, pre_Grand_Kru_monthly_2015a$day, sep="-"))
pre_Grand_Kru_monthly_2015a <- select(pre_Grand_Kru_monthly_2015a, date, Year, Month, day, Location)
pre_Grand_Kru_monthly_2015 <- full_join(pre_Grand_Kru_monthly_2015a, pre_Grand_Kru_monthly_2015)
pre_Grand_Kru_monthly <- rbind(select(pre_Grand_Kru_monthly,date, Year, Month, day, Location, pre), pre_Grand_Kru_monthly_2015)
rm(pre_Grand_Kru_monthly_2015, pre_Grand_Kru_monthly_2015a)
pre_Grand_Kru_monthly$measurement <- "pre"
pre_Grand_Kru_monthly <- rename(pre_Grand_Kru_monthly, Value=pre)

#Lofa
pre_Lofa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        pre1=pre.var[which(lon==10.25),which(lat==8.25),1:48],
                                        pre2=pre.var[which(lon==9.75),which(lat==8.25),1:48],
                                        pre3=pre.var[which(lon==9.75),which(lat==7.75),1:48]))
pre_Lofa_monthly$pre <- rowMeans(select(pre_Lofa_monthly, pre1, pre2, pre3))
pre_Lofa_monthly$Location <- 'Lofa'
pre_Lofa_monthly$date <- ymd(paste(pre_Lofa_monthly$Year, pre_Lofa_monthly$Month, pre_Lofa_monthly$day, sep="-"))
pre_Lofa_monthly_2015 <- select(pre_Lofa_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Lofa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Lofa_monthly_2015a$Location <- 'Lofa'
pre_Lofa_monthly_2015a$date <- ymd(paste(pre_Lofa_monthly_2015a$Year, pre_Lofa_monthly_2015a$Month, pre_Lofa_monthly_2015a$day, sep="-"))
pre_Lofa_monthly_2015a <- select(pre_Lofa_monthly_2015a, date, Year, Month, day, Location)
pre_Lofa_monthly_2015 <- full_join(pre_Lofa_monthly_2015a, pre_Lofa_monthly_2015)
pre_Lofa_monthly <- rbind(select(pre_Lofa_monthly,date, Year, Month, day, Location, pre), pre_Lofa_monthly_2015)
rm(pre_Lofa_monthly_2015, pre_Lofa_monthly_2015a)
pre_Lofa_monthly$measurement <- "pre"
pre_Lofa_monthly <- rename(pre_Lofa_monthly, Value=pre)

#Margibi
pre_Margibi_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           pre1=pre.var[which(lon==10.25),which(lat==6.75),1:48],
                                           pre2=pre.var[which(lon==10.25),which(lat==6.25),1:48]))
pre_Margibi_monthly$pre <- rowMeans(select(pre_Margibi_monthly, pre1, pre2))
pre_Margibi_monthly$Location <- 'Margibi'
pre_Margibi_monthly$date <- ymd(paste(pre_Margibi_monthly$Year, pre_Margibi_monthly$Month, pre_Margibi_monthly$day, sep="-"))
pre_Margibi_monthly_2015 <- select(pre_Margibi_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Margibi_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Margibi_monthly_2015a$Location <- 'Margibi'
pre_Margibi_monthly_2015a$date <- ymd(paste(pre_Margibi_monthly_2015a$Year, pre_Margibi_monthly_2015a$Month, pre_Margibi_monthly_2015a$day, sep="-"))
pre_Margibi_monthly_2015a <- select(pre_Margibi_monthly_2015a, date, Year, Month, day, Location)
pre_Margibi_monthly_2015 <- full_join(pre_Margibi_monthly_2015a, pre_Margibi_monthly_2015)
pre_Margibi_monthly <- rbind(select(pre_Margibi_monthly,date, Year, Month, day, Location, pre), pre_Margibi_monthly_2015)
rm(pre_Margibi_monthly_2015, pre_Margibi_monthly_2015a)
pre_Margibi_monthly$measurement <- "pre"
pre_Margibi_monthly <- rename(pre_Margibi_monthly, Value=pre)

#Maryland
pre_Maryland_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            pre=pre.var[which(lon==7.75),which(lat==4.75),1:48]))
pre_Maryland_monthly$Location <- 'Maryland'
pre_Maryland_monthly$date <- ymd(paste(pre_Maryland_monthly$Year, pre_Maryland_monthly$Month, pre_Maryland_monthly$day, sep="-"))
pre_Maryland_monthly_2015 <- select(pre_Maryland_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Maryland_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Maryland_monthly_2015a$Location <- 'Maryland'
pre_Maryland_monthly_2015a$date <- ymd(paste(pre_Maryland_monthly_2015a$Year, pre_Maryland_monthly_2015a$Month, pre_Maryland_monthly_2015a$day, sep="-"))
pre_Maryland_monthly_2015a <- select(pre_Maryland_monthly_2015a, date, Year, Month, day, Location)
pre_Maryland_monthly_2015 <- full_join(pre_Maryland_monthly_2015a, pre_Maryland_monthly_2015)
pre_Maryland_monthly <- rbind(select(pre_Maryland_monthly,date, Year, Month, day, Location, pre), pre_Maryland_monthly_2015)
rm(pre_Maryland_monthly_2015, pre_Maryland_monthly_2015a)
pre_Maryland_monthly$measurement <- "pre"
pre_Maryland_monthly <- rename(pre_Maryland_monthly, Value=pre)

#Montserrado
pre_Montserrado_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               pre=pre.var[which(lon==10.75),which(lat==6.25),1:48]))
pre_Montserrado_monthly$Location <- 'Montserrado'
pre_Montserrado_monthly$date <- ymd(paste(pre_Montserrado_monthly$Year, pre_Montserrado_monthly$Month, pre_Montserrado_monthly$day, sep="-"))
pre_Montserrado_monthly_2015 <- select(pre_Montserrado_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Montserrado_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Montserrado_monthly_2015a$Location <- 'Montserrado'
pre_Montserrado_monthly_2015a$date <- ymd(paste(pre_Montserrado_monthly_2015a$Year, pre_Montserrado_monthly_2015a$Month, pre_Montserrado_monthly_2015a$day, sep="-"))
pre_Montserrado_monthly_2015a <- select(pre_Montserrado_monthly_2015a, date, Year, Month, day, Location)
pre_Montserrado_monthly_2015 <- full_join(pre_Montserrado_monthly_2015a, pre_Montserrado_monthly_2015)
pre_Montserrado_monthly <- rbind(select(pre_Montserrado_monthly,date, Year, Month, day, Location, pre), pre_Montserrado_monthly_2015)
rm(pre_Montserrado_monthly_2015, pre_Montserrado_monthly_2015a)
pre_Montserrado_monthly$measurement <- "pre"
pre_Montserrado_monthly <- rename(pre_Montserrado_monthly, Value=pre)

#Nimba
pre_Nimba_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         pre1=pre.var[which(lon==8.75),which(lat==7.25),1:48],
                                         pre2=pre.var[which(lon==8.75),which(lat==6.75),1:48],
                                         pre3=pre.var[which(lon==8.75),which(lat==6.25),1:48]))
pre_Nimba_monthly$pre <- rowMeans(select(pre_Nimba_monthly, pre1, pre2, pre3))
pre_Nimba_monthly$Location <- 'Nimba'
pre_Nimba_monthly$date <- ymd(paste(pre_Nimba_monthly$Year, pre_Nimba_monthly$Month, pre_Nimba_monthly$day, sep="-"))
pre_Nimba_monthly_2015 <- select(pre_Nimba_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Nimba_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Nimba_monthly_2015a$Location <- 'Nimba'
pre_Nimba_monthly_2015a$date <- ymd(paste(pre_Nimba_monthly_2015a$Year, pre_Nimba_monthly_2015a$Month, pre_Nimba_monthly_2015a$day, sep="-"))
pre_Nimba_monthly_2015a <- select(pre_Nimba_monthly_2015a, date, Year, Month, day, Location)
pre_Nimba_monthly_2015 <- full_join(pre_Nimba_monthly_2015a, pre_Nimba_monthly_2015)
pre_Nimba_monthly <- rbind(select(pre_Nimba_monthly,date, Year, Month, day, Location, pre), pre_Nimba_monthly_2015)
rm(pre_Nimba_monthly_2015, pre_Nimba_monthly_2015a)
pre_Nimba_monthly$measurement <- "pre"
pre_Nimba_monthly <- rename(pre_Nimba_monthly, Value=pre)

#River_Gee
pre_River_Gee_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             pre1=pre.var[which(lon==8.25),which(lat==5.25),1:48],
                                             pre2=pre.var[which(lon==7.75),which(lat==5.25),1:48]))
pre_River_Gee_monthly$pre <- rowMeans(select(pre_River_Gee_monthly, pre1, pre2))
pre_River_Gee_monthly$Location <- 'River_Gee'
pre_River_Gee_monthly$date <- ymd(paste(pre_River_Gee_monthly$Year, pre_River_Gee_monthly$Month, pre_River_Gee_monthly$day, sep="-"))
pre_River_Gee_monthly_2015 <- select(pre_River_Gee_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_River_Gee_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_River_Gee_monthly_2015a$Location <- 'River_Gee'
pre_River_Gee_monthly_2015a$date <- ymd(paste(pre_River_Gee_monthly_2015a$Year, pre_River_Gee_monthly_2015a$Month, pre_River_Gee_monthly_2015a$day, sep="-"))
pre_River_Gee_monthly_2015a <- select(pre_River_Gee_monthly_2015a, date, Year, Month, day, Location)
pre_River_Gee_monthly_2015 <- full_join(pre_River_Gee_monthly_2015a, pre_River_Gee_monthly_2015)
pre_River_Gee_monthly <- rbind(select(pre_River_Gee_monthly,date, Year, Month, day, Location, pre), pre_River_Gee_monthly_2015)
rm(pre_River_Gee_monthly_2015, pre_River_Gee_monthly_2015a)
pre_River_Gee_monthly$measurement <- "pre"
pre_River_Gee_monthly <- rename(pre_River_Gee_monthly, Value=pre)

#Rivercess
pre_River_Cess_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                              Month=rep(seq(1,12,1), 4), day=1,
                                              pre1=pre.var[which(lon==9.25),which(lat==6.25),1:48],
                                              pre2=pre.var[which(lon==9.25),which(lat==5.75),1:48],
                                              pre3=pre.var[which(lon==9.75),which(lat==5.25),1:48]))
pre_River_Cess_monthly$pre <- rowMeans(select(pre_River_Cess_monthly, pre1, pre2, pre3))
pre_River_Cess_monthly$Location <- 'River_Cess'
pre_River_Cess_monthly$date <- ymd(paste(pre_River_Cess_monthly$Year, pre_River_Cess_monthly$Month, pre_River_Cess_monthly$day, sep="-"))
pre_River_Cess_monthly_2015 <- select(pre_River_Cess_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_River_Cess_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_River_Cess_monthly_2015a$Location <- 'River_Cess'
pre_River_Cess_monthly_2015a$date <- ymd(paste(pre_River_Cess_monthly_2015a$Year, pre_River_Cess_monthly_2015a$Month, pre_River_Cess_monthly_2015a$day, sep="-"))
pre_River_Cess_monthly_2015a <- select(pre_River_Cess_monthly_2015a, date, Year, Month, day, Location)
pre_River_Cess_monthly_2015 <- full_join(pre_River_Cess_monthly_2015a, pre_River_Cess_monthly_2015)
pre_River_Cess_monthly <- rbind(select(pre_River_Cess_monthly,date, Year, Month, day, Location, pre), pre_River_Cess_monthly_2015)
rm(pre_River_Cess_monthly_2015, pre_River_Cess_monthly_2015a)
pre_River_Cess_monthly$measurement <- "pre"
pre_River_Cess_monthly <- rename(pre_River_Cess_monthly, Value=pre)

#Sinoe
pre_Sinoe_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         pre1=pre.var[which(lon==8.75),which(lat==5.75),1:48],
                                         pre2=pre.var[which(lon==9.25),which(lat==5.25),1:48],
                                         pre3=pre.var[which(lon==8.75),which(lat==5.25),1:48],
                                         pre4=pre.var[which(lon==8.75),which(lat==4.75),1:48]))
pre_Sinoe_monthly$pre <- rowMeans(select(pre_Sinoe_monthly, pre1, pre2, pre3, pre4))
pre_Sinoe_monthly$Location <- 'Sinoe'
pre_Sinoe_monthly$date <- ymd(paste(pre_Sinoe_monthly$Year, pre_Sinoe_monthly$Month, pre_Sinoe_monthly$day, sep="-"))
pre_Sinoe_monthly_2015 <- select(pre_Sinoe_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Sinoe_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Sinoe_monthly_2015a$Location <- 'Sinoe'
pre_Sinoe_monthly_2015a$date <- ymd(paste(pre_Sinoe_monthly_2015a$Year, pre_Sinoe_monthly_2015a$Month, pre_Sinoe_monthly_2015a$day, sep="-"))
pre_Sinoe_monthly_2015a <- select(pre_Sinoe_monthly_2015a, date, Year, Month, day, Location)
pre_Sinoe_monthly_2015 <- full_join(pre_Sinoe_monthly_2015a, pre_Sinoe_monthly_2015)
pre_Sinoe_monthly <- rbind(select(pre_Sinoe_monthly,date, Year, Month, day, Location, pre), pre_Sinoe_monthly_2015)
rm(pre_Sinoe_monthly_2015, pre_Sinoe_monthly_2015a)
pre_Sinoe_monthly$measurement <- "pre"
pre_Sinoe_monthly <- rename(pre_Sinoe_monthly, Value=pre)

#Merging in long format
preLiberiamonthly_district <- rbind(pre_Bomi_monthly, pre_Bong_monthly, pre_Gbarpolu_monthly,
                                 pre_Grand_Bassa_monthly, pre_Grand_Cape_Mount_monthly, pre_Grand_Gedeh_monthly,
                                 pre_Grand_Kru_monthly, pre_Lofa_monthly, pre_Margibi_monthly,
                                 pre_Maryland_monthly, pre_Montserrado_monthly, pre_Nimba_monthly,
                                 pre_River_Gee_monthly, pre_River_Cess_monthly, pre_Sinoe_monthly)

#####################
#tmn - min Temp
#####################
tmn.full <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.tmn.dat.nc', write=FALSE)
tmn.var <- var.get.nc(tmn.full, "tmn")

#Bomi
tmn_Bomi_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmn=tmn.var[which(lon==10.75),which(lat==6.75),1:48]))
tmn_Bomi_monthly$Location <- 'Bomi'
tmn_Bomi_monthly$date <- ymd(paste(tmn_Bomi_monthly$Year, tmn_Bomi_monthly$Month, tmn_Bomi_monthly$day, sep="-"))
tmn_Bomi_monthly_2015 <- select(tmn_Bomi_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Bomi_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Bomi_monthly_2015a$Location <- 'Bomi'
tmn_Bomi_monthly_2015a$date <- ymd(paste(tmn_Bomi_monthly_2015a$Year, tmn_Bomi_monthly_2015a$Month, tmn_Bomi_monthly_2015a$day, sep="-"))
tmn_Bomi_monthly_2015a <- select(tmn_Bomi_monthly_2015a, date, Year, Month, day, Location)
tmn_Bomi_monthly_2015 <- full_join(tmn_Bomi_monthly_2015a, tmn_Bomi_monthly_2015)
tmn_Bomi_monthly <- rbind(select(tmn_Bomi_monthly,date, Year, Month, day, Location, tmn), tmn_Bomi_monthly_2015)
rm(tmn_Bomi_monthly_2015, tmn_Bomi_monthly_2015a)
tmn_Bomi_monthly$measurement <- "tmn"
tmn_Bomi_monthly <- rename(tmn_Bomi_monthly, Value=tmn)

#Bong
tmn_Bong_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmn1=tmn.var[which(lon==9.75),which(lat==7.25),1:48],
                                        tmn2=tmn.var[which(lon==9.25),which(lat==7.25),1:48],
                                        tmn3=tmn.var[which(lon==10.25),which(lat==6.75),1:48],
                                        tmn4=tmn.var[which(lon==9.75),which(lat==6.75),1:48],
                                        tmn5=tmn.var[which(lon==9.25),which(lat==6.75),1:48]))
tmn_Bong_monthly$tmn <- rowMeans(select(tmn_Bong_monthly, tmn1, tmn2, tmn3, tmn4, tmn5))
tmn_Bong_monthly$Location <- 'Bong'
tmn_Bong_monthly$date <- ymd(paste(tmn_Bong_monthly$Year, tmn_Bong_monthly$Month, tmn_Bong_monthly$day, sep="-"))
tmn_Bong_monthly_2015 <- select(tmn_Bong_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Bong_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Bong_monthly_2015a$Location <- 'Bong'
tmn_Bong_monthly_2015a$date <- ymd(paste(tmn_Bong_monthly_2015a$Year, tmn_Bong_monthly_2015a$Month, tmn_Bong_monthly_2015a$day, sep="-"))
tmn_Bong_monthly_2015a <- select(tmn_Bong_monthly_2015a, date, Year, Month, day, Location)
tmn_Bong_monthly_2015 <- full_join(tmn_Bong_monthly_2015a, tmn_Bong_monthly_2015)
tmn_Bong_monthly <- rbind(select(tmn_Bong_monthly,date, Year, Month, day, Location, tmn), tmn_Bong_monthly_2015)
rm(tmn_Bong_monthly_2015, tmn_Bong_monthly_2015a)
tmn_Bong_monthly$measurement <- "tmn"
tmn_Bong_monthly <- rename(tmn_Bong_monthly, Value=tmn)

#Gbarpolu
tmn_Gbarpolu_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            tmn1=tmn.var[which(lon==10.25),which(lat==7.75),1:48],
                                            tmn2=tmn.var[which(lon==10.25),which(lat==7.25),1:48],
                                            tmn3=tmn.var[which(lon==9.75),which(lat==7.25),1:48]))
tmn_Gbarpolu_monthly$tmn <- rowMeans(select(tmn_Gbarpolu_monthly, tmn1, tmn2, tmn3))
tmn_Gbarpolu_monthly$Location <- 'Gbarpolu'
tmn_Gbarpolu_monthly$date <- ymd(paste(tmn_Gbarpolu_monthly$Year, tmn_Gbarpolu_monthly$Month, tmn_Gbarpolu_monthly$day, sep="-"))
tmn_Gbarpolu_monthly_2015 <- select(tmn_Gbarpolu_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Gbarpolu_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Gbarpolu_monthly_2015a$Location <- 'Gbarpolu'
tmn_Gbarpolu_monthly_2015a$date <- ymd(paste(tmn_Gbarpolu_monthly_2015a$Year, tmn_Gbarpolu_monthly_2015a$Month, tmn_Gbarpolu_monthly_2015a$day, sep="-"))
tmn_Gbarpolu_monthly_2015a <- select(tmn_Gbarpolu_monthly_2015a, date, Year, Month, day, Location)
tmn_Gbarpolu_monthly_2015 <- full_join(tmn_Gbarpolu_monthly_2015a, tmn_Gbarpolu_monthly_2015)
tmn_Gbarpolu_monthly <- rbind(select(tmn_Gbarpolu_monthly,date, Year, Month, day, Location, tmn), tmn_Gbarpolu_monthly_2015)
rm(tmn_Gbarpolu_monthly_2015, tmn_Gbarpolu_monthly_2015a)
tmn_Gbarpolu_monthly$measurement <- "tmn"
tmn_Gbarpolu_monthly <- rename(tmn_Gbarpolu_monthly, Value=tmn)

#Grand_Bassa
tmn_Grand_Bassa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               tmn1=tmn.var[which(lon==10.25),which(lat==6.25),1:48],
                                               tmn2=tmn.var[which(lon==9.75),which(lat==6.25),1:48]))
tmn_Grand_Bassa_monthly$tmn <- rowMeans(select(tmn_Grand_Bassa_monthly, tmn1, tmn2))
tmn_Grand_Bassa_monthly$Location <- 'Grand_Bassa'
tmn_Grand_Bassa_monthly$date <- ymd(paste(tmn_Grand_Bassa_monthly$Year, tmn_Grand_Bassa_monthly$Month, tmn_Grand_Bassa_monthly$day, sep="-"))
tmn_Grand_Bassa_monthly_2015 <- select(tmn_Grand_Bassa_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Grand_Bassa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Grand_Bassa_monthly_2015a$Location <- 'Grand_Bassa'
tmn_Grand_Bassa_monthly_2015a$date <- ymd(paste(tmn_Grand_Bassa_monthly_2015a$Year, tmn_Grand_Bassa_monthly_2015a$Month, tmn_Grand_Bassa_monthly_2015a$day, sep="-"))
tmn_Grand_Bassa_monthly_2015a <- select(tmn_Grand_Bassa_monthly_2015a, date, Year, Month, day, Location)
tmn_Grand_Bassa_monthly_2015 <- full_join(tmn_Grand_Bassa_monthly_2015a, tmn_Grand_Bassa_monthly_2015)
tmn_Grand_Bassa_monthly <- rbind(select(tmn_Grand_Bassa_monthly,date, Year, Month, day, Location, tmn), tmn_Grand_Bassa_monthly_2015)
rm(tmn_Grand_Bassa_monthly_2015, tmn_Grand_Bassa_monthly_2015a)
tmn_Grand_Bassa_monthly$measurement <- "tmn"
tmn_Grand_Bassa_monthly <- rename(tmn_Grand_Bassa_monthly, Value=tmn)

#Grand_Cape_Mount
tmn_Grand_Cape_Mount_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                                    Month=rep(seq(1,12,1), 4), day=1,
                                                    tmn1=tmn.var[which(lon==11.25),which(lat==7.25),1:48],
                                                    tmn2=tmn.var[which(lon==10.75),which(lat==7.25),1:48],
                                                    tmn3=tmn.var[which(lon==11.25),which(lat==6.75),1:48]))
tmn_Grand_Cape_Mount_monthly$tmn <- rowMeans(select(tmn_Grand_Cape_Mount_monthly, tmn1, tmn2, tmn3))
tmn_Grand_Cape_Mount_monthly$Location <- 'Grand_Cape_Mount'
tmn_Grand_Cape_Mount_monthly$date <- ymd(paste(tmn_Grand_Cape_Mount_monthly$Year, tmn_Grand_Cape_Mount_monthly$Month, tmn_Grand_Cape_Mount_monthly$day, sep="-"))
tmn_Grand_Cape_Mount_monthly_2015 <- select(tmn_Grand_Cape_Mount_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Grand_Cape_Mount_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Grand_Cape_Mount_monthly_2015a$Location <- 'Grand_Cape_Mount'
tmn_Grand_Cape_Mount_monthly_2015a$date <- ymd(paste(tmn_Grand_Cape_Mount_monthly_2015a$Year, tmn_Grand_Cape_Mount_monthly_2015a$Month, tmn_Grand_Cape_Mount_monthly_2015a$day, sep="-"))
tmn_Grand_Cape_Mount_monthly_2015a <- select(tmn_Grand_Cape_Mount_monthly_2015a, date, Year, Month, day, Location)
tmn_Grand_Cape_Mount_monthly_2015 <- full_join(tmn_Grand_Cape_Mount_monthly_2015a, tmn_Grand_Cape_Mount_monthly_2015)
tmn_Grand_Cape_Mount_monthly <- rbind(select(tmn_Grand_Cape_Mount_monthly,date, Year, Month, day, Location, tmn), tmn_Grand_Cape_Mount_monthly_2015)
rm(tmn_Grand_Cape_Mount_monthly_2015, tmn_Grand_Cape_Mount_monthly_2015a)
tmn_Grand_Cape_Mount_monthly$measurement <- "tmn"
tmn_Grand_Cape_Mount_monthly <- rename(tmn_Grand_Cape_Mount_monthly, Value=tmn)

#Grand_Gedeh
tmn_Grand_Gedeh_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               tmn1=tmn.var[which(lon==8.75),which(lat==6.25),1:48],
                                               tmn2=tmn.var[which(lon==8.25),which(lat==6.25),1:48],
                                               tmn3=tmn.var[which(lon==8.75),which(lat==5.75),1:48],
                                               tmn4=tmn.var[which(lon==8.25),which(lat==5.75),1:48],
                                               tmn5=tmn.var[which(lon==7.75),which(lat==5.75),1:48]))
tmn_Grand_Gedeh_monthly$tmn <- rowMeans(select(tmn_Grand_Gedeh_monthly, tmn1, tmn2, tmn3, tmn4, tmn5))
tmn_Grand_Gedeh_monthly$Location <- 'Grand_Gedeh'
tmn_Grand_Gedeh_monthly$date <- ymd(paste(tmn_Grand_Gedeh_monthly$Year, tmn_Grand_Gedeh_monthly$Month, tmn_Grand_Gedeh_monthly$day, sep="-"))
tmn_Grand_Gedeh_monthly_2015 <- select(tmn_Grand_Gedeh_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Grand_Gedeh_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Grand_Gedeh_monthly_2015a$Location <- 'Grand_Gedeh'
tmn_Grand_Gedeh_monthly_2015a$date <- ymd(paste(tmn_Grand_Gedeh_monthly_2015a$Year, tmn_Grand_Gedeh_monthly_2015a$Month, tmn_Grand_Gedeh_monthly_2015a$day, sep="-"))
tmn_Grand_Gedeh_monthly_2015a <- select(tmn_Grand_Gedeh_monthly_2015a, date, Year, Month, day, Location)
tmn_Grand_Gedeh_monthly_2015 <- full_join(tmn_Grand_Gedeh_monthly_2015a, tmn_Grand_Gedeh_monthly_2015)
tmn_Grand_Gedeh_monthly <- rbind(select(tmn_Grand_Gedeh_monthly,date, Year, Month, day, Location, tmn), tmn_Grand_Gedeh_monthly_2015)
rm(tmn_Grand_Gedeh_monthly_2015, tmn_Grand_Gedeh_monthly_2015a)
tmn_Grand_Gedeh_monthly$measurement <- "tmn"
tmn_Grand_Gedeh_monthly <- rename(tmn_Grand_Gedeh_monthly, Value=tmn)

#Grand_Kru
tmn_Grand_Kru_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             tmn=tmn.var[which(lon==8.25),which(lat==4.75),1:48]))
tmn_Grand_Kru_monthly$Location <- 'Grand_Kru'
tmn_Grand_Kru_monthly$date <- ymd(paste(tmn_Grand_Kru_monthly$Year, tmn_Grand_Kru_monthly$Month, tmn_Grand_Kru_monthly$day, sep="-"))
tmn_Grand_Kru_monthly_2015 <- select(tmn_Grand_Kru_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Grand_Kru_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Grand_Kru_monthly_2015a$Location <- 'Grand_Kru'
tmn_Grand_Kru_monthly_2015a$date <- ymd(paste(tmn_Grand_Kru_monthly_2015a$Year, tmn_Grand_Kru_monthly_2015a$Month, tmn_Grand_Kru_monthly_2015a$day, sep="-"))
tmn_Grand_Kru_monthly_2015a <- select(tmn_Grand_Kru_monthly_2015a, date, Year, Month, day, Location)
tmn_Grand_Kru_monthly_2015 <- full_join(tmn_Grand_Kru_monthly_2015a, tmn_Grand_Kru_monthly_2015)
tmn_Grand_Kru_monthly <- rbind(select(tmn_Grand_Kru_monthly,date, Year, Month, day, Location, tmn), tmn_Grand_Kru_monthly_2015)
rm(tmn_Grand_Kru_monthly_2015, tmn_Grand_Kru_monthly_2015a)
tmn_Grand_Kru_monthly$measurement <- "tmn"
tmn_Grand_Kru_monthly <- rename(tmn_Grand_Kru_monthly, Value=tmn)

#Lofa
tmn_Lofa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmn1=tmn.var[which(lon==10.25),which(lat==8.25),1:48],
                                        tmn2=tmn.var[which(lon==9.75),which(lat==8.25),1:48],
                                        tmn3=tmn.var[which(lon==9.75),which(lat==7.75),1:48]))
tmn_Lofa_monthly$tmn <- rowMeans(select(tmn_Lofa_monthly, tmn1, tmn2, tmn3))
tmn_Lofa_monthly$Location <- 'Lofa'
tmn_Lofa_monthly$date <- ymd(paste(tmn_Lofa_monthly$Year, tmn_Lofa_monthly$Month, tmn_Lofa_monthly$day, sep="-"))
tmn_Lofa_monthly_2015 <- select(tmn_Lofa_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Lofa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Lofa_monthly_2015a$Location <- 'Lofa'
tmn_Lofa_monthly_2015a$date <- ymd(paste(tmn_Lofa_monthly_2015a$Year, tmn_Lofa_monthly_2015a$Month, tmn_Lofa_monthly_2015a$day, sep="-"))
tmn_Lofa_monthly_2015a <- select(tmn_Lofa_monthly_2015a, date, Year, Month, day, Location)
tmn_Lofa_monthly_2015 <- full_join(tmn_Lofa_monthly_2015a, tmn_Lofa_monthly_2015)
tmn_Lofa_monthly <- rbind(select(tmn_Lofa_monthly,date, Year, Month, day, Location, tmn), tmn_Lofa_monthly_2015)
rm(tmn_Lofa_monthly_2015, tmn_Lofa_monthly_2015a)
tmn_Lofa_monthly$measurement <- "tmn"
tmn_Lofa_monthly <- rename(tmn_Lofa_monthly, Value=tmn)

#Margibi
tmn_Margibi_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmn1=tmn.var[which(lon==10.25),which(lat==6.75),1:48],
                                           tmn2=tmn.var[which(lon==10.25),which(lat==6.25),1:48]))
tmn_Margibi_monthly$tmn <- rowMeans(select(tmn_Margibi_monthly, tmn1, tmn2))
tmn_Margibi_monthly$Location <- 'Margibi'
tmn_Margibi_monthly$date <- ymd(paste(tmn_Margibi_monthly$Year, tmn_Margibi_monthly$Month, tmn_Margibi_monthly$day, sep="-"))
tmn_Margibi_monthly_2015 <- select(tmn_Margibi_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Margibi_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Margibi_monthly_2015a$Location <- 'Margibi'
tmn_Margibi_monthly_2015a$date <- ymd(paste(tmn_Margibi_monthly_2015a$Year, tmn_Margibi_monthly_2015a$Month, tmn_Margibi_monthly_2015a$day, sep="-"))
tmn_Margibi_monthly_2015a <- select(tmn_Margibi_monthly_2015a, date, Year, Month, day, Location)
tmn_Margibi_monthly_2015 <- full_join(tmn_Margibi_monthly_2015a, tmn_Margibi_monthly_2015)
tmn_Margibi_monthly <- rbind(select(tmn_Margibi_monthly,date, Year, Month, day, Location, tmn), tmn_Margibi_monthly_2015)
rm(tmn_Margibi_monthly_2015, tmn_Margibi_monthly_2015a)
tmn_Margibi_monthly$measurement <- "tmn"
tmn_Margibi_monthly <- rename(tmn_Margibi_monthly, Value=tmn)

#Maryland
tmn_Maryland_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            tmn=tmn.var[which(lon==7.75),which(lat==4.75),1:48]))
tmn_Maryland_monthly$Location <- 'Maryland'
tmn_Maryland_monthly$date <- ymd(paste(tmn_Maryland_monthly$Year, tmn_Maryland_monthly$Month, tmn_Maryland_monthly$day, sep="-"))
tmn_Maryland_monthly_2015 <- select(tmn_Maryland_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Maryland_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Maryland_monthly_2015a$Location <- 'Maryland'
tmn_Maryland_monthly_2015a$date <- ymd(paste(tmn_Maryland_monthly_2015a$Year, tmn_Maryland_monthly_2015a$Month, tmn_Maryland_monthly_2015a$day, sep="-"))
tmn_Maryland_monthly_2015a <- select(tmn_Maryland_monthly_2015a, date, Year, Month, day, Location)
tmn_Maryland_monthly_2015 <- full_join(tmn_Maryland_monthly_2015a, tmn_Maryland_monthly_2015)
tmn_Maryland_monthly <- rbind(select(tmn_Maryland_monthly,date, Year, Month, day, Location, tmn), tmn_Maryland_monthly_2015)
rm(tmn_Maryland_monthly_2015, tmn_Maryland_monthly_2015a)
tmn_Maryland_monthly$measurement <- "tmn"
tmn_Maryland_monthly <- rename(tmn_Maryland_monthly, Value=tmn)

#Montserrado
tmn_Montserrado_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               tmn=tmn.var[which(lon==10.75),which(lat==6.25),1:48]))
tmn_Montserrado_monthly$Location <- 'Montserrado'
tmn_Montserrado_monthly$date <- ymd(paste(tmn_Montserrado_monthly$Year, tmn_Montserrado_monthly$Month, tmn_Montserrado_monthly$day, sep="-"))
tmn_Montserrado_monthly_2015 <- select(tmn_Montserrado_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Montserrado_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Montserrado_monthly_2015a$Location <- 'Montserrado'
tmn_Montserrado_monthly_2015a$date <- ymd(paste(tmn_Montserrado_monthly_2015a$Year, tmn_Montserrado_monthly_2015a$Month, tmn_Montserrado_monthly_2015a$day, sep="-"))
tmn_Montserrado_monthly_2015a <- select(tmn_Montserrado_monthly_2015a, date, Year, Month, day, Location)
tmn_Montserrado_monthly_2015 <- full_join(tmn_Montserrado_monthly_2015a, tmn_Montserrado_monthly_2015)
tmn_Montserrado_monthly <- rbind(select(tmn_Montserrado_monthly,date, Year, Month, day, Location, tmn), tmn_Montserrado_monthly_2015)
rm(tmn_Montserrado_monthly_2015, tmn_Montserrado_monthly_2015a)
tmn_Montserrado_monthly$measurement <- "tmn"
tmn_Montserrado_monthly <- rename(tmn_Montserrado_monthly, Value=tmn)

#Nimba
tmn_Nimba_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         tmn1=tmn.var[which(lon==8.75),which(lat==7.25),1:48],
                                         tmn2=tmn.var[which(lon==8.75),which(lat==6.75),1:48],
                                         tmn3=tmn.var[which(lon==8.75),which(lat==6.25),1:48]))
tmn_Nimba_monthly$tmn <- rowMeans(select(tmn_Nimba_monthly, tmn1, tmn2, tmn3))
tmn_Nimba_monthly$Location <- 'Nimba'
tmn_Nimba_monthly$date <- ymd(paste(tmn_Nimba_monthly$Year, tmn_Nimba_monthly$Month, tmn_Nimba_monthly$day, sep="-"))
tmn_Nimba_monthly_2015 <- select(tmn_Nimba_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Nimba_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Nimba_monthly_2015a$Location <- 'Nimba'
tmn_Nimba_monthly_2015a$date <- ymd(paste(tmn_Nimba_monthly_2015a$Year, tmn_Nimba_monthly_2015a$Month, tmn_Nimba_monthly_2015a$day, sep="-"))
tmn_Nimba_monthly_2015a <- select(tmn_Nimba_monthly_2015a, date, Year, Month, day, Location)
tmn_Nimba_monthly_2015 <- full_join(tmn_Nimba_monthly_2015a, tmn_Nimba_monthly_2015)
tmn_Nimba_monthly <- rbind(select(tmn_Nimba_monthly,date, Year, Month, day, Location, tmn), tmn_Nimba_monthly_2015)
rm(tmn_Nimba_monthly_2015, tmn_Nimba_monthly_2015a)
tmn_Nimba_monthly$measurement <- "tmn"
tmn_Nimba_monthly <- rename(tmn_Nimba_monthly, Value=tmn)

#River_Gee
tmn_River_Gee_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             tmn1=tmn.var[which(lon==8.25),which(lat==5.25),1:48],
                                             tmn2=tmn.var[which(lon==7.75),which(lat==5.25),1:48]))
tmn_River_Gee_monthly$tmn <- rowMeans(select(tmn_River_Gee_monthly, tmn1, tmn2))
tmn_River_Gee_monthly$Location <- 'River_Gee'
tmn_River_Gee_monthly$date <- ymd(paste(tmn_River_Gee_monthly$Year, tmn_River_Gee_monthly$Month, tmn_River_Gee_monthly$day, sep="-"))
tmn_River_Gee_monthly_2015 <- select(tmn_River_Gee_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_River_Gee_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_River_Gee_monthly_2015a$Location <- 'River_Gee'
tmn_River_Gee_monthly_2015a$date <- ymd(paste(tmn_River_Gee_monthly_2015a$Year, tmn_River_Gee_monthly_2015a$Month, tmn_River_Gee_monthly_2015a$day, sep="-"))
tmn_River_Gee_monthly_2015a <- select(tmn_River_Gee_monthly_2015a, date, Year, Month, day, Location)
tmn_River_Gee_monthly_2015 <- full_join(tmn_River_Gee_monthly_2015a, tmn_River_Gee_monthly_2015)
tmn_River_Gee_monthly <- rbind(select(tmn_River_Gee_monthly,date, Year, Month, day, Location, tmn), tmn_River_Gee_monthly_2015)
rm(tmn_River_Gee_monthly_2015, tmn_River_Gee_monthly_2015a)
tmn_River_Gee_monthly$measurement <- "tmn"
tmn_River_Gee_monthly <- rename(tmn_River_Gee_monthly, Value=tmn)

#Rivercess
tmn_River_Cess_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                              Month=rep(seq(1,12,1), 4), day=1,
                                              tmn1=tmn.var[which(lon==9.25),which(lat==6.25),1:48],
                                              tmn2=tmn.var[which(lon==9.25),which(lat==5.75),1:48],
                                              tmn3=tmn.var[which(lon==9.75),which(lat==5.25),1:48]))
tmn_River_Cess_monthly$tmn <- rowMeans(select(tmn_River_Cess_monthly, tmn1, tmn2, tmn3))
tmn_River_Cess_monthly$Location <- 'River_Cess'
tmn_River_Cess_monthly$date <- ymd(paste(tmn_River_Cess_monthly$Year, tmn_River_Cess_monthly$Month, tmn_River_Cess_monthly$day, sep="-"))
tmn_River_Cess_monthly_2015 <- select(tmn_River_Cess_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_River_Cess_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_River_Cess_monthly_2015a$Location <- 'River_Cess'
tmn_River_Cess_monthly_2015a$date <- ymd(paste(tmn_River_Cess_monthly_2015a$Year, tmn_River_Cess_monthly_2015a$Month, tmn_River_Cess_monthly_2015a$day, sep="-"))
tmn_River_Cess_monthly_2015a <- select(tmn_River_Cess_monthly_2015a, date, Year, Month, day, Location)
tmn_River_Cess_monthly_2015 <- full_join(tmn_River_Cess_monthly_2015a, tmn_River_Cess_monthly_2015)
tmn_River_Cess_monthly <- rbind(select(tmn_River_Cess_monthly,date, Year, Month, day, Location, tmn), tmn_River_Cess_monthly_2015)
rm(tmn_River_Cess_monthly_2015, tmn_River_Cess_monthly_2015a)
tmn_River_Cess_monthly$measurement <- "tmn"
tmn_River_Cess_monthly <- rename(tmn_River_Cess_monthly, Value=tmn)

#Sinoe
tmn_Sinoe_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         tmn1=tmn.var[which(lon==8.75),which(lat==5.75),1:48],
                                         tmn2=tmn.var[which(lon==9.25),which(lat==5.25),1:48],
                                         tmn3=tmn.var[which(lon==8.75),which(lat==5.25),1:48],
                                         tmn4=tmn.var[which(lon==8.75),which(lat==4.75),1:48]))
tmn_Sinoe_monthly$tmn <- rowMeans(select(tmn_Sinoe_monthly, tmn1, tmn2, tmn3, tmn4))
tmn_Sinoe_monthly$Location <- 'Sinoe'
tmn_Sinoe_monthly$date <- ymd(paste(tmn_Sinoe_monthly$Year, tmn_Sinoe_monthly$Month, tmn_Sinoe_monthly$day, sep="-"))
tmn_Sinoe_monthly_2015 <- select(tmn_Sinoe_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Sinoe_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Sinoe_monthly_2015a$Location <- 'Sinoe'
tmn_Sinoe_monthly_2015a$date <- ymd(paste(tmn_Sinoe_monthly_2015a$Year, tmn_Sinoe_monthly_2015a$Month, tmn_Sinoe_monthly_2015a$day, sep="-"))
tmn_Sinoe_monthly_2015a <- select(tmn_Sinoe_monthly_2015a, date, Year, Month, day, Location)
tmn_Sinoe_monthly_2015 <- full_join(tmn_Sinoe_monthly_2015a, tmn_Sinoe_monthly_2015)
tmn_Sinoe_monthly <- rbind(select(tmn_Sinoe_monthly,date, Year, Month, day, Location, tmn), tmn_Sinoe_monthly_2015)
rm(tmn_Sinoe_monthly_2015, tmn_Sinoe_monthly_2015a)
tmn_Sinoe_monthly$measurement <- "tmn"
tmn_Sinoe_monthly <- rename(tmn_Sinoe_monthly, Value=tmn)

#Merging in long format
tmnLiberiamonthly_district <- rbind(tmn_Bomi_monthly, tmn_Bong_monthly, tmn_Gbarpolu_monthly,
                                 tmn_Grand_Bassa_monthly, tmn_Grand_Cape_Mount_monthly, tmn_Grand_Gedeh_monthly,
                                 tmn_Grand_Kru_monthly, tmn_Lofa_monthly, tmn_Margibi_monthly,
                                 tmn_Maryland_monthly, tmn_Montserrado_monthly, tmn_Nimba_monthly,
                                 tmn_River_Gee_monthly, tmn_River_Cess_monthly, tmn_Sinoe_monthly)

#####################
#tmp - mean Temp
#####################
tmp.full <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.tmp.dat.nc', write=FALSE)
tmp.var <- var.get.nc(tmp.full, "tmp")

#Bomi
tmp_Bomi_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmp=tmp.var[which(lon==10.75),which(lat==6.75),1:48]))
tmp_Bomi_monthly$Location <- 'Bomi'
tmp_Bomi_monthly$date <- ymd(paste(tmp_Bomi_monthly$Year, tmp_Bomi_monthly$Month, tmp_Bomi_monthly$day, sep="-"))
tmp_Bomi_monthly_2015 <- select(tmp_Bomi_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Bomi_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Bomi_monthly_2015a$Location <- 'Bomi'
tmp_Bomi_monthly_2015a$date <- ymd(paste(tmp_Bomi_monthly_2015a$Year, tmp_Bomi_monthly_2015a$Month, tmp_Bomi_monthly_2015a$day, sep="-"))
tmp_Bomi_monthly_2015a <- select(tmp_Bomi_monthly_2015a, date, Year, Month, day, Location)
tmp_Bomi_monthly_2015 <- full_join(tmp_Bomi_monthly_2015a, tmp_Bomi_monthly_2015)
tmp_Bomi_monthly <- rbind(select(tmp_Bomi_monthly,date, Year, Month, day, Location, tmp), tmp_Bomi_monthly_2015)
rm(tmp_Bomi_monthly_2015, tmp_Bomi_monthly_2015a)
tmp_Bomi_monthly$measurement <- "tmp"
tmp_Bomi_monthly <- rename(tmp_Bomi_monthly, Value=tmp)

#Bong
tmp_Bong_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmp1=tmp.var[which(lon==9.75),which(lat==7.25),1:48],
                                        tmp2=tmp.var[which(lon==9.25),which(lat==7.25),1:48],
                                        tmp3=tmp.var[which(lon==10.25),which(lat==6.75),1:48],
                                        tmp4=tmp.var[which(lon==9.75),which(lat==6.75),1:48],
                                        tmp5=tmp.var[which(lon==9.25),which(lat==6.75),1:48]))
tmp_Bong_monthly$tmp <- rowMeans(select(tmp_Bong_monthly, tmp1, tmp2, tmp3, tmp4, tmp5))
tmp_Bong_monthly$Location <- 'Bong'
tmp_Bong_monthly$date <- ymd(paste(tmp_Bong_monthly$Year, tmp_Bong_monthly$Month, tmp_Bong_monthly$day, sep="-"))
tmp_Bong_monthly_2015 <- select(tmp_Bong_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Bong_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Bong_monthly_2015a$Location <- 'Bong'
tmp_Bong_monthly_2015a$date <- ymd(paste(tmp_Bong_monthly_2015a$Year, tmp_Bong_monthly_2015a$Month, tmp_Bong_monthly_2015a$day, sep="-"))
tmp_Bong_monthly_2015a <- select(tmp_Bong_monthly_2015a, date, Year, Month, day, Location)
tmp_Bong_monthly_2015 <- full_join(tmp_Bong_monthly_2015a, tmp_Bong_monthly_2015)
tmp_Bong_monthly <- rbind(select(tmp_Bong_monthly,date, Year, Month, day, Location, tmp), tmp_Bong_monthly_2015)
rm(tmp_Bong_monthly_2015, tmp_Bong_monthly_2015a)
tmp_Bong_monthly$measurement <- "tmp"
tmp_Bong_monthly <- rename(tmp_Bong_monthly, Value=tmp)

#Gbarpolu
tmp_Gbarpolu_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            tmp1=tmp.var[which(lon==10.25),which(lat==7.75),1:48],
                                            tmp2=tmp.var[which(lon==10.25),which(lat==7.25),1:48],
                                            tmp3=tmp.var[which(lon==9.75),which(lat==7.25),1:48]))
tmp_Gbarpolu_monthly$tmp <- rowMeans(select(tmp_Gbarpolu_monthly, tmp1, tmp2, tmp3))
tmp_Gbarpolu_monthly$Location <- 'Gbarpolu'
tmp_Gbarpolu_monthly$date <- ymd(paste(tmp_Gbarpolu_monthly$Year, tmp_Gbarpolu_monthly$Month, tmp_Gbarpolu_monthly$day, sep="-"))
tmp_Gbarpolu_monthly_2015 <- select(tmp_Gbarpolu_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Gbarpolu_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Gbarpolu_monthly_2015a$Location <- 'Gbarpolu'
tmp_Gbarpolu_monthly_2015a$date <- ymd(paste(tmp_Gbarpolu_monthly_2015a$Year, tmp_Gbarpolu_monthly_2015a$Month, tmp_Gbarpolu_monthly_2015a$day, sep="-"))
tmp_Gbarpolu_monthly_2015a <- select(tmp_Gbarpolu_monthly_2015a, date, Year, Month, day, Location)
tmp_Gbarpolu_monthly_2015 <- full_join(tmp_Gbarpolu_monthly_2015a, tmp_Gbarpolu_monthly_2015)
tmp_Gbarpolu_monthly <- rbind(select(tmp_Gbarpolu_monthly,date, Year, Month, day, Location, tmp), tmp_Gbarpolu_monthly_2015)
rm(tmp_Gbarpolu_monthly_2015, tmp_Gbarpolu_monthly_2015a)
tmp_Gbarpolu_monthly$measurement <- "tmp"
tmp_Gbarpolu_monthly <- rename(tmp_Gbarpolu_monthly, Value=tmp)

#Grand_Bassa
tmp_Grand_Bassa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               tmp1=tmp.var[which(lon==10.25),which(lat==6.25),1:48],
                                               tmp2=tmp.var[which(lon==9.75),which(lat==6.25),1:48]))
tmp_Grand_Bassa_monthly$tmp <- rowMeans(select(tmp_Grand_Bassa_monthly, tmp1, tmp2))
tmp_Grand_Bassa_monthly$Location <- 'Grand_Bassa'
tmp_Grand_Bassa_monthly$date <- ymd(paste(tmp_Grand_Bassa_monthly$Year, tmp_Grand_Bassa_monthly$Month, tmp_Grand_Bassa_monthly$day, sep="-"))
tmp_Grand_Bassa_monthly_2015 <- select(tmp_Grand_Bassa_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Grand_Bassa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Grand_Bassa_monthly_2015a$Location <- 'Grand_Bassa'
tmp_Grand_Bassa_monthly_2015a$date <- ymd(paste(tmp_Grand_Bassa_monthly_2015a$Year, tmp_Grand_Bassa_monthly_2015a$Month, tmp_Grand_Bassa_monthly_2015a$day, sep="-"))
tmp_Grand_Bassa_monthly_2015a <- select(tmp_Grand_Bassa_monthly_2015a, date, Year, Month, day, Location)
tmp_Grand_Bassa_monthly_2015 <- full_join(tmp_Grand_Bassa_monthly_2015a, tmp_Grand_Bassa_monthly_2015)
tmp_Grand_Bassa_monthly <- rbind(select(tmp_Grand_Bassa_monthly,date, Year, Month, day, Location, tmp), tmp_Grand_Bassa_monthly_2015)
rm(tmp_Grand_Bassa_monthly_2015, tmp_Grand_Bassa_monthly_2015a)
tmp_Grand_Bassa_monthly$measurement <- "tmp"
tmp_Grand_Bassa_monthly <- rename(tmp_Grand_Bassa_monthly, Value=tmp)

#Grand_Cape_Mount
tmp_Grand_Cape_Mount_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                                    Month=rep(seq(1,12,1), 4), day=1,
                                                    tmp1=tmp.var[which(lon==11.25),which(lat==7.25),1:48],
                                                    tmp2=tmp.var[which(lon==10.75),which(lat==7.25),1:48],
                                                    tmp3=tmp.var[which(lon==11.25),which(lat==6.75),1:48]))
tmp_Grand_Cape_Mount_monthly$tmp <- rowMeans(select(tmp_Grand_Cape_Mount_monthly, tmp1, tmp2, tmp3))
tmp_Grand_Cape_Mount_monthly$Location <- 'Grand_Cape_Mount'
tmp_Grand_Cape_Mount_monthly$date <- ymd(paste(tmp_Grand_Cape_Mount_monthly$Year, tmp_Grand_Cape_Mount_monthly$Month, tmp_Grand_Cape_Mount_monthly$day, sep="-"))
tmp_Grand_Cape_Mount_monthly_2015 <- select(tmp_Grand_Cape_Mount_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Grand_Cape_Mount_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Grand_Cape_Mount_monthly_2015a$Location <- 'Grand_Cape_Mount'
tmp_Grand_Cape_Mount_monthly_2015a$date <- ymd(paste(tmp_Grand_Cape_Mount_monthly_2015a$Year, tmp_Grand_Cape_Mount_monthly_2015a$Month, tmp_Grand_Cape_Mount_monthly_2015a$day, sep="-"))
tmp_Grand_Cape_Mount_monthly_2015a <- select(tmp_Grand_Cape_Mount_monthly_2015a, date, Year, Month, day, Location)
tmp_Grand_Cape_Mount_monthly_2015 <- full_join(tmp_Grand_Cape_Mount_monthly_2015a, tmp_Grand_Cape_Mount_monthly_2015)
tmp_Grand_Cape_Mount_monthly <- rbind(select(tmp_Grand_Cape_Mount_monthly,date, Year, Month, day, Location, tmp), tmp_Grand_Cape_Mount_monthly_2015)
rm(tmp_Grand_Cape_Mount_monthly_2015, tmp_Grand_Cape_Mount_monthly_2015a)
tmp_Grand_Cape_Mount_monthly$measurement <- "tmp"
tmp_Grand_Cape_Mount_monthly <- rename(tmp_Grand_Cape_Mount_monthly, Value=tmp)

#Grand_Gedeh
tmp_Grand_Gedeh_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               tmp1=tmp.var[which(lon==8.75),which(lat==6.25),1:48],
                                               tmp2=tmp.var[which(lon==8.25),which(lat==6.25),1:48],
                                               tmp3=tmp.var[which(lon==8.75),which(lat==5.75),1:48],
                                               tmp4=tmp.var[which(lon==8.25),which(lat==5.75),1:48],
                                               tmp5=tmp.var[which(lon==7.75),which(lat==5.75),1:48]))
tmp_Grand_Gedeh_monthly$tmp <- rowMeans(select(tmp_Grand_Gedeh_monthly, tmp1, tmp2, tmp3, tmp4, tmp5))
tmp_Grand_Gedeh_monthly$Location <- 'Grand_Gedeh'
tmp_Grand_Gedeh_monthly$date <- ymd(paste(tmp_Grand_Gedeh_monthly$Year, tmp_Grand_Gedeh_monthly$Month, tmp_Grand_Gedeh_monthly$day, sep="-"))
tmp_Grand_Gedeh_monthly_2015 <- select(tmp_Grand_Gedeh_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Grand_Gedeh_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Grand_Gedeh_monthly_2015a$Location <- 'Grand_Gedeh'
tmp_Grand_Gedeh_monthly_2015a$date <- ymd(paste(tmp_Grand_Gedeh_monthly_2015a$Year, tmp_Grand_Gedeh_monthly_2015a$Month, tmp_Grand_Gedeh_monthly_2015a$day, sep="-"))
tmp_Grand_Gedeh_monthly_2015a <- select(tmp_Grand_Gedeh_monthly_2015a, date, Year, Month, day, Location)
tmp_Grand_Gedeh_monthly_2015 <- full_join(tmp_Grand_Gedeh_monthly_2015a, tmp_Grand_Gedeh_monthly_2015)
tmp_Grand_Gedeh_monthly <- rbind(select(tmp_Grand_Gedeh_monthly,date, Year, Month, day, Location, tmp), tmp_Grand_Gedeh_monthly_2015)
rm(tmp_Grand_Gedeh_monthly_2015, tmp_Grand_Gedeh_monthly_2015a)
tmp_Grand_Gedeh_monthly$measurement <- "tmp"
tmp_Grand_Gedeh_monthly <- rename(tmp_Grand_Gedeh_monthly, Value=tmp)

#Grand_Kru
tmp_Grand_Kru_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             tmp=tmp.var[which(lon==8.25),which(lat==4.75),1:48]))
tmp_Grand_Kru_monthly$Location <- 'Grand_Kru'
tmp_Grand_Kru_monthly$date <- ymd(paste(tmp_Grand_Kru_monthly$Year, tmp_Grand_Kru_monthly$Month, tmp_Grand_Kru_monthly$day, sep="-"))
tmp_Grand_Kru_monthly_2015 <- select(tmp_Grand_Kru_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Grand_Kru_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Grand_Kru_monthly_2015a$Location <- 'Grand_Kru'
tmp_Grand_Kru_monthly_2015a$date <- ymd(paste(tmp_Grand_Kru_monthly_2015a$Year, tmp_Grand_Kru_monthly_2015a$Month, tmp_Grand_Kru_monthly_2015a$day, sep="-"))
tmp_Grand_Kru_monthly_2015a <- select(tmp_Grand_Kru_monthly_2015a, date, Year, Month, day, Location)
tmp_Grand_Kru_monthly_2015 <- full_join(tmp_Grand_Kru_monthly_2015a, tmp_Grand_Kru_monthly_2015)
tmp_Grand_Kru_monthly <- rbind(select(tmp_Grand_Kru_monthly,date, Year, Month, day, Location, tmp), tmp_Grand_Kru_monthly_2015)
rm(tmp_Grand_Kru_monthly_2015, tmp_Grand_Kru_monthly_2015a)
tmp_Grand_Kru_monthly$measurement <- "tmp"
tmp_Grand_Kru_monthly <- rename(tmp_Grand_Kru_monthly, Value=tmp)

#Lofa
tmp_Lofa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmp1=tmp.var[which(lon==10.25),which(lat==8.25),1:48],
                                        tmp2=tmp.var[which(lon==9.75),which(lat==8.25),1:48],
                                        tmp3=tmp.var[which(lon==9.75),which(lat==7.75),1:48]))
tmp_Lofa_monthly$tmp <- rowMeans(select(tmp_Lofa_monthly, tmp1, tmp2, tmp3))
tmp_Lofa_monthly$Location <- 'Lofa'
tmp_Lofa_monthly$date <- ymd(paste(tmp_Lofa_monthly$Year, tmp_Lofa_monthly$Month, tmp_Lofa_monthly$day, sep="-"))
tmp_Lofa_monthly_2015 <- select(tmp_Lofa_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Lofa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Lofa_monthly_2015a$Location <- 'Lofa'
tmp_Lofa_monthly_2015a$date <- ymd(paste(tmp_Lofa_monthly_2015a$Year, tmp_Lofa_monthly_2015a$Month, tmp_Lofa_monthly_2015a$day, sep="-"))
tmp_Lofa_monthly_2015a <- select(tmp_Lofa_monthly_2015a, date, Year, Month, day, Location)
tmp_Lofa_monthly_2015 <- full_join(tmp_Lofa_monthly_2015a, tmp_Lofa_monthly_2015)
tmp_Lofa_monthly <- rbind(select(tmp_Lofa_monthly,date, Year, Month, day, Location, tmp), tmp_Lofa_monthly_2015)
rm(tmp_Lofa_monthly_2015, tmp_Lofa_monthly_2015a)
tmp_Lofa_monthly$measurement <- "tmp"
tmp_Lofa_monthly <- rename(tmp_Lofa_monthly, Value=tmp)

#Margibi
tmp_Margibi_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmp1=tmp.var[which(lon==10.25),which(lat==6.75),1:48],
                                           tmp2=tmp.var[which(lon==10.25),which(lat==6.25),1:48]))
tmp_Margibi_monthly$tmp <- rowMeans(select(tmp_Margibi_monthly, tmp1, tmp2))
tmp_Margibi_monthly$Location <- 'Margibi'
tmp_Margibi_monthly$date <- ymd(paste(tmp_Margibi_monthly$Year, tmp_Margibi_monthly$Month, tmp_Margibi_monthly$day, sep="-"))
tmp_Margibi_monthly_2015 <- select(tmp_Margibi_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Margibi_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Margibi_monthly_2015a$Location <- 'Margibi'
tmp_Margibi_monthly_2015a$date <- ymd(paste(tmp_Margibi_monthly_2015a$Year, tmp_Margibi_monthly_2015a$Month, tmp_Margibi_monthly_2015a$day, sep="-"))
tmp_Margibi_monthly_2015a <- select(tmp_Margibi_monthly_2015a, date, Year, Month, day, Location)
tmp_Margibi_monthly_2015 <- full_join(tmp_Margibi_monthly_2015a, tmp_Margibi_monthly_2015)
tmp_Margibi_monthly <- rbind(select(tmp_Margibi_monthly,date, Year, Month, day, Location, tmp), tmp_Margibi_monthly_2015)
rm(tmp_Margibi_monthly_2015, tmp_Margibi_monthly_2015a)
tmp_Margibi_monthly$measurement <- "tmp"
tmp_Margibi_monthly <- rename(tmp_Margibi_monthly, Value=tmp)

#Maryland
tmp_Maryland_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            tmp=tmp.var[which(lon==7.75),which(lat==4.75),1:48]))
tmp_Maryland_monthly$Location <- 'Maryland'
tmp_Maryland_monthly$date <- ymd(paste(tmp_Maryland_monthly$Year, tmp_Maryland_monthly$Month, tmp_Maryland_monthly$day, sep="-"))
tmp_Maryland_monthly_2015 <- select(tmp_Maryland_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Maryland_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Maryland_monthly_2015a$Location <- 'Maryland'
tmp_Maryland_monthly_2015a$date <- ymd(paste(tmp_Maryland_monthly_2015a$Year, tmp_Maryland_monthly_2015a$Month, tmp_Maryland_monthly_2015a$day, sep="-"))
tmp_Maryland_monthly_2015a <- select(tmp_Maryland_monthly_2015a, date, Year, Month, day, Location)
tmp_Maryland_monthly_2015 <- full_join(tmp_Maryland_monthly_2015a, tmp_Maryland_monthly_2015)
tmp_Maryland_monthly <- rbind(select(tmp_Maryland_monthly,date, Year, Month, day, Location, tmp), tmp_Maryland_monthly_2015)
rm(tmp_Maryland_monthly_2015, tmp_Maryland_monthly_2015a)
tmp_Maryland_monthly$measurement <- "tmp"
tmp_Maryland_monthly <- rename(tmp_Maryland_monthly, Value=tmp)

#Montserrado
tmp_Montserrado_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               tmp=tmp.var[which(lon==10.75),which(lat==6.25),1:48]))
tmp_Montserrado_monthly$Location <- 'Montserrado'
tmp_Montserrado_monthly$date <- ymd(paste(tmp_Montserrado_monthly$Year, tmp_Montserrado_monthly$Month, tmp_Montserrado_monthly$day, sep="-"))
tmp_Montserrado_monthly_2015 <- select(tmp_Montserrado_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Montserrado_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Montserrado_monthly_2015a$Location <- 'Montserrado'
tmp_Montserrado_monthly_2015a$date <- ymd(paste(tmp_Montserrado_monthly_2015a$Year, tmp_Montserrado_monthly_2015a$Month, tmp_Montserrado_monthly_2015a$day, sep="-"))
tmp_Montserrado_monthly_2015a <- select(tmp_Montserrado_monthly_2015a, date, Year, Month, day, Location)
tmp_Montserrado_monthly_2015 <- full_join(tmp_Montserrado_monthly_2015a, tmp_Montserrado_monthly_2015)
tmp_Montserrado_monthly <- rbind(select(tmp_Montserrado_monthly,date, Year, Month, day, Location, tmp), tmp_Montserrado_monthly_2015)
rm(tmp_Montserrado_monthly_2015, tmp_Montserrado_monthly_2015a)
tmp_Montserrado_monthly$measurement <- "tmp"
tmp_Montserrado_monthly <- rename(tmp_Montserrado_monthly, Value=tmp)

#Nimba
tmp_Nimba_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         tmp1=tmp.var[which(lon==8.75),which(lat==7.25),1:48],
                                         tmp2=tmp.var[which(lon==8.75),which(lat==6.75),1:48],
                                         tmp3=tmp.var[which(lon==8.75),which(lat==6.25),1:48]))
tmp_Nimba_monthly$tmp <- rowMeans(select(tmp_Nimba_monthly, tmp1, tmp2, tmp3))
tmp_Nimba_monthly$Location <- 'Nimba'
tmp_Nimba_monthly$date <- ymd(paste(tmp_Nimba_monthly$Year, tmp_Nimba_monthly$Month, tmp_Nimba_monthly$day, sep="-"))
tmp_Nimba_monthly_2015 <- select(tmp_Nimba_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Nimba_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Nimba_monthly_2015a$Location <- 'Nimba'
tmp_Nimba_monthly_2015a$date <- ymd(paste(tmp_Nimba_monthly_2015a$Year, tmp_Nimba_monthly_2015a$Month, tmp_Nimba_monthly_2015a$day, sep="-"))
tmp_Nimba_monthly_2015a <- select(tmp_Nimba_monthly_2015a, date, Year, Month, day, Location)
tmp_Nimba_monthly_2015 <- full_join(tmp_Nimba_monthly_2015a, tmp_Nimba_monthly_2015)
tmp_Nimba_monthly <- rbind(select(tmp_Nimba_monthly,date, Year, Month, day, Location, tmp), tmp_Nimba_monthly_2015)
rm(tmp_Nimba_monthly_2015, tmp_Nimba_monthly_2015a)
tmp_Nimba_monthly$measurement <- "tmp"
tmp_Nimba_monthly <- rename(tmp_Nimba_monthly, Value=tmp)

#River_Gee
tmp_River_Gee_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             tmp1=tmp.var[which(lon==8.25),which(lat==5.25),1:48],
                                             tmp2=tmp.var[which(lon==7.75),which(lat==5.25),1:48]))
tmp_River_Gee_monthly$tmp <- rowMeans(select(tmp_River_Gee_monthly, tmp1, tmp2))
tmp_River_Gee_monthly$Location <- 'River_Gee'
tmp_River_Gee_monthly$date <- ymd(paste(tmp_River_Gee_monthly$Year, tmp_River_Gee_monthly$Month, tmp_River_Gee_monthly$day, sep="-"))
tmp_River_Gee_monthly_2015 <- select(tmp_River_Gee_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_River_Gee_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_River_Gee_monthly_2015a$Location <- 'River_Gee'
tmp_River_Gee_monthly_2015a$date <- ymd(paste(tmp_River_Gee_monthly_2015a$Year, tmp_River_Gee_monthly_2015a$Month, tmp_River_Gee_monthly_2015a$day, sep="-"))
tmp_River_Gee_monthly_2015a <- select(tmp_River_Gee_monthly_2015a, date, Year, Month, day, Location)
tmp_River_Gee_monthly_2015 <- full_join(tmp_River_Gee_monthly_2015a, tmp_River_Gee_monthly_2015)
tmp_River_Gee_monthly <- rbind(select(tmp_River_Gee_monthly,date, Year, Month, day, Location, tmp), tmp_River_Gee_monthly_2015)
rm(tmp_River_Gee_monthly_2015, tmp_River_Gee_monthly_2015a)
tmp_River_Gee_monthly$measurement <- "tmp"
tmp_River_Gee_monthly <- rename(tmp_River_Gee_monthly, Value=tmp)

#Rivercess
tmp_River_Cess_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                              Month=rep(seq(1,12,1), 4), day=1,
                                              tmp1=tmp.var[which(lon==9.25),which(lat==6.25),1:48],
                                              tmp2=tmp.var[which(lon==9.25),which(lat==5.75),1:48],
                                              tmp3=tmp.var[which(lon==9.75),which(lat==5.25),1:48]))
tmp_River_Cess_monthly$tmp <- rowMeans(select(tmp_River_Cess_monthly, tmp1, tmp2, tmp3))
tmp_River_Cess_monthly$Location <- 'River_Cess'
tmp_River_Cess_monthly$date <- ymd(paste(tmp_River_Cess_monthly$Year, tmp_River_Cess_monthly$Month, tmp_River_Cess_monthly$day, sep="-"))
tmp_River_Cess_monthly_2015 <- select(tmp_River_Cess_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_River_Cess_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_River_Cess_monthly_2015a$Location <- 'River_Cess'
tmp_River_Cess_monthly_2015a$date <- ymd(paste(tmp_River_Cess_monthly_2015a$Year, tmp_River_Cess_monthly_2015a$Month, tmp_River_Cess_monthly_2015a$day, sep="-"))
tmp_River_Cess_monthly_2015a <- select(tmp_River_Cess_monthly_2015a, date, Year, Month, day, Location)
tmp_River_Cess_monthly_2015 <- full_join(tmp_River_Cess_monthly_2015a, tmp_River_Cess_monthly_2015)
tmp_River_Cess_monthly <- rbind(select(tmp_River_Cess_monthly,date, Year, Month, day, Location, tmp), tmp_River_Cess_monthly_2015)
rm(tmp_River_Cess_monthly_2015, tmp_River_Cess_monthly_2015a)
tmp_River_Cess_monthly$measurement <- "tmp"
tmp_River_Cess_monthly <- rename(tmp_River_Cess_monthly, Value=tmp)

#Sinoe
tmp_Sinoe_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         tmp1=tmp.var[which(lon==8.75),which(lat==5.75),1:48],
                                         tmp2=tmp.var[which(lon==9.25),which(lat==5.25),1:48],
                                         tmp3=tmp.var[which(lon==8.75),which(lat==5.25),1:48],
                                         tmp4=tmp.var[which(lon==8.75),which(lat==4.75),1:48]))
tmp_Sinoe_monthly$tmp <- rowMeans(select(tmp_Sinoe_monthly, tmp1, tmp2, tmp3, tmp4))
tmp_Sinoe_monthly$Location <- 'Sinoe'
tmp_Sinoe_monthly$date <- ymd(paste(tmp_Sinoe_monthly$Year, tmp_Sinoe_monthly$Month, tmp_Sinoe_monthly$day, sep="-"))
tmp_Sinoe_monthly_2015 <- select(tmp_Sinoe_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Sinoe_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Sinoe_monthly_2015a$Location <- 'Sinoe'
tmp_Sinoe_monthly_2015a$date <- ymd(paste(tmp_Sinoe_monthly_2015a$Year, tmp_Sinoe_monthly_2015a$Month, tmp_Sinoe_monthly_2015a$day, sep="-"))
tmp_Sinoe_monthly_2015a <- select(tmp_Sinoe_monthly_2015a, date, Year, Month, day, Location)
tmp_Sinoe_monthly_2015 <- full_join(tmp_Sinoe_monthly_2015a, tmp_Sinoe_monthly_2015)
tmp_Sinoe_monthly <- rbind(select(tmp_Sinoe_monthly,date, Year, Month, day, Location, tmp), tmp_Sinoe_monthly_2015)
rm(tmp_Sinoe_monthly_2015, tmp_Sinoe_monthly_2015a)
tmp_Sinoe_monthly$measurement <- "tmp"
tmp_Sinoe_monthly <- rename(tmp_Sinoe_monthly, Value=tmp)

#Merging in long format
tmpLiberiamonthly_district <- rbind(tmp_Bomi_monthly, tmp_Bong_monthly, tmp_Gbarpolu_monthly,
                                 tmp_Grand_Bassa_monthly, tmp_Grand_Cape_Mount_monthly, tmp_Grand_Gedeh_monthly,
                                 tmp_Grand_Kru_monthly, tmp_Lofa_monthly, tmp_Margibi_monthly,
                                 tmp_Maryland_monthly, tmp_Montserrado_monthly, tmp_Nimba_monthly,
                                 tmp_River_Gee_monthly, tmp_River_Cess_monthly, tmp_Sinoe_monthly)

#####################
#tmx - max Temp
#####################
tmx.full <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.tmx.dat.nc', write=FALSE)
tmx.var <- var.get.nc(tmx.full, "tmx")

#Bomi
tmx_Bomi_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmx=tmx.var[which(lon==10.75),which(lat==6.75),1:48]))
tmx_Bomi_monthly$Location <- 'Bomi'
tmx_Bomi_monthly$date <- ymd(paste(tmx_Bomi_monthly$Year, tmx_Bomi_monthly$Month, tmx_Bomi_monthly$day, sep="-"))
tmx_Bomi_monthly_2015 <- select(tmx_Bomi_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Bomi_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Bomi_monthly_2015a$Location <- 'Bomi'
tmx_Bomi_monthly_2015a$date <- ymd(paste(tmx_Bomi_monthly_2015a$Year, tmx_Bomi_monthly_2015a$Month, tmx_Bomi_monthly_2015a$day, sep="-"))
tmx_Bomi_monthly_2015a <- select(tmx_Bomi_monthly_2015a, date, Year, Month, day, Location)
tmx_Bomi_monthly_2015 <- full_join(tmx_Bomi_monthly_2015a, tmx_Bomi_monthly_2015)
tmx_Bomi_monthly <- rbind(select(tmx_Bomi_monthly,date, Year, Month, day, Location, tmx), tmx_Bomi_monthly_2015)
rm(tmx_Bomi_monthly_2015, tmx_Bomi_monthly_2015a)
tmx_Bomi_monthly$measurement <- "tmx"
tmx_Bomi_monthly <- rename(tmx_Bomi_monthly, Value=tmx)

#Bong
tmx_Bong_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmx1=tmx.var[which(lon==9.75),which(lat==7.25),1:48],
                                        tmx2=tmx.var[which(lon==9.25),which(lat==7.25),1:48],
                                        tmx3=tmx.var[which(lon==10.25),which(lat==6.75),1:48],
                                        tmx4=tmx.var[which(lon==9.75),which(lat==6.75),1:48],
                                        tmx5=tmx.var[which(lon==9.25),which(lat==6.75),1:48]))
tmx_Bong_monthly$tmx <- rowMeans(select(tmx_Bong_monthly, tmx1, tmx2, tmx3, tmx4, tmx5))
tmx_Bong_monthly$Location <- 'Bong'
tmx_Bong_monthly$date <- ymd(paste(tmx_Bong_monthly$Year, tmx_Bong_monthly$Month, tmx_Bong_monthly$day, sep="-"))
tmx_Bong_monthly_2015 <- select(tmx_Bong_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Bong_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Bong_monthly_2015a$Location <- 'Bong'
tmx_Bong_monthly_2015a$date <- ymd(paste(tmx_Bong_monthly_2015a$Year, tmx_Bong_monthly_2015a$Month, tmx_Bong_monthly_2015a$day, sep="-"))
tmx_Bong_monthly_2015a <- select(tmx_Bong_monthly_2015a, date, Year, Month, day, Location)
tmx_Bong_monthly_2015 <- full_join(tmx_Bong_monthly_2015a, tmx_Bong_monthly_2015)
tmx_Bong_monthly <- rbind(select(tmx_Bong_monthly,date, Year, Month, day, Location, tmx), tmx_Bong_monthly_2015)
rm(tmx_Bong_monthly_2015, tmx_Bong_monthly_2015a)
tmx_Bong_monthly$measurement <- "tmx"
tmx_Bong_monthly <- rename(tmx_Bong_monthly, Value=tmx)

#Gbarpolu
tmx_Gbarpolu_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            tmx1=tmx.var[which(lon==10.25),which(lat==7.75),1:48],
                                            tmx2=tmx.var[which(lon==10.25),which(lat==7.25),1:48],
                                            tmx3=tmx.var[which(lon==9.75),which(lat==7.25),1:48]))
tmx_Gbarpolu_monthly$tmx <- rowMeans(select(tmx_Gbarpolu_monthly, tmx1, tmx2, tmx3))
tmx_Gbarpolu_monthly$Location <- 'Gbarpolu'
tmx_Gbarpolu_monthly$date <- ymd(paste(tmx_Gbarpolu_monthly$Year, tmx_Gbarpolu_monthly$Month, tmx_Gbarpolu_monthly$day, sep="-"))
tmx_Gbarpolu_monthly_2015 <- select(tmx_Gbarpolu_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Gbarpolu_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Gbarpolu_monthly_2015a$Location <- 'Gbarpolu'
tmx_Gbarpolu_monthly_2015a$date <- ymd(paste(tmx_Gbarpolu_monthly_2015a$Year, tmx_Gbarpolu_monthly_2015a$Month, tmx_Gbarpolu_monthly_2015a$day, sep="-"))
tmx_Gbarpolu_monthly_2015a <- select(tmx_Gbarpolu_monthly_2015a, date, Year, Month, day, Location)
tmx_Gbarpolu_monthly_2015 <- full_join(tmx_Gbarpolu_monthly_2015a, tmx_Gbarpolu_monthly_2015)
tmx_Gbarpolu_monthly <- rbind(select(tmx_Gbarpolu_monthly,date, Year, Month, day, Location, tmx), tmx_Gbarpolu_monthly_2015)
rm(tmx_Gbarpolu_monthly_2015, tmx_Gbarpolu_monthly_2015a)
tmx_Gbarpolu_monthly$measurement <- "tmx"
tmx_Gbarpolu_monthly <- rename(tmx_Gbarpolu_monthly, Value=tmx)

#Grand_Bassa
tmx_Grand_Bassa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               tmx1=tmx.var[which(lon==10.25),which(lat==6.25),1:48],
                                               tmx2=tmx.var[which(lon==9.75),which(lat==6.25),1:48]))
tmx_Grand_Bassa_monthly$tmx <- rowMeans(select(tmx_Grand_Bassa_monthly, tmx1, tmx2))
tmx_Grand_Bassa_monthly$Location <- 'Grand_Bassa'
tmx_Grand_Bassa_monthly$date <- ymd(paste(tmx_Grand_Bassa_monthly$Year, tmx_Grand_Bassa_monthly$Month, tmx_Grand_Bassa_monthly$day, sep="-"))
tmx_Grand_Bassa_monthly_2015 <- select(tmx_Grand_Bassa_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Grand_Bassa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Grand_Bassa_monthly_2015a$Location <- 'Grand_Bassa'
tmx_Grand_Bassa_monthly_2015a$date <- ymd(paste(tmx_Grand_Bassa_monthly_2015a$Year, tmx_Grand_Bassa_monthly_2015a$Month, tmx_Grand_Bassa_monthly_2015a$day, sep="-"))
tmx_Grand_Bassa_monthly_2015a <- select(tmx_Grand_Bassa_monthly_2015a, date, Year, Month, day, Location)
tmx_Grand_Bassa_monthly_2015 <- full_join(tmx_Grand_Bassa_monthly_2015a, tmx_Grand_Bassa_monthly_2015)
tmx_Grand_Bassa_monthly <- rbind(select(tmx_Grand_Bassa_monthly,date, Year, Month, day, Location, tmx), tmx_Grand_Bassa_monthly_2015)
rm(tmx_Grand_Bassa_monthly_2015, tmx_Grand_Bassa_monthly_2015a)
tmx_Grand_Bassa_monthly$measurement <- "tmx"
tmx_Grand_Bassa_monthly <- rename(tmx_Grand_Bassa_monthly, Value=tmx)

#Grand_Cape_Mount
tmx_Grand_Cape_Mount_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                                    Month=rep(seq(1,12,1), 4), day=1,
                                                    tmx1=tmx.var[which(lon==11.25),which(lat==7.25),1:48],
                                                    tmx2=tmx.var[which(lon==10.75),which(lat==7.25),1:48],
                                                    tmx3=tmx.var[which(lon==11.25),which(lat==6.75),1:48]))
tmx_Grand_Cape_Mount_monthly$tmx <- rowMeans(select(tmx_Grand_Cape_Mount_monthly, tmx1, tmx2, tmx3))
tmx_Grand_Cape_Mount_monthly$Location <- 'Grand_Cape_Mount'
tmx_Grand_Cape_Mount_monthly$date <- ymd(paste(tmx_Grand_Cape_Mount_monthly$Year, tmx_Grand_Cape_Mount_monthly$Month, tmx_Grand_Cape_Mount_monthly$day, sep="-"))
tmx_Grand_Cape_Mount_monthly_2015 <- select(tmx_Grand_Cape_Mount_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Grand_Cape_Mount_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Grand_Cape_Mount_monthly_2015a$Location <- 'Grand_Cape_Mount'
tmx_Grand_Cape_Mount_monthly_2015a$date <- ymd(paste(tmx_Grand_Cape_Mount_monthly_2015a$Year, tmx_Grand_Cape_Mount_monthly_2015a$Month, tmx_Grand_Cape_Mount_monthly_2015a$day, sep="-"))
tmx_Grand_Cape_Mount_monthly_2015a <- select(tmx_Grand_Cape_Mount_monthly_2015a, date, Year, Month, day, Location)
tmx_Grand_Cape_Mount_monthly_2015 <- full_join(tmx_Grand_Cape_Mount_monthly_2015a, tmx_Grand_Cape_Mount_monthly_2015)
tmx_Grand_Cape_Mount_monthly <- rbind(select(tmx_Grand_Cape_Mount_monthly,date, Year, Month, day, Location, tmx), tmx_Grand_Cape_Mount_monthly_2015)
rm(tmx_Grand_Cape_Mount_monthly_2015, tmx_Grand_Cape_Mount_monthly_2015a)
tmx_Grand_Cape_Mount_monthly$measurement <- "tmx"
tmx_Grand_Cape_Mount_monthly <- rename(tmx_Grand_Cape_Mount_monthly, Value=tmx)

#Grand_Gedeh
tmx_Grand_Gedeh_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               tmx1=tmx.var[which(lon==8.75),which(lat==6.25),1:48],
                                               tmx2=tmx.var[which(lon==8.25),which(lat==6.25),1:48],
                                               tmx3=tmx.var[which(lon==8.75),which(lat==5.75),1:48],
                                               tmx4=tmx.var[which(lon==8.25),which(lat==5.75),1:48],
                                               tmx5=tmx.var[which(lon==7.75),which(lat==5.75),1:48]))
tmx_Grand_Gedeh_monthly$tmx <- rowMeans(select(tmx_Grand_Gedeh_monthly, tmx1, tmx2, tmx3, tmx4, tmx5))
tmx_Grand_Gedeh_monthly$Location <- 'Grand_Gedeh'
tmx_Grand_Gedeh_monthly$date <- ymd(paste(tmx_Grand_Gedeh_monthly$Year, tmx_Grand_Gedeh_monthly$Month, tmx_Grand_Gedeh_monthly$day, sep="-"))
tmx_Grand_Gedeh_monthly_2015 <- select(tmx_Grand_Gedeh_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Grand_Gedeh_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Grand_Gedeh_monthly_2015a$Location <- 'Grand_Gedeh'
tmx_Grand_Gedeh_monthly_2015a$date <- ymd(paste(tmx_Grand_Gedeh_monthly_2015a$Year, tmx_Grand_Gedeh_monthly_2015a$Month, tmx_Grand_Gedeh_monthly_2015a$day, sep="-"))
tmx_Grand_Gedeh_monthly_2015a <- select(tmx_Grand_Gedeh_monthly_2015a, date, Year, Month, day, Location)
tmx_Grand_Gedeh_monthly_2015 <- full_join(tmx_Grand_Gedeh_monthly_2015a, tmx_Grand_Gedeh_monthly_2015)
tmx_Grand_Gedeh_monthly <- rbind(select(tmx_Grand_Gedeh_monthly,date, Year, Month, day, Location, tmx), tmx_Grand_Gedeh_monthly_2015)
rm(tmx_Grand_Gedeh_monthly_2015, tmx_Grand_Gedeh_monthly_2015a)
tmx_Grand_Gedeh_monthly$measurement <- "tmx"
tmx_Grand_Gedeh_monthly <- rename(tmx_Grand_Gedeh_monthly, Value=tmx)

#Grand_Kru
tmx_Grand_Kru_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             tmx=tmx.var[which(lon==8.25),which(lat==4.75),1:48]))
tmx_Grand_Kru_monthly$Location <- 'Grand_Kru'
tmx_Grand_Kru_monthly$date <- ymd(paste(tmx_Grand_Kru_monthly$Year, tmx_Grand_Kru_monthly$Month, tmx_Grand_Kru_monthly$day, sep="-"))
tmx_Grand_Kru_monthly_2015 <- select(tmx_Grand_Kru_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Grand_Kru_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Grand_Kru_monthly_2015a$Location <- 'Grand_Kru'
tmx_Grand_Kru_monthly_2015a$date <- ymd(paste(tmx_Grand_Kru_monthly_2015a$Year, tmx_Grand_Kru_monthly_2015a$Month, tmx_Grand_Kru_monthly_2015a$day, sep="-"))
tmx_Grand_Kru_monthly_2015a <- select(tmx_Grand_Kru_monthly_2015a, date, Year, Month, day, Location)
tmx_Grand_Kru_monthly_2015 <- full_join(tmx_Grand_Kru_monthly_2015a, tmx_Grand_Kru_monthly_2015)
tmx_Grand_Kru_monthly <- rbind(select(tmx_Grand_Kru_monthly,date, Year, Month, day, Location, tmx), tmx_Grand_Kru_monthly_2015)
rm(tmx_Grand_Kru_monthly_2015, tmx_Grand_Kru_monthly_2015a)
tmx_Grand_Kru_monthly$measurement <- "tmx"
tmx_Grand_Kru_monthly <- rename(tmx_Grand_Kru_monthly, Value=tmx)

#Lofa
tmx_Lofa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmx1=tmx.var[which(lon==10.25),which(lat==8.25),1:48],
                                        tmx2=tmx.var[which(lon==9.75),which(lat==8.25),1:48],
                                        tmx3=tmx.var[which(lon==9.75),which(lat==7.75),1:48]))
tmx_Lofa_monthly$tmx <- rowMeans(select(tmx_Lofa_monthly, tmx1, tmx2, tmx3))
tmx_Lofa_monthly$Location <- 'Lofa'
tmx_Lofa_monthly$date <- ymd(paste(tmx_Lofa_monthly$Year, tmx_Lofa_monthly$Month, tmx_Lofa_monthly$day, sep="-"))
tmx_Lofa_monthly_2015 <- select(tmx_Lofa_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Lofa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Lofa_monthly_2015a$Location <- 'Lofa'
tmx_Lofa_monthly_2015a$date <- ymd(paste(tmx_Lofa_monthly_2015a$Year, tmx_Lofa_monthly_2015a$Month, tmx_Lofa_monthly_2015a$day, sep="-"))
tmx_Lofa_monthly_2015a <- select(tmx_Lofa_monthly_2015a, date, Year, Month, day, Location)
tmx_Lofa_monthly_2015 <- full_join(tmx_Lofa_monthly_2015a, tmx_Lofa_monthly_2015)
tmx_Lofa_monthly <- rbind(select(tmx_Lofa_monthly,date, Year, Month, day, Location, tmx), tmx_Lofa_monthly_2015)
rm(tmx_Lofa_monthly_2015, tmx_Lofa_monthly_2015a)
tmx_Lofa_monthly$measurement <- "tmx"
tmx_Lofa_monthly <- rename(tmx_Lofa_monthly, Value=tmx)

#Margibi
tmx_Margibi_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmx1=tmx.var[which(lon==10.25),which(lat==6.75),1:48],
                                           tmx2=tmx.var[which(lon==10.25),which(lat==6.25),1:48]))
tmx_Margibi_monthly$tmx <- rowMeans(select(tmx_Margibi_monthly, tmx1, tmx2))
tmx_Margibi_monthly$Location <- 'Margibi'
tmx_Margibi_monthly$date <- ymd(paste(tmx_Margibi_monthly$Year, tmx_Margibi_monthly$Month, tmx_Margibi_monthly$day, sep="-"))
tmx_Margibi_monthly_2015 <- select(tmx_Margibi_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Margibi_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Margibi_monthly_2015a$Location <- 'Margibi'
tmx_Margibi_monthly_2015a$date <- ymd(paste(tmx_Margibi_monthly_2015a$Year, tmx_Margibi_monthly_2015a$Month, tmx_Margibi_monthly_2015a$day, sep="-"))
tmx_Margibi_monthly_2015a <- select(tmx_Margibi_monthly_2015a, date, Year, Month, day, Location)
tmx_Margibi_monthly_2015 <- full_join(tmx_Margibi_monthly_2015a, tmx_Margibi_monthly_2015)
tmx_Margibi_monthly <- rbind(select(tmx_Margibi_monthly,date, Year, Month, day, Location, tmx), tmx_Margibi_monthly_2015)
rm(tmx_Margibi_monthly_2015, tmx_Margibi_monthly_2015a)
tmx_Margibi_monthly$measurement <- "tmx"
tmx_Margibi_monthly <- rename(tmx_Margibi_monthly, Value=tmx)

#Maryland
tmx_Maryland_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            tmx=tmx.var[which(lon==7.75),which(lat==4.75),1:48]))
tmx_Maryland_monthly$Location <- 'Maryland'
tmx_Maryland_monthly$date <- ymd(paste(tmx_Maryland_monthly$Year, tmx_Maryland_monthly$Month, tmx_Maryland_monthly$day, sep="-"))
tmx_Maryland_monthly_2015 <- select(tmx_Maryland_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Maryland_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Maryland_monthly_2015a$Location <- 'Maryland'
tmx_Maryland_monthly_2015a$date <- ymd(paste(tmx_Maryland_monthly_2015a$Year, tmx_Maryland_monthly_2015a$Month, tmx_Maryland_monthly_2015a$day, sep="-"))
tmx_Maryland_monthly_2015a <- select(tmx_Maryland_monthly_2015a, date, Year, Month, day, Location)
tmx_Maryland_monthly_2015 <- full_join(tmx_Maryland_monthly_2015a, tmx_Maryland_monthly_2015)
tmx_Maryland_monthly <- rbind(select(tmx_Maryland_monthly,date, Year, Month, day, Location, tmx), tmx_Maryland_monthly_2015)
rm(tmx_Maryland_monthly_2015, tmx_Maryland_monthly_2015a)
tmx_Maryland_monthly$measurement <- "tmx"
tmx_Maryland_monthly <- rename(tmx_Maryland_monthly, Value=tmx)

#Montserrado
tmx_Montserrado_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               tmx=tmx.var[which(lon==10.75),which(lat==6.25),1:48]))
tmx_Montserrado_monthly$Location <- 'Montserrado'
tmx_Montserrado_monthly$date <- ymd(paste(tmx_Montserrado_monthly$Year, tmx_Montserrado_monthly$Month, tmx_Montserrado_monthly$day, sep="-"))
tmx_Montserrado_monthly_2015 <- select(tmx_Montserrado_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Montserrado_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Montserrado_monthly_2015a$Location <- 'Montserrado'
tmx_Montserrado_monthly_2015a$date <- ymd(paste(tmx_Montserrado_monthly_2015a$Year, tmx_Montserrado_monthly_2015a$Month, tmx_Montserrado_monthly_2015a$day, sep="-"))
tmx_Montserrado_monthly_2015a <- select(tmx_Montserrado_monthly_2015a, date, Year, Month, day, Location)
tmx_Montserrado_monthly_2015 <- full_join(tmx_Montserrado_monthly_2015a, tmx_Montserrado_monthly_2015)
tmx_Montserrado_monthly <- rbind(select(tmx_Montserrado_monthly,date, Year, Month, day, Location, tmx), tmx_Montserrado_monthly_2015)
rm(tmx_Montserrado_monthly_2015, tmx_Montserrado_monthly_2015a)
tmx_Montserrado_monthly$measurement <- "tmx"
tmx_Montserrado_monthly <- rename(tmx_Montserrado_monthly, Value=tmx)

#Nimba
tmx_Nimba_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         tmx1=tmx.var[which(lon==8.75),which(lat==7.25),1:48],
                                         tmx2=tmx.var[which(lon==8.75),which(lat==6.75),1:48],
                                         tmx3=tmx.var[which(lon==8.75),which(lat==6.25),1:48]))
tmx_Nimba_monthly$tmx <- rowMeans(select(tmx_Nimba_monthly, tmx1, tmx2, tmx3))
tmx_Nimba_monthly$Location <- 'Nimba'
tmx_Nimba_monthly$date <- ymd(paste(tmx_Nimba_monthly$Year, tmx_Nimba_monthly$Month, tmx_Nimba_monthly$day, sep="-"))
tmx_Nimba_monthly_2015 <- select(tmx_Nimba_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Nimba_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Nimba_monthly_2015a$Location <- 'Nimba'
tmx_Nimba_monthly_2015a$date <- ymd(paste(tmx_Nimba_monthly_2015a$Year, tmx_Nimba_monthly_2015a$Month, tmx_Nimba_monthly_2015a$day, sep="-"))
tmx_Nimba_monthly_2015a <- select(tmx_Nimba_monthly_2015a, date, Year, Month, day, Location)
tmx_Nimba_monthly_2015 <- full_join(tmx_Nimba_monthly_2015a, tmx_Nimba_monthly_2015)
tmx_Nimba_monthly <- rbind(select(tmx_Nimba_monthly,date, Year, Month, day, Location, tmx), tmx_Nimba_monthly_2015)
rm(tmx_Nimba_monthly_2015, tmx_Nimba_monthly_2015a)
tmx_Nimba_monthly$measurement <- "tmx"
tmx_Nimba_monthly <- rename(tmx_Nimba_monthly, Value=tmx)

#River_Gee
tmx_River_Gee_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             tmx1=tmx.var[which(lon==8.25),which(lat==5.25),1:48],
                                             tmx2=tmx.var[which(lon==7.75),which(lat==5.25),1:48]))
tmx_River_Gee_monthly$tmx <- rowMeans(select(tmx_River_Gee_monthly, tmx1, tmx2))
tmx_River_Gee_monthly$Location <- 'River_Gee'
tmx_River_Gee_monthly$date <- ymd(paste(tmx_River_Gee_monthly$Year, tmx_River_Gee_monthly$Month, tmx_River_Gee_monthly$day, sep="-"))
tmx_River_Gee_monthly_2015 <- select(tmx_River_Gee_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_River_Gee_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_River_Gee_monthly_2015a$Location <- 'River_Gee'
tmx_River_Gee_monthly_2015a$date <- ymd(paste(tmx_River_Gee_monthly_2015a$Year, tmx_River_Gee_monthly_2015a$Month, tmx_River_Gee_monthly_2015a$day, sep="-"))
tmx_River_Gee_monthly_2015a <- select(tmx_River_Gee_monthly_2015a, date, Year, Month, day, Location)
tmx_River_Gee_monthly_2015 <- full_join(tmx_River_Gee_monthly_2015a, tmx_River_Gee_monthly_2015)
tmx_River_Gee_monthly <- rbind(select(tmx_River_Gee_monthly,date, Year, Month, day, Location, tmx), tmx_River_Gee_monthly_2015)
rm(tmx_River_Gee_monthly_2015, tmx_River_Gee_monthly_2015a)
tmx_River_Gee_monthly$measurement <- "tmx"
tmx_River_Gee_monthly <- rename(tmx_River_Gee_monthly, Value=tmx)

#Rivercess
tmx_River_Cess_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                              Month=rep(seq(1,12,1), 4), day=1,
                                              tmx1=tmx.var[which(lon==9.25),which(lat==6.25),1:48],
                                              tmx2=tmx.var[which(lon==9.25),which(lat==5.75),1:48],
                                              tmx3=tmx.var[which(lon==9.75),which(lat==5.25),1:48]))
tmx_River_Cess_monthly$tmx <- rowMeans(select(tmx_River_Cess_monthly, tmx1, tmx2, tmx3))
tmx_River_Cess_monthly$Location <- 'River_Cess'
tmx_River_Cess_monthly$date <- ymd(paste(tmx_River_Cess_monthly$Year, tmx_River_Cess_monthly$Month, tmx_River_Cess_monthly$day, sep="-"))
tmx_River_Cess_monthly_2015 <- select(tmx_River_Cess_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_River_Cess_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_River_Cess_monthly_2015a$Location <- 'River_Cess'
tmx_River_Cess_monthly_2015a$date <- ymd(paste(tmx_River_Cess_monthly_2015a$Year, tmx_River_Cess_monthly_2015a$Month, tmx_River_Cess_monthly_2015a$day, sep="-"))
tmx_River_Cess_monthly_2015a <- select(tmx_River_Cess_monthly_2015a, date, Year, Month, day, Location)
tmx_River_Cess_monthly_2015 <- full_join(tmx_River_Cess_monthly_2015a, tmx_River_Cess_monthly_2015)
tmx_River_Cess_monthly <- rbind(select(tmx_River_Cess_monthly,date, Year, Month, day, Location, tmx), tmx_River_Cess_monthly_2015)
rm(tmx_River_Cess_monthly_2015, tmx_River_Cess_monthly_2015a)
tmx_River_Cess_monthly$measurement <- "tmx"
tmx_River_Cess_monthly <- rename(tmx_River_Cess_monthly, Value=tmx)

#Sinoe
tmx_Sinoe_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         tmx1=tmx.var[which(lon==8.75),which(lat==5.75),1:48],
                                         tmx2=tmx.var[which(lon==9.25),which(lat==5.25),1:48],
                                         tmx3=tmx.var[which(lon==8.75),which(lat==5.25),1:48],
                                         tmx4=tmx.var[which(lon==8.75),which(lat==4.75),1:48]))
tmx_Sinoe_monthly$tmx <- rowMeans(select(tmx_Sinoe_monthly, tmx1, tmx2, tmx3, tmx4))
tmx_Sinoe_monthly$Location <- 'Sinoe'
tmx_Sinoe_monthly$date <- ymd(paste(tmx_Sinoe_monthly$Year, tmx_Sinoe_monthly$Month, tmx_Sinoe_monthly$day, sep="-"))
tmx_Sinoe_monthly_2015 <- select(tmx_Sinoe_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Sinoe_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Sinoe_monthly_2015a$Location <- 'Sinoe'
tmx_Sinoe_monthly_2015a$date <- ymd(paste(tmx_Sinoe_monthly_2015a$Year, tmx_Sinoe_monthly_2015a$Month, tmx_Sinoe_monthly_2015a$day, sep="-"))
tmx_Sinoe_monthly_2015a <- select(tmx_Sinoe_monthly_2015a, date, Year, Month, day, Location)
tmx_Sinoe_monthly_2015 <- full_join(tmx_Sinoe_monthly_2015a, tmx_Sinoe_monthly_2015)
tmx_Sinoe_monthly <- rbind(select(tmx_Sinoe_monthly,date, Year, Month, day, Location, tmx), tmx_Sinoe_monthly_2015)
rm(tmx_Sinoe_monthly_2015, tmx_Sinoe_monthly_2015a)
tmx_Sinoe_monthly$measurement <- "tmx"
tmx_Sinoe_monthly <- rename(tmx_Sinoe_monthly, Value=tmx)

#Merging in long format
tmxLiberiamonthly_district <- rbind(tmx_Bomi_monthly, tmx_Bong_monthly, tmx_Gbarpolu_monthly,
                                 tmx_Grand_Bassa_monthly, tmx_Grand_Cape_Mount_monthly, tmx_Grand_Gedeh_monthly,
                                 tmx_Grand_Kru_monthly, tmx_Lofa_monthly, tmx_Margibi_monthly,
                                 tmx_Maryland_monthly, tmx_Montserrado_monthly, tmx_Nimba_monthly,
                                 tmx_River_Gee_monthly, tmx_River_Cess_monthly, tmx_Sinoe_monthly)

Liberia_monthly_district <- rbind(VapLiberiamonthly_district, wetLiberiamonthly_district, petLiberiamonthly_district,
                             dtrLiberiamonthly_district, preLiberiamonthly_district, tmnLiberiamonthly_district,
                             tmpLiberiamonthly_district, tmxLiberiamonthly_district)
write.csv(Liberia_monthly_district, file='Liberia_monthly_district.csv')

# use interpolation to get the weekly climate parameters
func_spline <- function(climatefactor,steps=12/52){
    sp<- splinefun(x=seq(1,length(climatefactor),1), y=climatefactor, method="fmm",  ties = mean)
    out <- sp(seq(1,length(climatefactor),steps))
    return(out)
    
}
Liberia_monthly_district <- read.csv('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/Liberia_monthly_district.csv')
Liberia_monthly_vap <- filter(Liberia_monthly_district, measurement=='Vap' & Year>='2013') %>% select(-X)
Liberia_monthly_wet <- filter(Liberia_monthly_district, measurement=='wet' & Year>='2013') %>% select(-X)
Liberia_monthly_pet <- filter(Liberia_monthly_district, measurement=='pet' & Year>='2013') %>% select(-X)
Liberia_monthly_pre <- filter(Liberia_monthly_district, measurement=='pre' & Year>='2013') %>% select(-X)
Liberia_monthly_tmn <- filter(Liberia_monthly_district, measurement=='tmn' & Year>='2013') %>% select(-X)
Liberia_monthly_tmp <- filter(Liberia_monthly_district, measurement=='tmp' & Year>='2013') %>% select(-X)
Liberia_monthly_tmx <- filter(Liberia_monthly_district, measurement=='tmx' & Year>='2013') %>% select(-X)
Liberia_monthly_dtr <- filter(Liberia_monthly_district, measurement=='dtr' & Year>='2013') %>% select(-X)

Liberia_weekly_vap <- tapply(Liberia_monthly_vap$Value,Liberia_monthly_vap$Location,func_spline,simplify = T)
Liberia_weekly_wet <- tapply(Liberia_monthly_wet$Value,Liberia_monthly_wet$Location,func_spline,simplify = T)
Liberia_weekly_pet <- tapply(Liberia_monthly_pet$Value,Liberia_monthly_pet$Location,func_spline,simplify = T)
Liberia_weekly_pre <- tapply(Liberia_monthly_pre$Value,Liberia_monthly_pre$Location,func_spline,simplify = T)
Liberia_weekly_tmn <- tapply(Liberia_monthly_tmn$Value,Liberia_monthly_tmn$Location,func_spline,simplify = T)
Liberia_weekly_tmp <- tapply(Liberia_monthly_tmp$Value,Liberia_monthly_tmp$Location,func_spline,simplify = T)
Liberia_weekly_tmx <- tapply(Liberia_monthly_tmx$Value,Liberia_monthly_tmx$Location,func_spline,simplify = T)
Liberia_weekly_dtr <- tapply(Liberia_monthly_tmx$Value,Liberia_monthly_tmx$Location,func_spline,simplify = T)

Liberia_weekly_climate <- as.data.frame(rep(names(Liberia_weekly_tmn), 152))
colnames(Liberia_weekly_climate) <- "Location"
Liberia_weekly_climate <- arrange(Liberia_weekly_climate, Location)
Liberia_weekly_climate$count_week <- 1:152
Liberia_weekly_climate <- cbind(Liberia_weekly_climate, vap=unlist(Liberia_weekly_vap), wet=unlist(Liberia_weekly_wet), pet=unlist(Liberia_weekly_pet),
                               pre=unlist(Liberia_weekly_pre), tmn=unlist(Liberia_weekly_tmn), tmp=unlist(Liberia_weekly_tmp), tmx=unlist(Liberia_weekly_tmx),
                               dtr=unlist(Liberia_weekly_dtr))
row.names(Liberia_weekly_climate) <- 1:length(Liberia_weekly_climate$Location)
Liberia_weekly_climate$wet <- ifelse(Liberia_weekly_climate$wet<1,0, Liberia_weekly_climate$wet)
Liberia_weekly_climate$pre <- ifelse(Liberia_weekly_climate$pre<1,0, Liberia_weekly_climate$pre)
Liberia_weekly_climate$Location <- toupper(Liberia_weekly_climate$Location)


# Obtain the weekly cases count for Liberia in order and add the count_week
Liberia_weekly_cases <- read.csv('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Cases/Liberia_cases_long.csv')
Liberia_weekly_cases <- Liberia_weekly_cases %>% filter(Source=='Patient database') %>% select(-X, -Source, -Indicator) %>% spread(Case_definition, Cases) %>% 
    arrange(Location, Epi_Week) %>% mutate(count_week=rep(53:171,15)) %>% filter(count_week<=152) %>% mutate(Total_cases=Confirmed+Probable) %>%
    select(-Confirmed, -Probable)


# now full joint the case counts data and the climate data
Liberia_weekly_climate$count_week <- as.numeric(as.character(Liberia_weekly_climate$count_week))
Liberia_weekly_cases_climate <- left_join(Liberia_weekly_climate,Liberia_weekly_cases,by=c("Location",'count_week'))
Liberia_weekly_cases_climate <- replace_na(Liberia_weekly_cases_climate, list(Total_cases=0))
Liberia_weekly_cases_climate$tmx <- as.numeric(as.character(Liberia_weekly_cases_climate$tmx))
Liberia_weekly_cases_climate$tmp <- as.numeric(as.character(Liberia_weekly_cases_climate$tmp))
Liberia_weekly_cases_climate$tmn <- as.numeric(as.character(Liberia_weekly_cases_climate$tmn))
Liberia_weekly_cases_climate$pre <- as.numeric(as.character(Liberia_weekly_cases_climate$pre))
Liberia_weekly_cases_climate$vap <- as.numeric(as.character(Liberia_weekly_cases_climate$vap))
Liberia_weekly_cases_climate$wet <- as.numeric(as.character(Liberia_weekly_cases_climate$wet))
Liberia_weekly_cases_climate$pet <- as.numeric(as.character(Liberia_weekly_cases_climate$pet))
Liberia_weekly_cases_climate$dtr <- as.numeric(as.character(Liberia_weekly_cases_climate$dtr))

write.csv(Liberia_weekly_cases_climate, 'Liberia_weekly_cases_climate.csv')
