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
#Bo
Vap_Bo_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         Vap1=vap.var[which(lon==11.75),which(lat==8.25),1:48],
                                         Vap2=vap.var[which(lon==11.75),which(lat==7.75),1:48]))
Vap_Bo_monthly$Vap <- rowMeans(select(Vap_Bo_monthly, Vap1, Vap2))
Vap_Bo_monthly$Location <- 'Bo'
Vap_Bo_monthly$date <- ymd(paste(Vap_Bo_monthly$Year, Vap_Bo_monthly$Month, Vap_Bo_monthly$day, sep="-"))
Vap_Bo_monthly_2015 <- select(Vap_Bo_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Bo_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Bo_monthly_2015a$Location <- 'Bo'
Vap_Bo_monthly_2015a$date <- ymd(paste(Vap_Bo_monthly_2015a$Year, Vap_Bo_monthly_2015a$Month, Vap_Bo_monthly_2015a$day, sep="-"))
Vap_Bo_monthly_2015a <- select(Vap_Bo_monthly_2015a, date, Year, Month, day, Location)
Vap_Bo_monthly_2015 <- full_join(Vap_Bo_monthly_2015a, Vap_Bo_monthly_2015)
Vap_Bo_monthly <- rbind(select(Vap_Bo_monthly,date, Year, Month, day, Location, Vap), Vap_Bo_monthly_2015)
rm(Vap_Bo_monthly_2015, Vap_Bo_monthly_2015a)
Vap_Bo_monthly$measurement <- "Vap"
Vap_Bo_monthly <- rename(Vap_Bo_monthly, Value=Vap)

#Bombali
Vap_Bombali_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                      Month=rep(seq(1,12,1), 4), day=1,
                                      Vap1=vap.var[which(lon==12.25),which(lat==9.75),1:48],
                                      Vap2=vap.var[which(lon==12.25),which(lat==9.25),1:48]))
Vap_Bombali_monthly$Vap <- rowMeans(select(Vap_Bombali_monthly, Vap1, Vap2))
Vap_Bombali_monthly$Location <- 'Bombali'
Vap_Bombali_monthly$date <- ymd(paste(Vap_Bombali_monthly$Year, Vap_Bombali_monthly$Month, Vap_Bombali_monthly$day, sep="-"))
Vap_Bombali_monthly_2015 <- select(Vap_Bombali_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Bombali_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Bombali_monthly_2015a$Location <- 'Bombali'
Vap_Bombali_monthly_2015a$date <- ymd(paste(Vap_Bombali_monthly_2015a$Year, Vap_Bombali_monthly_2015a$Month, Vap_Bombali_monthly_2015a$day, sep="-"))
Vap_Bombali_monthly_2015a <- select(Vap_Bombali_monthly_2015a, date, Year, Month, day, Location)
Vap_Bombali_monthly_2015 <- full_join(Vap_Bombali_monthly_2015a, Vap_Bombali_monthly_2015)
Vap_Bombali_monthly <- rbind(select(Vap_Bombali_monthly,date, Year, Month, day, Location, Vap), Vap_Bombali_monthly_2015)
rm(Vap_Bombali_monthly_2015, Vap_Bombali_monthly_2015a)
Vap_Bombali_monthly$measurement <- "Vap"
Vap_Bombali_monthly <- rename(Vap_Bombali_monthly, Value=Vap)

#Bonthe
Vap_Bonthe_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                      Month=rep(seq(1,12,1), 4), day=1,
                                      Vap1=vap.var[which(lon==12.75),which(lat==7.75),1:48],
                                      Vap2=vap.var[which(lon==12.25),which(lat==7.75),1:48],
                                      Vap3=vap.var[which(lon==12.25),which(lat==7.25),1:48]))
Vap_Bonthe_monthly$Vap <- rowMeans(select(Vap_Bonthe_monthly, Vap1, Vap2, Vap3))
Vap_Bonthe_monthly$Location <- 'Bonthe'
Vap_Bonthe_monthly$date <- ymd(paste(Vap_Bonthe_monthly$Year, Vap_Bonthe_monthly$Month, Vap_Bonthe_monthly$day, sep="-"))
Vap_Bonthe_monthly_2015 <- select(Vap_Bonthe_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Bonthe_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Bonthe_monthly_2015a$Location <- 'Bonthe'
Vap_Bonthe_monthly_2015a$date <- ymd(paste(Vap_Bonthe_monthly_2015a$Year, Vap_Bonthe_monthly_2015a$Month, Vap_Bonthe_monthly_2015a$day, sep="-"))
Vap_Bonthe_monthly_2015a <- select(Vap_Bonthe_monthly_2015a, date, Year, Month, day, Location)
Vap_Bonthe_monthly_2015 <- full_join(Vap_Bonthe_monthly_2015a, Vap_Bonthe_monthly_2015)
Vap_Bonthe_monthly <- rbind(select(Vap_Bonthe_monthly,date, Year, Month, day, Location, Vap), Vap_Bonthe_monthly_2015)
rm(Vap_Bonthe_monthly_2015, Vap_Bonthe_monthly_2015a)
Vap_Bonthe_monthly$measurement <- "Vap"
Vap_Bonthe_monthly <- rename(Vap_Bonthe_monthly, Value=Vap)

#Kailahun
Vap_Kailahun_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          Vap1=vap.var[which(lon==10.75),which(lat==8.25),1:48],
                                          Vap2=vap.var[which(lon==10.25),which(lat==8.25),1:48],
                                          Vap3=vap.var[which(lon==10.75),which(lat==7.75),1:48]))
Vap_Kailahun_monthly$Vap <- rowMeans(select(Vap_Kailahun_monthly, Vap1, Vap2, Vap3))
Vap_Kailahun_monthly$Location <- 'Kailahun'
Vap_Kailahun_monthly$date <- ymd(paste(Vap_Kailahun_monthly$Year, Vap_Kailahun_monthly$Month, Vap_Kailahun_monthly$day, sep="-"))
Vap_Kailahun_monthly_2015 <- select(Vap_Kailahun_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Kailahun_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Kailahun_monthly_2015a$Location <- 'Kailahun'
Vap_Kailahun_monthly_2015a$date <- ymd(paste(Vap_Kailahun_monthly_2015a$Year, Vap_Kailahun_monthly_2015a$Month, Vap_Kailahun_monthly_2015a$day, sep="-"))
Vap_Kailahun_monthly_2015a <- select(Vap_Kailahun_monthly_2015a, date, Year, Month, day, Location)
Vap_Kailahun_monthly_2015 <- full_join(Vap_Kailahun_monthly_2015a, Vap_Kailahun_monthly_2015)
Vap_Kailahun_monthly <- rbind(select(Vap_Kailahun_monthly,date, Year, Month, day, Location, Vap), Vap_Kailahun_monthly_2015)
rm(Vap_Kailahun_monthly_2015, Vap_Kailahun_monthly_2015a)
Vap_Kailahun_monthly$measurement <- "Vap"
Vap_Kailahun_monthly <- rename(Vap_Kailahun_monthly, Value=Vap)

#Kambia
Vap_Kambia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          Vap=vap.var[which(lon==12.75),which(lat==7.75),1:48]))
Vap_Kambia_monthly$Location <- 'Kambia'
Vap_Kambia_monthly$date <- ymd(paste(Vap_Kambia_monthly$Year, Vap_Kambia_monthly$Month, Vap_Kambia_monthly$day, sep="-"))
Vap_Kambia_monthly_2015 <- select(Vap_Kambia_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Kambia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Kambia_monthly_2015a$Location <- 'Kambia'
Vap_Kambia_monthly_2015a$date <- ymd(paste(Vap_Kambia_monthly_2015a$Year, Vap_Kambia_monthly_2015a$Month, Vap_Kambia_monthly_2015a$day, sep="-"))
Vap_Kambia_monthly_2015a <- select(Vap_Kambia_monthly_2015a, date, Year, Month, day, Location)
Vap_Kambia_monthly_2015 <- full_join(Vap_Kambia_monthly_2015a, Vap_Kambia_monthly_2015)
Vap_Kambia_monthly <- rbind(select(Vap_Kambia_monthly,date, Year, Month, day, Location, Vap), Vap_Kambia_monthly_2015)
rm(Vap_Kambia_monthly_2015, Vap_Kambia_monthly_2015a)
Vap_Kambia_monthly$measurement <- "Vap"
Vap_Kambia_monthly <- rename(Vap_Kambia_monthly, Value=Vap)

#Kenema
Vap_Kenema_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          Vap1=vap.var[which(lon==11.25),which(lat==8.25),1:48],
                                          Vap2=vap.var[which(lon==11.25),which(lat==7.75),1:48]))
Vap_Kenema_monthly$Vap <- rowMeans(select(Vap_Kenema_monthly, Vap1, Vap2))
Vap_Kenema_monthly$Location <- 'Kenema'
Vap_Kenema_monthly$date <- ymd(paste(Vap_Kenema_monthly$Year, Vap_Kenema_monthly$Month, Vap_Kenema_monthly$day, sep="-"))
Vap_Kenema_monthly_2015 <- select(Vap_Kenema_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Kenema_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Kenema_monthly_2015a$Location <- 'Kenema'
Vap_Kenema_monthly_2015a$date <- ymd(paste(Vap_Kenema_monthly_2015a$Year, Vap_Kenema_monthly_2015a$Month, Vap_Kenema_monthly_2015a$day, sep="-"))
Vap_Kenema_monthly_2015a <- select(Vap_Kenema_monthly_2015a, date, Year, Month, day, Location)
Vap_Kenema_monthly_2015 <- full_join(Vap_Kenema_monthly_2015a, Vap_Kenema_monthly_2015)
Vap_Kenema_monthly <- rbind(select(Vap_Kenema_monthly,date, Year, Month, day, Location, Vap), Vap_Kenema_monthly_2015)
rm(Vap_Kenema_monthly_2015, Vap_Kenema_monthly_2015a)
Vap_Kenema_monthly$measurement <- "Vap"
Vap_Kenema_monthly <- rename(Vap_Kenema_monthly, Value=Vap)

#Koinadugu
Vap_Koinadugu_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          Vap1=vap.var[which(lon==11.75),which(lat==9.75),1:48],
                                          Vap2=vap.var[which(lon==11.25),which(lat==9.75),1:48],
                                          Vap3=vap.var[which(lon==11.75),which(lat==9.25),1:48],
                                          Vap4=vap.var[which(lon==11.25),which(lat==9.25),1:48],
                                          Vap5=vap.var[which(lon==10.75),which(lat==9.25),1:48]))
Vap_Koinadugu_monthly$Vap <- rowMeans(select(Vap_Koinadugu_monthly, Vap1, Vap2, Vap3, Vap4, Vap5))
Vap_Koinadugu_monthly$Location <- 'Koinadugu'
Vap_Koinadugu_monthly$date <- ymd(paste(Vap_Koinadugu_monthly$Year, Vap_Koinadugu_monthly$Month, Vap_Koinadugu_monthly$day, sep="-"))
Vap_Koinadugu_monthly_2015 <- select(Vap_Koinadugu_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Koinadugu_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Koinadugu_monthly_2015a$Location <- 'Koinadugu'
Vap_Koinadugu_monthly_2015a$date <- ymd(paste(Vap_Koinadugu_monthly_2015a$Year, Vap_Koinadugu_monthly_2015a$Month, Vap_Koinadugu_monthly_2015a$day, sep="-"))
Vap_Koinadugu_monthly_2015a <- select(Vap_Koinadugu_monthly_2015a, date, Year, Month, day, Location)
Vap_Koinadugu_monthly_2015 <- full_join(Vap_Koinadugu_monthly_2015a, Vap_Koinadugu_monthly_2015)
Vap_Koinadugu_monthly <- rbind(select(Vap_Koinadugu_monthly,date, Year, Month, day, Location, Vap), Vap_Koinadugu_monthly_2015)
rm(Vap_Koinadugu_monthly_2015, Vap_Koinadugu_monthly_2015a)
Vap_Koinadugu_monthly$measurement <- "Vap"
Vap_Koinadugu_monthly <- rename(Vap_Koinadugu_monthly, Value=Vap)

#Kono
Vap_Kono_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          Vap1=vap.var[which(lon==11.25),which(lat==8.75),1:48],
                                          Vap2=vap.var[which(lon==10.75),which(lat==8.75),1:48]))
Vap_Kono_monthly$Vap <- rowMeans(select(Vap_Kono_monthly, Vap1, Vap2))
Vap_Kono_monthly$Location <- 'Kono'
Vap_Kono_monthly$date <- ymd(paste(Vap_Kono_monthly$Year, Vap_Kono_monthly$Month, Vap_Kono_monthly$day, sep="-"))
Vap_Kono_monthly_2015 <- select(Vap_Kono_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Kono_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Kono_monthly_2015a$Location <- 'Kono'
Vap_Kono_monthly_2015a$date <- ymd(paste(Vap_Kono_monthly_2015a$Year, Vap_Kono_monthly_2015a$Month, Vap_Kono_monthly_2015a$day, sep="-"))
Vap_Kono_monthly_2015a <- select(Vap_Kono_monthly_2015a, date, Year, Month, day, Location)
Vap_Kono_monthly_2015 <- full_join(Vap_Kono_monthly_2015a, Vap_Kono_monthly_2015)
Vap_Kono_monthly <- rbind(select(Vap_Kono_monthly,date, Year, Month, day, Location, Vap), Vap_Kono_monthly_2015)
rm(Vap_Kono_monthly_2015, Vap_Kono_monthly_2015a)
Vap_Kono_monthly$measurement <- "Vap"
Vap_Kono_monthly <- rename(Vap_Kono_monthly, Value=Vap)

#Moyamba
Vap_Moyamba_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          Vap1=vap.var[which(lon==12.75),which(lat==8.25),1:48],
                                          Vap2=vap.var[which(lon==12.25),which(lat==8.25),1:48],
                                          Vap3=vap.var[which(lon==11.75),which(lat==8.25),1:48]))
Vap_Moyamba_monthly$Vap <- rowMeans(select(Vap_Moyamba_monthly, Vap1, Vap2, Vap3))
Vap_Moyamba_monthly$Location <- 'Moyamba'
Vap_Moyamba_monthly$date <- ymd(paste(Vap_Moyamba_monthly$Year, Vap_Moyamba_monthly$Month, Vap_Moyamba_monthly$day, sep="-"))
Vap_Moyamba_monthly_2015 <- select(Vap_Moyamba_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Moyamba_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Moyamba_monthly_2015a$Location <- 'Moyamba'
Vap_Moyamba_monthly_2015a$date <- ymd(paste(Vap_Moyamba_monthly_2015a$Year, Vap_Moyamba_monthly_2015a$Month, Vap_Moyamba_monthly_2015a$day, sep="-"))
Vap_Moyamba_monthly_2015a <- select(Vap_Moyamba_monthly_2015a, date, Year, Month, day, Location)
Vap_Moyamba_monthly_2015 <- full_join(Vap_Moyamba_monthly_2015a, Vap_Moyamba_monthly_2015)
Vap_Moyamba_monthly <- rbind(select(Vap_Moyamba_monthly,date, Year, Month, day, Location, Vap), Vap_Moyamba_monthly_2015)
rm(Vap_Moyamba_monthly_2015, Vap_Moyamba_monthly_2015a)
Vap_Moyamba_monthly$measurement <- "Vap"
Vap_Moyamba_monthly <- rename(Vap_Moyamba_monthly, Value=Vap)

#Port Loko
Vap_PortLoko_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          Vap1=vap.var[which(lon==13.25),which(lat==8.75),1:48],
                                          Vap2=vap.var[which(lon==12.75),which(lat==8.75),1:48]))
Vap_PortLoko_monthly$Vap <- rowMeans(select(Vap_PortLoko_monthly, Vap1, Vap2))
Vap_PortLoko_monthly$Location <- 'PortLoko'
Vap_PortLoko_monthly$date <- ymd(paste(Vap_PortLoko_monthly$Year, Vap_PortLoko_monthly$Month, Vap_PortLoko_monthly$day, sep="-"))
Vap_PortLoko_monthly_2015 <- select(Vap_PortLoko_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_PortLoko_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_PortLoko_monthly_2015a$Location <- 'PortLoko'
Vap_PortLoko_monthly_2015a$date <- ymd(paste(Vap_PortLoko_monthly_2015a$Year, Vap_PortLoko_monthly_2015a$Month, Vap_PortLoko_monthly_2015a$day, sep="-"))
Vap_PortLoko_monthly_2015a <- select(Vap_PortLoko_monthly_2015a, date, Year, Month, day, Location)
Vap_PortLoko_monthly_2015 <- full_join(Vap_PortLoko_monthly_2015a, Vap_PortLoko_monthly_2015)
Vap_PortLoko_monthly <- rbind(select(Vap_PortLoko_monthly,date, Year, Month, day, Location, Vap), Vap_PortLoko_monthly_2015)
rm(Vap_PortLoko_monthly_2015, Vap_PortLoko_monthly_2015a)
Vap_PortLoko_monthly$measurement <- "Vap"
Vap_PortLoko_monthly <- rename(Vap_PortLoko_monthly, Value=Vap)

#Pujehun
Vap_Pujehun_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          Vap1=vap.var[which(lon==11.75),which(lat==7.25),1:48],
                                          Vap2=vap.var[which(lon==11.25),which(lat==7.25),1:48]))
Vap_Pujehun_monthly$Vap <- rowMeans(select(Vap_Pujehun_monthly, Vap1, Vap2))
Vap_Pujehun_monthly$Location <- 'Pujehun'
Vap_Pujehun_monthly$date <- ymd(paste(Vap_Pujehun_monthly$Year, Vap_Pujehun_monthly$Month, Vap_Pujehun_monthly$day, sep="-"))
Vap_Pujehun_monthly_2015 <- select(Vap_Pujehun_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Pujehun_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Pujehun_monthly_2015a$Location <- 'Pujehun'
Vap_Pujehun_monthly_2015a$date <- ymd(paste(Vap_Pujehun_monthly_2015a$Year, Vap_Pujehun_monthly_2015a$Month, Vap_Pujehun_monthly_2015a$day, sep="-"))
Vap_Pujehun_monthly_2015a <- select(Vap_Pujehun_monthly_2015a, date, Year, Month, day, Location)
Vap_Pujehun_monthly_2015 <- full_join(Vap_Pujehun_monthly_2015a, Vap_Pujehun_monthly_2015)
Vap_Pujehun_monthly <- rbind(select(Vap_Pujehun_monthly,date, Year, Month, day, Location, Vap), Vap_Pujehun_monthly_2015)
rm(Vap_Pujehun_monthly_2015, Vap_Pujehun_monthly_2015a)
Vap_Pujehun_monthly$measurement <- "Vap"
Vap_Pujehun_monthly <- rename(Vap_Pujehun_monthly, Value=Vap)

#Tonkolili
Vap_Tonkolili_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          Vap1=vap.var[which(lon==12.25),which(lat==8.75),1:48],
                                          Vap2=vap.var[which(lon==11.75),which(lat==8.75),1:48]))
Vap_Tonkolili_monthly$Vap <- rowMeans(select(Vap_Tonkolili_monthly, Vap1, Vap2))
Vap_Tonkolili_monthly$Location <- 'Tonkolili'
Vap_Tonkolili_monthly$date <- ymd(paste(Vap_Tonkolili_monthly$Year, Vap_Tonkolili_monthly$Month, Vap_Tonkolili_monthly$day, sep="-"))
Vap_Tonkolili_monthly_2015 <- select(Vap_Tonkolili_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Tonkolili_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Tonkolili_monthly_2015a$Location <- 'Tonkolili'
Vap_Tonkolili_monthly_2015a$date <- ymd(paste(Vap_Tonkolili_monthly_2015a$Year, Vap_Tonkolili_monthly_2015a$Month, Vap_Tonkolili_monthly_2015a$day, sep="-"))
Vap_Tonkolili_monthly_2015a <- select(Vap_Tonkolili_monthly_2015a, date, Year, Month, day, Location)
Vap_Tonkolili_monthly_2015 <- full_join(Vap_Tonkolili_monthly_2015a, Vap_Tonkolili_monthly_2015)
Vap_Tonkolili_monthly <- rbind(select(Vap_Tonkolili_monthly,date, Year, Month, day, Location, Vap), Vap_Tonkolili_monthly_2015)
rm(Vap_Tonkolili_monthly_2015, Vap_Tonkolili_monthly_2015a)
Vap_Tonkolili_monthly$measurement <- "Vap"
Vap_Tonkolili_monthly <- rename(Vap_Tonkolili_monthly, Value=Vap)

#Western rural
Vap_WesternRural_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          Vap=vap.var[which(lon==13.25),which(lat==8.25),1:48]))
Vap_WesternRural_monthly$Location <- 'WesternRural'
Vap_WesternRural_monthly$date <- ymd(paste(Vap_WesternRural_monthly$Year, Vap_WesternRural_monthly$Month, Vap_WesternRural_monthly$day, sep="-"))
Vap_WesternRural_monthly_2015 <- select(Vap_WesternRural_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_WesternRural_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_WesternRural_monthly_2015a$Location <- 'WesternRural'
Vap_WesternRural_monthly_2015a$date <- ymd(paste(Vap_WesternRural_monthly_2015a$Year, Vap_WesternRural_monthly_2015a$Month, Vap_WesternRural_monthly_2015a$day, sep="-"))
Vap_WesternRural_monthly_2015a <- select(Vap_WesternRural_monthly_2015a, date, Year, Month, day, Location)
Vap_WesternRural_monthly_2015 <- full_join(Vap_WesternRural_monthly_2015a, Vap_WesternRural_monthly_2015)
Vap_WesternRural_monthly <- rbind(select(Vap_WesternRural_monthly,date, Year, Month, day, Location, Vap), Vap_WesternRural_monthly_2015)
rm(Vap_WesternRural_monthly_2015, Vap_WesternRural_monthly_2015a)
Vap_WesternRural_monthly$measurement <- "Vap"
Vap_WesternRural_monthly <- rename(Vap_WesternRural_monthly, Value=Vap)

#Wester urban
Vap_WesternUrban_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                                Month=rep(seq(1,12,1), 4), day=1,
                                                Vap=vap.var[which(lon==13.25),which(lat==8.25),1:48]))
Vap_WesternUrban_monthly$Location <- 'WesternUrban'
Vap_WesternUrban_monthly$date <- ymd(paste(Vap_WesternUrban_monthly$Year, Vap_WesternUrban_monthly$Month, Vap_WesternUrban_monthly$day, sep="-"))
Vap_WesternUrban_monthly_2015 <- select(Vap_WesternUrban_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_WesternUrban_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_WesternUrban_monthly_2015a$Location <- 'WesternUrban'
Vap_WesternUrban_monthly_2015a$date <- ymd(paste(Vap_WesternUrban_monthly_2015a$Year, Vap_WesternUrban_monthly_2015a$Month, Vap_WesternUrban_monthly_2015a$day, sep="-"))
Vap_WesternUrban_monthly_2015a <- select(Vap_WesternUrban_monthly_2015a, date, Year, Month, day, Location)
Vap_WesternUrban_monthly_2015 <- full_join(Vap_WesternUrban_monthly_2015a, Vap_WesternUrban_monthly_2015)
Vap_WesternUrban_monthly <- rbind(select(Vap_WesternUrban_monthly,date, Year, Month, day, Location, Vap), Vap_WesternUrban_monthly_2015)
rm(Vap_WesternUrban_monthly_2015, Vap_WesternUrban_monthly_2015a)
Vap_WesternUrban_monthly$measurement <- "Vap"
Vap_WesternUrban_monthly <- rename(Vap_WesternUrban_monthly, Value=Vap)

#Merging in long format
Vap_SL_monthly_district <- rbind(Vap_Bo_monthly, Vap_Bombali_monthly, Vap_Bonthe_monthly,
                                     Vap_Kailahun_monthly, Vap_Kambia_monthly, Vap_Kenema_monthly,
                                     Vap_Koinadugu_monthly, Vap_Kono_monthly, Vap_Moyamba_monthly,
                                     Vap_PortLoko_monthly, Vap_Pujehun_monthly, Vap_Tonkolili_monthly,
                                     Vap_WesternRural_monthly, Vap_WesternUrban_monthly)

#####################
#wet - wet days
#####################
wet.full <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.01.2011.2014.wet.dat.nc', write=FALSE)
wet.var <- var.get.nc(wet.full, "wet")

#Bo
wet_Bo_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                      Month=rep(seq(1,12,1), 4), day=1,
                                      wet1=wet.var[which(lon==11.75),which(lat==8.25),1:48],
                                      wet2=wet.var[which(lon==11.75),which(lat==7.75),1:48]))
wet_Bo_monthly$wet <- rowMeans(select(wet_Bo_monthly, wet1, wet2))
wet_Bo_monthly$Location <- 'Bo'
wet_Bo_monthly$date <- ymd(paste(wet_Bo_monthly$Year, wet_Bo_monthly$Month, wet_Bo_monthly$day, sep="-"))
wet_Bo_monthly_2015 <- select(wet_Bo_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Bo_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Bo_monthly_2015a$Location <- 'Bo'
wet_Bo_monthly_2015a$date <- ymd(paste(wet_Bo_monthly_2015a$Year, wet_Bo_monthly_2015a$Month, wet_Bo_monthly_2015a$day, sep="-"))
wet_Bo_monthly_2015a <- select(wet_Bo_monthly_2015a, date, Year, Month, day, Location)
wet_Bo_monthly_2015 <- full_join(wet_Bo_monthly_2015a, wet_Bo_monthly_2015)
wet_Bo_monthly <- rbind(select(wet_Bo_monthly,date, Year, Month, day, Location, wet), wet_Bo_monthly_2015)
rm(wet_Bo_monthly_2015, wet_Bo_monthly_2015a)
wet_Bo_monthly$measurement <- "wet"
wet_Bo_monthly <- rename(wet_Bo_monthly, Value=wet)

#Bombali
wet_Bombali_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           wet1=wet.var[which(lon==12.25),which(lat==9.75),1:48],
                                           wet2=wet.var[which(lon==12.25),which(lat==9.25),1:48]))
wet_Bombali_monthly$wet <- rowMeans(select(wet_Bombali_monthly, wet1, wet2))
wet_Bombali_monthly$Location <- 'Bombali'
wet_Bombali_monthly$date <- ymd(paste(wet_Bombali_monthly$Year, wet_Bombali_monthly$Month, wet_Bombali_monthly$day, sep="-"))
wet_Bombali_monthly_2015 <- select(wet_Bombali_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Bombali_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Bombali_monthly_2015a$Location <- 'Bombali'
wet_Bombali_monthly_2015a$date <- ymd(paste(wet_Bombali_monthly_2015a$Year, wet_Bombali_monthly_2015a$Month, wet_Bombali_monthly_2015a$day, sep="-"))
wet_Bombali_monthly_2015a <- select(wet_Bombali_monthly_2015a, date, Year, Month, day, Location)
wet_Bombali_monthly_2015 <- full_join(wet_Bombali_monthly_2015a, wet_Bombali_monthly_2015)
wet_Bombali_monthly <- rbind(select(wet_Bombali_monthly,date, Year, Month, day, Location, wet), wet_Bombali_monthly_2015)
rm(wet_Bombali_monthly_2015, wet_Bombali_monthly_2015a)
wet_Bombali_monthly$measurement <- "wet"
wet_Bombali_monthly <- rename(wet_Bombali_monthly, Value=wet)

#Bonthe
wet_Bonthe_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          wet1=wet.var[which(lon==12.75),which(lat==7.75),1:48],
                                          wet2=wet.var[which(lon==12.25),which(lat==7.75),1:48],
                                          wet3=wet.var[which(lon==12.25),which(lat==7.25),1:48]))
wet_Bonthe_monthly$wet <- rowMeans(select(wet_Bonthe_monthly, wet1, wet2, wet3))
wet_Bonthe_monthly$Location <- 'Bonthe'
wet_Bonthe_monthly$date <- ymd(paste(wet_Bonthe_monthly$Year, wet_Bonthe_monthly$Month, wet_Bonthe_monthly$day, sep="-"))
wet_Bonthe_monthly_2015 <- select(wet_Bonthe_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Bonthe_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Bonthe_monthly_2015a$Location <- 'Bonthe'
wet_Bonthe_monthly_2015a$date <- ymd(paste(wet_Bonthe_monthly_2015a$Year, wet_Bonthe_monthly_2015a$Month, wet_Bonthe_monthly_2015a$day, sep="-"))
wet_Bonthe_monthly_2015a <- select(wet_Bonthe_monthly_2015a, date, Year, Month, day, Location)
wet_Bonthe_monthly_2015 <- full_join(wet_Bonthe_monthly_2015a, wet_Bonthe_monthly_2015)
wet_Bonthe_monthly <- rbind(select(wet_Bonthe_monthly,date, Year, Month, day, Location, wet), wet_Bonthe_monthly_2015)
rm(wet_Bonthe_monthly_2015, wet_Bonthe_monthly_2015a)
wet_Bonthe_monthly$measurement <- "wet"
wet_Bonthe_monthly <- rename(wet_Bonthe_monthly, Value=wet)

#Kailahun
wet_Kailahun_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            wet1=wet.var[which(lon==10.75),which(lat==8.25),1:48],
                                            wet2=wet.var[which(lon==10.25),which(lat==8.25),1:48],
                                            wet3=wet.var[which(lon==10.75),which(lat==7.75),1:48]))
wet_Kailahun_monthly$wet <- rowMeans(select(wet_Kailahun_monthly, wet1, wet2, wet3))
wet_Kailahun_monthly$Location <- 'Kailahun'
wet_Kailahun_monthly$date <- ymd(paste(wet_Kailahun_monthly$Year, wet_Kailahun_monthly$Month, wet_Kailahun_monthly$day, sep="-"))
wet_Kailahun_monthly_2015 <- select(wet_Kailahun_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Kailahun_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Kailahun_monthly_2015a$Location <- 'Kailahun'
wet_Kailahun_monthly_2015a$date <- ymd(paste(wet_Kailahun_monthly_2015a$Year, wet_Kailahun_monthly_2015a$Month, wet_Kailahun_monthly_2015a$day, sep="-"))
wet_Kailahun_monthly_2015a <- select(wet_Kailahun_monthly_2015a, date, Year, Month, day, Location)
wet_Kailahun_monthly_2015 <- full_join(wet_Kailahun_monthly_2015a, wet_Kailahun_monthly_2015)
wet_Kailahun_monthly <- rbind(select(wet_Kailahun_monthly,date, Year, Month, day, Location, wet), wet_Kailahun_monthly_2015)
rm(wet_Kailahun_monthly_2015, wet_Kailahun_monthly_2015a)
wet_Kailahun_monthly$measurement <- "wet"
wet_Kailahun_monthly <- rename(wet_Kailahun_monthly, Value=wet)

#Kambia
wet_Kambia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          wet=wet.var[which(lon==12.75),which(lat==7.75),1:48]))
wet_Kambia_monthly$Location <- 'Kambia'
wet_Kambia_monthly$date <- ymd(paste(wet_Kambia_monthly$Year, wet_Kambia_monthly$Month, wet_Kambia_monthly$day, sep="-"))
wet_Kambia_monthly_2015 <- select(wet_Kambia_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Kambia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Kambia_monthly_2015a$Location <- 'Kambia'
wet_Kambia_monthly_2015a$date <- ymd(paste(wet_Kambia_monthly_2015a$Year, wet_Kambia_monthly_2015a$Month, wet_Kambia_monthly_2015a$day, sep="-"))
wet_Kambia_monthly_2015a <- select(wet_Kambia_monthly_2015a, date, Year, Month, day, Location)
wet_Kambia_monthly_2015 <- full_join(wet_Kambia_monthly_2015a, wet_Kambia_monthly_2015)
wet_Kambia_monthly <- rbind(select(wet_Kambia_monthly,date, Year, Month, day, Location, wet), wet_Kambia_monthly_2015)
rm(wet_Kambia_monthly_2015, wet_Kambia_monthly_2015a)
wet_Kambia_monthly$measurement <- "wet"
wet_Kambia_monthly <- rename(wet_Kambia_monthly, Value=wet)

#Kenema
wet_Kenema_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          wet1=wet.var[which(lon==11.25),which(lat==8.25),1:48],
                                          wet2=wet.var[which(lon==11.25),which(lat==7.75),1:48]))
wet_Kenema_monthly$wet <- rowMeans(select(wet_Kenema_monthly, wet1, wet2))
wet_Kenema_monthly$Location <- 'Kenema'
wet_Kenema_monthly$date <- ymd(paste(wet_Kenema_monthly$Year, wet_Kenema_monthly$Month, wet_Kenema_monthly$day, sep="-"))
wet_Kenema_monthly_2015 <- select(wet_Kenema_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Kenema_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Kenema_monthly_2015a$Location <- 'Kenema'
wet_Kenema_monthly_2015a$date <- ymd(paste(wet_Kenema_monthly_2015a$Year, wet_Kenema_monthly_2015a$Month, wet_Kenema_monthly_2015a$day, sep="-"))
wet_Kenema_monthly_2015a <- select(wet_Kenema_monthly_2015a, date, Year, Month, day, Location)
wet_Kenema_monthly_2015 <- full_join(wet_Kenema_monthly_2015a, wet_Kenema_monthly_2015)
wet_Kenema_monthly <- rbind(select(wet_Kenema_monthly,date, Year, Month, day, Location, wet), wet_Kenema_monthly_2015)
rm(wet_Kenema_monthly_2015, wet_Kenema_monthly_2015a)
wet_Kenema_monthly$measurement <- "wet"
wet_Kenema_monthly <- rename(wet_Kenema_monthly, Value=wet)

#Koinadugu
wet_Koinadugu_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            wet1=wet.var[which(lon==11.75),which(lat==9.75),1:48],
                                            wet2=wet.var[which(lon==11.25),which(lat==9.75),1:48],
                                            wet3=wet.var[which(lon==11.75),which(lat==9.25),1:48],
                                            wet4=wet.var[which(lon==11.25),which(lat==9.25),1:48],
                                            wet5=wet.var[which(lon==10.75),which(lat==9.25),1:48]))
wet_Koinadugu_monthly$wet <- rowMeans(select(wet_Koinadugu_monthly, wet1, wet2, wet3, wet4, wet5))
wet_Koinadugu_monthly$Location <- 'Koinadugu'
wet_Koinadugu_monthly$date <- ymd(paste(wet_Koinadugu_monthly$Year, wet_Koinadugu_monthly$Month, wet_Koinadugu_monthly$day, sep="-"))
wet_Koinadugu_monthly_2015 <- select(wet_Koinadugu_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Koinadugu_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Koinadugu_monthly_2015a$Location <- 'Koinadugu'
wet_Koinadugu_monthly_2015a$date <- ymd(paste(wet_Koinadugu_monthly_2015a$Year, wet_Koinadugu_monthly_2015a$Month, wet_Koinadugu_monthly_2015a$day, sep="-"))
wet_Koinadugu_monthly_2015a <- select(wet_Koinadugu_monthly_2015a, date, Year, Month, day, Location)
wet_Koinadugu_monthly_2015 <- full_join(wet_Koinadugu_monthly_2015a, wet_Koinadugu_monthly_2015)
wet_Koinadugu_monthly <- rbind(select(wet_Koinadugu_monthly,date, Year, Month, day, Location, wet), wet_Koinadugu_monthly_2015)
rm(wet_Koinadugu_monthly_2015, wet_Koinadugu_monthly_2015a)
wet_Koinadugu_monthly$measurement <- "wet"
wet_Koinadugu_monthly <- rename(wet_Koinadugu_monthly, Value=wet)

#Kono
wet_Kono_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        wet1=wet.var[which(lon==11.25),which(lat==8.75),1:48],
                                        wet2=wet.var[which(lon==10.75),which(lat==8.75),1:48]))
wet_Kono_monthly$wet <- rowMeans(select(wet_Kono_monthly, wet1, wet2))
wet_Kono_monthly$Location <- 'Kono'
wet_Kono_monthly$date <- ymd(paste(wet_Kono_monthly$Year, wet_Kono_monthly$Month, wet_Kono_monthly$day, sep="-"))
wet_Kono_monthly_2015 <- select(wet_Kono_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Kono_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Kono_monthly_2015a$Location <- 'Kono'
wet_Kono_monthly_2015a$date <- ymd(paste(wet_Kono_monthly_2015a$Year, wet_Kono_monthly_2015a$Month, wet_Kono_monthly_2015a$day, sep="-"))
wet_Kono_monthly_2015a <- select(wet_Kono_monthly_2015a, date, Year, Month, day, Location)
wet_Kono_monthly_2015 <- full_join(wet_Kono_monthly_2015a, wet_Kono_monthly_2015)
wet_Kono_monthly <- rbind(select(wet_Kono_monthly,date, Year, Month, day, Location, wet), wet_Kono_monthly_2015)
rm(wet_Kono_monthly_2015, wet_Kono_monthly_2015a)
wet_Kono_monthly$measurement <- "wet"
wet_Kono_monthly <- rename(wet_Kono_monthly, Value=wet)

#Moyamba
wet_Moyamba_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           wet1=wet.var[which(lon==12.75),which(lat==8.25),1:48],
                                           wet2=wet.var[which(lon==12.25),which(lat==8.25),1:48],
                                           wet3=wet.var[which(lon==11.75),which(lat==8.25),1:48]))
wet_Moyamba_monthly$wet <- rowMeans(select(wet_Moyamba_monthly, wet1, wet2, wet3))
wet_Moyamba_monthly$Location <- 'Moyamba'
wet_Moyamba_monthly$date <- ymd(paste(wet_Moyamba_monthly$Year, wet_Moyamba_monthly$Month, wet_Moyamba_monthly$day, sep="-"))
wet_Moyamba_monthly_2015 <- select(wet_Moyamba_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Moyamba_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Moyamba_monthly_2015a$Location <- 'Moyamba'
wet_Moyamba_monthly_2015a$date <- ymd(paste(wet_Moyamba_monthly_2015a$Year, wet_Moyamba_monthly_2015a$Month, wet_Moyamba_monthly_2015a$day, sep="-"))
wet_Moyamba_monthly_2015a <- select(wet_Moyamba_monthly_2015a, date, Year, Month, day, Location)
wet_Moyamba_monthly_2015 <- full_join(wet_Moyamba_monthly_2015a, wet_Moyamba_monthly_2015)
wet_Moyamba_monthly <- rbind(select(wet_Moyamba_monthly,date, Year, Month, day, Location, wet), wet_Moyamba_monthly_2015)
rm(wet_Moyamba_monthly_2015, wet_Moyamba_monthly_2015a)
wet_Moyamba_monthly$measurement <- "wet"
wet_Moyamba_monthly <- rename(wet_Moyamba_monthly, Value=wet)

#Port Loko
wet_PortLoko_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            wet1=wet.var[which(lon==13.25),which(lat==8.75),1:48],
                                            wet2=wet.var[which(lon==12.75),which(lat==8.75),1:48]))
wet_PortLoko_monthly$wet <- rowMeans(select(wet_PortLoko_monthly, wet1, wet2))
wet_PortLoko_monthly$Location <- 'PortLoko'
wet_PortLoko_monthly$date <- ymd(paste(wet_PortLoko_monthly$Year, wet_PortLoko_monthly$Month, wet_PortLoko_monthly$day, sep="-"))
wet_PortLoko_monthly_2015 <- select(wet_PortLoko_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_PortLoko_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_PortLoko_monthly_2015a$Location <- 'PortLoko'
wet_PortLoko_monthly_2015a$date <- ymd(paste(wet_PortLoko_monthly_2015a$Year, wet_PortLoko_monthly_2015a$Month, wet_PortLoko_monthly_2015a$day, sep="-"))
wet_PortLoko_monthly_2015a <- select(wet_PortLoko_monthly_2015a, date, Year, Month, day, Location)
wet_PortLoko_monthly_2015 <- full_join(wet_PortLoko_monthly_2015a, wet_PortLoko_monthly_2015)
wet_PortLoko_monthly <- rbind(select(wet_PortLoko_monthly,date, Year, Month, day, Location, wet), wet_PortLoko_monthly_2015)
rm(wet_PortLoko_monthly_2015, wet_PortLoko_monthly_2015a)
wet_PortLoko_monthly$measurement <- "wet"
wet_PortLoko_monthly <- rename(wet_PortLoko_monthly, Value=wet)

#Pujehun
wet_Pujehun_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           wet1=wet.var[which(lon==11.75),which(lat==7.25),1:48],
                                           wet2=wet.var[which(lon==11.25),which(lat==7.25),1:48]))
wet_Pujehun_monthly$wet <- rowMeans(select(wet_Pujehun_monthly, wet1, wet2))
wet_Pujehun_monthly$Location <- 'Pujehun'
wet_Pujehun_monthly$date <- ymd(paste(wet_Pujehun_monthly$Year, wet_Pujehun_monthly$Month, wet_Pujehun_monthly$day, sep="-"))
wet_Pujehun_monthly_2015 <- select(wet_Pujehun_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Pujehun_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Pujehun_monthly_2015a$Location <- 'Pujehun'
wet_Pujehun_monthly_2015a$date <- ymd(paste(wet_Pujehun_monthly_2015a$Year, wet_Pujehun_monthly_2015a$Month, wet_Pujehun_monthly_2015a$day, sep="-"))
wet_Pujehun_monthly_2015a <- select(wet_Pujehun_monthly_2015a, date, Year, Month, day, Location)
wet_Pujehun_monthly_2015 <- full_join(wet_Pujehun_monthly_2015a, wet_Pujehun_monthly_2015)
wet_Pujehun_monthly <- rbind(select(wet_Pujehun_monthly,date, Year, Month, day, Location, wet), wet_Pujehun_monthly_2015)
rm(wet_Pujehun_monthly_2015, wet_Pujehun_monthly_2015a)
wet_Pujehun_monthly$measurement <- "wet"
wet_Pujehun_monthly <- rename(wet_Pujehun_monthly, Value=wet)

#Tonkolili
wet_Tonkolili_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             wet1=wet.var[which(lon==12.25),which(lat==8.75),1:48],
                                             wet2=wet.var[which(lon==11.75),which(lat==8.75),1:48]))
wet_Tonkolili_monthly$wet <- rowMeans(select(wet_Tonkolili_monthly, wet1, wet2))
wet_Tonkolili_monthly$Location <- 'Tonkolili'
wet_Tonkolili_monthly$date <- ymd(paste(wet_Tonkolili_monthly$Year, wet_Tonkolili_monthly$Month, wet_Tonkolili_monthly$day, sep="-"))
wet_Tonkolili_monthly_2015 <- select(wet_Tonkolili_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Tonkolili_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Tonkolili_monthly_2015a$Location <- 'Tonkolili'
wet_Tonkolili_monthly_2015a$date <- ymd(paste(wet_Tonkolili_monthly_2015a$Year, wet_Tonkolili_monthly_2015a$Month, wet_Tonkolili_monthly_2015a$day, sep="-"))
wet_Tonkolili_monthly_2015a <- select(wet_Tonkolili_monthly_2015a, date, Year, Month, day, Location)
wet_Tonkolili_monthly_2015 <- full_join(wet_Tonkolili_monthly_2015a, wet_Tonkolili_monthly_2015)
wet_Tonkolili_monthly <- rbind(select(wet_Tonkolili_monthly,date, Year, Month, day, Location, wet), wet_Tonkolili_monthly_2015)
rm(wet_Tonkolili_monthly_2015, wet_Tonkolili_monthly_2015a)
wet_Tonkolili_monthly$measurement <- "wet"
wet_Tonkolili_monthly <- rename(wet_Tonkolili_monthly, Value=wet)

#Western rural
wet_WesternRural_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                                Month=rep(seq(1,12,1), 4), day=1,
                                                wet=wet.var[which(lon==13.25),which(lat==8.25),1:48]))
wet_WesternRural_monthly$Location <- 'WesternRural'
wet_WesternRural_monthly$date <- ymd(paste(wet_WesternRural_monthly$Year, wet_WesternRural_monthly$Month, wet_WesternRural_monthly$day, sep="-"))
wet_WesternRural_monthly_2015 <- select(wet_WesternRural_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_WesternRural_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_WesternRural_monthly_2015a$Location <- 'WesternRural'
wet_WesternRural_monthly_2015a$date <- ymd(paste(wet_WesternRural_monthly_2015a$Year, wet_WesternRural_monthly_2015a$Month, wet_WesternRural_monthly_2015a$day, sep="-"))
wet_WesternRural_monthly_2015a <- select(wet_WesternRural_monthly_2015a, date, Year, Month, day, Location)
wet_WesternRural_monthly_2015 <- full_join(wet_WesternRural_monthly_2015a, wet_WesternRural_monthly_2015)
wet_WesternRural_monthly <- rbind(select(wet_WesternRural_monthly,date, Year, Month, day, Location, wet), wet_WesternRural_monthly_2015)
rm(wet_WesternRural_monthly_2015, wet_WesternRural_monthly_2015a)
wet_WesternRural_monthly$measurement <- "wet"
wet_WesternRural_monthly <- rename(wet_WesternRural_monthly, Value=wet)

#Wester urban
wet_WesternUrban_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                                Month=rep(seq(1,12,1), 4), day=1,
                                                wet=wet.var[which(lon==13.25),which(lat==8.25),1:48]))
wet_WesternUrban_monthly$Location <- 'WesternUrban'
wet_WesternUrban_monthly$date <- ymd(paste(wet_WesternUrban_monthly$Year, wet_WesternUrban_monthly$Month, wet_WesternUrban_monthly$day, sep="-"))
wet_WesternUrban_monthly_2015 <- select(wet_WesternUrban_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_WesternUrban_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_WesternUrban_monthly_2015a$Location <- 'WesternUrban'
wet_WesternUrban_monthly_2015a$date <- ymd(paste(wet_WesternUrban_monthly_2015a$Year, wet_WesternUrban_monthly_2015a$Month, wet_WesternUrban_monthly_2015a$day, sep="-"))
wet_WesternUrban_monthly_2015a <- select(wet_WesternUrban_monthly_2015a, date, Year, Month, day, Location)
wet_WesternUrban_monthly_2015 <- full_join(wet_WesternUrban_monthly_2015a, wet_WesternUrban_monthly_2015)
wet_WesternUrban_monthly <- rbind(select(wet_WesternUrban_monthly,date, Year, Month, day, Location, wet), wet_WesternUrban_monthly_2015)
rm(wet_WesternUrban_monthly_2015, wet_WesternUrban_monthly_2015a)
wet_WesternUrban_monthly$measurement <- "wet"
wet_WesternUrban_monthly <- rename(wet_WesternUrban_monthly, Value=wet)

#Merging in long format
wet_SL_monthly_district <- rbind(wet_Bo_monthly, wet_Bombali_monthly, wet_Bonthe_monthly,
                                 wet_Kailahun_monthly, wet_Kambia_monthly, wet_Kenema_monthly,
                                 wet_Koinadugu_monthly, wet_Kono_monthly, wet_Moyamba_monthly,
                                 wet_PortLoko_monthly, wet_Pujehun_monthly, wet_Tonkolili_monthly,
                                 wet_WesternRural_monthly, wet_WesternUrban_monthly)

#####################
#dtr - daily temperature range
#####################
dtr.full <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.dtr.dat.nc', write=FALSE)
dtr.var <- var.get.nc(dtr.full, "dtr")
#Bo
dtr_Bo_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                      Month=rep(seq(1,12,1), 4), day=1,
                                      dtr1=dtr.var[which(lon==11.75),which(lat==8.25),1:48],
                                      dtr2=dtr.var[which(lon==11.75),which(lat==7.75),1:48]))
dtr_Bo_monthly$dtr <- rowMeans(select(dtr_Bo_monthly, dtr1, dtr2))
dtr_Bo_monthly$Location <- 'Bo'
dtr_Bo_monthly$date <- ymd(paste(dtr_Bo_monthly$Year, dtr_Bo_monthly$Month, dtr_Bo_monthly$day, sep="-"))
dtr_Bo_monthly_2015 <- select(dtr_Bo_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Bo_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Bo_monthly_2015a$Location <- 'Bo'
dtr_Bo_monthly_2015a$date <- ymd(paste(dtr_Bo_monthly_2015a$Year, dtr_Bo_monthly_2015a$Month, dtr_Bo_monthly_2015a$day, sep="-"))
dtr_Bo_monthly_2015a <- select(dtr_Bo_monthly_2015a, date, Year, Month, day, Location)
dtr_Bo_monthly_2015 <- full_join(dtr_Bo_monthly_2015a, dtr_Bo_monthly_2015)
dtr_Bo_monthly <- rbind(select(dtr_Bo_monthly,date, Year, Month, day, Location, dtr), dtr_Bo_monthly_2015)
rm(dtr_Bo_monthly_2015, dtr_Bo_monthly_2015a)
dtr_Bo_monthly$measurement <- "dtr"
dtr_Bo_monthly <- rename(dtr_Bo_monthly, Value=dtr)

#Bombali
dtr_Bombali_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           dtr1=dtr.var[which(lon==12.25),which(lat==9.75),1:48],
                                           dtr2=dtr.var[which(lon==12.25),which(lat==9.25),1:48]))
dtr_Bombali_monthly$dtr <- rowMeans(select(dtr_Bombali_monthly, dtr1, dtr2))
dtr_Bombali_monthly$Location <- 'Bombali'
dtr_Bombali_monthly$date <- ymd(paste(dtr_Bombali_monthly$Year, dtr_Bombali_monthly$Month, dtr_Bombali_monthly$day, sep="-"))
dtr_Bombali_monthly_2015 <- select(dtr_Bombali_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Bombali_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Bombali_monthly_2015a$Location <- 'Bombali'
dtr_Bombali_monthly_2015a$date <- ymd(paste(dtr_Bombali_monthly_2015a$Year, dtr_Bombali_monthly_2015a$Month, dtr_Bombali_monthly_2015a$day, sep="-"))
dtr_Bombali_monthly_2015a <- select(dtr_Bombali_monthly_2015a, date, Year, Month, day, Location)
dtr_Bombali_monthly_2015 <- full_join(dtr_Bombali_monthly_2015a, dtr_Bombali_monthly_2015)
dtr_Bombali_monthly <- rbind(select(dtr_Bombali_monthly,date, Year, Month, day, Location, dtr), dtr_Bombali_monthly_2015)
rm(dtr_Bombali_monthly_2015, dtr_Bombali_monthly_2015a)
dtr_Bombali_monthly$measurement <- "dtr"
dtr_Bombali_monthly <- rename(dtr_Bombali_monthly, Value=dtr)

#Bonthe
dtr_Bonthe_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          dtr1=dtr.var[which(lon==12.75),which(lat==7.75),1:48],
                                          dtr2=dtr.var[which(lon==12.25),which(lat==7.75),1:48],
                                          dtr3=dtr.var[which(lon==12.25),which(lat==7.25),1:48]))
dtr_Bonthe_monthly$dtr <- rowMeans(select(dtr_Bonthe_monthly, dtr1, dtr2, dtr3))
dtr_Bonthe_monthly$Location <- 'Bonthe'
dtr_Bonthe_monthly$date <- ymd(paste(dtr_Bonthe_monthly$Year, dtr_Bonthe_monthly$Month, dtr_Bonthe_monthly$day, sep="-"))
dtr_Bonthe_monthly_2015 <- select(dtr_Bonthe_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Bonthe_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Bonthe_monthly_2015a$Location <- 'Bonthe'
dtr_Bonthe_monthly_2015a$date <- ymd(paste(dtr_Bonthe_monthly_2015a$Year, dtr_Bonthe_monthly_2015a$Month, dtr_Bonthe_monthly_2015a$day, sep="-"))
dtr_Bonthe_monthly_2015a <- select(dtr_Bonthe_monthly_2015a, date, Year, Month, day, Location)
dtr_Bonthe_monthly_2015 <- full_join(dtr_Bonthe_monthly_2015a, dtr_Bonthe_monthly_2015)
dtr_Bonthe_monthly <- rbind(select(dtr_Bonthe_monthly,date, Year, Month, day, Location, dtr), dtr_Bonthe_monthly_2015)
rm(dtr_Bonthe_monthly_2015, dtr_Bonthe_monthly_2015a)
dtr_Bonthe_monthly$measurement <- "dtr"
dtr_Bonthe_monthly <- rename(dtr_Bonthe_monthly, Value=dtr)

#Kailahun
dtr_Kailahun_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            dtr1=dtr.var[which(lon==10.75),which(lat==8.25),1:48],
                                            dtr2=dtr.var[which(lon==10.25),which(lat==8.25),1:48],
                                            dtr3=dtr.var[which(lon==10.75),which(lat==7.75),1:48]))
dtr_Kailahun_monthly$dtr <- rowMeans(select(dtr_Kailahun_monthly, dtr1, dtr2, dtr3))
dtr_Kailahun_monthly$Location <- 'Kailahun'
dtr_Kailahun_monthly$date <- ymd(paste(dtr_Kailahun_monthly$Year, dtr_Kailahun_monthly$Month, dtr_Kailahun_monthly$day, sep="-"))
dtr_Kailahun_monthly_2015 <- select(dtr_Kailahun_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Kailahun_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Kailahun_monthly_2015a$Location <- 'Kailahun'
dtr_Kailahun_monthly_2015a$date <- ymd(paste(dtr_Kailahun_monthly_2015a$Year, dtr_Kailahun_monthly_2015a$Month, dtr_Kailahun_monthly_2015a$day, sep="-"))
dtr_Kailahun_monthly_2015a <- select(dtr_Kailahun_monthly_2015a, date, Year, Month, day, Location)
dtr_Kailahun_monthly_2015 <- full_join(dtr_Kailahun_monthly_2015a, dtr_Kailahun_monthly_2015)
dtr_Kailahun_monthly <- rbind(select(dtr_Kailahun_monthly,date, Year, Month, day, Location, dtr), dtr_Kailahun_monthly_2015)
rm(dtr_Kailahun_monthly_2015, dtr_Kailahun_monthly_2015a)
dtr_Kailahun_monthly$measurement <- "dtr"
dtr_Kailahun_monthly <- rename(dtr_Kailahun_monthly, Value=dtr)

#Kambia
dtr_Kambia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          dtr=dtr.var[which(lon==12.75),which(lat==7.75),1:48]))
dtr_Kambia_monthly$Location <- 'Kambia'
dtr_Kambia_monthly$date <- ymd(paste(dtr_Kambia_monthly$Year, dtr_Kambia_monthly$Month, dtr_Kambia_monthly$day, sep="-"))
dtr_Kambia_monthly_2015 <- select(dtr_Kambia_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Kambia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Kambia_monthly_2015a$Location <- 'Kambia'
dtr_Kambia_monthly_2015a$date <- ymd(paste(dtr_Kambia_monthly_2015a$Year, dtr_Kambia_monthly_2015a$Month, dtr_Kambia_monthly_2015a$day, sep="-"))
dtr_Kambia_monthly_2015a <- select(dtr_Kambia_monthly_2015a, date, Year, Month, day, Location)
dtr_Kambia_monthly_2015 <- full_join(dtr_Kambia_monthly_2015a, dtr_Kambia_monthly_2015)
dtr_Kambia_monthly <- rbind(select(dtr_Kambia_monthly,date, Year, Month, day, Location, dtr), dtr_Kambia_monthly_2015)
rm(dtr_Kambia_monthly_2015, dtr_Kambia_monthly_2015a)
dtr_Kambia_monthly$measurement <- "dtr"
dtr_Kambia_monthly <- rename(dtr_Kambia_monthly, Value=dtr)

#Kenema
dtr_Kenema_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          dtr1=dtr.var[which(lon==11.25),which(lat==8.25),1:48],
                                          dtr2=dtr.var[which(lon==11.25),which(lat==7.75),1:48]))
dtr_Kenema_monthly$dtr <- rowMeans(select(dtr_Kenema_monthly, dtr1, dtr2))
dtr_Kenema_monthly$Location <- 'Kenema'
dtr_Kenema_monthly$date <- ymd(paste(dtr_Kenema_monthly$Year, dtr_Kenema_monthly$Month, dtr_Kenema_monthly$day, sep="-"))
dtr_Kenema_monthly_2015 <- select(dtr_Kenema_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Kenema_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Kenema_monthly_2015a$Location <- 'Kenema'
dtr_Kenema_monthly_2015a$date <- ymd(paste(dtr_Kenema_monthly_2015a$Year, dtr_Kenema_monthly_2015a$Month, dtr_Kenema_monthly_2015a$day, sep="-"))
dtr_Kenema_monthly_2015a <- select(dtr_Kenema_monthly_2015a, date, Year, Month, day, Location)
dtr_Kenema_monthly_2015 <- full_join(dtr_Kenema_monthly_2015a, dtr_Kenema_monthly_2015)
dtr_Kenema_monthly <- rbind(select(dtr_Kenema_monthly,date, Year, Month, day, Location, dtr), dtr_Kenema_monthly_2015)
rm(dtr_Kenema_monthly_2015, dtr_Kenema_monthly_2015a)
dtr_Kenema_monthly$measurement <- "dtr"
dtr_Kenema_monthly <- rename(dtr_Kenema_monthly, Value=dtr)

#Koinadugu
dtr_Koinadugu_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            dtr1=dtr.var[which(lon==11.75),which(lat==9.75),1:48],
                                            dtr2=dtr.var[which(lon==11.25),which(lat==9.75),1:48],
                                            dtr3=dtr.var[which(lon==11.75),which(lat==9.25),1:48],
                                            dtr4=dtr.var[which(lon==11.25),which(lat==9.25),1:48],
                                            dtr5=dtr.var[which(lon==10.75),which(lat==9.25),1:48]))
dtr_Koinadugu_monthly$dtr <- rowMeans(select(dtr_Koinadugu_monthly, dtr1, dtr2, dtr3, dtr4, dtr5))
dtr_Koinadugu_monthly$Location <- 'Koinadugu'
dtr_Koinadugu_monthly$date <- ymd(paste(dtr_Koinadugu_monthly$Year, dtr_Koinadugu_monthly$Month, dtr_Koinadugu_monthly$day, sep="-"))
dtr_Koinadugu_monthly_2015 <- select(dtr_Koinadugu_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Koinadugu_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Koinadugu_monthly_2015a$Location <- 'Koinadugu'
dtr_Koinadugu_monthly_2015a$date <- ymd(paste(dtr_Koinadugu_monthly_2015a$Year, dtr_Koinadugu_monthly_2015a$Month, dtr_Koinadugu_monthly_2015a$day, sep="-"))
dtr_Koinadugu_monthly_2015a <- select(dtr_Koinadugu_monthly_2015a, date, Year, Month, day, Location)
dtr_Koinadugu_monthly_2015 <- full_join(dtr_Koinadugu_monthly_2015a, dtr_Koinadugu_monthly_2015)
dtr_Koinadugu_monthly <- rbind(select(dtr_Koinadugu_monthly,date, Year, Month, day, Location, dtr), dtr_Koinadugu_monthly_2015)
rm(dtr_Koinadugu_monthly_2015, dtr_Koinadugu_monthly_2015a)
dtr_Koinadugu_monthly$measurement <- "dtr"
dtr_Koinadugu_monthly <- rename(dtr_Koinadugu_monthly, Value=dtr)

#Kono
dtr_Kono_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        dtr1=dtr.var[which(lon==11.25),which(lat==8.75),1:48],
                                        dtr2=dtr.var[which(lon==10.75),which(lat==8.75),1:48]))
dtr_Kono_monthly$dtr <- rowMeans(select(dtr_Kono_monthly, dtr1, dtr2))
dtr_Kono_monthly$Location <- 'Kono'
dtr_Kono_monthly$date <- ymd(paste(dtr_Kono_monthly$Year, dtr_Kono_monthly$Month, dtr_Kono_monthly$day, sep="-"))
dtr_Kono_monthly_2015 <- select(dtr_Kono_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Kono_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Kono_monthly_2015a$Location <- 'Kono'
dtr_Kono_monthly_2015a$date <- ymd(paste(dtr_Kono_monthly_2015a$Year, dtr_Kono_monthly_2015a$Month, dtr_Kono_monthly_2015a$day, sep="-"))
dtr_Kono_monthly_2015a <- select(dtr_Kono_monthly_2015a, date, Year, Month, day, Location)
dtr_Kono_monthly_2015 <- full_join(dtr_Kono_monthly_2015a, dtr_Kono_monthly_2015)
dtr_Kono_monthly <- rbind(select(dtr_Kono_monthly,date, Year, Month, day, Location, dtr), dtr_Kono_monthly_2015)
rm(dtr_Kono_monthly_2015, dtr_Kono_monthly_2015a)
dtr_Kono_monthly$measurement <- "dtr"
dtr_Kono_monthly <- rename(dtr_Kono_monthly, Value=dtr)

#Moyamba
dtr_Moyamba_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           dtr1=dtr.var[which(lon==12.75),which(lat==8.25),1:48],
                                           dtr2=dtr.var[which(lon==12.25),which(lat==8.25),1:48],
                                           dtr3=dtr.var[which(lon==11.75),which(lat==8.25),1:48]))
dtr_Moyamba_monthly$dtr <- rowMeans(select(dtr_Moyamba_monthly, dtr1, dtr2, dtr3))
dtr_Moyamba_monthly$Location <- 'Moyamba'
dtr_Moyamba_monthly$date <- ymd(paste(dtr_Moyamba_monthly$Year, dtr_Moyamba_monthly$Month, dtr_Moyamba_monthly$day, sep="-"))
dtr_Moyamba_monthly_2015 <- select(dtr_Moyamba_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Moyamba_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Moyamba_monthly_2015a$Location <- 'Moyamba'
dtr_Moyamba_monthly_2015a$date <- ymd(paste(dtr_Moyamba_monthly_2015a$Year, dtr_Moyamba_monthly_2015a$Month, dtr_Moyamba_monthly_2015a$day, sep="-"))
dtr_Moyamba_monthly_2015a <- select(dtr_Moyamba_monthly_2015a, date, Year, Month, day, Location)
dtr_Moyamba_monthly_2015 <- full_join(dtr_Moyamba_monthly_2015a, dtr_Moyamba_monthly_2015)
dtr_Moyamba_monthly <- rbind(select(dtr_Moyamba_monthly,date, Year, Month, day, Location, dtr), dtr_Moyamba_monthly_2015)
rm(dtr_Moyamba_monthly_2015, dtr_Moyamba_monthly_2015a)
dtr_Moyamba_monthly$measurement <- "dtr"
dtr_Moyamba_monthly <- rename(dtr_Moyamba_monthly, Value=dtr)

#Port Loko
dtr_PortLoko_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            dtr1=dtr.var[which(lon==13.25),which(lat==8.75),1:48],
                                            dtr2=dtr.var[which(lon==12.75),which(lat==8.75),1:48]))
dtr_PortLoko_monthly$dtr <- rowMeans(select(dtr_PortLoko_monthly, dtr1, dtr2))
dtr_PortLoko_monthly$Location <- 'PortLoko'
dtr_PortLoko_monthly$date <- ymd(paste(dtr_PortLoko_monthly$Year, dtr_PortLoko_monthly$Month, dtr_PortLoko_monthly$day, sep="-"))
dtr_PortLoko_monthly_2015 <- select(dtr_PortLoko_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_PortLoko_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_PortLoko_monthly_2015a$Location <- 'PortLoko'
dtr_PortLoko_monthly_2015a$date <- ymd(paste(dtr_PortLoko_monthly_2015a$Year, dtr_PortLoko_monthly_2015a$Month, dtr_PortLoko_monthly_2015a$day, sep="-"))
dtr_PortLoko_monthly_2015a <- select(dtr_PortLoko_monthly_2015a, date, Year, Month, day, Location)
dtr_PortLoko_monthly_2015 <- full_join(dtr_PortLoko_monthly_2015a, dtr_PortLoko_monthly_2015)
dtr_PortLoko_monthly <- rbind(select(dtr_PortLoko_monthly,date, Year, Month, day, Location, dtr), dtr_PortLoko_monthly_2015)
rm(dtr_PortLoko_monthly_2015, dtr_PortLoko_monthly_2015a)
dtr_PortLoko_monthly$measurement <- "dtr"
dtr_PortLoko_monthly <- rename(dtr_PortLoko_monthly, Value=dtr)

#Pujehun
dtr_Pujehun_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           dtr1=dtr.var[which(lon==11.75),which(lat==7.25),1:48],
                                           dtr2=dtr.var[which(lon==11.25),which(lat==7.25),1:48]))
dtr_Pujehun_monthly$dtr <- rowMeans(select(dtr_Pujehun_monthly, dtr1, dtr2))
dtr_Pujehun_monthly$Location <- 'Pujehun'
dtr_Pujehun_monthly$date <- ymd(paste(dtr_Pujehun_monthly$Year, dtr_Pujehun_monthly$Month, dtr_Pujehun_monthly$day, sep="-"))
dtr_Pujehun_monthly_2015 <- select(dtr_Pujehun_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Pujehun_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Pujehun_monthly_2015a$Location <- 'Pujehun'
dtr_Pujehun_monthly_2015a$date <- ymd(paste(dtr_Pujehun_monthly_2015a$Year, dtr_Pujehun_monthly_2015a$Month, dtr_Pujehun_monthly_2015a$day, sep="-"))
dtr_Pujehun_monthly_2015a <- select(dtr_Pujehun_monthly_2015a, date, Year, Month, day, Location)
dtr_Pujehun_monthly_2015 <- full_join(dtr_Pujehun_monthly_2015a, dtr_Pujehun_monthly_2015)
dtr_Pujehun_monthly <- rbind(select(dtr_Pujehun_monthly,date, Year, Month, day, Location, dtr), dtr_Pujehun_monthly_2015)
rm(dtr_Pujehun_monthly_2015, dtr_Pujehun_monthly_2015a)
dtr_Pujehun_monthly$measurement <- "dtr"
dtr_Pujehun_monthly <- rename(dtr_Pujehun_monthly, Value=dtr)

#Tonkolili
dtr_Tonkolili_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             dtr1=dtr.var[which(lon==12.25),which(lat==8.75),1:48],
                                             dtr2=dtr.var[which(lon==11.75),which(lat==8.75),1:48]))
dtr_Tonkolili_monthly$dtr <- rowMeans(select(dtr_Tonkolili_monthly, dtr1, dtr2))
dtr_Tonkolili_monthly$Location <- 'Tonkolili'
dtr_Tonkolili_monthly$date <- ymd(paste(dtr_Tonkolili_monthly$Year, dtr_Tonkolili_monthly$Month, dtr_Tonkolili_monthly$day, sep="-"))
dtr_Tonkolili_monthly_2015 <- select(dtr_Tonkolili_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Tonkolili_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Tonkolili_monthly_2015a$Location <- 'Tonkolili'
dtr_Tonkolili_monthly_2015a$date <- ymd(paste(dtr_Tonkolili_monthly_2015a$Year, dtr_Tonkolili_monthly_2015a$Month, dtr_Tonkolili_monthly_2015a$day, sep="-"))
dtr_Tonkolili_monthly_2015a <- select(dtr_Tonkolili_monthly_2015a, date, Year, Month, day, Location)
dtr_Tonkolili_monthly_2015 <- full_join(dtr_Tonkolili_monthly_2015a, dtr_Tonkolili_monthly_2015)
dtr_Tonkolili_monthly <- rbind(select(dtr_Tonkolili_monthly,date, Year, Month, day, Location, dtr), dtr_Tonkolili_monthly_2015)
rm(dtr_Tonkolili_monthly_2015, dtr_Tonkolili_monthly_2015a)
dtr_Tonkolili_monthly$measurement <- "dtr"
dtr_Tonkolili_monthly <- rename(dtr_Tonkolili_monthly, Value=dtr)

#Western rural
dtr_WesternRural_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                                Month=rep(seq(1,12,1), 4), day=1,
                                                dtr=dtr.var[which(lon==13.25),which(lat==8.25),1:48]))
dtr_WesternRural_monthly$Location <- 'WesternRural'
dtr_WesternRural_monthly$date <- ymd(paste(dtr_WesternRural_monthly$Year, dtr_WesternRural_monthly$Month, dtr_WesternRural_monthly$day, sep="-"))
dtr_WesternRural_monthly_2015 <- select(dtr_WesternRural_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_WesternRural_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_WesternRural_monthly_2015a$Location <- 'WesternRural'
dtr_WesternRural_monthly_2015a$date <- ymd(paste(dtr_WesternRural_monthly_2015a$Year, dtr_WesternRural_monthly_2015a$Month, dtr_WesternRural_monthly_2015a$day, sep="-"))
dtr_WesternRural_monthly_2015a <- select(dtr_WesternRural_monthly_2015a, date, Year, Month, day, Location)
dtr_WesternRural_monthly_2015 <- full_join(dtr_WesternRural_monthly_2015a, dtr_WesternRural_monthly_2015)
dtr_WesternRural_monthly <- rbind(select(dtr_WesternRural_monthly,date, Year, Month, day, Location, dtr), dtr_WesternRural_monthly_2015)
rm(dtr_WesternRural_monthly_2015, dtr_WesternRural_monthly_2015a)
dtr_WesternRural_monthly$measurement <- "dtr"
dtr_WesternRural_monthly <- rename(dtr_WesternRural_monthly, Value=dtr)

#Wester urban
dtr_WesternUrban_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                                Month=rep(seq(1,12,1), 4), day=1,
                                                dtr=dtr.var[which(lon==13.25),which(lat==8.25),1:48]))
dtr_WesternUrban_monthly$Location <- 'WesternUrban'
dtr_WesternUrban_monthly$date <- ymd(paste(dtr_WesternUrban_monthly$Year, dtr_WesternUrban_monthly$Month, dtr_WesternUrban_monthly$day, sep="-"))
dtr_WesternUrban_monthly_2015 <- select(dtr_WesternUrban_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_WesternUrban_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_WesternUrban_monthly_2015a$Location <- 'WesternUrban'
dtr_WesternUrban_monthly_2015a$date <- ymd(paste(dtr_WesternUrban_monthly_2015a$Year, dtr_WesternUrban_monthly_2015a$Month, dtr_WesternUrban_monthly_2015a$day, sep="-"))
dtr_WesternUrban_monthly_2015a <- select(dtr_WesternUrban_monthly_2015a, date, Year, Month, day, Location)
dtr_WesternUrban_monthly_2015 <- full_join(dtr_WesternUrban_monthly_2015a, dtr_WesternUrban_monthly_2015)
dtr_WesternUrban_monthly <- rbind(select(dtr_WesternUrban_monthly,date, Year, Month, day, Location, dtr), dtr_WesternUrban_monthly_2015)
rm(dtr_WesternUrban_monthly_2015, dtr_WesternUrban_monthly_2015a)
dtr_WesternUrban_monthly$measurement <- "dtr"
dtr_WesternUrban_monthly <- rename(dtr_WesternUrban_monthly, Value=dtr)

#Merging in long format
dtr_SL_monthly_district <- rbind(dtr_Bo_monthly, dtr_Bombali_monthly, dtr_Bonthe_monthly,
                                 dtr_Kailahun_monthly, dtr_Kambia_monthly, dtr_Kenema_monthly,
                                 dtr_Koinadugu_monthly, dtr_Kono_monthly, dtr_Moyamba_monthly,
                                 dtr_PortLoko_monthly, dtr_Pujehun_monthly, dtr_Tonkolili_monthly,
                                 dtr_WesternRural_monthly, dtr_WesternUrban_monthly)

#####################
#pet - potential evapotranspiration
#####################
pet.full <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.pet.dat.nc', write=FALSE)
pet.var <- var.get.nc(pet.full, "pet")

#Bo
pet_Bo_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                      Month=rep(seq(1,12,1), 4), day=1,
                                      pet1=pet.var[which(lon==11.75),which(lat==8.25),1:48],
                                      pet2=pet.var[which(lon==11.75),which(lat==7.75),1:48]))
pet_Bo_monthly$pet <- rowMeans(select(pet_Bo_monthly, pet1, pet2))
pet_Bo_monthly$Location <- 'Bo'
pet_Bo_monthly$date <- ymd(paste(pet_Bo_monthly$Year, pet_Bo_monthly$Month, pet_Bo_monthly$day, sep="-"))
pet_Bo_monthly_2015 <- select(pet_Bo_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Bo_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Bo_monthly_2015a$Location <- 'Bo'
pet_Bo_monthly_2015a$date <- ymd(paste(pet_Bo_monthly_2015a$Year, pet_Bo_monthly_2015a$Month, pet_Bo_monthly_2015a$day, sep="-"))
pet_Bo_monthly_2015a <- select(pet_Bo_monthly_2015a, date, Year, Month, day, Location)
pet_Bo_monthly_2015 <- full_join(pet_Bo_monthly_2015a, pet_Bo_monthly_2015)
pet_Bo_monthly <- rbind(select(pet_Bo_monthly,date, Year, Month, day, Location, pet), pet_Bo_monthly_2015)
rm(pet_Bo_monthly_2015, pet_Bo_monthly_2015a)
pet_Bo_monthly$measurement <- "pet"
pet_Bo_monthly <- rename(pet_Bo_monthly, Value=pet)

#Bombali
pet_Bombali_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           pet1=pet.var[which(lon==12.25),which(lat==9.75),1:48],
                                           pet2=pet.var[which(lon==12.25),which(lat==9.25),1:48]))
pet_Bombali_monthly$pet <- rowMeans(select(pet_Bombali_monthly, pet1, pet2))
pet_Bombali_monthly$Location <- 'Bombali'
pet_Bombali_monthly$date <- ymd(paste(pet_Bombali_monthly$Year, pet_Bombali_monthly$Month, pet_Bombali_monthly$day, sep="-"))
pet_Bombali_monthly_2015 <- select(pet_Bombali_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Bombali_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Bombali_monthly_2015a$Location <- 'Bombali'
pet_Bombali_monthly_2015a$date <- ymd(paste(pet_Bombali_monthly_2015a$Year, pet_Bombali_monthly_2015a$Month, pet_Bombali_monthly_2015a$day, sep="-"))
pet_Bombali_monthly_2015a <- select(pet_Bombali_monthly_2015a, date, Year, Month, day, Location)
pet_Bombali_monthly_2015 <- full_join(pet_Bombali_monthly_2015a, pet_Bombali_monthly_2015)
pet_Bombali_monthly <- rbind(select(pet_Bombali_monthly,date, Year, Month, day, Location, pet), pet_Bombali_monthly_2015)
rm(pet_Bombali_monthly_2015, pet_Bombali_monthly_2015a)
pet_Bombali_monthly$measurement <- "pet"
pet_Bombali_monthly <- rename(pet_Bombali_monthly, Value=pet)

#Bonthe
pet_Bonthe_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          pet1=pet.var[which(lon==12.75),which(lat==7.75),1:48],
                                          pet2=pet.var[which(lon==12.25),which(lat==7.75),1:48],
                                          pet3=pet.var[which(lon==12.25),which(lat==7.25),1:48]))
pet_Bonthe_monthly$pet <- rowMeans(select(pet_Bonthe_monthly, pet1, pet2, pet3))
pet_Bonthe_monthly$Location <- 'Bonthe'
pet_Bonthe_monthly$date <- ymd(paste(pet_Bonthe_monthly$Year, pet_Bonthe_monthly$Month, pet_Bonthe_monthly$day, sep="-"))
pet_Bonthe_monthly_2015 <- select(pet_Bonthe_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Bonthe_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Bonthe_monthly_2015a$Location <- 'Bonthe'
pet_Bonthe_monthly_2015a$date <- ymd(paste(pet_Bonthe_monthly_2015a$Year, pet_Bonthe_monthly_2015a$Month, pet_Bonthe_monthly_2015a$day, sep="-"))
pet_Bonthe_monthly_2015a <- select(pet_Bonthe_monthly_2015a, date, Year, Month, day, Location)
pet_Bonthe_monthly_2015 <- full_join(pet_Bonthe_monthly_2015a, pet_Bonthe_monthly_2015)
pet_Bonthe_monthly <- rbind(select(pet_Bonthe_monthly,date, Year, Month, day, Location, pet), pet_Bonthe_monthly_2015)
rm(pet_Bonthe_monthly_2015, pet_Bonthe_monthly_2015a)
pet_Bonthe_monthly$measurement <- "pet"
pet_Bonthe_monthly <- rename(pet_Bonthe_monthly, Value=pet)

#Kailahun
pet_Kailahun_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            pet1=pet.var[which(lon==10.75),which(lat==8.25),1:48],
                                            pet2=pet.var[which(lon==10.25),which(lat==8.25),1:48],
                                            pet3=pet.var[which(lon==10.75),which(lat==7.75),1:48]))
pet_Kailahun_monthly$pet <- rowMeans(select(pet_Kailahun_monthly, pet1, pet2, pet3))
pet_Kailahun_monthly$Location <- 'Kailahun'
pet_Kailahun_monthly$date <- ymd(paste(pet_Kailahun_monthly$Year, pet_Kailahun_monthly$Month, pet_Kailahun_monthly$day, sep="-"))
pet_Kailahun_monthly_2015 <- select(pet_Kailahun_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Kailahun_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Kailahun_monthly_2015a$Location <- 'Kailahun'
pet_Kailahun_monthly_2015a$date <- ymd(paste(pet_Kailahun_monthly_2015a$Year, pet_Kailahun_monthly_2015a$Month, pet_Kailahun_monthly_2015a$day, sep="-"))
pet_Kailahun_monthly_2015a <- select(pet_Kailahun_monthly_2015a, date, Year, Month, day, Location)
pet_Kailahun_monthly_2015 <- full_join(pet_Kailahun_monthly_2015a, pet_Kailahun_monthly_2015)
pet_Kailahun_monthly <- rbind(select(pet_Kailahun_monthly,date, Year, Month, day, Location, pet), pet_Kailahun_monthly_2015)
rm(pet_Kailahun_monthly_2015, pet_Kailahun_monthly_2015a)
pet_Kailahun_monthly$measurement <- "pet"
pet_Kailahun_monthly <- rename(pet_Kailahun_monthly, Value=pet)

#Kambia
pet_Kambia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          pet=pet.var[which(lon==12.75),which(lat==7.75),1:48]))
pet_Kambia_monthly$Location <- 'Kambia'
pet_Kambia_monthly$date <- ymd(paste(pet_Kambia_monthly$Year, pet_Kambia_monthly$Month, pet_Kambia_monthly$day, sep="-"))
pet_Kambia_monthly_2015 <- select(pet_Kambia_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Kambia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Kambia_monthly_2015a$Location <- 'Kambia'
pet_Kambia_monthly_2015a$date <- ymd(paste(pet_Kambia_monthly_2015a$Year, pet_Kambia_monthly_2015a$Month, pet_Kambia_monthly_2015a$day, sep="-"))
pet_Kambia_monthly_2015a <- select(pet_Kambia_monthly_2015a, date, Year, Month, day, Location)
pet_Kambia_monthly_2015 <- full_join(pet_Kambia_monthly_2015a, pet_Kambia_monthly_2015)
pet_Kambia_monthly <- rbind(select(pet_Kambia_monthly,date, Year, Month, day, Location, pet), pet_Kambia_monthly_2015)
rm(pet_Kambia_monthly_2015, pet_Kambia_monthly_2015a)
pet_Kambia_monthly$measurement <- "pet"
pet_Kambia_monthly <- rename(pet_Kambia_monthly, Value=pet)

#Kenema
pet_Kenema_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          pet1=pet.var[which(lon==11.25),which(lat==8.25),1:48],
                                          pet2=pet.var[which(lon==11.25),which(lat==7.75),1:48]))
pet_Kenema_monthly$pet <- rowMeans(select(pet_Kenema_monthly, pet1, pet2))
pet_Kenema_monthly$Location <- 'Kenema'
pet_Kenema_monthly$date <- ymd(paste(pet_Kenema_monthly$Year, pet_Kenema_monthly$Month, pet_Kenema_monthly$day, sep="-"))
pet_Kenema_monthly_2015 <- select(pet_Kenema_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Kenema_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Kenema_monthly_2015a$Location <- 'Kenema'
pet_Kenema_monthly_2015a$date <- ymd(paste(pet_Kenema_monthly_2015a$Year, pet_Kenema_monthly_2015a$Month, pet_Kenema_monthly_2015a$day, sep="-"))
pet_Kenema_monthly_2015a <- select(pet_Kenema_monthly_2015a, date, Year, Month, day, Location)
pet_Kenema_monthly_2015 <- full_join(pet_Kenema_monthly_2015a, pet_Kenema_monthly_2015)
pet_Kenema_monthly <- rbind(select(pet_Kenema_monthly,date, Year, Month, day, Location, pet), pet_Kenema_monthly_2015)
rm(pet_Kenema_monthly_2015, pet_Kenema_monthly_2015a)
pet_Kenema_monthly$measurement <- "pet"
pet_Kenema_monthly <- rename(pet_Kenema_monthly, Value=pet)

#Koinadugu
pet_Koinadugu_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            pet1=pet.var[which(lon==11.75),which(lat==9.75),1:48],
                                            pet2=pet.var[which(lon==11.25),which(lat==9.75),1:48],
                                            pet3=pet.var[which(lon==11.75),which(lat==9.25),1:48],
                                            pet4=pet.var[which(lon==11.25),which(lat==9.25),1:48],
                                            pet5=pet.var[which(lon==10.75),which(lat==9.25),1:48]))
pet_Koinadugu_monthly$pet <- rowMeans(select(pet_Koinadugu_monthly, pet1, pet2, pet3, pet4, pet5))
pet_Koinadugu_monthly$Location <- 'Koinadugu'
pet_Koinadugu_monthly$date <- ymd(paste(pet_Koinadugu_monthly$Year, pet_Koinadugu_monthly$Month, pet_Koinadugu_monthly$day, sep="-"))
pet_Koinadugu_monthly_2015 <- select(pet_Koinadugu_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Koinadugu_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Koinadugu_monthly_2015a$Location <- 'Koinadugu'
pet_Koinadugu_monthly_2015a$date <- ymd(paste(pet_Koinadugu_monthly_2015a$Year, pet_Koinadugu_monthly_2015a$Month, pet_Koinadugu_monthly_2015a$day, sep="-"))
pet_Koinadugu_monthly_2015a <- select(pet_Koinadugu_monthly_2015a, date, Year, Month, day, Location)
pet_Koinadugu_monthly_2015 <- full_join(pet_Koinadugu_monthly_2015a, pet_Koinadugu_monthly_2015)
pet_Koinadugu_monthly <- rbind(select(pet_Koinadugu_monthly,date, Year, Month, day, Location, pet), pet_Koinadugu_monthly_2015)
rm(pet_Koinadugu_monthly_2015, pet_Koinadugu_monthly_2015a)
pet_Koinadugu_monthly$measurement <- "pet"
pet_Koinadugu_monthly <- rename(pet_Koinadugu_monthly, Value=pet)

#Kono
pet_Kono_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        pet1=pet.var[which(lon==11.25),which(lat==8.75),1:48],
                                        pet2=pet.var[which(lon==10.75),which(lat==8.75),1:48]))
pet_Kono_monthly$pet <- rowMeans(select(pet_Kono_monthly, pet1, pet2))
pet_Kono_monthly$Location <- 'Kono'
pet_Kono_monthly$date <- ymd(paste(pet_Kono_monthly$Year, pet_Kono_monthly$Month, pet_Kono_monthly$day, sep="-"))
pet_Kono_monthly_2015 <- select(pet_Kono_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Kono_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Kono_monthly_2015a$Location <- 'Kono'
pet_Kono_monthly_2015a$date <- ymd(paste(pet_Kono_monthly_2015a$Year, pet_Kono_monthly_2015a$Month, pet_Kono_monthly_2015a$day, sep="-"))
pet_Kono_monthly_2015a <- select(pet_Kono_monthly_2015a, date, Year, Month, day, Location)
pet_Kono_monthly_2015 <- full_join(pet_Kono_monthly_2015a, pet_Kono_monthly_2015)
pet_Kono_monthly <- rbind(select(pet_Kono_monthly,date, Year, Month, day, Location, pet), pet_Kono_monthly_2015)
rm(pet_Kono_monthly_2015, pet_Kono_monthly_2015a)
pet_Kono_monthly$measurement <- "pet"
pet_Kono_monthly <- rename(pet_Kono_monthly, Value=pet)

#Moyamba
pet_Moyamba_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           pet1=pet.var[which(lon==12.75),which(lat==8.25),1:48],
                                           pet2=pet.var[which(lon==12.25),which(lat==8.25),1:48],
                                           pet3=pet.var[which(lon==11.75),which(lat==8.25),1:48]))
pet_Moyamba_monthly$pet <- rowMeans(select(pet_Moyamba_monthly, pet1, pet2, pet3))
pet_Moyamba_monthly$Location <- 'Moyamba'
pet_Moyamba_monthly$date <- ymd(paste(pet_Moyamba_monthly$Year, pet_Moyamba_monthly$Month, pet_Moyamba_monthly$day, sep="-"))
pet_Moyamba_monthly_2015 <- select(pet_Moyamba_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Moyamba_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Moyamba_monthly_2015a$Location <- 'Moyamba'
pet_Moyamba_monthly_2015a$date <- ymd(paste(pet_Moyamba_monthly_2015a$Year, pet_Moyamba_monthly_2015a$Month, pet_Moyamba_monthly_2015a$day, sep="-"))
pet_Moyamba_monthly_2015a <- select(pet_Moyamba_monthly_2015a, date, Year, Month, day, Location)
pet_Moyamba_monthly_2015 <- full_join(pet_Moyamba_monthly_2015a, pet_Moyamba_monthly_2015)
pet_Moyamba_monthly <- rbind(select(pet_Moyamba_monthly,date, Year, Month, day, Location, pet), pet_Moyamba_monthly_2015)
rm(pet_Moyamba_monthly_2015, pet_Moyamba_monthly_2015a)
pet_Moyamba_monthly$measurement <- "pet"
pet_Moyamba_monthly <- rename(pet_Moyamba_monthly, Value=pet)

#Port Loko
pet_PortLoko_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            pet1=pet.var[which(lon==13.25),which(lat==8.75),1:48],
                                            pet2=pet.var[which(lon==12.75),which(lat==8.75),1:48]))
pet_PortLoko_monthly$pet <- rowMeans(select(pet_PortLoko_monthly, pet1, pet2))
pet_PortLoko_monthly$Location <- 'PortLoko'
pet_PortLoko_monthly$date <- ymd(paste(pet_PortLoko_monthly$Year, pet_PortLoko_monthly$Month, pet_PortLoko_monthly$day, sep="-"))
pet_PortLoko_monthly_2015 <- select(pet_PortLoko_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_PortLoko_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_PortLoko_monthly_2015a$Location <- 'PortLoko'
pet_PortLoko_monthly_2015a$date <- ymd(paste(pet_PortLoko_monthly_2015a$Year, pet_PortLoko_monthly_2015a$Month, pet_PortLoko_monthly_2015a$day, sep="-"))
pet_PortLoko_monthly_2015a <- select(pet_PortLoko_monthly_2015a, date, Year, Month, day, Location)
pet_PortLoko_monthly_2015 <- full_join(pet_PortLoko_monthly_2015a, pet_PortLoko_monthly_2015)
pet_PortLoko_monthly <- rbind(select(pet_PortLoko_monthly,date, Year, Month, day, Location, pet), pet_PortLoko_monthly_2015)
rm(pet_PortLoko_monthly_2015, pet_PortLoko_monthly_2015a)
pet_PortLoko_monthly$measurement <- "pet"
pet_PortLoko_monthly <- rename(pet_PortLoko_monthly, Value=pet)

#Pujehun
pet_Pujehun_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           pet1=pet.var[which(lon==11.75),which(lat==7.25),1:48],
                                           pet2=pet.var[which(lon==11.25),which(lat==7.25),1:48]))
pet_Pujehun_monthly$pet <- rowMeans(select(pet_Pujehun_monthly, pet1, pet2))
pet_Pujehun_monthly$Location <- 'Pujehun'
pet_Pujehun_monthly$date <- ymd(paste(pet_Pujehun_monthly$Year, pet_Pujehun_monthly$Month, pet_Pujehun_monthly$day, sep="-"))
pet_Pujehun_monthly_2015 <- select(pet_Pujehun_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Pujehun_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Pujehun_monthly_2015a$Location <- 'Pujehun'
pet_Pujehun_monthly_2015a$date <- ymd(paste(pet_Pujehun_monthly_2015a$Year, pet_Pujehun_monthly_2015a$Month, pet_Pujehun_monthly_2015a$day, sep="-"))
pet_Pujehun_monthly_2015a <- select(pet_Pujehun_monthly_2015a, date, Year, Month, day, Location)
pet_Pujehun_monthly_2015 <- full_join(pet_Pujehun_monthly_2015a, pet_Pujehun_monthly_2015)
pet_Pujehun_monthly <- rbind(select(pet_Pujehun_monthly,date, Year, Month, day, Location, pet), pet_Pujehun_monthly_2015)
rm(pet_Pujehun_monthly_2015, pet_Pujehun_monthly_2015a)
pet_Pujehun_monthly$measurement <- "pet"
pet_Pujehun_monthly <- rename(pet_Pujehun_monthly, Value=pet)

#Tonkolili
pet_Tonkolili_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             pet1=pet.var[which(lon==12.25),which(lat==8.75),1:48],
                                             pet2=pet.var[which(lon==11.75),which(lat==8.75),1:48]))
pet_Tonkolili_monthly$pet <- rowMeans(select(pet_Tonkolili_monthly, pet1, pet2))
pet_Tonkolili_monthly$Location <- 'Tonkolili'
pet_Tonkolili_monthly$date <- ymd(paste(pet_Tonkolili_monthly$Year, pet_Tonkolili_monthly$Month, pet_Tonkolili_monthly$day, sep="-"))
pet_Tonkolili_monthly_2015 <- select(pet_Tonkolili_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Tonkolili_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Tonkolili_monthly_2015a$Location <- 'Tonkolili'
pet_Tonkolili_monthly_2015a$date <- ymd(paste(pet_Tonkolili_monthly_2015a$Year, pet_Tonkolili_monthly_2015a$Month, pet_Tonkolili_monthly_2015a$day, sep="-"))
pet_Tonkolili_monthly_2015a <- select(pet_Tonkolili_monthly_2015a, date, Year, Month, day, Location)
pet_Tonkolili_monthly_2015 <- full_join(pet_Tonkolili_monthly_2015a, pet_Tonkolili_monthly_2015)
pet_Tonkolili_monthly <- rbind(select(pet_Tonkolili_monthly,date, Year, Month, day, Location, pet), pet_Tonkolili_monthly_2015)
rm(pet_Tonkolili_monthly_2015, pet_Tonkolili_monthly_2015a)
pet_Tonkolili_monthly$measurement <- "pet"
pet_Tonkolili_monthly <- rename(pet_Tonkolili_monthly, Value=pet)

#Western rural
pet_WesternRural_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                                Month=rep(seq(1,12,1), 4), day=1,
                                                pet=pet.var[which(lon==13.25),which(lat==8.25),1:48]))
pet_WesternRural_monthly$Location <- 'WesternRural'
pet_WesternRural_monthly$date <- ymd(paste(pet_WesternRural_monthly$Year, pet_WesternRural_monthly$Month, pet_WesternRural_monthly$day, sep="-"))
pet_WesternRural_monthly_2015 <- select(pet_WesternRural_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_WesternRural_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_WesternRural_monthly_2015a$Location <- 'WesternRural'
pet_WesternRural_monthly_2015a$date <- ymd(paste(pet_WesternRural_monthly_2015a$Year, pet_WesternRural_monthly_2015a$Month, pet_WesternRural_monthly_2015a$day, sep="-"))
pet_WesternRural_monthly_2015a <- select(pet_WesternRural_monthly_2015a, date, Year, Month, day, Location)
pet_WesternRural_monthly_2015 <- full_join(pet_WesternRural_monthly_2015a, pet_WesternRural_monthly_2015)
pet_WesternRural_monthly <- rbind(select(pet_WesternRural_monthly,date, Year, Month, day, Location, pet), pet_WesternRural_monthly_2015)
rm(pet_WesternRural_monthly_2015, pet_WesternRural_monthly_2015a)
pet_WesternRural_monthly$measurement <- "pet"
pet_WesternRural_monthly <- rename(pet_WesternRural_monthly, Value=pet)

#Wester urban
pet_WesternUrban_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                                Month=rep(seq(1,12,1), 4), day=1,
                                                pet=pet.var[which(lon==13.25),which(lat==8.25),1:48]))
pet_WesternUrban_monthly$Location <- 'WesternUrban'
pet_WesternUrban_monthly$date <- ymd(paste(pet_WesternUrban_monthly$Year, pet_WesternUrban_monthly$Month, pet_WesternUrban_monthly$day, sep="-"))
pet_WesternUrban_monthly_2015 <- select(pet_WesternUrban_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_WesternUrban_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_WesternUrban_monthly_2015a$Location <- 'WesternUrban'
pet_WesternUrban_monthly_2015a$date <- ymd(paste(pet_WesternUrban_monthly_2015a$Year, pet_WesternUrban_monthly_2015a$Month, pet_WesternUrban_monthly_2015a$day, sep="-"))
pet_WesternUrban_monthly_2015a <- select(pet_WesternUrban_monthly_2015a, date, Year, Month, day, Location)
pet_WesternUrban_monthly_2015 <- full_join(pet_WesternUrban_monthly_2015a, pet_WesternUrban_monthly_2015)
pet_WesternUrban_monthly <- rbind(select(pet_WesternUrban_monthly,date, Year, Month, day, Location, pet), pet_WesternUrban_monthly_2015)
rm(pet_WesternUrban_monthly_2015, pet_WesternUrban_monthly_2015a)
pet_WesternUrban_monthly$measurement <- "pet"
pet_WesternUrban_monthly <- rename(pet_WesternUrban_monthly, Value=pet)

#Merging in long format
pet_SL_monthly_district <- rbind(pet_Bo_monthly, pet_Bombali_monthly, pet_Bonthe_monthly,
                                 pet_Kailahun_monthly, pet_Kambia_monthly, pet_Kenema_monthly,
                                 pet_Koinadugu_monthly, pet_Kono_monthly, pet_Moyamba_monthly,
                                 pet_PortLoko_monthly, pet_Pujehun_monthly, pet_Tonkolili_monthly,
                                 pet_WesternRural_monthly, pet_WesternUrban_monthly)

#####################
#pre - precipitation
#####################
pre.full <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.pre.dat.nc', write=FALSE)
pre.var <- var.get.nc(pre.full, "pre")
#Bo
pre_Bo_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                      Month=rep(seq(1,12,1), 4), day=1,
                                      pre1=pre.var[which(lon==11.75),which(lat==8.25),1:48],
                                      pre2=pre.var[which(lon==11.75),which(lat==7.75),1:48]))
pre_Bo_monthly$pre <- rowMeans(select(pre_Bo_monthly, pre1, pre2))
pre_Bo_monthly$Location <- 'Bo'
pre_Bo_monthly$date <- ymd(paste(pre_Bo_monthly$Year, pre_Bo_monthly$Month, pre_Bo_monthly$day, sep="-"))
pre_Bo_monthly_2015 <- select(pre_Bo_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Bo_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Bo_monthly_2015a$Location <- 'Bo'
pre_Bo_monthly_2015a$date <- ymd(paste(pre_Bo_monthly_2015a$Year, pre_Bo_monthly_2015a$Month, pre_Bo_monthly_2015a$day, sep="-"))
pre_Bo_monthly_2015a <- select(pre_Bo_monthly_2015a, date, Year, Month, day, Location)
pre_Bo_monthly_2015 <- full_join(pre_Bo_monthly_2015a, pre_Bo_monthly_2015)
pre_Bo_monthly <- rbind(select(pre_Bo_monthly,date, Year, Month, day, Location, pre), pre_Bo_monthly_2015)
rm(pre_Bo_monthly_2015, pre_Bo_monthly_2015a)
pre_Bo_monthly$measurement <- "pre"
pre_Bo_monthly <- rename(pre_Bo_monthly, Value=pre)

#Bombali
pre_Bombali_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           pre1=pre.var[which(lon==12.25),which(lat==9.75),1:48],
                                           pre2=pre.var[which(lon==12.25),which(lat==9.25),1:48]))
pre_Bombali_monthly$pre <- rowMeans(select(pre_Bombali_monthly, pre1, pre2))
pre_Bombali_monthly$Location <- 'Bombali'
pre_Bombali_monthly$date <- ymd(paste(pre_Bombali_monthly$Year, pre_Bombali_monthly$Month, pre_Bombali_monthly$day, sep="-"))
pre_Bombali_monthly_2015 <- select(pre_Bombali_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Bombali_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Bombali_monthly_2015a$Location <- 'Bombali'
pre_Bombali_monthly_2015a$date <- ymd(paste(pre_Bombali_monthly_2015a$Year, pre_Bombali_monthly_2015a$Month, pre_Bombali_monthly_2015a$day, sep="-"))
pre_Bombali_monthly_2015a <- select(pre_Bombali_monthly_2015a, date, Year, Month, day, Location)
pre_Bombali_monthly_2015 <- full_join(pre_Bombali_monthly_2015a, pre_Bombali_monthly_2015)
pre_Bombali_monthly <- rbind(select(pre_Bombali_monthly,date, Year, Month, day, Location, pre), pre_Bombali_monthly_2015)
rm(pre_Bombali_monthly_2015, pre_Bombali_monthly_2015a)
pre_Bombali_monthly$measurement <- "pre"
pre_Bombali_monthly <- rename(pre_Bombali_monthly, Value=pre)

#Bonthe
pre_Bonthe_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          pre1=pre.var[which(lon==12.75),which(lat==7.75),1:48],
                                          pre2=pre.var[which(lon==12.25),which(lat==7.75),1:48],
                                          pre3=pre.var[which(lon==12.25),which(lat==7.25),1:48]))
pre_Bonthe_monthly$pre <- rowMeans(select(pre_Bonthe_monthly, pre1, pre2, pre3))
pre_Bonthe_monthly$Location <- 'Bonthe'
pre_Bonthe_monthly$date <- ymd(paste(pre_Bonthe_monthly$Year, pre_Bonthe_monthly$Month, pre_Bonthe_monthly$day, sep="-"))
pre_Bonthe_monthly_2015 <- select(pre_Bonthe_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Bonthe_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Bonthe_monthly_2015a$Location <- 'Bonthe'
pre_Bonthe_monthly_2015a$date <- ymd(paste(pre_Bonthe_monthly_2015a$Year, pre_Bonthe_monthly_2015a$Month, pre_Bonthe_monthly_2015a$day, sep="-"))
pre_Bonthe_monthly_2015a <- select(pre_Bonthe_monthly_2015a, date, Year, Month, day, Location)
pre_Bonthe_monthly_2015 <- full_join(pre_Bonthe_monthly_2015a, pre_Bonthe_monthly_2015)
pre_Bonthe_monthly <- rbind(select(pre_Bonthe_monthly,date, Year, Month, day, Location, pre), pre_Bonthe_monthly_2015)
rm(pre_Bonthe_monthly_2015, pre_Bonthe_monthly_2015a)
pre_Bonthe_monthly$measurement <- "pre"
pre_Bonthe_monthly <- rename(pre_Bonthe_monthly, Value=pre)

#Kailahun
pre_Kailahun_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            pre1=pre.var[which(lon==10.75),which(lat==8.25),1:48],
                                            pre2=pre.var[which(lon==10.25),which(lat==8.25),1:48],
                                            pre3=pre.var[which(lon==10.75),which(lat==7.75),1:48]))
pre_Kailahun_monthly$pre <- rowMeans(select(pre_Kailahun_monthly, pre1, pre2, pre3))
pre_Kailahun_monthly$Location <- 'Kailahun'
pre_Kailahun_monthly$date <- ymd(paste(pre_Kailahun_monthly$Year, pre_Kailahun_monthly$Month, pre_Kailahun_monthly$day, sep="-"))
pre_Kailahun_monthly_2015 <- select(pre_Kailahun_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Kailahun_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Kailahun_monthly_2015a$Location <- 'Kailahun'
pre_Kailahun_monthly_2015a$date <- ymd(paste(pre_Kailahun_monthly_2015a$Year, pre_Kailahun_monthly_2015a$Month, pre_Kailahun_monthly_2015a$day, sep="-"))
pre_Kailahun_monthly_2015a <- select(pre_Kailahun_monthly_2015a, date, Year, Month, day, Location)
pre_Kailahun_monthly_2015 <- full_join(pre_Kailahun_monthly_2015a, pre_Kailahun_monthly_2015)
pre_Kailahun_monthly <- rbind(select(pre_Kailahun_monthly,date, Year, Month, day, Location, pre), pre_Kailahun_monthly_2015)
rm(pre_Kailahun_monthly_2015, pre_Kailahun_monthly_2015a)
pre_Kailahun_monthly$measurement <- "pre"
pre_Kailahun_monthly <- rename(pre_Kailahun_monthly, Value=pre)

#Kambia
pre_Kambia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          pre=pre.var[which(lon==12.75),which(lat==7.75),1:48]))
pre_Kambia_monthly$Location <- 'Kambia'
pre_Kambia_monthly$date <- ymd(paste(pre_Kambia_monthly$Year, pre_Kambia_monthly$Month, pre_Kambia_monthly$day, sep="-"))
pre_Kambia_monthly_2015 <- select(pre_Kambia_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Kambia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Kambia_monthly_2015a$Location <- 'Kambia'
pre_Kambia_monthly_2015a$date <- ymd(paste(pre_Kambia_monthly_2015a$Year, pre_Kambia_monthly_2015a$Month, pre_Kambia_monthly_2015a$day, sep="-"))
pre_Kambia_monthly_2015a <- select(pre_Kambia_monthly_2015a, date, Year, Month, day, Location)
pre_Kambia_monthly_2015 <- full_join(pre_Kambia_monthly_2015a, pre_Kambia_monthly_2015)
pre_Kambia_monthly <- rbind(select(pre_Kambia_monthly,date, Year, Month, day, Location, pre), pre_Kambia_monthly_2015)
rm(pre_Kambia_monthly_2015, pre_Kambia_monthly_2015a)
pre_Kambia_monthly$measurement <- "pre"
pre_Kambia_monthly <- rename(pre_Kambia_monthly, Value=pre)

#Kenema
pre_Kenema_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          pre1=pre.var[which(lon==11.25),which(lat==8.25),1:48],
                                          pre2=pre.var[which(lon==11.25),which(lat==7.75),1:48]))
pre_Kenema_monthly$pre <- rowMeans(select(pre_Kenema_monthly, pre1, pre2))
pre_Kenema_monthly$Location <- 'Kenema'
pre_Kenema_monthly$date <- ymd(paste(pre_Kenema_monthly$Year, pre_Kenema_monthly$Month, pre_Kenema_monthly$day, sep="-"))
pre_Kenema_monthly_2015 <- select(pre_Kenema_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Kenema_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Kenema_monthly_2015a$Location <- 'Kenema'
pre_Kenema_monthly_2015a$date <- ymd(paste(pre_Kenema_monthly_2015a$Year, pre_Kenema_monthly_2015a$Month, pre_Kenema_monthly_2015a$day, sep="-"))
pre_Kenema_monthly_2015a <- select(pre_Kenema_monthly_2015a, date, Year, Month, day, Location)
pre_Kenema_monthly_2015 <- full_join(pre_Kenema_monthly_2015a, pre_Kenema_monthly_2015)
pre_Kenema_monthly <- rbind(select(pre_Kenema_monthly,date, Year, Month, day, Location, pre), pre_Kenema_monthly_2015)
rm(pre_Kenema_monthly_2015, pre_Kenema_monthly_2015a)
pre_Kenema_monthly$measurement <- "pre"
pre_Kenema_monthly <- rename(pre_Kenema_monthly, Value=pre)

#Koinadugu
pre_Koinadugu_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            pre1=pre.var[which(lon==11.75),which(lat==9.75),1:48],
                                            pre2=pre.var[which(lon==11.25),which(lat==9.75),1:48],
                                            pre3=pre.var[which(lon==11.75),which(lat==9.25),1:48],
                                            pre4=pre.var[which(lon==11.25),which(lat==9.25),1:48],
                                            pre5=pre.var[which(lon==10.75),which(lat==9.25),1:48]))
pre_Koinadugu_monthly$pre <- rowMeans(select(pre_Koinadugu_monthly, pre1, pre2, pre3, pre4, pre5))
pre_Koinadugu_monthly$Location <- 'Koinadugu'
pre_Koinadugu_monthly$date <- ymd(paste(pre_Koinadugu_monthly$Year, pre_Koinadugu_monthly$Month, pre_Koinadugu_monthly$day, sep="-"))
pre_Koinadugu_monthly_2015 <- select(pre_Koinadugu_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Koinadugu_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Koinadugu_monthly_2015a$Location <- 'Koinadugu'
pre_Koinadugu_monthly_2015a$date <- ymd(paste(pre_Koinadugu_monthly_2015a$Year, pre_Koinadugu_monthly_2015a$Month, pre_Koinadugu_monthly_2015a$day, sep="-"))
pre_Koinadugu_monthly_2015a <- select(pre_Koinadugu_monthly_2015a, date, Year, Month, day, Location)
pre_Koinadugu_monthly_2015 <- full_join(pre_Koinadugu_monthly_2015a, pre_Koinadugu_monthly_2015)
pre_Koinadugu_monthly <- rbind(select(pre_Koinadugu_monthly,date, Year, Month, day, Location, pre), pre_Koinadugu_monthly_2015)
rm(pre_Koinadugu_monthly_2015, pre_Koinadugu_monthly_2015a)
pre_Koinadugu_monthly$measurement <- "pre"
pre_Koinadugu_monthly <- rename(pre_Koinadugu_monthly, Value=pre)

#Kono
pre_Kono_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        pre1=pre.var[which(lon==11.25),which(lat==8.75),1:48],
                                        pre2=pre.var[which(lon==10.75),which(lat==8.75),1:48]))
pre_Kono_monthly$pre <- rowMeans(select(pre_Kono_monthly, pre1, pre2))
pre_Kono_monthly$Location <- 'Kono'
pre_Kono_monthly$date <- ymd(paste(pre_Kono_monthly$Year, pre_Kono_monthly$Month, pre_Kono_monthly$day, sep="-"))
pre_Kono_monthly_2015 <- select(pre_Kono_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Kono_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Kono_monthly_2015a$Location <- 'Kono'
pre_Kono_monthly_2015a$date <- ymd(paste(pre_Kono_monthly_2015a$Year, pre_Kono_monthly_2015a$Month, pre_Kono_monthly_2015a$day, sep="-"))
pre_Kono_monthly_2015a <- select(pre_Kono_monthly_2015a, date, Year, Month, day, Location)
pre_Kono_monthly_2015 <- full_join(pre_Kono_monthly_2015a, pre_Kono_monthly_2015)
pre_Kono_monthly <- rbind(select(pre_Kono_monthly,date, Year, Month, day, Location, pre), pre_Kono_monthly_2015)
rm(pre_Kono_monthly_2015, pre_Kono_monthly_2015a)
pre_Kono_monthly$measurement <- "pre"
pre_Kono_monthly <- rename(pre_Kono_monthly, Value=pre)

#Moyamba
pre_Moyamba_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           pre1=pre.var[which(lon==12.75),which(lat==8.25),1:48],
                                           pre2=pre.var[which(lon==12.25),which(lat==8.25),1:48],
                                           pre3=pre.var[which(lon==11.75),which(lat==8.25),1:48]))
pre_Moyamba_monthly$pre <- rowMeans(select(pre_Moyamba_monthly, pre1, pre2, pre3))
pre_Moyamba_monthly$Location <- 'Moyamba'
pre_Moyamba_monthly$date <- ymd(paste(pre_Moyamba_monthly$Year, pre_Moyamba_monthly$Month, pre_Moyamba_monthly$day, sep="-"))
pre_Moyamba_monthly_2015 <- select(pre_Moyamba_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Moyamba_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Moyamba_monthly_2015a$Location <- 'Moyamba'
pre_Moyamba_monthly_2015a$date <- ymd(paste(pre_Moyamba_monthly_2015a$Year, pre_Moyamba_monthly_2015a$Month, pre_Moyamba_monthly_2015a$day, sep="-"))
pre_Moyamba_monthly_2015a <- select(pre_Moyamba_monthly_2015a, date, Year, Month, day, Location)
pre_Moyamba_monthly_2015 <- full_join(pre_Moyamba_monthly_2015a, pre_Moyamba_monthly_2015)
pre_Moyamba_monthly <- rbind(select(pre_Moyamba_monthly,date, Year, Month, day, Location, pre), pre_Moyamba_monthly_2015)
rm(pre_Moyamba_monthly_2015, pre_Moyamba_monthly_2015a)
pre_Moyamba_monthly$measurement <- "pre"
pre_Moyamba_monthly <- rename(pre_Moyamba_monthly, Value=pre)

#Port Loko
pre_PortLoko_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            pre1=pre.var[which(lon==13.25),which(lat==8.75),1:48],
                                            pre2=pre.var[which(lon==12.75),which(lat==8.75),1:48]))
pre_PortLoko_monthly$pre <- rowMeans(select(pre_PortLoko_monthly, pre1, pre2))
pre_PortLoko_monthly$Location <- 'PortLoko'
pre_PortLoko_monthly$date <- ymd(paste(pre_PortLoko_monthly$Year, pre_PortLoko_monthly$Month, pre_PortLoko_monthly$day, sep="-"))
pre_PortLoko_monthly_2015 <- select(pre_PortLoko_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_PortLoko_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_PortLoko_monthly_2015a$Location <- 'PortLoko'
pre_PortLoko_monthly_2015a$date <- ymd(paste(pre_PortLoko_monthly_2015a$Year, pre_PortLoko_monthly_2015a$Month, pre_PortLoko_monthly_2015a$day, sep="-"))
pre_PortLoko_monthly_2015a <- select(pre_PortLoko_monthly_2015a, date, Year, Month, day, Location)
pre_PortLoko_monthly_2015 <- full_join(pre_PortLoko_monthly_2015a, pre_PortLoko_monthly_2015)
pre_PortLoko_monthly <- rbind(select(pre_PortLoko_monthly,date, Year, Month, day, Location, pre), pre_PortLoko_monthly_2015)
rm(pre_PortLoko_monthly_2015, pre_PortLoko_monthly_2015a)
pre_PortLoko_monthly$measurement <- "pre"
pre_PortLoko_monthly <- rename(pre_PortLoko_monthly, Value=pre)

#Pujehun
pre_Pujehun_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           pre1=pre.var[which(lon==11.75),which(lat==7.25),1:48],
                                           pre2=pre.var[which(lon==11.25),which(lat==7.25),1:48]))
pre_Pujehun_monthly$pre <- rowMeans(select(pre_Pujehun_monthly, pre1, pre2))
pre_Pujehun_monthly$Location <- 'Pujehun'
pre_Pujehun_monthly$date <- ymd(paste(pre_Pujehun_monthly$Year, pre_Pujehun_monthly$Month, pre_Pujehun_monthly$day, sep="-"))
pre_Pujehun_monthly_2015 <- select(pre_Pujehun_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Pujehun_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Pujehun_monthly_2015a$Location <- 'Pujehun'
pre_Pujehun_monthly_2015a$date <- ymd(paste(pre_Pujehun_monthly_2015a$Year, pre_Pujehun_monthly_2015a$Month, pre_Pujehun_monthly_2015a$day, sep="-"))
pre_Pujehun_monthly_2015a <- select(pre_Pujehun_monthly_2015a, date, Year, Month, day, Location)
pre_Pujehun_monthly_2015 <- full_join(pre_Pujehun_monthly_2015a, pre_Pujehun_monthly_2015)
pre_Pujehun_monthly <- rbind(select(pre_Pujehun_monthly,date, Year, Month, day, Location, pre), pre_Pujehun_monthly_2015)
rm(pre_Pujehun_monthly_2015, pre_Pujehun_monthly_2015a)
pre_Pujehun_monthly$measurement <- "pre"
pre_Pujehun_monthly <- rename(pre_Pujehun_monthly, Value=pre)

#Tonkolili
pre_Tonkolili_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             pre1=pre.var[which(lon==12.25),which(lat==8.75),1:48],
                                             pre2=pre.var[which(lon==11.75),which(lat==8.75),1:48]))
pre_Tonkolili_monthly$pre <- rowMeans(select(pre_Tonkolili_monthly, pre1, pre2))
pre_Tonkolili_monthly$Location <- 'Tonkolili'
pre_Tonkolili_monthly$date <- ymd(paste(pre_Tonkolili_monthly$Year, pre_Tonkolili_monthly$Month, pre_Tonkolili_monthly$day, sep="-"))
pre_Tonkolili_monthly_2015 <- select(pre_Tonkolili_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Tonkolili_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Tonkolili_monthly_2015a$Location <- 'Tonkolili'
pre_Tonkolili_monthly_2015a$date <- ymd(paste(pre_Tonkolili_monthly_2015a$Year, pre_Tonkolili_monthly_2015a$Month, pre_Tonkolili_monthly_2015a$day, sep="-"))
pre_Tonkolili_monthly_2015a <- select(pre_Tonkolili_monthly_2015a, date, Year, Month, day, Location)
pre_Tonkolili_monthly_2015 <- full_join(pre_Tonkolili_monthly_2015a, pre_Tonkolili_monthly_2015)
pre_Tonkolili_monthly <- rbind(select(pre_Tonkolili_monthly,date, Year, Month, day, Location, pre), pre_Tonkolili_monthly_2015)
rm(pre_Tonkolili_monthly_2015, pre_Tonkolili_monthly_2015a)
pre_Tonkolili_monthly$measurement <- "pre"
pre_Tonkolili_monthly <- rename(pre_Tonkolili_monthly, Value=pre)

#Western rural
pre_WesternRural_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                                Month=rep(seq(1,12,1), 4), day=1,
                                                pre=pre.var[which(lon==13.25),which(lat==8.25),1:48]))
pre_WesternRural_monthly$Location <- 'WesternRural'
pre_WesternRural_monthly$date <- ymd(paste(pre_WesternRural_monthly$Year, pre_WesternRural_monthly$Month, pre_WesternRural_monthly$day, sep="-"))
pre_WesternRural_monthly_2015 <- select(pre_WesternRural_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_WesternRural_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_WesternRural_monthly_2015a$Location <- 'WesternRural'
pre_WesternRural_monthly_2015a$date <- ymd(paste(pre_WesternRural_monthly_2015a$Year, pre_WesternRural_monthly_2015a$Month, pre_WesternRural_monthly_2015a$day, sep="-"))
pre_WesternRural_monthly_2015a <- select(pre_WesternRural_monthly_2015a, date, Year, Month, day, Location)
pre_WesternRural_monthly_2015 <- full_join(pre_WesternRural_monthly_2015a, pre_WesternRural_monthly_2015)
pre_WesternRural_monthly <- rbind(select(pre_WesternRural_monthly,date, Year, Month, day, Location, pre), pre_WesternRural_monthly_2015)
rm(pre_WesternRural_monthly_2015, pre_WesternRural_monthly_2015a)
pre_WesternRural_monthly$measurement <- "pre"
pre_WesternRural_monthly <- rename(pre_WesternRural_monthly, Value=pre)

#Wester urban
pre_WesternUrban_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                                Month=rep(seq(1,12,1), 4), day=1,
                                                pre=pre.var[which(lon==13.25),which(lat==8.25),1:48]))
pre_WesternUrban_monthly$Location <- 'WesternUrban'
pre_WesternUrban_monthly$date <- ymd(paste(pre_WesternUrban_monthly$Year, pre_WesternUrban_monthly$Month, pre_WesternUrban_monthly$day, sep="-"))
pre_WesternUrban_monthly_2015 <- select(pre_WesternUrban_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_WesternUrban_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_WesternUrban_monthly_2015a$Location <- 'WesternUrban'
pre_WesternUrban_monthly_2015a$date <- ymd(paste(pre_WesternUrban_monthly_2015a$Year, pre_WesternUrban_monthly_2015a$Month, pre_WesternUrban_monthly_2015a$day, sep="-"))
pre_WesternUrban_monthly_2015a <- select(pre_WesternUrban_monthly_2015a, date, Year, Month, day, Location)
pre_WesternUrban_monthly_2015 <- full_join(pre_WesternUrban_monthly_2015a, pre_WesternUrban_monthly_2015)
pre_WesternUrban_monthly <- rbind(select(pre_WesternUrban_monthly,date, Year, Month, day, Location, pre), pre_WesternUrban_monthly_2015)
rm(pre_WesternUrban_monthly_2015, pre_WesternUrban_monthly_2015a)
pre_WesternUrban_monthly$measurement <- "pre"
pre_WesternUrban_monthly <- rename(pre_WesternUrban_monthly, Value=pre)

#Merging in long format
pre_SL_monthly_district <- rbind(pre_Bo_monthly, pre_Bombali_monthly, pre_Bonthe_monthly,
                                 pre_Kailahun_monthly, pre_Kambia_monthly, pre_Kenema_monthly,
                                 pre_Koinadugu_monthly, pre_Kono_monthly, pre_Moyamba_monthly,
                                 pre_PortLoko_monthly, pre_Pujehun_monthly, pre_Tonkolili_monthly,
                                 pre_WesternRural_monthly, pre_WesternUrban_monthly)

#####################
#tmn - min Temp
#####################
tmn.full <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.tmn.dat.nc', write=FALSE)
tmn.var <- var.get.nc(tmn.full, "tmn")
#Bo
tmn_Bo_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                      Month=rep(seq(1,12,1), 4), day=1,
                                      tmn1=tmn.var[which(lon==11.75),which(lat==8.25),1:48],
                                      tmn2=tmn.var[which(lon==11.75),which(lat==7.75),1:48]))
tmn_Bo_monthly$tmn <- rowMeans(select(tmn_Bo_monthly, tmn1, tmn2))
tmn_Bo_monthly$Location <- 'Bo'
tmn_Bo_monthly$date <- ymd(paste(tmn_Bo_monthly$Year, tmn_Bo_monthly$Month, tmn_Bo_monthly$day, sep="-"))
tmn_Bo_monthly_2015 <- select(tmn_Bo_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Bo_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Bo_monthly_2015a$Location <- 'Bo'
tmn_Bo_monthly_2015a$date <- ymd(paste(tmn_Bo_monthly_2015a$Year, tmn_Bo_monthly_2015a$Month, tmn_Bo_monthly_2015a$day, sep="-"))
tmn_Bo_monthly_2015a <- select(tmn_Bo_monthly_2015a, date, Year, Month, day, Location)
tmn_Bo_monthly_2015 <- full_join(tmn_Bo_monthly_2015a, tmn_Bo_monthly_2015)
tmn_Bo_monthly <- rbind(select(tmn_Bo_monthly,date, Year, Month, day, Location, tmn), tmn_Bo_monthly_2015)
rm(tmn_Bo_monthly_2015, tmn_Bo_monthly_2015a)
tmn_Bo_monthly$measurement <- "tmn"
tmn_Bo_monthly <- rename(tmn_Bo_monthly, Value=tmn)

#Bombali
tmn_Bombali_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmn1=tmn.var[which(lon==12.25),which(lat==9.75),1:48],
                                           tmn2=tmn.var[which(lon==12.25),which(lat==9.25),1:48]))
tmn_Bombali_monthly$tmn <- rowMeans(select(tmn_Bombali_monthly, tmn1, tmn2))
tmn_Bombali_monthly$Location <- 'Bombali'
tmn_Bombali_monthly$date <- ymd(paste(tmn_Bombali_monthly$Year, tmn_Bombali_monthly$Month, tmn_Bombali_monthly$day, sep="-"))
tmn_Bombali_monthly_2015 <- select(tmn_Bombali_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Bombali_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Bombali_monthly_2015a$Location <- 'Bombali'
tmn_Bombali_monthly_2015a$date <- ymd(paste(tmn_Bombali_monthly_2015a$Year, tmn_Bombali_monthly_2015a$Month, tmn_Bombali_monthly_2015a$day, sep="-"))
tmn_Bombali_monthly_2015a <- select(tmn_Bombali_monthly_2015a, date, Year, Month, day, Location)
tmn_Bombali_monthly_2015 <- full_join(tmn_Bombali_monthly_2015a, tmn_Bombali_monthly_2015)
tmn_Bombali_monthly <- rbind(select(tmn_Bombali_monthly,date, Year, Month, day, Location, tmn), tmn_Bombali_monthly_2015)
rm(tmn_Bombali_monthly_2015, tmn_Bombali_monthly_2015a)
tmn_Bombali_monthly$measurement <- "tmn"
tmn_Bombali_monthly <- rename(tmn_Bombali_monthly, Value=tmn)

#Bonthe
tmn_Bonthe_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmn1=tmn.var[which(lon==12.75),which(lat==7.75),1:48],
                                          tmn2=tmn.var[which(lon==12.25),which(lat==7.75),1:48],
                                          tmn3=tmn.var[which(lon==12.25),which(lat==7.25),1:48]))
tmn_Bonthe_monthly$tmn <- rowMeans(select(tmn_Bonthe_monthly, tmn1, tmn2, tmn3))
tmn_Bonthe_monthly$Location <- 'Bonthe'
tmn_Bonthe_monthly$date <- ymd(paste(tmn_Bonthe_monthly$Year, tmn_Bonthe_monthly$Month, tmn_Bonthe_monthly$day, sep="-"))
tmn_Bonthe_monthly_2015 <- select(tmn_Bonthe_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Bonthe_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Bonthe_monthly_2015a$Location <- 'Bonthe'
tmn_Bonthe_monthly_2015a$date <- ymd(paste(tmn_Bonthe_monthly_2015a$Year, tmn_Bonthe_monthly_2015a$Month, tmn_Bonthe_monthly_2015a$day, sep="-"))
tmn_Bonthe_monthly_2015a <- select(tmn_Bonthe_monthly_2015a, date, Year, Month, day, Location)
tmn_Bonthe_monthly_2015 <- full_join(tmn_Bonthe_monthly_2015a, tmn_Bonthe_monthly_2015)
tmn_Bonthe_monthly <- rbind(select(tmn_Bonthe_monthly,date, Year, Month, day, Location, tmn), tmn_Bonthe_monthly_2015)
rm(tmn_Bonthe_monthly_2015, tmn_Bonthe_monthly_2015a)
tmn_Bonthe_monthly$measurement <- "tmn"
tmn_Bonthe_monthly <- rename(tmn_Bonthe_monthly, Value=tmn)

#Kailahun
tmn_Kailahun_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            tmn1=tmn.var[which(lon==10.75),which(lat==8.25),1:48],
                                            tmn2=tmn.var[which(lon==10.25),which(lat==8.25),1:48],
                                            tmn3=tmn.var[which(lon==10.75),which(lat==7.75),1:48]))
tmn_Kailahun_monthly$tmn <- rowMeans(select(tmn_Kailahun_monthly, tmn1, tmn2, tmn3))
tmn_Kailahun_monthly$Location <- 'Kailahun'
tmn_Kailahun_monthly$date <- ymd(paste(tmn_Kailahun_monthly$Year, tmn_Kailahun_monthly$Month, tmn_Kailahun_monthly$day, sep="-"))
tmn_Kailahun_monthly_2015 <- select(tmn_Kailahun_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Kailahun_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Kailahun_monthly_2015a$Location <- 'Kailahun'
tmn_Kailahun_monthly_2015a$date <- ymd(paste(tmn_Kailahun_monthly_2015a$Year, tmn_Kailahun_monthly_2015a$Month, tmn_Kailahun_monthly_2015a$day, sep="-"))
tmn_Kailahun_monthly_2015a <- select(tmn_Kailahun_monthly_2015a, date, Year, Month, day, Location)
tmn_Kailahun_monthly_2015 <- full_join(tmn_Kailahun_monthly_2015a, tmn_Kailahun_monthly_2015)
tmn_Kailahun_monthly <- rbind(select(tmn_Kailahun_monthly,date, Year, Month, day, Location, tmn), tmn_Kailahun_monthly_2015)
rm(tmn_Kailahun_monthly_2015, tmn_Kailahun_monthly_2015a)
tmn_Kailahun_monthly$measurement <- "tmn"
tmn_Kailahun_monthly <- rename(tmn_Kailahun_monthly, Value=tmn)

#Kambia
tmn_Kambia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmn=tmn.var[which(lon==12.75),which(lat==7.75),1:48]))
tmn_Kambia_monthly$Location <- 'Kambia'
tmn_Kambia_monthly$date <- ymd(paste(tmn_Kambia_monthly$Year, tmn_Kambia_monthly$Month, tmn_Kambia_monthly$day, sep="-"))
tmn_Kambia_monthly_2015 <- select(tmn_Kambia_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Kambia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Kambia_monthly_2015a$Location <- 'Kambia'
tmn_Kambia_monthly_2015a$date <- ymd(paste(tmn_Kambia_monthly_2015a$Year, tmn_Kambia_monthly_2015a$Month, tmn_Kambia_monthly_2015a$day, sep="-"))
tmn_Kambia_monthly_2015a <- select(tmn_Kambia_monthly_2015a, date, Year, Month, day, Location)
tmn_Kambia_monthly_2015 <- full_join(tmn_Kambia_monthly_2015a, tmn_Kambia_monthly_2015)
tmn_Kambia_monthly <- rbind(select(tmn_Kambia_monthly,date, Year, Month, day, Location, tmn), tmn_Kambia_monthly_2015)
rm(tmn_Kambia_monthly_2015, tmn_Kambia_monthly_2015a)
tmn_Kambia_monthly$measurement <- "tmn"
tmn_Kambia_monthly <- rename(tmn_Kambia_monthly, Value=tmn)

#Kenema
tmn_Kenema_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmn1=tmn.var[which(lon==11.25),which(lat==8.25),1:48],
                                          tmn2=tmn.var[which(lon==11.25),which(lat==7.75),1:48]))
tmn_Kenema_monthly$tmn <- rowMeans(select(tmn_Kenema_monthly, tmn1, tmn2))
tmn_Kenema_monthly$Location <- 'Kenema'
tmn_Kenema_monthly$date <- ymd(paste(tmn_Kenema_monthly$Year, tmn_Kenema_monthly$Month, tmn_Kenema_monthly$day, sep="-"))
tmn_Kenema_monthly_2015 <- select(tmn_Kenema_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Kenema_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Kenema_monthly_2015a$Location <- 'Kenema'
tmn_Kenema_monthly_2015a$date <- ymd(paste(tmn_Kenema_monthly_2015a$Year, tmn_Kenema_monthly_2015a$Month, tmn_Kenema_monthly_2015a$day, sep="-"))
tmn_Kenema_monthly_2015a <- select(tmn_Kenema_monthly_2015a, date, Year, Month, day, Location)
tmn_Kenema_monthly_2015 <- full_join(tmn_Kenema_monthly_2015a, tmn_Kenema_monthly_2015)
tmn_Kenema_monthly <- rbind(select(tmn_Kenema_monthly,date, Year, Month, day, Location, tmn), tmn_Kenema_monthly_2015)
rm(tmn_Kenema_monthly_2015, tmn_Kenema_monthly_2015a)
tmn_Kenema_monthly$measurement <- "tmn"
tmn_Kenema_monthly <- rename(tmn_Kenema_monthly, Value=tmn)

#Koinadugu
tmn_Koinadugu_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            tmn1=tmn.var[which(lon==11.75),which(lat==9.75),1:48],
                                            tmn2=tmn.var[which(lon==11.25),which(lat==9.75),1:48],
                                            tmn3=tmn.var[which(lon==11.75),which(lat==9.25),1:48],
                                            tmn4=tmn.var[which(lon==11.25),which(lat==9.25),1:48],
                                            tmn5=tmn.var[which(lon==10.75),which(lat==9.25),1:48]))
tmn_Koinadugu_monthly$tmn <- rowMeans(select(tmn_Koinadugu_monthly, tmn1, tmn2, tmn3, tmn4, tmn5))
tmn_Koinadugu_monthly$Location <- 'Koinadugu'
tmn_Koinadugu_monthly$date <- ymd(paste(tmn_Koinadugu_monthly$Year, tmn_Koinadugu_monthly$Month, tmn_Koinadugu_monthly$day, sep="-"))
tmn_Koinadugu_monthly_2015 <- select(tmn_Koinadugu_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Koinadugu_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Koinadugu_monthly_2015a$Location <- 'Koinadugu'
tmn_Koinadugu_monthly_2015a$date <- ymd(paste(tmn_Koinadugu_monthly_2015a$Year, tmn_Koinadugu_monthly_2015a$Month, tmn_Koinadugu_monthly_2015a$day, sep="-"))
tmn_Koinadugu_monthly_2015a <- select(tmn_Koinadugu_monthly_2015a, date, Year, Month, day, Location)
tmn_Koinadugu_monthly_2015 <- full_join(tmn_Koinadugu_monthly_2015a, tmn_Koinadugu_monthly_2015)
tmn_Koinadugu_monthly <- rbind(select(tmn_Koinadugu_monthly,date, Year, Month, day, Location, tmn), tmn_Koinadugu_monthly_2015)
rm(tmn_Koinadugu_monthly_2015, tmn_Koinadugu_monthly_2015a)
tmn_Koinadugu_monthly$measurement <- "tmn"
tmn_Koinadugu_monthly <- rename(tmn_Koinadugu_monthly, Value=tmn)

#Kono
tmn_Kono_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmn1=tmn.var[which(lon==11.25),which(lat==8.75),1:48],
                                        tmn2=tmn.var[which(lon==10.75),which(lat==8.75),1:48]))
tmn_Kono_monthly$tmn <- rowMeans(select(tmn_Kono_monthly, tmn1, tmn2))
tmn_Kono_monthly$Location <- 'Kono'
tmn_Kono_monthly$date <- ymd(paste(tmn_Kono_monthly$Year, tmn_Kono_monthly$Month, tmn_Kono_monthly$day, sep="-"))
tmn_Kono_monthly_2015 <- select(tmn_Kono_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Kono_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Kono_monthly_2015a$Location <- 'Kono'
tmn_Kono_monthly_2015a$date <- ymd(paste(tmn_Kono_monthly_2015a$Year, tmn_Kono_monthly_2015a$Month, tmn_Kono_monthly_2015a$day, sep="-"))
tmn_Kono_monthly_2015a <- select(tmn_Kono_monthly_2015a, date, Year, Month, day, Location)
tmn_Kono_monthly_2015 <- full_join(tmn_Kono_monthly_2015a, tmn_Kono_monthly_2015)
tmn_Kono_monthly <- rbind(select(tmn_Kono_monthly,date, Year, Month, day, Location, tmn), tmn_Kono_monthly_2015)
rm(tmn_Kono_monthly_2015, tmn_Kono_monthly_2015a)
tmn_Kono_monthly$measurement <- "tmn"
tmn_Kono_monthly <- rename(tmn_Kono_monthly, Value=tmn)

#Moyamba
tmn_Moyamba_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmn1=tmn.var[which(lon==12.75),which(lat==8.25),1:48],
                                           tmn2=tmn.var[which(lon==12.25),which(lat==8.25),1:48],
                                           tmn3=tmn.var[which(lon==11.75),which(lat==8.25),1:48]))
tmn_Moyamba_monthly$tmn <- rowMeans(select(tmn_Moyamba_monthly, tmn1, tmn2, tmn3))
tmn_Moyamba_monthly$Location <- 'Moyamba'
tmn_Moyamba_monthly$date <- ymd(paste(tmn_Moyamba_monthly$Year, tmn_Moyamba_monthly$Month, tmn_Moyamba_monthly$day, sep="-"))
tmn_Moyamba_monthly_2015 <- select(tmn_Moyamba_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Moyamba_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Moyamba_monthly_2015a$Location <- 'Moyamba'
tmn_Moyamba_monthly_2015a$date <- ymd(paste(tmn_Moyamba_monthly_2015a$Year, tmn_Moyamba_monthly_2015a$Month, tmn_Moyamba_monthly_2015a$day, sep="-"))
tmn_Moyamba_monthly_2015a <- select(tmn_Moyamba_monthly_2015a, date, Year, Month, day, Location)
tmn_Moyamba_monthly_2015 <- full_join(tmn_Moyamba_monthly_2015a, tmn_Moyamba_monthly_2015)
tmn_Moyamba_monthly <- rbind(select(tmn_Moyamba_monthly,date, Year, Month, day, Location, tmn), tmn_Moyamba_monthly_2015)
rm(tmn_Moyamba_monthly_2015, tmn_Moyamba_monthly_2015a)
tmn_Moyamba_monthly$measurement <- "tmn"
tmn_Moyamba_monthly <- rename(tmn_Moyamba_monthly, Value=tmn)

#Port Loko
tmn_PortLoko_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            tmn1=tmn.var[which(lon==13.25),which(lat==8.75),1:48],
                                            tmn2=tmn.var[which(lon==12.75),which(lat==8.75),1:48]))
tmn_PortLoko_monthly$tmn <- rowMeans(select(tmn_PortLoko_monthly, tmn1, tmn2))
tmn_PortLoko_monthly$Location <- 'PortLoko'
tmn_PortLoko_monthly$date <- ymd(paste(tmn_PortLoko_monthly$Year, tmn_PortLoko_monthly$Month, tmn_PortLoko_monthly$day, sep="-"))
tmn_PortLoko_monthly_2015 <- select(tmn_PortLoko_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_PortLoko_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_PortLoko_monthly_2015a$Location <- 'PortLoko'
tmn_PortLoko_monthly_2015a$date <- ymd(paste(tmn_PortLoko_monthly_2015a$Year, tmn_PortLoko_monthly_2015a$Month, tmn_PortLoko_monthly_2015a$day, sep="-"))
tmn_PortLoko_monthly_2015a <- select(tmn_PortLoko_monthly_2015a, date, Year, Month, day, Location)
tmn_PortLoko_monthly_2015 <- full_join(tmn_PortLoko_monthly_2015a, tmn_PortLoko_monthly_2015)
tmn_PortLoko_monthly <- rbind(select(tmn_PortLoko_monthly,date, Year, Month, day, Location, tmn), tmn_PortLoko_monthly_2015)
rm(tmn_PortLoko_monthly_2015, tmn_PortLoko_monthly_2015a)
tmn_PortLoko_monthly$measurement <- "tmn"
tmn_PortLoko_monthly <- rename(tmn_PortLoko_monthly, Value=tmn)

#Pujehun
tmn_Pujehun_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmn1=tmn.var[which(lon==11.75),which(lat==7.25),1:48],
                                           tmn2=tmn.var[which(lon==11.25),which(lat==7.25),1:48]))
tmn_Pujehun_monthly$tmn <- rowMeans(select(tmn_Pujehun_monthly, tmn1, tmn2))
tmn_Pujehun_monthly$Location <- 'Pujehun'
tmn_Pujehun_monthly$date <- ymd(paste(tmn_Pujehun_monthly$Year, tmn_Pujehun_monthly$Month, tmn_Pujehun_monthly$day, sep="-"))
tmn_Pujehun_monthly_2015 <- select(tmn_Pujehun_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Pujehun_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Pujehun_monthly_2015a$Location <- 'Pujehun'
tmn_Pujehun_monthly_2015a$date <- ymd(paste(tmn_Pujehun_monthly_2015a$Year, tmn_Pujehun_monthly_2015a$Month, tmn_Pujehun_monthly_2015a$day, sep="-"))
tmn_Pujehun_monthly_2015a <- select(tmn_Pujehun_monthly_2015a, date, Year, Month, day, Location)
tmn_Pujehun_monthly_2015 <- full_join(tmn_Pujehun_monthly_2015a, tmn_Pujehun_monthly_2015)
tmn_Pujehun_monthly <- rbind(select(tmn_Pujehun_monthly,date, Year, Month, day, Location, tmn), tmn_Pujehun_monthly_2015)
rm(tmn_Pujehun_monthly_2015, tmn_Pujehun_monthly_2015a)
tmn_Pujehun_monthly$measurement <- "tmn"
tmn_Pujehun_monthly <- rename(tmn_Pujehun_monthly, Value=tmn)

#Tonkolili
tmn_Tonkolili_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             tmn1=tmn.var[which(lon==12.25),which(lat==8.75),1:48],
                                             tmn2=tmn.var[which(lon==11.75),which(lat==8.75),1:48]))
tmn_Tonkolili_monthly$tmn <- rowMeans(select(tmn_Tonkolili_monthly, tmn1, tmn2))
tmn_Tonkolili_monthly$Location <- 'Tonkolili'
tmn_Tonkolili_monthly$date <- ymd(paste(tmn_Tonkolili_monthly$Year, tmn_Tonkolili_monthly$Month, tmn_Tonkolili_monthly$day, sep="-"))
tmn_Tonkolili_monthly_2015 <- select(tmn_Tonkolili_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Tonkolili_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Tonkolili_monthly_2015a$Location <- 'Tonkolili'
tmn_Tonkolili_monthly_2015a$date <- ymd(paste(tmn_Tonkolili_monthly_2015a$Year, tmn_Tonkolili_monthly_2015a$Month, tmn_Tonkolili_monthly_2015a$day, sep="-"))
tmn_Tonkolili_monthly_2015a <- select(tmn_Tonkolili_monthly_2015a, date, Year, Month, day, Location)
tmn_Tonkolili_monthly_2015 <- full_join(tmn_Tonkolili_monthly_2015a, tmn_Tonkolili_monthly_2015)
tmn_Tonkolili_monthly <- rbind(select(tmn_Tonkolili_monthly,date, Year, Month, day, Location, tmn), tmn_Tonkolili_monthly_2015)
rm(tmn_Tonkolili_monthly_2015, tmn_Tonkolili_monthly_2015a)
tmn_Tonkolili_monthly$measurement <- "tmn"
tmn_Tonkolili_monthly <- rename(tmn_Tonkolili_monthly, Value=tmn)

#Western rural
tmn_WesternRural_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                                Month=rep(seq(1,12,1), 4), day=1,
                                                tmn=tmn.var[which(lon==13.25),which(lat==8.25),1:48]))
tmn_WesternRural_monthly$Location <- 'WesternRural'
tmn_WesternRural_monthly$date <- ymd(paste(tmn_WesternRural_monthly$Year, tmn_WesternRural_monthly$Month, tmn_WesternRural_monthly$day, sep="-"))
tmn_WesternRural_monthly_2015 <- select(tmn_WesternRural_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_WesternRural_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_WesternRural_monthly_2015a$Location <- 'WesternRural'
tmn_WesternRural_monthly_2015a$date <- ymd(paste(tmn_WesternRural_monthly_2015a$Year, tmn_WesternRural_monthly_2015a$Month, tmn_WesternRural_monthly_2015a$day, sep="-"))
tmn_WesternRural_monthly_2015a <- select(tmn_WesternRural_monthly_2015a, date, Year, Month, day, Location)
tmn_WesternRural_monthly_2015 <- full_join(tmn_WesternRural_monthly_2015a, tmn_WesternRural_monthly_2015)
tmn_WesternRural_monthly <- rbind(select(tmn_WesternRural_monthly,date, Year, Month, day, Location, tmn), tmn_WesternRural_monthly_2015)
rm(tmn_WesternRural_monthly_2015, tmn_WesternRural_monthly_2015a)
tmn_WesternRural_monthly$measurement <- "tmn"
tmn_WesternRural_monthly <- rename(tmn_WesternRural_monthly, Value=tmn)

#Wester urban
tmn_WesternUrban_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                                Month=rep(seq(1,12,1), 4), day=1,
                                                tmn=tmn.var[which(lon==13.25),which(lat==8.25),1:48]))
tmn_WesternUrban_monthly$Location <- 'WesternUrban'
tmn_WesternUrban_monthly$date <- ymd(paste(tmn_WesternUrban_monthly$Year, tmn_WesternUrban_monthly$Month, tmn_WesternUrban_monthly$day, sep="-"))
tmn_WesternUrban_monthly_2015 <- select(tmn_WesternUrban_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_WesternUrban_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_WesternUrban_monthly_2015a$Location <- 'WesternUrban'
tmn_WesternUrban_monthly_2015a$date <- ymd(paste(tmn_WesternUrban_monthly_2015a$Year, tmn_WesternUrban_monthly_2015a$Month, tmn_WesternUrban_monthly_2015a$day, sep="-"))
tmn_WesternUrban_monthly_2015a <- select(tmn_WesternUrban_monthly_2015a, date, Year, Month, day, Location)
tmn_WesternUrban_monthly_2015 <- full_join(tmn_WesternUrban_monthly_2015a, tmn_WesternUrban_monthly_2015)
tmn_WesternUrban_monthly <- rbind(select(tmn_WesternUrban_monthly,date, Year, Month, day, Location, tmn), tmn_WesternUrban_monthly_2015)
rm(tmn_WesternUrban_monthly_2015, tmn_WesternUrban_monthly_2015a)
tmn_WesternUrban_monthly$measurement <- "tmn"
tmn_WesternUrban_monthly <- rename(tmn_WesternUrban_monthly, Value=tmn)

#Merging in long format
tmn_SL_monthly_district <- rbind(tmn_Bo_monthly, tmn_Bombali_monthly, tmn_Bonthe_monthly,
                                 tmn_Kailahun_monthly, tmn_Kambia_monthly, tmn_Kenema_monthly,
                                 tmn_Koinadugu_monthly, tmn_Kono_monthly, tmn_Moyamba_monthly,
                                 tmn_PortLoko_monthly, tmn_Pujehun_monthly, tmn_Tonkolili_monthly,
                                 tmn_WesternRural_monthly, tmn_WesternUrban_monthly)

#####################
#tmp - mean Temp
#####################
tmp.full <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.tmp.dat.nc', write=FALSE)
tmp.var <- var.get.nc(tmp.full, "tmp")

#Bo
tmp_Bo_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                      Month=rep(seq(1,12,1), 4), day=1,
                                      tmp1=tmp.var[which(lon==11.75),which(lat==8.25),1:48],
                                      tmp2=tmp.var[which(lon==11.75),which(lat==7.75),1:48]))
tmp_Bo_monthly$tmp <- rowMeans(select(tmp_Bo_monthly, tmp1, tmp2))
tmp_Bo_monthly$Location <- 'Bo'
tmp_Bo_monthly$date <- ymd(paste(tmp_Bo_monthly$Year, tmp_Bo_monthly$Month, tmp_Bo_monthly$day, sep="-"))
tmp_Bo_monthly_2015 <- select(tmp_Bo_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Bo_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Bo_monthly_2015a$Location <- 'Bo'
tmp_Bo_monthly_2015a$date <- ymd(paste(tmp_Bo_monthly_2015a$Year, tmp_Bo_monthly_2015a$Month, tmp_Bo_monthly_2015a$day, sep="-"))
tmp_Bo_monthly_2015a <- select(tmp_Bo_monthly_2015a, date, Year, Month, day, Location)
tmp_Bo_monthly_2015 <- full_join(tmp_Bo_monthly_2015a, tmp_Bo_monthly_2015)
tmp_Bo_monthly <- rbind(select(tmp_Bo_monthly,date, Year, Month, day, Location, tmp), tmp_Bo_monthly_2015)
rm(tmp_Bo_monthly_2015, tmp_Bo_monthly_2015a)
tmp_Bo_monthly$measurement <- "tmp"
tmp_Bo_monthly <- rename(tmp_Bo_monthly, Value=tmp)

#Bombali
tmp_Bombali_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmp1=tmp.var[which(lon==12.25),which(lat==9.75),1:48],
                                           tmp2=tmp.var[which(lon==12.25),which(lat==9.25),1:48]))
tmp_Bombali_monthly$tmp <- rowMeans(select(tmp_Bombali_monthly, tmp1, tmp2))
tmp_Bombali_monthly$Location <- 'Bombali'
tmp_Bombali_monthly$date <- ymd(paste(tmp_Bombali_monthly$Year, tmp_Bombali_monthly$Month, tmp_Bombali_monthly$day, sep="-"))
tmp_Bombali_monthly_2015 <- select(tmp_Bombali_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Bombali_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Bombali_monthly_2015a$Location <- 'Bombali'
tmp_Bombali_monthly_2015a$date <- ymd(paste(tmp_Bombali_monthly_2015a$Year, tmp_Bombali_monthly_2015a$Month, tmp_Bombali_monthly_2015a$day, sep="-"))
tmp_Bombali_monthly_2015a <- select(tmp_Bombali_monthly_2015a, date, Year, Month, day, Location)
tmp_Bombali_monthly_2015 <- full_join(tmp_Bombali_monthly_2015a, tmp_Bombali_monthly_2015)
tmp_Bombali_monthly <- rbind(select(tmp_Bombali_monthly,date, Year, Month, day, Location, tmp), tmp_Bombali_monthly_2015)
rm(tmp_Bombali_monthly_2015, tmp_Bombali_monthly_2015a)
tmp_Bombali_monthly$measurement <- "tmp"
tmp_Bombali_monthly <- rename(tmp_Bombali_monthly, Value=tmp)

#Bonthe
tmp_Bonthe_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmp1=tmp.var[which(lon==12.75),which(lat==7.75),1:48],
                                          tmp2=tmp.var[which(lon==12.25),which(lat==7.75),1:48],
                                          tmp3=tmp.var[which(lon==12.25),which(lat==7.25),1:48]))
tmp_Bonthe_monthly$tmp <- rowMeans(select(tmp_Bonthe_monthly, tmp1, tmp2, tmp3))
tmp_Bonthe_monthly$Location <- 'Bonthe'
tmp_Bonthe_monthly$date <- ymd(paste(tmp_Bonthe_monthly$Year, tmp_Bonthe_monthly$Month, tmp_Bonthe_monthly$day, sep="-"))
tmp_Bonthe_monthly_2015 <- select(tmp_Bonthe_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Bonthe_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Bonthe_monthly_2015a$Location <- 'Bonthe'
tmp_Bonthe_monthly_2015a$date <- ymd(paste(tmp_Bonthe_monthly_2015a$Year, tmp_Bonthe_monthly_2015a$Month, tmp_Bonthe_monthly_2015a$day, sep="-"))
tmp_Bonthe_monthly_2015a <- select(tmp_Bonthe_monthly_2015a, date, Year, Month, day, Location)
tmp_Bonthe_monthly_2015 <- full_join(tmp_Bonthe_monthly_2015a, tmp_Bonthe_monthly_2015)
tmp_Bonthe_monthly <- rbind(select(tmp_Bonthe_monthly,date, Year, Month, day, Location, tmp), tmp_Bonthe_monthly_2015)
rm(tmp_Bonthe_monthly_2015, tmp_Bonthe_monthly_2015a)
tmp_Bonthe_monthly$measurement <- "tmp"
tmp_Bonthe_monthly <- rename(tmp_Bonthe_monthly, Value=tmp)

#Kailahun
tmp_Kailahun_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            tmp1=tmp.var[which(lon==10.75),which(lat==8.25),1:48],
                                            tmp2=tmp.var[which(lon==10.25),which(lat==8.25),1:48],
                                            tmp3=tmp.var[which(lon==10.75),which(lat==7.75),1:48]))
tmp_Kailahun_monthly$tmp <- rowMeans(select(tmp_Kailahun_monthly, tmp1, tmp2, tmp3))
tmp_Kailahun_monthly$Location <- 'Kailahun'
tmp_Kailahun_monthly$date <- ymd(paste(tmp_Kailahun_monthly$Year, tmp_Kailahun_monthly$Month, tmp_Kailahun_monthly$day, sep="-"))
tmp_Kailahun_monthly_2015 <- select(tmp_Kailahun_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Kailahun_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Kailahun_monthly_2015a$Location <- 'Kailahun'
tmp_Kailahun_monthly_2015a$date <- ymd(paste(tmp_Kailahun_monthly_2015a$Year, tmp_Kailahun_monthly_2015a$Month, tmp_Kailahun_monthly_2015a$day, sep="-"))
tmp_Kailahun_monthly_2015a <- select(tmp_Kailahun_monthly_2015a, date, Year, Month, day, Location)
tmp_Kailahun_monthly_2015 <- full_join(tmp_Kailahun_monthly_2015a, tmp_Kailahun_monthly_2015)
tmp_Kailahun_monthly <- rbind(select(tmp_Kailahun_monthly,date, Year, Month, day, Location, tmp), tmp_Kailahun_monthly_2015)
rm(tmp_Kailahun_monthly_2015, tmp_Kailahun_monthly_2015a)
tmp_Kailahun_monthly$measurement <- "tmp"
tmp_Kailahun_monthly <- rename(tmp_Kailahun_monthly, Value=tmp)

#Kambia
tmp_Kambia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmp=tmp.var[which(lon==12.75),which(lat==7.75),1:48]))
tmp_Kambia_monthly$Location <- 'Kambia'
tmp_Kambia_monthly$date <- ymd(paste(tmp_Kambia_monthly$Year, tmp_Kambia_monthly$Month, tmp_Kambia_monthly$day, sep="-"))
tmp_Kambia_monthly_2015 <- select(tmp_Kambia_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Kambia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Kambia_monthly_2015a$Location <- 'Kambia'
tmp_Kambia_monthly_2015a$date <- ymd(paste(tmp_Kambia_monthly_2015a$Year, tmp_Kambia_monthly_2015a$Month, tmp_Kambia_monthly_2015a$day, sep="-"))
tmp_Kambia_monthly_2015a <- select(tmp_Kambia_monthly_2015a, date, Year, Month, day, Location)
tmp_Kambia_monthly_2015 <- full_join(tmp_Kambia_monthly_2015a, tmp_Kambia_monthly_2015)
tmp_Kambia_monthly <- rbind(select(tmp_Kambia_monthly,date, Year, Month, day, Location, tmp), tmp_Kambia_monthly_2015)
rm(tmp_Kambia_monthly_2015, tmp_Kambia_monthly_2015a)
tmp_Kambia_monthly$measurement <- "tmp"
tmp_Kambia_monthly <- rename(tmp_Kambia_monthly, Value=tmp)

#Kenema
tmp_Kenema_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmp1=tmp.var[which(lon==11.25),which(lat==8.25),1:48],
                                          tmp2=tmp.var[which(lon==11.25),which(lat==7.75),1:48]))
tmp_Kenema_monthly$tmp <- rowMeans(select(tmp_Kenema_monthly, tmp1, tmp2))
tmp_Kenema_monthly$Location <- 'Kenema'
tmp_Kenema_monthly$date <- ymd(paste(tmp_Kenema_monthly$Year, tmp_Kenema_monthly$Month, tmp_Kenema_monthly$day, sep="-"))
tmp_Kenema_monthly_2015 <- select(tmp_Kenema_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Kenema_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Kenema_monthly_2015a$Location <- 'Kenema'
tmp_Kenema_monthly_2015a$date <- ymd(paste(tmp_Kenema_monthly_2015a$Year, tmp_Kenema_monthly_2015a$Month, tmp_Kenema_monthly_2015a$day, sep="-"))
tmp_Kenema_monthly_2015a <- select(tmp_Kenema_monthly_2015a, date, Year, Month, day, Location)
tmp_Kenema_monthly_2015 <- full_join(tmp_Kenema_monthly_2015a, tmp_Kenema_monthly_2015)
tmp_Kenema_monthly <- rbind(select(tmp_Kenema_monthly,date, Year, Month, day, Location, tmp), tmp_Kenema_monthly_2015)
rm(tmp_Kenema_monthly_2015, tmp_Kenema_monthly_2015a)
tmp_Kenema_monthly$measurement <- "tmp"
tmp_Kenema_monthly <- rename(tmp_Kenema_monthly, Value=tmp)

#Koinadugu
tmp_Koinadugu_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            tmp1=tmp.var[which(lon==11.75),which(lat==9.75),1:48],
                                            tmp2=tmp.var[which(lon==11.25),which(lat==9.75),1:48],
                                            tmp3=tmp.var[which(lon==11.75),which(lat==9.25),1:48],
                                            tmp4=tmp.var[which(lon==11.25),which(lat==9.25),1:48],
                                            tmp5=tmp.var[which(lon==10.75),which(lat==9.25),1:48]))
tmp_Koinadugu_monthly$tmp <- rowMeans(select(tmp_Koinadugu_monthly, tmp1, tmp2, tmp3, tmp4, tmp5))
tmp_Koinadugu_monthly$Location <- 'Koinadugu'
tmp_Koinadugu_monthly$date <- ymd(paste(tmp_Koinadugu_monthly$Year, tmp_Koinadugu_monthly$Month, tmp_Koinadugu_monthly$day, sep="-"))
tmp_Koinadugu_monthly_2015 <- select(tmp_Koinadugu_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Koinadugu_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Koinadugu_monthly_2015a$Location <- 'Koinadugu'
tmp_Koinadugu_monthly_2015a$date <- ymd(paste(tmp_Koinadugu_monthly_2015a$Year, tmp_Koinadugu_monthly_2015a$Month, tmp_Koinadugu_monthly_2015a$day, sep="-"))
tmp_Koinadugu_monthly_2015a <- select(tmp_Koinadugu_monthly_2015a, date, Year, Month, day, Location)
tmp_Koinadugu_monthly_2015 <- full_join(tmp_Koinadugu_monthly_2015a, tmp_Koinadugu_monthly_2015)
tmp_Koinadugu_monthly <- rbind(select(tmp_Koinadugu_monthly,date, Year, Month, day, Location, tmp), tmp_Koinadugu_monthly_2015)
rm(tmp_Koinadugu_monthly_2015, tmp_Koinadugu_monthly_2015a)
tmp_Koinadugu_monthly$measurement <- "tmp"
tmp_Koinadugu_monthly <- rename(tmp_Koinadugu_monthly, Value=tmp)

#Kono
tmp_Kono_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmp1=tmp.var[which(lon==11.25),which(lat==8.75),1:48],
                                        tmp2=tmp.var[which(lon==10.75),which(lat==8.75),1:48]))
tmp_Kono_monthly$tmp <- rowMeans(select(tmp_Kono_monthly, tmp1, tmp2))
tmp_Kono_monthly$Location <- 'Kono'
tmp_Kono_monthly$date <- ymd(paste(tmp_Kono_monthly$Year, tmp_Kono_monthly$Month, tmp_Kono_monthly$day, sep="-"))
tmp_Kono_monthly_2015 <- select(tmp_Kono_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Kono_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Kono_monthly_2015a$Location <- 'Kono'
tmp_Kono_monthly_2015a$date <- ymd(paste(tmp_Kono_monthly_2015a$Year, tmp_Kono_monthly_2015a$Month, tmp_Kono_monthly_2015a$day, sep="-"))
tmp_Kono_monthly_2015a <- select(tmp_Kono_monthly_2015a, date, Year, Month, day, Location)
tmp_Kono_monthly_2015 <- full_join(tmp_Kono_monthly_2015a, tmp_Kono_monthly_2015)
tmp_Kono_monthly <- rbind(select(tmp_Kono_monthly,date, Year, Month, day, Location, tmp), tmp_Kono_monthly_2015)
rm(tmp_Kono_monthly_2015, tmp_Kono_monthly_2015a)
tmp_Kono_monthly$measurement <- "tmp"
tmp_Kono_monthly <- rename(tmp_Kono_monthly, Value=tmp)

#Moyamba
tmp_Moyamba_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmp1=tmp.var[which(lon==12.75),which(lat==8.25),1:48],
                                           tmp2=tmp.var[which(lon==12.25),which(lat==8.25),1:48],
                                           tmp3=tmp.var[which(lon==11.75),which(lat==8.25),1:48]))
tmp_Moyamba_monthly$tmp <- rowMeans(select(tmp_Moyamba_monthly, tmp1, tmp2, tmp3))
tmp_Moyamba_monthly$Location <- 'Moyamba'
tmp_Moyamba_monthly$date <- ymd(paste(tmp_Moyamba_monthly$Year, tmp_Moyamba_monthly$Month, tmp_Moyamba_monthly$day, sep="-"))
tmp_Moyamba_monthly_2015 <- select(tmp_Moyamba_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Moyamba_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Moyamba_monthly_2015a$Location <- 'Moyamba'
tmp_Moyamba_monthly_2015a$date <- ymd(paste(tmp_Moyamba_monthly_2015a$Year, tmp_Moyamba_monthly_2015a$Month, tmp_Moyamba_monthly_2015a$day, sep="-"))
tmp_Moyamba_monthly_2015a <- select(tmp_Moyamba_monthly_2015a, date, Year, Month, day, Location)
tmp_Moyamba_monthly_2015 <- full_join(tmp_Moyamba_monthly_2015a, tmp_Moyamba_monthly_2015)
tmp_Moyamba_monthly <- rbind(select(tmp_Moyamba_monthly,date, Year, Month, day, Location, tmp), tmp_Moyamba_monthly_2015)
rm(tmp_Moyamba_monthly_2015, tmp_Moyamba_monthly_2015a)
tmp_Moyamba_monthly$measurement <- "tmp"
tmp_Moyamba_monthly <- rename(tmp_Moyamba_monthly, Value=tmp)

#Port Loko
tmp_PortLoko_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            tmp1=tmp.var[which(lon==13.25),which(lat==8.75),1:48],
                                            tmp2=tmp.var[which(lon==12.75),which(lat==8.75),1:48]))
tmp_PortLoko_monthly$tmp <- rowMeans(select(tmp_PortLoko_monthly, tmp1, tmp2))
tmp_PortLoko_monthly$Location <- 'PortLoko'
tmp_PortLoko_monthly$date <- ymd(paste(tmp_PortLoko_monthly$Year, tmp_PortLoko_monthly$Month, tmp_PortLoko_monthly$day, sep="-"))
tmp_PortLoko_monthly_2015 <- select(tmp_PortLoko_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_PortLoko_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_PortLoko_monthly_2015a$Location <- 'PortLoko'
tmp_PortLoko_monthly_2015a$date <- ymd(paste(tmp_PortLoko_monthly_2015a$Year, tmp_PortLoko_monthly_2015a$Month, tmp_PortLoko_monthly_2015a$day, sep="-"))
tmp_PortLoko_monthly_2015a <- select(tmp_PortLoko_monthly_2015a, date, Year, Month, day, Location)
tmp_PortLoko_monthly_2015 <- full_join(tmp_PortLoko_monthly_2015a, tmp_PortLoko_monthly_2015)
tmp_PortLoko_monthly <- rbind(select(tmp_PortLoko_monthly,date, Year, Month, day, Location, tmp), tmp_PortLoko_monthly_2015)
rm(tmp_PortLoko_monthly_2015, tmp_PortLoko_monthly_2015a)
tmp_PortLoko_monthly$measurement <- "tmp"
tmp_PortLoko_monthly <- rename(tmp_PortLoko_monthly, Value=tmp)

#Pujehun
tmp_Pujehun_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmp1=tmp.var[which(lon==11.75),which(lat==7.25),1:48],
                                           tmp2=tmp.var[which(lon==11.25),which(lat==7.25),1:48]))
tmp_Pujehun_monthly$tmp <- rowMeans(select(tmp_Pujehun_monthly, tmp1, tmp2))
tmp_Pujehun_monthly$Location <- 'Pujehun'
tmp_Pujehun_monthly$date <- ymd(paste(tmp_Pujehun_monthly$Year, tmp_Pujehun_monthly$Month, tmp_Pujehun_monthly$day, sep="-"))
tmp_Pujehun_monthly_2015 <- select(tmp_Pujehun_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Pujehun_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Pujehun_monthly_2015a$Location <- 'Pujehun'
tmp_Pujehun_monthly_2015a$date <- ymd(paste(tmp_Pujehun_monthly_2015a$Year, tmp_Pujehun_monthly_2015a$Month, tmp_Pujehun_monthly_2015a$day, sep="-"))
tmp_Pujehun_monthly_2015a <- select(tmp_Pujehun_monthly_2015a, date, Year, Month, day, Location)
tmp_Pujehun_monthly_2015 <- full_join(tmp_Pujehun_monthly_2015a, tmp_Pujehun_monthly_2015)
tmp_Pujehun_monthly <- rbind(select(tmp_Pujehun_monthly,date, Year, Month, day, Location, tmp), tmp_Pujehun_monthly_2015)
rm(tmp_Pujehun_monthly_2015, tmp_Pujehun_monthly_2015a)
tmp_Pujehun_monthly$measurement <- "tmp"
tmp_Pujehun_monthly <- rename(tmp_Pujehun_monthly, Value=tmp)

#Tonkolili
tmp_Tonkolili_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             tmp1=tmp.var[which(lon==12.25),which(lat==8.75),1:48],
                                             tmp2=tmp.var[which(lon==11.75),which(lat==8.75),1:48]))
tmp_Tonkolili_monthly$tmp <- rowMeans(select(tmp_Tonkolili_monthly, tmp1, tmp2))
tmp_Tonkolili_monthly$Location <- 'Tonkolili'
tmp_Tonkolili_monthly$date <- ymd(paste(tmp_Tonkolili_monthly$Year, tmp_Tonkolili_monthly$Month, tmp_Tonkolili_monthly$day, sep="-"))
tmp_Tonkolili_monthly_2015 <- select(tmp_Tonkolili_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Tonkolili_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Tonkolili_monthly_2015a$Location <- 'Tonkolili'
tmp_Tonkolili_monthly_2015a$date <- ymd(paste(tmp_Tonkolili_monthly_2015a$Year, tmp_Tonkolili_monthly_2015a$Month, tmp_Tonkolili_monthly_2015a$day, sep="-"))
tmp_Tonkolili_monthly_2015a <- select(tmp_Tonkolili_monthly_2015a, date, Year, Month, day, Location)
tmp_Tonkolili_monthly_2015 <- full_join(tmp_Tonkolili_monthly_2015a, tmp_Tonkolili_monthly_2015)
tmp_Tonkolili_monthly <- rbind(select(tmp_Tonkolili_monthly,date, Year, Month, day, Location, tmp), tmp_Tonkolili_monthly_2015)
rm(tmp_Tonkolili_monthly_2015, tmp_Tonkolili_monthly_2015a)
tmp_Tonkolili_monthly$measurement <- "tmp"
tmp_Tonkolili_monthly <- rename(tmp_Tonkolili_monthly, Value=tmp)

#Western rural
tmp_WesternRural_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                                Month=rep(seq(1,12,1), 4), day=1,
                                                tmp=tmp.var[which(lon==13.25),which(lat==8.25),1:48]))
tmp_WesternRural_monthly$Location <- 'WesternRural'
tmp_WesternRural_monthly$date <- ymd(paste(tmp_WesternRural_monthly$Year, tmp_WesternRural_monthly$Month, tmp_WesternRural_monthly$day, sep="-"))
tmp_WesternRural_monthly_2015 <- select(tmp_WesternRural_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_WesternRural_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_WesternRural_monthly_2015a$Location <- 'WesternRural'
tmp_WesternRural_monthly_2015a$date <- ymd(paste(tmp_WesternRural_monthly_2015a$Year, tmp_WesternRural_monthly_2015a$Month, tmp_WesternRural_monthly_2015a$day, sep="-"))
tmp_WesternRural_monthly_2015a <- select(tmp_WesternRural_monthly_2015a, date, Year, Month, day, Location)
tmp_WesternRural_monthly_2015 <- full_join(tmp_WesternRural_monthly_2015a, tmp_WesternRural_monthly_2015)
tmp_WesternRural_monthly <- rbind(select(tmp_WesternRural_monthly,date, Year, Month, day, Location, tmp), tmp_WesternRural_monthly_2015)
rm(tmp_WesternRural_monthly_2015, tmp_WesternRural_monthly_2015a)
tmp_WesternRural_monthly$measurement <- "tmp"
tmp_WesternRural_monthly <- rename(tmp_WesternRural_monthly, Value=tmp)

#Wester urban
tmp_WesternUrban_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                                Month=rep(seq(1,12,1), 4), day=1,
                                                tmp=tmp.var[which(lon==13.25),which(lat==8.25),1:48]))
tmp_WesternUrban_monthly$Location <- 'WesternUrban'
tmp_WesternUrban_monthly$date <- ymd(paste(tmp_WesternUrban_monthly$Year, tmp_WesternUrban_monthly$Month, tmp_WesternUrban_monthly$day, sep="-"))
tmp_WesternUrban_monthly_2015 <- select(tmp_WesternUrban_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_WesternUrban_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_WesternUrban_monthly_2015a$Location <- 'WesternUrban'
tmp_WesternUrban_monthly_2015a$date <- ymd(paste(tmp_WesternUrban_monthly_2015a$Year, tmp_WesternUrban_monthly_2015a$Month, tmp_WesternUrban_monthly_2015a$day, sep="-"))
tmp_WesternUrban_monthly_2015a <- select(tmp_WesternUrban_monthly_2015a, date, Year, Month, day, Location)
tmp_WesternUrban_monthly_2015 <- full_join(tmp_WesternUrban_monthly_2015a, tmp_WesternUrban_monthly_2015)
tmp_WesternUrban_monthly <- rbind(select(tmp_WesternUrban_monthly,date, Year, Month, day, Location, tmp), tmp_WesternUrban_monthly_2015)
rm(tmp_WesternUrban_monthly_2015, tmp_WesternUrban_monthly_2015a)
tmp_WesternUrban_monthly$measurement <- "tmp"
tmp_WesternUrban_monthly <- rename(tmp_WesternUrban_monthly, Value=tmp)

#Merging in long format
tmp_SL_monthly_district <- rbind(tmp_Bo_monthly, tmp_Bombali_monthly, tmp_Bonthe_monthly,
                                 tmp_Kailahun_monthly, tmp_Kambia_monthly, tmp_Kenema_monthly,
                                 tmp_Koinadugu_monthly, tmp_Kono_monthly, tmp_Moyamba_monthly,
                                 tmp_PortLoko_monthly, tmp_Pujehun_monthly, tmp_Tonkolili_monthly,
                                 tmp_WesternRural_monthly, tmp_WesternUrban_monthly)

#####################
#tmx - max Temp
#####################
tmx.full <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.tmx.dat.nc', write=FALSE)
tmx.var <- var.get.nc(tmx.full, "tmx")

#Bo
tmx_Bo_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                      Month=rep(seq(1,12,1), 4), day=1,
                                      tmx1=tmx.var[which(lon==11.75),which(lat==8.25),1:48],
                                      tmx2=tmx.var[which(lon==11.75),which(lat==7.75),1:48]))
tmx_Bo_monthly$tmx <- rowMeans(select(tmx_Bo_monthly, tmx1, tmx2))
tmx_Bo_monthly$Location <- 'Bo'
tmx_Bo_monthly$date <- ymd(paste(tmx_Bo_monthly$Year, tmx_Bo_monthly$Month, tmx_Bo_monthly$day, sep="-"))
tmx_Bo_monthly_2015 <- select(tmx_Bo_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Bo_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Bo_monthly_2015a$Location <- 'Bo'
tmx_Bo_monthly_2015a$date <- ymd(paste(tmx_Bo_monthly_2015a$Year, tmx_Bo_monthly_2015a$Month, tmx_Bo_monthly_2015a$day, sep="-"))
tmx_Bo_monthly_2015a <- select(tmx_Bo_monthly_2015a, date, Year, Month, day, Location)
tmx_Bo_monthly_2015 <- full_join(tmx_Bo_monthly_2015a, tmx_Bo_monthly_2015)
tmx_Bo_monthly <- rbind(select(tmx_Bo_monthly,date, Year, Month, day, Location, tmx), tmx_Bo_monthly_2015)
rm(tmx_Bo_monthly_2015, tmx_Bo_monthly_2015a)
tmx_Bo_monthly$measurement <- "tmx"
tmx_Bo_monthly <- rename(tmx_Bo_monthly, Value=tmx)

#Bombali
tmx_Bombali_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmx1=tmx.var[which(lon==12.25),which(lat==9.75),1:48],
                                           tmx2=tmx.var[which(lon==12.25),which(lat==9.25),1:48]))
tmx_Bombali_monthly$tmx <- rowMeans(select(tmx_Bombali_monthly, tmx1, tmx2))
tmx_Bombali_monthly$Location <- 'Bombali'
tmx_Bombali_monthly$date <- ymd(paste(tmx_Bombali_monthly$Year, tmx_Bombali_monthly$Month, tmx_Bombali_monthly$day, sep="-"))
tmx_Bombali_monthly_2015 <- select(tmx_Bombali_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Bombali_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Bombali_monthly_2015a$Location <- 'Bombali'
tmx_Bombali_monthly_2015a$date <- ymd(paste(tmx_Bombali_monthly_2015a$Year, tmx_Bombali_monthly_2015a$Month, tmx_Bombali_monthly_2015a$day, sep="-"))
tmx_Bombali_monthly_2015a <- select(tmx_Bombali_monthly_2015a, date, Year, Month, day, Location)
tmx_Bombali_monthly_2015 <- full_join(tmx_Bombali_monthly_2015a, tmx_Bombali_monthly_2015)
tmx_Bombali_monthly <- rbind(select(tmx_Bombali_monthly,date, Year, Month, day, Location, tmx), tmx_Bombali_monthly_2015)
rm(tmx_Bombali_monthly_2015, tmx_Bombali_monthly_2015a)
tmx_Bombali_monthly$measurement <- "tmx"
tmx_Bombali_monthly <- rename(tmx_Bombali_monthly, Value=tmx)

#Bonthe
tmx_Bonthe_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmx1=tmx.var[which(lon==12.75),which(lat==7.75),1:48],
                                          tmx2=tmx.var[which(lon==12.25),which(lat==7.75),1:48],
                                          tmx3=tmx.var[which(lon==12.25),which(lat==7.25),1:48]))
tmx_Bonthe_monthly$tmx <- rowMeans(select(tmx_Bonthe_monthly, tmx1, tmx2, tmx3))
tmx_Bonthe_monthly$Location <- 'Bonthe'
tmx_Bonthe_monthly$date <- ymd(paste(tmx_Bonthe_monthly$Year, tmx_Bonthe_monthly$Month, tmx_Bonthe_monthly$day, sep="-"))
tmx_Bonthe_monthly_2015 <- select(tmx_Bonthe_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Bonthe_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Bonthe_monthly_2015a$Location <- 'Bonthe'
tmx_Bonthe_monthly_2015a$date <- ymd(paste(tmx_Bonthe_monthly_2015a$Year, tmx_Bonthe_monthly_2015a$Month, tmx_Bonthe_monthly_2015a$day, sep="-"))
tmx_Bonthe_monthly_2015a <- select(tmx_Bonthe_monthly_2015a, date, Year, Month, day, Location)
tmx_Bonthe_monthly_2015 <- full_join(tmx_Bonthe_monthly_2015a, tmx_Bonthe_monthly_2015)
tmx_Bonthe_monthly <- rbind(select(tmx_Bonthe_monthly,date, Year, Month, day, Location, tmx), tmx_Bonthe_monthly_2015)
rm(tmx_Bonthe_monthly_2015, tmx_Bonthe_monthly_2015a)
tmx_Bonthe_monthly$measurement <- "tmx"
tmx_Bonthe_monthly <- rename(tmx_Bonthe_monthly, Value=tmx)

#Kailahun
tmx_Kailahun_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            tmx1=tmx.var[which(lon==10.75),which(lat==8.25),1:48],
                                            tmx2=tmx.var[which(lon==10.25),which(lat==8.25),1:48],
                                            tmx3=tmx.var[which(lon==10.75),which(lat==7.75),1:48]))
tmx_Kailahun_monthly$tmx <- rowMeans(select(tmx_Kailahun_monthly, tmx1, tmx2, tmx3))
tmx_Kailahun_monthly$Location <- 'Kailahun'
tmx_Kailahun_monthly$date <- ymd(paste(tmx_Kailahun_monthly$Year, tmx_Kailahun_monthly$Month, tmx_Kailahun_monthly$day, sep="-"))
tmx_Kailahun_monthly_2015 <- select(tmx_Kailahun_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Kailahun_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Kailahun_monthly_2015a$Location <- 'Kailahun'
tmx_Kailahun_monthly_2015a$date <- ymd(paste(tmx_Kailahun_monthly_2015a$Year, tmx_Kailahun_monthly_2015a$Month, tmx_Kailahun_monthly_2015a$day, sep="-"))
tmx_Kailahun_monthly_2015a <- select(tmx_Kailahun_monthly_2015a, date, Year, Month, day, Location)
tmx_Kailahun_monthly_2015 <- full_join(tmx_Kailahun_monthly_2015a, tmx_Kailahun_monthly_2015)
tmx_Kailahun_monthly <- rbind(select(tmx_Kailahun_monthly,date, Year, Month, day, Location, tmx), tmx_Kailahun_monthly_2015)
rm(tmx_Kailahun_monthly_2015, tmx_Kailahun_monthly_2015a)
tmx_Kailahun_monthly$measurement <- "tmx"
tmx_Kailahun_monthly <- rename(tmx_Kailahun_monthly, Value=tmx)

#Kambia
tmx_Kambia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmx=tmx.var[which(lon==12.75),which(lat==7.75),1:48]))
tmx_Kambia_monthly$Location <- 'Kambia'
tmx_Kambia_monthly$date <- ymd(paste(tmx_Kambia_monthly$Year, tmx_Kambia_monthly$Month, tmx_Kambia_monthly$day, sep="-"))
tmx_Kambia_monthly_2015 <- select(tmx_Kambia_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Kambia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Kambia_monthly_2015a$Location <- 'Kambia'
tmx_Kambia_monthly_2015a$date <- ymd(paste(tmx_Kambia_monthly_2015a$Year, tmx_Kambia_monthly_2015a$Month, tmx_Kambia_monthly_2015a$day, sep="-"))
tmx_Kambia_monthly_2015a <- select(tmx_Kambia_monthly_2015a, date, Year, Month, day, Location)
tmx_Kambia_monthly_2015 <- full_join(tmx_Kambia_monthly_2015a, tmx_Kambia_monthly_2015)
tmx_Kambia_monthly <- rbind(select(tmx_Kambia_monthly,date, Year, Month, day, Location, tmx), tmx_Kambia_monthly_2015)
rm(tmx_Kambia_monthly_2015, tmx_Kambia_monthly_2015a)
tmx_Kambia_monthly$measurement <- "tmx"
tmx_Kambia_monthly <- rename(tmx_Kambia_monthly, Value=tmx)

#Kenema
tmx_Kenema_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmx1=tmx.var[which(lon==11.25),which(lat==8.25),1:48],
                                          tmx2=tmx.var[which(lon==11.25),which(lat==7.75),1:48]))
tmx_Kenema_monthly$tmx <- rowMeans(select(tmx_Kenema_monthly, tmx1, tmx2))
tmx_Kenema_monthly$Location <- 'Kenema'
tmx_Kenema_monthly$date <- ymd(paste(tmx_Kenema_monthly$Year, tmx_Kenema_monthly$Month, tmx_Kenema_monthly$day, sep="-"))
tmx_Kenema_monthly_2015 <- select(tmx_Kenema_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Kenema_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Kenema_monthly_2015a$Location <- 'Kenema'
tmx_Kenema_monthly_2015a$date <- ymd(paste(tmx_Kenema_monthly_2015a$Year, tmx_Kenema_monthly_2015a$Month, tmx_Kenema_monthly_2015a$day, sep="-"))
tmx_Kenema_monthly_2015a <- select(tmx_Kenema_monthly_2015a, date, Year, Month, day, Location)
tmx_Kenema_monthly_2015 <- full_join(tmx_Kenema_monthly_2015a, tmx_Kenema_monthly_2015)
tmx_Kenema_monthly <- rbind(select(tmx_Kenema_monthly,date, Year, Month, day, Location, tmx), tmx_Kenema_monthly_2015)
rm(tmx_Kenema_monthly_2015, tmx_Kenema_monthly_2015a)
tmx_Kenema_monthly$measurement <- "tmx"
tmx_Kenema_monthly <- rename(tmx_Kenema_monthly, Value=tmx)

#Koinadugu
tmx_Koinadugu_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            tmx1=tmx.var[which(lon==11.75),which(lat==9.75),1:48],
                                            tmx2=tmx.var[which(lon==11.25),which(lat==9.75),1:48],
                                            tmx3=tmx.var[which(lon==11.75),which(lat==9.25),1:48],
                                            tmx4=tmx.var[which(lon==11.25),which(lat==9.25),1:48],
                                            tmx5=tmx.var[which(lon==10.75),which(lat==9.25),1:48]))
tmx_Koinadugu_monthly$tmx <- rowMeans(select(tmx_Koinadugu_monthly, tmx1, tmx2, tmx3, tmx4, tmx5))
tmx_Koinadugu_monthly$Location <- 'Koinadugu'
tmx_Koinadugu_monthly$date <- ymd(paste(tmx_Koinadugu_monthly$Year, tmx_Koinadugu_monthly$Month, tmx_Koinadugu_monthly$day, sep="-"))
tmx_Koinadugu_monthly_2015 <- select(tmx_Koinadugu_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Koinadugu_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Koinadugu_monthly_2015a$Location <- 'Koinadugu'
tmx_Koinadugu_monthly_2015a$date <- ymd(paste(tmx_Koinadugu_monthly_2015a$Year, tmx_Koinadugu_monthly_2015a$Month, tmx_Koinadugu_monthly_2015a$day, sep="-"))
tmx_Koinadugu_monthly_2015a <- select(tmx_Koinadugu_monthly_2015a, date, Year, Month, day, Location)
tmx_Koinadugu_monthly_2015 <- full_join(tmx_Koinadugu_monthly_2015a, tmx_Koinadugu_monthly_2015)
tmx_Koinadugu_monthly <- rbind(select(tmx_Koinadugu_monthly,date, Year, Month, day, Location, tmx), tmx_Koinadugu_monthly_2015)
rm(tmx_Koinadugu_monthly_2015, tmx_Koinadugu_monthly_2015a)
tmx_Koinadugu_monthly$measurement <- "tmx"
tmx_Koinadugu_monthly <- rename(tmx_Koinadugu_monthly, Value=tmx)

#Kono
tmx_Kono_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmx1=tmx.var[which(lon==11.25),which(lat==8.75),1:48],
                                        tmx2=tmx.var[which(lon==10.75),which(lat==8.75),1:48]))
tmx_Kono_monthly$tmx <- rowMeans(select(tmx_Kono_monthly, tmx1, tmx2))
tmx_Kono_monthly$Location <- 'Kono'
tmx_Kono_monthly$date <- ymd(paste(tmx_Kono_monthly$Year, tmx_Kono_monthly$Month, tmx_Kono_monthly$day, sep="-"))
tmx_Kono_monthly_2015 <- select(tmx_Kono_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Kono_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Kono_monthly_2015a$Location <- 'Kono'
tmx_Kono_monthly_2015a$date <- ymd(paste(tmx_Kono_monthly_2015a$Year, tmx_Kono_monthly_2015a$Month, tmx_Kono_monthly_2015a$day, sep="-"))
tmx_Kono_monthly_2015a <- select(tmx_Kono_monthly_2015a, date, Year, Month, day, Location)
tmx_Kono_monthly_2015 <- full_join(tmx_Kono_monthly_2015a, tmx_Kono_monthly_2015)
tmx_Kono_monthly <- rbind(select(tmx_Kono_monthly,date, Year, Month, day, Location, tmx), tmx_Kono_monthly_2015)
rm(tmx_Kono_monthly_2015, tmx_Kono_monthly_2015a)
tmx_Kono_monthly$measurement <- "tmx"
tmx_Kono_monthly <- rename(tmx_Kono_monthly, Value=tmx)

#Moyamba
tmx_Moyamba_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmx1=tmx.var[which(lon==12.75),which(lat==8.25),1:48],
                                           tmx2=tmx.var[which(lon==12.25),which(lat==8.25),1:48],
                                           tmx3=tmx.var[which(lon==11.75),which(lat==8.25),1:48]))
tmx_Moyamba_monthly$tmx <- rowMeans(select(tmx_Moyamba_monthly, tmx1, tmx2, tmx3))
tmx_Moyamba_monthly$Location <- 'Moyamba'
tmx_Moyamba_monthly$date <- ymd(paste(tmx_Moyamba_monthly$Year, tmx_Moyamba_monthly$Month, tmx_Moyamba_monthly$day, sep="-"))
tmx_Moyamba_monthly_2015 <- select(tmx_Moyamba_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Moyamba_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Moyamba_monthly_2015a$Location <- 'Moyamba'
tmx_Moyamba_monthly_2015a$date <- ymd(paste(tmx_Moyamba_monthly_2015a$Year, tmx_Moyamba_monthly_2015a$Month, tmx_Moyamba_monthly_2015a$day, sep="-"))
tmx_Moyamba_monthly_2015a <- select(tmx_Moyamba_monthly_2015a, date, Year, Month, day, Location)
tmx_Moyamba_monthly_2015 <- full_join(tmx_Moyamba_monthly_2015a, tmx_Moyamba_monthly_2015)
tmx_Moyamba_monthly <- rbind(select(tmx_Moyamba_monthly,date, Year, Month, day, Location, tmx), tmx_Moyamba_monthly_2015)
rm(tmx_Moyamba_monthly_2015, tmx_Moyamba_monthly_2015a)
tmx_Moyamba_monthly$measurement <- "tmx"
tmx_Moyamba_monthly <- rename(tmx_Moyamba_monthly, Value=tmx)

#Port Loko
tmx_PortLoko_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            tmx1=tmx.var[which(lon==13.25),which(lat==8.75),1:48],
                                            tmx2=tmx.var[which(lon==12.75),which(lat==8.75),1:48]))
tmx_PortLoko_monthly$tmx <- rowMeans(select(tmx_PortLoko_monthly, tmx1, tmx2))
tmx_PortLoko_monthly$Location <- 'PortLoko'
tmx_PortLoko_monthly$date <- ymd(paste(tmx_PortLoko_monthly$Year, tmx_PortLoko_monthly$Month, tmx_PortLoko_monthly$day, sep="-"))
tmx_PortLoko_monthly_2015 <- select(tmx_PortLoko_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_PortLoko_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_PortLoko_monthly_2015a$Location <- 'PortLoko'
tmx_PortLoko_monthly_2015a$date <- ymd(paste(tmx_PortLoko_monthly_2015a$Year, tmx_PortLoko_monthly_2015a$Month, tmx_PortLoko_monthly_2015a$day, sep="-"))
tmx_PortLoko_monthly_2015a <- select(tmx_PortLoko_monthly_2015a, date, Year, Month, day, Location)
tmx_PortLoko_monthly_2015 <- full_join(tmx_PortLoko_monthly_2015a, tmx_PortLoko_monthly_2015)
tmx_PortLoko_monthly <- rbind(select(tmx_PortLoko_monthly,date, Year, Month, day, Location, tmx), tmx_PortLoko_monthly_2015)
rm(tmx_PortLoko_monthly_2015, tmx_PortLoko_monthly_2015a)
tmx_PortLoko_monthly$measurement <- "tmx"
tmx_PortLoko_monthly <- rename(tmx_PortLoko_monthly, Value=tmx)

#Pujehun
tmx_Pujehun_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmx1=tmx.var[which(lon==11.75),which(lat==7.25),1:48],
                                           tmx2=tmx.var[which(lon==11.25),which(lat==7.25),1:48]))
tmx_Pujehun_monthly$tmx <- rowMeans(select(tmx_Pujehun_monthly, tmx1, tmx2))
tmx_Pujehun_monthly$Location <- 'Pujehun'
tmx_Pujehun_monthly$date <- ymd(paste(tmx_Pujehun_monthly$Year, tmx_Pujehun_monthly$Month, tmx_Pujehun_monthly$day, sep="-"))
tmx_Pujehun_monthly_2015 <- select(tmx_Pujehun_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Pujehun_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Pujehun_monthly_2015a$Location <- 'Pujehun'
tmx_Pujehun_monthly_2015a$date <- ymd(paste(tmx_Pujehun_monthly_2015a$Year, tmx_Pujehun_monthly_2015a$Month, tmx_Pujehun_monthly_2015a$day, sep="-"))
tmx_Pujehun_monthly_2015a <- select(tmx_Pujehun_monthly_2015a, date, Year, Month, day, Location)
tmx_Pujehun_monthly_2015 <- full_join(tmx_Pujehun_monthly_2015a, tmx_Pujehun_monthly_2015)
tmx_Pujehun_monthly <- rbind(select(tmx_Pujehun_monthly,date, Year, Month, day, Location, tmx), tmx_Pujehun_monthly_2015)
rm(tmx_Pujehun_monthly_2015, tmx_Pujehun_monthly_2015a)
tmx_Pujehun_monthly$measurement <- "tmx"
tmx_Pujehun_monthly <- rename(tmx_Pujehun_monthly, Value=tmx)

#Tonkolili
tmx_Tonkolili_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             tmx1=tmx.var[which(lon==12.25),which(lat==8.75),1:48],
                                             tmx2=tmx.var[which(lon==11.75),which(lat==8.75),1:48]))
tmx_Tonkolili_monthly$tmx <- rowMeans(select(tmx_Tonkolili_monthly, tmx1, tmx2))
tmx_Tonkolili_monthly$Location <- 'Tonkolili'
tmx_Tonkolili_monthly$date <- ymd(paste(tmx_Tonkolili_monthly$Year, tmx_Tonkolili_monthly$Month, tmx_Tonkolili_monthly$day, sep="-"))
tmx_Tonkolili_monthly_2015 <- select(tmx_Tonkolili_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Tonkolili_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Tonkolili_monthly_2015a$Location <- 'Tonkolili'
tmx_Tonkolili_monthly_2015a$date <- ymd(paste(tmx_Tonkolili_monthly_2015a$Year, tmx_Tonkolili_monthly_2015a$Month, tmx_Tonkolili_monthly_2015a$day, sep="-"))
tmx_Tonkolili_monthly_2015a <- select(tmx_Tonkolili_monthly_2015a, date, Year, Month, day, Location)
tmx_Tonkolili_monthly_2015 <- full_join(tmx_Tonkolili_monthly_2015a, tmx_Tonkolili_monthly_2015)
tmx_Tonkolili_monthly <- rbind(select(tmx_Tonkolili_monthly,date, Year, Month, day, Location, tmx), tmx_Tonkolili_monthly_2015)
rm(tmx_Tonkolili_monthly_2015, tmx_Tonkolili_monthly_2015a)
tmx_Tonkolili_monthly$measurement <- "tmx"
tmx_Tonkolili_monthly <- rename(tmx_Tonkolili_monthly, Value=tmx)

#Western rural
tmx_WesternRural_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                                Month=rep(seq(1,12,1), 4), day=1,
                                                tmx=tmx.var[which(lon==13.25),which(lat==8.25),1:48]))
tmx_WesternRural_monthly$Location <- 'WesternRural'
tmx_WesternRural_monthly$date <- ymd(paste(tmx_WesternRural_monthly$Year, tmx_WesternRural_monthly$Month, tmx_WesternRural_monthly$day, sep="-"))
tmx_WesternRural_monthly_2015 <- select(tmx_WesternRural_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_WesternRural_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_WesternRural_monthly_2015a$Location <- 'WesternRural'
tmx_WesternRural_monthly_2015a$date <- ymd(paste(tmx_WesternRural_monthly_2015a$Year, tmx_WesternRural_monthly_2015a$Month, tmx_WesternRural_monthly_2015a$day, sep="-"))
tmx_WesternRural_monthly_2015a <- select(tmx_WesternRural_monthly_2015a, date, Year, Month, day, Location)
tmx_WesternRural_monthly_2015 <- full_join(tmx_WesternRural_monthly_2015a, tmx_WesternRural_monthly_2015)
tmx_WesternRural_monthly <- rbind(select(tmx_WesternRural_monthly,date, Year, Month, day, Location, tmx), tmx_WesternRural_monthly_2015)
rm(tmx_WesternRural_monthly_2015, tmx_WesternRural_monthly_2015a)
tmx_WesternRural_monthly$measurement <- "tmx"
tmx_WesternRural_monthly <- rename(tmx_WesternRural_monthly, Value=tmx)

#Wester urban
tmx_WesternUrban_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                                Month=rep(seq(1,12,1), 4), day=1,
                                                tmx=tmx.var[which(lon==13.25),which(lat==8.25),1:48]))
tmx_WesternUrban_monthly$Location <- 'WesternUrban'
tmx_WesternUrban_monthly$date <- ymd(paste(tmx_WesternUrban_monthly$Year, tmx_WesternUrban_monthly$Month, tmx_WesternUrban_monthly$day, sep="-"))
tmx_WesternUrban_monthly_2015 <- select(tmx_WesternUrban_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_WesternUrban_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_WesternUrban_monthly_2015a$Location <- 'WesternUrban'
tmx_WesternUrban_monthly_2015a$date <- ymd(paste(tmx_WesternUrban_monthly_2015a$Year, tmx_WesternUrban_monthly_2015a$Month, tmx_WesternUrban_monthly_2015a$day, sep="-"))
tmx_WesternUrban_monthly_2015a <- select(tmx_WesternUrban_monthly_2015a, date, Year, Month, day, Location)
tmx_WesternUrban_monthly_2015 <- full_join(tmx_WesternUrban_monthly_2015a, tmx_WesternUrban_monthly_2015)
tmx_WesternUrban_monthly <- rbind(select(tmx_WesternUrban_monthly,date, Year, Month, day, Location, tmx), tmx_WesternUrban_monthly_2015)
rm(tmx_WesternUrban_monthly_2015, tmx_WesternUrban_monthly_2015a)
tmx_WesternUrban_monthly$measurement <- "tmx"
tmx_WesternUrban_monthly <- rename(tmx_WesternUrban_monthly, Value=tmx)

#Merging in long format
tmx_SL_monthly_district <- rbind(tmx_Bo_monthly, tmx_Bombali_monthly, tmx_Bonthe_monthly,
                                 tmx_Kailahun_monthly, tmx_Kambia_monthly, tmx_Kenema_monthly,
                                 tmx_Koinadugu_monthly, tmx_Kono_monthly, tmx_Moyamba_monthly,
                                 tmx_PortLoko_monthly, tmx_Pujehun_monthly, tmx_Tonkolili_monthly,
                                 tmx_WesternRural_monthly, tmx_WesternUrban_monthly)

SL_monthly_district <- rbind(Vap_SL_monthly_district, wet_SL_monthly_district, pet_SL_monthly_district,
                                 dtr_SL_monthly_district, pre_SL_monthly_district, tmn_SL_monthly_district,
                                 tmp_SL_monthly_district, tmx_SL_monthly_district)
write.csv(SL_monthly_district, file='SL_monthly_district.csv')

# use interpolation to get the weekly climate parameters
func_spline <- function(climatefactor,steps=12/52){
    sp<- splinefun(x=seq(1,length(climatefactor),1), y=climatefactor, method="fmm",  ties = mean)
    out <- sp(seq(1,length(climatefactor),steps))
    return(out)
    
}
SL_monthly_district <- read.csv('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/SL_monthly_district.csv')
SL_monthly_vap <- filter(SL_monthly_district, measurement=='Vap' & Year>='2013') %>% select(-X)
SL_monthly_wet <- filter(SL_monthly_district, measurement=='wet' & Year>='2013') %>% select(-X)
SL_monthly_pet <- filter(SL_monthly_district, measurement=='pet' & Year>='2013') %>% select(-X)
SL_monthly_pre <- filter(SL_monthly_district, measurement=='pre' & Year>='2013') %>% select(-X)
SL_monthly_tmn <- filter(SL_monthly_district, measurement=='tmn' & Year>='2013') %>% select(-X)
SL_monthly_tmp <- filter(SL_monthly_district, measurement=='tmp' & Year>='2013') %>% select(-X)
SL_monthly_tmx <- filter(SL_monthly_district, measurement=='tmx' & Year>='2013') %>% select(-X)
SL_monthly_dtr <- filter(SL_monthly_district, measurement=='dtr' & Year>='2013') %>% select(-X)

SL_weekly_vap <- tapply(SL_monthly_vap$Value,SL_monthly_vap$Location,func_spline,simplify = T)
SL_weekly_wet <- tapply(SL_monthly_wet$Value,SL_monthly_wet$Location,func_spline,simplify = T)
SL_weekly_pet <- tapply(SL_monthly_pet$Value,SL_monthly_pet$Location,func_spline,simplify = T)
SL_weekly_pre <- tapply(SL_monthly_pre$Value,SL_monthly_pre$Location,func_spline,simplify = T)
SL_weekly_tmn <- tapply(SL_monthly_tmn$Value,SL_monthly_tmn$Location,func_spline,simplify = T)
SL_weekly_tmp <- tapply(SL_monthly_tmp$Value,SL_monthly_tmp$Location,func_spline,simplify = T)
SL_weekly_tmx <- tapply(SL_monthly_tmx$Value,SL_monthly_tmx$Location,func_spline,simplify = T)
SL_weekly_dtr <- tapply(SL_monthly_tmx$Value,SL_monthly_tmx$Location,func_spline,simplify = T)

SL_weekly_climate <- as.data.frame(rep(names(SL_weekly_tmn), 152))
colnames(SL_weekly_climate) <- "Location"
SL_weekly_climate <- arrange(SL_weekly_climate, Location)
SL_weekly_climate$count_week <- 1:152
SL_weekly_climate <- cbind(SL_weekly_climate, vap=unlist(SL_weekly_vap), wet=unlist(SL_weekly_wet), pet=unlist(SL_weekly_pet),
                                pre=unlist(SL_weekly_pre), tmn=unlist(SL_weekly_tmn), tmp=unlist(SL_weekly_tmp), tmx=unlist(SL_weekly_tmx),
                                dtr=unlist(SL_weekly_dtr))
row.names(SL_weekly_climate) <- 1:length(SL_weekly_climate$Location)
SL_weekly_climate$wet <- ifelse(SL_weekly_climate$wet<1,0, SL_weekly_climate$wet)
SL_weekly_climate$pre <- ifelse(SL_weekly_climate$pre<1,0, SL_weekly_climate$pre)
SL_weekly_climate$Location <- toupper(SL_weekly_climate$Location)


# Obtain the weekly cases count for SL in order and add the count_week
SL_weekly_cases <- read.csv('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Cases/SL_cases_long.csv')
SL_weekly_cases <- SL_weekly_cases %>% filter(Source=='Patient database') %>% select(-X, -Source, -Indicator) %>% spread(Case_definition, Cases) %>% 
    arrange(Location, Epi_Week) %>% mutate(count_week=rep(53:171,14)) %>% filter(count_week<=152) %>% mutate(Total_cases=Confirmed+Probable) %>%
    select(-Confirmed, -Probable)


# now full joint the case counts data and the climate data
SL_weekly_cases$Location <- str_replace_all(SL_weekly_cases$Location, "WESTERN AREA URBAN", "WESTERNURBAN")
SL_weekly_cases$Location <- str_replace_all(SL_weekly_cases$Location, "WESTERN AREA RURAL", "WESTERNRURAL")
SL_weekly_cases$Location <- str_replace_all(SL_weekly_cases$Location, "PORT LOKO", "PORTLOKO")
SL_weekly_climate$count_week <- as.numeric(as.character(SL_weekly_climate$count_week))
SL_weekly_cases_climate <- left_join(SL_weekly_climate,SL_weekly_cases,by=c("Location",'count_week'))
SL_weekly_cases_climate <- replace_na(SL_weekly_cases_climate, list(Total_cases=0))
SL_weekly_cases_climate$tmx <- as.numeric(as.character(SL_weekly_cases_climate$tmx))
SL_weekly_cases_climate$tmp <- as.numeric(as.character(SL_weekly_cases_climate$tmp))
SL_weekly_cases_climate$tmn <- as.numeric(as.character(SL_weekly_cases_climate$tmn))
SL_weekly_cases_climate$pre <- as.numeric(as.character(SL_weekly_cases_climate$pre))
SL_weekly_cases_climate$vap <- as.numeric(as.character(SL_weekly_cases_climate$vap))
SL_weekly_cases_climate$wet <- as.numeric(as.character(SL_weekly_cases_climate$wet))
SL_weekly_cases_climate$pet <- as.numeric(as.character(SL_weekly_cases_climate$pet))
SL_weekly_cases_climate$dtr <- as.numeric(as.character(SL_weekly_cases_climate$dtr))

write.csv(SL_weekly_cases_climate, 'SL_weekly_cases_climate.csv')


