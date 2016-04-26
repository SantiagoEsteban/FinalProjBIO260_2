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

###################
#Guinea by District
###################
(latN/longW)
coord_district <- 'Beyla 9.25, (8.75; 8.25; 7.75); 8.75, (8.75; 8.25; 7.75); 8.25, (8.75; 8.25)
Boffa 10.75, (14.25, 13.75); 10.25, (14.25, 13.75)
Boke 11.75, 14.25; 11.25, (14.75, 14.25, 13.75); 10.75, (14.75, 14.25)
Conakry
Coyah 9.75, 13.25
Dabola 10.75, (11.25,10.75)
Dalaba 11.25, 12.25; 10.75, 12.25
Dinguiray 11.75, (11.25, 10.75, 10.25); 11.25, (11.25, 10.75, 10.25)
Dubreka 10.25, (13.75, 13.25)
Faranah 10.25, (11.25, 10.75, 10.25); 9.75, 10.75, 9.25, 10.75
Forecariah 9.75, 12.75; 9.25, (13.25, 12.75)
Fria 10.75, 13.75; 10.25, 13.75
Gaoual 12.25, (13.75, 13.25); 11.75, (13.75, 13.25, 12.75); 11.25, (13.75, 13.25)
GUECKEDOU 8.75, 10.25
KANKAN 10.75, 9.25; 10.25, (9.75, 9.25, 8.75); 9.75, (9.75, 9.25)
KEROUANE 9.75, (9.25, 8.75); 9.25, (9.75, 9.25, 8.75); 8.75, 9.25)
KINDIA 10.25, (13.25, 12.75, 12.25); 9.75, 12.75
KISSIDOUGOU 9.75, 10.25; 9.25, (10.25, 9.75)
KOUBIA 11.75, 11.75
KOUNDARA 12.75, 13.25; 12.25, (13.25, 12.75)
KOUROUSSA 11.25, 10.25; 10.75, (10.75, 10.25, 9.75); 10.25, (10.25, 9.75); 9.75, (10.25, 9.75)
LABE 11.75, 12.25; 11.25, 12.25
LELOUMA 11.75, 12.75; 11.25, 12.75
LOLA 8.25, 8.25; 7.75, 8.25
MACENTA 8.75, (9.75, 9.25); 8.25, 9.25
MALI 12.25, (12.75, 12.25, 11.75); 11.75, (12.75, 12.25)
MAMOU 10.75, 11.75; 10.25, (12.25, 11.75)
NZEREKORE 8.25, 8.75; 7.75, 8.75
PITA 11.25, (12.75, 12.25); 10.75, 12.75
SIGUIRI 12.25, 9.25; 11.75, (10.25, 9.75, 9.25); 11.25, (9.75, 9.25)
TELIMELE 11.25, (13.75, 13.25); 10.75, (13.75, 13.25)
TOUGUE 11.75, 11.25; 11.25, 11.75
yamou 7.75, 9.25; 7.25, (9.25, 8.75)'




#####################
#Vap - vapor pressure
#####################
vap.full <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.vap.dat.nc', write=FALSE)
vap.var <- var.get.nc(vap.full, "vap")
lon <- var.get.nc(vap.full, "lon")
lat <- var.get.nc(vap.full, "lat")
#Beyla
Vap_Beyla_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         Vap1=vap.var[which(lon==8.75),which(lat==9.25),1:48],
                                         Vap2=vap.var[which(lon==8.25),which(lat==9.25),1:48], 
                                         Vap3=vap.var[which(lon==7.75),which(lat==9.25),1:48],
                                         Vap4=vap.var[which(lon==8.75),which(lat==8.75),1:48], 
                                         Vap5=vap.var[which(lon==8.25),which(lat==8.75),1:48],
                                         Vap6=vap.var[which(lon==7.75),which(lat==8.75),1:48], 
                                         Vap7=vap.var[which(lon==8.75),which(lat==8.25),1:48],
                                         Vap8=vap.var[which(lon==8.25),which(lat==8.25),1:48]))
Vap_Beyla_monthly$Vap <- rowMeans(select(Vap_Beyla_monthly, Vap1, Vap2, Vap3, Vap4, Vap5, Vap6, Vap7, Vap8))
Vap_Beyla_monthly$Location <- 'Beyla'
Vap_Beyla_monthly$date <- ymd(paste(Vap_Beyla_monthly$Year, Vap_Beyla_monthly$Month, Vap_Beyla_monthly$day, sep="-"))
Vap_Beyla_monthly_2015 <- select(Vap_Beyla_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Beyla_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Beyla_monthly_2015a$Location <- 'Beyla'
Vap_Beyla_monthly_2015a$date <- ymd(paste(Vap_Beyla_monthly_2015a$Year, Vap_Beyla_monthly_2015a$Month, Vap_Beyla_monthly_2015a$day, sep="-"))
Vap_Beyla_monthly_2015a <- select(Vap_Beyla_monthly_2015a, date, Year, Month, day, Location)
Vap_Beyla_monthly_2015 <- full_join(Vap_Beyla_monthly_2015a, Vap_Beyla_monthly_2015)
Vap_Beyla_monthly <- rbind(select(Vap_Beyla_monthly,date, Year, Month, day, Location, Vap), Vap_Beyla_monthly_2015)
rm(Vap_Beyla_monthly_2015, Vap_Beyla_monthly_2015a)
Vap_Beyla_monthly$measurement <- "Vap"
Vap_Beyla_monthly <- rename(Vap_Beyla_monthly, Value=Vap)

#Boffa
Vap_Boffa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         Vap1=vap.var[which(lon==14.25),which(lat==10.75),1:48],
                                         Vap2=vap.var[which(lon==13.75),which(lat==10.75),1:48], 
                                         Vap3=vap.var[which(lon==14.25),which(lat==10.25),1:48],
                                         Vap4=vap.var[which(lon==13.75),which(lat==10.25),1:48]))
Vap_Boffa_monthly$Vap <- rowMeans(select(Vap_Boffa_monthly, Vap1, Vap2, Vap3, Vap4))
Vap_Boffa_monthly$Location <- 'Boffa'
Vap_Boffa_monthly$date <- ymd(paste(Vap_Boffa_monthly$Year, Vap_Boffa_monthly$Month, Vap_Boffa_monthly$day, sep="-"))
Vap_Boffa_monthly_2015 <- select(Vap_Boffa_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Boffa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Boffa_monthly_2015a$Location <- 'Boffa'
Vap_Boffa_monthly_2015a$date <- ymd(paste(Vap_Boffa_monthly_2015a$Year, Vap_Boffa_monthly_2015a$Month, Vap_Boffa_monthly_2015a$day, sep="-"))
Vap_Boffa_monthly_2015a <- select(Vap_Boffa_monthly_2015a, date, Year, Month, day, Location)
Vap_Boffa_monthly_2015 <- full_join(Vap_Boffa_monthly_2015a, Vap_Boffa_monthly_2015)
Vap_Boffa_monthly <- rbind(select(Vap_Boffa_monthly,date, Year, Month, day, Location, Vap), Vap_Boffa_monthly_2015)
rm(Vap_Boffa_monthly_2015, Vap_Boffa_monthly_2015a)
Vap_Boffa_monthly$measurement <- "Vap"
Vap_Boffa_monthly <- rename(Vap_Boffa_monthly, Value=Vap)
#Boke
Vap_Boke_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                        Vap1=vap.var[which(lon==14.25),which(lat==11.75),1:48],
                                         Vap2=vap.var[which(lon==14.75),which(lat==11.25),1:48], 
                                        Vap3=vap.var[which(lon==14.25),which(lat==11.25),1:48],
                                         Vap4=vap.var[which(lon==13.75),which(lat==11.25),1:48]))
Vap_Boke_monthly$Vap <- rowMeans(select(Vap_Boke_monthly, Vap1, Vap2, Vap3, Vap4))
Vap_Boke_monthly$Location <- 'Boke'
Vap_Boke_monthly$date <- ymd(paste(Vap_Boke_monthly$Year, Vap_Boke_monthly$Month, Vap_Boke_monthly$day, sep="-"))
Vap_Boke_monthly_2015 <- select(Vap_Boke_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Boke_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Boke_monthly_2015a$Location <- 'Boke'
Vap_Boke_monthly_2015a$date <- ymd(paste(Vap_Boke_monthly_2015a$Year, Vap_Boke_monthly_2015a$Month, Vap_Boke_monthly_2015a$day, sep="-"))
Vap_Boke_monthly_2015a <- select(Vap_Boke_monthly_2015a, date, Year, Month, day, Location)
Vap_Boke_monthly_2015 <- full_join(Vap_Boke_monthly_2015a, Vap_Boke_monthly_2015)
Vap_Boke_monthly <- rbind(select(Vap_Boke_monthly,date, Year, Month, day, Location, Vap), Vap_Boke_monthly_2015)
rm(Vap_Boke_monthly_2015, Vap_Boke_monthly_2015a)
Vap_Boke_monthly$measurement <- "Vap"
Vap_Boke_monthly <- rename(Vap_Boke_monthly, Value=Vap)
#Conakry
Vap_Conakry_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         Vap=vap.var[which(lon==13.75),which(lat==9.75),1:48]))
Vap_Conakry_monthly$Location <- 'Conakry'
Vap_Conakry_monthly$date <- ymd(paste(Vap_Conakry_monthly$Year, Vap_Conakry_monthly$Month, Vap_Conakry_monthly$day, sep="-"))
Vap_Conakry_monthly_2015 <- select(Vap_Conakry_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Conakry_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Conakry_monthly_2015a$Location <- 'Conakry'
Vap_Conakry_monthly_2015a$date <- ymd(paste(Vap_Conakry_monthly_2015a$Year, Vap_Conakry_monthly_2015a$Month, Vap_Conakry_monthly_2015a$day, sep="-"))
Vap_Conakry_monthly_2015a <- select(Vap_Conakry_monthly_2015a, date, Year, Month, day, Location)
Vap_Conakry_monthly_2015 <- full_join(Vap_Conakry_monthly_2015a, Vap_Conakry_monthly_2015)
Vap_Conakry_monthly <- rbind(select(Vap_Conakry_monthly,date, Year, Month, day, Location, Vap), Vap_Conakry_monthly_2015)
rm(Vap_Conakry_monthly_2015, Vap_Conakry_monthly_2015a)
Vap_Conakry_monthly$measurement <- "Vap"
Vap_Conakry_monthly <- rename(Vap_Conakry_monthly, Value=Vap)

#Coyah
Vap_Coyah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        Vap=vap.var[which(lon==9.75),which(lat==13.25),1:48]))
Vap_Coyah_monthly$Location <- 'Coyah'
Vap_Coyah_monthly$date <- ymd(paste(Vap_Coyah_monthly$Year, Vap_Coyah_monthly$Month, Vap_Coyah_monthly$day, sep="-"))
Vap_Coyah_monthly_2015 <- select(Vap_Coyah_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Coyah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Coyah_monthly_2015a$Location <- 'Coyah'
Vap_Coyah_monthly_2015a$date <- ymd(paste(Vap_Coyah_monthly_2015a$Year, Vap_Coyah_monthly_2015a$Month, Vap_Coyah_monthly_2015a$day, sep="-"))
Vap_Coyah_monthly_2015a <- select(Vap_Coyah_monthly_2015a, date, Year, Month, day, Location)
Vap_Coyah_monthly_2015 <- full_join(Vap_Coyah_monthly_2015a, Vap_Coyah_monthly_2015)
Vap_Coyah_monthly <- rbind(select(Vap_Coyah_monthly,date, Year, Month, day, Location, Vap), Vap_Coyah_monthly_2015)
rm(Vap_Coyah_monthly_2015, Vap_Coyah_monthly_2015a)
Vap_Coyah_monthly$measurement <- "Vap"
Vap_Coyah_monthly <- rename(Vap_Coyah_monthly, Value=Vap)
#Dabola
Vap_Dabola_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        Vap1=vap.var[which(lon==11.25),which(lat==10.75),1:48],
                                        Vap2=vap.var[which(lon==10.75),which(lat==10.75),1:48]))
Vap_Dabola_monthly$Vap <- rowMeans(select(Vap_Dabola_monthly, Vap1, Vap2))
Vap_Dabola_monthly$Location <- 'Dabola'
Vap_Dabola_monthly$date <- ymd(paste(Vap_Dabola_monthly$Year, Vap_Dabola_monthly$Month, Vap_Dabola_monthly$day, sep="-"))
Vap_Dabola_monthly_2015 <- select(Vap_Dabola_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Dabola_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Dabola_monthly_2015a$Location <- 'Dabola'
Vap_Dabola_monthly_2015a$date <- ymd(paste(Vap_Dabola_monthly_2015a$Year, Vap_Dabola_monthly_2015a$Month, Vap_Dabola_monthly_2015a$day, sep="-"))
Vap_Dabola_monthly_2015a <- select(Vap_Dabola_monthly_2015a, date, Year, Month, day, Location)
Vap_Dabola_monthly_2015 <- full_join(Vap_Dabola_monthly_2015a, Vap_Dabola_monthly_2015)
Vap_Dabola_monthly <- rbind(select(Vap_Dabola_monthly,date, Year, Month, day, Location, Vap), Vap_Dabola_monthly_2015)
rm(Vap_Dabola_monthly_2015, Vap_Dabola_monthly_2015a)
Vap_Dabola_monthly$measurement <- "Vap"
Vap_Dabola_monthly <- rename(Vap_Dabola_monthly, Value=Vap)
#Dalaba
Vap_Dalaba_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          Vap1=vap.var[which(lon==11.25),which(lat==12.25),1:48],
                                          Vap2=vap.var[which(lon==10.75),which(lat==12.25),1:48]))
Vap_Dalaba_monthly$Vap <- rowMeans(select(Vap_Dalaba_monthly, Vap1, Vap2))
Vap_Dalaba_monthly$Location <- 'Dalaba'
Vap_Dalaba_monthly$date <- ymd(paste(Vap_Dalaba_monthly$Year, Vap_Dalaba_monthly$Month, Vap_Dalaba_monthly$day, sep="-"))
Vap_Dalaba_monthly_2015 <- select(Vap_Dalaba_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Dalaba_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Dalaba_monthly_2015a$Location <- 'Dalaba'
Vap_Dalaba_monthly_2015a$date <- ymd(paste(Vap_Dalaba_monthly_2015a$Year, Vap_Dalaba_monthly_2015a$Month, Vap_Dalaba_monthly_2015a$day, sep="-"))
Vap_Dalaba_monthly_2015a <- select(Vap_Dalaba_monthly_2015a, date, Year, Month, day, Location)
Vap_Dalaba_monthly_2015 <- full_join(Vap_Dalaba_monthly_2015a, Vap_Dalaba_monthly_2015)
Vap_Dalaba_monthly <- rbind(select(Vap_Dalaba_monthly,date, Year, Month, day, Location, Vap), Vap_Dalaba_monthly_2015)
rm(Vap_Dalaba_monthly_2015, Vap_Dalaba_monthly_2015a)
Vap_Dalaba_monthly$measurement <- "Vap"
Vap_Dalaba_monthly <- rename(Vap_Dalaba_monthly, Value=Vap)
#Dinguiray
Vap_Dinguiray_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             Vap1=vap.var[which(lon==11.25),which(lat==11.75),1:48],
                                             Vap2=vap.var[which(lon==10.75),which(lat==11.75),1:48], 
                                             Vap3=vap.var[which(lon==10.25),which(lat==11.75),1:48],
                                             Vap4=vap.var[which(lon==11.25),which(lat==11.25),1:48],
                                             Vap5=vap.var[which(lon==10.75),which(lat==11.25),1:48],
                                             Vap6=vap.var[which(lon==10.25),which(lat==11.25),1:48]))
Vap_Dinguiray_monthly$Vap <- rowMeans(select(Vap_Dinguiray_monthly, Vap1, Vap2, Vap3, Vap4, Vap5, Vap6))
Vap_Dinguiray_monthly$Location <- 'Dinguiray'
Vap_Dinguiray_monthly$date <- ymd(paste(Vap_Dinguiray_monthly$Year, Vap_Dinguiray_monthly$Month, Vap_Dinguiray_monthly$day, sep="-"))
Vap_Dinguiray_monthly_2015 <- select(Vap_Dinguiray_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Dinguiray_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Dinguiray_monthly_2015a$Location <- 'Dinguiray'
Vap_Dinguiray_monthly_2015a$date <- ymd(paste(Vap_Dinguiray_monthly_2015a$Year, Vap_Dinguiray_monthly_2015a$Month, Vap_Dinguiray_monthly_2015a$day, sep="-"))
Vap_Dinguiray_monthly_2015a <- select(Vap_Dinguiray_monthly_2015a, date, Year, Month, day, Location)
Vap_Dinguiray_monthly_2015 <- full_join(Vap_Dinguiray_monthly_2015a, Vap_Dinguiray_monthly_2015)
Vap_Dinguiray_monthly <- rbind(select(Vap_Dinguiray_monthly,date, Year, Month, day, Location, Vap), Vap_Dinguiray_monthly_2015)
rm(Vap_Dinguiray_monthly_2015, Vap_Dinguiray_monthly_2015a)
Vap_Dinguiray_monthly$measurement <- "Vap"
Vap_Dinguiray_monthly <- rename(Vap_Dinguiray_monthly, Value=Vap)
#Dubreka
Vap_Dubreka_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             Vap1=vap.var[which(lon==13.75),which(lat==10.25),1:48],
                                             Vap2=vap.var[which(lon==13.25),which(lat==10.25),1:48]))
Vap_Dubreka_monthly$Vap <- rowMeans(select(Vap_Dubreka_monthly, Vap1, Vap2))
Vap_Dubreka_monthly$Location <- 'Dubreka'
Vap_Dubreka_monthly$date <- ymd(paste(Vap_Dubreka_monthly$Year, Vap_Dubreka_monthly$Month, Vap_Dubreka_monthly$day, sep="-"))
Vap_Dubreka_monthly_2015 <- select(Vap_Dubreka_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Dubreka_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Dubreka_monthly_2015a$Location <- 'Dubreka'
Vap_Dubreka_monthly_2015a$date <- ymd(paste(Vap_Dubreka_monthly_2015a$Year, Vap_Dubreka_monthly_2015a$Month, Vap_Dubreka_monthly_2015a$day, sep="-"))
Vap_Dubreka_monthly_2015a <- select(Vap_Dubreka_monthly_2015a, date, Year, Month, day, Location)
Vap_Dubreka_monthly_2015 <- full_join(Vap_Dubreka_monthly_2015a, Vap_Dubreka_monthly_2015)
Vap_Dubreka_monthly <- rbind(select(Vap_Dubreka_monthly,date, Year, Month, day, Location, Vap), Vap_Dubreka_monthly_2015)
rm(Vap_Dubreka_monthly_2015, Vap_Dubreka_monthly_2015a)
Vap_Dubreka_monthly$measurement <- "Vap"
Vap_Dubreka_monthly <- rename(Vap_Dubreka_monthly, Value=Vap)
#Faranah
Vap_Faranah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             Vap1=vap.var[which(lon==11.25),which(lat==10.25),1:48],
                                             Vap2=vap.var[which(lon==10.75),which(lat==10.25),1:48], 
                                             Vap3=vap.var[which(lon==10.25),which(lat==10.25),1:48],
                                             Vap4=vap.var[which(lon==10.75),which(lat==9.75),1:48],
                                             Vap5=vap.var[which(lon==10.75),which(lat==9.25),1:48]))
Vap_Faranah_monthly$Vap <- rowMeans(select(Vap_Faranah_monthly, Vap1, Vap2, Vap3, Vap4, Vap5))
Vap_Faranah_monthly$Location <- 'Faranah'
Vap_Faranah_monthly$date <- ymd(paste(Vap_Faranah_monthly$Year, Vap_Faranah_monthly$Month, Vap_Faranah_monthly$day, sep="-"))
Vap_Faranah_monthly_2015 <- select(Vap_Faranah_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Faranah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Faranah_monthly_2015a$Location <- 'Faranah'
Vap_Faranah_monthly_2015a$date <- ymd(paste(Vap_Faranah_monthly_2015a$Year, Vap_Faranah_monthly_2015a$Month, Vap_Faranah_monthly_2015a$day, sep="-"))
Vap_Faranah_monthly_2015a <- select(Vap_Faranah_monthly_2015a, date, Year, Month, day, Location)
Vap_Faranah_monthly_2015 <- full_join(Vap_Faranah_monthly_2015a, Vap_Faranah_monthly_2015)
Vap_Faranah_monthly <- rbind(select(Vap_Faranah_monthly,date, Year, Month, day, Location, Vap), Vap_Faranah_monthly_2015)
rm(Vap_Faranah_monthly_2015, Vap_Faranah_monthly_2015a)
Vap_Faranah_monthly$measurement <- "Vap"
Vap_Faranah_monthly <- rename(Vap_Faranah_monthly, Value=Vap)
#Forecariah
Vap_Forecariah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           Vap1=vap.var[which(lon==12.75),which(lat==9.75),1:48],
                                           Vap2=vap.var[which(lon==13.25),which(lat==9.25),1:48], 
                                           Vap3=vap.var[which(lon==12.75),which(lat==9.25),1:48]))
Vap_Forecariah_monthly$Vap <- rowMeans(select(Vap_Forecariah_monthly, Vap1, Vap2, Vap3))
Vap_Forecariah_monthly$Location <- 'Forecariah'
Vap_Forecariah_monthly$date <- ymd(paste(Vap_Forecariah_monthly$Year, Vap_Forecariah_monthly$Month, Vap_Forecariah_monthly$day, sep="-"))
Vap_Forecariah_monthly_2015 <- select(Vap_Forecariah_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Forecariah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Forecariah_monthly_2015a$Location <- 'Forecariah'
Vap_Forecariah_monthly_2015a$date <- ymd(paste(Vap_Forecariah_monthly_2015a$Year, Vap_Forecariah_monthly_2015a$Month, Vap_Forecariah_monthly_2015a$day, sep="-"))
Vap_Forecariah_monthly_2015a <- select(Vap_Forecariah_monthly_2015a, date, Year, Month, day, Location)
Vap_Forecariah_monthly_2015 <- full_join(Vap_Forecariah_monthly_2015a, Vap_Forecariah_monthly_2015)
Vap_Forecariah_monthly <- rbind(select(Vap_Forecariah_monthly,date, Year, Month, day, Location, Vap), Vap_Forecariah_monthly_2015)
rm(Vap_Forecariah_monthly_2015, Vap_Forecariah_monthly_2015a)
Vap_Forecariah_monthly$measurement <- "Vap"
Vap_Forecariah_monthly <- rename(Vap_Forecariah_monthly, Value=Vap)
#Fria
Vap_Fria_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                              Month=rep(seq(1,12,1), 4), day=1,
                                              Vap1=vap.var[which(lon==13.75),which(lat==10.75),1:48],
                                              Vap2=vap.var[which(lon==13.75),which(lat==10.25),1:48]))
Vap_Fria_monthly$Vap <- rowMeans(select(Vap_Fria_monthly, Vap1, Vap2))
Vap_Fria_monthly$Location <- 'Fria'
Vap_Fria_monthly$date <- ymd(paste(Vap_Fria_monthly$Year, Vap_Fria_monthly$Month, Vap_Fria_monthly$day, sep="-"))
Vap_Fria_monthly_2015 <- select(Vap_Fria_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Fria_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Fria_monthly_2015a$Location <- 'Fria'
Vap_Fria_monthly_2015a$date <- ymd(paste(Vap_Fria_monthly_2015a$Year, Vap_Fria_monthly_2015a$Month, Vap_Fria_monthly_2015a$day, sep="-"))
Vap_Fria_monthly_2015a <- select(Vap_Fria_monthly_2015a, date, Year, Month, day, Location)
Vap_Fria_monthly_2015 <- full_join(Vap_Fria_monthly_2015a, Vap_Fria_monthly_2015)
Vap_Fria_monthly <- rbind(select(Vap_Fria_monthly,date, Year, Month, day, Location, Vap), Vap_Fria_monthly_2015)
rm(Vap_Fria_monthly_2015, Vap_Fria_monthly_2015a)
Vap_Fria_monthly$measurement <- "Vap"
Vap_Fria_monthly <- rename(Vap_Fria_monthly, Value=Vap)
#Gaoual
Vap_Gaoual_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             Vap1=vap.var[which(lon==13.75),which(lat==12.25),1:48],
                                             Vap2=vap.var[which(lon==13.25),which(lat==12.25),1:48], 
                                             Vap3=vap.var[which(lon==13.75),which(lat==11.75),1:48],
                                             Vap4=vap.var[which(lon==13.25),which(lat==11.75),1:48],
                                             Vap5=vap.var[which(lon==12.75),which(lat==11.75),1:48],
                                             Vap6=vap.var[which(lon==13.75),which(lat==11.25),1:48],
                                             Vap7=vap.var[which(lon==13.25),which(lat==11.25),1:48]))
Vap_Gaoual_monthly$Vap <- rowMeans(select(Vap_Gaoual_monthly, Vap1, Vap2, Vap3, Vap4, Vap5, Vap6, Vap7))
Vap_Gaoual_monthly$Location <- 'Gaoual'
Vap_Gaoual_monthly$date <- ymd(paste(Vap_Gaoual_monthly$Year, Vap_Gaoual_monthly$Month, Vap_Gaoual_monthly$day, sep="-"))
Vap_Gaoual_monthly_2015 <- select(Vap_Gaoual_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Gaoual_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Gaoual_monthly_2015a$Location <- 'Gaoual'
Vap_Gaoual_monthly_2015a$date <- ymd(paste(Vap_Gaoual_monthly_2015a$Year, Vap_Gaoual_monthly_2015a$Month, Vap_Gaoual_monthly_2015a$day, sep="-"))
Vap_Gaoual_monthly_2015a <- select(Vap_Gaoual_monthly_2015a, date, Year, Month, day, Location)
Vap_Gaoual_monthly_2015 <- full_join(Vap_Gaoual_monthly_2015a, Vap_Gaoual_monthly_2015)
Vap_Gaoual_monthly <- rbind(select(Vap_Gaoual_monthly,date, Year, Month, day, Location, Vap), Vap_Gaoual_monthly_2015)
rm(Vap_Gaoual_monthly_2015, Vap_Gaoual_monthly_2015a)
Vap_Gaoual_monthly$measurement <- "Vap"
Vap_Gaoual_monthly <- rename(Vap_Gaoual_monthly, Value=Vap)
#Gueckedou
Vap_Gueckedou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          Vap=vap.var[which(lon==10.25),which(lat==8.75),1:48]))
Vap_Gueckedou_monthly$Location <- 'Gueckedou'
Vap_Gueckedou_monthly$date <- ymd(paste(Vap_Gueckedou_monthly$Year, Vap_Gueckedou_monthly$Month, Vap_Gueckedou_monthly$day, sep="-"))
Vap_Gueckedou_monthly_2015 <- select(Vap_Gueckedou_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Gueckedou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Gueckedou_monthly_2015a$Location <- 'Gueckedou'
Vap_Gueckedou_monthly_2015a$date <- ymd(paste(Vap_Gueckedou_monthly_2015a$Year, Vap_Gueckedou_monthly_2015a$Month, Vap_Gueckedou_monthly_2015a$day, sep="-"))
Vap_Gueckedou_monthly_2015a <- select(Vap_Gueckedou_monthly_2015a, date, Year, Month, day, Location)
Vap_Gueckedou_monthly_2015 <- full_join(Vap_Gueckedou_monthly_2015a, Vap_Gueckedou_monthly_2015)
Vap_Gueckedou_monthly <- rbind(select(Vap_Gueckedou_monthly,date, Year, Month, day, Location, Vap), Vap_Gueckedou_monthly_2015)
rm(Vap_Gueckedou_monthly_2015, Vap_Gueckedou_monthly_2015a)
Vap_Gueckedou_monthly$measurement <- "Vap"
Vap_Gueckedou_monthly <- rename(Vap_Gueckedou_monthly, Value=Vap)
#Kankan
Vap_Kankan_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          Vap1=vap.var[which(lon==9.25),which(lat==10.75),1:48],
                                          Vap2=vap.var[which(lon==9.75),which(lat==10.25),1:48], 
                                          Vap3=vap.var[which(lon==9.25),which(lat==10.25),1:48],
                                          Vap4=vap.var[which(lon==8.75),which(lat==10.25),1:48],
                                          Vap5=vap.var[which(lon==9.75),which(lat==9.75),1:48],
                                          Vap6=vap.var[which(lon==9.25),which(lat==9.75),1:48]))
Vap_Kankan_monthly$Vap <- rowMeans(select(Vap_Kankan_monthly, Vap1, Vap2, Vap3, Vap4, Vap5, Vap6))
Vap_Kankan_monthly$Location <- 'Kankan'
Vap_Kankan_monthly$date <- ymd(paste(Vap_Kankan_monthly$Year, Vap_Kankan_monthly$Month, Vap_Kankan_monthly$day, sep="-"))
Vap_Kankan_monthly_2015 <- select(Vap_Kankan_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Kankan_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Kankan_monthly_2015a$Location <- 'Kankan'
Vap_Kankan_monthly_2015a$date <- ymd(paste(Vap_Kankan_monthly_2015a$Year, Vap_Kankan_monthly_2015a$Month, Vap_Kankan_monthly_2015a$day, sep="-"))
Vap_Kankan_monthly_2015a <- select(Vap_Kankan_monthly_2015a, date, Year, Month, day, Location)
Vap_Kankan_monthly_2015 <- full_join(Vap_Kankan_monthly_2015a, Vap_Kankan_monthly_2015)
Vap_Kankan_monthly <- rbind(select(Vap_Kankan_monthly,date, Year, Month, day, Location, Vap), Vap_Kankan_monthly_2015)
rm(Vap_Kankan_monthly_2015, Vap_Kankan_monthly_2015a)
Vap_Kankan_monthly$measurement <- "Vap"
Vap_Kankan_monthly <- rename(Vap_Kankan_monthly, Value=Vap)
#Kerouane
Vap_Kerouane_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          Vap1=vap.var[which(lon==9.25),which(lat==9.75),1:48],
                                          Vap2=vap.var[which(lon==8.75),which(lat==9.75),1:48], 
                                          Vap3=vap.var[which(lon==9.75),which(lat==9.25),1:48],
                                          Vap4=vap.var[which(lon==9.25),which(lat==9.25),1:48],
                                          Vap5=vap.var[which(lon==8.75),which(lat==9.25),1:48],
                                          Vap6=vap.var[which(lon==9.25),which(lat==8.75),1:48]))
Vap_Kerouane_monthly$Vap <- rowMeans(select(Vap_Kerouane_monthly, Vap1, Vap2, Vap3, Vap4, Vap5, Vap6))
Vap_Kerouane_monthly$Location <- 'Kerouane'
Vap_Kerouane_monthly$date <- ymd(paste(Vap_Kerouane_monthly$Year, Vap_Kerouane_monthly$Month, Vap_Kerouane_monthly$day, sep="-"))
Vap_Kerouane_monthly_2015 <- select(Vap_Kerouane_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Kerouane_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Kerouane_monthly_2015a$Location <- 'Kerouane'
Vap_Kerouane_monthly_2015a$date <- ymd(paste(Vap_Kerouane_monthly_2015a$Year, Vap_Kerouane_monthly_2015a$Month, Vap_Kerouane_monthly_2015a$day, sep="-"))
Vap_Kerouane_monthly_2015a <- select(Vap_Kerouane_monthly_2015a, date, Year, Month, day, Location)
Vap_Kerouane_monthly_2015 <- full_join(Vap_Kerouane_monthly_2015a, Vap_Kerouane_monthly_2015)
Vap_Kerouane_monthly <- rbind(select(Vap_Kerouane_monthly,date, Year, Month, day, Location, Vap), Vap_Kerouane_monthly_2015)
rm(Vap_Kerouane_monthly_2015, Vap_Kerouane_monthly_2015a)
Vap_Kerouane_monthly$measurement <- "Vap"
Vap_Kerouane_monthly <- rename(Vap_Kerouane_monthly, Value=Vap)
#Kindia
Vap_Kindia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            Vap1=vap.var[which(lon==13.25),which(lat==10.25),1:48],
                                            Vap2=vap.var[which(lon==12.75),which(lat==10.25),1:48], 
                                            Vap3=vap.var[which(lon==12.25),which(lat==10.25),1:48],
                                            Vap4=vap.var[which(lon==12.75),which(lat==9.75),1:48]))
Vap_Kindia_monthly$Vap <- rowMeans(select(Vap_Kindia_monthly, Vap1, Vap2, Vap3, Vap4))
Vap_Kindia_monthly$Location <- 'Kindia'
Vap_Kindia_monthly$date <- ymd(paste(Vap_Kindia_monthly$Year, Vap_Kindia_monthly$Month, Vap_Kindia_monthly$day, sep="-"))
Vap_Kindia_monthly_2015 <- select(Vap_Kindia_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Kindia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Kindia_monthly_2015a$Location <- 'Kindia'
Vap_Kindia_monthly_2015a$date <- ymd(paste(Vap_Kindia_monthly_2015a$Year, Vap_Kindia_monthly_2015a$Month, Vap_Kindia_monthly_2015a$day, sep="-"))
Vap_Kindia_monthly_2015a <- select(Vap_Kindia_monthly_2015a, date, Year, Month, day, Location)
Vap_Kindia_monthly_2015 <- full_join(Vap_Kindia_monthly_2015a, Vap_Kindia_monthly_2015)
Vap_Kindia_monthly <- rbind(select(Vap_Kindia_monthly,date, Year, Month, day, Location, Vap), Vap_Kindia_monthly_2015)
rm(Vap_Kindia_monthly_2015, Vap_Kindia_monthly_2015a)
Vap_Kindia_monthly$measurement <- "Vap"
Vap_Kindia_monthly <- rename(Vap_Kindia_monthly, Value=Vap)
#KISSIDOUGOU
Vap_Kissidougou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          Vap1=vap.var[which(lon==10.25),which(lat==9.75),1:48],
                                          Vap2=vap.var[which(lon==10.25),which(lat==9.25),1:48], 
                                          Vap3=vap.var[which(lon==9.75),which(lat==9.25),1:48]))
Vap_Kissidougou_monthly$Vap <- rowMeans(select(Vap_Kissidougou_monthly, Vap1, Vap2, Vap3))
Vap_Kissidougou_monthly$Location <- 'Kissidougou'
Vap_Kissidougou_monthly$date <- ymd(paste(Vap_Kissidougou_monthly$Year, Vap_Kissidougou_monthly$Month, Vap_Kissidougou_monthly$day, sep="-"))
Vap_Kissidougou_monthly_2015 <- select(Vap_Kissidougou_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Kissidougou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Kissidougou_monthly_2015a$Location <- 'Kissidougou'
Vap_Kissidougou_monthly_2015a$date <- ymd(paste(Vap_Kissidougou_monthly_2015a$Year, Vap_Kissidougou_monthly_2015a$Month, Vap_Kissidougou_monthly_2015a$day, sep="-"))
Vap_Kissidougou_monthly_2015a <- select(Vap_Kissidougou_monthly_2015a, date, Year, Month, day, Location)
Vap_Kissidougou_monthly_2015 <- full_join(Vap_Kissidougou_monthly_2015a, Vap_Kissidougou_monthly_2015)
Vap_Kissidougou_monthly <- rbind(select(Vap_Kissidougou_monthly,date, Year, Month, day, Location, Vap), Vap_Kissidougou_monthly_2015)
rm(Vap_Kissidougou_monthly_2015, Vap_Kissidougou_monthly_2015a)
Vap_Kissidougou_monthly$measurement <- "Vap"
Vap_Kissidougou_monthly <- rename(Vap_Kissidougou_monthly, Value=Vap)
#Koubia
Vap_Koubia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               Vap=vap.var[which(lon==10.25),which(lat==9.75),1:48]))
Vap_Koubia_monthly$Location <- 'Koubia'
Vap_Koubia_monthly$date <- ymd(paste(Vap_Koubia_monthly$Year, Vap_Koubia_monthly$Month, Vap_Koubia_monthly$day, sep="-"))
Vap_Koubia_monthly_2015 <- select(Vap_Koubia_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Koubia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Koubia_monthly_2015a$Location <- 'Koubia'
Vap_Koubia_monthly_2015a$date <- ymd(paste(Vap_Koubia_monthly_2015a$Year, Vap_Koubia_monthly_2015a$Month, Vap_Koubia_monthly_2015a$day, sep="-"))
Vap_Koubia_monthly_2015a <- select(Vap_Koubia_monthly_2015a, date, Year, Month, day, Location)
Vap_Koubia_monthly_2015 <- full_join(Vap_Koubia_monthly_2015a, Vap_Koubia_monthly_2015)
Vap_Koubia_monthly <- rbind(select(Vap_Koubia_monthly,date, Year, Month, day, Location, Vap), Vap_Koubia_monthly_2015)
rm(Vap_Koubia_monthly_2015, Vap_Koubia_monthly_2015a)
Vap_Koubia_monthly$measurement <- "Vap"
Vap_Koubia_monthly <- rename(Vap_Koubia_monthly, Value=Vap)
#Koundara
Vap_Koundara_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               Vap1=vap.var[which(lon==13.25),which(lat==12.75),1:48],
                                               Vap2=vap.var[which(lon==13.25),which(lat==12.25),1:48], 
                                               Vap3=vap.var[which(lon==12.75),which(lat==12.25),1:48]))
Vap_Koundara_monthly$Vap <- rowMeans(select(Vap_Koundara_monthly, Vap1, Vap2, Vap3))
Vap_Koundara_monthly$Location <- 'Koundara'
Vap_Koundara_monthly$date <- ymd(paste(Vap_Koundara_monthly$Year, Vap_Koundara_monthly$Month, Vap_Koundara_monthly$day, sep="-"))
Vap_Koundara_monthly_2015 <- select(Vap_Koundara_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Koundara_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Koundara_monthly_2015a$Location <- 'Koundara'
Vap_Koundara_monthly_2015a$date <- ymd(paste(Vap_Koundara_monthly_2015a$Year, Vap_Koundara_monthly_2015a$Month, Vap_Koundara_monthly_2015a$day, sep="-"))
Vap_Koundara_monthly_2015a <- select(Vap_Koundara_monthly_2015a, date, Year, Month, day, Location)
Vap_Koundara_monthly_2015 <- full_join(Vap_Koundara_monthly_2015a, Vap_Koundara_monthly_2015)
Vap_Koundara_monthly <- rbind(select(Vap_Koundara_monthly,date, Year, Month, day, Location, Vap), Vap_Koundara_monthly_2015)
rm(Vap_Koundara_monthly_2015, Vap_Koundara_monthly_2015a)
Vap_Koundara_monthly$measurement <- "Vap"
Vap_Koundara_monthly <- rename(Vap_Koundara_monthly, Value=Vap)
#Kouroussa
Vap_Kouroussa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            Vap1=vap.var[which(lon==10.25),which(lat==11.25),1:48],
                                            Vap2=vap.var[which(lon==10.75),which(lat==10.75),1:48], 
                                            Vap3=vap.var[which(lon==10.25),which(lat==10.75),1:48],
                                            Vap4=vap.var[which(lon==9.75),which(lat==10.75),1:48],
                                            Vap5=vap.var[which(lon==10.25),which(lat==10.25),1:48],
                                            Vap6=vap.var[which(lon==9.75),which(lat==10.25),1:48],
                                            Vap7=vap.var[which(lon==10.25),which(lat==9.75),1:48],
                                            Vap8=vap.var[which(lon==9.75),which(lat==9.75),1:48]))
Vap_Kouroussa_monthly$Vap <- rowMeans(select(Vap_Kouroussa_monthly, Vap1, Vap2, Vap3, Vap4, Vap5, Vap6, Vap7, Vap8))
Vap_Kouroussa_monthly$Location <- 'Kouroussa'
Vap_Kouroussa_monthly$date <- ymd(paste(Vap_Kouroussa_monthly$Year, Vap_Kouroussa_monthly$Month, Vap_Kouroussa_monthly$day, sep="-"))
Vap_Kouroussa_monthly_2015 <- select(Vap_Kouroussa_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Kouroussa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Kouroussa_monthly_2015a$Location <- 'Kouroussa'
Vap_Kouroussa_monthly_2015a$date <- ymd(paste(Vap_Kouroussa_monthly_2015a$Year, Vap_Kouroussa_monthly_2015a$Month, Vap_Kouroussa_monthly_2015a$day, sep="-"))
Vap_Kouroussa_monthly_2015a <- select(Vap_Kouroussa_monthly_2015a, date, Year, Month, day, Location)
Vap_Kouroussa_monthly_2015 <- full_join(Vap_Kouroussa_monthly_2015a, Vap_Kouroussa_monthly_2015)
Vap_Kouroussa_monthly <- rbind(select(Vap_Kouroussa_monthly,date, Year, Month, day, Location, Vap), Vap_Kouroussa_monthly_2015)
rm(Vap_Kouroussa_monthly_2015, Vap_Kouroussa_monthly_2015a)
Vap_Kouroussa_monthly$measurement <- "Vap"
Vap_Kouroussa_monthly <- rename(Vap_Kouroussa_monthly, Value=Vap)
#Labe
Vap_Labe_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            Vap1=vap.var[which(lon==12.25),which(lat==11.75),1:48],
                                            Vap2=vap.var[which(lon==12.25),which(lat==11.25),1:48]))
Vap_Labe_monthly$Vap <- rowMeans(select(Vap_Labe_monthly, Vap1, Vap2))
Vap_Labe_monthly$Location <- 'Labe'
Vap_Labe_monthly$date <- ymd(paste(Vap_Labe_monthly$Year, Vap_Labe_monthly$Month, Vap_Labe_monthly$day, sep="-"))
Vap_Labe_monthly_2015 <- select(Vap_Labe_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Labe_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Labe_monthly_2015a$Location <- 'Labe'
Vap_Labe_monthly_2015a$date <- ymd(paste(Vap_Labe_monthly_2015a$Year, Vap_Labe_monthly_2015a$Month, Vap_Labe_monthly_2015a$day, sep="-"))
Vap_Labe_monthly_2015a <- select(Vap_Labe_monthly_2015a, date, Year, Month, day, Location)
Vap_Labe_monthly_2015 <- full_join(Vap_Labe_monthly_2015a, Vap_Labe_monthly_2015)
Vap_Labe_monthly <- rbind(select(Vap_Labe_monthly,date, Year, Month, day, Location, Vap), Vap_Labe_monthly_2015)
rm(Vap_Labe_monthly_2015, Vap_Labe_monthly_2015a)
Vap_Labe_monthly$measurement <- "Vap"
Vap_Labe_monthly <- rename(Vap_Labe_monthly, Value=Vap)
#Lelouma
Vap_Lelouma_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        Vap1=vap.var[which(lon==12.75),which(lat==11.75),1:48],
                                        Vap2=vap.var[which(lon==12.75),which(lat==11.25),1:48]))
Vap_Lelouma_monthly$Vap <- rowMeans(select(Vap_Lelouma_monthly, Vap1, Vap2))
Vap_Lelouma_monthly$Location <- 'Lelouma'
Vap_Lelouma_monthly$date <- ymd(paste(Vap_Lelouma_monthly$Year, Vap_Lelouma_monthly$Month, Vap_Lelouma_monthly$day, sep="-"))
Vap_Lelouma_monthly_2015 <- select(Vap_Lelouma_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Lelouma_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Lelouma_monthly_2015a$Location <- 'Lelouma'
Vap_Lelouma_monthly_2015a$date <- ymd(paste(Vap_Lelouma_monthly_2015a$Year, Vap_Lelouma_monthly_2015a$Month, Vap_Lelouma_monthly_2015a$day, sep="-"))
Vap_Lelouma_monthly_2015a <- select(Vap_Lelouma_monthly_2015a, date, Year, Month, day, Location)
Vap_Lelouma_monthly_2015 <- full_join(Vap_Lelouma_monthly_2015a, Vap_Lelouma_monthly_2015)
Vap_Lelouma_monthly <- rbind(select(Vap_Lelouma_monthly,date, Year, Month, day, Location, Vap), Vap_Lelouma_monthly_2015)
rm(Vap_Lelouma_monthly_2015, Vap_Lelouma_monthly_2015a)
Vap_Lelouma_monthly$measurement <- "Vap"
Vap_Lelouma_monthly <- rename(Vap_Lelouma_monthly, Value=Vap)
#Lola
Vap_Lola_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           Vap1=vap.var[which(lon==8.25),which(lat==8.25),1:48],
                                           Vap2=vap.var[which(lon==8.25),which(lat==7.75),1:48]))
Vap_Lola_monthly$Vap <- rowMeans(select(Vap_Lola_monthly, Vap1, Vap2))
Vap_Lola_monthly$Location <- 'Lola'
Vap_Lola_monthly$date <- ymd(paste(Vap_Lola_monthly$Year, Vap_Lola_monthly$Month, Vap_Lola_monthly$day, sep="-"))
Vap_Lola_monthly_2015 <- select(Vap_Lola_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Lola_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Lola_monthly_2015a$Location <- 'Lola'
Vap_Lola_monthly_2015a$date <- ymd(paste(Vap_Lola_monthly_2015a$Year, Vap_Lola_monthly_2015a$Month, Vap_Lola_monthly_2015a$day, sep="-"))
Vap_Lola_monthly_2015a <- select(Vap_Lola_monthly_2015a, date, Year, Month, day, Location)
Vap_Lola_monthly_2015 <- full_join(Vap_Lola_monthly_2015a, Vap_Lola_monthly_2015)
Vap_Lola_monthly <- rbind(select(Vap_Lola_monthly,date, Year, Month, day, Location, Vap), Vap_Lola_monthly_2015)
rm(Vap_Lola_monthly_2015, Vap_Lola_monthly_2015a)
Vap_Lola_monthly$measurement <- "Vap"
Vap_Lola_monthly <- rename(Vap_Lola_monthly, Value=Vap)
#Macenta
Vap_Macenta_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        Vap1=vap.var[which(lon==9.75),which(lat==8.75),1:48],
                                        Vap2=vap.var[which(lon==9.25),which(lat==8.75),1:48],
                                        Vap3=vap.var[which(lon==9.25),which(lat==8.25),1:48]))
Vap_Macenta_monthly$Vap <- rowMeans(select(Vap_Macenta_monthly, Vap1, Vap2, Vap3))
Vap_Macenta_monthly$Location <- 'Macenta'
Vap_Macenta_monthly$date <- ymd(paste(Vap_Macenta_monthly$Year, Vap_Macenta_monthly$Month, Vap_Macenta_monthly$day, sep="-"))
Vap_Macenta_monthly_2015 <- select(Vap_Macenta_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Macenta_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Macenta_monthly_2015a$Location <- 'Macenta'
Vap_Macenta_monthly_2015a$date <- ymd(paste(Vap_Macenta_monthly_2015a$Year, Vap_Macenta_monthly_2015a$Month, Vap_Macenta_monthly_2015a$day, sep="-"))
Vap_Macenta_monthly_2015a <- select(Vap_Macenta_monthly_2015a, date, Year, Month, day, Location)
Vap_Macenta_monthly_2015 <- full_join(Vap_Macenta_monthly_2015a, Vap_Macenta_monthly_2015)
Vap_Macenta_monthly <- rbind(select(Vap_Macenta_monthly,date, Year, Month, day, Location, Vap), Vap_Macenta_monthly_2015)
rm(Vap_Macenta_monthly_2015, Vap_Macenta_monthly_2015a)
Vap_Macenta_monthly$measurement <- "Vap"
Vap_Macenta_monthly <- rename(Vap_Macenta_monthly, Value=Vap)
#Mali
Vap_Mali_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             Vap1=vap.var[which(lon==12.75),which(lat==12.25),1:48],
                                             Vap2=vap.var[which(lon==12.25),which(lat==12.25),1:48], 
                                             Vap3=vap.var[which(lon==11.75),which(lat==12.25),1:48],
                                             Vap4=vap.var[which(lon==12.75),which(lat==11.75),1:48],
                                             Vap5=vap.var[which(lon==12.25),which(lat==11.75),1:48]))
Vap_Mali_monthly$Vap <- rowMeans(select(Vap_Mali_monthly, Vap1, Vap2, Vap3, Vap4, Vap5))
Vap_Mali_monthly$Location <- 'Mali'
Vap_Mali_monthly$date <- ymd(paste(Vap_Mali_monthly$Year, Vap_Mali_monthly$Month, Vap_Mali_monthly$day, sep="-"))
Vap_Mali_monthly_2015 <- select(Vap_Mali_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Mali_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Mali_monthly_2015a$Location <- 'Mali'
Vap_Mali_monthly_2015a$date <- ymd(paste(Vap_Mali_monthly_2015a$Year, Vap_Mali_monthly_2015a$Month, Vap_Mali_monthly_2015a$day, sep="-"))
Vap_Mali_monthly_2015a <- select(Vap_Mali_monthly_2015a, date, Year, Month, day, Location)
Vap_Mali_monthly_2015 <- full_join(Vap_Mali_monthly_2015a, Vap_Mali_monthly_2015)
Vap_Mali_monthly <- rbind(select(Vap_Mali_monthly,date, Year, Month, day, Location, Vap), Vap_Mali_monthly_2015)
rm(Vap_Mali_monthly_2015, Vap_Mali_monthly_2015a)
Vap_Mali_monthly$measurement <- "Vap"
Vap_Mali_monthly <- rename(Vap_Mali_monthly, Value=Vap)
#Mamou
Vap_Mamou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           Vap1=vap.var[which(lon==11.75),which(lat==10.75),1:48],
                                           Vap2=vap.var[which(lon==12.25),which(lat==10.25),1:48],
                                           Vap3=vap.var[which(lon==11.75),which(lat==10.25),1:48]))
Vap_Mamou_monthly$Vap <- rowMeans(select(Vap_Mamou_monthly, Vap1, Vap2, Vap3))
Vap_Mamou_monthly$Location <- 'Mamou'
Vap_Mamou_monthly$date <- ymd(paste(Vap_Mamou_monthly$Year, Vap_Mamou_monthly$Month, Vap_Mamou_monthly$day, sep="-"))
Vap_Mamou_monthly_2015 <- select(Vap_Mamou_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Mamou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Mamou_monthly_2015a$Location <- 'Mamou'
Vap_Mamou_monthly_2015a$date <- ymd(paste(Vap_Mamou_monthly_2015a$Year, Vap_Mamou_monthly_2015a$Month, Vap_Mamou_monthly_2015a$day, sep="-"))
Vap_Mamou_monthly_2015a <- select(Vap_Mamou_monthly_2015a, date, Year, Month, day, Location)
Vap_Mamou_monthly_2015 <- full_join(Vap_Mamou_monthly_2015a, Vap_Mamou_monthly_2015)
Vap_Mamou_monthly <- rbind(select(Vap_Mamou_monthly,date, Year, Month, day, Location, Vap), Vap_Mamou_monthly_2015)
rm(Vap_Mamou_monthly_2015, Vap_Mamou_monthly_2015a)
Vap_Mamou_monthly$measurement <- "Vap"
Vap_Mamou_monthly <- rename(Vap_Mamou_monthly, Value=Vap)
#Nzerekore
Vap_Nzerekore_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        Vap1=vap.var[which(lon==8.75),which(lat==8.25),1:48],
                                        Vap2=vap.var[which(lon==8.75),which(lat==7.75),1:48]))
Vap_Nzerekore_monthly$Vap <- rowMeans(select(Vap_Nzerekore_monthly, Vap1, Vap2))
Vap_Nzerekore_monthly$Location <- 'Nzerekore'
Vap_Nzerekore_monthly$date <- ymd(paste(Vap_Nzerekore_monthly$Year, Vap_Nzerekore_monthly$Month, Vap_Nzerekore_monthly$day, sep="-"))
Vap_Nzerekore_monthly_2015 <- select(Vap_Nzerekore_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Nzerekore_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Nzerekore_monthly_2015a$Location <- 'Nzerekore'
Vap_Nzerekore_monthly_2015a$date <- ymd(paste(Vap_Nzerekore_monthly_2015a$Year, Vap_Nzerekore_monthly_2015a$Month, Vap_Nzerekore_monthly_2015a$day, sep="-"))
Vap_Nzerekore_monthly_2015a <- select(Vap_Nzerekore_monthly_2015a, date, Year, Month, day, Location)
Vap_Nzerekore_monthly_2015 <- full_join(Vap_Nzerekore_monthly_2015a, Vap_Nzerekore_monthly_2015)
Vap_Nzerekore_monthly <- rbind(select(Vap_Nzerekore_monthly,date, Year, Month, day, Location, Vap), Vap_Nzerekore_monthly_2015)
rm(Vap_Nzerekore_monthly_2015, Vap_Nzerekore_monthly_2015a)
Vap_Nzerekore_monthly$measurement <- "Vap"
Vap_Nzerekore_monthly <- rename(Vap_Nzerekore_monthly, Value=Vap)
#Pita
Vap_Pita_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           Vap1=vap.var[which(lon==12.75),which(lat==11.25),1:48],
                                           Vap2=vap.var[which(lon==12.25),which(lat==11.25),1:48],
                                           Vap3=vap.var[which(lon==12.75),which(lat==10.75),1:48]))
Vap_Pita_monthly$Vap <- rowMeans(select(Vap_Pita_monthly, Vap1, Vap2, Vap3))
Vap_Pita_monthly$Location <- 'Pita'
Vap_Pita_monthly$date <- ymd(paste(Vap_Pita_monthly$Year, Vap_Pita_monthly$Month, Vap_Pita_monthly$day, sep="-"))
Vap_Pita_monthly_2015 <- select(Vap_Pita_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Pita_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Pita_monthly_2015a$Location <- 'Pita'
Vap_Pita_monthly_2015a$date <- ymd(paste(Vap_Pita_monthly_2015a$Year, Vap_Pita_monthly_2015a$Month, Vap_Pita_monthly_2015a$day, sep="-"))
Vap_Pita_monthly_2015a <- select(Vap_Pita_monthly_2015a, date, Year, Month, day, Location)
Vap_Pita_monthly_2015 <- full_join(Vap_Pita_monthly_2015a, Vap_Pita_monthly_2015)
Vap_Pita_monthly <- rbind(select(Vap_Pita_monthly,date, Year, Month, day, Location, Vap), Vap_Pita_monthly_2015)
rm(Vap_Pita_monthly_2015, Vap_Pita_monthly_2015a)
Vap_Pita_monthly$measurement <- "Vap"
Vap_Pita_monthly <- rename(Vap_Pita_monthly, Value=Vap)
#Siguiri
Vap_Siguiri_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        Vap1=vap.var[which(lon==9.25),which(lat==12.25),1:48],
                                        Vap2=vap.var[which(lon==10.25),which(lat==11.75),1:48], 
                                        Vap3=vap.var[which(lon==9.75),which(lat==11.75),1:48],
                                        Vap4=vap.var[which(lon==9.25),which(lat==11.75),1:48],
                                        Vap5=vap.var[which(lon==9.75),which(lat==11.25),1:48],
                                        Vap6=vap.var[which(lon==9.25),which(lat==11.25),1:48]))
Vap_Siguiri_monthly$Vap <- rowMeans(select(Vap_Siguiri_monthly, Vap1, Vap2, Vap3, Vap4, Vap5, Vap6))
Vap_Siguiri_monthly$Location <- 'Siguiri'
Vap_Siguiri_monthly$date <- ymd(paste(Vap_Siguiri_monthly$Year, Vap_Siguiri_monthly$Month, Vap_Siguiri_monthly$day, sep="-"))
Vap_Siguiri_monthly_2015 <- select(Vap_Siguiri_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Siguiri_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Siguiri_monthly_2015a$Location <- 'Siguiri'
Vap_Siguiri_monthly_2015a$date <- ymd(paste(Vap_Siguiri_monthly_2015a$Year, Vap_Siguiri_monthly_2015a$Month, Vap_Siguiri_monthly_2015a$day, sep="-"))
Vap_Siguiri_monthly_2015a <- select(Vap_Siguiri_monthly_2015a, date, Year, Month, day, Location)
Vap_Siguiri_monthly_2015 <- full_join(Vap_Siguiri_monthly_2015a, Vap_Siguiri_monthly_2015)
Vap_Siguiri_monthly <- rbind(select(Vap_Siguiri_monthly,date, Year, Month, day, Location, Vap), Vap_Siguiri_monthly_2015)
rm(Vap_Siguiri_monthly_2015, Vap_Siguiri_monthly_2015a)
Vap_Siguiri_monthly$measurement <- "Vap"
Vap_Siguiri_monthly <- rename(Vap_Siguiri_monthly, Value=Vap)
#Telimele
Vap_Telimele_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           Vap1=vap.var[which(lon==13.75),which(lat==11.25),1:48],
                                           Vap2=vap.var[which(lon==13.25),which(lat==11.25),1:48], 
                                           Vap3=vap.var[which(lon==13.75),which(lat==10.75),1:48],
                                           Vap4=vap.var[which(lon==13.25),which(lat==10.75),1:48]))
Vap_Telimele_monthly$Vap <- rowMeans(select(Vap_Telimele_monthly, Vap1, Vap2, Vap3, Vap4))
Vap_Telimele_monthly$Location <- 'Telimele'
Vap_Telimele_monthly$date <- ymd(paste(Vap_Telimele_monthly$Year, Vap_Telimele_monthly$Month, Vap_Telimele_monthly$day, sep="-"))
Vap_Telimele_monthly_2015 <- select(Vap_Telimele_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Telimele_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Telimele_monthly_2015a$Location <- 'Telimele'
Vap_Telimele_monthly_2015a$date <- ymd(paste(Vap_Telimele_monthly_2015a$Year, Vap_Telimele_monthly_2015a$Month, Vap_Telimele_monthly_2015a$day, sep="-"))
Vap_Telimele_monthly_2015a <- select(Vap_Telimele_monthly_2015a, date, Year, Month, day, Location)
Vap_Telimele_monthly_2015 <- full_join(Vap_Telimele_monthly_2015a, Vap_Telimele_monthly_2015)
Vap_Telimele_monthly <- rbind(select(Vap_Telimele_monthly,date, Year, Month, day, Location, Vap), Vap_Telimele_monthly_2015)
rm(Vap_Telimele_monthly_2015, Vap_Telimele_monthly_2015a)
Vap_Telimele_monthly$measurement <- "Vap"
Vap_Telimele_monthly <- rename(Vap_Telimele_monthly, Value=Vap)
#Tougue
Vap_Tougue_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4),day=1, 
                                             Vap1=vap.var[which(lon==11.25),which(lat==11.75),1:48],
                                             Vap2=vap.var[which(lon==11.75),which(lat==11.25),1:48]))
Vap_Tougue_monthly$Vap <- rowMeans(select(Vap_Tougue_monthly, Vap1, Vap2))
Vap_Tougue_monthly$Location <- 'Tougue'
Vap_Tougue_monthly$date <- ymd(paste(Vap_Tougue_monthly$Year, Vap_Tougue_monthly$Month, Vap_Tougue_monthly$day, sep="-"))
Vap_Tougue_monthly_2015 <- select(Vap_Tougue_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Tougue_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Tougue_monthly_2015a$Location <- 'Tougue'
Vap_Tougue_monthly_2015a$date <- ymd(paste(Vap_Tougue_monthly_2015a$Year, Vap_Tougue_monthly_2015a$Month, Vap_Tougue_monthly_2015a$day, sep="-"))
Vap_Tougue_monthly_2015a <- select(Vap_Tougue_monthly_2015a, date, Year, Month, day, Location)
Vap_Tougue_monthly_2015 <- full_join(Vap_Tougue_monthly_2015a, Vap_Tougue_monthly_2015)
Vap_Tougue_monthly <- rbind(select(Vap_Tougue_monthly,date, Year, Month, day, Location, Vap), Vap_Tougue_monthly_2015)
rm(Vap_Tougue_monthly_2015, Vap_Tougue_monthly_2015a)
Vap_Tougue_monthly$measurement <- "Vap"
Vap_Tougue_monthly <- rename(Vap_Tougue_monthly, Value=Vap)
#yamou
Vap_yamou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4),day=1, 
                                        Vap1=vap.var[which(lon==9.25),which(lat==7.75),1:48],
                                        Vap2=vap.var[which(lon==9.25),which(lat==7.25),1:48],
                                        Vap3=vap.var[which(lon==8.75),which(lat==7.25),1:48]))
Vap_yamou_monthly$Location <- 'yamou'
Vap_yamou_monthly$Vap <- rowMeans(select(Vap_yamou_monthly, Vap1, Vap2, Vap3))
Vap_yamou_monthly$date <- ymd(paste(Vap_yamou_monthly$Year, Vap_yamou_monthly$Month, Vap_yamou_monthly$day, sep="-"))
Vap_yamou_monthly_2015 <- select(Vap_yamou_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_yamou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_yamou_monthly_2015a$Location <- 'yamou'
Vap_yamou_monthly_2015a$date <- ymd(paste(Vap_yamou_monthly_2015a$Year, Vap_yamou_monthly_2015a$Month, Vap_yamou_monthly_2015a$day, sep="-"))
Vap_yamou_monthly_2015a <- select(Vap_yamou_monthly_2015a, date, Year, Month, day, Location)
Vap_yamou_monthly_2015 <- full_join(Vap_yamou_monthly_2015a, Vap_yamou_monthly_2015)
Vap_yamou_monthly <- rbind(select(Vap_yamou_monthly,date, Year, Month, day, Location, Vap), Vap_yamou_monthly_2015)
rm(Vap_yamou_monthly_2015, Vap_yamou_monthly_2015a)
Vap_yamou_monthly$measurement <- "Vap"
Vap_yamou_monthly <- rename(Vap_yamou_monthly, Value=Vap)

#Merging in long format
Vap_Guinea_monthly_district <- rbind(Vap_Beyla_monthly, Vap_Boke_monthly, Vap_Boffa_monthly,
                                     Vap_Conakry_monthly, Vap_Coyah_monthly, Vap_Dabola_monthly, Vap_Dalaba_monthly,
                                     Vap_Dinguiray_monthly, Vap_Dubreka_monthly, Vap_Faranah_monthly,
                                     Vap_Forecariah_monthly, Vap_Fria_monthly, Vap_Gaoual_monthly,
                                     Vap_Gueckedou_monthly, Vap_Kankan_monthly, Vap_Kerouane_monthly,
                                     Vap_Kindia_monthly, Vap_Kissidougou_monthly, Vap_Koubia_monthly,
                                     Vap_Koundara_monthly, Vap_Kouroussa_monthly, Vap_Labe_monthly,
                                     Vap_Lelouma_monthly, Vap_Lola_monthly, Vap_Macenta_monthly,
                                     Vap_Mali_monthly, Vap_Mamou_monthly, Vap_Nzerekore_monthly,
                                     Vap_Pita_monthly, Vap_Siguiri_monthly, Vap_Telimele_monthly,
                                     Vap_Tougue_monthly, Vap_yamou_monthly)


#####################
#Wet - Wet days
#####################
wet.full <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.01.2011.2014.wet.dat.nc', write=FALSE)
wet.var <- var.get.nc(wet.full, "wet")
lon <- var.get.nc(wet.full, "lon")
lat <- var.get.nc(wet.full, "lat")
#Beyla
wet_Beyla_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         wet1=wet.var[which(lon==8.75),which(lat==9.25),1:48],
                                         wet2=wet.var[which(lon==8.25),which(lat==9.25),1:48], 
                                         wet3=wet.var[which(lon==7.75),which(lat==9.25),1:48],
                                         wet4=wet.var[which(lon==8.75),which(lat==8.75),1:48], 
                                         wet5=wet.var[which(lon==8.25),which(lat==8.75),1:48],
                                         wet6=wet.var[which(lon==7.75),which(lat==8.75),1:48], 
                                         wet7=wet.var[which(lon==8.75),which(lat==8.25),1:48],
                                         wet8=wet.var[which(lon==8.25),which(lat==8.25),1:48]))
wet_Beyla_monthly$wet <- rowMeans(select(wet_Beyla_monthly, wet1, wet2, wet3, wet4, wet5, wet6, wet7, wet8))
wet_Beyla_monthly$Location <- 'Beyla'
wet_Beyla_monthly$date <- ymd(paste(wet_Beyla_monthly$Year, wet_Beyla_monthly$Month, wet_Beyla_monthly$day, sep="-"))
wet_Beyla_monthly_2015 <- select(wet_Beyla_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Beyla_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Beyla_monthly_2015a$Location <- 'Beyla'
wet_Beyla_monthly_2015a$date <- ymd(paste(wet_Beyla_monthly_2015a$Year, wet_Beyla_monthly_2015a$Month, wet_Beyla_monthly_2015a$day, sep="-"))
wet_Beyla_monthly_2015a <- select(wet_Beyla_monthly_2015a, date, Year, Month, day, Location)
wet_Beyla_monthly_2015 <- full_join(wet_Beyla_monthly_2015a, wet_Beyla_monthly_2015)
wet_Beyla_monthly <- rbind(select(wet_Beyla_monthly,date, Year, Month, day, Location, wet), wet_Beyla_monthly_2015)
rm(wet_Beyla_monthly_2015, wet_Beyla_monthly_2015a)
wet_Beyla_monthly$measurement <- "wet"
wet_Beyla_monthly <- rename(wet_Beyla_monthly, Value=wet)
#Boffa
wet_Boffa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         wet1=wet.var[which(lon==14.25),which(lat==10.75),1:48],
                                         wet2=wet.var[which(lon==13.75),which(lat==10.75),1:48], 
                                         wet3=wet.var[which(lon==14.25),which(lat==10.25),1:48],
                                         wet4=wet.var[which(lon==13.75),which(lat==10.25),1:48]))
wet_Boffa_monthly$wet <- rowMeans(select(wet_Boffa_monthly, wet1, wet2, wet3, wet4))
wet_Boffa_monthly$Location <- 'Boffa'
wet_Boffa_monthly$date <- ymd(paste(wet_Boffa_monthly$Year, wet_Boffa_monthly$Month, wet_Boffa_monthly$day, sep="-"))
wet_Boffa_monthly_2015 <- select(wet_Boffa_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Boffa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Boffa_monthly_2015a$Location <- 'Boffa'
wet_Boffa_monthly_2015a$date <- ymd(paste(wet_Boffa_monthly_2015a$Year, wet_Boffa_monthly_2015a$Month, wet_Boffa_monthly_2015a$day, sep="-"))
wet_Boffa_monthly_2015a <- select(wet_Boffa_monthly_2015a, date, Year, Month, day, Location)
wet_Boffa_monthly_2015 <- full_join(wet_Boffa_monthly_2015a, wet_Boffa_monthly_2015)
wet_Boffa_monthly <- rbind(select(wet_Boffa_monthly,date, Year, Month, day, Location, wet), wet_Boffa_monthly_2015)
rm(wet_Boffa_monthly_2015, wet_Boffa_monthly_2015a)
wet_Boffa_monthly$measurement <- "wet"
wet_Boffa_monthly <- rename(wet_Boffa_monthly, Value=wet)
#Boke
wet_Boke_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        wet1=wet.var[which(lon==14.25),which(lat==11.75),1:48],
                                        wet2=wet.var[which(lon==14.75),which(lat==11.25),1:48], 
                                        wet3=wet.var[which(lon==14.25),which(lat==11.25),1:48],
                                        wet4=wet.var[which(lon==13.75),which(lat==11.25),1:48]))
wet_Boke_monthly$wet <- rowMeans(select(wet_Boke_monthly, wet1, wet2, wet3, wet4))
wet_Boke_monthly$Location <- 'Boke'
wet_Boke_monthly$date <- ymd(paste(wet_Boke_monthly$Year, wet_Boke_monthly$Month, wet_Boke_monthly$day, sep="-"))
wet_Boke_monthly_2015 <- select(wet_Boke_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Boke_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Boke_monthly_2015a$Location <- 'Boke'
wet_Boke_monthly_2015a$date <- ymd(paste(wet_Boke_monthly_2015a$Year, wet_Boke_monthly_2015a$Month, wet_Boke_monthly_2015a$day, sep="-"))
wet_Boke_monthly_2015a <- select(wet_Boke_monthly_2015a, date, Year, Month, day, Location)
wet_Boke_monthly_2015 <- full_join(wet_Boke_monthly_2015a, wet_Boke_monthly_2015)
wet_Boke_monthly <- rbind(select(wet_Boke_monthly,date, Year, Month, day, Location, wet), wet_Boke_monthly_2015)
rm(wet_Boke_monthly_2015, wet_Boke_monthly_2015a)
wet_Boke_monthly$measurement <- "wet"
wet_Boke_monthly <- rename(wet_Boke_monthly, Value=wet)
#Conakry
wet_Conakry_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           wet=wet.var[which(lon==13.75),which(lat==9.75),1:48]))
wet_Conakry_monthly$Location <- 'Conakry'
wet_Conakry_monthly$date <- ymd(paste(wet_Conakry_monthly$Year, wet_Conakry_monthly$Month, wet_Conakry_monthly$day, sep="-"))
wet_Conakry_monthly_2015 <- select(wet_Conakry_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Conakry_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Conakry_monthly_2015a$Location <- 'Conakry'
wet_Conakry_monthly_2015a$date <- ymd(paste(wet_Conakry_monthly_2015a$Year, wet_Conakry_monthly_2015a$Month, wet_Conakry_monthly_2015a$day, sep="-"))
wet_Conakry_monthly_2015a <- select(wet_Conakry_monthly_2015a, date, Year, Month, day, Location)
wet_Conakry_monthly_2015 <- full_join(wet_Conakry_monthly_2015a, wet_Conakry_monthly_2015)
wet_Conakry_monthly <- rbind(select(wet_Conakry_monthly,date, Year, Month, day, Location, wet), wet_Conakry_monthly_2015)
rm(wet_Conakry_monthly_2015, wet_Conakry_monthly_2015a)
wet_Conakry_monthly$measurement <- "wet"
wet_Conakry_monthly <- rename(wet_Conakry_monthly, Value=wet)

#Coyah
wet_Coyah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         wet=wet.var[which(lon==9.75),which(lat==13.25),1:48]))
wet_Coyah_monthly$Location <- 'Coyah'
wet_Coyah_monthly$date <- ymd(paste(wet_Coyah_monthly$Year, wet_Coyah_monthly$Month, wet_Coyah_monthly$day, sep="-"))
wet_Coyah_monthly_2015 <- select(wet_Coyah_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Coyah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Coyah_monthly_2015a$Location <- 'Coyah'
wet_Coyah_monthly_2015a$date <- ymd(paste(wet_Coyah_monthly_2015a$Year, wet_Coyah_monthly_2015a$Month, wet_Coyah_monthly_2015a$day, sep="-"))
wet_Coyah_monthly_2015a <- select(wet_Coyah_monthly_2015a, date, Year, Month, day, Location)
wet_Coyah_monthly_2015 <- full_join(wet_Coyah_monthly_2015a, wet_Coyah_monthly_2015)
wet_Coyah_monthly <- rbind(select(wet_Coyah_monthly,date, Year, Month, day, Location, wet), wet_Coyah_monthly_2015)
rm(wet_Coyah_monthly_2015, wet_Coyah_monthly_2015a)
wet_Coyah_monthly$measurement <- "wet"
wet_Coyah_monthly <- rename(wet_Coyah_monthly, Value=wet)
#Dabola
wet_Dabola_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          wet1=wet.var[which(lon==11.25),which(lat==10.75),1:48],
                                          wet2=wet.var[which(lon==10.75),which(lat==10.75),1:48]))
wet_Dabola_monthly$wet <- rowMeans(select(wet_Dabola_monthly, wet1, wet2))
wet_Dabola_monthly$Location <- 'Dabola'
wet_Dabola_monthly$date <- ymd(paste(wet_Dabola_monthly$Year, wet_Dabola_monthly$Month, wet_Dabola_monthly$day, sep="-"))
wet_Dabola_monthly_2015 <- select(wet_Dabola_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Dabola_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Dabola_monthly_2015a$Location <- 'Dabola'
wet_Dabola_monthly_2015a$date <- ymd(paste(wet_Dabola_monthly_2015a$Year, wet_Dabola_monthly_2015a$Month, wet_Dabola_monthly_2015a$day, sep="-"))
wet_Dabola_monthly_2015a <- select(wet_Dabola_monthly_2015a, date, Year, Month, day, Location)
wet_Dabola_monthly_2015 <- full_join(wet_Dabola_monthly_2015a, wet_Dabola_monthly_2015)
wet_Dabola_monthly <- rbind(select(wet_Dabola_monthly,date, Year, Month, day, Location, wet), wet_Dabola_monthly_2015)
rm(wet_Dabola_monthly_2015, wet_Dabola_monthly_2015a)
wet_Dabola_monthly$measurement <- "wet"
wet_Dabola_monthly <- rename(wet_Dabola_monthly, Value=wet)
#Dalaba
wet_Dalaba_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          wet1=wet.var[which(lon==11.25),which(lat==12.25),1:48],
                                          wet2=wet.var[which(lon==10.75),which(lat==12.25),1:48]))
wet_Dalaba_monthly$wet <- rowMeans(select(wet_Dalaba_monthly, wet1, wet2))
wet_Dalaba_monthly$Location <- 'Dalaba'
wet_Dalaba_monthly$date <- ymd(paste(wet_Dalaba_monthly$Year, wet_Dalaba_monthly$Month, wet_Dalaba_monthly$day, sep="-"))
wet_Dalaba_monthly_2015 <- select(wet_Dalaba_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Dalaba_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Dalaba_monthly_2015a$Location <- 'Dalaba'
wet_Dalaba_monthly_2015a$date <- ymd(paste(wet_Dalaba_monthly_2015a$Year, wet_Dalaba_monthly_2015a$Month, wet_Dalaba_monthly_2015a$day, sep="-"))
wet_Dalaba_monthly_2015a <- select(wet_Dalaba_monthly_2015a, date, Year, Month, day, Location)
wet_Dalaba_monthly_2015 <- full_join(wet_Dalaba_monthly_2015a, wet_Dalaba_monthly_2015)
wet_Dalaba_monthly <- rbind(select(wet_Dalaba_monthly,date, Year, Month, day, Location, wet), wet_Dalaba_monthly_2015)
rm(wet_Dalaba_monthly_2015, wet_Dalaba_monthly_2015a)
wet_Dalaba_monthly$measurement <- "wet"
wet_Dalaba_monthly <- rename(wet_Dalaba_monthly, Value=wet)
#Dinguiray
wet_Dinguiray_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             wet1=wet.var[which(lon==11.25),which(lat==11.75),1:48],
                                             wet2=wet.var[which(lon==10.75),which(lat==11.75),1:48], 
                                             wet3=wet.var[which(lon==10.25),which(lat==11.75),1:48],
                                             wet4=wet.var[which(lon==11.25),which(lat==11.25),1:48],
                                             wet5=wet.var[which(lon==10.75),which(lat==11.25),1:48],
                                             wet6=wet.var[which(lon==10.25),which(lat==11.25),1:48]))
wet_Dinguiray_monthly$wet <- rowMeans(select(wet_Dinguiray_monthly, wet1, wet2, wet3, wet4, wet5, wet6))
wet_Dinguiray_monthly$Location <- 'Dinguiray'
wet_Dinguiray_monthly$date <- ymd(paste(wet_Dinguiray_monthly$Year, wet_Dinguiray_monthly$Month, wet_Dinguiray_monthly$day, sep="-"))
wet_Dinguiray_monthly_2015 <- select(wet_Dinguiray_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Dinguiray_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Dinguiray_monthly_2015a$Location <- 'Dinguiray'
wet_Dinguiray_monthly_2015a$date <- ymd(paste(wet_Dinguiray_monthly_2015a$Year, wet_Dinguiray_monthly_2015a$Month, wet_Dinguiray_monthly_2015a$day, sep="-"))
wet_Dinguiray_monthly_2015a <- select(wet_Dinguiray_monthly_2015a, date, Year, Month, day, Location)
wet_Dinguiray_monthly_2015 <- full_join(wet_Dinguiray_monthly_2015a, wet_Dinguiray_monthly_2015)
wet_Dinguiray_monthly <- rbind(select(wet_Dinguiray_monthly,date, Year, Month, day, Location, wet), wet_Dinguiray_monthly_2015)
rm(wet_Dinguiray_monthly_2015, wet_Dinguiray_monthly_2015a)
wet_Dinguiray_monthly$measurement <- "wet"
wet_Dinguiray_monthly <- rename(wet_Dinguiray_monthly, Value=wet)
#Dubreka
wet_Dubreka_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           wet1=wet.var[which(lon==13.75),which(lat==10.25),1:48],
                                           wet2=wet.var[which(lon==13.25),which(lat==10.25),1:48]))
wet_Dubreka_monthly$wet <- rowMeans(select(wet_Dubreka_monthly, wet1, wet2))
wet_Dubreka_monthly$Location <- 'Dubreka'
wet_Dubreka_monthly$date <- ymd(paste(wet_Dubreka_monthly$Year, wet_Dubreka_monthly$Month, wet_Dubreka_monthly$day, sep="-"))
wet_Dubreka_monthly_2015 <- select(wet_Dubreka_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Dubreka_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Dubreka_monthly_2015a$Location <- 'Dubreka'
wet_Dubreka_monthly_2015a$date <- ymd(paste(wet_Dubreka_monthly_2015a$Year, wet_Dubreka_monthly_2015a$Month, wet_Dubreka_monthly_2015a$day, sep="-"))
wet_Dubreka_monthly_2015a <- select(wet_Dubreka_monthly_2015a, date, Year, Month, day, Location)
wet_Dubreka_monthly_2015 <- full_join(wet_Dubreka_monthly_2015a, wet_Dubreka_monthly_2015)
wet_Dubreka_monthly <- rbind(select(wet_Dubreka_monthly,date, Year, Month, day, Location, wet), wet_Dubreka_monthly_2015)
rm(wet_Dubreka_monthly_2015, wet_Dubreka_monthly_2015a)
wet_Dubreka_monthly$measurement <- "wet"
wet_Dubreka_monthly <- rename(wet_Dubreka_monthly, Value=wet)
#Faranah
wet_Faranah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           wet1=wet.var[which(lon==11.25),which(lat==10.25),1:48],
                                           wet2=wet.var[which(lon==10.75),which(lat==10.25),1:48], 
                                           wet3=wet.var[which(lon==10.25),which(lat==10.25),1:48],
                                           wet4=wet.var[which(lon==10.75),which(lat==9.75),1:48],
                                           wet5=wet.var[which(lon==10.75),which(lat==9.25),1:48]))
wet_Faranah_monthly$wet <- rowMeans(select(wet_Faranah_monthly, wet1, wet2, wet3, wet4, wet5))
wet_Faranah_monthly$Location <- 'Faranah'
wet_Faranah_monthly$date <- ymd(paste(wet_Faranah_monthly$Year, wet_Faranah_monthly$Month, wet_Faranah_monthly$day, sep="-"))
wet_Faranah_monthly_2015 <- select(wet_Faranah_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Faranah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Faranah_monthly_2015a$Location <- 'Faranah'
wet_Faranah_monthly_2015a$date <- ymd(paste(wet_Faranah_monthly_2015a$Year, wet_Faranah_monthly_2015a$Month, wet_Faranah_monthly_2015a$day, sep="-"))
wet_Faranah_monthly_2015a <- select(wet_Faranah_monthly_2015a, date, Year, Month, day, Location)
wet_Faranah_monthly_2015 <- full_join(wet_Faranah_monthly_2015a, wet_Faranah_monthly_2015)
wet_Faranah_monthly <- rbind(select(wet_Faranah_monthly,date, Year, Month, day, Location, wet), wet_Faranah_monthly_2015)
rm(wet_Faranah_monthly_2015, wet_Faranah_monthly_2015a)
wet_Faranah_monthly$measurement <- "wet"
wet_Faranah_monthly <- rename(wet_Faranah_monthly, Value=wet)
#Forecariah
wet_Forecariah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                              Month=rep(seq(1,12,1), 4), day=1,
                                              wet1=wet.var[which(lon==12.75),which(lat==9.75),1:48],
                                              wet2=wet.var[which(lon==13.25),which(lat==9.25),1:48], 
                                              wet3=wet.var[which(lon==12.75),which(lat==9.25),1:48]))
wet_Forecariah_monthly$wet <- rowMeans(select(wet_Forecariah_monthly, wet1, wet2, wet3))
wet_Forecariah_monthly$Location <- 'Forecariah'
wet_Forecariah_monthly$date <- ymd(paste(wet_Forecariah_monthly$Year, wet_Forecariah_monthly$Month, wet_Forecariah_monthly$day, sep="-"))
wet_Forecariah_monthly_2015 <- select(wet_Forecariah_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Forecariah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Forecariah_monthly_2015a$Location <- 'Forecariah'
wet_Forecariah_monthly_2015a$date <- ymd(paste(wet_Forecariah_monthly_2015a$Year, wet_Forecariah_monthly_2015a$Month, wet_Forecariah_monthly_2015a$day, sep="-"))
wet_Forecariah_monthly_2015a <- select(wet_Forecariah_monthly_2015a, date, Year, Month, day, Location)
wet_Forecariah_monthly_2015 <- full_join(wet_Forecariah_monthly_2015a, wet_Forecariah_monthly_2015)
wet_Forecariah_monthly <- rbind(select(wet_Forecariah_monthly,date, Year, Month, day, Location, wet), wet_Forecariah_monthly_2015)
rm(wet_Forecariah_monthly_2015, wet_Forecariah_monthly_2015a)
wet_Forecariah_monthly$measurement <- "wet"
wet_Forecariah_monthly <- rename(wet_Forecariah_monthly, Value=wet)
#Fria
wet_Fria_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        wet1=wet.var[which(lon==13.75),which(lat==10.75),1:48],
                                        wet2=wet.var[which(lon==13.75),which(lat==10.25),1:48]))
wet_Fria_monthly$wet <- rowMeans(select(wet_Fria_monthly, wet1, wet2))
wet_Fria_monthly$Location <- 'Fria'
wet_Fria_monthly$date <- ymd(paste(wet_Fria_monthly$Year, wet_Fria_monthly$Month, wet_Fria_monthly$day, sep="-"))
wet_Fria_monthly_2015 <- select(wet_Fria_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Fria_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Fria_monthly_2015a$Location <- 'Fria'
wet_Fria_monthly_2015a$date <- ymd(paste(wet_Fria_monthly_2015a$Year, wet_Fria_monthly_2015a$Month, wet_Fria_monthly_2015a$day, sep="-"))
wet_Fria_monthly_2015a <- select(wet_Fria_monthly_2015a, date, Year, Month, day, Location)
wet_Fria_monthly_2015 <- full_join(wet_Fria_monthly_2015a, wet_Fria_monthly_2015)
wet_Fria_monthly <- rbind(select(wet_Fria_monthly,date, Year, Month, day, Location, wet), wet_Fria_monthly_2015)
rm(wet_Fria_monthly_2015, wet_Fria_monthly_2015a)
wet_Fria_monthly$measurement <- "wet"
wet_Fria_monthly <- rename(wet_Fria_monthly, Value=wet)
#Gaoual
wet_Gaoual_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          wet1=wet.var[which(lon==13.75),which(lat==12.25),1:48],
                                          wet2=wet.var[which(lon==13.25),which(lat==12.25),1:48], 
                                          wet3=wet.var[which(lon==13.75),which(lat==11.75),1:48],
                                          wet4=wet.var[which(lon==13.25),which(lat==11.75),1:48],
                                          wet5=wet.var[which(lon==12.75),which(lat==11.75),1:48],
                                          wet6=wet.var[which(lon==13.75),which(lat==11.25),1:48],
                                          wet7=wet.var[which(lon==13.25),which(lat==11.25),1:48]))
wet_Gaoual_monthly$wet <- rowMeans(select(wet_Gaoual_monthly, wet1, wet2, wet3, wet4, wet5, wet6, wet7))
wet_Gaoual_monthly$Location <- 'Gaoual'
wet_Gaoual_monthly$date <- ymd(paste(wet_Gaoual_monthly$Year, wet_Gaoual_monthly$Month, wet_Gaoual_monthly$day, sep="-"))
wet_Gaoual_monthly_2015 <- select(wet_Gaoual_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Gaoual_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Gaoual_monthly_2015a$Location <- 'Gaoual'
wet_Gaoual_monthly_2015a$date <- ymd(paste(wet_Gaoual_monthly_2015a$Year, wet_Gaoual_monthly_2015a$Month, wet_Gaoual_monthly_2015a$day, sep="-"))
wet_Gaoual_monthly_2015a <- select(wet_Gaoual_monthly_2015a, date, Year, Month, day, Location)
wet_Gaoual_monthly_2015 <- full_join(wet_Gaoual_monthly_2015a, wet_Gaoual_monthly_2015)
wet_Gaoual_monthly <- rbind(select(wet_Gaoual_monthly,date, Year, Month, day, Location, wet), wet_Gaoual_monthly_2015)
rm(wet_Gaoual_monthly_2015, wet_Gaoual_monthly_2015a)
wet_Gaoual_monthly$measurement <- "wet"
wet_Gaoual_monthly <- rename(wet_Gaoual_monthly, Value=wet)
#Gueckedou
wet_Gueckedou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             wet=wet.var[which(lon==10.25),which(lat==8.75),1:48]))
wet_Gueckedou_monthly$Location <- 'Gueckedou'
wet_Gueckedou_monthly$date <- ymd(paste(wet_Gueckedou_monthly$Year, wet_Gueckedou_monthly$Month, wet_Gueckedou_monthly$day, sep="-"))
wet_Gueckedou_monthly_2015 <- select(wet_Gueckedou_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Gueckedou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Gueckedou_monthly_2015a$Location <- 'Gueckedou'
wet_Gueckedou_monthly_2015a$date <- ymd(paste(wet_Gueckedou_monthly_2015a$Year, wet_Gueckedou_monthly_2015a$Month, wet_Gueckedou_monthly_2015a$day, sep="-"))
wet_Gueckedou_monthly_2015a <- select(wet_Gueckedou_monthly_2015a, date, Year, Month, day, Location)
wet_Gueckedou_monthly_2015 <- full_join(wet_Gueckedou_monthly_2015a, wet_Gueckedou_monthly_2015)
wet_Gueckedou_monthly <- rbind(select(wet_Gueckedou_monthly,date, Year, Month, day, Location, wet), wet_Gueckedou_monthly_2015)
rm(wet_Gueckedou_monthly_2015, wet_Gueckedou_monthly_2015a)
wet_Gueckedou_monthly$measurement <- "wet"
wet_Gueckedou_monthly <- rename(wet_Gueckedou_monthly, Value=wet)
#Kankan
wet_Kankan_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          wet1=wet.var[which(lon==9.25),which(lat==10.75),1:48],
                                          wet2=wet.var[which(lon==9.75),which(lat==10.25),1:48], 
                                          wet3=wet.var[which(lon==9.25),which(lat==10.25),1:48],
                                          wet4=wet.var[which(lon==8.75),which(lat==10.25),1:48],
                                          wet5=wet.var[which(lon==9.75),which(lat==9.75),1:48],
                                          wet6=wet.var[which(lon==9.25),which(lat==9.75),1:48]))
wet_Kankan_monthly$wet <- rowMeans(select(wet_Kankan_monthly, wet1, wet2, wet3, wet4, wet5, wet6))
wet_Kankan_monthly$Location <- 'Kankan'
wet_Kankan_monthly$date <- ymd(paste(wet_Kankan_monthly$Year, wet_Kankan_monthly$Month, wet_Kankan_monthly$day, sep="-"))
wet_Kankan_monthly_2015 <- select(wet_Kankan_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Kankan_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Kankan_monthly_2015a$Location <- 'Kankan'
wet_Kankan_monthly_2015a$date <- ymd(paste(wet_Kankan_monthly_2015a$Year, wet_Kankan_monthly_2015a$Month, wet_Kankan_monthly_2015a$day, sep="-"))
wet_Kankan_monthly_2015a <- select(wet_Kankan_monthly_2015a, date, Year, Month, day, Location)
wet_Kankan_monthly_2015 <- full_join(wet_Kankan_monthly_2015a, wet_Kankan_monthly_2015)
wet_Kankan_monthly <- rbind(select(wet_Kankan_monthly,date, Year, Month, day, Location, wet), wet_Kankan_monthly_2015)
rm(wet_Kankan_monthly_2015, wet_Kankan_monthly_2015a)
wet_Kankan_monthly$measurement <- "wet"
wet_Kankan_monthly <- rename(wet_Kankan_monthly, Value=wet)
#Kerouane
wet_Kerouane_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            wet1=wet.var[which(lon==9.25),which(lat==9.75),1:48],
                                            wet2=wet.var[which(lon==8.75),which(lat==9.75),1:48], 
                                            wet3=wet.var[which(lon==9.75),which(lat==9.25),1:48],
                                            wet4=wet.var[which(lon==9.25),which(lat==9.25),1:48],
                                            wet5=wet.var[which(lon==8.75),which(lat==9.25),1:48],
                                            wet6=wet.var[which(lon==9.25),which(lat==8.75),1:48]))
wet_Kerouane_monthly$wet <- rowMeans(select(wet_Kerouane_monthly, wet1, wet2, wet3, wet4, wet5, wet6))
wet_Kerouane_monthly$Location <- 'Kerouane'
wet_Kerouane_monthly$date <- ymd(paste(wet_Kerouane_monthly$Year, wet_Kerouane_monthly$Month, wet_Kerouane_monthly$day, sep="-"))
wet_Kerouane_monthly_2015 <- select(wet_Kerouane_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Kerouane_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Kerouane_monthly_2015a$Location <- 'Kerouane'
wet_Kerouane_monthly_2015a$date <- ymd(paste(wet_Kerouane_monthly_2015a$Year, wet_Kerouane_monthly_2015a$Month, wet_Kerouane_monthly_2015a$day, sep="-"))
wet_Kerouane_monthly_2015a <- select(wet_Kerouane_monthly_2015a, date, Year, Month, day, Location)
wet_Kerouane_monthly_2015 <- full_join(wet_Kerouane_monthly_2015a, wet_Kerouane_monthly_2015)
wet_Kerouane_monthly <- rbind(select(wet_Kerouane_monthly,date, Year, Month, day, Location, wet), wet_Kerouane_monthly_2015)
rm(wet_Kerouane_monthly_2015, wet_Kerouane_monthly_2015a)
wet_Kerouane_monthly$measurement <- "wet"
wet_Kerouane_monthly <- rename(wet_Kerouane_monthly, Value=wet)
#Kindia
wet_Kindia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          wet1=wet.var[which(lon==13.25),which(lat==10.25),1:48],
                                          wet2=wet.var[which(lon==12.75),which(lat==10.25),1:48], 
                                          wet3=wet.var[which(lon==12.25),which(lat==10.25),1:48],
                                          wet4=wet.var[which(lon==12.75),which(lat==9.75),1:48]))
wet_Kindia_monthly$wet <- rowMeans(select(wet_Kindia_monthly, wet1, wet2, wet3, wet4))
wet_Kindia_monthly$Location <- 'Kindia'
wet_Kindia_monthly$date <- ymd(paste(wet_Kindia_monthly$Year, wet_Kindia_monthly$Month, wet_Kindia_monthly$day, sep="-"))
wet_Kindia_monthly_2015 <- select(wet_Kindia_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Kindia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Kindia_monthly_2015a$Location <- 'Kindia'
wet_Kindia_monthly_2015a$date <- ymd(paste(wet_Kindia_monthly_2015a$Year, wet_Kindia_monthly_2015a$Month, wet_Kindia_monthly_2015a$day, sep="-"))
wet_Kindia_monthly_2015a <- select(wet_Kindia_monthly_2015a, date, Year, Month, day, Location)
wet_Kindia_monthly_2015 <- full_join(wet_Kindia_monthly_2015a, wet_Kindia_monthly_2015)
wet_Kindia_monthly <- rbind(select(wet_Kindia_monthly,date, Year, Month, day, Location, wet), wet_Kindia_monthly_2015)
rm(wet_Kindia_monthly_2015, wet_Kindia_monthly_2015a)
wet_Kindia_monthly$measurement <- "wet"
wet_Kindia_monthly <- rename(wet_Kindia_monthly, Value=wet)
#KISSIDOUGOU
wet_Kissidougou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               wet1=wet.var[which(lon==10.25),which(lat==9.75),1:48],
                                               wet2=wet.var[which(lon==10.25),which(lat==9.25),1:48], 
                                               wet3=wet.var[which(lon==9.75),which(lat==9.25),1:48]))
wet_Kissidougou_monthly$wet <- rowMeans(select(wet_Kissidougou_monthly, wet1, wet2, wet3))
wet_Kissidougou_monthly$Location <- 'Kissidougou'
wet_Kissidougou_monthly$date <- ymd(paste(wet_Kissidougou_monthly$Year, wet_Kissidougou_monthly$Month, wet_Kissidougou_monthly$day, sep="-"))
wet_Kissidougou_monthly_2015 <- select(wet_Kissidougou_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Kissidougou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Kissidougou_monthly_2015a$Location <- 'Kissidougou'
wet_Kissidougou_monthly_2015a$date <- ymd(paste(wet_Kissidougou_monthly_2015a$Year, wet_Kissidougou_monthly_2015a$Month, wet_Kissidougou_monthly_2015a$day, sep="-"))
wet_Kissidougou_monthly_2015a <- select(wet_Kissidougou_monthly_2015a, date, Year, Month, day, Location)
wet_Kissidougou_monthly_2015 <- full_join(wet_Kissidougou_monthly_2015a, wet_Kissidougou_monthly_2015)
wet_Kissidougou_monthly <- rbind(select(wet_Kissidougou_monthly,date, Year, Month, day, Location, wet), wet_Kissidougou_monthly_2015)
rm(wet_Kissidougou_monthly_2015, wet_Kissidougou_monthly_2015a)
wet_Kissidougou_monthly$measurement <- "wet"
wet_Kissidougou_monthly <- rename(wet_Kissidougou_monthly, Value=wet)
#Koubia
wet_Koubia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          wet=wet.var[which(lon==10.25),which(lat==9.75),1:48]))
wet_Koubia_monthly$Location <- 'Koubia'
wet_Koubia_monthly$date <- ymd(paste(wet_Koubia_monthly$Year, wet_Koubia_monthly$Month, wet_Koubia_monthly$day, sep="-"))
wet_Koubia_monthly_2015 <- select(wet_Koubia_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Koubia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Koubia_monthly_2015a$Location <- 'Koubia'
wet_Koubia_monthly_2015a$date <- ymd(paste(wet_Koubia_monthly_2015a$Year, wet_Koubia_monthly_2015a$Month, wet_Koubia_monthly_2015a$day, sep="-"))
wet_Koubia_monthly_2015a <- select(wet_Koubia_monthly_2015a, date, Year, Month, day, Location)
wet_Koubia_monthly_2015 <- full_join(wet_Koubia_monthly_2015a, wet_Koubia_monthly_2015)
wet_Koubia_monthly <- rbind(select(wet_Koubia_monthly,date, Year, Month, day, Location, wet), wet_Koubia_monthly_2015)
rm(wet_Koubia_monthly_2015, wet_Koubia_monthly_2015a)
wet_Koubia_monthly$measurement <- "wet"
wet_Koubia_monthly <- rename(wet_Koubia_monthly, Value=wet)
#Koundara
wet_Koundara_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            wet1=wet.var[which(lon==13.25),which(lat==12.75),1:48],
                                            wet2=wet.var[which(lon==13.25),which(lat==12.25),1:48], 
                                            wet3=wet.var[which(lon==12.75),which(lat==12.25),1:48]))
wet_Koundara_monthly$wet <- rowMeans(select(wet_Koundara_monthly, wet1, wet2, wet3))
wet_Koundara_monthly$Location <- 'Koundara'
wet_Koundara_monthly$date <- ymd(paste(wet_Koundara_monthly$Year, wet_Koundara_monthly$Month, wet_Koundara_monthly$day, sep="-"))
wet_Koundara_monthly_2015 <- select(wet_Koundara_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Koundara_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Koundara_monthly_2015a$Location <- 'Koundara'
wet_Koundara_monthly_2015a$date <- ymd(paste(wet_Koundara_monthly_2015a$Year, wet_Koundara_monthly_2015a$Month, wet_Koundara_monthly_2015a$day, sep="-"))
wet_Koundara_monthly_2015a <- select(wet_Koundara_monthly_2015a, date, Year, Month, day, Location)
wet_Koundara_monthly_2015 <- full_join(wet_Koundara_monthly_2015a, wet_Koundara_monthly_2015)
wet_Koundara_monthly <- rbind(select(wet_Koundara_monthly,date, Year, Month, day, Location, wet), wet_Koundara_monthly_2015)
rm(wet_Koundara_monthly_2015, wet_Koundara_monthly_2015a)
wet_Koundara_monthly$measurement <- "wet"
wet_Koundara_monthly <- rename(wet_Koundara_monthly, Value=wet)
#Kouroussa
wet_Kouroussa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             wet1=wet.var[which(lon==10.25),which(lat==11.25),1:48],
                                             wet2=wet.var[which(lon==10.75),which(lat==10.75),1:48], 
                                             wet3=wet.var[which(lon==10.25),which(lat==10.75),1:48],
                                             wet4=wet.var[which(lon==9.75),which(lat==10.75),1:48],
                                             wet5=wet.var[which(lon==10.25),which(lat==10.25),1:48],
                                             wet6=wet.var[which(lon==9.75),which(lat==10.25),1:48],
                                             wet7=wet.var[which(lon==10.25),which(lat==9.75),1:48],
                                             wet8=wet.var[which(lon==9.75),which(lat==9.75),1:48]))
wet_Kouroussa_monthly$wet <- rowMeans(select(wet_Kouroussa_monthly, wet1, wet2, wet3, wet4, wet5, wet6, wet7, wet8))
wet_Kouroussa_monthly$Location <- 'Kouroussa'
wet_Kouroussa_monthly$date <- ymd(paste(wet_Kouroussa_monthly$Year, wet_Kouroussa_monthly$Month, wet_Kouroussa_monthly$day, sep="-"))
wet_Kouroussa_monthly_2015 <- select(wet_Kouroussa_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Kouroussa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Kouroussa_monthly_2015a$Location <- 'Kouroussa'
wet_Kouroussa_monthly_2015a$date <- ymd(paste(wet_Kouroussa_monthly_2015a$Year, wet_Kouroussa_monthly_2015a$Month, wet_Kouroussa_monthly_2015a$day, sep="-"))
wet_Kouroussa_monthly_2015a <- select(wet_Kouroussa_monthly_2015a, date, Year, Month, day, Location)
wet_Kouroussa_monthly_2015 <- full_join(wet_Kouroussa_monthly_2015a, wet_Kouroussa_monthly_2015)
wet_Kouroussa_monthly <- rbind(select(wet_Kouroussa_monthly,date, Year, Month, day, Location, wet), wet_Kouroussa_monthly_2015)
rm(wet_Kouroussa_monthly_2015, wet_Kouroussa_monthly_2015a)
wet_Kouroussa_monthly$measurement <- "wet"
wet_Kouroussa_monthly <- rename(wet_Kouroussa_monthly, Value=wet)
#Labe
wet_Labe_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        wet1=wet.var[which(lon==12.25),which(lat==11.75),1:48],
                                        wet2=wet.var[which(lon==12.25),which(lat==11.25),1:48]))
wet_Labe_monthly$wet <- rowMeans(select(wet_Labe_monthly, wet1, wet2))
wet_Labe_monthly$Location <- 'Labe'
wet_Labe_monthly$date <- ymd(paste(wet_Labe_monthly$Year, wet_Labe_monthly$Month, wet_Labe_monthly$day, sep="-"))
wet_Labe_monthly_2015 <- select(wet_Labe_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Labe_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Labe_monthly_2015a$Location <- 'Labe'
wet_Labe_monthly_2015a$date <- ymd(paste(wet_Labe_monthly_2015a$Year, wet_Labe_monthly_2015a$Month, wet_Labe_monthly_2015a$day, sep="-"))
wet_Labe_monthly_2015a <- select(wet_Labe_monthly_2015a, date, Year, Month, day, Location)
wet_Labe_monthly_2015 <- full_join(wet_Labe_monthly_2015a, wet_Labe_monthly_2015)
wet_Labe_monthly <- rbind(select(wet_Labe_monthly,date, Year, Month, day, Location, wet), wet_Labe_monthly_2015)
rm(wet_Labe_monthly_2015, wet_Labe_monthly_2015a)
wet_Labe_monthly$measurement <- "wet"
wet_Labe_monthly <- rename(wet_Labe_monthly, Value=wet)
#Lelouma
wet_Lelouma_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           wet1=wet.var[which(lon==12.75),which(lat==11.75),1:48],
                                           wet2=wet.var[which(lon==12.75),which(lat==11.25),1:48]))
wet_Lelouma_monthly$wet <- rowMeans(select(wet_Lelouma_monthly, wet1, wet2))
wet_Lelouma_monthly$Location <- 'Lelouma'
wet_Lelouma_monthly$date <- ymd(paste(wet_Lelouma_monthly$Year, wet_Lelouma_monthly$Month, wet_Lelouma_monthly$day, sep="-"))
wet_Lelouma_monthly_2015 <- select(wet_Lelouma_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Lelouma_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Lelouma_monthly_2015a$Location <- 'Lelouma'
wet_Lelouma_monthly_2015a$date <- ymd(paste(wet_Lelouma_monthly_2015a$Year, wet_Lelouma_monthly_2015a$Month, wet_Lelouma_monthly_2015a$day, sep="-"))
wet_Lelouma_monthly_2015a <- select(wet_Lelouma_monthly_2015a, date, Year, Month, day, Location)
wet_Lelouma_monthly_2015 <- full_join(wet_Lelouma_monthly_2015a, wet_Lelouma_monthly_2015)
wet_Lelouma_monthly <- rbind(select(wet_Lelouma_monthly,date, Year, Month, day, Location, wet), wet_Lelouma_monthly_2015)
rm(wet_Lelouma_monthly_2015, wet_Lelouma_monthly_2015a)
wet_Lelouma_monthly$measurement <- "wet"
wet_Lelouma_monthly <- rename(wet_Lelouma_monthly, Value=wet)
#Lola
wet_Lola_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        wet1=wet.var[which(lon==8.25),which(lat==8.25),1:48],
                                        wet2=wet.var[which(lon==8.25),which(lat==7.75),1:48]))
wet_Lola_monthly$wet <- rowMeans(select(wet_Lola_monthly, wet1, wet2))
wet_Lola_monthly$Location <- 'Lola'
wet_Lola_monthly$date <- ymd(paste(wet_Lola_monthly$Year, wet_Lola_monthly$Month, wet_Lola_monthly$day, sep="-"))
wet_Lola_monthly_2015 <- select(wet_Lola_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Lola_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Lola_monthly_2015a$Location <- 'Lola'
wet_Lola_monthly_2015a$date <- ymd(paste(wet_Lola_monthly_2015a$Year, wet_Lola_monthly_2015a$Month, wet_Lola_monthly_2015a$day, sep="-"))
wet_Lola_monthly_2015a <- select(wet_Lola_monthly_2015a, date, Year, Month, day, Location)
wet_Lola_monthly_2015 <- full_join(wet_Lola_monthly_2015a, wet_Lola_monthly_2015)
wet_Lola_monthly <- rbind(select(wet_Lola_monthly,date, Year, Month, day, Location, wet), wet_Lola_monthly_2015)
rm(wet_Lola_monthly_2015, wet_Lola_monthly_2015a)
wet_Lola_monthly$measurement <- "wet"
wet_Lola_monthly <- rename(wet_Lola_monthly, Value=wet)
#Macenta
wet_Macenta_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           wet1=wet.var[which(lon==9.75),which(lat==8.75),1:48],
                                           wet2=wet.var[which(lon==9.25),which(lat==8.75),1:48],
                                           wet3=wet.var[which(lon==9.25),which(lat==8.25),1:48]))
wet_Macenta_monthly$wet <- rowMeans(select(wet_Macenta_monthly, wet1, wet2, wet3))
wet_Macenta_monthly$Location <- 'Macenta'
wet_Macenta_monthly$date <- ymd(paste(wet_Macenta_monthly$Year, wet_Macenta_monthly$Month, wet_Macenta_monthly$day, sep="-"))
wet_Macenta_monthly_2015 <- select(wet_Macenta_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Macenta_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Macenta_monthly_2015a$Location <- 'Macenta'
wet_Macenta_monthly_2015a$date <- ymd(paste(wet_Macenta_monthly_2015a$Year, wet_Macenta_monthly_2015a$Month, wet_Macenta_monthly_2015a$day, sep="-"))
wet_Macenta_monthly_2015a <- select(wet_Macenta_monthly_2015a, date, Year, Month, day, Location)
wet_Macenta_monthly_2015 <- full_join(wet_Macenta_monthly_2015a, wet_Macenta_monthly_2015)
wet_Macenta_monthly <- rbind(select(wet_Macenta_monthly,date, Year, Month, day, Location, wet), wet_Macenta_monthly_2015)
rm(wet_Macenta_monthly_2015, wet_Macenta_monthly_2015a)
wet_Macenta_monthly$measurement <- "wet"
wet_Macenta_monthly <- rename(wet_Macenta_monthly, Value=wet)
#Mali
wet_Mali_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        wet1=wet.var[which(lon==12.75),which(lat==12.25),1:48],
                                        wet2=wet.var[which(lon==12.25),which(lat==12.25),1:48], 
                                        wet3=wet.var[which(lon==11.75),which(lat==12.25),1:48],
                                        wet4=wet.var[which(lon==12.75),which(lat==11.75),1:48],
                                        wet5=wet.var[which(lon==12.25),which(lat==11.75),1:48]))
wet_Mali_monthly$wet <- rowMeans(select(wet_Mali_monthly, wet1, wet2, wet3, wet4, wet5))
wet_Mali_monthly$Location <- 'Mali'
wet_Mali_monthly$date <- ymd(paste(wet_Mali_monthly$Year, wet_Mali_monthly$Month, wet_Mali_monthly$day, sep="-"))
wet_Mali_monthly_2015 <- select(wet_Mali_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Mali_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Mali_monthly_2015a$Location <- 'Mali'
wet_Mali_monthly_2015a$date <- ymd(paste(wet_Mali_monthly_2015a$Year, wet_Mali_monthly_2015a$Month, wet_Mali_monthly_2015a$day, sep="-"))
wet_Mali_monthly_2015a <- select(wet_Mali_monthly_2015a, date, Year, Month, day, Location)
wet_Mali_monthly_2015 <- full_join(wet_Mali_monthly_2015a, wet_Mali_monthly_2015)
wet_Mali_monthly <- rbind(select(wet_Mali_monthly,date, Year, Month, day, Location, wet), wet_Mali_monthly_2015)
rm(wet_Mali_monthly_2015, wet_Mali_monthly_2015a)
wet_Mali_monthly$measurement <- "wet"
wet_Mali_monthly <- rename(wet_Mali_monthly, Value=wet)
#Mamou
wet_Mamou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         wet1=wet.var[which(lon==11.75),which(lat==10.75),1:48],
                                         wet2=wet.var[which(lon==12.25),which(lat==10.25),1:48],
                                         wet3=wet.var[which(lon==11.75),which(lat==10.25),1:48]))
wet_Mamou_monthly$wet <- rowMeans(select(wet_Mamou_monthly, wet1, wet2, wet3))
wet_Mamou_monthly$Location <- 'Mamou'
wet_Mamou_monthly$date <- ymd(paste(wet_Mamou_monthly$Year, wet_Mamou_monthly$Month, wet_Mamou_monthly$day, sep="-"))
wet_Mamou_monthly_2015 <- select(wet_Mamou_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Mamou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Mamou_monthly_2015a$Location <- 'Mamou'
wet_Mamou_monthly_2015a$date <- ymd(paste(wet_Mamou_monthly_2015a$Year, wet_Mamou_monthly_2015a$Month, wet_Mamou_monthly_2015a$day, sep="-"))
wet_Mamou_monthly_2015a <- select(wet_Mamou_monthly_2015a, date, Year, Month, day, Location)
wet_Mamou_monthly_2015 <- full_join(wet_Mamou_monthly_2015a, wet_Mamou_monthly_2015)
wet_Mamou_monthly <- rbind(select(wet_Mamou_monthly,date, Year, Month, day, Location, wet), wet_Mamou_monthly_2015)
rm(wet_Mamou_monthly_2015, wet_Mamou_monthly_2015a)
wet_Mamou_monthly$measurement <- "wet"
wet_Mamou_monthly <- rename(wet_Mamou_monthly, Value=wet)
#Nzerekore
wet_Nzerekore_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             wet1=wet.var[which(lon==8.75),which(lat==8.25),1:48],
                                             wet2=wet.var[which(lon==8.75),which(lat==7.75),1:48]))
wet_Nzerekore_monthly$wet <- rowMeans(select(wet_Nzerekore_monthly, wet1, wet2))
wet_Nzerekore_monthly$Location <- 'Nzerekore'
wet_Nzerekore_monthly$date <- ymd(paste(wet_Nzerekore_monthly$Year, wet_Nzerekore_monthly$Month, wet_Nzerekore_monthly$day, sep="-"))
wet_Nzerekore_monthly_2015 <- select(wet_Nzerekore_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Nzerekore_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Nzerekore_monthly_2015a$Location <- 'Nzerekore'
wet_Nzerekore_monthly_2015a$date <- ymd(paste(wet_Nzerekore_monthly_2015a$Year, wet_Nzerekore_monthly_2015a$Month, wet_Nzerekore_monthly_2015a$day, sep="-"))
wet_Nzerekore_monthly_2015a <- select(wet_Nzerekore_monthly_2015a, date, Year, Month, day, Location)
wet_Nzerekore_monthly_2015 <- full_join(wet_Nzerekore_monthly_2015a, wet_Nzerekore_monthly_2015)
wet_Nzerekore_monthly <- rbind(select(wet_Nzerekore_monthly,date, Year, Month, day, Location, wet), wet_Nzerekore_monthly_2015)
rm(wet_Nzerekore_monthly_2015, wet_Nzerekore_monthly_2015a)
wet_Nzerekore_monthly$measurement <- "wet"
wet_Nzerekore_monthly <- rename(wet_Nzerekore_monthly, Value=wet)
#Pita
wet_Pita_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        wet1=wet.var[which(lon==12.75),which(lat==11.25),1:48],
                                        wet2=wet.var[which(lon==12.25),which(lat==11.25),1:48],
                                        wet3=wet.var[which(lon==12.75),which(lat==10.75),1:48]))
wet_Pita_monthly$wet <- rowMeans(select(wet_Pita_monthly, wet1, wet2, wet3))
wet_Pita_monthly$Location <- 'Pita'
wet_Pita_monthly$date <- ymd(paste(wet_Pita_monthly$Year, wet_Pita_monthly$Month, wet_Pita_monthly$day, sep="-"))
wet_Pita_monthly_2015 <- select(wet_Pita_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Pita_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Pita_monthly_2015a$Location <- 'Pita'
wet_Pita_monthly_2015a$date <- ymd(paste(wet_Pita_monthly_2015a$Year, wet_Pita_monthly_2015a$Month, wet_Pita_monthly_2015a$day, sep="-"))
wet_Pita_monthly_2015a <- select(wet_Pita_monthly_2015a, date, Year, Month, day, Location)
wet_Pita_monthly_2015 <- full_join(wet_Pita_monthly_2015a, wet_Pita_monthly_2015)
wet_Pita_monthly <- rbind(select(wet_Pita_monthly,date, Year, Month, day, Location, wet), wet_Pita_monthly_2015)
rm(wet_Pita_monthly_2015, wet_Pita_monthly_2015a)
wet_Pita_monthly$measurement <- "wet"
wet_Pita_monthly <- rename(wet_Pita_monthly, Value=wet)
#Siguiri
wet_Siguiri_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           wet1=wet.var[which(lon==9.25),which(lat==12.25),1:48],
                                           wet2=wet.var[which(lon==10.25),which(lat==11.75),1:48], 
                                           wet3=wet.var[which(lon==9.75),which(lat==11.75),1:48],
                                           wet4=wet.var[which(lon==9.25),which(lat==11.75),1:48],
                                           wet5=wet.var[which(lon==9.75),which(lat==11.25),1:48],
                                           wet6=wet.var[which(lon==9.25),which(lat==11.25),1:48]))
wet_Siguiri_monthly$wet <- rowMeans(select(wet_Siguiri_monthly, wet1, wet2, wet3, wet4, wet5, wet6))
wet_Siguiri_monthly$Location <- 'Siguiri'
wet_Siguiri_monthly$date <- ymd(paste(wet_Siguiri_monthly$Year, wet_Siguiri_monthly$Month, wet_Siguiri_monthly$day, sep="-"))
wet_Siguiri_monthly_2015 <- select(wet_Siguiri_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Siguiri_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Siguiri_monthly_2015a$Location <- 'Siguiri'
wet_Siguiri_monthly_2015a$date <- ymd(paste(wet_Siguiri_monthly_2015a$Year, wet_Siguiri_monthly_2015a$Month, wet_Siguiri_monthly_2015a$day, sep="-"))
wet_Siguiri_monthly_2015a <- select(wet_Siguiri_monthly_2015a, date, Year, Month, day, Location)
wet_Siguiri_monthly_2015 <- full_join(wet_Siguiri_monthly_2015a, wet_Siguiri_monthly_2015)
wet_Siguiri_monthly <- rbind(select(wet_Siguiri_monthly,date, Year, Month, day, Location, wet), wet_Siguiri_monthly_2015)
rm(wet_Siguiri_monthly_2015, wet_Siguiri_monthly_2015a)
wet_Siguiri_monthly$measurement <- "wet"
wet_Siguiri_monthly <- rename(wet_Siguiri_monthly, Value=wet)
#Telimele
wet_Telimele_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            wet1=wet.var[which(lon==13.75),which(lat==11.25),1:48],
                                            wet2=wet.var[which(lon==13.25),which(lat==11.25),1:48], 
                                            wet3=wet.var[which(lon==13.75),which(lat==10.75),1:48],
                                            wet4=wet.var[which(lon==13.25),which(lat==10.75),1:48]))
wet_Telimele_monthly$wet <- rowMeans(select(wet_Telimele_monthly, wet1, wet2, wet3, wet4))
wet_Telimele_monthly$Location <- 'Telimele'
wet_Telimele_monthly$date <- ymd(paste(wet_Telimele_monthly$Year, wet_Telimele_monthly$Month, wet_Telimele_monthly$day, sep="-"))
wet_Telimele_monthly_2015 <- select(wet_Telimele_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Telimele_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Telimele_monthly_2015a$Location <- 'Telimele'
wet_Telimele_monthly_2015a$date <- ymd(paste(wet_Telimele_monthly_2015a$Year, wet_Telimele_monthly_2015a$Month, wet_Telimele_monthly_2015a$day, sep="-"))
wet_Telimele_monthly_2015a <- select(wet_Telimele_monthly_2015a, date, Year, Month, day, Location)
wet_Telimele_monthly_2015 <- full_join(wet_Telimele_monthly_2015a, wet_Telimele_monthly_2015)
wet_Telimele_monthly <- rbind(select(wet_Telimele_monthly,date, Year, Month, day, Location, wet), wet_Telimele_monthly_2015)
rm(wet_Telimele_monthly_2015, wet_Telimele_monthly_2015a)
wet_Telimele_monthly$measurement <- "wet"
wet_Telimele_monthly <- rename(wet_Telimele_monthly, Value=wet)
#Tougue
wet_Tougue_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4),day=1, 
                                          wet1=wet.var[which(lon==11.25),which(lat==11.75),1:48],
                                          wet2=wet.var[which(lon==11.75),which(lat==11.25),1:48]))
wet_Tougue_monthly$wet <- rowMeans(select(wet_Tougue_monthly, wet1, wet2))
wet_Tougue_monthly$Location <- 'Tougue'
wet_Tougue_monthly$date <- ymd(paste(wet_Tougue_monthly$Year, wet_Tougue_monthly$Month, wet_Tougue_monthly$day, sep="-"))
wet_Tougue_monthly_2015 <- select(wet_Tougue_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_Tougue_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_Tougue_monthly_2015a$Location <- 'Tougue'
wet_Tougue_monthly_2015a$date <- ymd(paste(wet_Tougue_monthly_2015a$Year, wet_Tougue_monthly_2015a$Month, wet_Tougue_monthly_2015a$day, sep="-"))
wet_Tougue_monthly_2015a <- select(wet_Tougue_monthly_2015a, date, Year, Month, day, Location)
wet_Tougue_monthly_2015 <- full_join(wet_Tougue_monthly_2015a, wet_Tougue_monthly_2015)
wet_Tougue_monthly <- rbind(select(wet_Tougue_monthly,date, Year, Month, day, Location, wet), wet_Tougue_monthly_2015)
rm(wet_Tougue_monthly_2015, wet_Tougue_monthly_2015a)
wet_Tougue_monthly$measurement <- "wet"
wet_Tougue_monthly <- rename(wet_Tougue_monthly, Value=wet)
#yamou
wet_yamou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4),day=1, 
                                         wet1=wet.var[which(lon==9.25),which(lat==7.75),1:48],
                                         wet2=wet.var[which(lon==9.25),which(lat==7.25),1:48],
                                         wet3=wet.var[which(lon==8.75),which(lat==7.25),1:48]))
wet_yamou_monthly$Location <- 'yamou'
wet_yamou_monthly$wet <- rowMeans(select(wet_yamou_monthly, wet1, wet2, wet3))
wet_yamou_monthly$date <- ymd(paste(wet_yamou_monthly$Year, wet_yamou_monthly$Month, wet_yamou_monthly$day, sep="-"))
wet_yamou_monthly_2015 <- select(wet_yamou_monthly, Location, Year, Month, wet) %>% group_by( Month) %>% summarize(wet=mean(wet))
wet_yamou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
wet_yamou_monthly_2015a$Location <- 'yamou'
wet_yamou_monthly_2015a$date <- ymd(paste(wet_yamou_monthly_2015a$Year, wet_yamou_monthly_2015a$Month, wet_yamou_monthly_2015a$day, sep="-"))
wet_yamou_monthly_2015a <- select(wet_yamou_monthly_2015a, date, Year, Month, day, Location)
wet_yamou_monthly_2015 <- full_join(wet_yamou_monthly_2015a, wet_yamou_monthly_2015)
wet_yamou_monthly <- rbind(select(wet_yamou_monthly,date, Year, Month, day, Location, wet), wet_yamou_monthly_2015)
rm(wet_yamou_monthly_2015, wet_yamou_monthly_2015a)
wet_yamou_monthly$measurement <- "wet"
wet_yamou_monthly <- rename(wet_yamou_monthly, Value=wet)

#Merging in long format
wet_Guinea_monthly_district <- rbind(wet_Beyla_monthly, wet_Boke_monthly, wet_Boffa_monthly,
                                     wet_Conakry_monthly, wet_Coyah_monthly, wet_Dabola_monthly, wet_Dalaba_monthly,
                                     wet_Dinguiray_monthly, wet_Dubreka_monthly, wet_Faranah_monthly,
                                     wet_Forecariah_monthly, wet_Fria_monthly, wet_Gaoual_monthly,
                                     wet_Gueckedou_monthly, wet_Kankan_monthly, wet_Kerouane_monthly,
                                     wet_Kindia_monthly, wet_Kissidougou_monthly, wet_Koubia_monthly,
                                     wet_Koundara_monthly, wet_Kouroussa_monthly, wet_Labe_monthly,
                                     wet_Lelouma_monthly, wet_Lola_monthly, wet_Macenta_monthly,
                                     wet_Mali_monthly, wet_Mamou_monthly, wet_Nzerekore_monthly,
                                     wet_Pita_monthly, wet_Siguiri_monthly, wet_Telimele_monthly,
                                     wet_Tougue_monthly, wet_yamou_monthly)

#####################
#dtr - daily temp range
#####################
dtr.full <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.dtr.dat.nc', write=FALSE)
dtr.var <- var.get.nc(dtr.full, "dtr")
lon <- var.get.nc(dtr.full, "lon")
lat <- var.get.nc(dtr.full, "lat")
#Beyla
dtr_Beyla_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         dtr1=dtr.var[which(lon==8.75),which(lat==9.25),1:48],
                                         dtr2=dtr.var[which(lon==8.25),which(lat==9.25),1:48], 
                                         dtr3=dtr.var[which(lon==7.75),which(lat==9.25),1:48],
                                         dtr4=dtr.var[which(lon==8.75),which(lat==8.75),1:48], 
                                         dtr5=dtr.var[which(lon==8.25),which(lat==8.75),1:48],
                                         dtr6=dtr.var[which(lon==7.75),which(lat==8.75),1:48], 
                                         dtr7=dtr.var[which(lon==8.75),which(lat==8.25),1:48],
                                         dtr8=dtr.var[which(lon==8.25),which(lat==8.25),1:48]))
dtr_Beyla_monthly$dtr <- rowMeans(select(dtr_Beyla_monthly, dtr1, dtr2, dtr3, dtr4, dtr5, dtr6, dtr7, dtr8))
dtr_Beyla_monthly$Location <- 'Beyla'
dtr_Beyla_monthly$date <- ymd(paste(dtr_Beyla_monthly$Year, dtr_Beyla_monthly$Month, dtr_Beyla_monthly$day, sep="-"))
dtr_Beyla_monthly_2015 <- select(dtr_Beyla_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Beyla_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Beyla_monthly_2015a$Location <- 'Beyla'
dtr_Beyla_monthly_2015a$date <- ymd(paste(dtr_Beyla_monthly_2015a$Year, dtr_Beyla_monthly_2015a$Month, dtr_Beyla_monthly_2015a$day, sep="-"))
dtr_Beyla_monthly_2015a <- select(dtr_Beyla_monthly_2015a, date, Year, Month, day, Location)
dtr_Beyla_monthly_2015 <- full_join(dtr_Beyla_monthly_2015a, dtr_Beyla_monthly_2015)
dtr_Beyla_monthly <- rbind(select(dtr_Beyla_monthly,date, Year, Month, day, Location, dtr), dtr_Beyla_monthly_2015)
rm(dtr_Beyla_monthly_2015, dtr_Beyla_monthly_2015a)
dtr_Beyla_monthly$measurement <- "dtr"
dtr_Beyla_monthly <- rename(dtr_Beyla_monthly, Value=dtr)
#Boffa
dtr_Boffa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         dtr1=dtr.var[which(lon==14.25),which(lat==10.75),1:48],
                                         dtr2=dtr.var[which(lon==13.75),which(lat==10.75),1:48], 
                                         dtr3=dtr.var[which(lon==14.25),which(lat==10.25),1:48],
                                         dtr4=dtr.var[which(lon==13.75),which(lat==10.25),1:48]))
dtr_Boffa_monthly$dtr <- rowMeans(select(dtr_Boffa_monthly, dtr1, dtr2, dtr3, dtr4))
dtr_Boffa_monthly$Location <- 'Boffa'
dtr_Boffa_monthly$date <- ymd(paste(dtr_Boffa_monthly$Year, dtr_Boffa_monthly$Month, dtr_Boffa_monthly$day, sep="-"))
dtr_Boffa_monthly_2015 <- select(dtr_Boffa_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Boffa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Boffa_monthly_2015a$Location <- 'Boffa'
dtr_Boffa_monthly_2015a$date <- ymd(paste(dtr_Boffa_monthly_2015a$Year, dtr_Boffa_monthly_2015a$Month, dtr_Boffa_monthly_2015a$day, sep="-"))
dtr_Boffa_monthly_2015a <- select(dtr_Boffa_monthly_2015a, date, Year, Month, day, Location)
dtr_Boffa_monthly_2015 <- full_join(dtr_Boffa_monthly_2015a, dtr_Boffa_monthly_2015)
dtr_Boffa_monthly <- rbind(select(dtr_Boffa_monthly,date, Year, Month, day, Location, dtr), dtr_Boffa_monthly_2015)
rm(dtr_Boffa_monthly_2015, dtr_Boffa_monthly_2015a)
dtr_Boffa_monthly$measurement <- "dtr"
dtr_Boffa_monthly <- rename(dtr_Boffa_monthly, Value=dtr)
#Boke
dtr_Boke_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        dtr1=dtr.var[which(lon==14.25),which(lat==11.75),1:48],
                                        dtr2=dtr.var[which(lon==14.75),which(lat==11.25),1:48], 
                                        dtr3=dtr.var[which(lon==14.25),which(lat==11.25),1:48],
                                        dtr4=dtr.var[which(lon==13.75),which(lat==11.25),1:48]))
dtr_Boke_monthly$dtr <- rowMeans(select(dtr_Boke_monthly, dtr1, dtr2, dtr3, dtr4))
dtr_Boke_monthly$Location <- 'Boke'
dtr_Boke_monthly$date <- ymd(paste(dtr_Boke_monthly$Year, dtr_Boke_monthly$Month, dtr_Boke_monthly$day, sep="-"))
dtr_Boke_monthly_2015 <- select(dtr_Boke_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Boke_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Boke_monthly_2015a$Location <- 'Boke'
dtr_Boke_monthly_2015a$date <- ymd(paste(dtr_Boke_monthly_2015a$Year, dtr_Boke_monthly_2015a$Month, dtr_Boke_monthly_2015a$day, sep="-"))
dtr_Boke_monthly_2015a <- select(dtr_Boke_monthly_2015a, date, Year, Month, day, Location)
dtr_Boke_monthly_2015 <- full_join(dtr_Boke_monthly_2015a, dtr_Boke_monthly_2015)
dtr_Boke_monthly <- rbind(select(dtr_Boke_monthly,date, Year, Month, day, Location, dtr), dtr_Boke_monthly_2015)
rm(dtr_Boke_monthly_2015, dtr_Boke_monthly_2015a)
dtr_Boke_monthly$measurement <- "dtr"
dtr_Boke_monthly <- rename(dtr_Boke_monthly, Value=dtr)
#Conakry
dtr_Conakry_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           dtr=dtr.var[which(lon==13.75),which(lat==9.75),1:48]))
dtr_Conakry_monthly$Location <- 'Conakry'
dtr_Conakry_monthly$date <- ymd(paste(dtr_Conakry_monthly$Year, dtr_Conakry_monthly$Month, dtr_Conakry_monthly$day, sep="-"))
dtr_Conakry_monthly_2015 <- select(dtr_Conakry_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Conakry_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Conakry_monthly_2015a$Location <- 'Conakry'
dtr_Conakry_monthly_2015a$date <- ymd(paste(dtr_Conakry_monthly_2015a$Year, dtr_Conakry_monthly_2015a$Month, dtr_Conakry_monthly_2015a$day, sep="-"))
dtr_Conakry_monthly_2015a <- select(dtr_Conakry_monthly_2015a, date, Year, Month, day, Location)
dtr_Conakry_monthly_2015 <- full_join(dtr_Conakry_monthly_2015a, dtr_Conakry_monthly_2015)
dtr_Conakry_monthly <- rbind(select(dtr_Conakry_monthly,date, Year, Month, day, Location, dtr), dtr_Conakry_monthly_2015)
rm(dtr_Conakry_monthly_2015, dtr_Conakry_monthly_2015a)
dtr_Conakry_monthly$measurement <- "dtr"
dtr_Conakry_monthly <- rename(dtr_Conakry_monthly, Value=dtr)

#Coyah
dtr_Coyah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         dtr=dtr.var[which(lon==9.75),which(lat==13.25),1:48]))
dtr_Coyah_monthly$Location <- 'Coyah'
dtr_Coyah_monthly$date <- ymd(paste(dtr_Coyah_monthly$Year, dtr_Coyah_monthly$Month, dtr_Coyah_monthly$day, sep="-"))
dtr_Coyah_monthly_2015 <- select(dtr_Coyah_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Coyah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Coyah_monthly_2015a$Location <- 'Coyah'
dtr_Coyah_monthly_2015a$date <- ymd(paste(dtr_Coyah_monthly_2015a$Year, dtr_Coyah_monthly_2015a$Month, dtr_Coyah_monthly_2015a$day, sep="-"))
dtr_Coyah_monthly_2015a <- select(dtr_Coyah_monthly_2015a, date, Year, Month, day, Location)
dtr_Coyah_monthly_2015 <- full_join(dtr_Coyah_monthly_2015a, dtr_Coyah_monthly_2015)
dtr_Coyah_monthly <- rbind(select(dtr_Coyah_monthly,date, Year, Month, day, Location, dtr), dtr_Coyah_monthly_2015)
rm(dtr_Coyah_monthly_2015, dtr_Coyah_monthly_2015a)
dtr_Coyah_monthly$measurement <- "dtr"
dtr_Coyah_monthly <- rename(dtr_Coyah_monthly, Value=dtr)
#Dabola
dtr_Dabola_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          dtr1=dtr.var[which(lon==11.25),which(lat==10.75),1:48],
                                          dtr2=dtr.var[which(lon==10.75),which(lat==10.75),1:48]))
dtr_Dabola_monthly$dtr <- rowMeans(select(dtr_Dabola_monthly, dtr1, dtr2))
dtr_Dabola_monthly$Location <- 'Dabola'
dtr_Dabola_monthly$date <- ymd(paste(dtr_Dabola_monthly$Year, dtr_Dabola_monthly$Month, dtr_Dabola_monthly$day, sep="-"))
dtr_Dabola_monthly_2015 <- select(dtr_Dabola_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Dabola_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Dabola_monthly_2015a$Location <- 'Dabola'
dtr_Dabola_monthly_2015a$date <- ymd(paste(dtr_Dabola_monthly_2015a$Year, dtr_Dabola_monthly_2015a$Month, dtr_Dabola_monthly_2015a$day, sep="-"))
dtr_Dabola_monthly_2015a <- select(dtr_Dabola_monthly_2015a, date, Year, Month, day, Location)
dtr_Dabola_monthly_2015 <- full_join(dtr_Dabola_monthly_2015a, dtr_Dabola_monthly_2015)
dtr_Dabola_monthly <- rbind(select(dtr_Dabola_monthly,date, Year, Month, day, Location, dtr), dtr_Dabola_monthly_2015)
rm(dtr_Dabola_monthly_2015, dtr_Dabola_monthly_2015a)
dtr_Dabola_monthly$measurement <- "dtr"
dtr_Dabola_monthly <- rename(dtr_Dabola_monthly, Value=dtr)
#Dalaba
dtr_Dalaba_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          dtr1=dtr.var[which(lon==11.25),which(lat==12.25),1:48],
                                          dtr2=dtr.var[which(lon==10.75),which(lat==12.25),1:48]))
dtr_Dalaba_monthly$dtr <- rowMeans(select(dtr_Dalaba_monthly, dtr1, dtr2))
dtr_Dalaba_monthly$Location <- 'Dalaba'
dtr_Dalaba_monthly$date <- ymd(paste(dtr_Dalaba_monthly$Year, dtr_Dalaba_monthly$Month, dtr_Dalaba_monthly$day, sep="-"))
dtr_Dalaba_monthly_2015 <- select(dtr_Dalaba_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Dalaba_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Dalaba_monthly_2015a$Location <- 'Dalaba'
dtr_Dalaba_monthly_2015a$date <- ymd(paste(dtr_Dalaba_monthly_2015a$Year, dtr_Dalaba_monthly_2015a$Month, dtr_Dalaba_monthly_2015a$day, sep="-"))
dtr_Dalaba_monthly_2015a <- select(dtr_Dalaba_monthly_2015a, date, Year, Month, day, Location)
dtr_Dalaba_monthly_2015 <- full_join(dtr_Dalaba_monthly_2015a, dtr_Dalaba_monthly_2015)
dtr_Dalaba_monthly <- rbind(select(dtr_Dalaba_monthly,date, Year, Month, day, Location, dtr), dtr_Dalaba_monthly_2015)
rm(dtr_Dalaba_monthly_2015, dtr_Dalaba_monthly_2015a)
dtr_Dalaba_monthly$measurement <- "dtr"
dtr_Dalaba_monthly <- rename(dtr_Dalaba_monthly, Value=dtr)
#Dinguiray
dtr_Dinguiray_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             dtr1=dtr.var[which(lon==11.25),which(lat==11.75),1:48],
                                             dtr2=dtr.var[which(lon==10.75),which(lat==11.75),1:48], 
                                             dtr3=dtr.var[which(lon==10.25),which(lat==11.75),1:48],
                                             dtr4=dtr.var[which(lon==11.25),which(lat==11.25),1:48],
                                             dtr5=dtr.var[which(lon==10.75),which(lat==11.25),1:48],
                                             dtr6=dtr.var[which(lon==10.25),which(lat==11.25),1:48]))
dtr_Dinguiray_monthly$dtr <- rowMeans(select(dtr_Dinguiray_monthly, dtr1, dtr2, dtr3, dtr4, dtr5, dtr6))
dtr_Dinguiray_monthly$Location <- 'Dinguiray'
dtr_Dinguiray_monthly$date <- ymd(paste(dtr_Dinguiray_monthly$Year, dtr_Dinguiray_monthly$Month, dtr_Dinguiray_monthly$day, sep="-"))
dtr_Dinguiray_monthly_2015 <- select(dtr_Dinguiray_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Dinguiray_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Dinguiray_monthly_2015a$Location <- 'Dinguiray'
dtr_Dinguiray_monthly_2015a$date <- ymd(paste(dtr_Dinguiray_monthly_2015a$Year, dtr_Dinguiray_monthly_2015a$Month, dtr_Dinguiray_monthly_2015a$day, sep="-"))
dtr_Dinguiray_monthly_2015a <- select(dtr_Dinguiray_monthly_2015a, date, Year, Month, day, Location)
dtr_Dinguiray_monthly_2015 <- full_join(dtr_Dinguiray_monthly_2015a, dtr_Dinguiray_monthly_2015)
dtr_Dinguiray_monthly <- rbind(select(dtr_Dinguiray_monthly,date, Year, Month, day, Location, dtr), dtr_Dinguiray_monthly_2015)
rm(dtr_Dinguiray_monthly_2015, dtr_Dinguiray_monthly_2015a)
dtr_Dinguiray_monthly$measurement <- "dtr"
dtr_Dinguiray_monthly <- rename(dtr_Dinguiray_monthly, Value=dtr)
#Dubreka
dtr_Dubreka_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           dtr1=dtr.var[which(lon==13.75),which(lat==10.25),1:48],
                                           dtr2=dtr.var[which(lon==13.25),which(lat==10.25),1:48]))
dtr_Dubreka_monthly$dtr <- rowMeans(select(dtr_Dubreka_monthly, dtr1, dtr2))
dtr_Dubreka_monthly$Location <- 'Dubreka'
dtr_Dubreka_monthly$date <- ymd(paste(dtr_Dubreka_monthly$Year, dtr_Dubreka_monthly$Month, dtr_Dubreka_monthly$day, sep="-"))
dtr_Dubreka_monthly_2015 <- select(dtr_Dubreka_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Dubreka_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Dubreka_monthly_2015a$Location <- 'Dubreka'
dtr_Dubreka_monthly_2015a$date <- ymd(paste(dtr_Dubreka_monthly_2015a$Year, dtr_Dubreka_monthly_2015a$Month, dtr_Dubreka_monthly_2015a$day, sep="-"))
dtr_Dubreka_monthly_2015a <- select(dtr_Dubreka_monthly_2015a, date, Year, Month, day, Location)
dtr_Dubreka_monthly_2015 <- full_join(dtr_Dubreka_monthly_2015a, dtr_Dubreka_monthly_2015)
dtr_Dubreka_monthly <- rbind(select(dtr_Dubreka_monthly,date, Year, Month, day, Location, dtr), dtr_Dubreka_monthly_2015)
rm(dtr_Dubreka_monthly_2015, dtr_Dubreka_monthly_2015a)
dtr_Dubreka_monthly$measurement <- "dtr"
dtr_Dubreka_monthly <- rename(dtr_Dubreka_monthly, Value=dtr)
#Faranah
dtr_Faranah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           dtr1=dtr.var[which(lon==11.25),which(lat==10.25),1:48],
                                           dtr2=dtr.var[which(lon==10.75),which(lat==10.25),1:48], 
                                           dtr3=dtr.var[which(lon==10.25),which(lat==10.25),1:48],
                                           dtr4=dtr.var[which(lon==10.75),which(lat==9.75),1:48],
                                           dtr5=dtr.var[which(lon==10.75),which(lat==9.25),1:48]))
dtr_Faranah_monthly$dtr <- rowMeans(select(dtr_Faranah_monthly, dtr1, dtr2, dtr3, dtr4, dtr5))
dtr_Faranah_monthly$Location <- 'Faranah'
dtr_Faranah_monthly$date <- ymd(paste(dtr_Faranah_monthly$Year, dtr_Faranah_monthly$Month, dtr_Faranah_monthly$day, sep="-"))
dtr_Faranah_monthly_2015 <- select(dtr_Faranah_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Faranah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Faranah_monthly_2015a$Location <- 'Faranah'
dtr_Faranah_monthly_2015a$date <- ymd(paste(dtr_Faranah_monthly_2015a$Year, dtr_Faranah_monthly_2015a$Month, dtr_Faranah_monthly_2015a$day, sep="-"))
dtr_Faranah_monthly_2015a <- select(dtr_Faranah_monthly_2015a, date, Year, Month, day, Location)
dtr_Faranah_monthly_2015 <- full_join(dtr_Faranah_monthly_2015a, dtr_Faranah_monthly_2015)
dtr_Faranah_monthly <- rbind(select(dtr_Faranah_monthly,date, Year, Month, day, Location, dtr), dtr_Faranah_monthly_2015)
rm(dtr_Faranah_monthly_2015, dtr_Faranah_monthly_2015a)
dtr_Faranah_monthly$measurement <- "dtr"
dtr_Faranah_monthly <- rename(dtr_Faranah_monthly, Value=dtr)
#Forecariah
dtr_Forecariah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                              Month=rep(seq(1,12,1), 4), day=1,
                                              dtr1=dtr.var[which(lon==12.75),which(lat==9.75),1:48],
                                              dtr2=dtr.var[which(lon==13.25),which(lat==9.25),1:48], 
                                              dtr3=dtr.var[which(lon==12.75),which(lat==9.25),1:48]))
dtr_Forecariah_monthly$dtr <- rowMeans(select(dtr_Forecariah_monthly, dtr1, dtr2, dtr3))
dtr_Forecariah_monthly$Location <- 'Forecariah'
dtr_Forecariah_monthly$date <- ymd(paste(dtr_Forecariah_monthly$Year, dtr_Forecariah_monthly$Month, dtr_Forecariah_monthly$day, sep="-"))
dtr_Forecariah_monthly_2015 <- select(dtr_Forecariah_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Forecariah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Forecariah_monthly_2015a$Location <- 'Forecariah'
dtr_Forecariah_monthly_2015a$date <- ymd(paste(dtr_Forecariah_monthly_2015a$Year, dtr_Forecariah_monthly_2015a$Month, dtr_Forecariah_monthly_2015a$day, sep="-"))
dtr_Forecariah_monthly_2015a <- select(dtr_Forecariah_monthly_2015a, date, Year, Month, day, Location)
dtr_Forecariah_monthly_2015 <- full_join(dtr_Forecariah_monthly_2015a, dtr_Forecariah_monthly_2015)
dtr_Forecariah_monthly <- rbind(select(dtr_Forecariah_monthly,date, Year, Month, day, Location, dtr), dtr_Forecariah_monthly_2015)
rm(dtr_Forecariah_monthly_2015, dtr_Forecariah_monthly_2015a)
dtr_Forecariah_monthly$measurement <- "dtr"
dtr_Forecariah_monthly <- rename(dtr_Forecariah_monthly, Value=dtr)
#Fria
dtr_Fria_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        dtr1=dtr.var[which(lon==13.75),which(lat==10.75),1:48],
                                        dtr2=dtr.var[which(lon==13.75),which(lat==10.25),1:48]))
dtr_Fria_monthly$dtr <- rowMeans(select(dtr_Fria_monthly, dtr1, dtr2))
dtr_Fria_monthly$Location <- 'Fria'
dtr_Fria_monthly$date <- ymd(paste(dtr_Fria_monthly$Year, dtr_Fria_monthly$Month, dtr_Fria_monthly$day, sep="-"))
dtr_Fria_monthly_2015 <- select(dtr_Fria_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Fria_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Fria_monthly_2015a$Location <- 'Fria'
dtr_Fria_monthly_2015a$date <- ymd(paste(dtr_Fria_monthly_2015a$Year, dtr_Fria_monthly_2015a$Month, dtr_Fria_monthly_2015a$day, sep="-"))
dtr_Fria_monthly_2015a <- select(dtr_Fria_monthly_2015a, date, Year, Month, day, Location)
dtr_Fria_monthly_2015 <- full_join(dtr_Fria_monthly_2015a, dtr_Fria_monthly_2015)
dtr_Fria_monthly <- rbind(select(dtr_Fria_monthly,date, Year, Month, day, Location, dtr), dtr_Fria_monthly_2015)
rm(dtr_Fria_monthly_2015, dtr_Fria_monthly_2015a)
dtr_Fria_monthly$measurement <- "dtr"
dtr_Fria_monthly <- rename(dtr_Fria_monthly, Value=dtr)
#Gaoual
dtr_Gaoual_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          dtr1=dtr.var[which(lon==13.75),which(lat==12.25),1:48],
                                          dtr2=dtr.var[which(lon==13.25),which(lat==12.25),1:48], 
                                          dtr3=dtr.var[which(lon==13.75),which(lat==11.75),1:48],
                                          dtr4=dtr.var[which(lon==13.25),which(lat==11.75),1:48],
                                          dtr5=dtr.var[which(lon==12.75),which(lat==11.75),1:48],
                                          dtr6=dtr.var[which(lon==13.75),which(lat==11.25),1:48],
                                          dtr7=dtr.var[which(lon==13.25),which(lat==11.25),1:48]))
dtr_Gaoual_monthly$dtr <- rowMeans(select(dtr_Gaoual_monthly, dtr1, dtr2, dtr3, dtr4, dtr5, dtr6, dtr7))
dtr_Gaoual_monthly$Location <- 'Gaoual'
dtr_Gaoual_monthly$date <- ymd(paste(dtr_Gaoual_monthly$Year, dtr_Gaoual_monthly$Month, dtr_Gaoual_monthly$day, sep="-"))
dtr_Gaoual_monthly_2015 <- select(dtr_Gaoual_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Gaoual_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Gaoual_monthly_2015a$Location <- 'Gaoual'
dtr_Gaoual_monthly_2015a$date <- ymd(paste(dtr_Gaoual_monthly_2015a$Year, dtr_Gaoual_monthly_2015a$Month, dtr_Gaoual_monthly_2015a$day, sep="-"))
dtr_Gaoual_monthly_2015a <- select(dtr_Gaoual_monthly_2015a, date, Year, Month, day, Location)
dtr_Gaoual_monthly_2015 <- full_join(dtr_Gaoual_monthly_2015a, dtr_Gaoual_monthly_2015)
dtr_Gaoual_monthly <- rbind(select(dtr_Gaoual_monthly,date, Year, Month, day, Location, dtr), dtr_Gaoual_monthly_2015)
rm(dtr_Gaoual_monthly_2015, dtr_Gaoual_monthly_2015a)
dtr_Gaoual_monthly$measurement <- "dtr"
dtr_Gaoual_monthly <- rename(dtr_Gaoual_monthly, Value=dtr)
#Gueckedou
dtr_Gueckedou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             dtr=dtr.var[which(lon==10.25),which(lat==8.75),1:48]))
dtr_Gueckedou_monthly$Location <- 'Gueckedou'
dtr_Gueckedou_monthly$date <- ymd(paste(dtr_Gueckedou_monthly$Year, dtr_Gueckedou_monthly$Month, dtr_Gueckedou_monthly$day, sep="-"))
dtr_Gueckedou_monthly_2015 <- select(dtr_Gueckedou_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Gueckedou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Gueckedou_monthly_2015a$Location <- 'Gueckedou'
dtr_Gueckedou_monthly_2015a$date <- ymd(paste(dtr_Gueckedou_monthly_2015a$Year, dtr_Gueckedou_monthly_2015a$Month, dtr_Gueckedou_monthly_2015a$day, sep="-"))
dtr_Gueckedou_monthly_2015a <- select(dtr_Gueckedou_monthly_2015a, date, Year, Month, day, Location)
dtr_Gueckedou_monthly_2015 <- full_join(dtr_Gueckedou_monthly_2015a, dtr_Gueckedou_monthly_2015)
dtr_Gueckedou_monthly <- rbind(select(dtr_Gueckedou_monthly,date, Year, Month, day, Location, dtr), dtr_Gueckedou_monthly_2015)
rm(dtr_Gueckedou_monthly_2015, dtr_Gueckedou_monthly_2015a)
dtr_Gueckedou_monthly$measurement <- "dtr"
dtr_Gueckedou_monthly <- rename(dtr_Gueckedou_monthly, Value=dtr)
#Kankan
dtr_Kankan_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          dtr1=dtr.var[which(lon==9.25),which(lat==10.75),1:48],
                                          dtr2=dtr.var[which(lon==9.75),which(lat==10.25),1:48], 
                                          dtr3=dtr.var[which(lon==9.25),which(lat==10.25),1:48],
                                          dtr4=dtr.var[which(lon==8.75),which(lat==10.25),1:48],
                                          dtr5=dtr.var[which(lon==9.75),which(lat==9.75),1:48],
                                          dtr6=dtr.var[which(lon==9.25),which(lat==9.75),1:48]))
dtr_Kankan_monthly$dtr <- rowMeans(select(dtr_Kankan_monthly, dtr1, dtr2, dtr3, dtr4, dtr5, dtr6))
dtr_Kankan_monthly$Location <- 'Kankan'
dtr_Kankan_monthly$date <- ymd(paste(dtr_Kankan_monthly$Year, dtr_Kankan_monthly$Month, dtr_Kankan_monthly$day, sep="-"))
dtr_Kankan_monthly_2015 <- select(dtr_Kankan_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Kankan_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Kankan_monthly_2015a$Location <- 'Kankan'
dtr_Kankan_monthly_2015a$date <- ymd(paste(dtr_Kankan_monthly_2015a$Year, dtr_Kankan_monthly_2015a$Month, dtr_Kankan_monthly_2015a$day, sep="-"))
dtr_Kankan_monthly_2015a <- select(dtr_Kankan_monthly_2015a, date, Year, Month, day, Location)
dtr_Kankan_monthly_2015 <- full_join(dtr_Kankan_monthly_2015a, dtr_Kankan_monthly_2015)
dtr_Kankan_monthly <- rbind(select(dtr_Kankan_monthly,date, Year, Month, day, Location, dtr), dtr_Kankan_monthly_2015)
rm(dtr_Kankan_monthly_2015, dtr_Kankan_monthly_2015a)
dtr_Kankan_monthly$measurement <- "dtr"
dtr_Kankan_monthly <- rename(dtr_Kankan_monthly, Value=dtr)
#Kerouane
dtr_Kerouane_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            dtr1=dtr.var[which(lon==9.25),which(lat==9.75),1:48],
                                            dtr2=dtr.var[which(lon==8.75),which(lat==9.75),1:48], 
                                            dtr3=dtr.var[which(lon==9.75),which(lat==9.25),1:48],
                                            dtr4=dtr.var[which(lon==9.25),which(lat==9.25),1:48],
                                            dtr5=dtr.var[which(lon==8.75),which(lat==9.25),1:48],
                                            dtr6=dtr.var[which(lon==9.25),which(lat==8.75),1:48]))
dtr_Kerouane_monthly$dtr <- rowMeans(select(dtr_Kerouane_monthly, dtr1, dtr2, dtr3, dtr4, dtr5, dtr6))
dtr_Kerouane_monthly$Location <- 'Kerouane'
dtr_Kerouane_monthly$date <- ymd(paste(dtr_Kerouane_monthly$Year, dtr_Kerouane_monthly$Month, dtr_Kerouane_monthly$day, sep="-"))
dtr_Kerouane_monthly_2015 <- select(dtr_Kerouane_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Kerouane_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Kerouane_monthly_2015a$Location <- 'Kerouane'
dtr_Kerouane_monthly_2015a$date <- ymd(paste(dtr_Kerouane_monthly_2015a$Year, dtr_Kerouane_monthly_2015a$Month, dtr_Kerouane_monthly_2015a$day, sep="-"))
dtr_Kerouane_monthly_2015a <- select(dtr_Kerouane_monthly_2015a, date, Year, Month, day, Location)
dtr_Kerouane_monthly_2015 <- full_join(dtr_Kerouane_monthly_2015a, dtr_Kerouane_monthly_2015)
dtr_Kerouane_monthly <- rbind(select(dtr_Kerouane_monthly,date, Year, Month, day, Location, dtr), dtr_Kerouane_monthly_2015)
rm(dtr_Kerouane_monthly_2015, dtr_Kerouane_monthly_2015a)
dtr_Kerouane_monthly$measurement <- "dtr"
dtr_Kerouane_monthly <- rename(dtr_Kerouane_monthly, Value=dtr)
#Kindia
dtr_Kindia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          dtr1=dtr.var[which(lon==13.25),which(lat==10.25),1:48],
                                          dtr2=dtr.var[which(lon==12.75),which(lat==10.25),1:48], 
                                          dtr3=dtr.var[which(lon==12.25),which(lat==10.25),1:48],
                                          dtr4=dtr.var[which(lon==12.75),which(lat==9.75),1:48]))
dtr_Kindia_monthly$dtr <- rowMeans(select(dtr_Kindia_monthly, dtr1, dtr2, dtr3, dtr4))
dtr_Kindia_monthly$Location <- 'Kindia'
dtr_Kindia_monthly$date <- ymd(paste(dtr_Kindia_monthly$Year, dtr_Kindia_monthly$Month, dtr_Kindia_monthly$day, sep="-"))
dtr_Kindia_monthly_2015 <- select(dtr_Kindia_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Kindia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Kindia_monthly_2015a$Location <- 'Kindia'
dtr_Kindia_monthly_2015a$date <- ymd(paste(dtr_Kindia_monthly_2015a$Year, dtr_Kindia_monthly_2015a$Month, dtr_Kindia_monthly_2015a$day, sep="-"))
dtr_Kindia_monthly_2015a <- select(dtr_Kindia_monthly_2015a, date, Year, Month, day, Location)
dtr_Kindia_monthly_2015 <- full_join(dtr_Kindia_monthly_2015a, dtr_Kindia_monthly_2015)
dtr_Kindia_monthly <- rbind(select(dtr_Kindia_monthly,date, Year, Month, day, Location, dtr), dtr_Kindia_monthly_2015)
rm(dtr_Kindia_monthly_2015, dtr_Kindia_monthly_2015a)
dtr_Kindia_monthly$measurement <- "dtr"
dtr_Kindia_monthly <- rename(dtr_Kindia_monthly, Value=dtr)
#KISSIDOUGOU
dtr_Kissidougou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               dtr1=dtr.var[which(lon==10.25),which(lat==9.75),1:48],
                                               dtr2=dtr.var[which(lon==10.25),which(lat==9.25),1:48], 
                                               dtr3=dtr.var[which(lon==9.75),which(lat==9.25),1:48]))
dtr_Kissidougou_monthly$dtr <- rowMeans(select(dtr_Kissidougou_monthly, dtr1, dtr2, dtr3))
dtr_Kissidougou_monthly$Location <- 'Kissidougou'
dtr_Kissidougou_monthly$date <- ymd(paste(dtr_Kissidougou_monthly$Year, dtr_Kissidougou_monthly$Month, dtr_Kissidougou_monthly$day, sep="-"))
dtr_Kissidougou_monthly_2015 <- select(dtr_Kissidougou_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Kissidougou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Kissidougou_monthly_2015a$Location <- 'Kissidougou'
dtr_Kissidougou_monthly_2015a$date <- ymd(paste(dtr_Kissidougou_monthly_2015a$Year, dtr_Kissidougou_monthly_2015a$Month, dtr_Kissidougou_monthly_2015a$day, sep="-"))
dtr_Kissidougou_monthly_2015a <- select(dtr_Kissidougou_monthly_2015a, date, Year, Month, day, Location)
dtr_Kissidougou_monthly_2015 <- full_join(dtr_Kissidougou_monthly_2015a, dtr_Kissidougou_monthly_2015)
dtr_Kissidougou_monthly <- rbind(select(dtr_Kissidougou_monthly,date, Year, Month, day, Location, dtr), dtr_Kissidougou_monthly_2015)
rm(dtr_Kissidougou_monthly_2015, dtr_Kissidougou_monthly_2015a)
dtr_Kissidougou_monthly$measurement <- "dtr"
dtr_Kissidougou_monthly <- rename(dtr_Kissidougou_monthly, Value=dtr)
#Koubia
dtr_Koubia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          dtr=dtr.var[which(lon==10.25),which(lat==9.75),1:48]))
dtr_Koubia_monthly$Location <- 'Koubia'
dtr_Koubia_monthly$date <- ymd(paste(dtr_Koubia_monthly$Year, dtr_Koubia_monthly$Month, dtr_Koubia_monthly$day, sep="-"))
dtr_Koubia_monthly_2015 <- select(dtr_Koubia_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Koubia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Koubia_monthly_2015a$Location <- 'Koubia'
dtr_Koubia_monthly_2015a$date <- ymd(paste(dtr_Koubia_monthly_2015a$Year, dtr_Koubia_monthly_2015a$Month, dtr_Koubia_monthly_2015a$day, sep="-"))
dtr_Koubia_monthly_2015a <- select(dtr_Koubia_monthly_2015a, date, Year, Month, day, Location)
dtr_Koubia_monthly_2015 <- full_join(dtr_Koubia_monthly_2015a, dtr_Koubia_monthly_2015)
dtr_Koubia_monthly <- rbind(select(dtr_Koubia_monthly,date, Year, Month, day, Location, dtr), dtr_Koubia_monthly_2015)
rm(dtr_Koubia_monthly_2015, dtr_Koubia_monthly_2015a)
dtr_Koubia_monthly$measurement <- "dtr"
dtr_Koubia_monthly <- rename(dtr_Koubia_monthly, Value=dtr)
#Koundara
dtr_Koundara_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            dtr1=dtr.var[which(lon==13.25),which(lat==12.75),1:48],
                                            dtr2=dtr.var[which(lon==13.25),which(lat==12.25),1:48], 
                                            dtr3=dtr.var[which(lon==12.75),which(lat==12.25),1:48]))
dtr_Koundara_monthly$dtr <- rowMeans(select(dtr_Koundara_monthly, dtr1, dtr2, dtr3))
dtr_Koundara_monthly$Location <- 'Koundara'
dtr_Koundara_monthly$date <- ymd(paste(dtr_Koundara_monthly$Year, dtr_Koundara_monthly$Month, dtr_Koundara_monthly$day, sep="-"))
dtr_Koundara_monthly_2015 <- select(dtr_Koundara_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Koundara_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Koundara_monthly_2015a$Location <- 'Koundara'
dtr_Koundara_monthly_2015a$date <- ymd(paste(dtr_Koundara_monthly_2015a$Year, dtr_Koundara_monthly_2015a$Month, dtr_Koundara_monthly_2015a$day, sep="-"))
dtr_Koundara_monthly_2015a <- select(dtr_Koundara_monthly_2015a, date, Year, Month, day, Location)
dtr_Koundara_monthly_2015 <- full_join(dtr_Koundara_monthly_2015a, dtr_Koundara_monthly_2015)
dtr_Koundara_monthly <- rbind(select(dtr_Koundara_monthly,date, Year, Month, day, Location, dtr), dtr_Koundara_monthly_2015)
rm(dtr_Koundara_monthly_2015, dtr_Koundara_monthly_2015a)
dtr_Koundara_monthly$measurement <- "dtr"
dtr_Koundara_monthly <- rename(dtr_Koundara_monthly, Value=dtr)
#Kouroussa
dtr_Kouroussa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             dtr1=dtr.var[which(lon==10.25),which(lat==11.25),1:48],
                                             dtr2=dtr.var[which(lon==10.75),which(lat==10.75),1:48], 
                                             dtr3=dtr.var[which(lon==10.25),which(lat==10.75),1:48],
                                             dtr4=dtr.var[which(lon==9.75),which(lat==10.75),1:48],
                                             dtr5=dtr.var[which(lon==10.25),which(lat==10.25),1:48],
                                             dtr6=dtr.var[which(lon==9.75),which(lat==10.25),1:48],
                                             dtr7=dtr.var[which(lon==10.25),which(lat==9.75),1:48],
                                             dtr8=dtr.var[which(lon==9.75),which(lat==9.75),1:48]))
dtr_Kouroussa_monthly$dtr <- rowMeans(select(dtr_Kouroussa_monthly, dtr1, dtr2, dtr3, dtr4, dtr5, dtr6, dtr7, dtr8))
dtr_Kouroussa_monthly$Location <- 'Kouroussa'
dtr_Kouroussa_monthly$date <- ymd(paste(dtr_Kouroussa_monthly$Year, dtr_Kouroussa_monthly$Month, dtr_Kouroussa_monthly$day, sep="-"))
dtr_Kouroussa_monthly_2015 <- select(dtr_Kouroussa_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Kouroussa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Kouroussa_monthly_2015a$Location <- 'Kouroussa'
dtr_Kouroussa_monthly_2015a$date <- ymd(paste(dtr_Kouroussa_monthly_2015a$Year, dtr_Kouroussa_monthly_2015a$Month, dtr_Kouroussa_monthly_2015a$day, sep="-"))
dtr_Kouroussa_monthly_2015a <- select(dtr_Kouroussa_monthly_2015a, date, Year, Month, day, Location)
dtr_Kouroussa_monthly_2015 <- full_join(dtr_Kouroussa_monthly_2015a, dtr_Kouroussa_monthly_2015)
dtr_Kouroussa_monthly <- rbind(select(dtr_Kouroussa_monthly,date, Year, Month, day, Location, dtr), dtr_Kouroussa_monthly_2015)
rm(dtr_Kouroussa_monthly_2015, dtr_Kouroussa_monthly_2015a)
dtr_Kouroussa_monthly$measurement <- "dtr"
dtr_Kouroussa_monthly <- rename(dtr_Kouroussa_monthly, Value=dtr)
#Labe
dtr_Labe_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        dtr1=dtr.var[which(lon==12.25),which(lat==11.75),1:48],
                                        dtr2=dtr.var[which(lon==12.25),which(lat==11.25),1:48]))
dtr_Labe_monthly$dtr <- rowMeans(select(dtr_Labe_monthly, dtr1, dtr2))
dtr_Labe_monthly$Location <- 'Labe'
dtr_Labe_monthly$date <- ymd(paste(dtr_Labe_monthly$Year, dtr_Labe_monthly$Month, dtr_Labe_monthly$day, sep="-"))
dtr_Labe_monthly_2015 <- select(dtr_Labe_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Labe_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Labe_monthly_2015a$Location <- 'Labe'
dtr_Labe_monthly_2015a$date <- ymd(paste(dtr_Labe_monthly_2015a$Year, dtr_Labe_monthly_2015a$Month, dtr_Labe_monthly_2015a$day, sep="-"))
dtr_Labe_monthly_2015a <- select(dtr_Labe_monthly_2015a, date, Year, Month, day, Location)
dtr_Labe_monthly_2015 <- full_join(dtr_Labe_monthly_2015a, dtr_Labe_monthly_2015)
dtr_Labe_monthly <- rbind(select(dtr_Labe_monthly,date, Year, Month, day, Location, dtr), dtr_Labe_monthly_2015)
rm(dtr_Labe_monthly_2015, dtr_Labe_monthly_2015a)
dtr_Labe_monthly$measurement <- "dtr"
dtr_Labe_monthly <- rename(dtr_Labe_monthly, Value=dtr)
#Lelouma
dtr_Lelouma_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           dtr1=dtr.var[which(lon==12.75),which(lat==11.75),1:48],
                                           dtr2=dtr.var[which(lon==12.75),which(lat==11.25),1:48]))
dtr_Lelouma_monthly$dtr <- rowMeans(select(dtr_Lelouma_monthly, dtr1, dtr2))
dtr_Lelouma_monthly$Location <- 'Lelouma'
dtr_Lelouma_monthly$date <- ymd(paste(dtr_Lelouma_monthly$Year, dtr_Lelouma_monthly$Month, dtr_Lelouma_monthly$day, sep="-"))
dtr_Lelouma_monthly_2015 <- select(dtr_Lelouma_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Lelouma_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Lelouma_monthly_2015a$Location <- 'Lelouma'
dtr_Lelouma_monthly_2015a$date <- ymd(paste(dtr_Lelouma_monthly_2015a$Year, dtr_Lelouma_monthly_2015a$Month, dtr_Lelouma_monthly_2015a$day, sep="-"))
dtr_Lelouma_monthly_2015a <- select(dtr_Lelouma_monthly_2015a, date, Year, Month, day, Location)
dtr_Lelouma_monthly_2015 <- full_join(dtr_Lelouma_monthly_2015a, dtr_Lelouma_monthly_2015)
dtr_Lelouma_monthly <- rbind(select(dtr_Lelouma_monthly,date, Year, Month, day, Location, dtr), dtr_Lelouma_monthly_2015)
rm(dtr_Lelouma_monthly_2015, dtr_Lelouma_monthly_2015a)
dtr_Lelouma_monthly$measurement <- "dtr"
dtr_Lelouma_monthly <- rename(dtr_Lelouma_monthly, Value=dtr)
#Lola
dtr_Lola_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        dtr1=dtr.var[which(lon==8.25),which(lat==8.25),1:48],
                                        dtr2=dtr.var[which(lon==8.25),which(lat==7.75),1:48]))
dtr_Lola_monthly$dtr <- rowMeans(select(dtr_Lola_monthly, dtr1, dtr2))
dtr_Lola_monthly$Location <- 'Lola'
dtr_Lola_monthly$date <- ymd(paste(dtr_Lola_monthly$Year, dtr_Lola_monthly$Month, dtr_Lola_monthly$day, sep="-"))
dtr_Lola_monthly_2015 <- select(dtr_Lola_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Lola_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Lola_monthly_2015a$Location <- 'Lola'
dtr_Lola_monthly_2015a$date <- ymd(paste(dtr_Lola_monthly_2015a$Year, dtr_Lola_monthly_2015a$Month, dtr_Lola_monthly_2015a$day, sep="-"))
dtr_Lola_monthly_2015a <- select(dtr_Lola_monthly_2015a, date, Year, Month, day, Location)
dtr_Lola_monthly_2015 <- full_join(dtr_Lola_monthly_2015a, dtr_Lola_monthly_2015)
dtr_Lola_monthly <- rbind(select(dtr_Lola_monthly,date, Year, Month, day, Location, dtr), dtr_Lola_monthly_2015)
rm(dtr_Lola_monthly_2015, dtr_Lola_monthly_2015a)
dtr_Lola_monthly$measurement <- "dtr"
dtr_Lola_monthly <- rename(dtr_Lola_monthly, Value=dtr)
#Macenta
dtr_Macenta_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           dtr1=dtr.var[which(lon==9.75),which(lat==8.75),1:48],
                                           dtr2=dtr.var[which(lon==9.25),which(lat==8.75),1:48],
                                           dtr3=dtr.var[which(lon==9.25),which(lat==8.25),1:48]))
dtr_Macenta_monthly$dtr <- rowMeans(select(dtr_Macenta_monthly, dtr1, dtr2, dtr3))
dtr_Macenta_monthly$Location <- 'Macenta'
dtr_Macenta_monthly$date <- ymd(paste(dtr_Macenta_monthly$Year, dtr_Macenta_monthly$Month, dtr_Macenta_monthly$day, sep="-"))
dtr_Macenta_monthly_2015 <- select(dtr_Macenta_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Macenta_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Macenta_monthly_2015a$Location <- 'Macenta'
dtr_Macenta_monthly_2015a$date <- ymd(paste(dtr_Macenta_monthly_2015a$Year, dtr_Macenta_monthly_2015a$Month, dtr_Macenta_monthly_2015a$day, sep="-"))
dtr_Macenta_monthly_2015a <- select(dtr_Macenta_monthly_2015a, date, Year, Month, day, Location)
dtr_Macenta_monthly_2015 <- full_join(dtr_Macenta_monthly_2015a, dtr_Macenta_monthly_2015)
dtr_Macenta_monthly <- rbind(select(dtr_Macenta_monthly,date, Year, Month, day, Location, dtr), dtr_Macenta_monthly_2015)
rm(dtr_Macenta_monthly_2015, dtr_Macenta_monthly_2015a)
dtr_Macenta_monthly$measurement <- "dtr"
dtr_Macenta_monthly <- rename(dtr_Macenta_monthly, Value=dtr)
#Mali
dtr_Mali_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        dtr1=dtr.var[which(lon==12.75),which(lat==12.25),1:48],
                                        dtr2=dtr.var[which(lon==12.25),which(lat==12.25),1:48], 
                                        dtr3=dtr.var[which(lon==11.75),which(lat==12.25),1:48],
                                        dtr4=dtr.var[which(lon==12.75),which(lat==11.75),1:48],
                                        dtr5=dtr.var[which(lon==12.25),which(lat==11.75),1:48]))
dtr_Mali_monthly$dtr <- rowMeans(select(dtr_Mali_monthly, dtr1, dtr2, dtr3, dtr4, dtr5))
dtr_Mali_monthly$Location <- 'Mali'
dtr_Mali_monthly$date <- ymd(paste(dtr_Mali_monthly$Year, dtr_Mali_monthly$Month, dtr_Mali_monthly$day, sep="-"))
dtr_Mali_monthly_2015 <- select(dtr_Mali_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Mali_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Mali_monthly_2015a$Location <- 'Mali'
dtr_Mali_monthly_2015a$date <- ymd(paste(dtr_Mali_monthly_2015a$Year, dtr_Mali_monthly_2015a$Month, dtr_Mali_monthly_2015a$day, sep="-"))
dtr_Mali_monthly_2015a <- select(dtr_Mali_monthly_2015a, date, Year, Month, day, Location)
dtr_Mali_monthly_2015 <- full_join(dtr_Mali_monthly_2015a, dtr_Mali_monthly_2015)
dtr_Mali_monthly <- rbind(select(dtr_Mali_monthly,date, Year, Month, day, Location, dtr), dtr_Mali_monthly_2015)
rm(dtr_Mali_monthly_2015, dtr_Mali_monthly_2015a)
dtr_Mali_monthly$measurement <- "dtr"
dtr_Mali_monthly <- rename(dtr_Mali_monthly, Value=dtr)
#Mamou
dtr_Mamou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         dtr1=dtr.var[which(lon==11.75),which(lat==10.75),1:48],
                                         dtr2=dtr.var[which(lon==12.25),which(lat==10.25),1:48],
                                         dtr3=dtr.var[which(lon==11.75),which(lat==10.25),1:48]))
dtr_Mamou_monthly$dtr <- rowMeans(select(dtr_Mamou_monthly, dtr1, dtr2, dtr3))
dtr_Mamou_monthly$Location <- 'Mamou'
dtr_Mamou_monthly$date <- ymd(paste(dtr_Mamou_monthly$Year, dtr_Mamou_monthly$Month, dtr_Mamou_monthly$day, sep="-"))
dtr_Mamou_monthly_2015 <- select(dtr_Mamou_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Mamou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Mamou_monthly_2015a$Location <- 'Mamou'
dtr_Mamou_monthly_2015a$date <- ymd(paste(dtr_Mamou_monthly_2015a$Year, dtr_Mamou_monthly_2015a$Month, dtr_Mamou_monthly_2015a$day, sep="-"))
dtr_Mamou_monthly_2015a <- select(dtr_Mamou_monthly_2015a, date, Year, Month, day, Location)
dtr_Mamou_monthly_2015 <- full_join(dtr_Mamou_monthly_2015a, dtr_Mamou_monthly_2015)
dtr_Mamou_monthly <- rbind(select(dtr_Mamou_monthly,date, Year, Month, day, Location, dtr), dtr_Mamou_monthly_2015)
rm(dtr_Mamou_monthly_2015, dtr_Mamou_monthly_2015a)
dtr_Mamou_monthly$measurement <- "dtr"
dtr_Mamou_monthly <- rename(dtr_Mamou_monthly, Value=dtr)
#Nzerekore
dtr_Nzerekore_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             dtr1=dtr.var[which(lon==8.75),which(lat==8.25),1:48],
                                             dtr2=dtr.var[which(lon==8.75),which(lat==7.75),1:48]))
dtr_Nzerekore_monthly$dtr <- rowMeans(select(dtr_Nzerekore_monthly, dtr1, dtr2))
dtr_Nzerekore_monthly$Location <- 'Nzerekore'
dtr_Nzerekore_monthly$date <- ymd(paste(dtr_Nzerekore_monthly$Year, dtr_Nzerekore_monthly$Month, dtr_Nzerekore_monthly$day, sep="-"))
dtr_Nzerekore_monthly_2015 <- select(dtr_Nzerekore_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Nzerekore_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Nzerekore_monthly_2015a$Location <- 'Nzerekore'
dtr_Nzerekore_monthly_2015a$date <- ymd(paste(dtr_Nzerekore_monthly_2015a$Year, dtr_Nzerekore_monthly_2015a$Month, dtr_Nzerekore_monthly_2015a$day, sep="-"))
dtr_Nzerekore_monthly_2015a <- select(dtr_Nzerekore_monthly_2015a, date, Year, Month, day, Location)
dtr_Nzerekore_monthly_2015 <- full_join(dtr_Nzerekore_monthly_2015a, dtr_Nzerekore_monthly_2015)
dtr_Nzerekore_monthly <- rbind(select(dtr_Nzerekore_monthly,date, Year, Month, day, Location, dtr), dtr_Nzerekore_monthly_2015)
rm(dtr_Nzerekore_monthly_2015, dtr_Nzerekore_monthly_2015a)
dtr_Nzerekore_monthly$measurement <- "dtr"
dtr_Nzerekore_monthly <- rename(dtr_Nzerekore_monthly, Value=dtr)
#Pita
dtr_Pita_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        dtr1=dtr.var[which(lon==12.75),which(lat==11.25),1:48],
                                        dtr2=dtr.var[which(lon==12.25),which(lat==11.25),1:48],
                                        dtr3=dtr.var[which(lon==12.75),which(lat==10.75),1:48]))
dtr_Pita_monthly$dtr <- rowMeans(select(dtr_Pita_monthly, dtr1, dtr2, dtr3))
dtr_Pita_monthly$Location <- 'Pita'
dtr_Pita_monthly$date <- ymd(paste(dtr_Pita_monthly$Year, dtr_Pita_monthly$Month, dtr_Pita_monthly$day, sep="-"))
dtr_Pita_monthly_2015 <- select(dtr_Pita_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Pita_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Pita_monthly_2015a$Location <- 'Pita'
dtr_Pita_monthly_2015a$date <- ymd(paste(dtr_Pita_monthly_2015a$Year, dtr_Pita_monthly_2015a$Month, dtr_Pita_monthly_2015a$day, sep="-"))
dtr_Pita_monthly_2015a <- select(dtr_Pita_monthly_2015a, date, Year, Month, day, Location)
dtr_Pita_monthly_2015 <- full_join(dtr_Pita_monthly_2015a, dtr_Pita_monthly_2015)
dtr_Pita_monthly <- rbind(select(dtr_Pita_monthly,date, Year, Month, day, Location, dtr), dtr_Pita_monthly_2015)
rm(dtr_Pita_monthly_2015, dtr_Pita_monthly_2015a)
dtr_Pita_monthly$measurement <- "dtr"
dtr_Pita_monthly <- rename(dtr_Pita_monthly, Value=dtr)
#Siguiri
dtr_Siguiri_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           dtr1=dtr.var[which(lon==9.25),which(lat==12.25),1:48],
                                           dtr2=dtr.var[which(lon==10.25),which(lat==11.75),1:48], 
                                           dtr3=dtr.var[which(lon==9.75),which(lat==11.75),1:48],
                                           dtr4=dtr.var[which(lon==9.25),which(lat==11.75),1:48],
                                           dtr5=dtr.var[which(lon==9.75),which(lat==11.25),1:48],
                                           dtr6=dtr.var[which(lon==9.25),which(lat==11.25),1:48]))
dtr_Siguiri_monthly$dtr <- rowMeans(select(dtr_Siguiri_monthly, dtr1, dtr2, dtr3, dtr4, dtr5, dtr6))
dtr_Siguiri_monthly$Location <- 'Siguiri'
dtr_Siguiri_monthly$date <- ymd(paste(dtr_Siguiri_monthly$Year, dtr_Siguiri_monthly$Month, dtr_Siguiri_monthly$day, sep="-"))
dtr_Siguiri_monthly_2015 <- select(dtr_Siguiri_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Siguiri_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Siguiri_monthly_2015a$Location <- 'Siguiri'
dtr_Siguiri_monthly_2015a$date <- ymd(paste(dtr_Siguiri_monthly_2015a$Year, dtr_Siguiri_monthly_2015a$Month, dtr_Siguiri_monthly_2015a$day, sep="-"))
dtr_Siguiri_monthly_2015a <- select(dtr_Siguiri_monthly_2015a, date, Year, Month, day, Location)
dtr_Siguiri_monthly_2015 <- full_join(dtr_Siguiri_monthly_2015a, dtr_Siguiri_monthly_2015)
dtr_Siguiri_monthly <- rbind(select(dtr_Siguiri_monthly,date, Year, Month, day, Location, dtr), dtr_Siguiri_monthly_2015)
rm(dtr_Siguiri_monthly_2015, dtr_Siguiri_monthly_2015a)
dtr_Siguiri_monthly$measurement <- "dtr"
dtr_Siguiri_monthly <- rename(dtr_Siguiri_monthly, Value=dtr)
#Telimele
dtr_Telimele_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            dtr1=dtr.var[which(lon==13.75),which(lat==11.25),1:48],
                                            dtr2=dtr.var[which(lon==13.25),which(lat==11.25),1:48], 
                                            dtr3=dtr.var[which(lon==13.75),which(lat==10.75),1:48],
                                            dtr4=dtr.var[which(lon==13.25),which(lat==10.75),1:48]))
dtr_Telimele_monthly$dtr <- rowMeans(select(dtr_Telimele_monthly, dtr1, dtr2, dtr3, dtr4))
dtr_Telimele_monthly$Location <- 'Telimele'
dtr_Telimele_monthly$date <- ymd(paste(dtr_Telimele_monthly$Year, dtr_Telimele_monthly$Month, dtr_Telimele_monthly$day, sep="-"))
dtr_Telimele_monthly_2015 <- select(dtr_Telimele_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Telimele_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Telimele_monthly_2015a$Location <- 'Telimele'
dtr_Telimele_monthly_2015a$date <- ymd(paste(dtr_Telimele_monthly_2015a$Year, dtr_Telimele_monthly_2015a$Month, dtr_Telimele_monthly_2015a$day, sep="-"))
dtr_Telimele_monthly_2015a <- select(dtr_Telimele_monthly_2015a, date, Year, Month, day, Location)
dtr_Telimele_monthly_2015 <- full_join(dtr_Telimele_monthly_2015a, dtr_Telimele_monthly_2015)
dtr_Telimele_monthly <- rbind(select(dtr_Telimele_monthly,date, Year, Month, day, Location, dtr), dtr_Telimele_monthly_2015)
rm(dtr_Telimele_monthly_2015, dtr_Telimele_monthly_2015a)
dtr_Telimele_monthly$measurement <- "dtr"
dtr_Telimele_monthly <- rename(dtr_Telimele_monthly, Value=dtr)
#Tougue
dtr_Tougue_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4),day=1, 
                                          dtr1=dtr.var[which(lon==11.25),which(lat==11.75),1:48],
                                          dtr2=dtr.var[which(lon==11.75),which(lat==11.25),1:48]))
dtr_Tougue_monthly$dtr <- rowMeans(select(dtr_Tougue_monthly, dtr1, dtr2))
dtr_Tougue_monthly$Location <- 'Tougue'
dtr_Tougue_monthly$date <- ymd(paste(dtr_Tougue_monthly$Year, dtr_Tougue_monthly$Month, dtr_Tougue_monthly$day, sep="-"))
dtr_Tougue_monthly_2015 <- select(dtr_Tougue_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_Tougue_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_Tougue_monthly_2015a$Location <- 'Tougue'
dtr_Tougue_monthly_2015a$date <- ymd(paste(dtr_Tougue_monthly_2015a$Year, dtr_Tougue_monthly_2015a$Month, dtr_Tougue_monthly_2015a$day, sep="-"))
dtr_Tougue_monthly_2015a <- select(dtr_Tougue_monthly_2015a, date, Year, Month, day, Location)
dtr_Tougue_monthly_2015 <- full_join(dtr_Tougue_monthly_2015a, dtr_Tougue_monthly_2015)
dtr_Tougue_monthly <- rbind(select(dtr_Tougue_monthly,date, Year, Month, day, Location, dtr), dtr_Tougue_monthly_2015)
rm(dtr_Tougue_monthly_2015, dtr_Tougue_monthly_2015a)
dtr_Tougue_monthly$measurement <- "dtr"
dtr_Tougue_monthly <- rename(dtr_Tougue_monthly, Value=dtr)
#yamou
dtr_yamou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4),day=1, 
                                         dtr1=dtr.var[which(lon==9.25),which(lat==7.75),1:48],
                                         dtr2=dtr.var[which(lon==9.25),which(lat==7.25),1:48],
                                         dtr3=dtr.var[which(lon==8.75),which(lat==7.25),1:48]))
dtr_yamou_monthly$Location <- 'yamou'
dtr_yamou_monthly$dtr <- rowMeans(select(dtr_yamou_monthly, dtr1, dtr2, dtr3))
dtr_yamou_monthly$date <- ymd(paste(dtr_yamou_monthly$Year, dtr_yamou_monthly$Month, dtr_yamou_monthly$day, sep="-"))
dtr_yamou_monthly_2015 <- select(dtr_yamou_monthly, Location, Year, Month, dtr) %>% group_by( Month) %>% summarize(dtr=mean(dtr))
dtr_yamou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
dtr_yamou_monthly_2015a$Location <- 'yamou'
dtr_yamou_monthly_2015a$date <- ymd(paste(dtr_yamou_monthly_2015a$Year, dtr_yamou_monthly_2015a$Month, dtr_yamou_monthly_2015a$day, sep="-"))
dtr_yamou_monthly_2015a <- select(dtr_yamou_monthly_2015a, date, Year, Month, day, Location)
dtr_yamou_monthly_2015 <- full_join(dtr_yamou_monthly_2015a, dtr_yamou_monthly_2015)
dtr_yamou_monthly <- rbind(select(dtr_yamou_monthly,date, Year, Month, day, Location, dtr), dtr_yamou_monthly_2015)
rm(dtr_yamou_monthly_2015, dtr_yamou_monthly_2015a)
dtr_yamou_monthly$measurement <- "dtr"
dtr_yamou_monthly <- rename(dtr_yamou_monthly, Value=dtr)

#Merging in long format
dtr_Guinea_monthly_district <- rbind(dtr_Beyla_monthly, dtr_Boke_monthly, dtr_Boffa_monthly,
                                     dtr_Conakry_monthly, dtr_Coyah_monthly, dtr_Dabola_monthly, dtr_Dalaba_monthly,
                                     dtr_Dinguiray_monthly, dtr_Dubreka_monthly, dtr_Faranah_monthly,
                                     dtr_Forecariah_monthly, dtr_Fria_monthly, dtr_Gaoual_monthly,
                                     dtr_Gueckedou_monthly, dtr_Kankan_monthly, dtr_Kerouane_monthly,
                                     dtr_Kindia_monthly, dtr_Kissidougou_monthly, dtr_Koubia_monthly,
                                     dtr_Koundara_monthly, dtr_Kouroussa_monthly, dtr_Labe_monthly,
                                     dtr_Lelouma_monthly, dtr_Lola_monthly, dtr_Macenta_monthly,
                                     dtr_Mali_monthly, dtr_Mamou_monthly, dtr_Nzerekore_monthly,
                                     dtr_Pita_monthly, dtr_Siguiri_monthly, dtr_Telimele_monthly,
                                     dtr_Tougue_monthly, dtr_yamou_monthly)

#####################
#pet - potential evapotranspiration
#####################
pet.full <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.pet.dat.nc', write=FALSE)
pet.var <- var.get.nc(pet.full, "pet")
lon <- var.get.nc(pet.full, "lon")
lat <- var.get.nc(pet.full, "lat")
#Beyla
pet_Beyla_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         pet1=pet.var[which(lon==8.75),which(lat==9.25),1:48],
                                         pet2=pet.var[which(lon==8.25),which(lat==9.25),1:48], 
                                         pet3=pet.var[which(lon==7.75),which(lat==9.25),1:48],
                                         pet4=pet.var[which(lon==8.75),which(lat==8.75),1:48], 
                                         pet5=pet.var[which(lon==8.25),which(lat==8.75),1:48],
                                         pet6=pet.var[which(lon==7.75),which(lat==8.75),1:48], 
                                         pet7=pet.var[which(lon==8.75),which(lat==8.25),1:48],
                                         pet8=pet.var[which(lon==8.25),which(lat==8.25),1:48]))
pet_Beyla_monthly$pet <- rowMeans(select(pet_Beyla_monthly, pet1, pet2, pet3, pet4, pet5, pet6, pet7, pet8))
pet_Beyla_monthly$Location <- 'Beyla'
pet_Beyla_monthly$date <- ymd(paste(pet_Beyla_monthly$Year, pet_Beyla_monthly$Month, pet_Beyla_monthly$day, sep="-"))
pet_Beyla_monthly_2015 <- select(pet_Beyla_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Beyla_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Beyla_monthly_2015a$Location <- 'Beyla'
pet_Beyla_monthly_2015a$date <- ymd(paste(pet_Beyla_monthly_2015a$Year, pet_Beyla_monthly_2015a$Month, pet_Beyla_monthly_2015a$day, sep="-"))
pet_Beyla_monthly_2015a <- select(pet_Beyla_monthly_2015a, date, Year, Month, day, Location)
pet_Beyla_monthly_2015 <- full_join(pet_Beyla_monthly_2015a, pet_Beyla_monthly_2015)
pet_Beyla_monthly <- rbind(select(pet_Beyla_monthly,date, Year, Month, day, Location, pet), pet_Beyla_monthly_2015)
rm(pet_Beyla_monthly_2015, pet_Beyla_monthly_2015a)
pet_Beyla_monthly$measurement <- "pet"
pet_Beyla_monthly <- rename(pet_Beyla_monthly, Value=pet)
#Boffa
pet_Boffa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         pet1=pet.var[which(lon==14.25),which(lat==10.75),1:48],
                                         pet2=pet.var[which(lon==13.75),which(lat==10.75),1:48], 
                                         pet3=pet.var[which(lon==14.25),which(lat==10.25),1:48],
                                         pet4=pet.var[which(lon==13.75),which(lat==10.25),1:48]))
pet_Boffa_monthly$pet <- rowMeans(select(pet_Boffa_monthly, pet1, pet2, pet3, pet4))
pet_Boffa_monthly$Location <- 'Boffa'
pet_Boffa_monthly$date <- ymd(paste(pet_Boffa_monthly$Year, pet_Boffa_monthly$Month, pet_Boffa_monthly$day, sep="-"))
pet_Boffa_monthly_2015 <- select(pet_Boffa_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Boffa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Boffa_monthly_2015a$Location <- 'Boffa'
pet_Boffa_monthly_2015a$date <- ymd(paste(pet_Boffa_monthly_2015a$Year, pet_Boffa_monthly_2015a$Month, pet_Boffa_monthly_2015a$day, sep="-"))
pet_Boffa_monthly_2015a <- select(pet_Boffa_monthly_2015a, date, Year, Month, day, Location)
pet_Boffa_monthly_2015 <- full_join(pet_Boffa_monthly_2015a, pet_Boffa_monthly_2015)
pet_Boffa_monthly <- rbind(select(pet_Boffa_monthly,date, Year, Month, day, Location, pet), pet_Boffa_monthly_2015)
rm(pet_Boffa_monthly_2015, pet_Boffa_monthly_2015a)
pet_Boffa_monthly$measurement <- "pet"
pet_Boffa_monthly <- rename(pet_Boffa_monthly, Value=pet)
#Boke
pet_Boke_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        pet1=pet.var[which(lon==14.25),which(lat==11.75),1:48],
                                        pet2=pet.var[which(lon==14.75),which(lat==11.25),1:48], 
                                        pet3=pet.var[which(lon==14.25),which(lat==11.25),1:48],
                                        pet4=pet.var[which(lon==13.75),which(lat==11.25),1:48]))
pet_Boke_monthly$pet <- rowMeans(select(pet_Boke_monthly, pet1, pet2, pet3, pet4))
pet_Boke_monthly$Location <- 'Boke'
pet_Boke_monthly$date <- ymd(paste(pet_Boke_monthly$Year, pet_Boke_monthly$Month, pet_Boke_monthly$day, sep="-"))
pet_Boke_monthly_2015 <- select(pet_Boke_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Boke_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Boke_monthly_2015a$Location <- 'Boke'
pet_Boke_monthly_2015a$date <- ymd(paste(pet_Boke_monthly_2015a$Year, pet_Boke_monthly_2015a$Month, pet_Boke_monthly_2015a$day, sep="-"))
pet_Boke_monthly_2015a <- select(pet_Boke_monthly_2015a, date, Year, Month, day, Location)
pet_Boke_monthly_2015 <- full_join(pet_Boke_monthly_2015a, pet_Boke_monthly_2015)
pet_Boke_monthly <- rbind(select(pet_Boke_monthly,date, Year, Month, day, Location, pet), pet_Boke_monthly_2015)
rm(pet_Boke_monthly_2015, pet_Boke_monthly_2015a)
pet_Boke_monthly$measurement <- "pet"
pet_Boke_monthly <- rename(pet_Boke_monthly, Value=pet)
#Conakry
pet_Conakry_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           pet=pet.var[which(lon==13.75),which(lat==9.75),1:48]))
pet_Conakry_monthly$Location <- 'Conakry'
pet_Conakry_monthly$date <- ymd(paste(pet_Conakry_monthly$Year, pet_Conakry_monthly$Month, pet_Conakry_monthly$day, sep="-"))
pet_Conakry_monthly_2015 <- select(pet_Conakry_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Conakry_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Conakry_monthly_2015a$Location <- 'Conakry'
pet_Conakry_monthly_2015a$date <- ymd(paste(pet_Conakry_monthly_2015a$Year, pet_Conakry_monthly_2015a$Month, pet_Conakry_monthly_2015a$day, sep="-"))
pet_Conakry_monthly_2015a <- select(pet_Conakry_monthly_2015a, date, Year, Month, day, Location)
pet_Conakry_monthly_2015 <- full_join(pet_Conakry_monthly_2015a, pet_Conakry_monthly_2015)
pet_Conakry_monthly <- rbind(select(pet_Conakry_monthly,date, Year, Month, day, Location, pet), pet_Conakry_monthly_2015)
rm(pet_Conakry_monthly_2015, pet_Conakry_monthly_2015a)
pet_Conakry_monthly$measurement <- "pet"
pet_Conakry_monthly <- rename(pet_Conakry_monthly, Value=pet)

#Coyah
pet_Coyah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         pet=pet.var[which(lon==9.75),which(lat==13.25),1:48]))
pet_Coyah_monthly$Location <- 'Coyah'
pet_Coyah_monthly$date <- ymd(paste(pet_Coyah_monthly$Year, pet_Coyah_monthly$Month, pet_Coyah_monthly$day, sep="-"))
pet_Coyah_monthly_2015 <- select(pet_Coyah_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Coyah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Coyah_monthly_2015a$Location <- 'Coyah'
pet_Coyah_monthly_2015a$date <- ymd(paste(pet_Coyah_monthly_2015a$Year, pet_Coyah_monthly_2015a$Month, pet_Coyah_monthly_2015a$day, sep="-"))
pet_Coyah_monthly_2015a <- select(pet_Coyah_monthly_2015a, date, Year, Month, day, Location)
pet_Coyah_monthly_2015 <- full_join(pet_Coyah_monthly_2015a, pet_Coyah_monthly_2015)
pet_Coyah_monthly <- rbind(select(pet_Coyah_monthly,date, Year, Month, day, Location, pet), pet_Coyah_monthly_2015)
rm(pet_Coyah_monthly_2015, pet_Coyah_monthly_2015a)
pet_Coyah_monthly$measurement <- "pet"
pet_Coyah_monthly <- rename(pet_Coyah_monthly, Value=pet)
#Dabola
pet_Dabola_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          pet1=pet.var[which(lon==11.25),which(lat==10.75),1:48],
                                          pet2=pet.var[which(lon==10.75),which(lat==10.75),1:48]))
pet_Dabola_monthly$pet <- rowMeans(select(pet_Dabola_monthly, pet1, pet2))
pet_Dabola_monthly$Location <- 'Dabola'
pet_Dabola_monthly$date <- ymd(paste(pet_Dabola_monthly$Year, pet_Dabola_monthly$Month, pet_Dabola_monthly$day, sep="-"))
pet_Dabola_monthly_2015 <- select(pet_Dabola_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Dabola_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Dabola_monthly_2015a$Location <- 'Dabola'
pet_Dabola_monthly_2015a$date <- ymd(paste(pet_Dabola_monthly_2015a$Year, pet_Dabola_monthly_2015a$Month, pet_Dabola_monthly_2015a$day, sep="-"))
pet_Dabola_monthly_2015a <- select(pet_Dabola_monthly_2015a, date, Year, Month, day, Location)
pet_Dabola_monthly_2015 <- full_join(pet_Dabola_monthly_2015a, pet_Dabola_monthly_2015)
pet_Dabola_monthly <- rbind(select(pet_Dabola_monthly,date, Year, Month, day, Location, pet), pet_Dabola_monthly_2015)
rm(pet_Dabola_monthly_2015, pet_Dabola_monthly_2015a)
pet_Dabola_monthly$measurement <- "pet"
pet_Dabola_monthly <- rename(pet_Dabola_monthly, Value=pet)
#Dalaba
pet_Dalaba_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          pet1=pet.var[which(lon==11.25),which(lat==12.25),1:48],
                                          pet2=pet.var[which(lon==10.75),which(lat==12.25),1:48]))
pet_Dalaba_monthly$pet <- rowMeans(select(pet_Dalaba_monthly, pet1, pet2))
pet_Dalaba_monthly$Location <- 'Dalaba'
pet_Dalaba_monthly$date <- ymd(paste(pet_Dalaba_monthly$Year, pet_Dalaba_monthly$Month, pet_Dalaba_monthly$day, sep="-"))
pet_Dalaba_monthly_2015 <- select(pet_Dalaba_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Dalaba_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Dalaba_monthly_2015a$Location <- 'Dalaba'
pet_Dalaba_monthly_2015a$date <- ymd(paste(pet_Dalaba_monthly_2015a$Year, pet_Dalaba_monthly_2015a$Month, pet_Dalaba_monthly_2015a$day, sep="-"))
pet_Dalaba_monthly_2015a <- select(pet_Dalaba_monthly_2015a, date, Year, Month, day, Location)
pet_Dalaba_monthly_2015 <- full_join(pet_Dalaba_monthly_2015a, pet_Dalaba_monthly_2015)
pet_Dalaba_monthly <- rbind(select(pet_Dalaba_monthly,date, Year, Month, day, Location, pet), pet_Dalaba_monthly_2015)
rm(pet_Dalaba_monthly_2015, pet_Dalaba_monthly_2015a)
pet_Dalaba_monthly$measurement <- "pet"
pet_Dalaba_monthly <- rename(pet_Dalaba_monthly, Value=pet)
#Dinguiray
pet_Dinguiray_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             pet1=pet.var[which(lon==11.25),which(lat==11.75),1:48],
                                             pet2=pet.var[which(lon==10.75),which(lat==11.75),1:48], 
                                             pet3=pet.var[which(lon==10.25),which(lat==11.75),1:48],
                                             pet4=pet.var[which(lon==11.25),which(lat==11.25),1:48],
                                             pet5=pet.var[which(lon==10.75),which(lat==11.25),1:48],
                                             pet6=pet.var[which(lon==10.25),which(lat==11.25),1:48]))
pet_Dinguiray_monthly$pet <- rowMeans(select(pet_Dinguiray_monthly, pet1, pet2, pet3, pet4, pet5, pet6))
pet_Dinguiray_monthly$Location <- 'Dinguiray'
pet_Dinguiray_monthly$date <- ymd(paste(pet_Dinguiray_monthly$Year, pet_Dinguiray_monthly$Month, pet_Dinguiray_monthly$day, sep="-"))
pet_Dinguiray_monthly_2015 <- select(pet_Dinguiray_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Dinguiray_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Dinguiray_monthly_2015a$Location <- 'Dinguiray'
pet_Dinguiray_monthly_2015a$date <- ymd(paste(pet_Dinguiray_monthly_2015a$Year, pet_Dinguiray_monthly_2015a$Month, pet_Dinguiray_monthly_2015a$day, sep="-"))
pet_Dinguiray_monthly_2015a <- select(pet_Dinguiray_monthly_2015a, date, Year, Month, day, Location)
pet_Dinguiray_monthly_2015 <- full_join(pet_Dinguiray_monthly_2015a, pet_Dinguiray_monthly_2015)
pet_Dinguiray_monthly <- rbind(select(pet_Dinguiray_monthly,date, Year, Month, day, Location, pet), pet_Dinguiray_monthly_2015)
rm(pet_Dinguiray_monthly_2015, pet_Dinguiray_monthly_2015a)
pet_Dinguiray_monthly$measurement <- "pet"
pet_Dinguiray_monthly <- rename(pet_Dinguiray_monthly, Value=pet)
#Dubreka
pet_Dubreka_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           pet1=pet.var[which(lon==13.75),which(lat==10.25),1:48],
                                           pet2=pet.var[which(lon==13.25),which(lat==10.25),1:48]))
pet_Dubreka_monthly$pet <- rowMeans(select(pet_Dubreka_monthly, pet1, pet2))
pet_Dubreka_monthly$Location <- 'Dubreka'
pet_Dubreka_monthly$date <- ymd(paste(pet_Dubreka_monthly$Year, pet_Dubreka_monthly$Month, pet_Dubreka_monthly$day, sep="-"))
pet_Dubreka_monthly_2015 <- select(pet_Dubreka_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Dubreka_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Dubreka_monthly_2015a$Location <- 'Dubreka'
pet_Dubreka_monthly_2015a$date <- ymd(paste(pet_Dubreka_monthly_2015a$Year, pet_Dubreka_monthly_2015a$Month, pet_Dubreka_monthly_2015a$day, sep="-"))
pet_Dubreka_monthly_2015a <- select(pet_Dubreka_monthly_2015a, date, Year, Month, day, Location)
pet_Dubreka_monthly_2015 <- full_join(pet_Dubreka_monthly_2015a, pet_Dubreka_monthly_2015)
pet_Dubreka_monthly <- rbind(select(pet_Dubreka_monthly,date, Year, Month, day, Location, pet), pet_Dubreka_monthly_2015)
rm(pet_Dubreka_monthly_2015, pet_Dubreka_monthly_2015a)
pet_Dubreka_monthly$measurement <- "pet"
pet_Dubreka_monthly <- rename(pet_Dubreka_monthly, Value=pet)
#Faranah
pet_Faranah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           pet1=pet.var[which(lon==11.25),which(lat==10.25),1:48],
                                           pet2=pet.var[which(lon==10.75),which(lat==10.25),1:48], 
                                           pet3=pet.var[which(lon==10.25),which(lat==10.25),1:48],
                                           pet4=pet.var[which(lon==10.75),which(lat==9.75),1:48],
                                           pet5=pet.var[which(lon==10.75),which(lat==9.25),1:48]))
pet_Faranah_monthly$pet <- rowMeans(select(pet_Faranah_monthly, pet1, pet2, pet3, pet4, pet5))
pet_Faranah_monthly$Location <- 'Faranah'
pet_Faranah_monthly$date <- ymd(paste(pet_Faranah_monthly$Year, pet_Faranah_monthly$Month, pet_Faranah_monthly$day, sep="-"))
pet_Faranah_monthly_2015 <- select(pet_Faranah_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Faranah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Faranah_monthly_2015a$Location <- 'Faranah'
pet_Faranah_monthly_2015a$date <- ymd(paste(pet_Faranah_monthly_2015a$Year, pet_Faranah_monthly_2015a$Month, pet_Faranah_monthly_2015a$day, sep="-"))
pet_Faranah_monthly_2015a <- select(pet_Faranah_monthly_2015a, date, Year, Month, day, Location)
pet_Faranah_monthly_2015 <- full_join(pet_Faranah_monthly_2015a, pet_Faranah_monthly_2015)
pet_Faranah_monthly <- rbind(select(pet_Faranah_monthly,date, Year, Month, day, Location, pet), pet_Faranah_monthly_2015)
rm(pet_Faranah_monthly_2015, pet_Faranah_monthly_2015a)
pet_Faranah_monthly$measurement <- "pet"
pet_Faranah_monthly <- rename(pet_Faranah_monthly, Value=pet)
#Forecariah
pet_Forecariah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                              Month=rep(seq(1,12,1), 4), day=1,
                                              pet1=pet.var[which(lon==12.75),which(lat==9.75),1:48],
                                              pet2=pet.var[which(lon==13.25),which(lat==9.25),1:48], 
                                              pet3=pet.var[which(lon==12.75),which(lat==9.25),1:48]))
pet_Forecariah_monthly$pet <- rowMeans(select(pet_Forecariah_monthly, pet1, pet2, pet3))
pet_Forecariah_monthly$Location <- 'Forecariah'
pet_Forecariah_monthly$date <- ymd(paste(pet_Forecariah_monthly$Year, pet_Forecariah_monthly$Month, pet_Forecariah_monthly$day, sep="-"))
pet_Forecariah_monthly_2015 <- select(pet_Forecariah_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Forecariah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Forecariah_monthly_2015a$Location <- 'Forecariah'
pet_Forecariah_monthly_2015a$date <- ymd(paste(pet_Forecariah_monthly_2015a$Year, pet_Forecariah_monthly_2015a$Month, pet_Forecariah_monthly_2015a$day, sep="-"))
pet_Forecariah_monthly_2015a <- select(pet_Forecariah_monthly_2015a, date, Year, Month, day, Location)
pet_Forecariah_monthly_2015 <- full_join(pet_Forecariah_monthly_2015a, pet_Forecariah_monthly_2015)
pet_Forecariah_monthly <- rbind(select(pet_Forecariah_monthly,date, Year, Month, day, Location, pet), pet_Forecariah_monthly_2015)
rm(pet_Forecariah_monthly_2015, pet_Forecariah_monthly_2015a)
pet_Forecariah_monthly$measurement <- "pet"
pet_Forecariah_monthly <- rename(pet_Forecariah_monthly, Value=pet)
#Fria
pet_Fria_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        pet1=pet.var[which(lon==13.75),which(lat==10.75),1:48],
                                        pet2=pet.var[which(lon==13.75),which(lat==10.25),1:48]))
pet_Fria_monthly$pet <- rowMeans(select(pet_Fria_monthly, pet1, pet2))
pet_Fria_monthly$Location <- 'Fria'
pet_Fria_monthly$date <- ymd(paste(pet_Fria_monthly$Year, pet_Fria_monthly$Month, pet_Fria_monthly$day, sep="-"))
pet_Fria_monthly_2015 <- select(pet_Fria_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Fria_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Fria_monthly_2015a$Location <- 'Fria'
pet_Fria_monthly_2015a$date <- ymd(paste(pet_Fria_monthly_2015a$Year, pet_Fria_monthly_2015a$Month, pet_Fria_monthly_2015a$day, sep="-"))
pet_Fria_monthly_2015a <- select(pet_Fria_monthly_2015a, date, Year, Month, day, Location)
pet_Fria_monthly_2015 <- full_join(pet_Fria_monthly_2015a, pet_Fria_monthly_2015)
pet_Fria_monthly <- rbind(select(pet_Fria_monthly,date, Year, Month, day, Location, pet), pet_Fria_monthly_2015)
rm(pet_Fria_monthly_2015, pet_Fria_monthly_2015a)
pet_Fria_monthly$measurement <- "pet"
pet_Fria_monthly <- rename(pet_Fria_monthly, Value=pet)
#Gaoual
pet_Gaoual_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          pet1=pet.var[which(lon==13.75),which(lat==12.25),1:48],
                                          pet2=pet.var[which(lon==13.25),which(lat==12.25),1:48], 
                                          pet3=pet.var[which(lon==13.75),which(lat==11.75),1:48],
                                          pet4=pet.var[which(lon==13.25),which(lat==11.75),1:48],
                                          pet5=pet.var[which(lon==12.75),which(lat==11.75),1:48],
                                          pet6=pet.var[which(lon==13.75),which(lat==11.25),1:48],
                                          pet7=pet.var[which(lon==13.25),which(lat==11.25),1:48]))
pet_Gaoual_monthly$pet <- rowMeans(select(pet_Gaoual_monthly, pet1, pet2, pet3, pet4, pet5, pet6, pet7))
pet_Gaoual_monthly$Location <- 'Gaoual'
pet_Gaoual_monthly$date <- ymd(paste(pet_Gaoual_monthly$Year, pet_Gaoual_monthly$Month, pet_Gaoual_monthly$day, sep="-"))
pet_Gaoual_monthly_2015 <- select(pet_Gaoual_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Gaoual_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Gaoual_monthly_2015a$Location <- 'Gaoual'
pet_Gaoual_monthly_2015a$date <- ymd(paste(pet_Gaoual_monthly_2015a$Year, pet_Gaoual_monthly_2015a$Month, pet_Gaoual_monthly_2015a$day, sep="-"))
pet_Gaoual_monthly_2015a <- select(pet_Gaoual_monthly_2015a, date, Year, Month, day, Location)
pet_Gaoual_monthly_2015 <- full_join(pet_Gaoual_monthly_2015a, pet_Gaoual_monthly_2015)
pet_Gaoual_monthly <- rbind(select(pet_Gaoual_monthly,date, Year, Month, day, Location, pet), pet_Gaoual_monthly_2015)
rm(pet_Gaoual_monthly_2015, pet_Gaoual_monthly_2015a)
pet_Gaoual_monthly$measurement <- "pet"
pet_Gaoual_monthly <- rename(pet_Gaoual_monthly, Value=pet)
#Gueckedou
pet_Gueckedou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             pet=pet.var[which(lon==10.25),which(lat==8.75),1:48]))
pet_Gueckedou_monthly$Location <- 'Gueckedou'
pet_Gueckedou_monthly$date <- ymd(paste(pet_Gueckedou_monthly$Year, pet_Gueckedou_monthly$Month, pet_Gueckedou_monthly$day, sep="-"))
pet_Gueckedou_monthly_2015 <- select(pet_Gueckedou_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Gueckedou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Gueckedou_monthly_2015a$Location <- 'Gueckedou'
pet_Gueckedou_monthly_2015a$date <- ymd(paste(pet_Gueckedou_monthly_2015a$Year, pet_Gueckedou_monthly_2015a$Month, pet_Gueckedou_monthly_2015a$day, sep="-"))
pet_Gueckedou_monthly_2015a <- select(pet_Gueckedou_monthly_2015a, date, Year, Month, day, Location)
pet_Gueckedou_monthly_2015 <- full_join(pet_Gueckedou_monthly_2015a, pet_Gueckedou_monthly_2015)
pet_Gueckedou_monthly <- rbind(select(pet_Gueckedou_monthly,date, Year, Month, day, Location, pet), pet_Gueckedou_monthly_2015)
rm(pet_Gueckedou_monthly_2015, pet_Gueckedou_monthly_2015a)
pet_Gueckedou_monthly$measurement <- "pet"
pet_Gueckedou_monthly <- rename(pet_Gueckedou_monthly, Value=pet)
#Kankan
pet_Kankan_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          pet1=pet.var[which(lon==9.25),which(lat==10.75),1:48],
                                          pet2=pet.var[which(lon==9.75),which(lat==10.25),1:48], 
                                          pet3=pet.var[which(lon==9.25),which(lat==10.25),1:48],
                                          pet4=pet.var[which(lon==8.75),which(lat==10.25),1:48],
                                          pet5=pet.var[which(lon==9.75),which(lat==9.75),1:48],
                                          pet6=pet.var[which(lon==9.25),which(lat==9.75),1:48]))
pet_Kankan_monthly$pet <- rowMeans(select(pet_Kankan_monthly, pet1, pet2, pet3, pet4, pet5, pet6))
pet_Kankan_monthly$Location <- 'Kankan'
pet_Kankan_monthly$date <- ymd(paste(pet_Kankan_monthly$Year, pet_Kankan_monthly$Month, pet_Kankan_monthly$day, sep="-"))
pet_Kankan_monthly_2015 <- select(pet_Kankan_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Kankan_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Kankan_monthly_2015a$Location <- 'Kankan'
pet_Kankan_monthly_2015a$date <- ymd(paste(pet_Kankan_monthly_2015a$Year, pet_Kankan_monthly_2015a$Month, pet_Kankan_monthly_2015a$day, sep="-"))
pet_Kankan_monthly_2015a <- select(pet_Kankan_monthly_2015a, date, Year, Month, day, Location)
pet_Kankan_monthly_2015 <- full_join(pet_Kankan_monthly_2015a, pet_Kankan_monthly_2015)
pet_Kankan_monthly <- rbind(select(pet_Kankan_monthly,date, Year, Month, day, Location, pet), pet_Kankan_monthly_2015)
rm(pet_Kankan_monthly_2015, pet_Kankan_monthly_2015a)
pet_Kankan_monthly$measurement <- "pet"
pet_Kankan_monthly <- rename(pet_Kankan_monthly, Value=pet)
#Kerouane
pet_Kerouane_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            pet1=pet.var[which(lon==9.25),which(lat==9.75),1:48],
                                            pet2=pet.var[which(lon==8.75),which(lat==9.75),1:48], 
                                            pet3=pet.var[which(lon==9.75),which(lat==9.25),1:48],
                                            pet4=pet.var[which(lon==9.25),which(lat==9.25),1:48],
                                            pet5=pet.var[which(lon==8.75),which(lat==9.25),1:48],
                                            pet6=pet.var[which(lon==9.25),which(lat==8.75),1:48]))
pet_Kerouane_monthly$pet <- rowMeans(select(pet_Kerouane_monthly, pet1, pet2, pet3, pet4, pet5, pet6))
pet_Kerouane_monthly$Location <- 'Kerouane'
pet_Kerouane_monthly$date <- ymd(paste(pet_Kerouane_monthly$Year, pet_Kerouane_monthly$Month, pet_Kerouane_monthly$day, sep="-"))
pet_Kerouane_monthly_2015 <- select(pet_Kerouane_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Kerouane_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Kerouane_monthly_2015a$Location <- 'Kerouane'
pet_Kerouane_monthly_2015a$date <- ymd(paste(pet_Kerouane_monthly_2015a$Year, pet_Kerouane_monthly_2015a$Month, pet_Kerouane_monthly_2015a$day, sep="-"))
pet_Kerouane_monthly_2015a <- select(pet_Kerouane_monthly_2015a, date, Year, Month, day, Location)
pet_Kerouane_monthly_2015 <- full_join(pet_Kerouane_monthly_2015a, pet_Kerouane_monthly_2015)
pet_Kerouane_monthly <- rbind(select(pet_Kerouane_monthly,date, Year, Month, day, Location, pet), pet_Kerouane_monthly_2015)
rm(pet_Kerouane_monthly_2015, pet_Kerouane_monthly_2015a)
pet_Kerouane_monthly$measurement <- "pet"
pet_Kerouane_monthly <- rename(pet_Kerouane_monthly, Value=pet)
#Kindia
pet_Kindia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          pet1=pet.var[which(lon==13.25),which(lat==10.25),1:48],
                                          pet2=pet.var[which(lon==12.75),which(lat==10.25),1:48], 
                                          pet3=pet.var[which(lon==12.25),which(lat==10.25),1:48],
                                          pet4=pet.var[which(lon==12.75),which(lat==9.75),1:48]))
pet_Kindia_monthly$pet <- rowMeans(select(pet_Kindia_monthly, pet1, pet2, pet3, pet4))
pet_Kindia_monthly$Location <- 'Kindia'
pet_Kindia_monthly$date <- ymd(paste(pet_Kindia_monthly$Year, pet_Kindia_monthly$Month, pet_Kindia_monthly$day, sep="-"))
pet_Kindia_monthly_2015 <- select(pet_Kindia_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Kindia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Kindia_monthly_2015a$Location <- 'Kindia'
pet_Kindia_monthly_2015a$date <- ymd(paste(pet_Kindia_monthly_2015a$Year, pet_Kindia_monthly_2015a$Month, pet_Kindia_monthly_2015a$day, sep="-"))
pet_Kindia_monthly_2015a <- select(pet_Kindia_monthly_2015a, date, Year, Month, day, Location)
pet_Kindia_monthly_2015 <- full_join(pet_Kindia_monthly_2015a, pet_Kindia_monthly_2015)
pet_Kindia_monthly <- rbind(select(pet_Kindia_monthly,date, Year, Month, day, Location, pet), pet_Kindia_monthly_2015)
rm(pet_Kindia_monthly_2015, pet_Kindia_monthly_2015a)
pet_Kindia_monthly$measurement <- "pet"
pet_Kindia_monthly <- rename(pet_Kindia_monthly, Value=pet)
#KISSIDOUGOU
pet_Kissidougou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               pet1=pet.var[which(lon==10.25),which(lat==9.75),1:48],
                                               pet2=pet.var[which(lon==10.25),which(lat==9.25),1:48], 
                                               pet3=pet.var[which(lon==9.75),which(lat==9.25),1:48]))
pet_Kissidougou_monthly$pet <- rowMeans(select(pet_Kissidougou_monthly, pet1, pet2, pet3))
pet_Kissidougou_monthly$Location <- 'Kissidougou'
pet_Kissidougou_monthly$date <- ymd(paste(pet_Kissidougou_monthly$Year, pet_Kissidougou_monthly$Month, pet_Kissidougou_monthly$day, sep="-"))
pet_Kissidougou_monthly_2015 <- select(pet_Kissidougou_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Kissidougou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Kissidougou_monthly_2015a$Location <- 'Kissidougou'
pet_Kissidougou_monthly_2015a$date <- ymd(paste(pet_Kissidougou_monthly_2015a$Year, pet_Kissidougou_monthly_2015a$Month, pet_Kissidougou_monthly_2015a$day, sep="-"))
pet_Kissidougou_monthly_2015a <- select(pet_Kissidougou_monthly_2015a, date, Year, Month, day, Location)
pet_Kissidougou_monthly_2015 <- full_join(pet_Kissidougou_monthly_2015a, pet_Kissidougou_monthly_2015)
pet_Kissidougou_monthly <- rbind(select(pet_Kissidougou_monthly,date, Year, Month, day, Location, pet), pet_Kissidougou_monthly_2015)
rm(pet_Kissidougou_monthly_2015, pet_Kissidougou_monthly_2015a)
pet_Kissidougou_monthly$measurement <- "pet"
pet_Kissidougou_monthly <- rename(pet_Kissidougou_monthly, Value=pet)
#Koubia
pet_Koubia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          pet=pet.var[which(lon==10.25),which(lat==9.75),1:48]))
pet_Koubia_monthly$Location <- 'Koubia'
pet_Koubia_monthly$date <- ymd(paste(pet_Koubia_monthly$Year, pet_Koubia_monthly$Month, pet_Koubia_monthly$day, sep="-"))
pet_Koubia_monthly_2015 <- select(pet_Koubia_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Koubia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Koubia_monthly_2015a$Location <- 'Koubia'
pet_Koubia_monthly_2015a$date <- ymd(paste(pet_Koubia_monthly_2015a$Year, pet_Koubia_monthly_2015a$Month, pet_Koubia_monthly_2015a$day, sep="-"))
pet_Koubia_monthly_2015a <- select(pet_Koubia_monthly_2015a, date, Year, Month, day, Location)
pet_Koubia_monthly_2015 <- full_join(pet_Koubia_monthly_2015a, pet_Koubia_monthly_2015)
pet_Koubia_monthly <- rbind(select(pet_Koubia_monthly,date, Year, Month, day, Location, pet), pet_Koubia_monthly_2015)
rm(pet_Koubia_monthly_2015, pet_Koubia_monthly_2015a)
pet_Koubia_monthly$measurement <- "pet"
pet_Koubia_monthly <- rename(pet_Koubia_monthly, Value=pet)
#Koundara
pet_Koundara_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            pet1=pet.var[which(lon==13.25),which(lat==12.75),1:48],
                                            pet2=pet.var[which(lon==13.25),which(lat==12.25),1:48], 
                                            pet3=pet.var[which(lon==12.75),which(lat==12.25),1:48]))
pet_Koundara_monthly$pet <- rowMeans(select(pet_Koundara_monthly, pet1, pet2, pet3))
pet_Koundara_monthly$Location <- 'Koundara'
pet_Koundara_monthly$date <- ymd(paste(pet_Koundara_monthly$Year, pet_Koundara_monthly$Month, pet_Koundara_monthly$day, sep="-"))
pet_Koundara_monthly_2015 <- select(pet_Koundara_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Koundara_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Koundara_monthly_2015a$Location <- 'Koundara'
pet_Koundara_monthly_2015a$date <- ymd(paste(pet_Koundara_monthly_2015a$Year, pet_Koundara_monthly_2015a$Month, pet_Koundara_monthly_2015a$day, sep="-"))
pet_Koundara_monthly_2015a <- select(pet_Koundara_monthly_2015a, date, Year, Month, day, Location)
pet_Koundara_monthly_2015 <- full_join(pet_Koundara_monthly_2015a, pet_Koundara_monthly_2015)
pet_Koundara_monthly <- rbind(select(pet_Koundara_monthly,date, Year, Month, day, Location, pet), pet_Koundara_monthly_2015)
rm(pet_Koundara_monthly_2015, pet_Koundara_monthly_2015a)
pet_Koundara_monthly$measurement <- "pet"
pet_Koundara_monthly <- rename(pet_Koundara_monthly, Value=pet)
#Kouroussa
pet_Kouroussa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             pet1=pet.var[which(lon==10.25),which(lat==11.25),1:48],
                                             pet2=pet.var[which(lon==10.75),which(lat==10.75),1:48], 
                                             pet3=pet.var[which(lon==10.25),which(lat==10.75),1:48],
                                             pet4=pet.var[which(lon==9.75),which(lat==10.75),1:48],
                                             pet5=pet.var[which(lon==10.25),which(lat==10.25),1:48],
                                             pet6=pet.var[which(lon==9.75),which(lat==10.25),1:48],
                                             pet7=pet.var[which(lon==10.25),which(lat==9.75),1:48],
                                             pet8=pet.var[which(lon==9.75),which(lat==9.75),1:48]))
pet_Kouroussa_monthly$pet <- rowMeans(select(pet_Kouroussa_monthly, pet1, pet2, pet3, pet4, pet5, pet6, pet7, pet8))
pet_Kouroussa_monthly$Location <- 'Kouroussa'
pet_Kouroussa_monthly$date <- ymd(paste(pet_Kouroussa_monthly$Year, pet_Kouroussa_monthly$Month, pet_Kouroussa_monthly$day, sep="-"))
pet_Kouroussa_monthly_2015 <- select(pet_Kouroussa_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Kouroussa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Kouroussa_monthly_2015a$Location <- 'Kouroussa'
pet_Kouroussa_monthly_2015a$date <- ymd(paste(pet_Kouroussa_monthly_2015a$Year, pet_Kouroussa_monthly_2015a$Month, pet_Kouroussa_monthly_2015a$day, sep="-"))
pet_Kouroussa_monthly_2015a <- select(pet_Kouroussa_monthly_2015a, date, Year, Month, day, Location)
pet_Kouroussa_monthly_2015 <- full_join(pet_Kouroussa_monthly_2015a, pet_Kouroussa_monthly_2015)
pet_Kouroussa_monthly <- rbind(select(pet_Kouroussa_monthly,date, Year, Month, day, Location, pet), pet_Kouroussa_monthly_2015)
rm(pet_Kouroussa_monthly_2015, pet_Kouroussa_monthly_2015a)
pet_Kouroussa_monthly$measurement <- "pet"
pet_Kouroussa_monthly <- rename(pet_Kouroussa_monthly, Value=pet)
#Labe
pet_Labe_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        pet1=pet.var[which(lon==12.25),which(lat==11.75),1:48],
                                        pet2=pet.var[which(lon==12.25),which(lat==11.25),1:48]))
pet_Labe_monthly$pet <- rowMeans(select(pet_Labe_monthly, pet1, pet2))
pet_Labe_monthly$Location <- 'Labe'
pet_Labe_monthly$date <- ymd(paste(pet_Labe_monthly$Year, pet_Labe_monthly$Month, pet_Labe_monthly$day, sep="-"))
pet_Labe_monthly_2015 <- select(pet_Labe_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Labe_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Labe_monthly_2015a$Location <- 'Labe'
pet_Labe_monthly_2015a$date <- ymd(paste(pet_Labe_monthly_2015a$Year, pet_Labe_monthly_2015a$Month, pet_Labe_monthly_2015a$day, sep="-"))
pet_Labe_monthly_2015a <- select(pet_Labe_monthly_2015a, date, Year, Month, day, Location)
pet_Labe_monthly_2015 <- full_join(pet_Labe_monthly_2015a, pet_Labe_monthly_2015)
pet_Labe_monthly <- rbind(select(pet_Labe_monthly,date, Year, Month, day, Location, pet), pet_Labe_monthly_2015)
rm(pet_Labe_monthly_2015, pet_Labe_monthly_2015a)
pet_Labe_monthly$measurement <- "pet"
pet_Labe_monthly <- rename(pet_Labe_monthly, Value=pet)
#Lelouma
pet_Lelouma_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           pet1=pet.var[which(lon==12.75),which(lat==11.75),1:48],
                                           pet2=pet.var[which(lon==12.75),which(lat==11.25),1:48]))
pet_Lelouma_monthly$pet <- rowMeans(select(pet_Lelouma_monthly, pet1, pet2))
pet_Lelouma_monthly$Location <- 'Lelouma'
pet_Lelouma_monthly$date <- ymd(paste(pet_Lelouma_monthly$Year, pet_Lelouma_monthly$Month, pet_Lelouma_monthly$day, sep="-"))
pet_Lelouma_monthly_2015 <- select(pet_Lelouma_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Lelouma_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Lelouma_monthly_2015a$Location <- 'Lelouma'
pet_Lelouma_monthly_2015a$date <- ymd(paste(pet_Lelouma_monthly_2015a$Year, pet_Lelouma_monthly_2015a$Month, pet_Lelouma_monthly_2015a$day, sep="-"))
pet_Lelouma_monthly_2015a <- select(pet_Lelouma_monthly_2015a, date, Year, Month, day, Location)
pet_Lelouma_monthly_2015 <- full_join(pet_Lelouma_monthly_2015a, pet_Lelouma_monthly_2015)
pet_Lelouma_monthly <- rbind(select(pet_Lelouma_monthly,date, Year, Month, day, Location, pet), pet_Lelouma_monthly_2015)
rm(pet_Lelouma_monthly_2015, pet_Lelouma_monthly_2015a)
pet_Lelouma_monthly$measurement <- "pet"
pet_Lelouma_monthly <- rename(pet_Lelouma_monthly, Value=pet)
#Lola
pet_Lola_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        pet1=pet.var[which(lon==8.25),which(lat==8.25),1:48],
                                        pet2=pet.var[which(lon==8.25),which(lat==7.75),1:48]))
pet_Lola_monthly$pet <- rowMeans(select(pet_Lola_monthly, pet1, pet2))
pet_Lola_monthly$Location <- 'Lola'
pet_Lola_monthly$date <- ymd(paste(pet_Lola_monthly$Year, pet_Lola_monthly$Month, pet_Lola_monthly$day, sep="-"))
pet_Lola_monthly_2015 <- select(pet_Lola_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Lola_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Lola_monthly_2015a$Location <- 'Lola'
pet_Lola_monthly_2015a$date <- ymd(paste(pet_Lola_monthly_2015a$Year, pet_Lola_monthly_2015a$Month, pet_Lola_monthly_2015a$day, sep="-"))
pet_Lola_monthly_2015a <- select(pet_Lola_monthly_2015a, date, Year, Month, day, Location)
pet_Lola_monthly_2015 <- full_join(pet_Lola_monthly_2015a, pet_Lola_monthly_2015)
pet_Lola_monthly <- rbind(select(pet_Lola_monthly,date, Year, Month, day, Location, pet), pet_Lola_monthly_2015)
rm(pet_Lola_monthly_2015, pet_Lola_monthly_2015a)
pet_Lola_monthly$measurement <- "pet"
pet_Lola_monthly <- rename(pet_Lola_monthly, Value=pet)
#Macenta
pet_Macenta_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           pet1=pet.var[which(lon==9.75),which(lat==8.75),1:48],
                                           pet2=pet.var[which(lon==9.25),which(lat==8.75),1:48],
                                           pet3=pet.var[which(lon==9.25),which(lat==8.25),1:48]))
pet_Macenta_monthly$pet <- rowMeans(select(pet_Macenta_monthly, pet1, pet2, pet3))
pet_Macenta_monthly$Location <- 'Macenta'
pet_Macenta_monthly$date <- ymd(paste(pet_Macenta_monthly$Year, pet_Macenta_monthly$Month, pet_Macenta_monthly$day, sep="-"))
pet_Macenta_monthly_2015 <- select(pet_Macenta_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Macenta_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Macenta_monthly_2015a$Location <- 'Macenta'
pet_Macenta_monthly_2015a$date <- ymd(paste(pet_Macenta_monthly_2015a$Year, pet_Macenta_monthly_2015a$Month, pet_Macenta_monthly_2015a$day, sep="-"))
pet_Macenta_monthly_2015a <- select(pet_Macenta_monthly_2015a, date, Year, Month, day, Location)
pet_Macenta_monthly_2015 <- full_join(pet_Macenta_monthly_2015a, pet_Macenta_monthly_2015)
pet_Macenta_monthly <- rbind(select(pet_Macenta_monthly,date, Year, Month, day, Location, pet), pet_Macenta_monthly_2015)
rm(pet_Macenta_monthly_2015, pet_Macenta_monthly_2015a)
pet_Macenta_monthly$measurement <- "pet"
pet_Macenta_monthly <- rename(pet_Macenta_monthly, Value=pet)
#Mali
pet_Mali_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        pet1=pet.var[which(lon==12.75),which(lat==12.25),1:48],
                                        pet2=pet.var[which(lon==12.25),which(lat==12.25),1:48], 
                                        pet3=pet.var[which(lon==11.75),which(lat==12.25),1:48],
                                        pet4=pet.var[which(lon==12.75),which(lat==11.75),1:48],
                                        pet5=pet.var[which(lon==12.25),which(lat==11.75),1:48]))
pet_Mali_monthly$pet <- rowMeans(select(pet_Mali_monthly, pet1, pet2, pet3, pet4, pet5))
pet_Mali_monthly$Location <- 'Mali'
pet_Mali_monthly$date <- ymd(paste(pet_Mali_monthly$Year, pet_Mali_monthly$Month, pet_Mali_monthly$day, sep="-"))
pet_Mali_monthly_2015 <- select(pet_Mali_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Mali_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Mali_monthly_2015a$Location <- 'Mali'
pet_Mali_monthly_2015a$date <- ymd(paste(pet_Mali_monthly_2015a$Year, pet_Mali_monthly_2015a$Month, pet_Mali_monthly_2015a$day, sep="-"))
pet_Mali_monthly_2015a <- select(pet_Mali_monthly_2015a, date, Year, Month, day, Location)
pet_Mali_monthly_2015 <- full_join(pet_Mali_monthly_2015a, pet_Mali_monthly_2015)
pet_Mali_monthly <- rbind(select(pet_Mali_monthly,date, Year, Month, day, Location, pet), pet_Mali_monthly_2015)
rm(pet_Mali_monthly_2015, pet_Mali_monthly_2015a)
pet_Mali_monthly$measurement <- "pet"
pet_Mali_monthly <- rename(pet_Mali_monthly, Value=pet)
#Mamou
pet_Mamou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         pet1=pet.var[which(lon==11.75),which(lat==10.75),1:48],
                                         pet2=pet.var[which(lon==12.25),which(lat==10.25),1:48],
                                         pet3=pet.var[which(lon==11.75),which(lat==10.25),1:48]))
pet_Mamou_monthly$pet <- rowMeans(select(pet_Mamou_monthly, pet1, pet2, pet3))
pet_Mamou_monthly$Location <- 'Mamou'
pet_Mamou_monthly$date <- ymd(paste(pet_Mamou_monthly$Year, pet_Mamou_monthly$Month, pet_Mamou_monthly$day, sep="-"))
pet_Mamou_monthly_2015 <- select(pet_Mamou_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Mamou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Mamou_monthly_2015a$Location <- 'Mamou'
pet_Mamou_monthly_2015a$date <- ymd(paste(pet_Mamou_monthly_2015a$Year, pet_Mamou_monthly_2015a$Month, pet_Mamou_monthly_2015a$day, sep="-"))
pet_Mamou_monthly_2015a <- select(pet_Mamou_monthly_2015a, date, Year, Month, day, Location)
pet_Mamou_monthly_2015 <- full_join(pet_Mamou_monthly_2015a, pet_Mamou_monthly_2015)
pet_Mamou_monthly <- rbind(select(pet_Mamou_monthly,date, Year, Month, day, Location, pet), pet_Mamou_monthly_2015)
rm(pet_Mamou_monthly_2015, pet_Mamou_monthly_2015a)
pet_Mamou_monthly$measurement <- "pet"
pet_Mamou_monthly <- rename(pet_Mamou_monthly, Value=pet)
#Nzerekore
pet_Nzerekore_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             pet1=pet.var[which(lon==8.75),which(lat==8.25),1:48],
                                             pet2=pet.var[which(lon==8.75),which(lat==7.75),1:48]))
pet_Nzerekore_monthly$pet <- rowMeans(select(pet_Nzerekore_monthly, pet1, pet2))
pet_Nzerekore_monthly$Location <- 'Nzerekore'
pet_Nzerekore_monthly$date <- ymd(paste(pet_Nzerekore_monthly$Year, pet_Nzerekore_monthly$Month, pet_Nzerekore_monthly$day, sep="-"))
pet_Nzerekore_monthly_2015 <- select(pet_Nzerekore_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Nzerekore_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Nzerekore_monthly_2015a$Location <- 'Nzerekore'
pet_Nzerekore_monthly_2015a$date <- ymd(paste(pet_Nzerekore_monthly_2015a$Year, pet_Nzerekore_monthly_2015a$Month, pet_Nzerekore_monthly_2015a$day, sep="-"))
pet_Nzerekore_monthly_2015a <- select(pet_Nzerekore_monthly_2015a, date, Year, Month, day, Location)
pet_Nzerekore_monthly_2015 <- full_join(pet_Nzerekore_monthly_2015a, pet_Nzerekore_monthly_2015)
pet_Nzerekore_monthly <- rbind(select(pet_Nzerekore_monthly,date, Year, Month, day, Location, pet), pet_Nzerekore_monthly_2015)
rm(pet_Nzerekore_monthly_2015, pet_Nzerekore_monthly_2015a)
pet_Nzerekore_monthly$measurement <- "pet"
pet_Nzerekore_monthly <- rename(pet_Nzerekore_monthly, Value=pet)
#Pita
pet_Pita_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        pet1=pet.var[which(lon==12.75),which(lat==11.25),1:48],
                                        pet2=pet.var[which(lon==12.25),which(lat==11.25),1:48],
                                        pet3=pet.var[which(lon==12.75),which(lat==10.75),1:48]))
pet_Pita_monthly$pet <- rowMeans(select(pet_Pita_monthly, pet1, pet2, pet3))
pet_Pita_monthly$Location <- 'Pita'
pet_Pita_monthly$date <- ymd(paste(pet_Pita_monthly$Year, pet_Pita_monthly$Month, pet_Pita_monthly$day, sep="-"))
pet_Pita_monthly_2015 <- select(pet_Pita_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Pita_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Pita_monthly_2015a$Location <- 'Pita'
pet_Pita_monthly_2015a$date <- ymd(paste(pet_Pita_monthly_2015a$Year, pet_Pita_monthly_2015a$Month, pet_Pita_monthly_2015a$day, sep="-"))
pet_Pita_monthly_2015a <- select(pet_Pita_monthly_2015a, date, Year, Month, day, Location)
pet_Pita_monthly_2015 <- full_join(pet_Pita_monthly_2015a, pet_Pita_monthly_2015)
pet_Pita_monthly <- rbind(select(pet_Pita_monthly,date, Year, Month, day, Location, pet), pet_Pita_monthly_2015)
rm(pet_Pita_monthly_2015, pet_Pita_monthly_2015a)
pet_Pita_monthly$measurement <- "pet"
pet_Pita_monthly <- rename(pet_Pita_monthly, Value=pet)
#Siguiri
pet_Siguiri_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           pet1=pet.var[which(lon==9.25),which(lat==12.25),1:48],
                                           pet2=pet.var[which(lon==10.25),which(lat==11.75),1:48], 
                                           pet3=pet.var[which(lon==9.75),which(lat==11.75),1:48],
                                           pet4=pet.var[which(lon==9.25),which(lat==11.75),1:48],
                                           pet5=pet.var[which(lon==9.75),which(lat==11.25),1:48],
                                           pet6=pet.var[which(lon==9.25),which(lat==11.25),1:48]))
pet_Siguiri_monthly$pet <- rowMeans(select(pet_Siguiri_monthly, pet1, pet2, pet3, pet4, pet5, pet6))
pet_Siguiri_monthly$Location <- 'Siguiri'
pet_Siguiri_monthly$date <- ymd(paste(pet_Siguiri_monthly$Year, pet_Siguiri_monthly$Month, pet_Siguiri_monthly$day, sep="-"))
pet_Siguiri_monthly_2015 <- select(pet_Siguiri_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Siguiri_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Siguiri_monthly_2015a$Location <- 'Siguiri'
pet_Siguiri_monthly_2015a$date <- ymd(paste(pet_Siguiri_monthly_2015a$Year, pet_Siguiri_monthly_2015a$Month, pet_Siguiri_monthly_2015a$day, sep="-"))
pet_Siguiri_monthly_2015a <- select(pet_Siguiri_monthly_2015a, date, Year, Month, day, Location)
pet_Siguiri_monthly_2015 <- full_join(pet_Siguiri_monthly_2015a, pet_Siguiri_monthly_2015)
pet_Siguiri_monthly <- rbind(select(pet_Siguiri_monthly,date, Year, Month, day, Location, pet), pet_Siguiri_monthly_2015)
rm(pet_Siguiri_monthly_2015, pet_Siguiri_monthly_2015a)
pet_Siguiri_monthly$measurement <- "pet"
pet_Siguiri_monthly <- rename(pet_Siguiri_monthly, Value=pet)
#Telimele
pet_Telimele_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            pet1=pet.var[which(lon==13.75),which(lat==11.25),1:48],
                                            pet2=pet.var[which(lon==13.25),which(lat==11.25),1:48], 
                                            pet3=pet.var[which(lon==13.75),which(lat==10.75),1:48],
                                            pet4=pet.var[which(lon==13.25),which(lat==10.75),1:48]))
pet_Telimele_monthly$pet <- rowMeans(select(pet_Telimele_monthly, pet1, pet2, pet3, pet4))
pet_Telimele_monthly$Location <- 'Telimele'
pet_Telimele_monthly$date <- ymd(paste(pet_Telimele_monthly$Year, pet_Telimele_monthly$Month, pet_Telimele_monthly$day, sep="-"))
pet_Telimele_monthly_2015 <- select(pet_Telimele_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Telimele_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Telimele_monthly_2015a$Location <- 'Telimele'
pet_Telimele_monthly_2015a$date <- ymd(paste(pet_Telimele_monthly_2015a$Year, pet_Telimele_monthly_2015a$Month, pet_Telimele_monthly_2015a$day, sep="-"))
pet_Telimele_monthly_2015a <- select(pet_Telimele_monthly_2015a, date, Year, Month, day, Location)
pet_Telimele_monthly_2015 <- full_join(pet_Telimele_monthly_2015a, pet_Telimele_monthly_2015)
pet_Telimele_monthly <- rbind(select(pet_Telimele_monthly,date, Year, Month, day, Location, pet), pet_Telimele_monthly_2015)
rm(pet_Telimele_monthly_2015, pet_Telimele_monthly_2015a)
pet_Telimele_monthly$measurement <- "pet"
pet_Telimele_monthly <- rename(pet_Telimele_monthly, Value=pet)
#Tougue
pet_Tougue_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4),day=1, 
                                          pet1=pet.var[which(lon==11.25),which(lat==11.75),1:48],
                                          pet2=pet.var[which(lon==11.75),which(lat==11.25),1:48]))
pet_Tougue_monthly$pet <- rowMeans(select(pet_Tougue_monthly, pet1, pet2))
pet_Tougue_monthly$Location <- 'Tougue'
pet_Tougue_monthly$date <- ymd(paste(pet_Tougue_monthly$Year, pet_Tougue_monthly$Month, pet_Tougue_monthly$day, sep="-"))
pet_Tougue_monthly_2015 <- select(pet_Tougue_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_Tougue_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_Tougue_monthly_2015a$Location <- 'Tougue'
pet_Tougue_monthly_2015a$date <- ymd(paste(pet_Tougue_monthly_2015a$Year, pet_Tougue_monthly_2015a$Month, pet_Tougue_monthly_2015a$day, sep="-"))
pet_Tougue_monthly_2015a <- select(pet_Tougue_monthly_2015a, date, Year, Month, day, Location)
pet_Tougue_monthly_2015 <- full_join(pet_Tougue_monthly_2015a, pet_Tougue_monthly_2015)
pet_Tougue_monthly <- rbind(select(pet_Tougue_monthly,date, Year, Month, day, Location, pet), pet_Tougue_monthly_2015)
rm(pet_Tougue_monthly_2015, pet_Tougue_monthly_2015a)
pet_Tougue_monthly$measurement <- "pet"
pet_Tougue_monthly <- rename(pet_Tougue_monthly, Value=pet)
#yamou
pet_yamou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4),day=1, 
                                         pet1=pet.var[which(lon==9.25),which(lat==7.75),1:48],
                                         pet2=pet.var[which(lon==9.25),which(lat==7.25),1:48],
                                         pet3=pet.var[which(lon==8.75),which(lat==7.25),1:48]))
pet_yamou_monthly$Location <- 'yamou'
pet_yamou_monthly$pet <- rowMeans(select(pet_yamou_monthly, pet1, pet2, pet3))
pet_yamou_monthly$date <- ymd(paste(pet_yamou_monthly$Year, pet_yamou_monthly$Month, pet_yamou_monthly$day, sep="-"))
pet_yamou_monthly_2015 <- select(pet_yamou_monthly, Location, Year, Month, pet) %>% group_by( Month) %>% summarize(pet=mean(pet))
pet_yamou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pet_yamou_monthly_2015a$Location <- 'yamou'
pet_yamou_monthly_2015a$date <- ymd(paste(pet_yamou_monthly_2015a$Year, pet_yamou_monthly_2015a$Month, pet_yamou_monthly_2015a$day, sep="-"))
pet_yamou_monthly_2015a <- select(pet_yamou_monthly_2015a, date, Year, Month, day, Location)
pet_yamou_monthly_2015 <- full_join(pet_yamou_monthly_2015a, pet_yamou_monthly_2015)
pet_yamou_monthly <- rbind(select(pet_yamou_monthly,date, Year, Month, day, Location, pet), pet_yamou_monthly_2015)
rm(pet_yamou_monthly_2015, pet_yamou_monthly_2015a)
pet_yamou_monthly$measurement <- "pet"
pet_yamou_monthly <- rename(pet_yamou_monthly, Value=pet)

#Merging in long format
pet_Guinea_monthly_district <- rbind(pet_Beyla_monthly, pet_Boke_monthly, pet_Boffa_monthly,
                                     pet_Conakry_monthly, pet_Coyah_monthly, pet_Dabola_monthly, pet_Dalaba_monthly,
                                     pet_Dinguiray_monthly, pet_Dubreka_monthly, pet_Faranah_monthly,
                                     pet_Forecariah_monthly, pet_Fria_monthly, pet_Gaoual_monthly,
                                     pet_Gueckedou_monthly, pet_Kankan_monthly, pet_Kerouane_monthly,
                                     pet_Kindia_monthly, pet_Kissidougou_monthly, pet_Koubia_monthly,
                                     pet_Koundara_monthly, pet_Kouroussa_monthly, pet_Labe_monthly,
                                     pet_Lelouma_monthly, pet_Lola_monthly, pet_Macenta_monthly,
                                     pet_Mali_monthly, pet_Mamou_monthly, pet_Nzerekore_monthly,
                                     pet_Pita_monthly, pet_Siguiri_monthly, pet_Telimele_monthly,
                                     pet_Tougue_monthly, pet_yamou_monthly)

#####################
#pre - precipitation in mm
#####################
pre.full <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.pre.dat.nc', write=FALSE)
pre.var <- var.get.nc(pre.full, "pre")
lon <- var.get.nc(pre.full, "lon")
lat <- var.get.nc(pre.full, "lat")
#Beyla
pre_Beyla_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         pre1=pre.var[which(lon==8.75),which(lat==9.25),1:48],
                                         pre2=pre.var[which(lon==8.25),which(lat==9.25),1:48], 
                                         pre3=pre.var[which(lon==7.75),which(lat==9.25),1:48],
                                         pre4=pre.var[which(lon==8.75),which(lat==8.75),1:48], 
                                         pre5=pre.var[which(lon==8.25),which(lat==8.75),1:48],
                                         pre6=pre.var[which(lon==7.75),which(lat==8.75),1:48], 
                                         pre7=pre.var[which(lon==8.75),which(lat==8.25),1:48],
                                         pre8=pre.var[which(lon==8.25),which(lat==8.25),1:48]))
pre_Beyla_monthly$pre <- rowMeans(select(pre_Beyla_monthly, pre1, pre2, pre3, pre4, pre5, pre6, pre7, pre8))
pre_Beyla_monthly$Location <- 'Beyla'
pre_Beyla_monthly$date <- ymd(paste(pre_Beyla_monthly$Year, pre_Beyla_monthly$Month, pre_Beyla_monthly$day, sep="-"))
pre_Beyla_monthly_2015 <- select(pre_Beyla_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Beyla_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Beyla_monthly_2015a$Location <- 'Beyla'
pre_Beyla_monthly_2015a$date <- ymd(paste(pre_Beyla_monthly_2015a$Year, pre_Beyla_monthly_2015a$Month, pre_Beyla_monthly_2015a$day, sep="-"))
pre_Beyla_monthly_2015a <- select(pre_Beyla_monthly_2015a, date, Year, Month, day, Location)
pre_Beyla_monthly_2015 <- full_join(pre_Beyla_monthly_2015a, pre_Beyla_monthly_2015)
pre_Beyla_monthly <- rbind(select(pre_Beyla_monthly,date, Year, Month, day, Location, pre), pre_Beyla_monthly_2015)
rm(pre_Beyla_monthly_2015, pre_Beyla_monthly_2015a)
pre_Beyla_monthly$measurement <- "pre"
pre_Beyla_monthly <- rename(pre_Beyla_monthly, Value=pre)
#Boffa
pre_Boffa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         pre1=pre.var[which(lon==14.25),which(lat==10.75),1:48],
                                         pre2=pre.var[which(lon==13.75),which(lat==10.75),1:48], 
                                         pre3=pre.var[which(lon==14.25),which(lat==10.25),1:48],
                                         pre4=pre.var[which(lon==13.75),which(lat==10.25),1:48]))
pre_Boffa_monthly$pre <- rowMeans(select(pre_Boffa_monthly, pre1, pre2, pre3, pre4))
pre_Boffa_monthly$Location <- 'Boffa'
pre_Boffa_monthly$date <- ymd(paste(pre_Boffa_monthly$Year, pre_Boffa_monthly$Month, pre_Boffa_monthly$day, sep="-"))
pre_Boffa_monthly_2015 <- select(pre_Boffa_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Boffa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Boffa_monthly_2015a$Location <- 'Boffa'
pre_Boffa_monthly_2015a$date <- ymd(paste(pre_Boffa_monthly_2015a$Year, pre_Boffa_monthly_2015a$Month, pre_Boffa_monthly_2015a$day, sep="-"))
pre_Boffa_monthly_2015a <- select(pre_Boffa_monthly_2015a, date, Year, Month, day, Location)
pre_Boffa_monthly_2015 <- full_join(pre_Boffa_monthly_2015a, pre_Boffa_monthly_2015)
pre_Boffa_monthly <- rbind(select(pre_Boffa_monthly,date, Year, Month, day, Location, pre), pre_Boffa_monthly_2015)
rm(pre_Boffa_monthly_2015, pre_Boffa_monthly_2015a)
pre_Boffa_monthly$measurement <- "pre"
pre_Boffa_monthly <- rename(pre_Boffa_monthly, Value=pre)
#Boke
pre_Boke_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        pre1=pre.var[which(lon==14.25),which(lat==11.75),1:48],
                                        pre2=pre.var[which(lon==14.75),which(lat==11.25),1:48], 
                                        pre3=pre.var[which(lon==14.25),which(lat==11.25),1:48],
                                        pre4=pre.var[which(lon==13.75),which(lat==11.25),1:48]))
pre_Boke_monthly$pre <- rowMeans(select(pre_Boke_monthly, pre1, pre2, pre3, pre4))
pre_Boke_monthly$Location <- 'Boke'
pre_Boke_monthly$date <- ymd(paste(pre_Boke_monthly$Year, pre_Boke_monthly$Month, pre_Boke_monthly$day, sep="-"))
pre_Boke_monthly_2015 <- select(pre_Boke_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Boke_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Boke_monthly_2015a$Location <- 'Boke'
pre_Boke_monthly_2015a$date <- ymd(paste(pre_Boke_monthly_2015a$Year, pre_Boke_monthly_2015a$Month, pre_Boke_monthly_2015a$day, sep="-"))
pre_Boke_monthly_2015a <- select(pre_Boke_monthly_2015a, date, Year, Month, day, Location)
pre_Boke_monthly_2015 <- full_join(pre_Boke_monthly_2015a, pre_Boke_monthly_2015)
pre_Boke_monthly <- rbind(select(pre_Boke_monthly,date, Year, Month, day, Location, pre), pre_Boke_monthly_2015)
rm(pre_Boke_monthly_2015, pre_Boke_monthly_2015a)
pre_Boke_monthly$measurement <- "pre"
pre_Boke_monthly <- rename(pre_Boke_monthly, Value=pre)
#Conakry
pre_Conakry_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           pre=pre.var[which(lon==13.75),which(lat==9.75),1:48]))
pre_Conakry_monthly$Location <- 'Conakry'
pre_Conakry_monthly$date <- ymd(paste(pre_Conakry_monthly$Year, pre_Conakry_monthly$Month, pre_Conakry_monthly$day, sep="-"))
pre_Conakry_monthly_2015 <- select(pre_Conakry_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Conakry_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Conakry_monthly_2015a$Location <- 'Conakry'
pre_Conakry_monthly_2015a$date <- ymd(paste(pre_Conakry_monthly_2015a$Year, pre_Conakry_monthly_2015a$Month, pre_Conakry_monthly_2015a$day, sep="-"))
pre_Conakry_monthly_2015a <- select(pre_Conakry_monthly_2015a, date, Year, Month, day, Location)
pre_Conakry_monthly_2015 <- full_join(pre_Conakry_monthly_2015a, pre_Conakry_monthly_2015)
pre_Conakry_monthly <- rbind(select(pre_Conakry_monthly,date, Year, Month, day, Location, pre), pre_Conakry_monthly_2015)
rm(pre_Conakry_monthly_2015, pre_Conakry_monthly_2015a)
pre_Conakry_monthly$measurement <- "pre"
pre_Conakry_monthly <- rename(pre_Conakry_monthly, Value=pre)
#Coyah
pre_Coyah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         pre=pre.var[which(lon==9.75),which(lat==13.25),1:48]))
pre_Coyah_monthly$Location <- 'Coyah'
pre_Coyah_monthly$date <- ymd(paste(pre_Coyah_monthly$Year, pre_Coyah_monthly$Month, pre_Coyah_monthly$day, sep="-"))
pre_Coyah_monthly_2015 <- select(pre_Coyah_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Coyah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Coyah_monthly_2015a$Location <- 'Coyah'
pre_Coyah_monthly_2015a$date <- ymd(paste(pre_Coyah_monthly_2015a$Year, pre_Coyah_monthly_2015a$Month, pre_Coyah_monthly_2015a$day, sep="-"))
pre_Coyah_monthly_2015a <- select(pre_Coyah_monthly_2015a, date, Year, Month, day, Location)
pre_Coyah_monthly_2015 <- full_join(pre_Coyah_monthly_2015a, pre_Coyah_monthly_2015)
pre_Coyah_monthly <- rbind(select(pre_Coyah_monthly,date, Year, Month, day, Location, pre), pre_Coyah_monthly_2015)
rm(pre_Coyah_monthly_2015, pre_Coyah_monthly_2015a)
pre_Coyah_monthly$measurement <- "pre"
pre_Coyah_monthly <- rename(pre_Coyah_monthly, Value=pre)
#Dabola
pre_Dabola_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          pre1=pre.var[which(lon==11.25),which(lat==10.75),1:48],
                                          pre2=pre.var[which(lon==10.75),which(lat==10.75),1:48]))
pre_Dabola_monthly$pre <- rowMeans(select(pre_Dabola_monthly, pre1, pre2))
pre_Dabola_monthly$Location <- 'Dabola'
pre_Dabola_monthly$date <- ymd(paste(pre_Dabola_monthly$Year, pre_Dabola_monthly$Month, pre_Dabola_monthly$day, sep="-"))
pre_Dabola_monthly_2015 <- select(pre_Dabola_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Dabola_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Dabola_monthly_2015a$Location <- 'Dabola'
pre_Dabola_monthly_2015a$date <- ymd(paste(pre_Dabola_monthly_2015a$Year, pre_Dabola_monthly_2015a$Month, pre_Dabola_monthly_2015a$day, sep="-"))
pre_Dabola_monthly_2015a <- select(pre_Dabola_monthly_2015a, date, Year, Month, day, Location)
pre_Dabola_monthly_2015 <- full_join(pre_Dabola_monthly_2015a, pre_Dabola_monthly_2015)
pre_Dabola_monthly <- rbind(select(pre_Dabola_monthly,date, Year, Month, day, Location, pre), pre_Dabola_monthly_2015)
rm(pre_Dabola_monthly_2015, pre_Dabola_monthly_2015a)
pre_Dabola_monthly$measurement <- "pre"
pre_Dabola_monthly <- rename(pre_Dabola_monthly, Value=pre)
#Dalaba
pre_Dalaba_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          pre1=pre.var[which(lon==11.25),which(lat==12.25),1:48],
                                          pre2=pre.var[which(lon==10.75),which(lat==12.25),1:48]))
pre_Dalaba_monthly$pre <- rowMeans(select(pre_Dalaba_monthly, pre1, pre2))
pre_Dalaba_monthly$Location <- 'Dalaba'
pre_Dalaba_monthly$date <- ymd(paste(pre_Dalaba_monthly$Year, pre_Dalaba_monthly$Month, pre_Dalaba_monthly$day, sep="-"))
pre_Dalaba_monthly_2015 <- select(pre_Dalaba_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Dalaba_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Dalaba_monthly_2015a$Location <- 'Dalaba'
pre_Dalaba_monthly_2015a$date <- ymd(paste(pre_Dalaba_monthly_2015a$Year, pre_Dalaba_monthly_2015a$Month, pre_Dalaba_monthly_2015a$day, sep="-"))
pre_Dalaba_monthly_2015a <- select(pre_Dalaba_monthly_2015a, date, Year, Month, day, Location)
pre_Dalaba_monthly_2015 <- full_join(pre_Dalaba_monthly_2015a, pre_Dalaba_monthly_2015)
pre_Dalaba_monthly <- rbind(select(pre_Dalaba_monthly,date, Year, Month, day, Location, pre), pre_Dalaba_monthly_2015)
rm(pre_Dalaba_monthly_2015, pre_Dalaba_monthly_2015a)
pre_Dalaba_monthly$measurement <- "pre"
pre_Dalaba_monthly <- rename(pre_Dalaba_monthly, Value=pre)
#Dinguiray
pre_Dinguiray_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             pre1=pre.var[which(lon==11.25),which(lat==11.75),1:48],
                                             pre2=pre.var[which(lon==10.75),which(lat==11.75),1:48], 
                                             pre3=pre.var[which(lon==10.25),which(lat==11.75),1:48],
                                             pre4=pre.var[which(lon==11.25),which(lat==11.25),1:48],
                                             pre5=pre.var[which(lon==10.75),which(lat==11.25),1:48],
                                             pre6=pre.var[which(lon==10.25),which(lat==11.25),1:48]))
pre_Dinguiray_monthly$pre <- rowMeans(select(pre_Dinguiray_monthly, pre1, pre2, pre3, pre4, pre5, pre6))
pre_Dinguiray_monthly$Location <- 'Dinguiray'
pre_Dinguiray_monthly$date <- ymd(paste(pre_Dinguiray_monthly$Year, pre_Dinguiray_monthly$Month, pre_Dinguiray_monthly$day, sep="-"))
pre_Dinguiray_monthly_2015 <- select(pre_Dinguiray_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Dinguiray_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Dinguiray_monthly_2015a$Location <- 'Dinguiray'
pre_Dinguiray_monthly_2015a$date <- ymd(paste(pre_Dinguiray_monthly_2015a$Year, pre_Dinguiray_monthly_2015a$Month, pre_Dinguiray_monthly_2015a$day, sep="-"))
pre_Dinguiray_monthly_2015a <- select(pre_Dinguiray_monthly_2015a, date, Year, Month, day, Location)
pre_Dinguiray_monthly_2015 <- full_join(pre_Dinguiray_monthly_2015a, pre_Dinguiray_monthly_2015)
pre_Dinguiray_monthly <- rbind(select(pre_Dinguiray_monthly,date, Year, Month, day, Location, pre), pre_Dinguiray_monthly_2015)
rm(pre_Dinguiray_monthly_2015, pre_Dinguiray_monthly_2015a)
pre_Dinguiray_monthly$measurement <- "pre"
pre_Dinguiray_monthly <- rename(pre_Dinguiray_monthly, Value=pre)
#Dubreka
pre_Dubreka_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           pre1=pre.var[which(lon==13.75),which(lat==10.25),1:48],
                                           pre2=pre.var[which(lon==13.25),which(lat==10.25),1:48]))
pre_Dubreka_monthly$pre <- rowMeans(select(pre_Dubreka_monthly, pre1, pre2))
pre_Dubreka_monthly$Location <- 'Dubreka'
pre_Dubreka_monthly$date <- ymd(paste(pre_Dubreka_monthly$Year, pre_Dubreka_monthly$Month, pre_Dubreka_monthly$day, sep="-"))
pre_Dubreka_monthly_2015 <- select(pre_Dubreka_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Dubreka_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Dubreka_monthly_2015a$Location <- 'Dubreka'
pre_Dubreka_monthly_2015a$date <- ymd(paste(pre_Dubreka_monthly_2015a$Year, pre_Dubreka_monthly_2015a$Month, pre_Dubreka_monthly_2015a$day, sep="-"))
pre_Dubreka_monthly_2015a <- select(pre_Dubreka_monthly_2015a, date, Year, Month, day, Location)
pre_Dubreka_monthly_2015 <- full_join(pre_Dubreka_monthly_2015a, pre_Dubreka_monthly_2015)
pre_Dubreka_monthly <- rbind(select(pre_Dubreka_monthly,date, Year, Month, day, Location, pre), pre_Dubreka_monthly_2015)
rm(pre_Dubreka_monthly_2015, pre_Dubreka_monthly_2015a)
pre_Dubreka_monthly$measurement <- "pre"
pre_Dubreka_monthly <- rename(pre_Dubreka_monthly, Value=pre)
#Faranah
pre_Faranah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           pre1=pre.var[which(lon==11.25),which(lat==10.25),1:48],
                                           pre2=pre.var[which(lon==10.75),which(lat==10.25),1:48], 
                                           pre3=pre.var[which(lon==10.25),which(lat==10.25),1:48],
                                           pre4=pre.var[which(lon==10.75),which(lat==9.75),1:48],
                                           pre5=pre.var[which(lon==10.75),which(lat==9.25),1:48]))
pre_Faranah_monthly$pre <- rowMeans(select(pre_Faranah_monthly, pre1, pre2, pre3, pre4, pre5))
pre_Faranah_monthly$Location <- 'Faranah'
pre_Faranah_monthly$date <- ymd(paste(pre_Faranah_monthly$Year, pre_Faranah_monthly$Month, pre_Faranah_monthly$day, sep="-"))
pre_Faranah_monthly_2015 <- select(pre_Faranah_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Faranah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Faranah_monthly_2015a$Location <- 'Faranah'
pre_Faranah_monthly_2015a$date <- ymd(paste(pre_Faranah_monthly_2015a$Year, pre_Faranah_monthly_2015a$Month, pre_Faranah_monthly_2015a$day, sep="-"))
pre_Faranah_monthly_2015a <- select(pre_Faranah_monthly_2015a, date, Year, Month, day, Location)
pre_Faranah_monthly_2015 <- full_join(pre_Faranah_monthly_2015a, pre_Faranah_monthly_2015)
pre_Faranah_monthly <- rbind(select(pre_Faranah_monthly,date, Year, Month, day, Location, pre), pre_Faranah_monthly_2015)
rm(pre_Faranah_monthly_2015, pre_Faranah_monthly_2015a)
pre_Faranah_monthly$measurement <- "pre"
pre_Faranah_monthly <- rename(pre_Faranah_monthly, Value=pre)
#Forecariah
pre_Forecariah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                              Month=rep(seq(1,12,1), 4), day=1,
                                              pre1=pre.var[which(lon==12.75),which(lat==9.75),1:48],
                                              pre2=pre.var[which(lon==13.25),which(lat==9.25),1:48], 
                                              pre3=pre.var[which(lon==12.75),which(lat==9.25),1:48]))
pre_Forecariah_monthly$pre <- rowMeans(select(pre_Forecariah_monthly, pre1, pre2, pre3))
pre_Forecariah_monthly$Location <- 'Forecariah'
pre_Forecariah_monthly$date <- ymd(paste(pre_Forecariah_monthly$Year, pre_Forecariah_monthly$Month, pre_Forecariah_monthly$day, sep="-"))
pre_Forecariah_monthly_2015 <- select(pre_Forecariah_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Forecariah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Forecariah_monthly_2015a$Location <- 'Forecariah'
pre_Forecariah_monthly_2015a$date <- ymd(paste(pre_Forecariah_monthly_2015a$Year, pre_Forecariah_monthly_2015a$Month, pre_Forecariah_monthly_2015a$day, sep="-"))
pre_Forecariah_monthly_2015a <- select(pre_Forecariah_monthly_2015a, date, Year, Month, day, Location)
pre_Forecariah_monthly_2015 <- full_join(pre_Forecariah_monthly_2015a, pre_Forecariah_monthly_2015)
pre_Forecariah_monthly <- rbind(select(pre_Forecariah_monthly,date, Year, Month, day, Location, pre), pre_Forecariah_monthly_2015)
rm(pre_Forecariah_monthly_2015, pre_Forecariah_monthly_2015a)
pre_Forecariah_monthly$measurement <- "pre"
pre_Forecariah_monthly <- rename(pre_Forecariah_monthly, Value=pre)
#Fria
pre_Fria_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        pre1=pre.var[which(lon==13.75),which(lat==10.75),1:48],
                                        pre2=pre.var[which(lon==13.75),which(lat==10.25),1:48]))
pre_Fria_monthly$pre <- rowMeans(select(pre_Fria_monthly, pre1, pre2))
pre_Fria_monthly$Location <- 'Fria'
pre_Fria_monthly$date <- ymd(paste(pre_Fria_monthly$Year, pre_Fria_monthly$Month, pre_Fria_monthly$day, sep="-"))
pre_Fria_monthly_2015 <- select(pre_Fria_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Fria_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Fria_monthly_2015a$Location <- 'Fria'
pre_Fria_monthly_2015a$date <- ymd(paste(pre_Fria_monthly_2015a$Year, pre_Fria_monthly_2015a$Month, pre_Fria_monthly_2015a$day, sep="-"))
pre_Fria_monthly_2015a <- select(pre_Fria_monthly_2015a, date, Year, Month, day, Location)
pre_Fria_monthly_2015 <- full_join(pre_Fria_monthly_2015a, pre_Fria_monthly_2015)
pre_Fria_monthly <- rbind(select(pre_Fria_monthly,date, Year, Month, day, Location, pre), pre_Fria_monthly_2015)
rm(pre_Fria_monthly_2015, pre_Fria_monthly_2015a)
pre_Fria_monthly$measurement <- "pre"
pre_Fria_monthly <- rename(pre_Fria_monthly, Value=pre)
#Gaoual
pre_Gaoual_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          pre1=pre.var[which(lon==13.75),which(lat==12.25),1:48],
                                          pre2=pre.var[which(lon==13.25),which(lat==12.25),1:48], 
                                          pre3=pre.var[which(lon==13.75),which(lat==11.75),1:48],
                                          pre4=pre.var[which(lon==13.25),which(lat==11.75),1:48],
                                          pre5=pre.var[which(lon==12.75),which(lat==11.75),1:48],
                                          pre6=pre.var[which(lon==13.75),which(lat==11.25),1:48],
                                          pre7=pre.var[which(lon==13.25),which(lat==11.25),1:48]))
pre_Gaoual_monthly$pre <- rowMeans(select(pre_Gaoual_monthly, pre1, pre2, pre3, pre4, pre5, pre6, pre7))
pre_Gaoual_monthly$Location <- 'Gaoual'
pre_Gaoual_monthly$date <- ymd(paste(pre_Gaoual_monthly$Year, pre_Gaoual_monthly$Month, pre_Gaoual_monthly$day, sep="-"))
pre_Gaoual_monthly_2015 <- select(pre_Gaoual_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Gaoual_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Gaoual_monthly_2015a$Location <- 'Gaoual'
pre_Gaoual_monthly_2015a$date <- ymd(paste(pre_Gaoual_monthly_2015a$Year, pre_Gaoual_monthly_2015a$Month, pre_Gaoual_monthly_2015a$day, sep="-"))
pre_Gaoual_monthly_2015a <- select(pre_Gaoual_monthly_2015a, date, Year, Month, day, Location)
pre_Gaoual_monthly_2015 <- full_join(pre_Gaoual_monthly_2015a, pre_Gaoual_monthly_2015)
pre_Gaoual_monthly <- rbind(select(pre_Gaoual_monthly,date, Year, Month, day, Location, pre), pre_Gaoual_monthly_2015)
rm(pre_Gaoual_monthly_2015, pre_Gaoual_monthly_2015a)
pre_Gaoual_monthly$measurement <- "pre"
pre_Gaoual_monthly <- rename(pre_Gaoual_monthly, Value=pre)
#Gueckedou
pre_Gueckedou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             pre=pre.var[which(lon==10.25),which(lat==8.75),1:48]))
pre_Gueckedou_monthly$Location <- 'Gueckedou'
pre_Gueckedou_monthly$date <- ymd(paste(pre_Gueckedou_monthly$Year, pre_Gueckedou_monthly$Month, pre_Gueckedou_monthly$day, sep="-"))
pre_Gueckedou_monthly_2015 <- select(pre_Gueckedou_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Gueckedou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Gueckedou_monthly_2015a$Location <- 'Gueckedou'
pre_Gueckedou_monthly_2015a$date <- ymd(paste(pre_Gueckedou_monthly_2015a$Year, pre_Gueckedou_monthly_2015a$Month, pre_Gueckedou_monthly_2015a$day, sep="-"))
pre_Gueckedou_monthly_2015a <- select(pre_Gueckedou_monthly_2015a, date, Year, Month, day, Location)
pre_Gueckedou_monthly_2015 <- full_join(pre_Gueckedou_monthly_2015a, pre_Gueckedou_monthly_2015)
pre_Gueckedou_monthly <- rbind(select(pre_Gueckedou_monthly,date, Year, Month, day, Location, pre), pre_Gueckedou_monthly_2015)
rm(pre_Gueckedou_monthly_2015, pre_Gueckedou_monthly_2015a)
pre_Gueckedou_monthly$measurement <- "pre"
pre_Gueckedou_monthly <- rename(pre_Gueckedou_monthly, Value=pre)
#Kankan
pre_Kankan_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          pre1=pre.var[which(lon==9.25),which(lat==10.75),1:48],
                                          pre2=pre.var[which(lon==9.75),which(lat==10.25),1:48], 
                                          pre3=pre.var[which(lon==9.25),which(lat==10.25),1:48],
                                          pre4=pre.var[which(lon==8.75),which(lat==10.25),1:48],
                                          pre5=pre.var[which(lon==9.75),which(lat==9.75),1:48],
                                          pre6=pre.var[which(lon==9.25),which(lat==9.75),1:48]))
pre_Kankan_monthly$pre <- rowMeans(select(pre_Kankan_monthly, pre1, pre2, pre3, pre4, pre5, pre6))
pre_Kankan_monthly$Location <- 'Kankan'
pre_Kankan_monthly$date <- ymd(paste(pre_Kankan_monthly$Year, pre_Kankan_monthly$Month, pre_Kankan_monthly$day, sep="-"))
pre_Kankan_monthly_2015 <- select(pre_Kankan_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Kankan_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Kankan_monthly_2015a$Location <- 'Kankan'
pre_Kankan_monthly_2015a$date <- ymd(paste(pre_Kankan_monthly_2015a$Year, pre_Kankan_monthly_2015a$Month, pre_Kankan_monthly_2015a$day, sep="-"))
pre_Kankan_monthly_2015a <- select(pre_Kankan_monthly_2015a, date, Year, Month, day, Location)
pre_Kankan_monthly_2015 <- full_join(pre_Kankan_monthly_2015a, pre_Kankan_monthly_2015)
pre_Kankan_monthly <- rbind(select(pre_Kankan_monthly,date, Year, Month, day, Location, pre), pre_Kankan_monthly_2015)
rm(pre_Kankan_monthly_2015, pre_Kankan_monthly_2015a)
pre_Kankan_monthly$measurement <- "pre"
pre_Kankan_monthly <- rename(pre_Kankan_monthly, Value=pre)
#Kerouane
pre_Kerouane_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            pre1=pre.var[which(lon==9.25),which(lat==9.75),1:48],
                                            pre2=pre.var[which(lon==8.75),which(lat==9.75),1:48], 
                                            pre3=pre.var[which(lon==9.75),which(lat==9.25),1:48],
                                            pre4=pre.var[which(lon==9.25),which(lat==9.25),1:48],
                                            pre5=pre.var[which(lon==8.75),which(lat==9.25),1:48],
                                            pre6=pre.var[which(lon==9.25),which(lat==8.75),1:48]))
pre_Kerouane_monthly$pre <- rowMeans(select(pre_Kerouane_monthly, pre1, pre2, pre3, pre4, pre5, pre6))
pre_Kerouane_monthly$Location <- 'Kerouane'
pre_Kerouane_monthly$date <- ymd(paste(pre_Kerouane_monthly$Year, pre_Kerouane_monthly$Month, pre_Kerouane_monthly$day, sep="-"))
pre_Kerouane_monthly_2015 <- select(pre_Kerouane_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Kerouane_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Kerouane_monthly_2015a$Location <- 'Kerouane'
pre_Kerouane_monthly_2015a$date <- ymd(paste(pre_Kerouane_monthly_2015a$Year, pre_Kerouane_monthly_2015a$Month, pre_Kerouane_monthly_2015a$day, sep="-"))
pre_Kerouane_monthly_2015a <- select(pre_Kerouane_monthly_2015a, date, Year, Month, day, Location)
pre_Kerouane_monthly_2015 <- full_join(pre_Kerouane_monthly_2015a, pre_Kerouane_monthly_2015)
pre_Kerouane_monthly <- rbind(select(pre_Kerouane_monthly,date, Year, Month, day, Location, pre), pre_Kerouane_monthly_2015)
rm(pre_Kerouane_monthly_2015, pre_Kerouane_monthly_2015a)
pre_Kerouane_monthly$measurement <- "pre"
pre_Kerouane_monthly <- rename(pre_Kerouane_monthly, Value=pre)
#Kindia
pre_Kindia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          pre1=pre.var[which(lon==13.25),which(lat==10.25),1:48],
                                          pre2=pre.var[which(lon==12.75),which(lat==10.25),1:48], 
                                          pre3=pre.var[which(lon==12.25),which(lat==10.25),1:48],
                                          pre4=pre.var[which(lon==12.75),which(lat==9.75),1:48]))
pre_Kindia_monthly$pre <- rowMeans(select(pre_Kindia_monthly, pre1, pre2, pre3, pre4))
pre_Kindia_monthly$Location <- 'Kindia'
pre_Kindia_monthly$date <- ymd(paste(pre_Kindia_monthly$Year, pre_Kindia_monthly$Month, pre_Kindia_monthly$day, sep="-"))
pre_Kindia_monthly_2015 <- select(pre_Kindia_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Kindia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Kindia_monthly_2015a$Location <- 'Kindia'
pre_Kindia_monthly_2015a$date <- ymd(paste(pre_Kindia_monthly_2015a$Year, pre_Kindia_monthly_2015a$Month, pre_Kindia_monthly_2015a$day, sep="-"))
pre_Kindia_monthly_2015a <- select(pre_Kindia_monthly_2015a, date, Year, Month, day, Location)
pre_Kindia_monthly_2015 <- full_join(pre_Kindia_monthly_2015a, pre_Kindia_monthly_2015)
pre_Kindia_monthly <- rbind(select(pre_Kindia_monthly,date, Year, Month, day, Location, pre), pre_Kindia_monthly_2015)
rm(pre_Kindia_monthly_2015, pre_Kindia_monthly_2015a)
pre_Kindia_monthly$measurement <- "pre"
pre_Kindia_monthly <- rename(pre_Kindia_monthly, Value=pre)
#KISSIDOUGOU
pre_Kissidougou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               pre1=pre.var[which(lon==10.25),which(lat==9.75),1:48],
                                               pre2=pre.var[which(lon==10.25),which(lat==9.25),1:48], 
                                               pre3=pre.var[which(lon==9.75),which(lat==9.25),1:48]))
pre_Kissidougou_monthly$pre <- rowMeans(select(pre_Kissidougou_monthly, pre1, pre2, pre3))
pre_Kissidougou_monthly$Location <- 'Kissidougou'
pre_Kissidougou_monthly$date <- ymd(paste(pre_Kissidougou_monthly$Year, pre_Kissidougou_monthly$Month, pre_Kissidougou_monthly$day, sep="-"))
pre_Kissidougou_monthly_2015 <- select(pre_Kissidougou_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Kissidougou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Kissidougou_monthly_2015a$Location <- 'Kissidougou'
pre_Kissidougou_monthly_2015a$date <- ymd(paste(pre_Kissidougou_monthly_2015a$Year, pre_Kissidougou_monthly_2015a$Month, pre_Kissidougou_monthly_2015a$day, sep="-"))
pre_Kissidougou_monthly_2015a <- select(pre_Kissidougou_monthly_2015a, date, Year, Month, day, Location)
pre_Kissidougou_monthly_2015 <- full_join(pre_Kissidougou_monthly_2015a, pre_Kissidougou_monthly_2015)
pre_Kissidougou_monthly <- rbind(select(pre_Kissidougou_monthly,date, Year, Month, day, Location, pre), pre_Kissidougou_monthly_2015)
rm(pre_Kissidougou_monthly_2015, pre_Kissidougou_monthly_2015a)
pre_Kissidougou_monthly$measurement <- "pre"
pre_Kissidougou_monthly <- rename(pre_Kissidougou_monthly, Value=pre)
#Koubia
pre_Koubia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          pre=pre.var[which(lon==10.25),which(lat==9.75),1:48]))
pre_Koubia_monthly$Location <- 'Koubia'
pre_Koubia_monthly$date <- ymd(paste(pre_Koubia_monthly$Year, pre_Koubia_monthly$Month, pre_Koubia_monthly$day, sep="-"))
pre_Koubia_monthly_2015 <- select(pre_Koubia_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Koubia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Koubia_monthly_2015a$Location <- 'Koubia'
pre_Koubia_monthly_2015a$date <- ymd(paste(pre_Koubia_monthly_2015a$Year, pre_Koubia_monthly_2015a$Month, pre_Koubia_monthly_2015a$day, sep="-"))
pre_Koubia_monthly_2015a <- select(pre_Koubia_monthly_2015a, date, Year, Month, day, Location)
pre_Koubia_monthly_2015 <- full_join(pre_Koubia_monthly_2015a, pre_Koubia_monthly_2015)
pre_Koubia_monthly <- rbind(select(pre_Koubia_monthly,date, Year, Month, day, Location, pre), pre_Koubia_monthly_2015)
rm(pre_Koubia_monthly_2015, pre_Koubia_monthly_2015a)
pre_Koubia_monthly$measurement <- "pre"
pre_Koubia_monthly <- rename(pre_Koubia_monthly, Value=pre)
#Koundara
pre_Koundara_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            pre1=pre.var[which(lon==13.25),which(lat==12.75),1:48],
                                            pre2=pre.var[which(lon==13.25),which(lat==12.25),1:48], 
                                            pre3=pre.var[which(lon==12.75),which(lat==12.25),1:48]))
pre_Koundara_monthly$pre <- rowMeans(select(pre_Koundara_monthly, pre1, pre2, pre3))
pre_Koundara_monthly$Location <- 'Koundara'
pre_Koundara_monthly$date <- ymd(paste(pre_Koundara_monthly$Year, pre_Koundara_monthly$Month, pre_Koundara_monthly$day, sep="-"))
pre_Koundara_monthly_2015 <- select(pre_Koundara_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Koundara_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Koundara_monthly_2015a$Location <- 'Koundara'
pre_Koundara_monthly_2015a$date <- ymd(paste(pre_Koundara_monthly_2015a$Year, pre_Koundara_monthly_2015a$Month, pre_Koundara_monthly_2015a$day, sep="-"))
pre_Koundara_monthly_2015a <- select(pre_Koundara_monthly_2015a, date, Year, Month, day, Location)
pre_Koundara_monthly_2015 <- full_join(pre_Koundara_monthly_2015a, pre_Koundara_monthly_2015)
pre_Koundara_monthly <- rbind(select(pre_Koundara_monthly,date, Year, Month, day, Location, pre), pre_Koundara_monthly_2015)
rm(pre_Koundara_monthly_2015, pre_Koundara_monthly_2015a)
pre_Koundara_monthly$measurement <- "pre"
pre_Koundara_monthly <- rename(pre_Koundara_monthly, Value=pre)
#Kouroussa
pre_Kouroussa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             pre1=pre.var[which(lon==10.25),which(lat==11.25),1:48],
                                             pre2=pre.var[which(lon==10.75),which(lat==10.75),1:48], 
                                             pre3=pre.var[which(lon==10.25),which(lat==10.75),1:48],
                                             pre4=pre.var[which(lon==9.75),which(lat==10.75),1:48],
                                             pre5=pre.var[which(lon==10.25),which(lat==10.25),1:48],
                                             pre6=pre.var[which(lon==9.75),which(lat==10.25),1:48],
                                             pre7=pre.var[which(lon==10.25),which(lat==9.75),1:48],
                                             pre8=pre.var[which(lon==9.75),which(lat==9.75),1:48]))
pre_Kouroussa_monthly$pre <- rowMeans(select(pre_Kouroussa_monthly, pre1, pre2, pre3, pre4, pre5, pre6, pre7, pre8))
pre_Kouroussa_monthly$Location <- 'Kouroussa'
pre_Kouroussa_monthly$date <- ymd(paste(pre_Kouroussa_monthly$Year, pre_Kouroussa_monthly$Month, pre_Kouroussa_monthly$day, sep="-"))
pre_Kouroussa_monthly_2015 <- select(pre_Kouroussa_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Kouroussa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Kouroussa_monthly_2015a$Location <- 'Kouroussa'
pre_Kouroussa_monthly_2015a$date <- ymd(paste(pre_Kouroussa_monthly_2015a$Year, pre_Kouroussa_monthly_2015a$Month, pre_Kouroussa_monthly_2015a$day, sep="-"))
pre_Kouroussa_monthly_2015a <- select(pre_Kouroussa_monthly_2015a, date, Year, Month, day, Location)
pre_Kouroussa_monthly_2015 <- full_join(pre_Kouroussa_monthly_2015a, pre_Kouroussa_monthly_2015)
pre_Kouroussa_monthly <- rbind(select(pre_Kouroussa_monthly,date, Year, Month, day, Location, pre), pre_Kouroussa_monthly_2015)
rm(pre_Kouroussa_monthly_2015, pre_Kouroussa_monthly_2015a)
pre_Kouroussa_monthly$measurement <- "pre"
pre_Kouroussa_monthly <- rename(pre_Kouroussa_monthly, Value=pre)
#Labe
pre_Labe_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        pre1=pre.var[which(lon==12.25),which(lat==11.75),1:48],
                                        pre2=pre.var[which(lon==12.25),which(lat==11.25),1:48]))
pre_Labe_monthly$pre <- rowMeans(select(pre_Labe_monthly, pre1, pre2))
pre_Labe_monthly$Location <- 'Labe'
pre_Labe_monthly$date <- ymd(paste(pre_Labe_monthly$Year, pre_Labe_monthly$Month, pre_Labe_monthly$day, sep="-"))
pre_Labe_monthly_2015 <- select(pre_Labe_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Labe_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Labe_monthly_2015a$Location <- 'Labe'
pre_Labe_monthly_2015a$date <- ymd(paste(pre_Labe_monthly_2015a$Year, pre_Labe_monthly_2015a$Month, pre_Labe_monthly_2015a$day, sep="-"))
pre_Labe_monthly_2015a <- select(pre_Labe_monthly_2015a, date, Year, Month, day, Location)
pre_Labe_monthly_2015 <- full_join(pre_Labe_monthly_2015a, pre_Labe_monthly_2015)
pre_Labe_monthly <- rbind(select(pre_Labe_monthly,date, Year, Month, day, Location, pre), pre_Labe_monthly_2015)
rm(pre_Labe_monthly_2015, pre_Labe_monthly_2015a)
pre_Labe_monthly$measurement <- "pre"
pre_Labe_monthly <- rename(pre_Labe_monthly, Value=pre)
#Lelouma
pre_Lelouma_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           pre1=pre.var[which(lon==12.75),which(lat==11.75),1:48],
                                           pre2=pre.var[which(lon==12.75),which(lat==11.25),1:48]))
pre_Lelouma_monthly$pre <- rowMeans(select(pre_Lelouma_monthly, pre1, pre2))
pre_Lelouma_monthly$Location <- 'Lelouma'
pre_Lelouma_monthly$date <- ymd(paste(pre_Lelouma_monthly$Year, pre_Lelouma_monthly$Month, pre_Lelouma_monthly$day, sep="-"))
pre_Lelouma_monthly_2015 <- select(pre_Lelouma_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Lelouma_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Lelouma_monthly_2015a$Location <- 'Lelouma'
pre_Lelouma_monthly_2015a$date <- ymd(paste(pre_Lelouma_monthly_2015a$Year, pre_Lelouma_monthly_2015a$Month, pre_Lelouma_monthly_2015a$day, sep="-"))
pre_Lelouma_monthly_2015a <- select(pre_Lelouma_monthly_2015a, date, Year, Month, day, Location)
pre_Lelouma_monthly_2015 <- full_join(pre_Lelouma_monthly_2015a, pre_Lelouma_monthly_2015)
pre_Lelouma_monthly <- rbind(select(pre_Lelouma_monthly,date, Year, Month, day, Location, pre), pre_Lelouma_monthly_2015)
rm(pre_Lelouma_monthly_2015, pre_Lelouma_monthly_2015a)
pre_Lelouma_monthly$measurement <- "pre"
pre_Lelouma_monthly <- rename(pre_Lelouma_monthly, Value=pre)
#Lola
pre_Lola_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        pre1=pre.var[which(lon==8.25),which(lat==8.25),1:48],
                                        pre2=pre.var[which(lon==8.25),which(lat==7.75),1:48]))
pre_Lola_monthly$pre <- rowMeans(select(pre_Lola_monthly, pre1, pre2))
pre_Lola_monthly$Location <- 'Lola'
pre_Lola_monthly$date <- ymd(paste(pre_Lola_monthly$Year, pre_Lola_monthly$Month, pre_Lola_monthly$day, sep="-"))
pre_Lola_monthly_2015 <- select(pre_Lola_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Lola_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Lola_monthly_2015a$Location <- 'Lola'
pre_Lola_monthly_2015a$date <- ymd(paste(pre_Lola_monthly_2015a$Year, pre_Lola_monthly_2015a$Month, pre_Lola_monthly_2015a$day, sep="-"))
pre_Lola_monthly_2015a <- select(pre_Lola_monthly_2015a, date, Year, Month, day, Location)
pre_Lola_monthly_2015 <- full_join(pre_Lola_monthly_2015a, pre_Lola_monthly_2015)
pre_Lola_monthly <- rbind(select(pre_Lola_monthly,date, Year, Month, day, Location, pre), pre_Lola_monthly_2015)
rm(pre_Lola_monthly_2015, pre_Lola_monthly_2015a)
pre_Lola_monthly$measurement <- "pre"
pre_Lola_monthly <- rename(pre_Lola_monthly, Value=pre)
#Macenta
pre_Macenta_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           pre1=pre.var[which(lon==9.75),which(lat==8.75),1:48],
                                           pre2=pre.var[which(lon==9.25),which(lat==8.75),1:48],
                                           pre3=pre.var[which(lon==9.25),which(lat==8.25),1:48]))
pre_Macenta_monthly$pre <- rowMeans(select(pre_Macenta_monthly, pre1, pre2, pre3))
pre_Macenta_monthly$Location <- 'Macenta'
pre_Macenta_monthly$date <- ymd(paste(pre_Macenta_monthly$Year, pre_Macenta_monthly$Month, pre_Macenta_monthly$day, sep="-"))
pre_Macenta_monthly_2015 <- select(pre_Macenta_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Macenta_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Macenta_monthly_2015a$Location <- 'Macenta'
pre_Macenta_monthly_2015a$date <- ymd(paste(pre_Macenta_monthly_2015a$Year, pre_Macenta_monthly_2015a$Month, pre_Macenta_monthly_2015a$day, sep="-"))
pre_Macenta_monthly_2015a <- select(pre_Macenta_monthly_2015a, date, Year, Month, day, Location)
pre_Macenta_monthly_2015 <- full_join(pre_Macenta_monthly_2015a, pre_Macenta_monthly_2015)
pre_Macenta_monthly <- rbind(select(pre_Macenta_monthly,date, Year, Month, day, Location, pre), pre_Macenta_monthly_2015)
rm(pre_Macenta_monthly_2015, pre_Macenta_monthly_2015a)
pre_Macenta_monthly$measurement <- "pre"
pre_Macenta_monthly <- rename(pre_Macenta_monthly, Value=pre)
#Mali
pre_Mali_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        pre1=pre.var[which(lon==12.75),which(lat==12.25),1:48],
                                        pre2=pre.var[which(lon==12.25),which(lat==12.25),1:48], 
                                        pre3=pre.var[which(lon==11.75),which(lat==12.25),1:48],
                                        pre4=pre.var[which(lon==12.75),which(lat==11.75),1:48],
                                        pre5=pre.var[which(lon==12.25),which(lat==11.75),1:48]))
pre_Mali_monthly$pre <- rowMeans(select(pre_Mali_monthly, pre1, pre2, pre3, pre4, pre5))
pre_Mali_monthly$Location <- 'Mali'
pre_Mali_monthly$date <- ymd(paste(pre_Mali_monthly$Year, pre_Mali_monthly$Month, pre_Mali_monthly$day, sep="-"))
pre_Mali_monthly_2015 <- select(pre_Mali_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Mali_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Mali_monthly_2015a$Location <- 'Mali'
pre_Mali_monthly_2015a$date <- ymd(paste(pre_Mali_monthly_2015a$Year, pre_Mali_monthly_2015a$Month, pre_Mali_monthly_2015a$day, sep="-"))
pre_Mali_monthly_2015a <- select(pre_Mali_monthly_2015a, date, Year, Month, day, Location)
pre_Mali_monthly_2015 <- full_join(pre_Mali_monthly_2015a, pre_Mali_monthly_2015)
pre_Mali_monthly <- rbind(select(pre_Mali_monthly,date, Year, Month, day, Location, pre), pre_Mali_monthly_2015)
rm(pre_Mali_monthly_2015, pre_Mali_monthly_2015a)
pre_Mali_monthly$measurement <- "pre"
pre_Mali_monthly <- rename(pre_Mali_monthly, Value=pre)
#Mamou
pre_Mamou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         pre1=pre.var[which(lon==11.75),which(lat==10.75),1:48],
                                         pre2=pre.var[which(lon==12.25),which(lat==10.25),1:48],
                                         pre3=pre.var[which(lon==11.75),which(lat==10.25),1:48]))
pre_Mamou_monthly$pre <- rowMeans(select(pre_Mamou_monthly, pre1, pre2, pre3))
pre_Mamou_monthly$Location <- 'Mamou'
pre_Mamou_monthly$date <- ymd(paste(pre_Mamou_monthly$Year, pre_Mamou_monthly$Month, pre_Mamou_monthly$day, sep="-"))
pre_Mamou_monthly_2015 <- select(pre_Mamou_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Mamou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Mamou_monthly_2015a$Location <- 'Mamou'
pre_Mamou_monthly_2015a$date <- ymd(paste(pre_Mamou_monthly_2015a$Year, pre_Mamou_monthly_2015a$Month, pre_Mamou_monthly_2015a$day, sep="-"))
pre_Mamou_monthly_2015a <- select(pre_Mamou_monthly_2015a, date, Year, Month, day, Location)
pre_Mamou_monthly_2015 <- full_join(pre_Mamou_monthly_2015a, pre_Mamou_monthly_2015)
pre_Mamou_monthly <- rbind(select(pre_Mamou_monthly,date, Year, Month, day, Location, pre), pre_Mamou_monthly_2015)
rm(pre_Mamou_monthly_2015, pre_Mamou_monthly_2015a)
pre_Mamou_monthly$measurement <- "pre"
pre_Mamou_monthly <- rename(pre_Mamou_monthly, Value=pre)
#Nzerekore
pre_Nzerekore_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             pre1=pre.var[which(lon==8.75),which(lat==8.25),1:48],
                                             pre2=pre.var[which(lon==8.75),which(lat==7.75),1:48]))
pre_Nzerekore_monthly$pre <- rowMeans(select(pre_Nzerekore_monthly, pre1, pre2))
pre_Nzerekore_monthly$Location <- 'Nzerekore'
pre_Nzerekore_monthly$date <- ymd(paste(pre_Nzerekore_monthly$Year, pre_Nzerekore_monthly$Month, pre_Nzerekore_monthly$day, sep="-"))
pre_Nzerekore_monthly_2015 <- select(pre_Nzerekore_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Nzerekore_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Nzerekore_monthly_2015a$Location <- 'Nzerekore'
pre_Nzerekore_monthly_2015a$date <- ymd(paste(pre_Nzerekore_monthly_2015a$Year, pre_Nzerekore_monthly_2015a$Month, pre_Nzerekore_monthly_2015a$day, sep="-"))
pre_Nzerekore_monthly_2015a <- select(pre_Nzerekore_monthly_2015a, date, Year, Month, day, Location)
pre_Nzerekore_monthly_2015 <- full_join(pre_Nzerekore_monthly_2015a, pre_Nzerekore_monthly_2015)
pre_Nzerekore_monthly <- rbind(select(pre_Nzerekore_monthly,date, Year, Month, day, Location, pre), pre_Nzerekore_monthly_2015)
rm(pre_Nzerekore_monthly_2015, pre_Nzerekore_monthly_2015a)
pre_Nzerekore_monthly$measurement <- "pre"
pre_Nzerekore_monthly <- rename(pre_Nzerekore_monthly, Value=pre)
#Pita
pre_Pita_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        pre1=pre.var[which(lon==12.75),which(lat==11.25),1:48],
                                        pre2=pre.var[which(lon==12.25),which(lat==11.25),1:48],
                                        pre3=pre.var[which(lon==12.75),which(lat==10.75),1:48]))
pre_Pita_monthly$pre <- rowMeans(select(pre_Pita_monthly, pre1, pre2, pre3))
pre_Pita_monthly$Location <- 'Pita'
pre_Pita_monthly$date <- ymd(paste(pre_Pita_monthly$Year, pre_Pita_monthly$Month, pre_Pita_monthly$day, sep="-"))
pre_Pita_monthly_2015 <- select(pre_Pita_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Pita_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Pita_monthly_2015a$Location <- 'Pita'
pre_Pita_monthly_2015a$date <- ymd(paste(pre_Pita_monthly_2015a$Year, pre_Pita_monthly_2015a$Month, pre_Pita_monthly_2015a$day, sep="-"))
pre_Pita_monthly_2015a <- select(pre_Pita_monthly_2015a, date, Year, Month, day, Location)
pre_Pita_monthly_2015 <- full_join(pre_Pita_monthly_2015a, pre_Pita_monthly_2015)
pre_Pita_monthly <- rbind(select(pre_Pita_monthly,date, Year, Month, day, Location, pre), pre_Pita_monthly_2015)
rm(pre_Pita_monthly_2015, pre_Pita_monthly_2015a)
pre_Pita_monthly$measurement <- "pre"
pre_Pita_monthly <- rename(pre_Pita_monthly, Value=pre)
#Siguiri
pre_Siguiri_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           pre1=pre.var[which(lon==9.25),which(lat==12.25),1:48],
                                           pre2=pre.var[which(lon==10.25),which(lat==11.75),1:48], 
                                           pre3=pre.var[which(lon==9.75),which(lat==11.75),1:48],
                                           pre4=pre.var[which(lon==9.25),which(lat==11.75),1:48],
                                           pre5=pre.var[which(lon==9.75),which(lat==11.25),1:48],
                                           pre6=pre.var[which(lon==9.25),which(lat==11.25),1:48]))
pre_Siguiri_monthly$pre <- rowMeans(select(pre_Siguiri_monthly, pre1, pre2, pre3, pre4, pre5, pre6))
pre_Siguiri_monthly$Location <- 'Siguiri'
pre_Siguiri_monthly$date <- ymd(paste(pre_Siguiri_monthly$Year, pre_Siguiri_monthly$Month, pre_Siguiri_monthly$day, sep="-"))
pre_Siguiri_monthly_2015 <- select(pre_Siguiri_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Siguiri_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Siguiri_monthly_2015a$Location <- 'Siguiri'
pre_Siguiri_monthly_2015a$date <- ymd(paste(pre_Siguiri_monthly_2015a$Year, pre_Siguiri_monthly_2015a$Month, pre_Siguiri_monthly_2015a$day, sep="-"))
pre_Siguiri_monthly_2015a <- select(pre_Siguiri_monthly_2015a, date, Year, Month, day, Location)
pre_Siguiri_monthly_2015 <- full_join(pre_Siguiri_monthly_2015a, pre_Siguiri_monthly_2015)
pre_Siguiri_monthly <- rbind(select(pre_Siguiri_monthly,date, Year, Month, day, Location, pre), pre_Siguiri_monthly_2015)
rm(pre_Siguiri_monthly_2015, pre_Siguiri_monthly_2015a)
pre_Siguiri_monthly$measurement <- "pre"
pre_Siguiri_monthly <- rename(pre_Siguiri_monthly, Value=pre)
#Telimele
pre_Telimele_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            pre1=pre.var[which(lon==13.75),which(lat==11.25),1:48],
                                            pre2=pre.var[which(lon==13.25),which(lat==11.25),1:48], 
                                            pre3=pre.var[which(lon==13.75),which(lat==10.75),1:48],
                                            pre4=pre.var[which(lon==13.25),which(lat==10.75),1:48]))
pre_Telimele_monthly$pre <- rowMeans(select(pre_Telimele_monthly, pre1, pre2, pre3, pre4))
pre_Telimele_monthly$Location <- 'Telimele'
pre_Telimele_monthly$date <- ymd(paste(pre_Telimele_monthly$Year, pre_Telimele_monthly$Month, pre_Telimele_monthly$day, sep="-"))
pre_Telimele_monthly_2015 <- select(pre_Telimele_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Telimele_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Telimele_monthly_2015a$Location <- 'Telimele'
pre_Telimele_monthly_2015a$date <- ymd(paste(pre_Telimele_monthly_2015a$Year, pre_Telimele_monthly_2015a$Month, pre_Telimele_monthly_2015a$day, sep="-"))
pre_Telimele_monthly_2015a <- select(pre_Telimele_monthly_2015a, date, Year, Month, day, Location)
pre_Telimele_monthly_2015 <- full_join(pre_Telimele_monthly_2015a, pre_Telimele_monthly_2015)
pre_Telimele_monthly <- rbind(select(pre_Telimele_monthly,date, Year, Month, day, Location, pre), pre_Telimele_monthly_2015)
rm(pre_Telimele_monthly_2015, pre_Telimele_monthly_2015a)
pre_Telimele_monthly$measurement <- "pre"
pre_Telimele_monthly <- rename(pre_Telimele_monthly, Value=pre)
#Tougue
pre_Tougue_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4),day=1, 
                                          pre1=pre.var[which(lon==11.25),which(lat==11.75),1:48],
                                          pre2=pre.var[which(lon==11.75),which(lat==11.25),1:48]))
pre_Tougue_monthly$pre <- rowMeans(select(pre_Tougue_monthly, pre1, pre2))
pre_Tougue_monthly$Location <- 'Tougue'
pre_Tougue_monthly$date <- ymd(paste(pre_Tougue_monthly$Year, pre_Tougue_monthly$Month, pre_Tougue_monthly$day, sep="-"))
pre_Tougue_monthly_2015 <- select(pre_Tougue_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_Tougue_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_Tougue_monthly_2015a$Location <- 'Tougue'
pre_Tougue_monthly_2015a$date <- ymd(paste(pre_Tougue_monthly_2015a$Year, pre_Tougue_monthly_2015a$Month, pre_Tougue_monthly_2015a$day, sep="-"))
pre_Tougue_monthly_2015a <- select(pre_Tougue_monthly_2015a, date, Year, Month, day, Location)
pre_Tougue_monthly_2015 <- full_join(pre_Tougue_monthly_2015a, pre_Tougue_monthly_2015)
pre_Tougue_monthly <- rbind(select(pre_Tougue_monthly,date, Year, Month, day, Location, pre), pre_Tougue_monthly_2015)
rm(pre_Tougue_monthly_2015, pre_Tougue_monthly_2015a)
pre_Tougue_monthly$measurement <- "pre"
pre_Tougue_monthly <- rename(pre_Tougue_monthly, Value=pre)
#yamou
pre_yamou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4),day=1, 
                                         pre1=pre.var[which(lon==9.25),which(lat==7.75),1:48],
                                         pre2=pre.var[which(lon==9.25),which(lat==7.25),1:48],
                                         pre3=pre.var[which(lon==8.75),which(lat==7.25),1:48]))
pre_yamou_monthly$Location <- 'yamou'
pre_yamou_monthly$pre <- rowMeans(select(pre_yamou_monthly, pre1, pre2, pre3))
pre_yamou_monthly$date <- ymd(paste(pre_yamou_monthly$Year, pre_yamou_monthly$Month, pre_yamou_monthly$day, sep="-"))
pre_yamou_monthly_2015 <- select(pre_yamou_monthly, Location, Year, Month, pre) %>% group_by( Month) %>% summarize(pre=mean(pre))
pre_yamou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
pre_yamou_monthly_2015a$Location <- 'yamou'
pre_yamou_monthly_2015a$date <- ymd(paste(pre_yamou_monthly_2015a$Year, pre_yamou_monthly_2015a$Month, pre_yamou_monthly_2015a$day, sep="-"))
pre_yamou_monthly_2015a <- select(pre_yamou_monthly_2015a, date, Year, Month, day, Location)
pre_yamou_monthly_2015 <- full_join(pre_yamou_monthly_2015a, pre_yamou_monthly_2015)
pre_yamou_monthly <- rbind(select(pre_yamou_monthly,date, Year, Month, day, Location, pre), pre_yamou_monthly_2015)
rm(pre_yamou_monthly_2015, pre_yamou_monthly_2015a)
pre_yamou_monthly$measurement <- "pre"
pre_yamou_monthly <- rename(pre_yamou_monthly, Value=pre)

#Merging in long format
pre_Guinea_monthly_district <- rbind(pre_Beyla_monthly, pre_Boke_monthly, pre_Boffa_monthly,
                                     pre_Conakry_monthly, pre_Coyah_monthly, pre_Dabola_monthly, pre_Dalaba_monthly,
                                     pre_Dinguiray_monthly, pre_Dubreka_monthly, pre_Faranah_monthly,
                                     pre_Forecariah_monthly, pre_Fria_monthly, pre_Gaoual_monthly,
                                     pre_Gueckedou_monthly, pre_Kankan_monthly, pre_Kerouane_monthly,
                                     pre_Kindia_monthly, pre_Kissidougou_monthly, pre_Koubia_monthly,
                                     pre_Koundara_monthly, pre_Kouroussa_monthly, pre_Labe_monthly,
                                     pre_Lelouma_monthly, pre_Lola_monthly, pre_Macenta_monthly,
                                     pre_Mali_monthly, pre_Mamou_monthly, pre_Nzerekore_monthly,
                                     pre_Pita_monthly, pre_Siguiri_monthly, pre_Telimele_monthly,
                                     pre_Tougue_monthly, pre_yamou_monthly)

#####################
#tmn - min Temp
#####################
tmn.full <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.tmn.dat.nc', write=FALSE)
tmn.var <- var.get.nc(tmn.full, "tmn")
lon <- var.get.nc(tmn.full, "lon")
lat <- var.get.nc(tmn.full, "lat")
#Beyla
tmn_Beyla_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         tmn1=tmn.var[which(lon==8.75),which(lat==9.25),1:48],
                                         tmn2=tmn.var[which(lon==8.25),which(lat==9.25),1:48], 
                                         tmn3=tmn.var[which(lon==7.75),which(lat==9.25),1:48],
                                         tmn4=tmn.var[which(lon==8.75),which(lat==8.75),1:48], 
                                         tmn5=tmn.var[which(lon==8.25),which(lat==8.75),1:48],
                                         tmn6=tmn.var[which(lon==7.75),which(lat==8.75),1:48], 
                                         tmn7=tmn.var[which(lon==8.75),which(lat==8.25),1:48],
                                         tmn8=tmn.var[which(lon==8.25),which(lat==8.25),1:48]))
tmn_Beyla_monthly$tmn <- rowMeans(select(tmn_Beyla_monthly, tmn1, tmn2, tmn3, tmn4, tmn5, tmn6, tmn7, tmn8))
tmn_Beyla_monthly$Location <- 'Beyla'
tmn_Beyla_monthly$date <- ymd(paste(tmn_Beyla_monthly$Year, tmn_Beyla_monthly$Month, tmn_Beyla_monthly$day, sep="-"))
tmn_Beyla_monthly_2015 <- select(tmn_Beyla_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Beyla_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Beyla_monthly_2015a$Location <- 'Beyla'
tmn_Beyla_monthly_2015a$date <- ymd(paste(tmn_Beyla_monthly_2015a$Year, tmn_Beyla_monthly_2015a$Month, tmn_Beyla_monthly_2015a$day, sep="-"))
tmn_Beyla_monthly_2015a <- select(tmn_Beyla_monthly_2015a, date, Year, Month, day, Location)
tmn_Beyla_monthly_2015 <- full_join(tmn_Beyla_monthly_2015a, tmn_Beyla_monthly_2015)
tmn_Beyla_monthly <- rbind(select(tmn_Beyla_monthly,date, Year, Month, day, Location, tmn), tmn_Beyla_monthly_2015)
rm(tmn_Beyla_monthly_2015, tmn_Beyla_monthly_2015a)
tmn_Beyla_monthly$measurement <- "tmn"
tmn_Beyla_monthly <- rename(tmn_Beyla_monthly, Value=tmn)
#Boffa
tmn_Boffa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         tmn1=tmn.var[which(lon==14.25),which(lat==10.75),1:48],
                                         tmn2=tmn.var[which(lon==13.75),which(lat==10.75),1:48], 
                                         tmn3=tmn.var[which(lon==14.25),which(lat==10.25),1:48],
                                         tmn4=tmn.var[which(lon==13.75),which(lat==10.25),1:48]))
tmn_Boffa_monthly$tmn <- rowMeans(select(tmn_Boffa_monthly, tmn1, tmn2, tmn3, tmn4))
tmn_Boffa_monthly$Location <- 'Boffa'
tmn_Boffa_monthly$date <- ymd(paste(tmn_Boffa_monthly$Year, tmn_Boffa_monthly$Month, tmn_Boffa_monthly$day, sep="-"))
tmn_Boffa_monthly_2015 <- select(tmn_Boffa_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Boffa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Boffa_monthly_2015a$Location <- 'Boffa'
tmn_Boffa_monthly_2015a$date <- ymd(paste(tmn_Boffa_monthly_2015a$Year, tmn_Boffa_monthly_2015a$Month, tmn_Boffa_monthly_2015a$day, sep="-"))
tmn_Boffa_monthly_2015a <- select(tmn_Boffa_monthly_2015a, date, Year, Month, day, Location)
tmn_Boffa_monthly_2015 <- full_join(tmn_Boffa_monthly_2015a, tmn_Boffa_monthly_2015)
tmn_Boffa_monthly <- rbind(select(tmn_Boffa_monthly,date, Year, Month, day, Location, tmn), tmn_Boffa_monthly_2015)
rm(tmn_Boffa_monthly_2015, tmn_Boffa_monthly_2015a)
tmn_Boffa_monthly$measurement <- "tmn"
tmn_Boffa_monthly <- rename(tmn_Boffa_monthly, Value=tmn)
#Boke
tmn_Boke_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmn1=tmn.var[which(lon==14.25),which(lat==11.75),1:48],
                                        tmn2=tmn.var[which(lon==14.75),which(lat==11.25),1:48], 
                                        tmn3=tmn.var[which(lon==14.25),which(lat==11.25),1:48],
                                        tmn4=tmn.var[which(lon==13.75),which(lat==11.25),1:48]))
tmn_Boke_monthly$tmn <- rowMeans(select(tmn_Boke_monthly, tmn1, tmn2, tmn3, tmn4))
tmn_Boke_monthly$Location <- 'Boke'
tmn_Boke_monthly$date <- ymd(paste(tmn_Boke_monthly$Year, tmn_Boke_monthly$Month, tmn_Boke_monthly$day, sep="-"))
tmn_Boke_monthly_2015 <- select(tmn_Boke_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Boke_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Boke_monthly_2015a$Location <- 'Boke'
tmn_Boke_monthly_2015a$date <- ymd(paste(tmn_Boke_monthly_2015a$Year, tmn_Boke_monthly_2015a$Month, tmn_Boke_monthly_2015a$day, sep="-"))
tmn_Boke_monthly_2015a <- select(tmn_Boke_monthly_2015a, date, Year, Month, day, Location)
tmn_Boke_monthly_2015 <- full_join(tmn_Boke_monthly_2015a, tmn_Boke_monthly_2015)
tmn_Boke_monthly <- rbind(select(tmn_Boke_monthly,date, Year, Month, day, Location, tmn), tmn_Boke_monthly_2015)
rm(tmn_Boke_monthly_2015, tmn_Boke_monthly_2015a)
tmn_Boke_monthly$measurement <- "tmn"
tmn_Boke_monthly <- rename(tmn_Boke_monthly, Value=tmn)
#Conakry
tmn_Conakry_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmn=tmn.var[which(lon==13.75),which(lat==9.75),1:48]))
tmn_Conakry_monthly$Location <- 'Conakry'
tmn_Conakry_monthly$date <- ymd(paste(tmn_Conakry_monthly$Year, tmn_Conakry_monthly$Month, tmn_Conakry_monthly$day, sep="-"))
tmn_Conakry_monthly_2015 <- select(tmn_Conakry_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Conakry_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Conakry_monthly_2015a$Location <- 'Conakry'
tmn_Conakry_monthly_2015a$date <- ymd(paste(tmn_Conakry_monthly_2015a$Year, tmn_Conakry_monthly_2015a$Month, tmn_Conakry_monthly_2015a$day, sep="-"))
tmn_Conakry_monthly_2015a <- select(tmn_Conakry_monthly_2015a, date, Year, Month, day, Location)
tmn_Conakry_monthly_2015 <- full_join(tmn_Conakry_monthly_2015a, tmn_Conakry_monthly_2015)
tmn_Conakry_monthly <- rbind(select(tmn_Conakry_monthly,date, Year, Month, day, Location, tmn), tmn_Conakry_monthly_2015)
rm(tmn_Conakry_monthly_2015, tmn_Conakry_monthly_2015a)
tmn_Conakry_monthly$measurement <- "tmn"
tmn_Conakry_monthly <- rename(tmn_Conakry_monthly, Value=tmn)
#Coyah
tmn_Coyah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         tmn=tmn.var[which(lon==9.75),which(lat==13.25),1:48]))
tmn_Coyah_monthly$Location <- 'Coyah'
tmn_Coyah_monthly$date <- ymd(paste(tmn_Coyah_monthly$Year, tmn_Coyah_monthly$Month, tmn_Coyah_monthly$day, sep="-"))
tmn_Coyah_monthly_2015 <- select(tmn_Coyah_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Coyah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Coyah_monthly_2015a$Location <- 'Coyah'
tmn_Coyah_monthly_2015a$date <- ymd(paste(tmn_Coyah_monthly_2015a$Year, tmn_Coyah_monthly_2015a$Month, tmn_Coyah_monthly_2015a$day, sep="-"))
tmn_Coyah_monthly_2015a <- select(tmn_Coyah_monthly_2015a, date, Year, Month, day, Location)
tmn_Coyah_monthly_2015 <- full_join(tmn_Coyah_monthly_2015a, tmn_Coyah_monthly_2015)
tmn_Coyah_monthly <- rbind(select(tmn_Coyah_monthly,date, Year, Month, day, Location, tmn), tmn_Coyah_monthly_2015)
rm(tmn_Coyah_monthly_2015, tmn_Coyah_monthly_2015a)
tmn_Coyah_monthly$measurement <- "tmn"
tmn_Coyah_monthly <- rename(tmn_Coyah_monthly, Value=tmn)
#Dabola
tmn_Dabola_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmn1=tmn.var[which(lon==11.25),which(lat==10.75),1:48],
                                          tmn2=tmn.var[which(lon==10.75),which(lat==10.75),1:48]))
tmn_Dabola_monthly$tmn <- rowMeans(select(tmn_Dabola_monthly, tmn1, tmn2))
tmn_Dabola_monthly$Location <- 'Dabola'
tmn_Dabola_monthly$date <- ymd(paste(tmn_Dabola_monthly$Year, tmn_Dabola_monthly$Month, tmn_Dabola_monthly$day, sep="-"))
tmn_Dabola_monthly_2015 <- select(tmn_Dabola_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Dabola_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Dabola_monthly_2015a$Location <- 'Dabola'
tmn_Dabola_monthly_2015a$date <- ymd(paste(tmn_Dabola_monthly_2015a$Year, tmn_Dabola_monthly_2015a$Month, tmn_Dabola_monthly_2015a$day, sep="-"))
tmn_Dabola_monthly_2015a <- select(tmn_Dabola_monthly_2015a, date, Year, Month, day, Location)
tmn_Dabola_monthly_2015 <- full_join(tmn_Dabola_monthly_2015a, tmn_Dabola_monthly_2015)
tmn_Dabola_monthly <- rbind(select(tmn_Dabola_monthly,date, Year, Month, day, Location, tmn), tmn_Dabola_monthly_2015)
rm(tmn_Dabola_monthly_2015, tmn_Dabola_monthly_2015a)
tmn_Dabola_monthly$measurement <- "tmn"
tmn_Dabola_monthly <- rename(tmn_Dabola_monthly, Value=tmn)
#Dalaba
tmn_Dalaba_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmn1=tmn.var[which(lon==11.25),which(lat==12.25),1:48],
                                          tmn2=tmn.var[which(lon==10.75),which(lat==12.25),1:48]))
tmn_Dalaba_monthly$tmn <- rowMeans(select(tmn_Dalaba_monthly, tmn1, tmn2))
tmn_Dalaba_monthly$Location <- 'Dalaba'
tmn_Dalaba_monthly$date <- ymd(paste(tmn_Dalaba_monthly$Year, tmn_Dalaba_monthly$Month, tmn_Dalaba_monthly$day, sep="-"))
tmn_Dalaba_monthly_2015 <- select(tmn_Dalaba_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Dalaba_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Dalaba_monthly_2015a$Location <- 'Dalaba'
tmn_Dalaba_monthly_2015a$date <- ymd(paste(tmn_Dalaba_monthly_2015a$Year, tmn_Dalaba_monthly_2015a$Month, tmn_Dalaba_monthly_2015a$day, sep="-"))
tmn_Dalaba_monthly_2015a <- select(tmn_Dalaba_monthly_2015a, date, Year, Month, day, Location)
tmn_Dalaba_monthly_2015 <- full_join(tmn_Dalaba_monthly_2015a, tmn_Dalaba_monthly_2015)
tmn_Dalaba_monthly <- rbind(select(tmn_Dalaba_monthly,date, Year, Month, day, Location, tmn), tmn_Dalaba_monthly_2015)
rm(tmn_Dalaba_monthly_2015, tmn_Dalaba_monthly_2015a)
tmn_Dalaba_monthly$measurement <- "tmn"
tmn_Dalaba_monthly <- rename(tmn_Dalaba_monthly, Value=tmn)
#Dinguiray
tmn_Dinguiray_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             tmn1=tmn.var[which(lon==11.25),which(lat==11.75),1:48],
                                             tmn2=tmn.var[which(lon==10.75),which(lat==11.75),1:48], 
                                             tmn3=tmn.var[which(lon==10.25),which(lat==11.75),1:48],
                                             tmn4=tmn.var[which(lon==11.25),which(lat==11.25),1:48],
                                             tmn5=tmn.var[which(lon==10.75),which(lat==11.25),1:48],
                                             tmn6=tmn.var[which(lon==10.25),which(lat==11.25),1:48]))
tmn_Dinguiray_monthly$tmn <- rowMeans(select(tmn_Dinguiray_monthly, tmn1, tmn2, tmn3, tmn4, tmn5, tmn6))
tmn_Dinguiray_monthly$Location <- 'Dinguiray'
tmn_Dinguiray_monthly$date <- ymd(paste(tmn_Dinguiray_monthly$Year, tmn_Dinguiray_monthly$Month, tmn_Dinguiray_monthly$day, sep="-"))
tmn_Dinguiray_monthly_2015 <- select(tmn_Dinguiray_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Dinguiray_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Dinguiray_monthly_2015a$Location <- 'Dinguiray'
tmn_Dinguiray_monthly_2015a$date <- ymd(paste(tmn_Dinguiray_monthly_2015a$Year, tmn_Dinguiray_monthly_2015a$Month, tmn_Dinguiray_monthly_2015a$day, sep="-"))
tmn_Dinguiray_monthly_2015a <- select(tmn_Dinguiray_monthly_2015a, date, Year, Month, day, Location)
tmn_Dinguiray_monthly_2015 <- full_join(tmn_Dinguiray_monthly_2015a, tmn_Dinguiray_monthly_2015)
tmn_Dinguiray_monthly <- rbind(select(tmn_Dinguiray_monthly,date, Year, Month, day, Location, tmn), tmn_Dinguiray_monthly_2015)
rm(tmn_Dinguiray_monthly_2015, tmn_Dinguiray_monthly_2015a)
tmn_Dinguiray_monthly$measurement <- "tmn"
tmn_Dinguiray_monthly <- rename(tmn_Dinguiray_monthly, Value=tmn)
#Dubreka
tmn_Dubreka_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmn1=tmn.var[which(lon==13.75),which(lat==10.25),1:48],
                                           tmn2=tmn.var[which(lon==13.25),which(lat==10.25),1:48]))
tmn_Dubreka_monthly$tmn <- rowMeans(select(tmn_Dubreka_monthly, tmn1, tmn2))
tmn_Dubreka_monthly$Location <- 'Dubreka'
tmn_Dubreka_monthly$date <- ymd(paste(tmn_Dubreka_monthly$Year, tmn_Dubreka_monthly$Month, tmn_Dubreka_monthly$day, sep="-"))
tmn_Dubreka_monthly_2015 <- select(tmn_Dubreka_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Dubreka_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Dubreka_monthly_2015a$Location <- 'Dubreka'
tmn_Dubreka_monthly_2015a$date <- ymd(paste(tmn_Dubreka_monthly_2015a$Year, tmn_Dubreka_monthly_2015a$Month, tmn_Dubreka_monthly_2015a$day, sep="-"))
tmn_Dubreka_monthly_2015a <- select(tmn_Dubreka_monthly_2015a, date, Year, Month, day, Location)
tmn_Dubreka_monthly_2015 <- full_join(tmn_Dubreka_monthly_2015a, tmn_Dubreka_monthly_2015)
tmn_Dubreka_monthly <- rbind(select(tmn_Dubreka_monthly,date, Year, Month, day, Location, tmn), tmn_Dubreka_monthly_2015)
rm(tmn_Dubreka_monthly_2015, tmn_Dubreka_monthly_2015a)
tmn_Dubreka_monthly$measurement <- "tmn"
tmn_Dubreka_monthly <- rename(tmn_Dubreka_monthly, Value=tmn)
#Faranah
tmn_Faranah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmn1=tmn.var[which(lon==11.25),which(lat==10.25),1:48],
                                           tmn2=tmn.var[which(lon==10.75),which(lat==10.25),1:48], 
                                           tmn3=tmn.var[which(lon==10.25),which(lat==10.25),1:48],
                                           tmn4=tmn.var[which(lon==10.75),which(lat==9.75),1:48],
                                           tmn5=tmn.var[which(lon==10.75),which(lat==9.25),1:48]))
tmn_Faranah_monthly$tmn <- rowMeans(select(tmn_Faranah_monthly, tmn1, tmn2, tmn3, tmn4, tmn5))
tmn_Faranah_monthly$Location <- 'Faranah'
tmn_Faranah_monthly$date <- ymd(paste(tmn_Faranah_monthly$Year, tmn_Faranah_monthly$Month, tmn_Faranah_monthly$day, sep="-"))
tmn_Faranah_monthly_2015 <- select(tmn_Faranah_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Faranah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Faranah_monthly_2015a$Location <- 'Faranah'
tmn_Faranah_monthly_2015a$date <- ymd(paste(tmn_Faranah_monthly_2015a$Year, tmn_Faranah_monthly_2015a$Month, tmn_Faranah_monthly_2015a$day, sep="-"))
tmn_Faranah_monthly_2015a <- select(tmn_Faranah_monthly_2015a, date, Year, Month, day, Location)
tmn_Faranah_monthly_2015 <- full_join(tmn_Faranah_monthly_2015a, tmn_Faranah_monthly_2015)
tmn_Faranah_monthly <- rbind(select(tmn_Faranah_monthly,date, Year, Month, day, Location, tmn), tmn_Faranah_monthly_2015)
rm(tmn_Faranah_monthly_2015, tmn_Faranah_monthly_2015a)
tmn_Faranah_monthly$measurement <- "tmn"
tmn_Faranah_monthly <- rename(tmn_Faranah_monthly, Value=tmn)
#Forecariah
tmn_Forecariah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                              Month=rep(seq(1,12,1), 4), day=1,
                                              tmn1=tmn.var[which(lon==12.75),which(lat==9.75),1:48],
                                              tmn2=tmn.var[which(lon==13.25),which(lat==9.25),1:48], 
                                              tmn3=tmn.var[which(lon==12.75),which(lat==9.25),1:48]))
tmn_Forecariah_monthly$tmn <- rowMeans(select(tmn_Forecariah_monthly, tmn1, tmn2, tmn3))
tmn_Forecariah_monthly$Location <- 'Forecariah'
tmn_Forecariah_monthly$date <- ymd(paste(tmn_Forecariah_monthly$Year, tmn_Forecariah_monthly$Month, tmn_Forecariah_monthly$day, sep="-"))
tmn_Forecariah_monthly_2015 <- select(tmn_Forecariah_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Forecariah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Forecariah_monthly_2015a$Location <- 'Forecariah'
tmn_Forecariah_monthly_2015a$date <- ymd(paste(tmn_Forecariah_monthly_2015a$Year, tmn_Forecariah_monthly_2015a$Month, tmn_Forecariah_monthly_2015a$day, sep="-"))
tmn_Forecariah_monthly_2015a <- select(tmn_Forecariah_monthly_2015a, date, Year, Month, day, Location)
tmn_Forecariah_monthly_2015 <- full_join(tmn_Forecariah_monthly_2015a, tmn_Forecariah_monthly_2015)
tmn_Forecariah_monthly <- rbind(select(tmn_Forecariah_monthly,date, Year, Month, day, Location, tmn), tmn_Forecariah_monthly_2015)
rm(tmn_Forecariah_monthly_2015, tmn_Forecariah_monthly_2015a)
tmn_Forecariah_monthly$measurement <- "tmn"
tmn_Forecariah_monthly <- rename(tmn_Forecariah_monthly, Value=tmn)
#Fria
tmn_Fria_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmn1=tmn.var[which(lon==13.75),which(lat==10.75),1:48],
                                        tmn2=tmn.var[which(lon==13.75),which(lat==10.25),1:48]))
tmn_Fria_monthly$tmn <- rowMeans(select(tmn_Fria_monthly, tmn1, tmn2))
tmn_Fria_monthly$Location <- 'Fria'
tmn_Fria_monthly$date <- ymd(paste(tmn_Fria_monthly$Year, tmn_Fria_monthly$Month, tmn_Fria_monthly$day, sep="-"))
tmn_Fria_monthly_2015 <- select(tmn_Fria_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Fria_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Fria_monthly_2015a$Location <- 'Fria'
tmn_Fria_monthly_2015a$date <- ymd(paste(tmn_Fria_monthly_2015a$Year, tmn_Fria_monthly_2015a$Month, tmn_Fria_monthly_2015a$day, sep="-"))
tmn_Fria_monthly_2015a <- select(tmn_Fria_monthly_2015a, date, Year, Month, day, Location)
tmn_Fria_monthly_2015 <- full_join(tmn_Fria_monthly_2015a, tmn_Fria_monthly_2015)
tmn_Fria_monthly <- rbind(select(tmn_Fria_monthly,date, Year, Month, day, Location, tmn), tmn_Fria_monthly_2015)
rm(tmn_Fria_monthly_2015, tmn_Fria_monthly_2015a)
tmn_Fria_monthly$measurement <- "tmn"
tmn_Fria_monthly <- rename(tmn_Fria_monthly, Value=tmn)
#Gaoual
tmn_Gaoual_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmn1=tmn.var[which(lon==13.75),which(lat==12.25),1:48],
                                          tmn2=tmn.var[which(lon==13.25),which(lat==12.25),1:48], 
                                          tmn3=tmn.var[which(lon==13.75),which(lat==11.75),1:48],
                                          tmn4=tmn.var[which(lon==13.25),which(lat==11.75),1:48],
                                          tmn5=tmn.var[which(lon==12.75),which(lat==11.75),1:48],
                                          tmn6=tmn.var[which(lon==13.75),which(lat==11.25),1:48],
                                          tmn7=tmn.var[which(lon==13.25),which(lat==11.25),1:48]))
tmn_Gaoual_monthly$tmn <- rowMeans(select(tmn_Gaoual_monthly, tmn1, tmn2, tmn3, tmn4, tmn5, tmn6, tmn7))
tmn_Gaoual_monthly$Location <- 'Gaoual'
tmn_Gaoual_monthly$date <- ymd(paste(tmn_Gaoual_monthly$Year, tmn_Gaoual_monthly$Month, tmn_Gaoual_monthly$day, sep="-"))
tmn_Gaoual_monthly_2015 <- select(tmn_Gaoual_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Gaoual_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Gaoual_monthly_2015a$Location <- 'Gaoual'
tmn_Gaoual_monthly_2015a$date <- ymd(paste(tmn_Gaoual_monthly_2015a$Year, tmn_Gaoual_monthly_2015a$Month, tmn_Gaoual_monthly_2015a$day, sep="-"))
tmn_Gaoual_monthly_2015a <- select(tmn_Gaoual_monthly_2015a, date, Year, Month, day, Location)
tmn_Gaoual_monthly_2015 <- full_join(tmn_Gaoual_monthly_2015a, tmn_Gaoual_monthly_2015)
tmn_Gaoual_monthly <- rbind(select(tmn_Gaoual_monthly,date, Year, Month, day, Location, tmn), tmn_Gaoual_monthly_2015)
rm(tmn_Gaoual_monthly_2015, tmn_Gaoual_monthly_2015a)
tmn_Gaoual_monthly$measurement <- "tmn"
tmn_Gaoual_monthly <- rename(tmn_Gaoual_monthly, Value=tmn)
#Gueckedou
tmn_Gueckedou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             tmn=tmn.var[which(lon==10.25),which(lat==8.75),1:48]))
tmn_Gueckedou_monthly$Location <- 'Gueckedou'
tmn_Gueckedou_monthly$date <- ymd(paste(tmn_Gueckedou_monthly$Year, tmn_Gueckedou_monthly$Month, tmn_Gueckedou_monthly$day, sep="-"))
tmn_Gueckedou_monthly_2015 <- select(tmn_Gueckedou_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Gueckedou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Gueckedou_monthly_2015a$Location <- 'Gueckedou'
tmn_Gueckedou_monthly_2015a$date <- ymd(paste(tmn_Gueckedou_monthly_2015a$Year, tmn_Gueckedou_monthly_2015a$Month, tmn_Gueckedou_monthly_2015a$day, sep="-"))
tmn_Gueckedou_monthly_2015a <- select(tmn_Gueckedou_monthly_2015a, date, Year, Month, day, Location)
tmn_Gueckedou_monthly_2015 <- full_join(tmn_Gueckedou_monthly_2015a, tmn_Gueckedou_monthly_2015)
tmn_Gueckedou_monthly <- rbind(select(tmn_Gueckedou_monthly,date, Year, Month, day, Location, tmn), tmn_Gueckedou_monthly_2015)
rm(tmn_Gueckedou_monthly_2015, tmn_Gueckedou_monthly_2015a)
tmn_Gueckedou_monthly$measurement <- "tmn"
tmn_Gueckedou_monthly <- rename(tmn_Gueckedou_monthly, Value=tmn)
#Kankan
tmn_Kankan_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmn1=tmn.var[which(lon==9.25),which(lat==10.75),1:48],
                                          tmn2=tmn.var[which(lon==9.75),which(lat==10.25),1:48], 
                                          tmn3=tmn.var[which(lon==9.25),which(lat==10.25),1:48],
                                          tmn4=tmn.var[which(lon==8.75),which(lat==10.25),1:48],
                                          tmn5=tmn.var[which(lon==9.75),which(lat==9.75),1:48],
                                          tmn6=tmn.var[which(lon==9.25),which(lat==9.75),1:48]))
tmn_Kankan_monthly$tmn <- rowMeans(select(tmn_Kankan_monthly, tmn1, tmn2, tmn3, tmn4, tmn5, tmn6))
tmn_Kankan_monthly$Location <- 'Kankan'
tmn_Kankan_monthly$date <- ymd(paste(tmn_Kankan_monthly$Year, tmn_Kankan_monthly$Month, tmn_Kankan_monthly$day, sep="-"))
tmn_Kankan_monthly_2015 <- select(tmn_Kankan_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Kankan_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Kankan_monthly_2015a$Location <- 'Kankan'
tmn_Kankan_monthly_2015a$date <- ymd(paste(tmn_Kankan_monthly_2015a$Year, tmn_Kankan_monthly_2015a$Month, tmn_Kankan_monthly_2015a$day, sep="-"))
tmn_Kankan_monthly_2015a <- select(tmn_Kankan_monthly_2015a, date, Year, Month, day, Location)
tmn_Kankan_monthly_2015 <- full_join(tmn_Kankan_monthly_2015a, tmn_Kankan_monthly_2015)
tmn_Kankan_monthly <- rbind(select(tmn_Kankan_monthly,date, Year, Month, day, Location, tmn), tmn_Kankan_monthly_2015)
rm(tmn_Kankan_monthly_2015, tmn_Kankan_monthly_2015a)
tmn_Kankan_monthly$measurement <- "tmn"
tmn_Kankan_monthly <- rename(tmn_Kankan_monthly, Value=tmn)
#Kerouane
tmn_Kerouane_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            tmn1=tmn.var[which(lon==9.25),which(lat==9.75),1:48],
                                            tmn2=tmn.var[which(lon==8.75),which(lat==9.75),1:48], 
                                            tmn3=tmn.var[which(lon==9.75),which(lat==9.25),1:48],
                                            tmn4=tmn.var[which(lon==9.25),which(lat==9.25),1:48],
                                            tmn5=tmn.var[which(lon==8.75),which(lat==9.25),1:48],
                                            tmn6=tmn.var[which(lon==9.25),which(lat==8.75),1:48]))
tmn_Kerouane_monthly$tmn <- rowMeans(select(tmn_Kerouane_monthly, tmn1, tmn2, tmn3, tmn4, tmn5, tmn6))
tmn_Kerouane_monthly$Location <- 'Kerouane'
tmn_Kerouane_monthly$date <- ymd(paste(tmn_Kerouane_monthly$Year, tmn_Kerouane_monthly$Month, tmn_Kerouane_monthly$day, sep="-"))
tmn_Kerouane_monthly_2015 <- select(tmn_Kerouane_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Kerouane_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Kerouane_monthly_2015a$Location <- 'Kerouane'
tmn_Kerouane_monthly_2015a$date <- ymd(paste(tmn_Kerouane_monthly_2015a$Year, tmn_Kerouane_monthly_2015a$Month, tmn_Kerouane_monthly_2015a$day, sep="-"))
tmn_Kerouane_monthly_2015a <- select(tmn_Kerouane_monthly_2015a, date, Year, Month, day, Location)
tmn_Kerouane_monthly_2015 <- full_join(tmn_Kerouane_monthly_2015a, tmn_Kerouane_monthly_2015)
tmn_Kerouane_monthly <- rbind(select(tmn_Kerouane_monthly,date, Year, Month, day, Location, tmn), tmn_Kerouane_monthly_2015)
rm(tmn_Kerouane_monthly_2015, tmn_Kerouane_monthly_2015a)
tmn_Kerouane_monthly$measurement <- "tmn"
tmn_Kerouane_monthly <- rename(tmn_Kerouane_monthly, Value=tmn)
#Kindia
tmn_Kindia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmn1=tmn.var[which(lon==13.25),which(lat==10.25),1:48],
                                          tmn2=tmn.var[which(lon==12.75),which(lat==10.25),1:48], 
                                          tmn3=tmn.var[which(lon==12.25),which(lat==10.25),1:48],
                                          tmn4=tmn.var[which(lon==12.75),which(lat==9.75),1:48]))
tmn_Kindia_monthly$tmn <- rowMeans(select(tmn_Kindia_monthly, tmn1, tmn2, tmn3, tmn4))
tmn_Kindia_monthly$Location <- 'Kindia'
tmn_Kindia_monthly$date <- ymd(paste(tmn_Kindia_monthly$Year, tmn_Kindia_monthly$Month, tmn_Kindia_monthly$day, sep="-"))
tmn_Kindia_monthly_2015 <- select(tmn_Kindia_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Kindia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Kindia_monthly_2015a$Location <- 'Kindia'
tmn_Kindia_monthly_2015a$date <- ymd(paste(tmn_Kindia_monthly_2015a$Year, tmn_Kindia_monthly_2015a$Month, tmn_Kindia_monthly_2015a$day, sep="-"))
tmn_Kindia_monthly_2015a <- select(tmn_Kindia_monthly_2015a, date, Year, Month, day, Location)
tmn_Kindia_monthly_2015 <- full_join(tmn_Kindia_monthly_2015a, tmn_Kindia_monthly_2015)
tmn_Kindia_monthly <- rbind(select(tmn_Kindia_monthly,date, Year, Month, day, Location, tmn), tmn_Kindia_monthly_2015)
rm(tmn_Kindia_monthly_2015, tmn_Kindia_monthly_2015a)
tmn_Kindia_monthly$measurement <- "tmn"
tmn_Kindia_monthly <- rename(tmn_Kindia_monthly, Value=tmn)
#KISSIDOUGOU
tmn_Kissidougou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               tmn1=tmn.var[which(lon==10.25),which(lat==9.75),1:48],
                                               tmn2=tmn.var[which(lon==10.25),which(lat==9.25),1:48], 
                                               tmn3=tmn.var[which(lon==9.75),which(lat==9.25),1:48]))
tmn_Kissidougou_monthly$tmn <- rowMeans(select(tmn_Kissidougou_monthly, tmn1, tmn2, tmn3))
tmn_Kissidougou_monthly$Location <- 'Kissidougou'
tmn_Kissidougou_monthly$date <- ymd(paste(tmn_Kissidougou_monthly$Year, tmn_Kissidougou_monthly$Month, tmn_Kissidougou_monthly$day, sep="-"))
tmn_Kissidougou_monthly_2015 <- select(tmn_Kissidougou_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Kissidougou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Kissidougou_monthly_2015a$Location <- 'Kissidougou'
tmn_Kissidougou_monthly_2015a$date <- ymd(paste(tmn_Kissidougou_monthly_2015a$Year, tmn_Kissidougou_monthly_2015a$Month, tmn_Kissidougou_monthly_2015a$day, sep="-"))
tmn_Kissidougou_monthly_2015a <- select(tmn_Kissidougou_monthly_2015a, date, Year, Month, day, Location)
tmn_Kissidougou_monthly_2015 <- full_join(tmn_Kissidougou_monthly_2015a, tmn_Kissidougou_monthly_2015)
tmn_Kissidougou_monthly <- rbind(select(tmn_Kissidougou_monthly,date, Year, Month, day, Location, tmn), tmn_Kissidougou_monthly_2015)
rm(tmn_Kissidougou_monthly_2015, tmn_Kissidougou_monthly_2015a)
tmn_Kissidougou_monthly$measurement <- "tmn"
tmn_Kissidougou_monthly <- rename(tmn_Kissidougou_monthly, Value=tmn)
#Koubia
tmn_Koubia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmn=tmn.var[which(lon==10.25),which(lat==9.75),1:48]))
tmn_Koubia_monthly$Location <- 'Koubia'
tmn_Koubia_monthly$date <- ymd(paste(tmn_Koubia_monthly$Year, tmn_Koubia_monthly$Month, tmn_Koubia_monthly$day, sep="-"))
tmn_Koubia_monthly_2015 <- select(tmn_Koubia_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Koubia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Koubia_monthly_2015a$Location <- 'Koubia'
tmn_Koubia_monthly_2015a$date <- ymd(paste(tmn_Koubia_monthly_2015a$Year, tmn_Koubia_monthly_2015a$Month, tmn_Koubia_monthly_2015a$day, sep="-"))
tmn_Koubia_monthly_2015a <- select(tmn_Koubia_monthly_2015a, date, Year, Month, day, Location)
tmn_Koubia_monthly_2015 <- full_join(tmn_Koubia_monthly_2015a, tmn_Koubia_monthly_2015)
tmn_Koubia_monthly <- rbind(select(tmn_Koubia_monthly,date, Year, Month, day, Location, tmn), tmn_Koubia_monthly_2015)
rm(tmn_Koubia_monthly_2015, tmn_Koubia_monthly_2015a)
tmn_Koubia_monthly$measurement <- "tmn"
tmn_Koubia_monthly <- rename(tmn_Koubia_monthly, Value=tmn)
#Koundara
tmn_Koundara_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            tmn1=tmn.var[which(lon==13.25),which(lat==12.75),1:48],
                                            tmn2=tmn.var[which(lon==13.25),which(lat==12.25),1:48], 
                                            tmn3=tmn.var[which(lon==12.75),which(lat==12.25),1:48]))
tmn_Koundara_monthly$tmn <- rowMeans(select(tmn_Koundara_monthly, tmn1, tmn2, tmn3))
tmn_Koundara_monthly$Location <- 'Koundara'
tmn_Koundara_monthly$date <- ymd(paste(tmn_Koundara_monthly$Year, tmn_Koundara_monthly$Month, tmn_Koundara_monthly$day, sep="-"))
tmn_Koundara_monthly_2015 <- select(tmn_Koundara_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Koundara_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Koundara_monthly_2015a$Location <- 'Koundara'
tmn_Koundara_monthly_2015a$date <- ymd(paste(tmn_Koundara_monthly_2015a$Year, tmn_Koundara_monthly_2015a$Month, tmn_Koundara_monthly_2015a$day, sep="-"))
tmn_Koundara_monthly_2015a <- select(tmn_Koundara_monthly_2015a, date, Year, Month, day, Location)
tmn_Koundara_monthly_2015 <- full_join(tmn_Koundara_monthly_2015a, tmn_Koundara_monthly_2015)
tmn_Koundara_monthly <- rbind(select(tmn_Koundara_monthly,date, Year, Month, day, Location, tmn), tmn_Koundara_monthly_2015)
rm(tmn_Koundara_monthly_2015, tmn_Koundara_monthly_2015a)
tmn_Koundara_monthly$measurement <- "tmn"
tmn_Koundara_monthly <- rename(tmn_Koundara_monthly, Value=tmn)
#Kouroussa
tmn_Kouroussa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             tmn1=tmn.var[which(lon==10.25),which(lat==11.25),1:48],
                                             tmn2=tmn.var[which(lon==10.75),which(lat==10.75),1:48], 
                                             tmn3=tmn.var[which(lon==10.25),which(lat==10.75),1:48],
                                             tmn4=tmn.var[which(lon==9.75),which(lat==10.75),1:48],
                                             tmn5=tmn.var[which(lon==10.25),which(lat==10.25),1:48],
                                             tmn6=tmn.var[which(lon==9.75),which(lat==10.25),1:48],
                                             tmn7=tmn.var[which(lon==10.25),which(lat==9.75),1:48],
                                             tmn8=tmn.var[which(lon==9.75),which(lat==9.75),1:48]))
tmn_Kouroussa_monthly$tmn <- rowMeans(select(tmn_Kouroussa_monthly, tmn1, tmn2, tmn3, tmn4, tmn5, tmn6, tmn7, tmn8))
tmn_Kouroussa_monthly$Location <- 'Kouroussa'
tmn_Kouroussa_monthly$date <- ymd(paste(tmn_Kouroussa_monthly$Year, tmn_Kouroussa_monthly$Month, tmn_Kouroussa_monthly$day, sep="-"))
tmn_Kouroussa_monthly_2015 <- select(tmn_Kouroussa_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Kouroussa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Kouroussa_monthly_2015a$Location <- 'Kouroussa'
tmn_Kouroussa_monthly_2015a$date <- ymd(paste(tmn_Kouroussa_monthly_2015a$Year, tmn_Kouroussa_monthly_2015a$Month, tmn_Kouroussa_monthly_2015a$day, sep="-"))
tmn_Kouroussa_monthly_2015a <- select(tmn_Kouroussa_monthly_2015a, date, Year, Month, day, Location)
tmn_Kouroussa_monthly_2015 <- full_join(tmn_Kouroussa_monthly_2015a, tmn_Kouroussa_monthly_2015)
tmn_Kouroussa_monthly <- rbind(select(tmn_Kouroussa_monthly,date, Year, Month, day, Location, tmn), tmn_Kouroussa_monthly_2015)
rm(tmn_Kouroussa_monthly_2015, tmn_Kouroussa_monthly_2015a)
tmn_Kouroussa_monthly$measurement <- "tmn"
tmn_Kouroussa_monthly <- rename(tmn_Kouroussa_monthly, Value=tmn)
#Labe
tmn_Labe_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmn1=tmn.var[which(lon==12.25),which(lat==11.75),1:48],
                                        tmn2=tmn.var[which(lon==12.25),which(lat==11.25),1:48]))
tmn_Labe_monthly$tmn <- rowMeans(select(tmn_Labe_monthly, tmn1, tmn2))
tmn_Labe_monthly$Location <- 'Labe'
tmn_Labe_monthly$date <- ymd(paste(tmn_Labe_monthly$Year, tmn_Labe_monthly$Month, tmn_Labe_monthly$day, sep="-"))
tmn_Labe_monthly_2015 <- select(tmn_Labe_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Labe_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Labe_monthly_2015a$Location <- 'Labe'
tmn_Labe_monthly_2015a$date <- ymd(paste(tmn_Labe_monthly_2015a$Year, tmn_Labe_monthly_2015a$Month, tmn_Labe_monthly_2015a$day, sep="-"))
tmn_Labe_monthly_2015a <- select(tmn_Labe_monthly_2015a, date, Year, Month, day, Location)
tmn_Labe_monthly_2015 <- full_join(tmn_Labe_monthly_2015a, tmn_Labe_monthly_2015)
tmn_Labe_monthly <- rbind(select(tmn_Labe_monthly,date, Year, Month, day, Location, tmn), tmn_Labe_monthly_2015)
rm(tmn_Labe_monthly_2015, tmn_Labe_monthly_2015a)
tmn_Labe_monthly$measurement <- "tmn"
tmn_Labe_monthly <- rename(tmn_Labe_monthly, Value=tmn)
#Lelouma
tmn_Lelouma_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmn1=tmn.var[which(lon==12.75),which(lat==11.75),1:48],
                                           tmn2=tmn.var[which(lon==12.75),which(lat==11.25),1:48]))
tmn_Lelouma_monthly$tmn <- rowMeans(select(tmn_Lelouma_monthly, tmn1, tmn2))
tmn_Lelouma_monthly$Location <- 'Lelouma'
tmn_Lelouma_monthly$date <- ymd(paste(tmn_Lelouma_monthly$Year, tmn_Lelouma_monthly$Month, tmn_Lelouma_monthly$day, sep="-"))
tmn_Lelouma_monthly_2015 <- select(tmn_Lelouma_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Lelouma_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Lelouma_monthly_2015a$Location <- 'Lelouma'
tmn_Lelouma_monthly_2015a$date <- ymd(paste(tmn_Lelouma_monthly_2015a$Year, tmn_Lelouma_monthly_2015a$Month, tmn_Lelouma_monthly_2015a$day, sep="-"))
tmn_Lelouma_monthly_2015a <- select(tmn_Lelouma_monthly_2015a, date, Year, Month, day, Location)
tmn_Lelouma_monthly_2015 <- full_join(tmn_Lelouma_monthly_2015a, tmn_Lelouma_monthly_2015)
tmn_Lelouma_monthly <- rbind(select(tmn_Lelouma_monthly,date, Year, Month, day, Location, tmn), tmn_Lelouma_monthly_2015)
rm(tmn_Lelouma_monthly_2015, tmn_Lelouma_monthly_2015a)
tmn_Lelouma_monthly$measurement <- "tmn"
tmn_Lelouma_monthly <- rename(tmn_Lelouma_monthly, Value=tmn)
#Lola
tmn_Lola_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmn1=tmn.var[which(lon==8.25),which(lat==8.25),1:48],
                                        tmn2=tmn.var[which(lon==8.25),which(lat==7.75),1:48]))
tmn_Lola_monthly$tmn <- rowMeans(select(tmn_Lola_monthly, tmn1, tmn2))
tmn_Lola_monthly$Location <- 'Lola'
tmn_Lola_monthly$date <- ymd(paste(tmn_Lola_monthly$Year, tmn_Lola_monthly$Month, tmn_Lola_monthly$day, sep="-"))
tmn_Lola_monthly_2015 <- select(tmn_Lola_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Lola_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Lola_monthly_2015a$Location <- 'Lola'
tmn_Lola_monthly_2015a$date <- ymd(paste(tmn_Lola_monthly_2015a$Year, tmn_Lola_monthly_2015a$Month, tmn_Lola_monthly_2015a$day, sep="-"))
tmn_Lola_monthly_2015a <- select(tmn_Lola_monthly_2015a, date, Year, Month, day, Location)
tmn_Lola_monthly_2015 <- full_join(tmn_Lola_monthly_2015a, tmn_Lola_monthly_2015)
tmn_Lola_monthly <- rbind(select(tmn_Lola_monthly,date, Year, Month, day, Location, tmn), tmn_Lola_monthly_2015)
rm(tmn_Lola_monthly_2015, tmn_Lola_monthly_2015a)
tmn_Lola_monthly$measurement <- "tmn"
tmn_Lola_monthly <- rename(tmn_Lola_monthly, Value=tmn)
#Macenta
tmn_Macenta_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmn1=tmn.var[which(lon==9.75),which(lat==8.75),1:48],
                                           tmn2=tmn.var[which(lon==9.25),which(lat==8.75),1:48],
                                           tmn3=tmn.var[which(lon==9.25),which(lat==8.25),1:48]))
tmn_Macenta_monthly$tmn <- rowMeans(select(tmn_Macenta_monthly, tmn1, tmn2, tmn3))
tmn_Macenta_monthly$Location <- 'Macenta'
tmn_Macenta_monthly$date <- ymd(paste(tmn_Macenta_monthly$Year, tmn_Macenta_monthly$Month, tmn_Macenta_monthly$day, sep="-"))
tmn_Macenta_monthly_2015 <- select(tmn_Macenta_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Macenta_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Macenta_monthly_2015a$Location <- 'Macenta'
tmn_Macenta_monthly_2015a$date <- ymd(paste(tmn_Macenta_monthly_2015a$Year, tmn_Macenta_monthly_2015a$Month, tmn_Macenta_monthly_2015a$day, sep="-"))
tmn_Macenta_monthly_2015a <- select(tmn_Macenta_monthly_2015a, date, Year, Month, day, Location)
tmn_Macenta_monthly_2015 <- full_join(tmn_Macenta_monthly_2015a, tmn_Macenta_monthly_2015)
tmn_Macenta_monthly <- rbind(select(tmn_Macenta_monthly,date, Year, Month, day, Location, tmn), tmn_Macenta_monthly_2015)
rm(tmn_Macenta_monthly_2015, tmn_Macenta_monthly_2015a)
tmn_Macenta_monthly$measurement <- "tmn"
tmn_Macenta_monthly <- rename(tmn_Macenta_monthly, Value=tmn)
#Mali
tmn_Mali_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmn1=tmn.var[which(lon==12.75),which(lat==12.25),1:48],
                                        tmn2=tmn.var[which(lon==12.25),which(lat==12.25),1:48], 
                                        tmn3=tmn.var[which(lon==11.75),which(lat==12.25),1:48],
                                        tmn4=tmn.var[which(lon==12.75),which(lat==11.75),1:48],
                                        tmn5=tmn.var[which(lon==12.25),which(lat==11.75),1:48]))
tmn_Mali_monthly$tmn <- rowMeans(select(tmn_Mali_monthly, tmn1, tmn2, tmn3, tmn4, tmn5))
tmn_Mali_monthly$Location <- 'Mali'
tmn_Mali_monthly$date <- ymd(paste(tmn_Mali_monthly$Year, tmn_Mali_monthly$Month, tmn_Mali_monthly$day, sep="-"))
tmn_Mali_monthly_2015 <- select(tmn_Mali_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Mali_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Mali_monthly_2015a$Location <- 'Mali'
tmn_Mali_monthly_2015a$date <- ymd(paste(tmn_Mali_monthly_2015a$Year, tmn_Mali_monthly_2015a$Month, tmn_Mali_monthly_2015a$day, sep="-"))
tmn_Mali_monthly_2015a <- select(tmn_Mali_monthly_2015a, date, Year, Month, day, Location)
tmn_Mali_monthly_2015 <- full_join(tmn_Mali_monthly_2015a, tmn_Mali_monthly_2015)
tmn_Mali_monthly <- rbind(select(tmn_Mali_monthly,date, Year, Month, day, Location, tmn), tmn_Mali_monthly_2015)
rm(tmn_Mali_monthly_2015, tmn_Mali_monthly_2015a)
tmn_Mali_monthly$measurement <- "tmn"
tmn_Mali_monthly <- rename(tmn_Mali_monthly, Value=tmn)
#Mamou
tmn_Mamou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         tmn1=tmn.var[which(lon==11.75),which(lat==10.75),1:48],
                                         tmn2=tmn.var[which(lon==12.25),which(lat==10.25),1:48],
                                         tmn3=tmn.var[which(lon==11.75),which(lat==10.25),1:48]))
tmn_Mamou_monthly$tmn <- rowMeans(select(tmn_Mamou_monthly, tmn1, tmn2, tmn3))
tmn_Mamou_monthly$Location <- 'Mamou'
tmn_Mamou_monthly$date <- ymd(paste(tmn_Mamou_monthly$Year, tmn_Mamou_monthly$Month, tmn_Mamou_monthly$day, sep="-"))
tmn_Mamou_monthly_2015 <- select(tmn_Mamou_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Mamou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Mamou_monthly_2015a$Location <- 'Mamou'
tmn_Mamou_monthly_2015a$date <- ymd(paste(tmn_Mamou_monthly_2015a$Year, tmn_Mamou_monthly_2015a$Month, tmn_Mamou_monthly_2015a$day, sep="-"))
tmn_Mamou_monthly_2015a <- select(tmn_Mamou_monthly_2015a, date, Year, Month, day, Location)
tmn_Mamou_monthly_2015 <- full_join(tmn_Mamou_monthly_2015a, tmn_Mamou_monthly_2015)
tmn_Mamou_monthly <- rbind(select(tmn_Mamou_monthly,date, Year, Month, day, Location, tmn), tmn_Mamou_monthly_2015)
rm(tmn_Mamou_monthly_2015, tmn_Mamou_monthly_2015a)
tmn_Mamou_monthly$measurement <- "tmn"
tmn_Mamou_monthly <- rename(tmn_Mamou_monthly, Value=tmn)
#Nzerekore
tmn_Nzerekore_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             tmn1=tmn.var[which(lon==8.75),which(lat==8.25),1:48],
                                             tmn2=tmn.var[which(lon==8.75),which(lat==7.75),1:48]))
tmn_Nzerekore_monthly$tmn <- rowMeans(select(tmn_Nzerekore_monthly, tmn1, tmn2))
tmn_Nzerekore_monthly$Location <- 'Nzerekore'
tmn_Nzerekore_monthly$date <- ymd(paste(tmn_Nzerekore_monthly$Year, tmn_Nzerekore_monthly$Month, tmn_Nzerekore_monthly$day, sep="-"))
tmn_Nzerekore_monthly_2015 <- select(tmn_Nzerekore_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Nzerekore_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Nzerekore_monthly_2015a$Location <- 'Nzerekore'
tmn_Nzerekore_monthly_2015a$date <- ymd(paste(tmn_Nzerekore_monthly_2015a$Year, tmn_Nzerekore_monthly_2015a$Month, tmn_Nzerekore_monthly_2015a$day, sep="-"))
tmn_Nzerekore_monthly_2015a <- select(tmn_Nzerekore_monthly_2015a, date, Year, Month, day, Location)
tmn_Nzerekore_monthly_2015 <- full_join(tmn_Nzerekore_monthly_2015a, tmn_Nzerekore_monthly_2015)
tmn_Nzerekore_monthly <- rbind(select(tmn_Nzerekore_monthly,date, Year, Month, day, Location, tmn), tmn_Nzerekore_monthly_2015)
rm(tmn_Nzerekore_monthly_2015, tmn_Nzerekore_monthly_2015a)
tmn_Nzerekore_monthly$measurement <- "tmn"
tmn_Nzerekore_monthly <- rename(tmn_Nzerekore_monthly, Value=tmn)
#Pita
tmn_Pita_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmn1=tmn.var[which(lon==12.75),which(lat==11.25),1:48],
                                        tmn2=tmn.var[which(lon==12.25),which(lat==11.25),1:48],
                                        tmn3=tmn.var[which(lon==12.75),which(lat==10.75),1:48]))
tmn_Pita_monthly$tmn <- rowMeans(select(tmn_Pita_monthly, tmn1, tmn2, tmn3))
tmn_Pita_monthly$Location <- 'Pita'
tmn_Pita_monthly$date <- ymd(paste(tmn_Pita_monthly$Year, tmn_Pita_monthly$Month, tmn_Pita_monthly$day, sep="-"))
tmn_Pita_monthly_2015 <- select(tmn_Pita_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Pita_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Pita_monthly_2015a$Location <- 'Pita'
tmn_Pita_monthly_2015a$date <- ymd(paste(tmn_Pita_monthly_2015a$Year, tmn_Pita_monthly_2015a$Month, tmn_Pita_monthly_2015a$day, sep="-"))
tmn_Pita_monthly_2015a <- select(tmn_Pita_monthly_2015a, date, Year, Month, day, Location)
tmn_Pita_monthly_2015 <- full_join(tmn_Pita_monthly_2015a, tmn_Pita_monthly_2015)
tmn_Pita_monthly <- rbind(select(tmn_Pita_monthly,date, Year, Month, day, Location, tmn), tmn_Pita_monthly_2015)
rm(tmn_Pita_monthly_2015, tmn_Pita_monthly_2015a)
tmn_Pita_monthly$measurement <- "tmn"
tmn_Pita_monthly <- rename(tmn_Pita_monthly, Value=tmn)
#Siguiri
tmn_Siguiri_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmn1=tmn.var[which(lon==9.25),which(lat==12.25),1:48],
                                           tmn2=tmn.var[which(lon==10.25),which(lat==11.75),1:48], 
                                           tmn3=tmn.var[which(lon==9.75),which(lat==11.75),1:48],
                                           tmn4=tmn.var[which(lon==9.25),which(lat==11.75),1:48],
                                           tmn5=tmn.var[which(lon==9.75),which(lat==11.25),1:48],
                                           tmn6=tmn.var[which(lon==9.25),which(lat==11.25),1:48]))
tmn_Siguiri_monthly$tmn <- rowMeans(select(tmn_Siguiri_monthly, tmn1, tmn2, tmn3, tmn4, tmn5, tmn6))
tmn_Siguiri_monthly$Location <- 'Siguiri'
tmn_Siguiri_monthly$date <- ymd(paste(tmn_Siguiri_monthly$Year, tmn_Siguiri_monthly$Month, tmn_Siguiri_monthly$day, sep="-"))
tmn_Siguiri_monthly_2015 <- select(tmn_Siguiri_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Siguiri_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Siguiri_monthly_2015a$Location <- 'Siguiri'
tmn_Siguiri_monthly_2015a$date <- ymd(paste(tmn_Siguiri_monthly_2015a$Year, tmn_Siguiri_monthly_2015a$Month, tmn_Siguiri_monthly_2015a$day, sep="-"))
tmn_Siguiri_monthly_2015a <- select(tmn_Siguiri_monthly_2015a, date, Year, Month, day, Location)
tmn_Siguiri_monthly_2015 <- full_join(tmn_Siguiri_monthly_2015a, tmn_Siguiri_monthly_2015)
tmn_Siguiri_monthly <- rbind(select(tmn_Siguiri_monthly,date, Year, Month, day, Location, tmn), tmn_Siguiri_monthly_2015)
rm(tmn_Siguiri_monthly_2015, tmn_Siguiri_monthly_2015a)
tmn_Siguiri_monthly$measurement <- "tmn"
tmn_Siguiri_monthly <- rename(tmn_Siguiri_monthly, Value=tmn)
#Telimele
tmn_Telimele_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            tmn1=tmn.var[which(lon==13.75),which(lat==11.25),1:48],
                                            tmn2=tmn.var[which(lon==13.25),which(lat==11.25),1:48], 
                                            tmn3=tmn.var[which(lon==13.75),which(lat==10.75),1:48],
                                            tmn4=tmn.var[which(lon==13.25),which(lat==10.75),1:48]))
tmn_Telimele_monthly$tmn <- rowMeans(select(tmn_Telimele_monthly, tmn1, tmn2, tmn3, tmn4))
tmn_Telimele_monthly$Location <- 'Telimele'
tmn_Telimele_monthly$date <- ymd(paste(tmn_Telimele_monthly$Year, tmn_Telimele_monthly$Month, tmn_Telimele_monthly$day, sep="-"))
tmn_Telimele_monthly_2015 <- select(tmn_Telimele_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Telimele_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Telimele_monthly_2015a$Location <- 'Telimele'
tmn_Telimele_monthly_2015a$date <- ymd(paste(tmn_Telimele_monthly_2015a$Year, tmn_Telimele_monthly_2015a$Month, tmn_Telimele_monthly_2015a$day, sep="-"))
tmn_Telimele_monthly_2015a <- select(tmn_Telimele_monthly_2015a, date, Year, Month, day, Location)
tmn_Telimele_monthly_2015 <- full_join(tmn_Telimele_monthly_2015a, tmn_Telimele_monthly_2015)
tmn_Telimele_monthly <- rbind(select(tmn_Telimele_monthly,date, Year, Month, day, Location, tmn), tmn_Telimele_monthly_2015)
rm(tmn_Telimele_monthly_2015, tmn_Telimele_monthly_2015a)
tmn_Telimele_monthly$measurement <- "tmn"
tmn_Telimele_monthly <- rename(tmn_Telimele_monthly, Value=tmn)
#Tougue
tmn_Tougue_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4),day=1, 
                                          tmn1=tmn.var[which(lon==11.25),which(lat==11.75),1:48],
                                          tmn2=tmn.var[which(lon==11.75),which(lat==11.25),1:48]))
tmn_Tougue_monthly$tmn <- rowMeans(select(tmn_Tougue_monthly, tmn1, tmn2))
tmn_Tougue_monthly$Location <- 'Tougue'
tmn_Tougue_monthly$date <- ymd(paste(tmn_Tougue_monthly$Year, tmn_Tougue_monthly$Month, tmn_Tougue_monthly$day, sep="-"))
tmn_Tougue_monthly_2015 <- select(tmn_Tougue_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_Tougue_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_Tougue_monthly_2015a$Location <- 'Tougue'
tmn_Tougue_monthly_2015a$date <- ymd(paste(tmn_Tougue_monthly_2015a$Year, tmn_Tougue_monthly_2015a$Month, tmn_Tougue_monthly_2015a$day, sep="-"))
tmn_Tougue_monthly_2015a <- select(tmn_Tougue_monthly_2015a, date, Year, Month, day, Location)
tmn_Tougue_monthly_2015 <- full_join(tmn_Tougue_monthly_2015a, tmn_Tougue_monthly_2015)
tmn_Tougue_monthly <- rbind(select(tmn_Tougue_monthly,date, Year, Month, day, Location, tmn), tmn_Tougue_monthly_2015)
rm(tmn_Tougue_monthly_2015, tmn_Tougue_monthly_2015a)
tmn_Tougue_monthly$measurement <- "tmn"
tmn_Tougue_monthly <- rename(tmn_Tougue_monthly, Value=tmn)
#yamou
tmn_yamou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4),day=1, 
                                         tmn1=tmn.var[which(lon==9.25),which(lat==7.75),1:48],
                                         tmn2=tmn.var[which(lon==9.25),which(lat==7.25),1:48],
                                         tmn3=tmn.var[which(lon==8.75),which(lat==7.25),1:48]))
tmn_yamou_monthly$Location <- 'yamou'
tmn_yamou_monthly$tmn <- rowMeans(select(tmn_yamou_monthly, tmn1, tmn2, tmn3))
tmn_yamou_monthly$date <- ymd(paste(tmn_yamou_monthly$Year, tmn_yamou_monthly$Month, tmn_yamou_monthly$day, sep="-"))
tmn_yamou_monthly_2015 <- select(tmn_yamou_monthly, Location, Year, Month, tmn) %>% group_by( Month) %>% summarize(tmn=mean(tmn))
tmn_yamou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmn_yamou_monthly_2015a$Location <- 'yamou'
tmn_yamou_monthly_2015a$date <- ymd(paste(tmn_yamou_monthly_2015a$Year, tmn_yamou_monthly_2015a$Month, tmn_yamou_monthly_2015a$day, sep="-"))
tmn_yamou_monthly_2015a <- select(tmn_yamou_monthly_2015a, date, Year, Month, day, Location)
tmn_yamou_monthly_2015 <- full_join(tmn_yamou_monthly_2015a, tmn_yamou_monthly_2015)
tmn_yamou_monthly <- rbind(select(tmn_yamou_monthly,date, Year, Month, day, Location, tmn), tmn_yamou_monthly_2015)
rm(tmn_yamou_monthly_2015, tmn_yamou_monthly_2015a)
tmn_yamou_monthly$measurement <- "tmn"
tmn_yamou_monthly <- rename(tmn_yamou_monthly, Value=tmn)

#Merging in long format
tmn_Guinea_monthly_district <- rbind(tmn_Beyla_monthly, tmn_Boke_monthly, tmn_Boffa_monthly,
                                     tmn_Conakry_monthly, tmn_Coyah_monthly, tmn_Dabola_monthly, tmn_Dalaba_monthly,
                                     tmn_Dinguiray_monthly, tmn_Dubreka_monthly, tmn_Faranah_monthly,
                                     tmn_Forecariah_monthly, tmn_Fria_monthly, tmn_Gaoual_monthly,
                                     tmn_Gueckedou_monthly, tmn_Kankan_monthly, tmn_Kerouane_monthly,
                                     tmn_Kindia_monthly, tmn_Kissidougou_monthly, tmn_Koubia_monthly,
                                     tmn_Koundara_monthly, tmn_Kouroussa_monthly, tmn_Labe_monthly,
                                     tmn_Lelouma_monthly, tmn_Lola_monthly, tmn_Macenta_monthly,
                                     tmn_Mali_monthly, tmn_Mamou_monthly, tmn_Nzerekore_monthly,
                                     tmn_Pita_monthly, tmn_Siguiri_monthly, tmn_Telimele_monthly,
                                     tmn_Tougue_monthly, tmn_yamou_monthly)

#####################
#tmp - mean Temp
#####################
tmp.full <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.tmp.dat.nc', write=FALSE)
tmp.var <- var.get.nc(tmp.full, "tmp")
lon <- var.get.nc(tmp.full, "lon")
lat <- var.get.nc(tmp.full, "lat")
#Beyla
tmp_Beyla_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         tmp1=tmp.var[which(lon==8.75),which(lat==9.25),1:48],
                                         tmp2=tmp.var[which(lon==8.25),which(lat==9.25),1:48], 
                                         tmp3=tmp.var[which(lon==7.75),which(lat==9.25),1:48],
                                         tmp4=tmp.var[which(lon==8.75),which(lat==8.75),1:48], 
                                         tmp5=tmp.var[which(lon==8.25),which(lat==8.75),1:48],
                                         tmp6=tmp.var[which(lon==7.75),which(lat==8.75),1:48], 
                                         tmp7=tmp.var[which(lon==8.75),which(lat==8.25),1:48],
                                         tmp8=tmp.var[which(lon==8.25),which(lat==8.25),1:48]))
tmp_Beyla_monthly$tmp <- rowMeans(select(tmp_Beyla_monthly, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8))
tmp_Beyla_monthly$Location <- 'Beyla'
tmp_Beyla_monthly$date <- ymd(paste(tmp_Beyla_monthly$Year, tmp_Beyla_monthly$Month, tmp_Beyla_monthly$day, sep="-"))
tmp_Beyla_monthly_2015 <- select(tmp_Beyla_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Beyla_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Beyla_monthly_2015a$Location <- 'Beyla'
tmp_Beyla_monthly_2015a$date <- ymd(paste(tmp_Beyla_monthly_2015a$Year, tmp_Beyla_monthly_2015a$Month, tmp_Beyla_monthly_2015a$day, sep="-"))
tmp_Beyla_monthly_2015a <- select(tmp_Beyla_monthly_2015a, date, Year, Month, day, Location)
tmp_Beyla_monthly_2015 <- full_join(tmp_Beyla_monthly_2015a, tmp_Beyla_monthly_2015)
tmp_Beyla_monthly <- rbind(select(tmp_Beyla_monthly,date, Year, Month, day, Location, tmp), tmp_Beyla_monthly_2015)
rm(tmp_Beyla_monthly_2015, tmp_Beyla_monthly_2015a)
tmp_Beyla_monthly$measurement <- "tmp"
tmp_Beyla_monthly <- rename(tmp_Beyla_monthly, Value=tmp)
#Boffa
tmp_Boffa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         tmp1=tmp.var[which(lon==14.25),which(lat==10.75),1:48],
                                         tmp2=tmp.var[which(lon==13.75),which(lat==10.75),1:48], 
                                         tmp3=tmp.var[which(lon==14.25),which(lat==10.25),1:48],
                                         tmp4=tmp.var[which(lon==13.75),which(lat==10.25),1:48]))
tmp_Boffa_monthly$tmp <- rowMeans(select(tmp_Boffa_monthly, tmp1, tmp2, tmp3, tmp4))
tmp_Boffa_monthly$Location <- 'Boffa'
tmp_Boffa_monthly$date <- ymd(paste(tmp_Boffa_monthly$Year, tmp_Boffa_monthly$Month, tmp_Boffa_monthly$day, sep="-"))
tmp_Boffa_monthly_2015 <- select(tmp_Boffa_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Boffa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Boffa_monthly_2015a$Location <- 'Boffa'
tmp_Boffa_monthly_2015a$date <- ymd(paste(tmp_Boffa_monthly_2015a$Year, tmp_Boffa_monthly_2015a$Month, tmp_Boffa_monthly_2015a$day, sep="-"))
tmp_Boffa_monthly_2015a <- select(tmp_Boffa_monthly_2015a, date, Year, Month, day, Location)
tmp_Boffa_monthly_2015 <- full_join(tmp_Boffa_monthly_2015a, tmp_Boffa_monthly_2015)
tmp_Boffa_monthly <- rbind(select(tmp_Boffa_monthly,date, Year, Month, day, Location, tmp), tmp_Boffa_monthly_2015)
rm(tmp_Boffa_monthly_2015, tmp_Boffa_monthly_2015a)
tmp_Boffa_monthly$measurement <- "tmp"
tmp_Boffa_monthly <- rename(tmp_Boffa_monthly, Value=tmp)
#Boke
tmp_Boke_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmp1=tmp.var[which(lon==14.25),which(lat==11.75),1:48],
                                        tmp2=tmp.var[which(lon==14.75),which(lat==11.25),1:48], 
                                        tmp3=tmp.var[which(lon==14.25),which(lat==11.25),1:48],
                                        tmp4=tmp.var[which(lon==13.75),which(lat==11.25),1:48]))
tmp_Boke_monthly$tmp <- rowMeans(select(tmp_Boke_monthly, tmp1, tmp2, tmp3, tmp4))
tmp_Boke_monthly$Location <- 'Boke'
tmp_Boke_monthly$date <- ymd(paste(tmp_Boke_monthly$Year, tmp_Boke_monthly$Month, tmp_Boke_monthly$day, sep="-"))
tmp_Boke_monthly_2015 <- select(tmp_Boke_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Boke_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Boke_monthly_2015a$Location <- 'Boke'
tmp_Boke_monthly_2015a$date <- ymd(paste(tmp_Boke_monthly_2015a$Year, tmp_Boke_monthly_2015a$Month, tmp_Boke_monthly_2015a$day, sep="-"))
tmp_Boke_monthly_2015a <- select(tmp_Boke_monthly_2015a, date, Year, Month, day, Location)
tmp_Boke_monthly_2015 <- full_join(tmp_Boke_monthly_2015a, tmp_Boke_monthly_2015)
tmp_Boke_monthly <- rbind(select(tmp_Boke_monthly,date, Year, Month, day, Location, tmp), tmp_Boke_monthly_2015)
rm(tmp_Boke_monthly_2015, tmp_Boke_monthly_2015a)
tmp_Boke_monthly$measurement <- "tmp"
tmp_Boke_monthly <- rename(tmp_Boke_monthly, Value=tmp)
#Conakry
tmp_Conakry_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmp=tmp.var[which(lon==13.75),which(lat==9.75),1:48]))
tmp_Conakry_monthly$Location <- 'Conakry'
tmp_Conakry_monthly$date <- ymd(paste(tmp_Conakry_monthly$Year, tmp_Conakry_monthly$Month, tmp_Conakry_monthly$day, sep="-"))
tmp_Conakry_monthly_2015 <- select(tmp_Conakry_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Conakry_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Conakry_monthly_2015a$Location <- 'Conakry'
tmp_Conakry_monthly_2015a$date <- ymd(paste(tmp_Conakry_monthly_2015a$Year, tmp_Conakry_monthly_2015a$Month, tmp_Conakry_monthly_2015a$day, sep="-"))
tmp_Conakry_monthly_2015a <- select(tmp_Conakry_monthly_2015a, date, Year, Month, day, Location)
tmp_Conakry_monthly_2015 <- full_join(tmp_Conakry_monthly_2015a, tmp_Conakry_monthly_2015)
tmp_Conakry_monthly <- rbind(select(tmp_Conakry_monthly,date, Year, Month, day, Location, tmp), tmp_Conakry_monthly_2015)
rm(tmp_Conakry_monthly_2015, tmp_Conakry_monthly_2015a)
tmp_Conakry_monthly$measurement <- "tmp"
tmp_Conakry_monthly <- rename(tmp_Conakry_monthly, Value=tmp)
#Coyah
tmp_Coyah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         tmp=tmp.var[which(lon==9.75),which(lat==13.25),1:48]))
tmp_Coyah_monthly$Location <- 'Coyah'
tmp_Coyah_monthly$date <- ymd(paste(tmp_Coyah_monthly$Year, tmp_Coyah_monthly$Month, tmp_Coyah_monthly$day, sep="-"))
tmp_Coyah_monthly_2015 <- select(tmp_Coyah_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Coyah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Coyah_monthly_2015a$Location <- 'Coyah'
tmp_Coyah_monthly_2015a$date <- ymd(paste(tmp_Coyah_monthly_2015a$Year, tmp_Coyah_monthly_2015a$Month, tmp_Coyah_monthly_2015a$day, sep="-"))
tmp_Coyah_monthly_2015a <- select(tmp_Coyah_monthly_2015a, date, Year, Month, day, Location)
tmp_Coyah_monthly_2015 <- full_join(tmp_Coyah_monthly_2015a, tmp_Coyah_monthly_2015)
tmp_Coyah_monthly <- rbind(select(tmp_Coyah_monthly,date, Year, Month, day, Location, tmp), tmp_Coyah_monthly_2015)
rm(tmp_Coyah_monthly_2015, tmp_Coyah_monthly_2015a)
tmp_Coyah_monthly$measurement <- "tmp"
tmp_Coyah_monthly <- rename(tmp_Coyah_monthly, Value=tmp)
#Dabola
tmp_Dabola_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmp1=tmp.var[which(lon==11.25),which(lat==10.75),1:48],
                                          tmp2=tmp.var[which(lon==10.75),which(lat==10.75),1:48]))
tmp_Dabola_monthly$tmp <- rowMeans(select(tmp_Dabola_monthly, tmp1, tmp2))
tmp_Dabola_monthly$Location <- 'Dabola'
tmp_Dabola_monthly$date <- ymd(paste(tmp_Dabola_monthly$Year, tmp_Dabola_monthly$Month, tmp_Dabola_monthly$day, sep="-"))
tmp_Dabola_monthly_2015 <- select(tmp_Dabola_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Dabola_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Dabola_monthly_2015a$Location <- 'Dabola'
tmp_Dabola_monthly_2015a$date <- ymd(paste(tmp_Dabola_monthly_2015a$Year, tmp_Dabola_monthly_2015a$Month, tmp_Dabola_monthly_2015a$day, sep="-"))
tmp_Dabola_monthly_2015a <- select(tmp_Dabola_monthly_2015a, date, Year, Month, day, Location)
tmp_Dabola_monthly_2015 <- full_join(tmp_Dabola_monthly_2015a, tmp_Dabola_monthly_2015)
tmp_Dabola_monthly <- rbind(select(tmp_Dabola_monthly,date, Year, Month, day, Location, tmp), tmp_Dabola_monthly_2015)
rm(tmp_Dabola_monthly_2015, tmp_Dabola_monthly_2015a)
tmp_Dabola_monthly$measurement <- "tmp"
tmp_Dabola_monthly <- rename(tmp_Dabola_monthly, Value=tmp)
#Dalaba
tmp_Dalaba_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmp1=tmp.var[which(lon==11.25),which(lat==12.25),1:48],
                                          tmp2=tmp.var[which(lon==10.75),which(lat==12.25),1:48]))
tmp_Dalaba_monthly$tmp <- rowMeans(select(tmp_Dalaba_monthly, tmp1, tmp2))
tmp_Dalaba_monthly$Location <- 'Dalaba'
tmp_Dalaba_monthly$date <- ymd(paste(tmp_Dalaba_monthly$Year, tmp_Dalaba_monthly$Month, tmp_Dalaba_monthly$day, sep="-"))
tmp_Dalaba_monthly_2015 <- select(tmp_Dalaba_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Dalaba_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Dalaba_monthly_2015a$Location <- 'Dalaba'
tmp_Dalaba_monthly_2015a$date <- ymd(paste(tmp_Dalaba_monthly_2015a$Year, tmp_Dalaba_monthly_2015a$Month, tmp_Dalaba_monthly_2015a$day, sep="-"))
tmp_Dalaba_monthly_2015a <- select(tmp_Dalaba_monthly_2015a, date, Year, Month, day, Location)
tmp_Dalaba_monthly_2015 <- full_join(tmp_Dalaba_monthly_2015a, tmp_Dalaba_monthly_2015)
tmp_Dalaba_monthly <- rbind(select(tmp_Dalaba_monthly,date, Year, Month, day, Location, tmp), tmp_Dalaba_monthly_2015)
rm(tmp_Dalaba_monthly_2015, tmp_Dalaba_monthly_2015a)
tmp_Dalaba_monthly$measurement <- "tmp"
tmp_Dalaba_monthly <- rename(tmp_Dalaba_monthly, Value=tmp)
#Dinguiray
tmp_Dinguiray_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             tmp1=tmp.var[which(lon==11.25),which(lat==11.75),1:48],
                                             tmp2=tmp.var[which(lon==10.75),which(lat==11.75),1:48], 
                                             tmp3=tmp.var[which(lon==10.25),which(lat==11.75),1:48],
                                             tmp4=tmp.var[which(lon==11.25),which(lat==11.25),1:48],
                                             tmp5=tmp.var[which(lon==10.75),which(lat==11.25),1:48],
                                             tmp6=tmp.var[which(lon==10.25),which(lat==11.25),1:48]))
tmp_Dinguiray_monthly$tmp <- rowMeans(select(tmp_Dinguiray_monthly, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6))
tmp_Dinguiray_monthly$Location <- 'Dinguiray'
tmp_Dinguiray_monthly$date <- ymd(paste(tmp_Dinguiray_monthly$Year, tmp_Dinguiray_monthly$Month, tmp_Dinguiray_monthly$day, sep="-"))
tmp_Dinguiray_monthly_2015 <- select(tmp_Dinguiray_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Dinguiray_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Dinguiray_monthly_2015a$Location <- 'Dinguiray'
tmp_Dinguiray_monthly_2015a$date <- ymd(paste(tmp_Dinguiray_monthly_2015a$Year, tmp_Dinguiray_monthly_2015a$Month, tmp_Dinguiray_monthly_2015a$day, sep="-"))
tmp_Dinguiray_monthly_2015a <- select(tmp_Dinguiray_monthly_2015a, date, Year, Month, day, Location)
tmp_Dinguiray_monthly_2015 <- full_join(tmp_Dinguiray_monthly_2015a, tmp_Dinguiray_monthly_2015)
tmp_Dinguiray_monthly <- rbind(select(tmp_Dinguiray_monthly,date, Year, Month, day, Location, tmp), tmp_Dinguiray_monthly_2015)
rm(tmp_Dinguiray_monthly_2015, tmp_Dinguiray_monthly_2015a)
tmp_Dinguiray_monthly$measurement <- "tmp"
tmp_Dinguiray_monthly <- rename(tmp_Dinguiray_monthly, Value=tmp)
#Dubreka
tmp_Dubreka_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmp1=tmp.var[which(lon==13.75),which(lat==10.25),1:48],
                                           tmp2=tmp.var[which(lon==13.25),which(lat==10.25),1:48]))
tmp_Dubreka_monthly$tmp <- rowMeans(select(tmp_Dubreka_monthly, tmp1, tmp2))
tmp_Dubreka_monthly$Location <- 'Dubreka'
tmp_Dubreka_monthly$date <- ymd(paste(tmp_Dubreka_monthly$Year, tmp_Dubreka_monthly$Month, tmp_Dubreka_monthly$day, sep="-"))
tmp_Dubreka_monthly_2015 <- select(tmp_Dubreka_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Dubreka_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Dubreka_monthly_2015a$Location <- 'Dubreka'
tmp_Dubreka_monthly_2015a$date <- ymd(paste(tmp_Dubreka_monthly_2015a$Year, tmp_Dubreka_monthly_2015a$Month, tmp_Dubreka_monthly_2015a$day, sep="-"))
tmp_Dubreka_monthly_2015a <- select(tmp_Dubreka_monthly_2015a, date, Year, Month, day, Location)
tmp_Dubreka_monthly_2015 <- full_join(tmp_Dubreka_monthly_2015a, tmp_Dubreka_monthly_2015)
tmp_Dubreka_monthly <- rbind(select(tmp_Dubreka_monthly,date, Year, Month, day, Location, tmp), tmp_Dubreka_monthly_2015)
rm(tmp_Dubreka_monthly_2015, tmp_Dubreka_monthly_2015a)
tmp_Dubreka_monthly$measurement <- "tmp"
tmp_Dubreka_monthly <- rename(tmp_Dubreka_monthly, Value=tmp)
#Faranah
tmp_Faranah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmp1=tmp.var[which(lon==11.25),which(lat==10.25),1:48],
                                           tmp2=tmp.var[which(lon==10.75),which(lat==10.25),1:48], 
                                           tmp3=tmp.var[which(lon==10.25),which(lat==10.25),1:48],
                                           tmp4=tmp.var[which(lon==10.75),which(lat==9.75),1:48],
                                           tmp5=tmp.var[which(lon==10.75),which(lat==9.25),1:48]))
tmp_Faranah_monthly$tmp <- rowMeans(select(tmp_Faranah_monthly, tmp1, tmp2, tmp3, tmp4, tmp5))
tmp_Faranah_monthly$Location <- 'Faranah'
tmp_Faranah_monthly$date <- ymd(paste(tmp_Faranah_monthly$Year, tmp_Faranah_monthly$Month, tmp_Faranah_monthly$day, sep="-"))
tmp_Faranah_monthly_2015 <- select(tmp_Faranah_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Faranah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Faranah_monthly_2015a$Location <- 'Faranah'
tmp_Faranah_monthly_2015a$date <- ymd(paste(tmp_Faranah_monthly_2015a$Year, tmp_Faranah_monthly_2015a$Month, tmp_Faranah_monthly_2015a$day, sep="-"))
tmp_Faranah_monthly_2015a <- select(tmp_Faranah_monthly_2015a, date, Year, Month, day, Location)
tmp_Faranah_monthly_2015 <- full_join(tmp_Faranah_monthly_2015a, tmp_Faranah_monthly_2015)
tmp_Faranah_monthly <- rbind(select(tmp_Faranah_monthly,date, Year, Month, day, Location, tmp), tmp_Faranah_monthly_2015)
rm(tmp_Faranah_monthly_2015, tmp_Faranah_monthly_2015a)
tmp_Faranah_monthly$measurement <- "tmp"
tmp_Faranah_monthly <- rename(tmp_Faranah_monthly, Value=tmp)
#Forecariah
tmp_Forecariah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                              Month=rep(seq(1,12,1), 4), day=1,
                                              tmp1=tmp.var[which(lon==12.75),which(lat==9.75),1:48],
                                              tmp2=tmp.var[which(lon==13.25),which(lat==9.25),1:48], 
                                              tmp3=tmp.var[which(lon==12.75),which(lat==9.25),1:48]))
tmp_Forecariah_monthly$tmp <- rowMeans(select(tmp_Forecariah_monthly, tmp1, tmp2, tmp3))
tmp_Forecariah_monthly$Location <- 'Forecariah'
tmp_Forecariah_monthly$date <- ymd(paste(tmp_Forecariah_monthly$Year, tmp_Forecariah_monthly$Month, tmp_Forecariah_monthly$day, sep="-"))
tmp_Forecariah_monthly_2015 <- select(tmp_Forecariah_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Forecariah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Forecariah_monthly_2015a$Location <- 'Forecariah'
tmp_Forecariah_monthly_2015a$date <- ymd(paste(tmp_Forecariah_monthly_2015a$Year, tmp_Forecariah_monthly_2015a$Month, tmp_Forecariah_monthly_2015a$day, sep="-"))
tmp_Forecariah_monthly_2015a <- select(tmp_Forecariah_monthly_2015a, date, Year, Month, day, Location)
tmp_Forecariah_monthly_2015 <- full_join(tmp_Forecariah_monthly_2015a, tmp_Forecariah_monthly_2015)
tmp_Forecariah_monthly <- rbind(select(tmp_Forecariah_monthly,date, Year, Month, day, Location, tmp), tmp_Forecariah_monthly_2015)
rm(tmp_Forecariah_monthly_2015, tmp_Forecariah_monthly_2015a)
tmp_Forecariah_monthly$measurement <- "tmp"
tmp_Forecariah_monthly <- rename(tmp_Forecariah_monthly, Value=tmp)
#Fria
tmp_Fria_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmp1=tmp.var[which(lon==13.75),which(lat==10.75),1:48],
                                        tmp2=tmp.var[which(lon==13.75),which(lat==10.25),1:48]))
tmp_Fria_monthly$tmp <- rowMeans(select(tmp_Fria_monthly, tmp1, tmp2))
tmp_Fria_monthly$Location <- 'Fria'
tmp_Fria_monthly$date <- ymd(paste(tmp_Fria_monthly$Year, tmp_Fria_monthly$Month, tmp_Fria_monthly$day, sep="-"))
tmp_Fria_monthly_2015 <- select(tmp_Fria_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Fria_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Fria_monthly_2015a$Location <- 'Fria'
tmp_Fria_monthly_2015a$date <- ymd(paste(tmp_Fria_monthly_2015a$Year, tmp_Fria_monthly_2015a$Month, tmp_Fria_monthly_2015a$day, sep="-"))
tmp_Fria_monthly_2015a <- select(tmp_Fria_monthly_2015a, date, Year, Month, day, Location)
tmp_Fria_monthly_2015 <- full_join(tmp_Fria_monthly_2015a, tmp_Fria_monthly_2015)
tmp_Fria_monthly <- rbind(select(tmp_Fria_monthly,date, Year, Month, day, Location, tmp), tmp_Fria_monthly_2015)
rm(tmp_Fria_monthly_2015, tmp_Fria_monthly_2015a)
tmp_Fria_monthly$measurement <- "tmp"
tmp_Fria_monthly <- rename(tmp_Fria_monthly, Value=tmp)
#Gaoual
tmp_Gaoual_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmp1=tmp.var[which(lon==13.75),which(lat==12.25),1:48],
                                          tmp2=tmp.var[which(lon==13.25),which(lat==12.25),1:48], 
                                          tmp3=tmp.var[which(lon==13.75),which(lat==11.75),1:48],
                                          tmp4=tmp.var[which(lon==13.25),which(lat==11.75),1:48],
                                          tmp5=tmp.var[which(lon==12.75),which(lat==11.75),1:48],
                                          tmp6=tmp.var[which(lon==13.75),which(lat==11.25),1:48],
                                          tmp7=tmp.var[which(lon==13.25),which(lat==11.25),1:48]))
tmp_Gaoual_monthly$tmp <- rowMeans(select(tmp_Gaoual_monthly, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7))
tmp_Gaoual_monthly$Location <- 'Gaoual'
tmp_Gaoual_monthly$date <- ymd(paste(tmp_Gaoual_monthly$Year, tmp_Gaoual_monthly$Month, tmp_Gaoual_monthly$day, sep="-"))
tmp_Gaoual_monthly_2015 <- select(tmp_Gaoual_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Gaoual_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Gaoual_monthly_2015a$Location <- 'Gaoual'
tmp_Gaoual_monthly_2015a$date <- ymd(paste(tmp_Gaoual_monthly_2015a$Year, tmp_Gaoual_monthly_2015a$Month, tmp_Gaoual_monthly_2015a$day, sep="-"))
tmp_Gaoual_monthly_2015a <- select(tmp_Gaoual_monthly_2015a, date, Year, Month, day, Location)
tmp_Gaoual_monthly_2015 <- full_join(tmp_Gaoual_monthly_2015a, tmp_Gaoual_monthly_2015)
tmp_Gaoual_monthly <- rbind(select(tmp_Gaoual_monthly,date, Year, Month, day, Location, tmp), tmp_Gaoual_monthly_2015)
rm(tmp_Gaoual_monthly_2015, tmp_Gaoual_monthly_2015a)
tmp_Gaoual_monthly$measurement <- "tmp"
tmp_Gaoual_monthly <- rename(tmp_Gaoual_monthly, Value=tmp)
#Gueckedou
tmp_Gueckedou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             tmp=tmp.var[which(lon==10.25),which(lat==8.75),1:48]))
tmp_Gueckedou_monthly$Location <- 'Gueckedou'
tmp_Gueckedou_monthly$date <- ymd(paste(tmp_Gueckedou_monthly$Year, tmp_Gueckedou_monthly$Month, tmp_Gueckedou_monthly$day, sep="-"))
tmp_Gueckedou_monthly_2015 <- select(tmp_Gueckedou_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Gueckedou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Gueckedou_monthly_2015a$Location <- 'Gueckedou'
tmp_Gueckedou_monthly_2015a$date <- ymd(paste(tmp_Gueckedou_monthly_2015a$Year, tmp_Gueckedou_monthly_2015a$Month, tmp_Gueckedou_monthly_2015a$day, sep="-"))
tmp_Gueckedou_monthly_2015a <- select(tmp_Gueckedou_monthly_2015a, date, Year, Month, day, Location)
tmp_Gueckedou_monthly_2015 <- full_join(tmp_Gueckedou_monthly_2015a, tmp_Gueckedou_monthly_2015)
tmp_Gueckedou_monthly <- rbind(select(tmp_Gueckedou_monthly,date, Year, Month, day, Location, tmp), tmp_Gueckedou_monthly_2015)
rm(tmp_Gueckedou_monthly_2015, tmp_Gueckedou_monthly_2015a)
tmp_Gueckedou_monthly$measurement <- "tmp"
tmp_Gueckedou_monthly <- rename(tmp_Gueckedou_monthly, Value=tmp)
#Kankan
tmp_Kankan_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmp1=tmp.var[which(lon==9.25),which(lat==10.75),1:48],
                                          tmp2=tmp.var[which(lon==9.75),which(lat==10.25),1:48], 
                                          tmp3=tmp.var[which(lon==9.25),which(lat==10.25),1:48],
                                          tmp4=tmp.var[which(lon==8.75),which(lat==10.25),1:48],
                                          tmp5=tmp.var[which(lon==9.75),which(lat==9.75),1:48],
                                          tmp6=tmp.var[which(lon==9.25),which(lat==9.75),1:48]))
tmp_Kankan_monthly$tmp <- rowMeans(select(tmp_Kankan_monthly, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6))
tmp_Kankan_monthly$Location <- 'Kankan'
tmp_Kankan_monthly$date <- ymd(paste(tmp_Kankan_monthly$Year, tmp_Kankan_monthly$Month, tmp_Kankan_monthly$day, sep="-"))
tmp_Kankan_monthly_2015 <- select(tmp_Kankan_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Kankan_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Kankan_monthly_2015a$Location <- 'Kankan'
tmp_Kankan_monthly_2015a$date <- ymd(paste(tmp_Kankan_monthly_2015a$Year, tmp_Kankan_monthly_2015a$Month, tmp_Kankan_monthly_2015a$day, sep="-"))
tmp_Kankan_monthly_2015a <- select(tmp_Kankan_monthly_2015a, date, Year, Month, day, Location)
tmp_Kankan_monthly_2015 <- full_join(tmp_Kankan_monthly_2015a, tmp_Kankan_monthly_2015)
tmp_Kankan_monthly <- rbind(select(tmp_Kankan_monthly,date, Year, Month, day, Location, tmp), tmp_Kankan_monthly_2015)
rm(tmp_Kankan_monthly_2015, tmp_Kankan_monthly_2015a)
tmp_Kankan_monthly$measurement <- "tmp"
tmp_Kankan_monthly <- rename(tmp_Kankan_monthly, Value=tmp)
#Kerouane
tmp_Kerouane_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            tmp1=tmp.var[which(lon==9.25),which(lat==9.75),1:48],
                                            tmp2=tmp.var[which(lon==8.75),which(lat==9.75),1:48], 
                                            tmp3=tmp.var[which(lon==9.75),which(lat==9.25),1:48],
                                            tmp4=tmp.var[which(lon==9.25),which(lat==9.25),1:48],
                                            tmp5=tmp.var[which(lon==8.75),which(lat==9.25),1:48],
                                            tmp6=tmp.var[which(lon==9.25),which(lat==8.75),1:48]))
tmp_Kerouane_monthly$tmp <- rowMeans(select(tmp_Kerouane_monthly, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6))
tmp_Kerouane_monthly$Location <- 'Kerouane'
tmp_Kerouane_monthly$date <- ymd(paste(tmp_Kerouane_monthly$Year, tmp_Kerouane_monthly$Month, tmp_Kerouane_monthly$day, sep="-"))
tmp_Kerouane_monthly_2015 <- select(tmp_Kerouane_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Kerouane_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Kerouane_monthly_2015a$Location <- 'Kerouane'
tmp_Kerouane_monthly_2015a$date <- ymd(paste(tmp_Kerouane_monthly_2015a$Year, tmp_Kerouane_monthly_2015a$Month, tmp_Kerouane_monthly_2015a$day, sep="-"))
tmp_Kerouane_monthly_2015a <- select(tmp_Kerouane_monthly_2015a, date, Year, Month, day, Location)
tmp_Kerouane_monthly_2015 <- full_join(tmp_Kerouane_monthly_2015a, tmp_Kerouane_monthly_2015)
tmp_Kerouane_monthly <- rbind(select(tmp_Kerouane_monthly,date, Year, Month, day, Location, tmp), tmp_Kerouane_monthly_2015)
rm(tmp_Kerouane_monthly_2015, tmp_Kerouane_monthly_2015a)
tmp_Kerouane_monthly$measurement <- "tmp"
tmp_Kerouane_monthly <- rename(tmp_Kerouane_monthly, Value=tmp)
#Kindia
tmp_Kindia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmp1=tmp.var[which(lon==13.25),which(lat==10.25),1:48],
                                          tmp2=tmp.var[which(lon==12.75),which(lat==10.25),1:48], 
                                          tmp3=tmp.var[which(lon==12.25),which(lat==10.25),1:48],
                                          tmp4=tmp.var[which(lon==12.75),which(lat==9.75),1:48]))
tmp_Kindia_monthly$tmp <- rowMeans(select(tmp_Kindia_monthly, tmp1, tmp2, tmp3, tmp4))
tmp_Kindia_monthly$Location <- 'Kindia'
tmp_Kindia_monthly$date <- ymd(paste(tmp_Kindia_monthly$Year, tmp_Kindia_monthly$Month, tmp_Kindia_monthly$day, sep="-"))
tmp_Kindia_monthly_2015 <- select(tmp_Kindia_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Kindia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Kindia_monthly_2015a$Location <- 'Kindia'
tmp_Kindia_monthly_2015a$date <- ymd(paste(tmp_Kindia_monthly_2015a$Year, tmp_Kindia_monthly_2015a$Month, tmp_Kindia_monthly_2015a$day, sep="-"))
tmp_Kindia_monthly_2015a <- select(tmp_Kindia_monthly_2015a, date, Year, Month, day, Location)
tmp_Kindia_monthly_2015 <- full_join(tmp_Kindia_monthly_2015a, tmp_Kindia_monthly_2015)
tmp_Kindia_monthly <- rbind(select(tmp_Kindia_monthly,date, Year, Month, day, Location, tmp), tmp_Kindia_monthly_2015)
rm(tmp_Kindia_monthly_2015, tmp_Kindia_monthly_2015a)
tmp_Kindia_monthly$measurement <- "tmp"
tmp_Kindia_monthly <- rename(tmp_Kindia_monthly, Value=tmp)
#KISSIDOUGOU
tmp_Kissidougou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               tmp1=tmp.var[which(lon==10.25),which(lat==9.75),1:48],
                                               tmp2=tmp.var[which(lon==10.25),which(lat==9.25),1:48], 
                                               tmp3=tmp.var[which(lon==9.75),which(lat==9.25),1:48]))
tmp_Kissidougou_monthly$tmp <- rowMeans(select(tmp_Kissidougou_monthly, tmp1, tmp2, tmp3))
tmp_Kissidougou_monthly$Location <- 'Kissidougou'
tmp_Kissidougou_monthly$date <- ymd(paste(tmp_Kissidougou_monthly$Year, tmp_Kissidougou_monthly$Month, tmp_Kissidougou_monthly$day, sep="-"))
tmp_Kissidougou_monthly_2015 <- select(tmp_Kissidougou_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Kissidougou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Kissidougou_monthly_2015a$Location <- 'Kissidougou'
tmp_Kissidougou_monthly_2015a$date <- ymd(paste(tmp_Kissidougou_monthly_2015a$Year, tmp_Kissidougou_monthly_2015a$Month, tmp_Kissidougou_monthly_2015a$day, sep="-"))
tmp_Kissidougou_monthly_2015a <- select(tmp_Kissidougou_monthly_2015a, date, Year, Month, day, Location)
tmp_Kissidougou_monthly_2015 <- full_join(tmp_Kissidougou_monthly_2015a, tmp_Kissidougou_monthly_2015)
tmp_Kissidougou_monthly <- rbind(select(tmp_Kissidougou_monthly,date, Year, Month, day, Location, tmp), tmp_Kissidougou_monthly_2015)
rm(tmp_Kissidougou_monthly_2015, tmp_Kissidougou_monthly_2015a)
tmp_Kissidougou_monthly$measurement <- "tmp"
tmp_Kissidougou_monthly <- rename(tmp_Kissidougou_monthly, Value=tmp)
#Koubia
tmp_Koubia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmp=tmp.var[which(lon==10.25),which(lat==9.75),1:48]))
tmp_Koubia_monthly$Location <- 'Koubia'
tmp_Koubia_monthly$date <- ymd(paste(tmp_Koubia_monthly$Year, tmp_Koubia_monthly$Month, tmp_Koubia_monthly$day, sep="-"))
tmp_Koubia_monthly_2015 <- select(tmp_Koubia_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Koubia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Koubia_monthly_2015a$Location <- 'Koubia'
tmp_Koubia_monthly_2015a$date <- ymd(paste(tmp_Koubia_monthly_2015a$Year, tmp_Koubia_monthly_2015a$Month, tmp_Koubia_monthly_2015a$day, sep="-"))
tmp_Koubia_monthly_2015a <- select(tmp_Koubia_monthly_2015a, date, Year, Month, day, Location)
tmp_Koubia_monthly_2015 <- full_join(tmp_Koubia_monthly_2015a, tmp_Koubia_monthly_2015)
tmp_Koubia_monthly <- rbind(select(tmp_Koubia_monthly,date, Year, Month, day, Location, tmp), tmp_Koubia_monthly_2015)
rm(tmp_Koubia_monthly_2015, tmp_Koubia_monthly_2015a)
tmp_Koubia_monthly$measurement <- "tmp"
tmp_Koubia_monthly <- rename(tmp_Koubia_monthly, Value=tmp)
#Koundara
tmp_Koundara_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            tmp1=tmp.var[which(lon==13.25),which(lat==12.75),1:48],
                                            tmp2=tmp.var[which(lon==13.25),which(lat==12.25),1:48], 
                                            tmp3=tmp.var[which(lon==12.75),which(lat==12.25),1:48]))
tmp_Koundara_monthly$tmp <- rowMeans(select(tmp_Koundara_monthly, tmp1, tmp2, tmp3))
tmp_Koundara_monthly$Location <- 'Koundara'
tmp_Koundara_monthly$date <- ymd(paste(tmp_Koundara_monthly$Year, tmp_Koundara_monthly$Month, tmp_Koundara_monthly$day, sep="-"))
tmp_Koundara_monthly_2015 <- select(tmp_Koundara_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Koundara_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Koundara_monthly_2015a$Location <- 'Koundara'
tmp_Koundara_monthly_2015a$date <- ymd(paste(tmp_Koundara_monthly_2015a$Year, tmp_Koundara_monthly_2015a$Month, tmp_Koundara_monthly_2015a$day, sep="-"))
tmp_Koundara_monthly_2015a <- select(tmp_Koundara_monthly_2015a, date, Year, Month, day, Location)
tmp_Koundara_monthly_2015 <- full_join(tmp_Koundara_monthly_2015a, tmp_Koundara_monthly_2015)
tmp_Koundara_monthly <- rbind(select(tmp_Koundara_monthly,date, Year, Month, day, Location, tmp), tmp_Koundara_monthly_2015)
rm(tmp_Koundara_monthly_2015, tmp_Koundara_monthly_2015a)
tmp_Koundara_monthly$measurement <- "tmp"
tmp_Koundara_monthly <- rename(tmp_Koundara_monthly, Value=tmp)
#Kouroussa
tmp_Kouroussa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             tmp1=tmp.var[which(lon==10.25),which(lat==11.25),1:48],
                                             tmp2=tmp.var[which(lon==10.75),which(lat==10.75),1:48], 
                                             tmp3=tmp.var[which(lon==10.25),which(lat==10.75),1:48],
                                             tmp4=tmp.var[which(lon==9.75),which(lat==10.75),1:48],
                                             tmp5=tmp.var[which(lon==10.25),which(lat==10.25),1:48],
                                             tmp6=tmp.var[which(lon==9.75),which(lat==10.25),1:48],
                                             tmp7=tmp.var[which(lon==10.25),which(lat==9.75),1:48],
                                             tmp8=tmp.var[which(lon==9.75),which(lat==9.75),1:48]))
tmp_Kouroussa_monthly$tmp <- rowMeans(select(tmp_Kouroussa_monthly, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7, tmp8))
tmp_Kouroussa_monthly$Location <- 'Kouroussa'
tmp_Kouroussa_monthly$date <- ymd(paste(tmp_Kouroussa_monthly$Year, tmp_Kouroussa_monthly$Month, tmp_Kouroussa_monthly$day, sep="-"))
tmp_Kouroussa_monthly_2015 <- select(tmp_Kouroussa_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Kouroussa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Kouroussa_monthly_2015a$Location <- 'Kouroussa'
tmp_Kouroussa_monthly_2015a$date <- ymd(paste(tmp_Kouroussa_monthly_2015a$Year, tmp_Kouroussa_monthly_2015a$Month, tmp_Kouroussa_monthly_2015a$day, sep="-"))
tmp_Kouroussa_monthly_2015a <- select(tmp_Kouroussa_monthly_2015a, date, Year, Month, day, Location)
tmp_Kouroussa_monthly_2015 <- full_join(tmp_Kouroussa_monthly_2015a, tmp_Kouroussa_monthly_2015)
tmp_Kouroussa_monthly <- rbind(select(tmp_Kouroussa_monthly,date, Year, Month, day, Location, tmp), tmp_Kouroussa_monthly_2015)
rm(tmp_Kouroussa_monthly_2015, tmp_Kouroussa_monthly_2015a)
tmp_Kouroussa_monthly$measurement <- "tmp"
tmp_Kouroussa_monthly <- rename(tmp_Kouroussa_monthly, Value=tmp)
#Labe
tmp_Labe_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmp1=tmp.var[which(lon==12.25),which(lat==11.75),1:48],
                                        tmp2=tmp.var[which(lon==12.25),which(lat==11.25),1:48]))
tmp_Labe_monthly$tmp <- rowMeans(select(tmp_Labe_monthly, tmp1, tmp2))
tmp_Labe_monthly$Location <- 'Labe'
tmp_Labe_monthly$date <- ymd(paste(tmp_Labe_monthly$Year, tmp_Labe_monthly$Month, tmp_Labe_monthly$day, sep="-"))
tmp_Labe_monthly_2015 <- select(tmp_Labe_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Labe_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Labe_monthly_2015a$Location <- 'Labe'
tmp_Labe_monthly_2015a$date <- ymd(paste(tmp_Labe_monthly_2015a$Year, tmp_Labe_monthly_2015a$Month, tmp_Labe_monthly_2015a$day, sep="-"))
tmp_Labe_monthly_2015a <- select(tmp_Labe_monthly_2015a, date, Year, Month, day, Location)
tmp_Labe_monthly_2015 <- full_join(tmp_Labe_monthly_2015a, tmp_Labe_monthly_2015)
tmp_Labe_monthly <- rbind(select(tmp_Labe_monthly,date, Year, Month, day, Location, tmp), tmp_Labe_monthly_2015)
rm(tmp_Labe_monthly_2015, tmp_Labe_monthly_2015a)
tmp_Labe_monthly$measurement <- "tmp"
tmp_Labe_monthly <- rename(tmp_Labe_monthly, Value=tmp)
#Lelouma
tmp_Lelouma_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmp1=tmp.var[which(lon==12.75),which(lat==11.75),1:48],
                                           tmp2=tmp.var[which(lon==12.75),which(lat==11.25),1:48]))
tmp_Lelouma_monthly$tmp <- rowMeans(select(tmp_Lelouma_monthly, tmp1, tmp2))
tmp_Lelouma_monthly$Location <- 'Lelouma'
tmp_Lelouma_monthly$date <- ymd(paste(tmp_Lelouma_monthly$Year, tmp_Lelouma_monthly$Month, tmp_Lelouma_monthly$day, sep="-"))
tmp_Lelouma_monthly_2015 <- select(tmp_Lelouma_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Lelouma_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Lelouma_monthly_2015a$Location <- 'Lelouma'
tmp_Lelouma_monthly_2015a$date <- ymd(paste(tmp_Lelouma_monthly_2015a$Year, tmp_Lelouma_monthly_2015a$Month, tmp_Lelouma_monthly_2015a$day, sep="-"))
tmp_Lelouma_monthly_2015a <- select(tmp_Lelouma_monthly_2015a, date, Year, Month, day, Location)
tmp_Lelouma_monthly_2015 <- full_join(tmp_Lelouma_monthly_2015a, tmp_Lelouma_monthly_2015)
tmp_Lelouma_monthly <- rbind(select(tmp_Lelouma_monthly,date, Year, Month, day, Location, tmp), tmp_Lelouma_monthly_2015)
rm(tmp_Lelouma_monthly_2015, tmp_Lelouma_monthly_2015a)
tmp_Lelouma_monthly$measurement <- "tmp"
tmp_Lelouma_monthly <- rename(tmp_Lelouma_monthly, Value=tmp)
#Lola
tmp_Lola_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmp1=tmp.var[which(lon==8.25),which(lat==8.25),1:48],
                                        tmp2=tmp.var[which(lon==8.25),which(lat==7.75),1:48]))
tmp_Lola_monthly$tmp <- rowMeans(select(tmp_Lola_monthly, tmp1, tmp2))
tmp_Lola_monthly$Location <- 'Lola'
tmp_Lola_monthly$date <- ymd(paste(tmp_Lola_monthly$Year, tmp_Lola_monthly$Month, tmp_Lola_monthly$day, sep="-"))
tmp_Lola_monthly_2015 <- select(tmp_Lola_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Lola_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Lola_monthly_2015a$Location <- 'Lola'
tmp_Lola_monthly_2015a$date <- ymd(paste(tmp_Lola_monthly_2015a$Year, tmp_Lola_monthly_2015a$Month, tmp_Lola_monthly_2015a$day, sep="-"))
tmp_Lola_monthly_2015a <- select(tmp_Lola_monthly_2015a, date, Year, Month, day, Location)
tmp_Lola_monthly_2015 <- full_join(tmp_Lola_monthly_2015a, tmp_Lola_monthly_2015)
tmp_Lola_monthly <- rbind(select(tmp_Lola_monthly,date, Year, Month, day, Location, tmp), tmp_Lola_monthly_2015)
rm(tmp_Lola_monthly_2015, tmp_Lola_monthly_2015a)
tmp_Lola_monthly$measurement <- "tmp"
tmp_Lola_monthly <- rename(tmp_Lola_monthly, Value=tmp)
#Macenta
tmp_Macenta_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmp1=tmp.var[which(lon==9.75),which(lat==8.75),1:48],
                                           tmp2=tmp.var[which(lon==9.25),which(lat==8.75),1:48],
                                           tmp3=tmp.var[which(lon==9.25),which(lat==8.25),1:48]))
tmp_Macenta_monthly$tmp <- rowMeans(select(tmp_Macenta_monthly, tmp1, tmp2, tmp3))
tmp_Macenta_monthly$Location <- 'Macenta'
tmp_Macenta_monthly$date <- ymd(paste(tmp_Macenta_monthly$Year, tmp_Macenta_monthly$Month, tmp_Macenta_monthly$day, sep="-"))
tmp_Macenta_monthly_2015 <- select(tmp_Macenta_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Macenta_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Macenta_monthly_2015a$Location <- 'Macenta'
tmp_Macenta_monthly_2015a$date <- ymd(paste(tmp_Macenta_monthly_2015a$Year, tmp_Macenta_monthly_2015a$Month, tmp_Macenta_monthly_2015a$day, sep="-"))
tmp_Macenta_monthly_2015a <- select(tmp_Macenta_monthly_2015a, date, Year, Month, day, Location)
tmp_Macenta_monthly_2015 <- full_join(tmp_Macenta_monthly_2015a, tmp_Macenta_monthly_2015)
tmp_Macenta_monthly <- rbind(select(tmp_Macenta_monthly,date, Year, Month, day, Location, tmp), tmp_Macenta_monthly_2015)
rm(tmp_Macenta_monthly_2015, tmp_Macenta_monthly_2015a)
tmp_Macenta_monthly$measurement <- "tmp"
tmp_Macenta_monthly <- rename(tmp_Macenta_monthly, Value=tmp)
#Mali
tmp_Mali_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmp1=tmp.var[which(lon==12.75),which(lat==12.25),1:48],
                                        tmp2=tmp.var[which(lon==12.25),which(lat==12.25),1:48], 
                                        tmp3=tmp.var[which(lon==11.75),which(lat==12.25),1:48],
                                        tmp4=tmp.var[which(lon==12.75),which(lat==11.75),1:48],
                                        tmp5=tmp.var[which(lon==12.25),which(lat==11.75),1:48]))
tmp_Mali_monthly$tmp <- rowMeans(select(tmp_Mali_monthly, tmp1, tmp2, tmp3, tmp4, tmp5))
tmp_Mali_monthly$Location <- 'Mali'
tmp_Mali_monthly$date <- ymd(paste(tmp_Mali_monthly$Year, tmp_Mali_monthly$Month, tmp_Mali_monthly$day, sep="-"))
tmp_Mali_monthly_2015 <- select(tmp_Mali_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Mali_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Mali_monthly_2015a$Location <- 'Mali'
tmp_Mali_monthly_2015a$date <- ymd(paste(tmp_Mali_monthly_2015a$Year, tmp_Mali_monthly_2015a$Month, tmp_Mali_monthly_2015a$day, sep="-"))
tmp_Mali_monthly_2015a <- select(tmp_Mali_monthly_2015a, date, Year, Month, day, Location)
tmp_Mali_monthly_2015 <- full_join(tmp_Mali_monthly_2015a, tmp_Mali_monthly_2015)
tmp_Mali_monthly <- rbind(select(tmp_Mali_monthly,date, Year, Month, day, Location, tmp), tmp_Mali_monthly_2015)
rm(tmp_Mali_monthly_2015, tmp_Mali_monthly_2015a)
tmp_Mali_monthly$measurement <- "tmp"
tmp_Mali_monthly <- rename(tmp_Mali_monthly, Value=tmp)
#Mamou
tmp_Mamou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         tmp1=tmp.var[which(lon==11.75),which(lat==10.75),1:48],
                                         tmp2=tmp.var[which(lon==12.25),which(lat==10.25),1:48],
                                         tmp3=tmp.var[which(lon==11.75),which(lat==10.25),1:48]))
tmp_Mamou_monthly$tmp <- rowMeans(select(tmp_Mamou_monthly, tmp1, tmp2, tmp3))
tmp_Mamou_monthly$Location <- 'Mamou'
tmp_Mamou_monthly$date <- ymd(paste(tmp_Mamou_monthly$Year, tmp_Mamou_monthly$Month, tmp_Mamou_monthly$day, sep="-"))
tmp_Mamou_monthly_2015 <- select(tmp_Mamou_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Mamou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Mamou_monthly_2015a$Location <- 'Mamou'
tmp_Mamou_monthly_2015a$date <- ymd(paste(tmp_Mamou_monthly_2015a$Year, tmp_Mamou_monthly_2015a$Month, tmp_Mamou_monthly_2015a$day, sep="-"))
tmp_Mamou_monthly_2015a <- select(tmp_Mamou_monthly_2015a, date, Year, Month, day, Location)
tmp_Mamou_monthly_2015 <- full_join(tmp_Mamou_monthly_2015a, tmp_Mamou_monthly_2015)
tmp_Mamou_monthly <- rbind(select(tmp_Mamou_monthly,date, Year, Month, day, Location, tmp), tmp_Mamou_monthly_2015)
rm(tmp_Mamou_monthly_2015, tmp_Mamou_monthly_2015a)
tmp_Mamou_monthly$measurement <- "tmp"
tmp_Mamou_monthly <- rename(tmp_Mamou_monthly, Value=tmp)
#Nzerekore
tmp_Nzerekore_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             tmp1=tmp.var[which(lon==8.75),which(lat==8.25),1:48],
                                             tmp2=tmp.var[which(lon==8.75),which(lat==7.75),1:48]))
tmp_Nzerekore_monthly$tmp <- rowMeans(select(tmp_Nzerekore_monthly, tmp1, tmp2))
tmp_Nzerekore_monthly$Location <- 'Nzerekore'
tmp_Nzerekore_monthly$date <- ymd(paste(tmp_Nzerekore_monthly$Year, tmp_Nzerekore_monthly$Month, tmp_Nzerekore_monthly$day, sep="-"))
tmp_Nzerekore_monthly_2015 <- select(tmp_Nzerekore_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Nzerekore_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Nzerekore_monthly_2015a$Location <- 'Nzerekore'
tmp_Nzerekore_monthly_2015a$date <- ymd(paste(tmp_Nzerekore_monthly_2015a$Year, tmp_Nzerekore_monthly_2015a$Month, tmp_Nzerekore_monthly_2015a$day, sep="-"))
tmp_Nzerekore_monthly_2015a <- select(tmp_Nzerekore_monthly_2015a, date, Year, Month, day, Location)
tmp_Nzerekore_monthly_2015 <- full_join(tmp_Nzerekore_monthly_2015a, tmp_Nzerekore_monthly_2015)
tmp_Nzerekore_monthly <- rbind(select(tmp_Nzerekore_monthly,date, Year, Month, day, Location, tmp), tmp_Nzerekore_monthly_2015)
rm(tmp_Nzerekore_monthly_2015, tmp_Nzerekore_monthly_2015a)
tmp_Nzerekore_monthly$measurement <- "tmp"
tmp_Nzerekore_monthly <- rename(tmp_Nzerekore_monthly, Value=tmp)
#Pita
tmp_Pita_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmp1=tmp.var[which(lon==12.75),which(lat==11.25),1:48],
                                        tmp2=tmp.var[which(lon==12.25),which(lat==11.25),1:48],
                                        tmp3=tmp.var[which(lon==12.75),which(lat==10.75),1:48]))
tmp_Pita_monthly$tmp <- rowMeans(select(tmp_Pita_monthly, tmp1, tmp2, tmp3))
tmp_Pita_monthly$Location <- 'Pita'
tmp_Pita_monthly$date <- ymd(paste(tmp_Pita_monthly$Year, tmp_Pita_monthly$Month, tmp_Pita_monthly$day, sep="-"))
tmp_Pita_monthly_2015 <- select(tmp_Pita_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Pita_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Pita_monthly_2015a$Location <- 'Pita'
tmp_Pita_monthly_2015a$date <- ymd(paste(tmp_Pita_monthly_2015a$Year, tmp_Pita_monthly_2015a$Month, tmp_Pita_monthly_2015a$day, sep="-"))
tmp_Pita_monthly_2015a <- select(tmp_Pita_monthly_2015a, date, Year, Month, day, Location)
tmp_Pita_monthly_2015 <- full_join(tmp_Pita_monthly_2015a, tmp_Pita_monthly_2015)
tmp_Pita_monthly <- rbind(select(tmp_Pita_monthly,date, Year, Month, day, Location, tmp), tmp_Pita_monthly_2015)
rm(tmp_Pita_monthly_2015, tmp_Pita_monthly_2015a)
tmp_Pita_monthly$measurement <- "tmp"
tmp_Pita_monthly <- rename(tmp_Pita_monthly, Value=tmp)
#Siguiri
tmp_Siguiri_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmp1=tmp.var[which(lon==9.25),which(lat==12.25),1:48],
                                           tmp2=tmp.var[which(lon==10.25),which(lat==11.75),1:48], 
                                           tmp3=tmp.var[which(lon==9.75),which(lat==11.75),1:48],
                                           tmp4=tmp.var[which(lon==9.25),which(lat==11.75),1:48],
                                           tmp5=tmp.var[which(lon==9.75),which(lat==11.25),1:48],
                                           tmp6=tmp.var[which(lon==9.25),which(lat==11.25),1:48]))
tmp_Siguiri_monthly$tmp <- rowMeans(select(tmp_Siguiri_monthly, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6))
tmp_Siguiri_monthly$Location <- 'Siguiri'
tmp_Siguiri_monthly$date <- ymd(paste(tmp_Siguiri_monthly$Year, tmp_Siguiri_monthly$Month, tmp_Siguiri_monthly$day, sep="-"))
tmp_Siguiri_monthly_2015 <- select(tmp_Siguiri_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Siguiri_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Siguiri_monthly_2015a$Location <- 'Siguiri'
tmp_Siguiri_monthly_2015a$date <- ymd(paste(tmp_Siguiri_monthly_2015a$Year, tmp_Siguiri_monthly_2015a$Month, tmp_Siguiri_monthly_2015a$day, sep="-"))
tmp_Siguiri_monthly_2015a <- select(tmp_Siguiri_monthly_2015a, date, Year, Month, day, Location)
tmp_Siguiri_monthly_2015 <- full_join(tmp_Siguiri_monthly_2015a, tmp_Siguiri_monthly_2015)
tmp_Siguiri_monthly <- rbind(select(tmp_Siguiri_monthly,date, Year, Month, day, Location, tmp), tmp_Siguiri_monthly_2015)
rm(tmp_Siguiri_monthly_2015, tmp_Siguiri_monthly_2015a)
tmp_Siguiri_monthly$measurement <- "tmp"
tmp_Siguiri_monthly <- rename(tmp_Siguiri_monthly, Value=tmp)
#Telimele
tmp_Telimele_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            tmp1=tmp.var[which(lon==13.75),which(lat==11.25),1:48],
                                            tmp2=tmp.var[which(lon==13.25),which(lat==11.25),1:48], 
                                            tmp3=tmp.var[which(lon==13.75),which(lat==10.75),1:48],
                                            tmp4=tmp.var[which(lon==13.25),which(lat==10.75),1:48]))
tmp_Telimele_monthly$tmp <- rowMeans(select(tmp_Telimele_monthly, tmp1, tmp2, tmp3, tmp4))
tmp_Telimele_monthly$Location <- 'Telimele'
tmp_Telimele_monthly$date <- ymd(paste(tmp_Telimele_monthly$Year, tmp_Telimele_monthly$Month, tmp_Telimele_monthly$day, sep="-"))
tmp_Telimele_monthly_2015 <- select(tmp_Telimele_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Telimele_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Telimele_monthly_2015a$Location <- 'Telimele'
tmp_Telimele_monthly_2015a$date <- ymd(paste(tmp_Telimele_monthly_2015a$Year, tmp_Telimele_monthly_2015a$Month, tmp_Telimele_monthly_2015a$day, sep="-"))
tmp_Telimele_monthly_2015a <- select(tmp_Telimele_monthly_2015a, date, Year, Month, day, Location)
tmp_Telimele_monthly_2015 <- full_join(tmp_Telimele_monthly_2015a, tmp_Telimele_monthly_2015)
tmp_Telimele_monthly <- rbind(select(tmp_Telimele_monthly,date, Year, Month, day, Location, tmp), tmp_Telimele_monthly_2015)
rm(tmp_Telimele_monthly_2015, tmp_Telimele_monthly_2015a)
tmp_Telimele_monthly$measurement <- "tmp"
tmp_Telimele_monthly <- rename(tmp_Telimele_monthly, Value=tmp)
#Tougue
tmp_Tougue_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4),day=1, 
                                          tmp1=tmp.var[which(lon==11.25),which(lat==11.75),1:48],
                                          tmp2=tmp.var[which(lon==11.75),which(lat==11.25),1:48]))
tmp_Tougue_monthly$tmp <- rowMeans(select(tmp_Tougue_monthly, tmp1, tmp2))
tmp_Tougue_monthly$Location <- 'Tougue'
tmp_Tougue_monthly$date <- ymd(paste(tmp_Tougue_monthly$Year, tmp_Tougue_monthly$Month, tmp_Tougue_monthly$day, sep="-"))
tmp_Tougue_monthly_2015 <- select(tmp_Tougue_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_Tougue_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_Tougue_monthly_2015a$Location <- 'Tougue'
tmp_Tougue_monthly_2015a$date <- ymd(paste(tmp_Tougue_monthly_2015a$Year, tmp_Tougue_monthly_2015a$Month, tmp_Tougue_monthly_2015a$day, sep="-"))
tmp_Tougue_monthly_2015a <- select(tmp_Tougue_monthly_2015a, date, Year, Month, day, Location)
tmp_Tougue_monthly_2015 <- full_join(tmp_Tougue_monthly_2015a, tmp_Tougue_monthly_2015)
tmp_Tougue_monthly <- rbind(select(tmp_Tougue_monthly,date, Year, Month, day, Location, tmp), tmp_Tougue_monthly_2015)
rm(tmp_Tougue_monthly_2015, tmp_Tougue_monthly_2015a)
tmp_Tougue_monthly$measurement <- "tmp"
tmp_Tougue_monthly <- rename(tmp_Tougue_monthly, Value=tmp)
#yamou
tmp_yamou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4),day=1, 
                                         tmp1=tmp.var[which(lon==9.25),which(lat==7.75),1:48],
                                         tmp2=tmp.var[which(lon==9.25),which(lat==7.25),1:48],
                                         tmp3=tmp.var[which(lon==8.75),which(lat==7.25),1:48]))
tmp_yamou_monthly$Location <- 'yamou'
tmp_yamou_monthly$tmp <- rowMeans(select(tmp_yamou_monthly, tmp1, tmp2, tmp3))
tmp_yamou_monthly$date <- ymd(paste(tmp_yamou_monthly$Year, tmp_yamou_monthly$Month, tmp_yamou_monthly$day, sep="-"))
tmp_yamou_monthly_2015 <- select(tmp_yamou_monthly, Location, Year, Month, tmp) %>% group_by( Month) %>% summarize(tmp=mean(tmp))
tmp_yamou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmp_yamou_monthly_2015a$Location <- 'yamou'
tmp_yamou_monthly_2015a$date <- ymd(paste(tmp_yamou_monthly_2015a$Year, tmp_yamou_monthly_2015a$Month, tmp_yamou_monthly_2015a$day, sep="-"))
tmp_yamou_monthly_2015a <- select(tmp_yamou_monthly_2015a, date, Year, Month, day, Location)
tmp_yamou_monthly_2015 <- full_join(tmp_yamou_monthly_2015a, tmp_yamou_monthly_2015)
tmp_yamou_monthly <- rbind(select(tmp_yamou_monthly,date, Year, Month, day, Location, tmp), tmp_yamou_monthly_2015)
rm(tmp_yamou_monthly_2015, tmp_yamou_monthly_2015a)
tmp_yamou_monthly$measurement <- "tmp"
tmp_yamou_monthly <- rename(tmp_yamou_monthly, Value=tmp)

#Merging in long format
tmp_Guinea_monthly_district <- rbind(tmp_Beyla_monthly, tmp_Boke_monthly, tmp_Boffa_monthly,
                                     tmp_Conakry_monthly, tmp_Coyah_monthly, tmp_Dabola_monthly, tmp_Dalaba_monthly,
                                     tmp_Dinguiray_monthly, tmp_Dubreka_monthly, tmp_Faranah_monthly,
                                     tmp_Forecariah_monthly, tmp_Fria_monthly, tmp_Gaoual_monthly,
                                     tmp_Gueckedou_monthly, tmp_Kankan_monthly, tmp_Kerouane_monthly,
                                     tmp_Kindia_monthly, tmp_Kissidougou_monthly, tmp_Koubia_monthly,
                                     tmp_Koundara_monthly, tmp_Kouroussa_monthly, tmp_Labe_monthly,
                                     tmp_Lelouma_monthly, tmp_Lola_monthly, tmp_Macenta_monthly,
                                     tmp_Mali_monthly, tmp_Mamou_monthly, tmp_Nzerekore_monthly,
                                     tmp_Pita_monthly, tmp_Siguiri_monthly, tmp_Telimele_monthly,
                                     tmp_Tougue_monthly, tmp_yamou_monthly)


###############
#tmx - max Temp
###############
tmx.full <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.tmx.dat.nc', write=FALSE)
tmx.var <- var.get.nc(tmx.full, "tmx")
lon <- var.get.nc(tmx.full, "lon")
lat <- var.get.nc(tmx.full, "lat")
#Beyla
tmx_Beyla_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         tmx1=tmx.var[which(lon==8.75),which(lat==9.25),1:48],
                                         tmx2=tmx.var[which(lon==8.25),which(lat==9.25),1:48], 
                                         tmx3=tmx.var[which(lon==7.75),which(lat==9.25),1:48],
                                         tmx4=tmx.var[which(lon==8.75),which(lat==8.75),1:48], 
                                         tmx5=tmx.var[which(lon==8.25),which(lat==8.75),1:48],
                                         tmx6=tmx.var[which(lon==7.75),which(lat==8.75),1:48], 
                                         tmx7=tmx.var[which(lon==8.75),which(lat==8.25),1:48],
                                         tmx8=tmx.var[which(lon==8.25),which(lat==8.25),1:48]))
tmx_Beyla_monthly$tmx <- rowMeans(select(tmx_Beyla_monthly, tmx1, tmx2, tmx3, tmx4, tmx5, tmx6, tmx7, tmx8))
tmx_Beyla_monthly$Location <- 'Beyla'
tmx_Beyla_monthly$date <- ymd(paste(tmx_Beyla_monthly$Year, tmx_Beyla_monthly$Month, tmx_Beyla_monthly$day, sep="-"))
tmx_Beyla_monthly_2015 <- select(tmx_Beyla_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Beyla_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Beyla_monthly_2015a$Location <- 'Beyla'
tmx_Beyla_monthly_2015a$date <- ymd(paste(tmx_Beyla_monthly_2015a$Year, tmx_Beyla_monthly_2015a$Month, tmx_Beyla_monthly_2015a$day, sep="-"))
tmx_Beyla_monthly_2015a <- select(tmx_Beyla_monthly_2015a, date, Year, Month, day, Location)
tmx_Beyla_monthly_2015 <- full_join(tmx_Beyla_monthly_2015a, tmx_Beyla_monthly_2015)
tmx_Beyla_monthly <- rbind(select(tmx_Beyla_monthly,date, Year, Month, day, Location, tmx), tmx_Beyla_monthly_2015)
rm(tmx_Beyla_monthly_2015, tmx_Beyla_monthly_2015a)
tmx_Beyla_monthly$measurement <- "tmx"
tmx_Beyla_monthly <- rename(tmx_Beyla_monthly, Value=tmx)
#Boffa
tmx_Boffa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         tmx1=tmx.var[which(lon==14.25),which(lat==10.75),1:48],
                                         tmx2=tmx.var[which(lon==13.75),which(lat==10.75),1:48], 
                                         tmx3=tmx.var[which(lon==14.25),which(lat==10.25),1:48],
                                         tmx4=tmx.var[which(lon==13.75),which(lat==10.25),1:48]))
tmx_Boffa_monthly$tmx <- rowMeans(select(tmx_Boffa_monthly, tmx1, tmx2, tmx3, tmx4))
tmx_Boffa_monthly$Location <- 'Boffa'
tmx_Boffa_monthly$date <- ymd(paste(tmx_Boffa_monthly$Year, tmx_Boffa_monthly$Month, tmx_Boffa_monthly$day, sep="-"))
tmx_Boffa_monthly_2015 <- select(tmx_Boffa_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Boffa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Boffa_monthly_2015a$Location <- 'Boffa'
tmx_Boffa_monthly_2015a$date <- ymd(paste(tmx_Boffa_monthly_2015a$Year, tmx_Boffa_monthly_2015a$Month, tmx_Boffa_monthly_2015a$day, sep="-"))
tmx_Boffa_monthly_2015a <- select(tmx_Boffa_monthly_2015a, date, Year, Month, day, Location)
tmx_Boffa_monthly_2015 <- full_join(tmx_Boffa_monthly_2015a, tmx_Boffa_monthly_2015)
tmx_Boffa_monthly <- rbind(select(tmx_Boffa_monthly,date, Year, Month, day, Location, tmx), tmx_Boffa_monthly_2015)
rm(tmx_Boffa_monthly_2015, tmx_Boffa_monthly_2015a)
tmx_Boffa_monthly$measurement <- "tmx"
tmx_Boffa_monthly <- rename(tmx_Boffa_monthly, Value=tmx)
#Boke
tmx_Boke_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmx1=tmx.var[which(lon==14.25),which(lat==11.75),1:48],
                                        tmx2=tmx.var[which(lon==14.75),which(lat==11.25),1:48], 
                                        tmx3=tmx.var[which(lon==14.25),which(lat==11.25),1:48],
                                        tmx4=tmx.var[which(lon==13.75),which(lat==11.25),1:48]))
tmx_Boke_monthly$tmx <- rowMeans(select(tmx_Boke_monthly, tmx1, tmx2, tmx3, tmx4))
tmx_Boke_monthly$Location <- 'Boke'
tmx_Boke_monthly$date <- ymd(paste(tmx_Boke_monthly$Year, tmx_Boke_monthly$Month, tmx_Boke_monthly$day, sep="-"))
tmx_Boke_monthly_2015 <- select(tmx_Boke_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Boke_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Boke_monthly_2015a$Location <- 'Boke'
tmx_Boke_monthly_2015a$date <- ymd(paste(tmx_Boke_monthly_2015a$Year, tmx_Boke_monthly_2015a$Month, tmx_Boke_monthly_2015a$day, sep="-"))
tmx_Boke_monthly_2015a <- select(tmx_Boke_monthly_2015a, date, Year, Month, day, Location)
tmx_Boke_monthly_2015 <- full_join(tmx_Boke_monthly_2015a, tmx_Boke_monthly_2015)
tmx_Boke_monthly <- rbind(select(tmx_Boke_monthly,date, Year, Month, day, Location, tmx), tmx_Boke_monthly_2015)
rm(tmx_Boke_monthly_2015, tmx_Boke_monthly_2015a)
tmx_Boke_monthly$measurement <- "tmx"
tmx_Boke_monthly <- rename(tmx_Boke_monthly, Value=tmx)
#Conakry
tmx_Conakry_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmx=tmx.var[which(lon==13.75),which(lat==9.75),1:48]))
tmx_Conakry_monthly$Location <- 'Conakry'
tmx_Conakry_monthly$date <- ymd(paste(tmx_Conakry_monthly$Year, tmx_Conakry_monthly$Month, tmx_Conakry_monthly$day, sep="-"))
tmx_Conakry_monthly_2015 <- select(tmx_Conakry_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Conakry_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Conakry_monthly_2015a$Location <- 'Conakry'
tmx_Conakry_monthly_2015a$date <- ymd(paste(tmx_Conakry_monthly_2015a$Year, tmx_Conakry_monthly_2015a$Month, tmx_Conakry_monthly_2015a$day, sep="-"))
tmx_Conakry_monthly_2015a <- select(tmx_Conakry_monthly_2015a, date, Year, Month, day, Location)
tmx_Conakry_monthly_2015 <- full_join(tmx_Conakry_monthly_2015a, tmx_Conakry_monthly_2015)
tmx_Conakry_monthly <- rbind(select(tmx_Conakry_monthly,date, Year, Month, day, Location, tmx), tmx_Conakry_monthly_2015)
rm(tmx_Conakry_monthly_2015, tmx_Conakry_monthly_2015a)
tmx_Conakry_monthly$measurement <- "tmx"
tmx_Conakry_monthly <- rename(tmx_Conakry_monthly, Value=tmx)
#Coyah
tmx_Coyah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         tmx=tmx.var[which(lon==9.75),which(lat==13.25),1:48]))
tmx_Coyah_monthly$Location <- 'Coyah'
tmx_Coyah_monthly$date <- ymd(paste(tmx_Coyah_monthly$Year, tmx_Coyah_monthly$Month, tmx_Coyah_monthly$day, sep="-"))
tmx_Coyah_monthly_2015 <- select(tmx_Coyah_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Coyah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Coyah_monthly_2015a$Location <- 'Coyah'
tmx_Coyah_monthly_2015a$date <- ymd(paste(tmx_Coyah_monthly_2015a$Year, tmx_Coyah_monthly_2015a$Month, tmx_Coyah_monthly_2015a$day, sep="-"))
tmx_Coyah_monthly_2015a <- select(tmx_Coyah_monthly_2015a, date, Year, Month, day, Location)
tmx_Coyah_monthly_2015 <- full_join(tmx_Coyah_monthly_2015a, tmx_Coyah_monthly_2015)
tmx_Coyah_monthly <- rbind(select(tmx_Coyah_monthly,date, Year, Month, day, Location, tmx), tmx_Coyah_monthly_2015)
rm(tmx_Coyah_monthly_2015, tmx_Coyah_monthly_2015a)
tmx_Coyah_monthly$measurement <- "tmx"
tmx_Coyah_monthly <- rename(tmx_Coyah_monthly, Value=tmx)
#Dabola
tmx_Dabola_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmx1=tmx.var[which(lon==11.25),which(lat==10.75),1:48],
                                          tmx2=tmx.var[which(lon==10.75),which(lat==10.75),1:48]))
tmx_Dabola_monthly$tmx <- rowMeans(select(tmx_Dabola_monthly, tmx1, tmx2))
tmx_Dabola_monthly$Location <- 'Dabola'
tmx_Dabola_monthly$date <- ymd(paste(tmx_Dabola_monthly$Year, tmx_Dabola_monthly$Month, tmx_Dabola_monthly$day, sep="-"))
tmx_Dabola_monthly_2015 <- select(tmx_Dabola_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Dabola_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Dabola_monthly_2015a$Location <- 'Dabola'
tmx_Dabola_monthly_2015a$date <- ymd(paste(tmx_Dabola_monthly_2015a$Year, tmx_Dabola_monthly_2015a$Month, tmx_Dabola_monthly_2015a$day, sep="-"))
tmx_Dabola_monthly_2015a <- select(tmx_Dabola_monthly_2015a, date, Year, Month, day, Location)
tmx_Dabola_monthly_2015 <- full_join(tmx_Dabola_monthly_2015a, tmx_Dabola_monthly_2015)
tmx_Dabola_monthly <- rbind(select(tmx_Dabola_monthly,date, Year, Month, day, Location, tmx), tmx_Dabola_monthly_2015)
rm(tmx_Dabola_monthly_2015, tmx_Dabola_monthly_2015a)
tmx_Dabola_monthly$measurement <- "tmx"
tmx_Dabola_monthly <- rename(tmx_Dabola_monthly, Value=tmx)
#Dalaba
tmx_Dalaba_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmx1=tmx.var[which(lon==11.25),which(lat==12.25),1:48],
                                          tmx2=tmx.var[which(lon==10.75),which(lat==12.25),1:48]))
tmx_Dalaba_monthly$tmx <- rowMeans(select(tmx_Dalaba_monthly, tmx1, tmx2))
tmx_Dalaba_monthly$Location <- 'Dalaba'
tmx_Dalaba_monthly$date <- ymd(paste(tmx_Dalaba_monthly$Year, tmx_Dalaba_monthly$Month, tmx_Dalaba_monthly$day, sep="-"))
tmx_Dalaba_monthly_2015 <- select(tmx_Dalaba_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Dalaba_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Dalaba_monthly_2015a$Location <- 'Dalaba'
tmx_Dalaba_monthly_2015a$date <- ymd(paste(tmx_Dalaba_monthly_2015a$Year, tmx_Dalaba_monthly_2015a$Month, tmx_Dalaba_monthly_2015a$day, sep="-"))
tmx_Dalaba_monthly_2015a <- select(tmx_Dalaba_monthly_2015a, date, Year, Month, day, Location)
tmx_Dalaba_monthly_2015 <- full_join(tmx_Dalaba_monthly_2015a, tmx_Dalaba_monthly_2015)
tmx_Dalaba_monthly <- rbind(select(tmx_Dalaba_monthly,date, Year, Month, day, Location, tmx), tmx_Dalaba_monthly_2015)
rm(tmx_Dalaba_monthly_2015, tmx_Dalaba_monthly_2015a)
tmx_Dalaba_monthly$measurement <- "tmx"
tmx_Dalaba_monthly <- rename(tmx_Dalaba_monthly, Value=tmx)
#Dinguiray
tmx_Dinguiray_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             tmx1=tmx.var[which(lon==11.25),which(lat==11.75),1:48],
                                             tmx2=tmx.var[which(lon==10.75),which(lat==11.75),1:48], 
                                             tmx3=tmx.var[which(lon==10.25),which(lat==11.75),1:48],
                                             tmx4=tmx.var[which(lon==11.25),which(lat==11.25),1:48],
                                             tmx5=tmx.var[which(lon==10.75),which(lat==11.25),1:48],
                                             tmx6=tmx.var[which(lon==10.25),which(lat==11.25),1:48]))
tmx_Dinguiray_monthly$tmx <- rowMeans(select(tmx_Dinguiray_monthly, tmx1, tmx2, tmx3, tmx4, tmx5, tmx6))
tmx_Dinguiray_monthly$Location <- 'Dinguiray'
tmx_Dinguiray_monthly$date <- ymd(paste(tmx_Dinguiray_monthly$Year, tmx_Dinguiray_monthly$Month, tmx_Dinguiray_monthly$day, sep="-"))
tmx_Dinguiray_monthly_2015 <- select(tmx_Dinguiray_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Dinguiray_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Dinguiray_monthly_2015a$Location <- 'Dinguiray'
tmx_Dinguiray_monthly_2015a$date <- ymd(paste(tmx_Dinguiray_monthly_2015a$Year, tmx_Dinguiray_monthly_2015a$Month, tmx_Dinguiray_monthly_2015a$day, sep="-"))
tmx_Dinguiray_monthly_2015a <- select(tmx_Dinguiray_monthly_2015a, date, Year, Month, day, Location)
tmx_Dinguiray_monthly_2015 <- full_join(tmx_Dinguiray_monthly_2015a, tmx_Dinguiray_monthly_2015)
tmx_Dinguiray_monthly <- rbind(select(tmx_Dinguiray_monthly,date, Year, Month, day, Location, tmx), tmx_Dinguiray_monthly_2015)
rm(tmx_Dinguiray_monthly_2015, tmx_Dinguiray_monthly_2015a)
tmx_Dinguiray_monthly$measurement <- "tmx"
tmx_Dinguiray_monthly <- rename(tmx_Dinguiray_monthly, Value=tmx)
#Dubreka
tmx_Dubreka_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmx1=tmx.var[which(lon==13.75),which(lat==10.25),1:48],
                                           tmx2=tmx.var[which(lon==13.25),which(lat==10.25),1:48]))
tmx_Dubreka_monthly$tmx <- rowMeans(select(tmx_Dubreka_monthly, tmx1, tmx2))
tmx_Dubreka_monthly$Location <- 'Dubreka'
tmx_Dubreka_monthly$date <- ymd(paste(tmx_Dubreka_monthly$Year, tmx_Dubreka_monthly$Month, tmx_Dubreka_monthly$day, sep="-"))
tmx_Dubreka_monthly_2015 <- select(tmx_Dubreka_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Dubreka_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Dubreka_monthly_2015a$Location <- 'Dubreka'
tmx_Dubreka_monthly_2015a$date <- ymd(paste(tmx_Dubreka_monthly_2015a$Year, tmx_Dubreka_monthly_2015a$Month, tmx_Dubreka_monthly_2015a$day, sep="-"))
tmx_Dubreka_monthly_2015a <- select(tmx_Dubreka_monthly_2015a, date, Year, Month, day, Location)
tmx_Dubreka_monthly_2015 <- full_join(tmx_Dubreka_monthly_2015a, tmx_Dubreka_monthly_2015)
tmx_Dubreka_monthly <- rbind(select(tmx_Dubreka_monthly,date, Year, Month, day, Location, tmx), tmx_Dubreka_monthly_2015)
rm(tmx_Dubreka_monthly_2015, tmx_Dubreka_monthly_2015a)
tmx_Dubreka_monthly$measurement <- "tmx"
tmx_Dubreka_monthly <- rename(tmx_Dubreka_monthly, Value=tmx)
#Faranah
tmx_Faranah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmx1=tmx.var[which(lon==11.25),which(lat==10.25),1:48],
                                           tmx2=tmx.var[which(lon==10.75),which(lat==10.25),1:48], 
                                           tmx3=tmx.var[which(lon==10.25),which(lat==10.25),1:48],
                                           tmx4=tmx.var[which(lon==10.75),which(lat==9.75),1:48],
                                           tmx5=tmx.var[which(lon==10.75),which(lat==9.25),1:48]))
tmx_Faranah_monthly$tmx <- rowMeans(select(tmx_Faranah_monthly, tmx1, tmx2, tmx3, tmx4, tmx5))
tmx_Faranah_monthly$Location <- 'Faranah'
tmx_Faranah_monthly$date <- ymd(paste(tmx_Faranah_monthly$Year, tmx_Faranah_monthly$Month, tmx_Faranah_monthly$day, sep="-"))
tmx_Faranah_monthly_2015 <- select(tmx_Faranah_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Faranah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Faranah_monthly_2015a$Location <- 'Faranah'
tmx_Faranah_monthly_2015a$date <- ymd(paste(tmx_Faranah_monthly_2015a$Year, tmx_Faranah_monthly_2015a$Month, tmx_Faranah_monthly_2015a$day, sep="-"))
tmx_Faranah_monthly_2015a <- select(tmx_Faranah_monthly_2015a, date, Year, Month, day, Location)
tmx_Faranah_monthly_2015 <- full_join(tmx_Faranah_monthly_2015a, tmx_Faranah_monthly_2015)
tmx_Faranah_monthly <- rbind(select(tmx_Faranah_monthly,date, Year, Month, day, Location, tmx), tmx_Faranah_monthly_2015)
rm(tmx_Faranah_monthly_2015, tmx_Faranah_monthly_2015a)
tmx_Faranah_monthly$measurement <- "tmx"
tmx_Faranah_monthly <- rename(tmx_Faranah_monthly, Value=tmx)
#Forecariah
tmx_Forecariah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                              Month=rep(seq(1,12,1), 4), day=1,
                                              tmx1=tmx.var[which(lon==12.75),which(lat==9.75),1:48],
                                              tmx2=tmx.var[which(lon==13.25),which(lat==9.25),1:48], 
                                              tmx3=tmx.var[which(lon==12.75),which(lat==9.25),1:48]))
tmx_Forecariah_monthly$tmx <- rowMeans(select(tmx_Forecariah_monthly, tmx1, tmx2, tmx3))
tmx_Forecariah_monthly$Location <- 'Forecariah'
tmx_Forecariah_monthly$date <- ymd(paste(tmx_Forecariah_monthly$Year, tmx_Forecariah_monthly$Month, tmx_Forecariah_monthly$day, sep="-"))
tmx_Forecariah_monthly_2015 <- select(tmx_Forecariah_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Forecariah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Forecariah_monthly_2015a$Location <- 'Forecariah'
tmx_Forecariah_monthly_2015a$date <- ymd(paste(tmx_Forecariah_monthly_2015a$Year, tmx_Forecariah_monthly_2015a$Month, tmx_Forecariah_monthly_2015a$day, sep="-"))
tmx_Forecariah_monthly_2015a <- select(tmx_Forecariah_monthly_2015a, date, Year, Month, day, Location)
tmx_Forecariah_monthly_2015 <- full_join(tmx_Forecariah_monthly_2015a, tmx_Forecariah_monthly_2015)
tmx_Forecariah_monthly <- rbind(select(tmx_Forecariah_monthly,date, Year, Month, day, Location, tmx), tmx_Forecariah_monthly_2015)
rm(tmx_Forecariah_monthly_2015, tmx_Forecariah_monthly_2015a)
tmx_Forecariah_monthly$measurement <- "tmx"
tmx_Forecariah_monthly <- rename(tmx_Forecariah_monthly, Value=tmx)
#Fria
tmx_Fria_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmx1=tmx.var[which(lon==13.75),which(lat==10.75),1:48],
                                        tmx2=tmx.var[which(lon==13.75),which(lat==10.25),1:48]))
tmx_Fria_monthly$tmx <- rowMeans(select(tmx_Fria_monthly, tmx1, tmx2))
tmx_Fria_monthly$Location <- 'Fria'
tmx_Fria_monthly$date <- ymd(paste(tmx_Fria_monthly$Year, tmx_Fria_monthly$Month, tmx_Fria_monthly$day, sep="-"))
tmx_Fria_monthly_2015 <- select(tmx_Fria_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Fria_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Fria_monthly_2015a$Location <- 'Fria'
tmx_Fria_monthly_2015a$date <- ymd(paste(tmx_Fria_monthly_2015a$Year, tmx_Fria_monthly_2015a$Month, tmx_Fria_monthly_2015a$day, sep="-"))
tmx_Fria_monthly_2015a <- select(tmx_Fria_monthly_2015a, date, Year, Month, day, Location)
tmx_Fria_monthly_2015 <- full_join(tmx_Fria_monthly_2015a, tmx_Fria_monthly_2015)
tmx_Fria_monthly <- rbind(select(tmx_Fria_monthly,date, Year, Month, day, Location, tmx), tmx_Fria_monthly_2015)
rm(tmx_Fria_monthly_2015, tmx_Fria_monthly_2015a)
tmx_Fria_monthly$measurement <- "tmx"
tmx_Fria_monthly <- rename(tmx_Fria_monthly, Value=tmx)
#Gaoual
tmx_Gaoual_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmx1=tmx.var[which(lon==13.75),which(lat==12.25),1:48],
                                          tmx2=tmx.var[which(lon==13.25),which(lat==12.25),1:48], 
                                          tmx3=tmx.var[which(lon==13.75),which(lat==11.75),1:48],
                                          tmx4=tmx.var[which(lon==13.25),which(lat==11.75),1:48],
                                          tmx5=tmx.var[which(lon==12.75),which(lat==11.75),1:48],
                                          tmx6=tmx.var[which(lon==13.75),which(lat==11.25),1:48],
                                          tmx7=tmx.var[which(lon==13.25),which(lat==11.25),1:48]))
tmx_Gaoual_monthly$tmx <- rowMeans(select(tmx_Gaoual_monthly, tmx1, tmx2, tmx3, tmx4, tmx5, tmx6, tmx7))
tmx_Gaoual_monthly$Location <- 'Gaoual'
tmx_Gaoual_monthly$date <- ymd(paste(tmx_Gaoual_monthly$Year, tmx_Gaoual_monthly$Month, tmx_Gaoual_monthly$day, sep="-"))
tmx_Gaoual_monthly_2015 <- select(tmx_Gaoual_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Gaoual_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Gaoual_monthly_2015a$Location <- 'Gaoual'
tmx_Gaoual_monthly_2015a$date <- ymd(paste(tmx_Gaoual_monthly_2015a$Year, tmx_Gaoual_monthly_2015a$Month, tmx_Gaoual_monthly_2015a$day, sep="-"))
tmx_Gaoual_monthly_2015a <- select(tmx_Gaoual_monthly_2015a, date, Year, Month, day, Location)
tmx_Gaoual_monthly_2015 <- full_join(tmx_Gaoual_monthly_2015a, tmx_Gaoual_monthly_2015)
tmx_Gaoual_monthly <- rbind(select(tmx_Gaoual_monthly,date, Year, Month, day, Location, tmx), tmx_Gaoual_monthly_2015)
rm(tmx_Gaoual_monthly_2015, tmx_Gaoual_monthly_2015a)
tmx_Gaoual_monthly$measurement <- "tmx"
tmx_Gaoual_monthly <- rename(tmx_Gaoual_monthly, Value=tmx)
#Gueckedou
tmx_Gueckedou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             tmx=tmx.var[which(lon==10.25),which(lat==8.75),1:48]))
tmx_Gueckedou_monthly$Location <- 'Gueckedou'
tmx_Gueckedou_monthly$date <- ymd(paste(tmx_Gueckedou_monthly$Year, tmx_Gueckedou_monthly$Month, tmx_Gueckedou_monthly$day, sep="-"))
tmx_Gueckedou_monthly_2015 <- select(tmx_Gueckedou_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Gueckedou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Gueckedou_monthly_2015a$Location <- 'Gueckedou'
tmx_Gueckedou_monthly_2015a$date <- ymd(paste(tmx_Gueckedou_monthly_2015a$Year, tmx_Gueckedou_monthly_2015a$Month, tmx_Gueckedou_monthly_2015a$day, sep="-"))
tmx_Gueckedou_monthly_2015a <- select(tmx_Gueckedou_monthly_2015a, date, Year, Month, day, Location)
tmx_Gueckedou_monthly_2015 <- full_join(tmx_Gueckedou_monthly_2015a, tmx_Gueckedou_monthly_2015)
tmx_Gueckedou_monthly <- rbind(select(tmx_Gueckedou_monthly,date, Year, Month, day, Location, tmx), tmx_Gueckedou_monthly_2015)
rm(tmx_Gueckedou_monthly_2015, tmx_Gueckedou_monthly_2015a)
tmx_Gueckedou_monthly$measurement <- "tmx"
tmx_Gueckedou_monthly <- rename(tmx_Gueckedou_monthly, Value=tmx)
#Kankan
tmx_Kankan_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmx1=tmx.var[which(lon==9.25),which(lat==10.75),1:48],
                                          tmx2=tmx.var[which(lon==9.75),which(lat==10.25),1:48], 
                                          tmx3=tmx.var[which(lon==9.25),which(lat==10.25),1:48],
                                          tmx4=tmx.var[which(lon==8.75),which(lat==10.25),1:48],
                                          tmx5=tmx.var[which(lon==9.75),which(lat==9.75),1:48],
                                          tmx6=tmx.var[which(lon==9.25),which(lat==9.75),1:48]))
tmx_Kankan_monthly$tmx <- rowMeans(select(tmx_Kankan_monthly, tmx1, tmx2, tmx3, tmx4, tmx5, tmx6))
tmx_Kankan_monthly$Location <- 'Kankan'
tmx_Kankan_monthly$date <- ymd(paste(tmx_Kankan_monthly$Year, tmx_Kankan_monthly$Month, tmx_Kankan_monthly$day, sep="-"))
tmx_Kankan_monthly_2015 <- select(tmx_Kankan_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Kankan_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Kankan_monthly_2015a$Location <- 'Kankan'
tmx_Kankan_monthly_2015a$date <- ymd(paste(tmx_Kankan_monthly_2015a$Year, tmx_Kankan_monthly_2015a$Month, tmx_Kankan_monthly_2015a$day, sep="-"))
tmx_Kankan_monthly_2015a <- select(tmx_Kankan_monthly_2015a, date, Year, Month, day, Location)
tmx_Kankan_monthly_2015 <- full_join(tmx_Kankan_monthly_2015a, tmx_Kankan_monthly_2015)
tmx_Kankan_monthly <- rbind(select(tmx_Kankan_monthly,date, Year, Month, day, Location, tmx), tmx_Kankan_monthly_2015)
rm(tmx_Kankan_monthly_2015, tmx_Kankan_monthly_2015a)
tmx_Kankan_monthly$measurement <- "tmx"
tmx_Kankan_monthly <- rename(tmx_Kankan_monthly, Value=tmx)
#Kerouane
tmx_Kerouane_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            tmx1=tmx.var[which(lon==9.25),which(lat==9.75),1:48],
                                            tmx2=tmx.var[which(lon==8.75),which(lat==9.75),1:48], 
                                            tmx3=tmx.var[which(lon==9.75),which(lat==9.25),1:48],
                                            tmx4=tmx.var[which(lon==9.25),which(lat==9.25),1:48],
                                            tmx5=tmx.var[which(lon==8.75),which(lat==9.25),1:48],
                                            tmx6=tmx.var[which(lon==9.25),which(lat==8.75),1:48]))
tmx_Kerouane_monthly$tmx <- rowMeans(select(tmx_Kerouane_monthly, tmx1, tmx2, tmx3, tmx4, tmx5, tmx6))
tmx_Kerouane_monthly$Location <- 'Kerouane'
tmx_Kerouane_monthly$date <- ymd(paste(tmx_Kerouane_monthly$Year, tmx_Kerouane_monthly$Month, tmx_Kerouane_monthly$day, sep="-"))
tmx_Kerouane_monthly_2015 <- select(tmx_Kerouane_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Kerouane_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Kerouane_monthly_2015a$Location <- 'Kerouane'
tmx_Kerouane_monthly_2015a$date <- ymd(paste(tmx_Kerouane_monthly_2015a$Year, tmx_Kerouane_monthly_2015a$Month, tmx_Kerouane_monthly_2015a$day, sep="-"))
tmx_Kerouane_monthly_2015a <- select(tmx_Kerouane_monthly_2015a, date, Year, Month, day, Location)
tmx_Kerouane_monthly_2015 <- full_join(tmx_Kerouane_monthly_2015a, tmx_Kerouane_monthly_2015)
tmx_Kerouane_monthly <- rbind(select(tmx_Kerouane_monthly,date, Year, Month, day, Location, tmx), tmx_Kerouane_monthly_2015)
rm(tmx_Kerouane_monthly_2015, tmx_Kerouane_monthly_2015a)
tmx_Kerouane_monthly$measurement <- "tmx"
tmx_Kerouane_monthly <- rename(tmx_Kerouane_monthly, Value=tmx)
#Kindia
tmx_Kindia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmx1=tmx.var[which(lon==13.25),which(lat==10.25),1:48],
                                          tmx2=tmx.var[which(lon==12.75),which(lat==10.25),1:48], 
                                          tmx3=tmx.var[which(lon==12.25),which(lat==10.25),1:48],
                                          tmx4=tmx.var[which(lon==12.75),which(lat==9.75),1:48]))
tmx_Kindia_monthly$tmx <- rowMeans(select(tmx_Kindia_monthly, tmx1, tmx2, tmx3, tmx4))
tmx_Kindia_monthly$Location <- 'Kindia'
tmx_Kindia_monthly$date <- ymd(paste(tmx_Kindia_monthly$Year, tmx_Kindia_monthly$Month, tmx_Kindia_monthly$day, sep="-"))
tmx_Kindia_monthly_2015 <- select(tmx_Kindia_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Kindia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Kindia_monthly_2015a$Location <- 'Kindia'
tmx_Kindia_monthly_2015a$date <- ymd(paste(tmx_Kindia_monthly_2015a$Year, tmx_Kindia_monthly_2015a$Month, tmx_Kindia_monthly_2015a$day, sep="-"))
tmx_Kindia_monthly_2015a <- select(tmx_Kindia_monthly_2015a, date, Year, Month, day, Location)
tmx_Kindia_monthly_2015 <- full_join(tmx_Kindia_monthly_2015a, tmx_Kindia_monthly_2015)
tmx_Kindia_monthly <- rbind(select(tmx_Kindia_monthly,date, Year, Month, day, Location, tmx), tmx_Kindia_monthly_2015)
rm(tmx_Kindia_monthly_2015, tmx_Kindia_monthly_2015a)
tmx_Kindia_monthly$measurement <- "tmx"
tmx_Kindia_monthly <- rename(tmx_Kindia_monthly, Value=tmx)
#KISSIDOUGOU
tmx_Kissidougou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               tmx1=tmx.var[which(lon==10.25),which(lat==9.75),1:48],
                                               tmx2=tmx.var[which(lon==10.25),which(lat==9.25),1:48], 
                                               tmx3=tmx.var[which(lon==9.75),which(lat==9.25),1:48]))
tmx_Kissidougou_monthly$tmx <- rowMeans(select(tmx_Kissidougou_monthly, tmx1, tmx2, tmx3))
tmx_Kissidougou_monthly$Location <- 'Kissidougou'
tmx_Kissidougou_monthly$date <- ymd(paste(tmx_Kissidougou_monthly$Year, tmx_Kissidougou_monthly$Month, tmx_Kissidougou_monthly$day, sep="-"))
tmx_Kissidougou_monthly_2015 <- select(tmx_Kissidougou_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Kissidougou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Kissidougou_monthly_2015a$Location <- 'Kissidougou'
tmx_Kissidougou_monthly_2015a$date <- ymd(paste(tmx_Kissidougou_monthly_2015a$Year, tmx_Kissidougou_monthly_2015a$Month, tmx_Kissidougou_monthly_2015a$day, sep="-"))
tmx_Kissidougou_monthly_2015a <- select(tmx_Kissidougou_monthly_2015a, date, Year, Month, day, Location)
tmx_Kissidougou_monthly_2015 <- full_join(tmx_Kissidougou_monthly_2015a, tmx_Kissidougou_monthly_2015)
tmx_Kissidougou_monthly <- rbind(select(tmx_Kissidougou_monthly,date, Year, Month, day, Location, tmx), tmx_Kissidougou_monthly_2015)
rm(tmx_Kissidougou_monthly_2015, tmx_Kissidougou_monthly_2015a)
tmx_Kissidougou_monthly$measurement <- "tmx"
tmx_Kissidougou_monthly <- rename(tmx_Kissidougou_monthly, Value=tmx)
#Koubia
tmx_Koubia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          tmx=tmx.var[which(lon==10.25),which(lat==9.75),1:48]))
tmx_Koubia_monthly$Location <- 'Koubia'
tmx_Koubia_monthly$date <- ymd(paste(tmx_Koubia_monthly$Year, tmx_Koubia_monthly$Month, tmx_Koubia_monthly$day, sep="-"))
tmx_Koubia_monthly_2015 <- select(tmx_Koubia_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Koubia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Koubia_monthly_2015a$Location <- 'Koubia'
tmx_Koubia_monthly_2015a$date <- ymd(paste(tmx_Koubia_monthly_2015a$Year, tmx_Koubia_monthly_2015a$Month, tmx_Koubia_monthly_2015a$day, sep="-"))
tmx_Koubia_monthly_2015a <- select(tmx_Koubia_monthly_2015a, date, Year, Month, day, Location)
tmx_Koubia_monthly_2015 <- full_join(tmx_Koubia_monthly_2015a, tmx_Koubia_monthly_2015)
tmx_Koubia_monthly <- rbind(select(tmx_Koubia_monthly,date, Year, Month, day, Location, tmx), tmx_Koubia_monthly_2015)
rm(tmx_Koubia_monthly_2015, tmx_Koubia_monthly_2015a)
tmx_Koubia_monthly$measurement <- "tmx"
tmx_Koubia_monthly <- rename(tmx_Koubia_monthly, Value=tmx)
#Koundara
tmx_Koundara_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            tmx1=tmx.var[which(lon==13.25),which(lat==12.75),1:48],
                                            tmx2=tmx.var[which(lon==13.25),which(lat==12.25),1:48], 
                                            tmx3=tmx.var[which(lon==12.75),which(lat==12.25),1:48]))
tmx_Koundara_monthly$tmx <- rowMeans(select(tmx_Koundara_monthly, tmx1, tmx2, tmx3))
tmx_Koundara_monthly$Location <- 'Koundara'
tmx_Koundara_monthly$date <- ymd(paste(tmx_Koundara_monthly$Year, tmx_Koundara_monthly$Month, tmx_Koundara_monthly$day, sep="-"))
tmx_Koundara_monthly_2015 <- select(tmx_Koundara_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Koundara_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Koundara_monthly_2015a$Location <- 'Koundara'
tmx_Koundara_monthly_2015a$date <- ymd(paste(tmx_Koundara_monthly_2015a$Year, tmx_Koundara_monthly_2015a$Month, tmx_Koundara_monthly_2015a$day, sep="-"))
tmx_Koundara_monthly_2015a <- select(tmx_Koundara_monthly_2015a, date, Year, Month, day, Location)
tmx_Koundara_monthly_2015 <- full_join(tmx_Koundara_monthly_2015a, tmx_Koundara_monthly_2015)
tmx_Koundara_monthly <- rbind(select(tmx_Koundara_monthly,date, Year, Month, day, Location, tmx), tmx_Koundara_monthly_2015)
rm(tmx_Koundara_monthly_2015, tmx_Koundara_monthly_2015a)
tmx_Koundara_monthly$measurement <- "tmx"
tmx_Koundara_monthly <- rename(tmx_Koundara_monthly, Value=tmx)
#Kouroussa
tmx_Kouroussa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             tmx1=tmx.var[which(lon==10.25),which(lat==11.25),1:48],
                                             tmx2=tmx.var[which(lon==10.75),which(lat==10.75),1:48], 
                                             tmx3=tmx.var[which(lon==10.25),which(lat==10.75),1:48],
                                             tmx4=tmx.var[which(lon==9.75),which(lat==10.75),1:48],
                                             tmx5=tmx.var[which(lon==10.25),which(lat==10.25),1:48],
                                             tmx6=tmx.var[which(lon==9.75),which(lat==10.25),1:48],
                                             tmx7=tmx.var[which(lon==10.25),which(lat==9.75),1:48],
                                             tmx8=tmx.var[which(lon==9.75),which(lat==9.75),1:48]))
tmx_Kouroussa_monthly$tmx <- rowMeans(select(tmx_Kouroussa_monthly, tmx1, tmx2, tmx3, tmx4, tmx5, tmx6, tmx7, tmx8))
tmx_Kouroussa_monthly$Location <- 'Kouroussa'
tmx_Kouroussa_monthly$date <- ymd(paste(tmx_Kouroussa_monthly$Year, tmx_Kouroussa_monthly$Month, tmx_Kouroussa_monthly$day, sep="-"))
tmx_Kouroussa_monthly_2015 <- select(tmx_Kouroussa_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Kouroussa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Kouroussa_monthly_2015a$Location <- 'Kouroussa'
tmx_Kouroussa_monthly_2015a$date <- ymd(paste(tmx_Kouroussa_monthly_2015a$Year, tmx_Kouroussa_monthly_2015a$Month, tmx_Kouroussa_monthly_2015a$day, sep="-"))
tmx_Kouroussa_monthly_2015a <- select(tmx_Kouroussa_monthly_2015a, date, Year, Month, day, Location)
tmx_Kouroussa_monthly_2015 <- full_join(tmx_Kouroussa_monthly_2015a, tmx_Kouroussa_monthly_2015)
tmx_Kouroussa_monthly <- rbind(select(tmx_Kouroussa_monthly,date, Year, Month, day, Location, tmx), tmx_Kouroussa_monthly_2015)
rm(tmx_Kouroussa_monthly_2015, tmx_Kouroussa_monthly_2015a)
tmx_Kouroussa_monthly$measurement <- "tmx"
tmx_Kouroussa_monthly <- rename(tmx_Kouroussa_monthly, Value=tmx)
#Labe
tmx_Labe_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmx1=tmx.var[which(lon==12.25),which(lat==11.75),1:48],
                                        tmx2=tmx.var[which(lon==12.25),which(lat==11.25),1:48]))
tmx_Labe_monthly$tmx <- rowMeans(select(tmx_Labe_monthly, tmx1, tmx2))
tmx_Labe_monthly$Location <- 'Labe'
tmx_Labe_monthly$date <- ymd(paste(tmx_Labe_monthly$Year, tmx_Labe_monthly$Month, tmx_Labe_monthly$day, sep="-"))
tmx_Labe_monthly_2015 <- select(tmx_Labe_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Labe_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Labe_monthly_2015a$Location <- 'Labe'
tmx_Labe_monthly_2015a$date <- ymd(paste(tmx_Labe_monthly_2015a$Year, tmx_Labe_monthly_2015a$Month, tmx_Labe_monthly_2015a$day, sep="-"))
tmx_Labe_monthly_2015a <- select(tmx_Labe_monthly_2015a, date, Year, Month, day, Location)
tmx_Labe_monthly_2015 <- full_join(tmx_Labe_monthly_2015a, tmx_Labe_monthly_2015)
tmx_Labe_monthly <- rbind(select(tmx_Labe_monthly,date, Year, Month, day, Location, tmx), tmx_Labe_monthly_2015)
rm(tmx_Labe_monthly_2015, tmx_Labe_monthly_2015a)
tmx_Labe_monthly$measurement <- "tmx"
tmx_Labe_monthly <- rename(tmx_Labe_monthly, Value=tmx)
#Lelouma
tmx_Lelouma_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmx1=tmx.var[which(lon==12.75),which(lat==11.75),1:48],
                                           tmx2=tmx.var[which(lon==12.75),which(lat==11.25),1:48]))
tmx_Lelouma_monthly$tmx <- rowMeans(select(tmx_Lelouma_monthly, tmx1, tmx2))
tmx_Lelouma_monthly$Location <- 'Lelouma'
tmx_Lelouma_monthly$date <- ymd(paste(tmx_Lelouma_monthly$Year, tmx_Lelouma_monthly$Month, tmx_Lelouma_monthly$day, sep="-"))
tmx_Lelouma_monthly_2015 <- select(tmx_Lelouma_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Lelouma_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Lelouma_monthly_2015a$Location <- 'Lelouma'
tmx_Lelouma_monthly_2015a$date <- ymd(paste(tmx_Lelouma_monthly_2015a$Year, tmx_Lelouma_monthly_2015a$Month, tmx_Lelouma_monthly_2015a$day, sep="-"))
tmx_Lelouma_monthly_2015a <- select(tmx_Lelouma_monthly_2015a, date, Year, Month, day, Location)
tmx_Lelouma_monthly_2015 <- full_join(tmx_Lelouma_monthly_2015a, tmx_Lelouma_monthly_2015)
tmx_Lelouma_monthly <- rbind(select(tmx_Lelouma_monthly,date, Year, Month, day, Location, tmx), tmx_Lelouma_monthly_2015)
rm(tmx_Lelouma_monthly_2015, tmx_Lelouma_monthly_2015a)
tmx_Lelouma_monthly$measurement <- "tmx"
tmx_Lelouma_monthly <- rename(tmx_Lelouma_monthly, Value=tmx)
#Lola
tmx_Lola_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmx1=tmx.var[which(lon==8.25),which(lat==8.25),1:48],
                                        tmx2=tmx.var[which(lon==8.25),which(lat==7.75),1:48]))
tmx_Lola_monthly$tmx <- rowMeans(select(tmx_Lola_monthly, tmx1, tmx2))
tmx_Lola_monthly$Location <- 'Lola'
tmx_Lola_monthly$date <- ymd(paste(tmx_Lola_monthly$Year, tmx_Lola_monthly$Month, tmx_Lola_monthly$day, sep="-"))
tmx_Lola_monthly_2015 <- select(tmx_Lola_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Lola_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Lola_monthly_2015a$Location <- 'Lola'
tmx_Lola_monthly_2015a$date <- ymd(paste(tmx_Lola_monthly_2015a$Year, tmx_Lola_monthly_2015a$Month, tmx_Lola_monthly_2015a$day, sep="-"))
tmx_Lola_monthly_2015a <- select(tmx_Lola_monthly_2015a, date, Year, Month, day, Location)
tmx_Lola_monthly_2015 <- full_join(tmx_Lola_monthly_2015a, tmx_Lola_monthly_2015)
tmx_Lola_monthly <- rbind(select(tmx_Lola_monthly,date, Year, Month, day, Location, tmx), tmx_Lola_monthly_2015)
rm(tmx_Lola_monthly_2015, tmx_Lola_monthly_2015a)
tmx_Lola_monthly$measurement <- "tmx"
tmx_Lola_monthly <- rename(tmx_Lola_monthly, Value=tmx)
#Macenta
tmx_Macenta_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmx1=tmx.var[which(lon==9.75),which(lat==8.75),1:48],
                                           tmx2=tmx.var[which(lon==9.25),which(lat==8.75),1:48],
                                           tmx3=tmx.var[which(lon==9.25),which(lat==8.25),1:48]))
tmx_Macenta_monthly$tmx <- rowMeans(select(tmx_Macenta_monthly, tmx1, tmx2, tmx3))
tmx_Macenta_monthly$Location <- 'Macenta'
tmx_Macenta_monthly$date <- ymd(paste(tmx_Macenta_monthly$Year, tmx_Macenta_monthly$Month, tmx_Macenta_monthly$day, sep="-"))
tmx_Macenta_monthly_2015 <- select(tmx_Macenta_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Macenta_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Macenta_monthly_2015a$Location <- 'Macenta'
tmx_Macenta_monthly_2015a$date <- ymd(paste(tmx_Macenta_monthly_2015a$Year, tmx_Macenta_monthly_2015a$Month, tmx_Macenta_monthly_2015a$day, sep="-"))
tmx_Macenta_monthly_2015a <- select(tmx_Macenta_monthly_2015a, date, Year, Month, day, Location)
tmx_Macenta_monthly_2015 <- full_join(tmx_Macenta_monthly_2015a, tmx_Macenta_monthly_2015)
tmx_Macenta_monthly <- rbind(select(tmx_Macenta_monthly,date, Year, Month, day, Location, tmx), tmx_Macenta_monthly_2015)
rm(tmx_Macenta_monthly_2015, tmx_Macenta_monthly_2015a)
tmx_Macenta_monthly$measurement <- "tmx"
tmx_Macenta_monthly <- rename(tmx_Macenta_monthly, Value=tmx)
#Mali
tmx_Mali_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmx1=tmx.var[which(lon==12.75),which(lat==12.25),1:48],
                                        tmx2=tmx.var[which(lon==12.25),which(lat==12.25),1:48], 
                                        tmx3=tmx.var[which(lon==11.75),which(lat==12.25),1:48],
                                        tmx4=tmx.var[which(lon==12.75),which(lat==11.75),1:48],
                                        tmx5=tmx.var[which(lon==12.25),which(lat==11.75),1:48]))
tmx_Mali_monthly$tmx <- rowMeans(select(tmx_Mali_monthly, tmx1, tmx2, tmx3, tmx4, tmx5))
tmx_Mali_monthly$Location <- 'Mali'
tmx_Mali_monthly$date <- ymd(paste(tmx_Mali_monthly$Year, tmx_Mali_monthly$Month, tmx_Mali_monthly$day, sep="-"))
tmx_Mali_monthly_2015 <- select(tmx_Mali_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Mali_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Mali_monthly_2015a$Location <- 'Mali'
tmx_Mali_monthly_2015a$date <- ymd(paste(tmx_Mali_monthly_2015a$Year, tmx_Mali_monthly_2015a$Month, tmx_Mali_monthly_2015a$day, sep="-"))
tmx_Mali_monthly_2015a <- select(tmx_Mali_monthly_2015a, date, Year, Month, day, Location)
tmx_Mali_monthly_2015 <- full_join(tmx_Mali_monthly_2015a, tmx_Mali_monthly_2015)
tmx_Mali_monthly <- rbind(select(tmx_Mali_monthly,date, Year, Month, day, Location, tmx), tmx_Mali_monthly_2015)
rm(tmx_Mali_monthly_2015, tmx_Mali_monthly_2015a)
tmx_Mali_monthly$measurement <- "tmx"
tmx_Mali_monthly <- rename(tmx_Mali_monthly, Value=tmx)
#Mamou
tmx_Mamou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         tmx1=tmx.var[which(lon==11.75),which(lat==10.75),1:48],
                                         tmx2=tmx.var[which(lon==12.25),which(lat==10.25),1:48],
                                         tmx3=tmx.var[which(lon==11.75),which(lat==10.25),1:48]))
tmx_Mamou_monthly$tmx <- rowMeans(select(tmx_Mamou_monthly, tmx1, tmx2, tmx3))
tmx_Mamou_monthly$Location <- 'Mamou'
tmx_Mamou_monthly$date <- ymd(paste(tmx_Mamou_monthly$Year, tmx_Mamou_monthly$Month, tmx_Mamou_monthly$day, sep="-"))
tmx_Mamou_monthly_2015 <- select(tmx_Mamou_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Mamou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Mamou_monthly_2015a$Location <- 'Mamou'
tmx_Mamou_monthly_2015a$date <- ymd(paste(tmx_Mamou_monthly_2015a$Year, tmx_Mamou_monthly_2015a$Month, tmx_Mamou_monthly_2015a$day, sep="-"))
tmx_Mamou_monthly_2015a <- select(tmx_Mamou_monthly_2015a, date, Year, Month, day, Location)
tmx_Mamou_monthly_2015 <- full_join(tmx_Mamou_monthly_2015a, tmx_Mamou_monthly_2015)
tmx_Mamou_monthly <- rbind(select(tmx_Mamou_monthly,date, Year, Month, day, Location, tmx), tmx_Mamou_monthly_2015)
rm(tmx_Mamou_monthly_2015, tmx_Mamou_monthly_2015a)
tmx_Mamou_monthly$measurement <- "tmx"
tmx_Mamou_monthly <- rename(tmx_Mamou_monthly, Value=tmx)
#Nzerekore
tmx_Nzerekore_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             tmx1=tmx.var[which(lon==8.75),which(lat==8.25),1:48],
                                             tmx2=tmx.var[which(lon==8.75),which(lat==7.75),1:48]))
tmx_Nzerekore_monthly$tmx <- rowMeans(select(tmx_Nzerekore_monthly, tmx1, tmx2))
tmx_Nzerekore_monthly$Location <- 'Nzerekore'
tmx_Nzerekore_monthly$date <- ymd(paste(tmx_Nzerekore_monthly$Year, tmx_Nzerekore_monthly$Month, tmx_Nzerekore_monthly$day, sep="-"))
tmx_Nzerekore_monthly_2015 <- select(tmx_Nzerekore_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Nzerekore_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Nzerekore_monthly_2015a$Location <- 'Nzerekore'
tmx_Nzerekore_monthly_2015a$date <- ymd(paste(tmx_Nzerekore_monthly_2015a$Year, tmx_Nzerekore_monthly_2015a$Month, tmx_Nzerekore_monthly_2015a$day, sep="-"))
tmx_Nzerekore_monthly_2015a <- select(tmx_Nzerekore_monthly_2015a, date, Year, Month, day, Location)
tmx_Nzerekore_monthly_2015 <- full_join(tmx_Nzerekore_monthly_2015a, tmx_Nzerekore_monthly_2015)
tmx_Nzerekore_monthly <- rbind(select(tmx_Nzerekore_monthly,date, Year, Month, day, Location, tmx), tmx_Nzerekore_monthly_2015)
rm(tmx_Nzerekore_monthly_2015, tmx_Nzerekore_monthly_2015a)
tmx_Nzerekore_monthly$measurement <- "tmx"
tmx_Nzerekore_monthly <- rename(tmx_Nzerekore_monthly, Value=tmx)
#Pita
tmx_Pita_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        tmx1=tmx.var[which(lon==12.75),which(lat==11.25),1:48],
                                        tmx2=tmx.var[which(lon==12.25),which(lat==11.25),1:48],
                                        tmx3=tmx.var[which(lon==12.75),which(lat==10.75),1:48]))
tmx_Pita_monthly$tmx <- rowMeans(select(tmx_Pita_monthly, tmx1, tmx2, tmx3))
tmx_Pita_monthly$Location <- 'Pita'
tmx_Pita_monthly$date <- ymd(paste(tmx_Pita_monthly$Year, tmx_Pita_monthly$Month, tmx_Pita_monthly$day, sep="-"))
tmx_Pita_monthly_2015 <- select(tmx_Pita_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Pita_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Pita_monthly_2015a$Location <- 'Pita'
tmx_Pita_monthly_2015a$date <- ymd(paste(tmx_Pita_monthly_2015a$Year, tmx_Pita_monthly_2015a$Month, tmx_Pita_monthly_2015a$day, sep="-"))
tmx_Pita_monthly_2015a <- select(tmx_Pita_monthly_2015a, date, Year, Month, day, Location)
tmx_Pita_monthly_2015 <- full_join(tmx_Pita_monthly_2015a, tmx_Pita_monthly_2015)
tmx_Pita_monthly <- rbind(select(tmx_Pita_monthly,date, Year, Month, day, Location, tmx), tmx_Pita_monthly_2015)
rm(tmx_Pita_monthly_2015, tmx_Pita_monthly_2015a)
tmx_Pita_monthly$measurement <- "tmx"
tmx_Pita_monthly <- rename(tmx_Pita_monthly, Value=tmx)
#Siguiri
tmx_Siguiri_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           tmx1=tmx.var[which(lon==9.25),which(lat==12.25),1:48],
                                           tmx2=tmx.var[which(lon==10.25),which(lat==11.75),1:48], 
                                           tmx3=tmx.var[which(lon==9.75),which(lat==11.75),1:48],
                                           tmx4=tmx.var[which(lon==9.25),which(lat==11.75),1:48],
                                           tmx5=tmx.var[which(lon==9.75),which(lat==11.25),1:48],
                                           tmx6=tmx.var[which(lon==9.25),which(lat==11.25),1:48]))
tmx_Siguiri_monthly$tmx <- rowMeans(select(tmx_Siguiri_monthly, tmx1, tmx2, tmx3, tmx4, tmx5, tmx6))
tmx_Siguiri_monthly$Location <- 'Siguiri'
tmx_Siguiri_monthly$date <- ymd(paste(tmx_Siguiri_monthly$Year, tmx_Siguiri_monthly$Month, tmx_Siguiri_monthly$day, sep="-"))
tmx_Siguiri_monthly_2015 <- select(tmx_Siguiri_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Siguiri_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Siguiri_monthly_2015a$Location <- 'Siguiri'
tmx_Siguiri_monthly_2015a$date <- ymd(paste(tmx_Siguiri_monthly_2015a$Year, tmx_Siguiri_monthly_2015a$Month, tmx_Siguiri_monthly_2015a$day, sep="-"))
tmx_Siguiri_monthly_2015a <- select(tmx_Siguiri_monthly_2015a, date, Year, Month, day, Location)
tmx_Siguiri_monthly_2015 <- full_join(tmx_Siguiri_monthly_2015a, tmx_Siguiri_monthly_2015)
tmx_Siguiri_monthly <- rbind(select(tmx_Siguiri_monthly,date, Year, Month, day, Location, tmx), tmx_Siguiri_monthly_2015)
rm(tmx_Siguiri_monthly_2015, tmx_Siguiri_monthly_2015a)
tmx_Siguiri_monthly$measurement <- "tmx"
tmx_Siguiri_monthly <- rename(tmx_Siguiri_monthly, Value=tmx)
#Telimele
tmx_Telimele_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            tmx1=tmx.var[which(lon==13.75),which(lat==11.25),1:48],
                                            tmx2=tmx.var[which(lon==13.25),which(lat==11.25),1:48], 
                                            tmx3=tmx.var[which(lon==13.75),which(lat==10.75),1:48],
                                            tmx4=tmx.var[which(lon==13.25),which(lat==10.75),1:48]))
tmx_Telimele_monthly$tmx <- rowMeans(select(tmx_Telimele_monthly, tmx1, tmx2, tmx3, tmx4))
tmx_Telimele_monthly$Location <- 'Telimele'
tmx_Telimele_monthly$date <- ymd(paste(tmx_Telimele_monthly$Year, tmx_Telimele_monthly$Month, tmx_Telimele_monthly$day, sep="-"))
tmx_Telimele_monthly_2015 <- select(tmx_Telimele_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Telimele_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Telimele_monthly_2015a$Location <- 'Telimele'
tmx_Telimele_monthly_2015a$date <- ymd(paste(tmx_Telimele_monthly_2015a$Year, tmx_Telimele_monthly_2015a$Month, tmx_Telimele_monthly_2015a$day, sep="-"))
tmx_Telimele_monthly_2015a <- select(tmx_Telimele_monthly_2015a, date, Year, Month, day, Location)
tmx_Telimele_monthly_2015 <- full_join(tmx_Telimele_monthly_2015a, tmx_Telimele_monthly_2015)
tmx_Telimele_monthly <- rbind(select(tmx_Telimele_monthly,date, Year, Month, day, Location, tmx), tmx_Telimele_monthly_2015)
rm(tmx_Telimele_monthly_2015, tmx_Telimele_monthly_2015a)
tmx_Telimele_monthly$measurement <- "tmx"
tmx_Telimele_monthly <- rename(tmx_Telimele_monthly, Value=tmx)
#Tougue
tmx_Tougue_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4),day=1, 
                                          tmx1=tmx.var[which(lon==11.25),which(lat==11.75),1:48],
                                          tmx2=tmx.var[which(lon==11.75),which(lat==11.25),1:48]))
tmx_Tougue_monthly$tmx <- rowMeans(select(tmx_Tougue_monthly, tmx1, tmx2))
tmx_Tougue_monthly$Location <- 'Tougue'
tmx_Tougue_monthly$date <- ymd(paste(tmx_Tougue_monthly$Year, tmx_Tougue_monthly$Month, tmx_Tougue_monthly$day, sep="-"))
tmx_Tougue_monthly_2015 <- select(tmx_Tougue_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_Tougue_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_Tougue_monthly_2015a$Location <- 'Tougue'
tmx_Tougue_monthly_2015a$date <- ymd(paste(tmx_Tougue_monthly_2015a$Year, tmx_Tougue_monthly_2015a$Month, tmx_Tougue_monthly_2015a$day, sep="-"))
tmx_Tougue_monthly_2015a <- select(tmx_Tougue_monthly_2015a, date, Year, Month, day, Location)
tmx_Tougue_monthly_2015 <- full_join(tmx_Tougue_monthly_2015a, tmx_Tougue_monthly_2015)
tmx_Tougue_monthly <- rbind(select(tmx_Tougue_monthly,date, Year, Month, day, Location, tmx), tmx_Tougue_monthly_2015)
rm(tmx_Tougue_monthly_2015, tmx_Tougue_monthly_2015a)
tmx_Tougue_monthly$measurement <- "tmx"
tmx_Tougue_monthly <- rename(tmx_Tougue_monthly, Value=tmx)
#yamou
tmx_yamou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4),day=1, 
                                         tmx1=tmx.var[which(lon==9.25),which(lat==7.75),1:48],
                                         tmx2=tmx.var[which(lon==9.25),which(lat==7.25),1:48],
                                         tmx3=tmx.var[which(lon==8.75),which(lat==7.25),1:48]))
tmx_yamou_monthly$Location <- 'yamou'
tmx_yamou_monthly$tmx <- rowMeans(select(tmx_yamou_monthly, tmx1, tmx2, tmx3))
tmx_yamou_monthly$date <- ymd(paste(tmx_yamou_monthly$Year, tmx_yamou_monthly$Month, tmx_yamou_monthly$day, sep="-"))
tmx_yamou_monthly_2015 <- select(tmx_yamou_monthly, Location, Year, Month, tmx) %>% group_by( Month) %>% summarize(tmx=mean(tmx))
tmx_yamou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
tmx_yamou_monthly_2015a$Location <- 'yamou'
tmx_yamou_monthly_2015a$date <- ymd(paste(tmx_yamou_monthly_2015a$Year, tmx_yamou_monthly_2015a$Month, tmx_yamou_monthly_2015a$day, sep="-"))
tmx_yamou_monthly_2015a <- select(tmx_yamou_monthly_2015a, date, Year, Month, day, Location)
tmx_yamou_monthly_2015 <- full_join(tmx_yamou_monthly_2015a, tmx_yamou_monthly_2015)
tmx_yamou_monthly <- rbind(select(tmx_yamou_monthly,date, Year, Month, day, Location, tmx), tmx_yamou_monthly_2015)
rm(tmx_yamou_monthly_2015, tmx_yamou_monthly_2015a)
tmx_yamou_monthly$measurement <- "tmx"
tmx_yamou_monthly <- rename(tmx_yamou_monthly, Value=tmx)

#Merging in long format
tmx_Guinea_monthly_district <- rbind(tmx_Beyla_monthly, tmx_Boke_monthly, tmx_Boffa_monthly,
                                     tmx_Conakry_monthly, tmx_Coyah_monthly, tmx_Dabola_monthly, tmx_Dalaba_monthly,
                                     tmx_Dinguiray_monthly, tmx_Dubreka_monthly, tmx_Faranah_monthly,
                                     tmx_Forecariah_monthly, tmx_Fria_monthly, tmx_Gaoual_monthly,
                                     tmx_Gueckedou_monthly, tmx_Kankan_monthly, tmx_Kerouane_monthly,
                                     tmx_Kindia_monthly, tmx_Kissidougou_monthly, tmx_Koubia_monthly,
                                     tmx_Koundara_monthly, tmx_Kouroussa_monthly, tmx_Labe_monthly,
                                     tmx_Lelouma_monthly, tmx_Lola_monthly, tmx_Macenta_monthly,
                                     tmx_Mali_monthly, tmx_Mamou_monthly, tmx_Nzerekore_monthly,
                                     tmx_Pita_monthly, tmx_Siguiri_monthly, tmx_Telimele_monthly,
                                     tmx_Tougue_monthly, tmx_yamou_monthly)

Guinea_monthly_district <- rbind(Vap_Guinea_monthly_district, wet_Guinea_monthly_district, pet_Guinea_monthly_district,
                                dtr_Guinea_monthly_district, pre_Guinea_monthly_district, tmn_Guinea_monthly_district,
                                tmp_Guinea_monthly_district, tmx_Guinea_monthly_district)
write.csv(Guinea_monthly_district, file='Guinea_monthly_district.csv')

Guinea_monthly_district <- read.csv('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/Guinea_monthly_district.csv')

# use interpolation to get the weekly climate parameters
func_spline <- function(climatefactor,steps=12/52){
    sp<- splinefun(x=seq(1,length(climatefactor),1), y=climatefactor, method="fmm",  ties = mean)
    out <- sp(seq(1,length(climatefactor),steps))
    return(out)
    
}
guinea_monthly_vap <- filter(Guinea_monthly_district, measurement=='Vap' & Year>='2013') %>% select(-X)
guinea_monthly_wet <- filter(Guinea_monthly_district, measurement=='wet' & Year>='2013') %>% select(-X)
guinea_monthly_pet <- filter(Guinea_monthly_district, measurement=='pet' & Year>='2013') %>% select(-X)
guinea_monthly_pre <- filter(Guinea_monthly_district, measurement=='pre' & Year>='2013') %>% select(-X)
guinea_monthly_tmn <- filter(Guinea_monthly_district, measurement=='tmn' & Year>='2013') %>% select(-X)
guinea_monthly_tmp <- filter(Guinea_monthly_district, measurement=='tmp' & Year>='2013') %>% select(-X)
guinea_monthly_tmx <- filter(Guinea_monthly_district, measurement=='tmx' & Year>='2013') %>% select(-X)
guinea_monthly_dtr <- filter(Guinea_monthly_district, measurement=='dtr' & Year>='2013') %>% select(-X)

guinea_weekly_vap <- tapply(guinea_monthly_vap$Value,guinea_monthly_vap$Location,func_spline,simplify = T)
guinea_weekly_wet <- tapply(guinea_monthly_wet$Value,guinea_monthly_wet$Location,func_spline,simplify = T)
guinea_weekly_pet <- tapply(guinea_monthly_pet$Value,guinea_monthly_pet$Location,func_spline,simplify = T)
guinea_weekly_pre <- tapply(guinea_monthly_pre$Value,guinea_monthly_pre$Location,func_spline,simplify = T)
guinea_weekly_tmn <- tapply(guinea_monthly_tmn$Value,guinea_monthly_tmn$Location,func_spline,simplify = T)
guinea_weekly_tmp <- tapply(guinea_monthly_tmp$Value,guinea_monthly_tmp$Location,func_spline,simplify = T)
guinea_weekly_tmx <- tapply(guinea_monthly_tmx$Value,guinea_monthly_tmx$Location,func_spline,simplify = T)
guinea_weekly_dtr <- tapply(guinea_monthly_dtr$Value,guinea_monthly_dtr$Location,func_spline,simplify = T)

guinea_weekly_climate <- as.data.frame(rep(names(guinea_weekly_tmn), 152))
colnames(guinea_weekly_climate) <- "Location"
guinea_weekly_climate <- arrange(guinea_weekly_climate, Location)
guinea_weekly_climate$count_week <- 1:152
guinea_weekly_climate <- cbind(guinea_weekly_climate, vap=unlist(guinea_weekly_vap), wet=unlist(guinea_weekly_wet), pet=unlist(guinea_weekly_pet),
                               pre=unlist(guinea_weekly_pre), tmn=unlist(guinea_weekly_tmn), tmp=unlist(guinea_weekly_tmp), tmx=unlist(guinea_weekly_tmx),
                               dtr=unlist(guinea_weekly_dtr))
row.names(guinea_weekly_climate) <- 1:length(guinea_weekly_climate$Location)
guinea_weekly_climate$wet <- ifelse(guinea_weekly_climate$wet<1,0, guinea_weekly_climate$wet)
guinea_weekly_climate$pre <- ifelse(guinea_weekly_climate$pre<1,0, guinea_weekly_climate$pre)
guinea_weekly_climate$Location <- toupper(guinea_weekly_climate$Location)


# Obtain the weekly cases count for Guinea in order and add the count_week
guinea_weekly_cases <- read.csv('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Cases/guinea_cases_long.csv')
guinea_weekly_cases <- guinea_weekly_cases %>% filter(Source=='Patient database') %>% select(-X, -Source, -Indicator) %>% spread(Case_definition, Cases) %>% 
    arrange(Location, Epi_Week) %>% mutate(count_week=rep(53:171,33)) %>% filter(count_week<=152) %>% mutate(Total_cases=Confirmed+Probable) %>%
    select(-Confirmed, -Probable)


# now full joint the case counts data and the climate data
guinea_weekly_climate$count_week <- as.numeric(as.character(guinea_weekly_climate$count_week))
guinea_weekly_cases_climate <- left_join(guinea_weekly_climate,guinea_weekly_cases,by=c("Location",'count_week'))
guinea_weekly_cases_climate <- replace_na(guinea_weekly_cases_climate, list(Total_cases=0))
guinea_weekly_cases_climate$tmx <- as.numeric(as.character(guinea_weekly_cases_climate$tmx))
guinea_weekly_cases_climate$tmp <- as.numeric(as.character(guinea_weekly_cases_climate$tmp))
guinea_weekly_cases_climate$tmn <- as.numeric(as.character(guinea_weekly_cases_climate$tmn))
guinea_weekly_cases_climate$pre <- as.numeric(as.character(guinea_weekly_cases_climate$pre))
guinea_weekly_cases_climate$vap <- as.numeric(as.character(guinea_weekly_cases_climate$vap))
guinea_weekly_cases_climate$wet <- as.numeric(as.character(guinea_weekly_cases_climate$wet))
guinea_weekly_cases_climate$pet <- as.numeric(as.character(guinea_weekly_cases_climate$pet))
guinea_weekly_cases_climate$dtr <- as.numeric(as.character(guinea_weekly_cases_climate$dtr))

write.csv(guinea_weekly_cases_climate, 'guinea_weekly_cases_climate.csv')
