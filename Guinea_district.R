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
YOMOU 7.75, 9.25; 7.25, (9.25, 8.75)'

#Vap - vapor pressure
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
Vap_Beyla_monthly$Vap <- rowMeans(select(Vap_Beyla_monthly, Vap1, Vap2, Vap3))
Vap_Beyla_monthly$date <- ymd(paste(Vap_Beyla_monthly$Year, Vap_Beyla_monthly$Month, Vap_Beyla_monthly$day, sep="-"))
Vap_Beyla_monthly_2015 <- select(Vap_Beyla_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Beyla_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Beyla_monthly_2015a$Location <- 'Beyla'
Vap_Beyla_monthly_2015a$date <- ymd(paste(Vap_Beyla_monthly_2015a$Year, Vap_Beyla_monthly_2015a$Month, Vap_Beyla_monthly_2015a$day, sep="-"))
Vap_Beyla_monthly_2015a <- select(Vap_Beyla_monthly_2015a, date, Year, Month, day, Location)
Vap_Beyla_monthly_2015 <- full_join(Vap_Beyla_monthly_2015a, Vap_Beyla_monthly_2015)
Vap_Beyla_monthly <- rbind(select(Vap_Beyla_monthly,date, Year, Month, day, Location, Vap), Vap_Beyla_monthly_2015)
rm(Vap_Beyla_monthly_2015, Vap_Beyla_monthly_2015a)
#Boffa
Vap_Boffa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                         Vap1=vap.var[which(lon==14.25),which(lat==10.75),1:48],
                                         Vap2=vap.var[which(lon==13.75),which(lat==10.75),1:48], 
                                         Vap3=vap.var[which(lon==14.25),which(lat==10.25),1:48],
                                         Vap4=vap.var[which(lon==13.75),which(lat==10.25),1:48]))
Vap_Boffa_monthly$Vap <- rowMeans(select(Vap_Boffa_monthly, Vap1, Vap2, Vap3, Vap4))
Vap_Boffa_monthly$Location <- 'Boffa'
Vap_Boffa_monthly$Vap <- rowMeans(select(Vap_Boffa_monthly, Vap1, Vap2, Vap3))
Vap_Boffa_monthly$date <- ymd(paste(Vap_Boffa_monthly$Year, Vap_Boffa_monthly$Month, Vap_Boffa_monthly$day, sep="-"))
Vap_Boffa_monthly_2015 <- select(Vap_Boffa_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Boffa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Boffa_monthly_2015a$Location <- 'Boffa'
Vap_Boffa_monthly_2015a$date <- ymd(paste(Vap_Boffa_monthly_2015a$Year, Vap_Boffa_monthly_2015a$Month, Vap_Boffa_monthly_2015a$day, sep="-"))
Vap_Boffa_monthly_2015a <- select(Vap_Boffa_monthly_2015a, date, Year, Month, day, Location)
Vap_Boffa_monthly_2015 <- full_join(Vap_Boffa_monthly_2015a, Vap_Boffa_monthly_2015)
Vap_Boffa_monthly <- rbind(select(Vap_Boffa_monthly,date, Year, Month, day, Location, Vap), Vap_Boffa_monthly_2015)
rm(Vap_Boffa_monthly_2015, Vap_Boffa_monthly_2015a)
#Boke
Vap_Boke_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), day=1,
                                        Vap1=vap.var[which(lon==14.25),which(lat==11.75),1:48],
                                         Vap2=vap.var[which(lon==14.75),which(lat==11.25),1:48], Vap3=vap.var[which(lon==14.25),which(lat==11.25),1:48],
                                         Vap4=vap.var[which(lon==13.75),which(lat==11.25),1:48]))
Vap_Boke_monthly$Vap <- rowMeans(select(Vap_Boke_monthly, Vap1, Vap2, Vap3, Vap4))
Vap_Boke_monthly$Location <- 'Boke'
Vap_Boke_monthly$Vap <- rowMeans(select(Vap_Boke_monthly, Vap1, Vap2, Vap3))
Vap_Boke_monthly$date <- ymd(paste(Vap_Boke_monthly$Year, Vap_Boke_monthly$Month, Vap_Boke_monthly$day, sep="-"))
Vap_Boke_monthly_2015 <- select(Vap_Boke_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Boke_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Boke_monthly_2015a$Location <- 'Boke'
Vap_Boke_monthly_2015a$date <- ymd(paste(Vap_Boke_monthly_2015a$Year, Vap_Boke_monthly_2015a$Month, Vap_Boke_monthly_2015a$day, sep="-"))
Vap_Boke_monthly_2015a <- select(Vap_Boke_monthly_2015a, date, Year, Month, day, Location)
Vap_Boke_monthly_2015 <- full_join(Vap_Boke_monthly_2015a, Vap_Boke_monthly_2015)
Vap_Boke_monthly <- rbind(select(Vap_Boke_monthly,date, Year, Month, day, Location, Vap), Vap_Boke_monthly_2015)
rm(Vap_Boke_monthly_2015, Vap_Boke_monthly_2015a)
#Coyah
Vap_Coyah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        Vap=vap.var[which(lon==9.75),which(lat==13.25),1:48]))
Vap_Coyah_monthly$Location <- 'Coyah'
Vap_Coyah_monthly$Vap <- rowMeans(select(Vap_Coyah_monthly, Vap1, Vap2, Vap3))
Vap_Coyah_monthly$date <- ymd(paste(Vap_Coyah_monthly$Year, Vap_Coyah_monthly$Month, Vap_Coyah_monthly$day, sep="-"))
Vap_Coyah_monthly_2015 <- select(Vap_Coyah_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Coyah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Coyah_monthly_2015a$Location <- 'Coyah'
Vap_Coyah_monthly_2015a$date <- ymd(paste(Vap_Coyah_monthly_2015a$Year, Vap_Coyah_monthly_2015a$Month, Vap_Coyah_monthly_2015a$day, sep="-"))
Vap_Coyah_monthly_2015a <- select(Vap_Coyah_monthly_2015a, date, Year, Month, day, Location)
Vap_Coyah_monthly_2015 <- full_join(Vap_Coyah_monthly_2015a, Vap_Coyah_monthly_2015)
Vap_Coyah_monthly <- rbind(select(Vap_Coyah_monthly,date, Year, Month, day, Location, Vap), Vap_Coyah_monthly_2015)
rm(Vap_Coyah_monthly_2015, Vap_Coyah_monthly_2015a)
#Dabola
Vap_Dabola_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        Vap1=vap.var[which(lon==11.25),which(lat==10.75),1:48],
                                        Vap2=vap.var[which(lon==10.75),which(lat==10.75),1:48]))
Vap_Dabola_monthly$Vap <- rowMeans(select(Vap_Dabola_monthly, Vap1, Vap2))
Vap_Dabola_monthly$Location <- 'Dabola'
Vap_Dabola_monthly$Vap <- rowMeans(select(Vap_Dabola_monthly, Vap1, Vap2, Vap3))
Vap_Dabola_monthly$date <- ymd(paste(Vap_Dabola_monthly$Year, Vap_Dabola_monthly$Month, Vap_Dabola_monthly$day, sep="-"))
Vap_Dabola_monthly_2015 <- select(Vap_Dabola_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Dabola_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Dabola_monthly_2015a$Location <- 'Dabola'
Vap_Dabola_monthly_2015a$date <- ymd(paste(Vap_Dabola_monthly_2015a$Year, Vap_Dabola_monthly_2015a$Month, Vap_Dabola_monthly_2015a$day, sep="-"))
Vap_Dabola_monthly_2015a <- select(Vap_Dabola_monthly_2015a, date, Year, Month, day, Location)
Vap_Dabola_monthly_2015 <- full_join(Vap_Dabola_monthly_2015a, Vap_Dabola_monthly_2015)
Vap_Dabola_monthly <- rbind(select(Vap_Dabola_monthly,date, Year, Month, day, Location, Vap), Vap_Dabola_monthly_2015)
rm(Vap_Dabola_monthly_2015, Vap_Dabola_monthly_2015a)
#Dalaba
Vap_Dalaba_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          Vap1=vap.var[which(lon==11.25),which(lat==12.25),1:48],
                                          Vap2=vap.var[which(lon==10.75),which(lat==12.25),1:48]))
Vap_Dalaba_monthly$Vap <- rowMeans(select(Vap_Dalaba_monthly, Vap1, Vap2))
Vap_Dalaba_monthly$Location <- 'Dalaba'
Vap_Dalaba_monthly$Vap <- rowMeans(select(Vap_Dalaba_monthly, Vap1, Vap2, Vap3))
Vap_Dalaba_monthly$date <- ymd(paste(Vap_Dalaba_monthly$Year, Vap_Dalaba_monthly$Month, Vap_Dalaba_monthly$day, sep="-"))
Vap_Dalaba_monthly_2015 <- select(Vap_Dalaba_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Dalaba_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Dalaba_monthly_2015a$Location <- 'Dalaba'
Vap_Dalaba_monthly_2015a$date <- ymd(paste(Vap_Dalaba_monthly_2015a$Year, Vap_Dalaba_monthly_2015a$Month, Vap_Dalaba_monthly_2015a$day, sep="-"))
Vap_Dalaba_monthly_2015a <- select(Vap_Dalaba_monthly_2015a, date, Year, Month, day, Location)
Vap_Dalaba_monthly_2015 <- full_join(Vap_Dalaba_monthly_2015a, Vap_Dalaba_monthly_2015)
Vap_Dalaba_monthly <- rbind(select(Vap_Dalaba_monthly,date, Year, Month, day, Location, Vap), Vap_Dalaba_monthly_2015)
rm(Vap_Dalaba_monthly_2015, Vap_Dalaba_monthly_2015a)
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
Vap_Dinguiray_monthly$Vap <- rowMeans(select(Vap_Dinguiray_monthly, Vap1, Vap2, Vap3))
Vap_Dinguiray_monthly$date <- ymd(paste(Vap_Dinguiray_monthly$Year, Vap_Dinguiray_monthly$Month, Vap_Dinguiray_monthly$day, sep="-"))
Vap_Dinguiray_monthly_2015 <- select(Vap_Dinguiray_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Dinguiray_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Dinguiray_monthly_2015a$Location <- 'Dinguiray'
Vap_Dinguiray_monthly_2015a$date <- ymd(paste(Vap_Dinguiray_monthly_2015a$Year, Vap_Dinguiray_monthly_2015a$Month, Vap_Dinguiray_monthly_2015a$day, sep="-"))
Vap_Dinguiray_monthly_2015a <- select(Vap_Dinguiray_monthly_2015a, date, Year, Month, day, Location)
Vap_Dinguiray_monthly_2015 <- full_join(Vap_Dinguiray_monthly_2015a, Vap_Dinguiray_monthly_2015)
Vap_Dinguiray_monthly <- rbind(select(Vap_Dinguiray_monthly,date, Year, Month, day, Location, Vap), Vap_Dinguiray_monthly_2015)
rm(Vap_Dinguiray_monthly_2015, Vap_Dinguiray_monthly_2015a)
#Dubreka
Vap_Dubreka_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), day=1,
                                             Vap1=vap.var[which(lon==13.75),which(lat==10.25),1:48],
                                             Vap2=vap.var[which(lon==13.25),which(lat==10.25),1:48]))
Vap_Dubreka_monthly$Vap <- rowMeans(select(Vap_Dubreka_monthly, Vap1, Vap2))
Vap_Dubreka_monthly$Location <- 'Dubreka'
Vap_Dubreka_monthly$Vap <- rowMeans(select(Vap_Dubreka_monthly, Vap1, Vap2, Vap3))
Vap_Dubreka_monthly$date <- ymd(paste(Vap_Dubreka_monthly$Year, Vap_Dubreka_monthly$Month, Vap_Dubreka_monthly$day, sep="-"))
Vap_Dubreka_monthly_2015 <- select(Vap_Dubreka_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Dubreka_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Dubreka_monthly_2015a$Location <- 'Dubreka'
Vap_Dubreka_monthly_2015a$date <- ymd(paste(Vap_Dubreka_monthly_2015a$Year, Vap_Dubreka_monthly_2015a$Month, Vap_Dubreka_monthly_2015a$day, sep="-"))
Vap_Dubreka_monthly_2015a <- select(Vap_Dubreka_monthly_2015a, date, Year, Month, day, Location)
Vap_Dubreka_monthly_2015 <- full_join(Vap_Dubreka_monthly_2015a, Vap_Dubreka_monthly_2015)
Vap_Dubreka_monthly <- rbind(select(Vap_Dubreka_monthly,date, Year, Month, day, Location, Vap), Vap_Dubreka_monthly_2015)
rm(Vap_Dubreka_monthly_2015, Vap_Dubreka_monthly_2015a)
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
Vap_Faranah_monthly$Vap <- rowMeans(select(Vap_Faranah_monthly, Vap1, Vap2, Vap3))
Vap_Faranah_monthly$date <- ymd(paste(Vap_Faranah_monthly$Year, Vap_Faranah_monthly$Month, Vap_Faranah_monthly$day, sep="-"))
Vap_Faranah_monthly_2015 <- select(Vap_Faranah_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Faranah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Faranah_monthly_2015a$Location <- 'Faranah'
Vap_Faranah_monthly_2015a$date <- ymd(paste(Vap_Faranah_monthly_2015a$Year, Vap_Faranah_monthly_2015a$Month, Vap_Faranah_monthly_2015a$day, sep="-"))
Vap_Faranah_monthly_2015a <- select(Vap_Faranah_monthly_2015a, date, Year, Month, day, Location)
Vap_Faranah_monthly_2015 <- full_join(Vap_Faranah_monthly_2015a, Vap_Faranah_monthly_2015)
Vap_Faranah_monthly <- rbind(select(Vap_Faranah_monthly,date, Year, Month, day, Location, Vap), Vap_Faranah_monthly_2015)
rm(Vap_Faranah_monthly_2015, Vap_Faranah_monthly_2015a)
#Forecariah
Vap_Forecariah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           Vap1=vap.var[which(lon==12.75),which(lat==9.75),1:48],
                                           Vap2=vap.var[which(lon==13.25),which(lat==9.25),1:48], 
                                           Vap3=vap.var[which(lon==12.75),which(lat==9.25),1:48]))
Vap_Forecariah_monthly$Vap <- rowMeans(select(Vap_Forecariah_monthly, Vap1, Vap2, Vap3))
Vap_Forecariah_monthly$Location <- 'Forecariah'
Vap_Forecariah_monthly$Vap <- rowMeans(select(Vap_Forecariah_monthly, Vap1, Vap2, Vap3))
Vap_Forecariah_monthly$date <- ymd(paste(Vap_Forecariah_monthly$Year, Vap_Forecariah_monthly$Month, Vap_Forecariah_monthly$day, sep="-"))
Vap_Forecariah_monthly_2015 <- select(Vap_Forecariah_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Forecariah_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Forecariah_monthly_2015a$Location <- 'Forecariah'
Vap_Forecariah_monthly_2015a$date <- ymd(paste(Vap_Forecariah_monthly_2015a$Year, Vap_Forecariah_monthly_2015a$Month, Vap_Forecariah_monthly_2015a$day, sep="-"))
Vap_Forecariah_monthly_2015a <- select(Vap_Forecariah_monthly_2015a, date, Year, Month, day, Location)
Vap_Forecariah_monthly_2015 <- full_join(Vap_Forecariah_monthly_2015a, Vap_Forecariah_monthly_2015)
Vap_Forecariah_monthly <- rbind(select(Vap_Forecariah_monthly,date, Year, Month, day, Location, Vap), Vap_Forecariah_monthly_2015)
rm(Vap_Forecariah_monthly_2015, Vap_Forecariah_monthly_2015a)
#Fria
Vap_Fria_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                              Month=rep(seq(1,12,1), 4), day=1,
                                              Vap1=vap.var[which(lon==13.75),which(lat==10.75),1:48],
                                              Vap2=vap.var[which(lon==13.75),which(lat==10.25),1:48]))
Vap_Fria_monthly$Vap <- rowMeans(select(Vap_Fria_monthly, Vap1, Vap2))
Vap_Fria_monthly$Location <- 'Fria'
Vap_Fria_monthly$Vap <- rowMeans(select(Vap_Fria_monthly, Vap1, Vap2, Vap3))
Vap_Fria_monthly$date <- ymd(paste(Vap_Fria_monthly$Year, Vap_Fria_monthly$Month, Vap_Fria_monthly$day, sep="-"))
Vap_Fria_monthly_2015 <- select(Vap_Fria_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Fria_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Fria_monthly_2015a$Location <- 'Fria'
Vap_Fria_monthly_2015a$date <- ymd(paste(Vap_Fria_monthly_2015a$Year, Vap_Fria_monthly_2015a$Month, Vap_Fria_monthly_2015a$day, sep="-"))
Vap_Fria_monthly_2015a <- select(Vap_Fria_monthly_2015a, date, Year, Month, day, Location)
Vap_Fria_monthly_2015 <- full_join(Vap_Fria_monthly_2015a, Vap_Fria_monthly_2015)
Vap_Fria_monthly <- rbind(select(Vap_Fria_monthly,date, Year, Month, day, Location, Vap), Vap_Fria_monthly_2015)
rm(Vap_Fria_monthly_2015, Vap_Fria_monthly_2015a)
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
Vap_Gaoual_monthly$Vap <- rowMeans(select(Vap_Gaoual_monthly, Vap1, Vap2, Vap3))
Vap_Gaoual_monthly$date <- ymd(paste(Vap_Gaoual_monthly$Year, Vap_Gaoual_monthly$Month, Vap_Gaoual_monthly$day, sep="-"))
Vap_Gaoual_monthly_2015 <- select(Vap_Gaoual_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Gaoual_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Gaoual_monthly_2015a$Location <- 'Gaoual'
Vap_Gaoual_monthly_2015a$date <- ymd(paste(Vap_Gaoual_monthly_2015a$Year, Vap_Gaoual_monthly_2015a$Month, Vap_Gaoual_monthly_2015a$day, sep="-"))
Vap_Gaoual_monthly_2015a <- select(Vap_Gaoual_monthly_2015a, date, Year, Month, day, Location)
Vap_Gaoual_monthly_2015 <- full_join(Vap_Gaoual_monthly_2015a, Vap_Gaoual_monthly_2015)
Vap_Gaoual_monthly <- rbind(select(Vap_Gaoual_monthly,date, Year, Month, day, Location, Vap), Vap_Gaoual_monthly_2015)
rm(Vap_Gaoual_monthly_2015, Vap_Gaoual_monthly_2015a)
#Gueckedou
Vap_Gueckedou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          Vap=vap.var[which(lon==10.25),which(lat==8.75),1:48]))
Vap_Gueckedou_monthly$Location <- 'Gueckedou'
Vap_Gueckedou_monthly$Vap <- rowMeans(select(Vap_Gueckedou_monthly, Vap1, Vap2, Vap3))
Vap_Gueckedou_monthly$date <- ymd(paste(Vap_Gueckedou_monthly$Year, Vap_Gueckedou_monthly$Month, Vap_Gueckedou_monthly$day, sep="-"))
Vap_Gueckedou_monthly_2015 <- select(Vap_Gueckedou_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Gueckedou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Gueckedou_monthly_2015a$Location <- 'Gueckedou'
Vap_Gueckedou_monthly_2015a$date <- ymd(paste(Vap_Gueckedou_monthly_2015a$Year, Vap_Gueckedou_monthly_2015a$Month, Vap_Gueckedou_monthly_2015a$day, sep="-"))
Vap_Gueckedou_monthly_2015a <- select(Vap_Gueckedou_monthly_2015a, date, Year, Month, day, Location)
Vap_Gueckedou_monthly_2015 <- full_join(Vap_Gueckedou_monthly_2015a, Vap_Gueckedou_monthly_2015)
Vap_Gueckedou_monthly <- rbind(select(Vap_Gueckedou_monthly,date, Year, Month, day, Location, Vap), Vap_Gueckedou_monthly_2015)
rm(Vap_Gueckedou_monthly_2015, Vap_Gueckedou_monthly_2015a)
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
Vap_Kankan_monthly$Vap <- rowMeans(select(Vap_Kankan_monthly, Vap1, Vap2, Vap3))
Vap_Kankan_monthly$date <- ymd(paste(Vap_Kankan_monthly$Year, Vap_Kankan_monthly$Month, Vap_Kankan_monthly$day, sep="-"))
Vap_Kankan_monthly_2015 <- select(Vap_Kankan_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Kankan_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Kankan_monthly_2015a$Location <- 'Kankan'
Vap_Kankan_monthly_2015a$date <- ymd(paste(Vap_Kankan_monthly_2015a$Year, Vap_Kankan_monthly_2015a$Month, Vap_Kankan_monthly_2015a$day, sep="-"))
Vap_Kankan_monthly_2015a <- select(Vap_Kankan_monthly_2015a, date, Year, Month, day, Location)
Vap_Kankan_monthly_2015 <- full_join(Vap_Kankan_monthly_2015a, Vap_Kankan_monthly_2015)
Vap_Kankan_monthly <- rbind(select(Vap_Kankan_monthly,date, Year, Month, day, Location, Vap), Vap_Kankan_monthly_2015)
rm(Vap_Kankan_monthly_2015, Vap_Kankan_monthly_2015a)
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
Vap_Kerouane_monthly$Vap <- rowMeans(select(Vap_Kerouane_monthly, Vap1, Vap2, Vap3))
Vap_Kerouane_monthly$date <- ymd(paste(Vap_Kerouane_monthly$Year, Vap_Kerouane_monthly$Month, Vap_Kerouane_monthly$day, sep="-"))
Vap_Kerouane_monthly_2015 <- select(Vap_Kerouane_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Kerouane_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Kerouane_monthly_2015a$Location <- 'Kerouane'
Vap_Kerouane_monthly_2015a$date <- ymd(paste(Vap_Kerouane_monthly_2015a$Year, Vap_Kerouane_monthly_2015a$Month, Vap_Kerouane_monthly_2015a$day, sep="-"))
Vap_Kerouane_monthly_2015a <- select(Vap_Kerouane_monthly_2015a, date, Year, Month, day, Location)
Vap_Kerouane_monthly_2015 <- full_join(Vap_Kerouane_monthly_2015a, Vap_Kerouane_monthly_2015)
Vap_Kerouane_monthly <- rbind(select(Vap_Kerouane_monthly,date, Year, Month, day, Location, Vap), Vap_Kerouane_monthly_2015)
rm(Vap_Kerouane_monthly_2015, Vap_Kerouane_monthly_2015a)
#Kindia
Vap_Kindia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            Vap1=vap.var[which(lon==13.25),which(lat==10.25),1:48],
                                            Vap2=vap.var[which(lon==12.75),which(lat==10.25),1:48], 
                                            Vap3=vap.var[which(lon==12.25),which(lat==10.25),1:48],
                                            Vap4=vap.var[which(lon==12.75),which(lat==9.75),1:48]))
Vap_Kindia_monthly$Vap <- rowMeans(select(Vap_Kindia_monthly, Vap1, Vap2, Vap3, Vap4))
Vap_Kindia_monthly$Location <- 'Kindia'
Vap_Kindia_monthly$Vap <- rowMeans(select(Vap_Kindia_monthly, Vap1, Vap2, Vap3))
Vap_Kindia_monthly$date <- ymd(paste(Vap_Kindia_monthly$Year, Vap_Kindia_monthly$Month, Vap_Kindia_monthly$day, sep="-"))
Vap_Kindia_monthly_2015 <- select(Vap_Kindia_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Kindia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Kindia_monthly_2015a$Location <- 'Kindia'
Vap_Kindia_monthly_2015a$date <- ymd(paste(Vap_Kindia_monthly_2015a$Year, Vap_Kindia_monthly_2015a$Month, Vap_Kindia_monthly_2015a$day, sep="-"))
Vap_Kindia_monthly_2015a <- select(Vap_Kindia_monthly_2015a, date, Year, Month, day, Location)
Vap_Kindia_monthly_2015 <- full_join(Vap_Kindia_monthly_2015a, Vap_Kindia_monthly_2015)
Vap_Kindia_monthly <- rbind(select(Vap_Kindia_monthly,date, Year, Month, day, Location, Vap), Vap_Kindia_monthly_2015)
rm(Vap_Kindia_monthly_2015, Vap_Kindia_monthly_2015a)
#KISSIDOUGOU
Vap_Kissidougou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), day=1,
                                          Vap1=vap.var[which(lon==10.25),which(lat==9.75),1:48],
                                          Vap2=vap.var[which(lon==10.25),which(lat==9.25),1:48], 
                                          Vap3=vap.var[which(lon==9.75),which(lat==9.25),1:48]))
Vap_Kissidougou_monthly$Vap <- rowMeans(select(Vap_Kissidougou_monthly, Vap1, Vap2, Vap3))
Vap_Kissidougou_monthly$Location <- 'Kissidougou'
Vap_Kissidougou_monthly$Vap <- rowMeans(select(Vap_Kissidougou_monthly, Vap1, Vap2, Vap3))
Vap_Kissidougou_monthly$date <- ymd(paste(Vap_Kissidougou_monthly$Year, Vap_Kissidougou_monthly$Month, Vap_Kissidougou_monthly$day, sep="-"))
Vap_Kissidougou_monthly_2015 <- select(Vap_Kissidougou_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Kissidougou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Kissidougou_monthly_2015a$Location <- 'Kissidougou'
Vap_Kissidougou_monthly_2015a$date <- ymd(paste(Vap_Kissidougou_monthly_2015a$Year, Vap_Kissidougou_monthly_2015a$Month, Vap_Kissidougou_monthly_2015a$day, sep="-"))
Vap_Kissidougou_monthly_2015a <- select(Vap_Kissidougou_monthly_2015a, date, Year, Month, day, Location)
Vap_Kissidougou_monthly_2015 <- full_join(Vap_Kissidougou_monthly_2015a, Vap_Kissidougou_monthly_2015)
Vap_Kissidougou_monthly <- rbind(select(Vap_Kissidougou_monthly,date, Year, Month, day, Location, Vap), Vap_Kissidougou_monthly_2015)
rm(Vap_Kissidougou_monthly_2015, Vap_Kissidougou_monthly_2015a)
#Koubia
Vap_Koubia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               Vap=vap.var[which(lon==10.25),which(lat==9.75),1:48]))
Vap_Koubia_monthly$Location <- 'Koubia'
Vap_Koubia_monthly$Vap <- rowMeans(select(Vap_Koubia_monthly, Vap1, Vap2, Vap3))
Vap_Koubia_monthly$date <- ymd(paste(Vap_Koubia_monthly$Year, Vap_Koubia_monthly$Month, Vap_Koubia_monthly$day, sep="-"))
Vap_Koubia_monthly_2015 <- select(Vap_Koubia_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Koubia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Koubia_monthly_2015a$Location <- 'Koubia'
Vap_Koubia_monthly_2015a$date <- ymd(paste(Vap_Koubia_monthly_2015a$Year, Vap_Koubia_monthly_2015a$Month, Vap_Koubia_monthly_2015a$day, sep="-"))
Vap_Koubia_monthly_2015a <- select(Vap_Koubia_monthly_2015a, date, Year, Month, day, Location)
Vap_Koubia_monthly_2015 <- full_join(Vap_Koubia_monthly_2015a, Vap_Koubia_monthly_2015)
Vap_Koubia_monthly <- rbind(select(Vap_Koubia_monthly,date, Year, Month, day, Location, Vap), Vap_Koubia_monthly_2015)
rm(Vap_Koubia_monthly_2015, Vap_Koubia_monthly_2015a)
#Koundara
Vap_Koundara_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), day=1,
                                               Vap1=vap.var[which(lon==13.25),which(lat==12.75),1:48],
                                               Vap2=vap.var[which(lon==13.25),which(lat==12.25),1:48], 
                                               Vap3=vap.var[which(lon==12.75),which(lat==12.25),1:48]))
Vap_Koundara_monthly$Vap <- rowMeans(select(Vap_Koundara_monthly, Vap1, Vap2, Vap3))
Vap_Koundara_monthly$Location <- 'Koundara'
Vap_Koundara_monthly$Vap <- rowMeans(select(Vap_Koundara_monthly, Vap1, Vap2, Vap3))
Vap_Koundara_monthly$date <- ymd(paste(Vap_Koundara_monthly$Year, Vap_Koundara_monthly$Month, Vap_Koundara_monthly$day, sep="-"))
Vap_Koundara_monthly_2015 <- select(Vap_Koundara_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Koundara_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Koundara_monthly_2015a$Location <- 'Koundara'
Vap_Koundara_monthly_2015a$date <- ymd(paste(Vap_Koundara_monthly_2015a$Year, Vap_Koundara_monthly_2015a$Month, Vap_Koundara_monthly_2015a$day, sep="-"))
Vap_Koundara_monthly_2015a <- select(Vap_Koundara_monthly_2015a, date, Year, Month, day, Location)
Vap_Koundara_monthly_2015 <- full_join(Vap_Koundara_monthly_2015a, Vap_Koundara_monthly_2015)
Vap_Koundara_monthly <- rbind(select(Vap_Koundara_monthly,date, Year, Month, day, Location, Vap), Vap_Koundara_monthly_2015)
rm(Vap_Koundara_monthly_2015, Vap_Koundara_monthly_2015a)
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
Vap_Kouroussa_monthly$Vap <- rowMeans(select(Vap_Kouroussa_monthly, Vap1, Vap2, Vap3))
Vap_Kouroussa_monthly$date <- ymd(paste(Vap_Kouroussa_monthly$Year, Vap_Kouroussa_monthly$Month, Vap_Kouroussa_monthly$day, sep="-"))
Vap_Kouroussa_monthly_2015 <- select(Vap_Kouroussa_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Kouroussa_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Kouroussa_monthly_2015a$Location <- 'Kouroussa'
Vap_Kouroussa_monthly_2015a$date <- ymd(paste(Vap_Kouroussa_monthly_2015a$Year, Vap_Kouroussa_monthly_2015a$Month, Vap_Kouroussa_monthly_2015a$day, sep="-"))
Vap_Kouroussa_monthly_2015a <- select(Vap_Kouroussa_monthly_2015a, date, Year, Month, day, Location)
Vap_Kouroussa_monthly_2015 <- full_join(Vap_Kouroussa_monthly_2015a, Vap_Kouroussa_monthly_2015)
Vap_Kouroussa_monthly <- rbind(select(Vap_Kouroussa_monthly,date, Year, Month, day, Location, Vap), Vap_Kouroussa_monthly_2015)
rm(Vap_Kouroussa_monthly_2015, Vap_Kouroussa_monthly_2015a)
#Labe
Vap_Labe_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), day=1,
                                            Vap1=vap.var[which(lon==12.25),which(lat==11.75),1:48],
                                            Vap2=vap.var[which(lon==12.25),which(lat==11.25),1:48]))
Vap_Labe_monthly$Vap <- rowMeans(select(Vap_Labe_monthly, Vap1, Vap2))
Vap_Labe_monthly$Location <- 'Labe'
Vap_Labe_monthly$Vap <- rowMeans(select(Vap_Labe_monthly, Vap1, Vap2, Vap3))
Vap_Labe_monthly$date <- ymd(paste(Vap_Labe_monthly$Year, Vap_Labe_monthly$Month, Vap_Labe_monthly$day, sep="-"))
Vap_Labe_monthly_2015 <- select(Vap_Labe_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Labe_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Labe_monthly_2015a$Location <- 'Labe'
Vap_Labe_monthly_2015a$date <- ymd(paste(Vap_Labe_monthly_2015a$Year, Vap_Labe_monthly_2015a$Month, Vap_Labe_monthly_2015a$day, sep="-"))
Vap_Labe_monthly_2015a <- select(Vap_Labe_monthly_2015a, date, Year, Month, day, Location)
Vap_Labe_monthly_2015 <- full_join(Vap_Labe_monthly_2015a, Vap_Labe_monthly_2015)
Vap_Labe_monthly <- rbind(select(Vap_Labe_monthly,date, Year, Month, day, Location, Vap), Vap_Labe_monthly_2015)
rm(Vap_Labe_monthly_2015, Vap_Labe_monthly_2015a)
#Lelouma
Vap_Lelouma_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        Vap1=vap.var[which(lon==12.75),which(lat==11.75),1:48],
                                        Vap2=vap.var[which(lon==12.75),which(lat==11.25),1:48]))
Vap_Lelouma_monthly$Vap <- rowMeans(select(Vap_Lelouma_monthly, Vap1, Vap2))
Vap_Lelouma_monthly$Location <- 'Lelouma'
Vap_Lelouma_monthly$Vap <- rowMeans(select(Vap_Lelouma_monthly, Vap1, Vap2, Vap3))
Vap_Lelouma_monthly$date <- ymd(paste(Vap_Lelouma_monthly$Year, Vap_Lelouma_monthly$Month, Vap_Lelouma_monthly$day, sep="-"))
Vap_Lelouma_monthly_2015 <- select(Vap_Lelouma_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Lelouma_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Lelouma_monthly_2015a$Location <- 'Lelouma'
Vap_Lelouma_monthly_2015a$date <- ymd(paste(Vap_Lelouma_monthly_2015a$Year, Vap_Lelouma_monthly_2015a$Month, Vap_Lelouma_monthly_2015a$day, sep="-"))
Vap_Lelouma_monthly_2015a <- select(Vap_Lelouma_monthly_2015a, date, Year, Month, day, Location)
Vap_Lelouma_monthly_2015 <- full_join(Vap_Lelouma_monthly_2015a, Vap_Lelouma_monthly_2015)
Vap_Lelouma_monthly <- rbind(select(Vap_Lelouma_monthly,date, Year, Month, day, Location, Vap), Vap_Lelouma_monthly_2015)
rm(Vap_Lelouma_monthly_2015, Vap_Lelouma_monthly_2015a)
#Lola
Vap_Lola_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           Vap1=vap.var[which(lon==8.25),which(lat==8.25),1:48],
                                           Vap2=vap.var[which(lon==8.25),which(lat==7.75),1:48]))
Vap_Lola_monthly$Vap <- rowMeans(select(Vap_Lola_monthly, Vap1, Vap2))
Vap_Lola_monthly$Location <- 'Lola'
Vap_Lola_monthly$Vap <- rowMeans(select(Vap_Lola_monthly, Vap1, Vap2, Vap3))
Vap_Lola_monthly$date <- ymd(paste(Vap_Lola_monthly$Year, Vap_Lola_monthly$Month, Vap_Lola_monthly$day, sep="-"))
Vap_Lola_monthly_2015 <- select(Vap_Lola_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Lola_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Lola_monthly_2015a$Location <- 'Lola'
Vap_Lola_monthly_2015a$date <- ymd(paste(Vap_Lola_monthly_2015a$Year, Vap_Lola_monthly_2015a$Month, Vap_Lola_monthly_2015a$day, sep="-"))
Vap_Lola_monthly_2015a <- select(Vap_Lola_monthly_2015a, date, Year, Month, day, Location)
Vap_Lola_monthly_2015 <- full_join(Vap_Lola_monthly_2015a, Vap_Lola_monthly_2015)
Vap_Lola_monthly <- rbind(select(Vap_Lola_monthly,date, Year, Month, day, Location, Vap), Vap_Lola_monthly_2015)
rm(Vap_Lola_monthly_2015, Vap_Lola_monthly_2015a)
#Macenta
Vap_Macenta_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        Vap1=vap.var[which(lon==9.75),which(lat==8.75),1:48],
                                        Vap2=vap.var[which(lon==9.25),which(lat==8.75),1:48],
                                        Vap3=vap.var[which(lon==9.25),which(lat==8.25),1:48]))
Vap_Macenta_monthly$Vap <- rowMeans(select(Vap_Macenta_monthly, Vap1, Vap2, Vap3))
Vap_Macenta_monthly$Location <- 'Macenta'
Vap_Macenta_monthly$Vap <- rowMeans(select(Vap_Macenta_monthly, Vap1, Vap2, Vap3))
Vap_Macenta_monthly$date <- ymd(paste(Vap_Macenta_monthly$Year, Vap_Macenta_monthly$Month, Vap_Macenta_monthly$day, sep="-"))
Vap_Macenta_monthly_2015 <- select(Vap_Macenta_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Macenta_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Macenta_monthly_2015a$Location <- 'Macenta'
Vap_Macenta_monthly_2015a$date <- ymd(paste(Vap_Macenta_monthly_2015a$Year, Vap_Macenta_monthly_2015a$Month, Vap_Macenta_monthly_2015a$day, sep="-"))
Vap_Macenta_monthly_2015a <- select(Vap_Macenta_monthly_2015a, date, Year, Month, day, Location)
Vap_Macenta_monthly_2015 <- full_join(Vap_Macenta_monthly_2015a, Vap_Macenta_monthly_2015)
Vap_Macenta_monthly <- rbind(select(Vap_Macenta_monthly,date, Year, Month, day, Location, Vap), Vap_Macenta_monthly_2015)
rm(Vap_Macenta_monthly_2015, Vap_Macenta_monthly_2015a)
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
Vap_Mali_monthly$Vap <- rowMeans(select(Vap_Mali_monthly, Vap1, Vap2, Vap3))
Vap_Mali_monthly$date <- ymd(paste(Vap_Mali_monthly$Year, Vap_Mali_monthly$Month, Vap_Mali_monthly$day, sep="-"))
Vap_Mali_monthly_2015 <- select(Vap_Mali_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Mali_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Mali_monthly_2015a$Location <- 'Mali'
Vap_Mali_monthly_2015a$date <- ymd(paste(Vap_Mali_monthly_2015a$Year, Vap_Mali_monthly_2015a$Month, Vap_Mali_monthly_2015a$day, sep="-"))
Vap_Mali_monthly_2015a <- select(Vap_Mali_monthly_2015a, date, Year, Month, day, Location)
Vap_Mali_monthly_2015 <- full_join(Vap_Mali_monthly_2015a, Vap_Mali_monthly_2015)
Vap_Mali_monthly <- rbind(select(Vap_Mali_monthly,date, Year, Month, day, Location, Vap), Vap_Mali_monthly_2015)
rm(Vap_Mali_monthly_2015, Vap_Mali_monthly_2015a)
#Mamou
Vap_Mamou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           Vap1=vap.var[which(lon==11.75),which(lat==10.75),1:48],
                                           Vap2=vap.var[which(lon==12.25),which(lat==10.25),1:48],
                                           Vap3=vap.var[which(lon==11.75),which(lat==10.25),1:48]))
Vap_Mamou_monthly$Vap <- rowMeans(select(Vap_Mamou_monthly, Vap1, Vap2, Vap3))
Vap_Mamou_monthly$Location <- 'Mamou'
Vap_Mamou_monthly$Vap <- rowMeans(select(Vap_Mamou_monthly, Vap1, Vap2, Vap3))
Vap_Mamou_monthly$date <- ymd(paste(Vap_Mamou_monthly$Year, Vap_Mamou_monthly$Month, Vap_Mamou_monthly$day, sep="-"))
Vap_Mamou_monthly_2015 <- select(Vap_Mamou_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Mamou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Mamou_monthly_2015a$Location <- 'Mamou'
Vap_Mamou_monthly_2015a$date <- ymd(paste(Vap_Mamou_monthly_2015a$Year, Vap_Mamou_monthly_2015a$Month, Vap_Mamou_monthly_2015a$day, sep="-"))
Vap_Mamou_monthly_2015a <- select(Vap_Mamou_monthly_2015a, date, Year, Month, day, Location)
Vap_Mamou_monthly_2015 <- full_join(Vap_Mamou_monthly_2015a, Vap_Mamou_monthly_2015)
Vap_Mamou_monthly <- rbind(select(Vap_Mamou_monthly,date, Year, Month, day, Location, Vap), Vap_Mamou_monthly_2015)
rm(Vap_Mamou_monthly_2015, Vap_Mamou_monthly_2015a)
#Nzerekore
Vap_Nzerekore_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), day=1,
                                        Vap1=vap.var[which(lon==8.75),which(lat==8.25),1:48],
                                        Vap2=vap.var[which(lon==8.75),which(lat==7.75),1:48]))
Vap_Nzerekore_monthly$Vap <- rowMeans(select(Vap_Nzerekore_monthly, Vap1, Vap2))
Vap_Nzerekore_monthly$Location <- 'Nzerekore'
Vap_Nzerekore_monthly$Vap <- rowMeans(select(Vap_Nzerekore_monthly, Vap1, Vap2, Vap3))
Vap_Nzerekore_monthly$date <- ymd(paste(Vap_Nzerekore_monthly$Year, Vap_Nzerekore_monthly$Month, Vap_Nzerekore_monthly$day, sep="-"))
Vap_Nzerekore_monthly_2015 <- select(Vap_Nzerekore_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Nzerekore_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Nzerekore_monthly_2015a$Location <- 'Nzerekore'
Vap_Nzerekore_monthly_2015a$date <- ymd(paste(Vap_Nzerekore_monthly_2015a$Year, Vap_Nzerekore_monthly_2015a$Month, Vap_Nzerekore_monthly_2015a$day, sep="-"))
Vap_Nzerekore_monthly_2015a <- select(Vap_Nzerekore_monthly_2015a, date, Year, Month, day, Location)
Vap_Nzerekore_monthly_2015 <- full_join(Vap_Nzerekore_monthly_2015a, Vap_Nzerekore_monthly_2015)
Vap_Nzerekore_monthly <- rbind(select(Vap_Nzerekore_monthly,date, Year, Month, day, Location, Vap), Vap_Nzerekore_monthly_2015)
rm(Vap_Nzerekore_monthly_2015, Vap_Nzerekore_monthly_2015a)
#Pita
Vap_Pita_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           Vap1=vap.var[which(lon==12.75),which(lat==11.25),1:48],
                                           Vap2=vap.var[which(lon==12.25),which(lat==11.25),1:48],
                                           Vap3=vap.var[which(lon==12.75),which(lat==10.75),1:48]))
Vap_Pita_monthly$Vap <- rowMeans(select(Vap_Pita_monthly, Vap1, Vap2, Vap3))
Vap_Pita_monthly$Location <- 'Pita'
Vap_Pita_monthly$Vap <- rowMeans(select(Vap_Pita_monthly, Vap1, Vap2, Vap3))
Vap_Pita_monthly$date <- ymd(paste(Vap_Pita_monthly$Year, Vap_Pita_monthly$Month, Vap_Pita_monthly$day, sep="-"))
Vap_Pita_monthly_2015 <- select(Vap_Pita_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Pita_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Pita_monthly_2015a$Location <- 'Pita'
Vap_Pita_monthly_2015a$date <- ymd(paste(Vap_Pita_monthly_2015a$Year, Vap_Pita_monthly_2015a$Month, Vap_Pita_monthly_2015a$day, sep="-"))
Vap_Pita_monthly_2015a <- select(Vap_Pita_monthly_2015a, date, Year, Month, day, Location)
Vap_Pita_monthly_2015 <- full_join(Vap_Pita_monthly_2015a, Vap_Pita_monthly_2015)
Vap_Pita_monthly <- rbind(select(Vap_Pita_monthly,date, Year, Month, day, Location, Vap), Vap_Pita_monthly_2015)
rm(Vap_Pita_monthly_2015, Vap_Pita_monthly_2015a)
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
Vap_Siguiri_monthly$Vap <- rowMeans(select(Vap_Siguiri_monthly, Vap1, Vap2, Vap3))
Vap_Siguiri_monthly$date <- ymd(paste(Vap_Siguiri_monthly$Year, Vap_Siguiri_monthly$Month, Vap_Siguiri_monthly$day, sep="-"))
Vap_Siguiri_monthly_2015 <- select(Vap_Siguiri_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Siguiri_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Siguiri_monthly_2015a$Location <- 'Siguiri'
Vap_Siguiri_monthly_2015a$date <- ymd(paste(Vap_Siguiri_monthly_2015a$Year, Vap_Siguiri_monthly_2015a$Month, Vap_Siguiri_monthly_2015a$day, sep="-"))
Vap_Siguiri_monthly_2015a <- select(Vap_Siguiri_monthly_2015a, date, Year, Month, day, Location)
Vap_Siguiri_monthly_2015 <- full_join(Vap_Siguiri_monthly_2015a, Vap_Siguiri_monthly_2015)
Vap_Siguiri_monthly <- rbind(select(Vap_Siguiri_monthly,date, Year, Month, day, Location, Vap), Vap_Siguiri_monthly_2015)
rm(Vap_Siguiri_monthly_2015, Vap_Siguiri_monthly_2015a)
#Telimele
Vap_Telimele_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), day=1,
                                           Vap1=vap.var[which(lon==13.75),which(lat==11.25),1:48],
                                           Vap2=vap.var[which(lon==13.25),which(lat==11.25),1:48], 
                                           Vap3=vap.var[which(lon==13.75),which(lat==10.75),1:48],
                                           Vap4=vap.var[which(lon==13.25),which(lat==10.75),1:48]))
Vap_Telimele_monthly$Vap <- rowMeans(select(Vap_Telimele_monthly, Vap1, Vap2, Vap3, Vap4, Vap5, Vap6))
Vap_Telimele_monthly$Location <- 'Telimele'
Vap_Telimele_monthly$Vap <- rowMeans(select(Vap_Telimele_monthly, Vap1, Vap2, Vap3))
Vap_Telimele_monthly$date <- ymd(paste(Vap_Telimele_monthly$Year, Vap_Telimele_monthly$Month, Vap_Telimele_monthly$day, sep="-"))
Vap_Telimele_monthly_2015 <- select(Vap_Telimele_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Telimele_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Telimele_monthly_2015a$Location <- 'Telimele'
Vap_Telimele_monthly_2015a$date <- ymd(paste(Vap_Telimele_monthly_2015a$Year, Vap_Telimele_monthly_2015a$Month, Vap_Telimele_monthly_2015a$day, sep="-"))
Vap_Telimele_monthly_2015a <- select(Vap_Telimele_monthly_2015a, date, Year, Month, day, Location)
Vap_Telimele_monthly_2015 <- full_join(Vap_Telimele_monthly_2015a, Vap_Telimele_monthly_2015)
Vap_Telimele_monthly <- rbind(select(Vap_Telimele_monthly,date, Year, Month, day, Location, Vap), Vap_Telimele_monthly_2015)
rm(Vap_Telimele_monthly_2015, Vap_Telimele_monthly_2015a)
#Tougue
Vap_Tougue_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4),day=1, 
                                             Vap1=vap.var[which(lon==11.25),which(lat==11.75),1:48],
                                             Vap2=vap.var[which(lon==11.75),which(lat==11.25),1:48]))
Vap_Tougue_monthly$Vap <- rowMeans(select(Vap_Tougue_monthly, Vap1, Vap2))
Vap_Tougue_monthly$Location <- 'Tougue'
Vap_Tougue_monthly$Vap <- rowMeans(select(Vap_Tougue_monthly, Vap1, Vap2, Vap3))
Vap_Tougue_monthly$date <- ymd(paste(Vap_Tougue_monthly$Year, Vap_Tougue_monthly$Month, Vap_Tougue_monthly$day, sep="-"))
Vap_Tougue_monthly_2015 <- select(Vap_Tougue_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Tougue_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Tougue_monthly_2015a$Location <- 'Tougue'
Vap_Tougue_monthly_2015a$date <- ymd(paste(Vap_Tougue_monthly_2015a$Year, Vap_Tougue_monthly_2015a$Month, Vap_Tougue_monthly_2015a$day, sep="-"))
Vap_Tougue_monthly_2015a <- select(Vap_Tougue_monthly_2015a, date, Year, Month, day, Location)
Vap_Tougue_monthly_2015 <- full_join(Vap_Tougue_monthly_2015a, Vap_Tougue_monthly_2015)
Vap_Tougue_monthly <- rbind(select(Vap_Tougue_monthly,date, Year, Month, day, Location, Vap), Vap_Tougue_monthly_2015)
rm(Vap_Tougue_monthly_2015, Vap_Tougue_monthly_2015a)
#Yomou
Vap_Yomou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4),day=1, 
                                        Vap1=vap.var[which(lon==9.25),which(lat==7.75),1:48],
                                        Vap2=vap.var[which(lon==9.25),which(lat==7.25),1:48],
                                        Vap3=vap.var[which(lon==8.75),which(lat==7.25),1:48]))
Vap_Yomou_monthly$Location <- 'Yomou'
Vap_Yomou_monthly$Vap <- rowMeans(select(Vap_Yomou_monthly, Vap1, Vap2, Vap3))
Vap_Yomou_monthly$date <- ymd(paste(Vap_Yomou_monthly$Year, Vap_Yomou_monthly$Month, Vap_Yomou_monthly$day, sep="-"))
Vap_Yomou_monthly_2015 <- select(Vap_Yomou_monthly, Location, Year, Month, Vap) %>% group_by( Month) %>% summarize(Vap=mean(Vap))
Vap_Yomou_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), day=1))
Vap_Yomou_monthly_2015a$Location <- 'Yomou'
Vap_Yomou_monthly_2015a$date <- ymd(paste(Vap_Yomou_monthly_2015a$Year, Vap_Yomou_monthly_2015a$Month, Vap_Yomou_monthly_2015a$day, sep="-"))
Vap_Yomou_monthly_2015a <- select(Vap_Yomou_monthly_2015a, date, Year, Month, day, Location)
Vap_Yomou_monthly_2015 <- full_join(Vap_Yomou_monthly_2015a, Vap_Yomou_monthly_2015)
Vap_Yomou_monthly <- rbind(select(Vap_Yomou_monthly,date, Year, Month, day, Location, Vap), Vap_Yomou_monthly_2015)
rm(Vap_Yomou_monthly_2015, Vap_Yomou_monthly_2015a)
