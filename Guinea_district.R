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
                                         Month=rep(seq(1,12,1), 4), Vap1=vap.var[which(lon==8.75),which(lat==9.25),1:48],
                                         Vap2=vap.var[which(lon==8.25),which(lat==9.25),1:48], Vap3=vap.var[which(lon==7.75),which(lat==9.25),1:48],
                                         Vap4=vap.var[which(lon==8.75),which(lat==8.75),1:48], Vap5=vap.var[which(lon==8.25),which(lat==8.75),1:48],
                                         Vap6=vap.var[which(lon==7.75),which(lat==8.75),1:48], Vap7=vap.var[which(lon==8.75),which(lat==8.25),1:48],
                                         Vap8=vap.var[which(lon==8.25),which(lat==8.25),1:48]))
Vap_Beyla_monthly$Vap <- rowMeans(select(Vap_Beyla_monthly, Vap1, Vap2, Vap3, Vap4, Vap5, Vap6, Vap7, Vap8))
#Boffa
Vap_Boffa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), Vap1=vap.var[which(lon==14.25),which(lat==10.75),1:48],
                                         Vap2=vap.var[which(lon==13.75),which(lat==10.75),1:48], Vap3=vap.var[which(lon==14.25),which(lat==10.25),1:48],
                                         Vap4=vap.var[which(lon==13.75),which(lat==10.25),1:48]))
Vap_Boffa_monthly$Vap <- rowMeans(select(Vap_Boffa_monthly, Vap1, Vap2, Vap3, Vap4))
#Boke
Vap_Boke_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                         Month=rep(seq(1,12,1), 4), Vap1=vap.var[which(lon==14.25),which(lat==11.75),1:48],
                                         Vap2=vap.var[which(lon==14.75),which(lat==11.25),1:48], Vap3=vap.var[which(lon==14.25),which(lat==11.25),1:48],
                                         Vap4=vap.var[which(lon==13.75),which(lat==11.25),1:48]))
Vap_Boke_monthly$Vap <- rowMeans(select(Vap_Boke_monthly, Vap1, Vap2, Vap3, Vap4))
#Coyah
Vap_Coyah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), Vap=vap.var[which(lon==9.75),which(lat==13.25),1:48]))
#Dabola
Vap_Dabola_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), Vap1=vap.var[which(lon==11.25),which(lat==10.75),1:48],
                                        Vap2=vap.var[which(lon==10.75),which(lat==10.75),1:48]))
Vap_Dabola_monthly$Vap <- rowMeans(select(Vap_Dabola_monthly, Vap1, Vap2))
#Dalaba
Vap_Dalaba_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), Vap1=vap.var[which(lon==11.25),which(lat==12.25),1:48],
                                          Vap2=vap.var[which(lon==10.75),which(lat==12.25),1:48]))
Vap_Dalaba_monthly$Vap <- rowMeans(select(Vap_Dalaba_monthly, Vap1, Vap2))
#Dinguiray
Vap_Dinguiray_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), 
                                             Vap1=vap.var[which(lon==11.25),which(lat==11.75),1:48],
                                             Vap2=vap.var[which(lon==10.75),which(lat==11.75),1:48], 
                                             Vap3=vap.var[which(lon==10.25),which(lat==11.75),1:48],
                                             Vap4=vap.var[which(lon==11.25),which(lat==11.25),1:48],
                                             Vap5=vap.var[which(lon==10.75),which(lat==11.25),1:48],
                                             Vap6=vap.var[which(lon==10.25),which(lat==11.25),1:48]))
Vap_Dinguiray_monthly$Vap <- rowMeans(select(Vap_Dinguiray_monthly, Vap1, Vap2, Vap3, Vap4, Vap5, Vap6))
#Dubreka
Vap_Dubreka_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), 
                                             Vap1=vap.var[which(lon==13.75),which(lat==10.25),1:48],
                                             Vap2=vap.var[which(lon==13.25),which(lat==10.25),1:48]))
Vap_Dubreka_monthly$Vap <- rowMeans(select(Vap_Dubreka_monthly, Vap1, Vap2))
#Faranah
Vap_Faranah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), 
                                             Vap1=vap.var[which(lon==11.25),which(lat==10.25),1:48],
                                             Vap2=vap.var[which(lon==10.75),which(lat==10.25),1:48], 
                                             Vap3=vap.var[which(lon==10.25),which(lat==10.25),1:48],
                                             Vap4=vap.var[which(lon==10.75),which(lat==9.75),1:48],
                                             Vap5=vap.var[which(lon==10.75),which(lat==9.25),1:48]))
Vap_Faranah_monthly$Vap <- rowMeans(select(Vap_Faranah_monthly, Vap1, Vap2, Vap3, Vap4, Vap5))
#Forecariah
Vap_Forecariah_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), 
                                           Vap1=vap.var[which(lon==12.75),which(lat==9.75),1:48],
                                           Vap2=vap.var[which(lon==13.25),which(lat==9.25),1:48], 
                                           Vap3=vap.var[which(lon==12.75),which(lat==9.25),1:48]))
Vap_Forecariah_monthly$Vap <- rowMeans(select(Vap_Forecariah_monthly, Vap1, Vap2, Vap3))
#Fria
Vap_Fria_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                              Month=rep(seq(1,12,1), 4), 
                                              Vap1=vap.var[which(lon==13.75),which(lat==10.75),1:48],
                                              Vap2=vap.var[which(lon==13.75),which(lat==10.25),1:48]))
Vap_Fria_monthly$Vap <- rowMeans(select(Vap_Fria_monthly, Vap1, Vap2))
#Gaoual
Vap_Gaoual_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), 
                                             Vap1=vap.var[which(lon==13.75),which(lat==12.25),1:48],
                                             Vap2=vap.var[which(lon==13.25),which(lat==12.25),1:48], 
                                             Vap3=vap.var[which(lon==13.75),which(lat==11.75),1:48],
                                             Vap4=vap.var[which(lon==13.25),which(lat==11.75),1:48],
                                             Vap5=vap.var[which(lon==12.75),which(lat==11.75),1:48],
                                             Vap6=vap.var[which(lon==13.75),which(lat==11.25),1:48],
                                             Vap7=vap.var[which(lon==13.25),which(lat==11.25),1:48]))
Vap_Gaoual_monthly$Vap <- rowMeans(select(Vap_Gaoual_monthly, Vap1, Vap2, Vap3, Vap4, Vap5, Vap6, Vap7))
#Gueckedou
Vap_Gueckedou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), 
                                          Vap=vap.var[which(lon==10.25),which(lat==8.75),1:48]))
#Kankan
Vap_Kankan_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), 
                                          Vap1=vap.var[which(lon==9.25),which(lat==10.75),1:48],
                                          Vap2=vap.var[which(lon==9.75),which(lat==10.25),1:48], 
                                          Vap3=vap.var[which(lon==9.25),which(lat==10.25),1:48],
                                          Vap4=vap.var[which(lon==8.75),which(lat==10.25),1:48],
                                          Vap5=vap.var[which(lon==9.75),which(lat==9.75),1:48],
                                          Vap6=vap.var[which(lon==9.25),which(lat==9.75),1:48]))
Vap_Kankan_monthly$Vap <- rowMeans(select(Vap_Kankan_monthly, Vap1, Vap2, Vap3, Vap4, Vap5, Vap6))
#Kerouane
Vap_Kerouane_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), 
                                          Vap1=vap.var[which(lon==9.25),which(lat==9.75),1:48],
                                          Vap2=vap.var[which(lon==8.75),which(lat==9.75),1:48], 
                                          Vap3=vap.var[which(lon==9.75),which(lat==9.25),1:48],
                                          Vap4=vap.var[which(lon==9.25),which(lat==9.25),1:48],
                                          Vap5=vap.var[which(lon==8.75),which(lat==9.25),1:48],
                                          Vap6=vap.var[which(lon==9.25),which(lat==8.75),1:48]))
Vap_Kerouane_monthly$Vap <- rowMeans(select(Vap_Kerouane_monthly, Vap1, Vap2, Vap3, Vap4, Vap5, Vap6))
#Kindia
Vap_Kindia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), 
                                            Vap1=vap.var[which(lon==13.25),which(lat==10.25),1:48],
                                            Vap2=vap.var[which(lon==12.75),which(lat==10.25),1:48], 
                                            Vap3=vap.var[which(lon==12.25),which(lat==10.25),1:48],
                                            Vap4=vap.var[which(lon==12.75),which(lat==9.75),1:48]))
Vap_Kindia_monthly$Vap <- rowMeans(select(Vap_Kindia_monthly, Vap1, Vap2, Vap3, Vap4))
#KISSIDOUGOU
Vap_Kissidougou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                          Month=rep(seq(1,12,1), 4), 
                                          Vap1=vap.var[which(lon==10.25),which(lat==9.75),1:48],
                                          Vap2=vap.var[which(lon==10.25),which(lat==9.25),1:48], 
                                          Vap3=vap.var[which(lon==9.75),which(lat==9.25),1:48]))
Vap_Kissidougou_monthly$Vap <- rowMeans(select(Vap_Kissidougou_monthly, Vap1, Vap2, Vap3))
#Koubia
Vap_Koubia_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), 
                                               Vap=vap.var[which(lon==10.25),which(lat==9.75),1:48]))
#Koundara
Vap_Koundara_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                               Month=rep(seq(1,12,1), 4), 
                                               Vap1=vap.var[which(lon==13.25),which(lat==12.75),1:48],
                                               Vap2=vap.var[which(lon==13.25),which(lat==12.25),1:48], 
                                               Vap3=vap.var[which(lon==12.75),which(lat==12.25),1:48]))
Vap_Koundara_monthly$Vap <- rowMeans(select(Vap_Koundara_monthly, Vap1, Vap2, Vap3))
#Kouroussa
Vap_Kouroussa_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), 
                                            Vap1=vap.var[which(lon==10.25),which(lat==11.25),1:48],
                                            Vap2=vap.var[which(lon==10.75),which(lat==10.75),1:48], 
                                            Vap3=vap.var[which(lon==10.25),which(lat==10.75),1:48],
                                            Vap4=vap.var[which(lon==9.75),which(lat==10.75),1:48],
                                            Vap5=vap.var[which(lon==10.25),which(lat==10.25),1:48],
                                            Vap6=vap.var[which(lon==9.75),which(lat==10.25),1:48],
                                            Vap7=vap.var[which(lon==10.25),which(lat==9.75),1:48],
                                            Vap8=vap.var[which(lon==9.75),which(lat==9.75),1:48]))
Vap_Kouroussa_monthly$Vap <- rowMeans(select(Vap_Kouroussa_monthly, Vap1, Vap2, Vap3, Vap4, Vap5, Vap6, Vap7, Vap8))
#Labe
Vap_Labe_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                            Month=rep(seq(1,12,1), 4), 
                                            Vap1=vap.var[which(lon==12.25),which(lat==11.75),1:48],
                                            Vap2=vap.var[which(lon==12.25),which(lat==11.25),1:48]))
Vap_Labe_monthly$Vap <- rowMeans(select(Vap_Labe_monthly, Vap1, Vap2))
#Lelouma
Vap_Lelouma_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), 
                                        Vap1=vap.var[which(lon==12.75),which(lat==11.75),1:48],
                                        Vap2=vap.var[which(lon==12.75),which(lat==11.25),1:48]))
Vap_Lelouma_monthly$Vap <- rowMeans(select(Vap_Lelouma_monthly, Vap1, Vap2))
#Lola
Vap_Lola_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), 
                                           Vap1=vap.var[which(lon==8.25),which(lat==8.25),1:48],
                                           Vap2=vap.var[which(lon==8.25),which(lat==7.75),1:48]))
Vap_Lola_monthly$Vap <- rowMeans(select(Vap_Lola_monthly, Vap1, Vap2))
#Macenta
Vap_Macenta_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), 
                                        Vap1=vap.var[which(lon==9.75),which(lat==8.75),1:48],
                                        Vap2=vap.var[which(lon==9.25),which(lat==8.75),1:48],
                                        Vap3=vap.var[which(lon==9.25),which(lat==8.25),1:48]))
Vap_Macenta_monthly$Vap <- rowMeans(select(Vap_Macenta_monthly, Vap1, Vap2, Vap3))
#Mali
Vap_Mali_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), 
                                             Vap1=vap.var[which(lon==12.75),which(lat==12.25),1:48],
                                             Vap2=vap.var[which(lon==12.25),which(lat==12.25),1:48], 
                                             Vap3=vap.var[which(lon==11.75),which(lat==12.25),1:48],
                                             Vap4=vap.var[which(lon==12.75),which(lat==11.75),1:48],
                                             Vap5=vap.var[which(lon==12.25),which(lat==11.75),1:48]))
Vap_Mali_monthly$Vap <- rowMeans(select(Vap_Mali_monthly, Vap1, Vap2, Vap3, Vap4, Vap5))
#Mamou
Vap_Mamou_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), 
                                           Vap1=vap.var[which(lon==11.75),which(lat==10.75),1:48],
                                           Vap2=vap.var[which(lon==12.25),which(lat==10.25),1:48],
                                           Vap3=vap.var[which(lon==11.75),which(lat==10.25),1:48]))
Vap_Mamou_monthly$Vap <- rowMeans(select(Vap_Mamou_monthly, Vap1, Vap2, Vap3))
#Nzerekore
Vap_Nzerekore_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), 
                                        Vap1=vap.var[which(lon==8.75),which(lat==8.25),1:48],
                                        Vap2=vap.var[which(lon==8.75),which(lat==7.75),1:48]))
Vap_Nzerekore_monthly$Vap <- rowMeans(select(Vap_Nzerekore_monthly, Vap1, Vap2))
#Pita
Vap_Pita_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), 
                                           Vap1=vap.var[which(lon==12.75),which(lat==11.25),1:48],
                                           Vap2=vap.var[which(lon==12.25),which(lat==11.25),1:48],
                                           Vap3=vap.var[which(lon==12.75),which(lat==10.75),1:48]))
Vap_Pita_monthly$Vap <- rowMeans(select(Vap_Pita_monthly, Vap1, Vap2, Vap3))
#Siguiri
Vap_Siguiri_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                        Month=rep(seq(1,12,1), 4), 
                                        Vap1=vap.var[which(lon==9.25),which(lat==12.25),1:48],
                                        Vap2=vap.var[which(lon==10.25),which(lat==11.75),1:48], 
                                        Vap3=vap.var[which(lon==9.75),which(lat==11.75),1:48],
                                        Vap4=vap.var[which(lon==9.25),which(lat==11.75),1:48],
                                        Vap5=vap.var[which(lon==9.75),which(lat==11.25),1:48],
                                        Vap6=vap.var[which(lon==9.25),which(lat==11.25),1:48]))
Vap_Siguiri_monthly$Vap <- rowMeans(select(Vap_Siguiri_monthly, Vap1, Vap2, Vap3, Vap4, Vap5, Vap6))
#Telimele
Vap_Telimele_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                           Month=rep(seq(1,12,1), 4), 
                                           Vap1=vap.var[which(lon==13.75),which(lat==11.25),1:48],
                                           Vap2=vap.var[which(lon==13.25),which(lat==11.25),1:48], 
                                           Vap3=vap.var[which(lon==13.75),which(lat==10.75),1:48],
                                           Vap4=vap.var[which(lon==13.25),which(lat==10.75),1:48]))
Vap_Telimele_monthly$Vap <- rowMeans(select(Vap_Telimele_monthly, Vap1, Vap2, Vap3, Vap4, Vap5, Vap6))
#Tougue
Vap_Tougue_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), 
                                             Month=rep(seq(1,12,1), 4), 
                                             Vap1=vap.var[which(lon==8.75),which(lat==8.25),1:48],
                                             Vap2=vap.var[which(lon==8.75),which(lat==7.75),1:48]))
Vap_Tougue_monthly$Vap <- rowMeans(select(Vap_Tougue_monthly, Vap1, Vap2))