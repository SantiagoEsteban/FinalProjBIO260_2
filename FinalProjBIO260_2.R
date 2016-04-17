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

###############
#Daily Weather Data
###############
weather <- read_csv("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/721339.csv")
weather$PRCP[weather$PRCP==-9999] <- NA
weather$TMAX[weather$TMAX==-9999] <- NA
weather$TMIN[weather$TMIN==-9999] <- NA
weather$DATE <- ymd(weather$DATE)
weather_Guinea <- filter(weather, STATION_NAME!='LUNGI SL' & STATION_NAME!='KANKAN GV' & STATION_NAME!='MONROVIA ROBERTS INTERNATIONAL LI')
weather_SierraLeone <- filter(weather, STATION_NAME=='LUNGI SL')
weather_Liberia <- filter(weather, STATION_NAME=='MONROVIA ROBERTS INTERNATIONAL LI')
weather_Guinea$week <- floor_date(weather_Guinea$DATE, unit="week")
weather_Guinea$month <- floor_date(weather_Guinea$DATE, unit="month")
weather_Guinea <- as.data.frame(unlist(weather_Guinea))


##########
#Guinea
##########
#DAILY
ggplot(data=filter(weather_Guinea, STATION_NAME=='CONAKRY AERO GV')) +
    geom_point(aes(x=DATE, y=TMAX, color=TMAX)) +
                   scale_colour_gradient(low="yellow", high="red") + 
    geom_smooth(aes(x=DATE, y=TMAX), color='red', method='loess', span=0.2) + 
    geom_point(aes(x=DATE, y=TMIN, color=TMIN)) +
    geom_smooth(aes(x=DATE, y=TMIN), color='blue', method='loess', span=0.2)

#Weekly
ggplot(data=filter(weather_Guinea, STATION_NAME=='CONAKRY AERO GV' & DATE>'2014-06-01')) + 
           geom_boxplot(aes(y=TMAX, x=week, group=week)) + 
    geom_smooth(aes(x=DATE, y=TMAX), method='loess', span=0.2) +
    geom_boxplot(aes(y=TMIN, x=week, group=week)) + 
    geom_smooth(aes(x=DATE, y=TMIN), method='loess', span=0.2)


#Monthly Conakry
#Vap - vapor pressure
vap <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.vap.dat.nc', write=FALSE)
vap2 <- read.nc(vap, unpack=TRUE)
vap3 <- var.get.nc(vap, "vap")
Vap_Conakry_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), Month=rep(seq(1,12,1), 4), Vap=vap3[333,200,1:48]))

#Wet - wet day frequency in days
Wet <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.01.2011.2014.wet.dat.nc', write=FALSE)
Wet2 <- read.nc(Wet, unpack=TRUE)
Wet3 <- var.get.nc(Wet, "wet")
Wet_Conakry_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), Month=rep(seq(1,12,1), 4), Wet=Wet3[333,200,1:48]))

#dtr - daily temperature range
dtr <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.dtr.dat.nc', write=FALSE)
dtr2 <- read.nc(dtr, unpack=TRUE)
dtr3 <- var.get.nc(dtr, "dtr")
dtr_Conakry_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), Month=rep(seq(1,12,1), 4), dtr=dtr3[333,200,1:48]))

#pet - potential evapotranspiration
pet <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.pet.dat.nc', write=FALSE)
pet2 <- read.nc(pet, unpack=TRUE)
pet3 <- var.get.nc(pet, "pet")
pet_Conakry_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), Month=rep(seq(1,12,1), 4), pet=pet3[333,200,1:48]))

#Pre - precipitation
pre <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.pre.dat.nc', write=FALSE)
pre2 <- read.nc(pre, unpack=TRUE)
pre3 <- var.get.nc(pre, "pre")
pre_Conakry_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), Month=rep(seq(1,12,1), 4), pre=pre3[333,200,1:48]))

#Tmn - Tmin
tmn <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.tmn.dat.nc', write=FALSE)
tmn2 <- read.nc(tmn, unpack=TRUE)
tmn3 <- var.get.nc(tmn, "tmn")
tmn_Conakry_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), Month=rep(seq(1,12,1), 4), tmn=tmn3[333,200,1:48]))

#Tmp - Tmean
tmp <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.tmp.dat.nc', write=FALSE)
tmp2 <- read.nc(tmn, unpack=TRUE)
tmp3 <- var.get.nc(tmp, "tmp")
tmp_Conakry_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), Month=rep(seq(1,12,1), 4), tmp=tmp3[333,200,1:48]))

#Tmx - Tmax
tmx <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.tmx.dat.nc', write=FALSE)
tmx2 <- read.nc(tmx, unpack=TRUE)
tmx3 <- var.get.nc(tmx, "tmx")
tmx_Conakry_monthly <- as.data.frame(cbind(Year=c(rep(2011, 12), rep(2012, 12), rep(2013, 12), rep(2014, 12)), Month=rep(seq(1,12,1), 4), tmx=tmx3[333,200,1:48]))

#Join
Conakry_monthly <- full_join(Vap_Conakry_monthly, Wet_Conakry_monthly)
Conakry_monthly <- full_join(Conakry_monthly, dtr_Conakry_monthly)
Conakry_monthly <- full_join(Conakry_monthly, pet_Conakry_monthly)
Conakry_monthly <- full_join(Conakry_monthly, pre_Conakry_monthly)
Conakry_monthly <- full_join(Conakry_monthly, tmn_Conakry_monthly)
Conakry_monthly <- full_join(Conakry_monthly, tmp_Conakry_monthly)
Conakry_monthly <- full_join(Conakry_monthly, tmx_Conakry_monthly)
Conakry_monthly$day <- 1
Conakry_monthly$date <- ymd(paste(Conakry_monthly$Year, Conakry_monthly$Month, Conakry_monthly$day, sep="-"))
Conakry_monthly <- select(Conakry_monthly, date, Year, Month, day, Vap, Wet, dtr, pet, pre, tmn, tmx, tmp)

Conakry_monthly_2015 <- group_by(Conakry_monthly, Month) %>% summarize(Vap=mean(Vap), Wet=mean(Wet),
                                                                       dtr=mean(dtr), pet=mean(pet),
                                                                       pre=mean(pre), tmn=mean(tmn),
                                                                       tmx=mean(tmx), tmp=mean(tmp))

Conakry_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), 
                                             day=1)) 
Conakry_monthly_2015a$date <- ymd(paste(Conakry_monthly_2015a$Year, Conakry_monthly_2015a$Month, Conakry_monthly_2015a$day, sep="-"))
Conakry_monthly_2015a <- select(Conakry_monthly_2015a, date, Year, Month, day)
Conakry_monthly_2015 <- full_join(Conakry_monthly_2015a, Conakry_monthly_2015)
Conakry_monthly <- rbind(Conakry_monthly, Conakry_monthly_2015)
Conakry_monthly_long <- gather(Conakry_monthly, measure, value, 5:12)

#Graph
ggplot(Conakry_monthly_long, aes(x=date, y=value, color=measure)) + 
    stat_smooth(n=24, method="loess", span=0.2)+facet_grid(measure~., scale='free')

write.csv(Conakry_monthly, file="Conakry_monthly.csv")

#############
#Guinea CASES
#############

guinea_cases <- read_csv("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Cases/Guinea_Cases_district.csv")
guinea_cases <- gather(guinea_cases, Week, Cases, 5:123)
guinea_cases$Epi_Week <- gsub("^.*\\("," ",x=guinea_cases$Week)
guinea_cases$Epi_Week <- gsub("\\)"," ",x=guinea_cases$Epi_Week)
colnames(guinea_cases) <- c('Location', 'Source', 'Indicator', 'Case_definition', 'Week', 'Cases', 'Epi_Week')
write.csv(guinea_cases, "guinea_cases_long.csv")

ggplot(aes(x=Epi_Week, y=Cases), 
       data=filter(guinea_cases, Source=="Patient database" & Case_definition=="Confirmed" & Epi_Week>'2014-06-01')) + 
    geom_line(aes(color=Cases, group=Location)) + facet_grid(Location~.)

ggplot(aes(x=Epi_Week, y=Cases), 
       data=filter(guinea_cases, Source=="Patient database" & Case_definition=="Confirmed" & Location=='CONAKRY' )) + 
    geom_line(aes(color=Cases, group=Location))

#####
#Guinea MAPS
#####
Guinea <- get_map(location = c(lon = -11.15, lat = 9.950287),
                    color = "color",
                    source = "google",
                    maptype = "satellite",
                    zoom = 6, scale=2)

name <- c('CONAKRY AERO, GV', 'KINDIA, GV', 'N ZEREKORE KONIA, GV')
lon <- as.numeric(c(-13.62, -12.87,  -8.833))     
lat <- as.numeric(c(9.57, 10.05, 7.733))

Guinea_stations <- as.data.frame(cbind(lon, lat, name))
Guinea_stations$lon <- as.numeric(as.character((Guinea_stations$lon)))
Guinea_stations$lat <- as.numeric(as.character((Guinea_stations$lat)))

library(sp)
guineards <- readRDS("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Guinea-Admin/GIN_adm1.rds")
guineashp.df <- fortify(guineards)

ggmap(Guinea, extent='normal') +
    scale_x_continuous(limits = c(-15.5, -7.5), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(7, 13.5), expand = c(0, 0)) +
    geom_polygon(aes(x = long,
                     y = lat,
                     group=group,
                     color=id), 
                 size=1,
                 data=guineashp.df, fill=NA) + 
    coord_map() + 
    labs(x = "Longitude",y = "Latitude") +
    geom_point(aes(x=lon, y=lat, shape=name), color="red", size=5, data=Guinea_stations)


#############
#Sierra Leone
#############

#Monthly Freetown
#Vap - vapor pressure
vap <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.vap.dat.nc', write=FALSE)
vap2 <- read.nc(vap, unpack=TRUE)
vap3 <- var.get.nc(vap, "vap")
Vap_Freetown_monthly <- as.data.frame(cbind(Year=c(rep(2013, 12), rep(2014, 12)), Month=rep(seq(1,12,1), 2), Vap=vap3[334,197,25:48]))

#Wet - wet day frequency in days
Wet <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.01.2011.2014.wet.dat.nc', write=FALSE)
Wet2 <- read.nc(Wet, unpack=TRUE)
Wet3 <- var.get.nc(Wet, "wet")
Wet_Freetown_monthly <- as.data.frame(cbind(Year=c(rep(2013, 12), rep(2014, 12)), Month=rep(seq(1,12,1), 2), Wet=Wet3[334,197,25:48]))

#dtr - daily temperature range
dtr <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.dtr.dat.nc', write=FALSE)
dtr2 <- read.nc(dtr, unpack=TRUE)
dtr3 <- var.get.nc(dtr, "dtr")
dtr_Freetown_monthly <- as.data.frame(cbind(Year=c(rep(2013, 12), rep(2014, 12)), Month=rep(seq(1,12,1), 2), dtr=dtr3[334,197,25:48]))

#pet - potential evapotranspiration
pet <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.pet.dat.nc', write=FALSE)
pet2 <- read.nc(pet, unpack=TRUE)
pet3 <- var.get.nc(pet, "pet")
pet_Freetown_monthly <- as.data.frame(cbind(Year=c(rep(2013, 12), rep(2014, 12)), Month=rep(seq(1,12,1), 2), pet=pet3[334,197,25:48]))

#Pre - precipitation
pre <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.pre.dat.nc', write=FALSE)
pre2 <- read.nc(pre, unpack=TRUE)
pre3 <- var.get.nc(pre, "pre")
pre_Freetown_monthly <- as.data.frame(cbind(Year=c(rep(2013, 12), rep(2014, 12)), Month=rep(seq(1,12,1), 2), pre=pre3[334,197,25:48]))

#Tmn - Tmin
tmn <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.tmn.dat.nc', write=FALSE)
tmn2 <- read.nc(tmn, unpack=TRUE)
tmn3 <- var.get.nc(tmn, "tmn")
tmn_Freetown_monthly <- as.data.frame(cbind(Year=c(rep(2013, 12), rep(2014, 12)), Month=rep(seq(1,12,1), 2), tmn=tmn3[334,197,25:48]))

#Tmp - Tmean
tmp <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.tmp.dat.nc', write=FALSE)
tmp2 <- read.nc(tmn, unpack=TRUE)
tmp3 <- var.get.nc(tmp, "tmp")
tmp_Freetown_monthly <- as.data.frame(cbind(Year=c(rep(2013, 12), rep(2014, 12)), Month=rep(seq(1,12,1), 2), tmp=tmp3[334,197,25:48]))

#Tmx - Tmax
tmx <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.tmx.dat.nc', write=FALSE)
tmx2 <- read.nc(tmx, unpack=TRUE)
tmx3 <- var.get.nc(tmx, "tmx")
tmx_Freetown_monthly <- as.data.frame(cbind(Year=c(rep(2013, 12), rep(2014, 12)), Month=rep(seq(1,12,1), 2), tmx=tmx3[334,197,25:48]))

#Join
Freetown_monthly <- full_join(Vap_Freetown_monthly, Wet_Freetown_monthly)
Freetown_monthly <- full_join(Freetown_monthly, dtr_Freetown_monthly)
Freetown_monthly <- full_join(Freetown_monthly, pet_Freetown_monthly)
Freetown_monthly <- full_join(Freetown_monthly, pre_Freetown_monthly)
Freetown_monthly <- full_join(Freetown_monthly, tmn_Freetown_monthly)
Freetown_monthly <- full_join(Freetown_monthly, tmp_Freetown_monthly)
Freetown_monthly <- full_join(Freetown_monthly, tmx_Freetown_monthly)
Freetown_monthly$day <- 1
Freetown_monthly$date <- ymd(paste(Freetown_monthly$Year, Freetown_monthly$Month, Freetown_monthly$day, sep="-"))
Freetown_monthly <- select(Freetown_monthly, date, Year, Month, day, Vap, Wet, dtr, pet, pre, tmn, tmx, tmp)
Freetown_monthly_2015 <- group_by(Freetown_monthly, Month) %>% summarize(Vap=mean(Vap), Wet=mean(Wet),
                                                                       dtr=mean(dtr), pet=mean(pet),
                                                                       pre=mean(pre), tmn=mean(tmn),
                                                                       tmx=mean(tmx), tmp=mean(tmp))

Freetown_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), 
                                             day=1)) 
Freetown_monthly_2015a$date <- ymd(paste(Freetown_monthly_2015a$Year, Freetown_monthly_2015a$Month, Freetown_monthly_2015a$day, sep="-"))
Freetown_monthly_2015a <- select(Freetown_monthly_2015a, date, Year, Month, day)
Freetown_monthly_2015 <- full_join(Freetown_monthly_2015a, Freetown_monthly_2015)
Freetown_monthly <- rbind(Freetown_monthly, Freetown_monthly_2015)
Freetown_monthly_long <- gather(Freetown_monthly, measure, value, 5:12)

#Graph
ggplot(Freetown_monthly_long, aes(x=date, y=value, color=measure)) + 
    stat_smooth(n=24, method="loess", span=0.2)+facet_grid(measure~., scale='free')

write.csv(Freetown_monthly, file="Freetown_monthly.csv")

##########
#SL CASES
##########

SierraLeone_cases <- read_csv("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Cases/SierraLeone_Cases_district.csv")
SierraLeone_cases <- gather(SierraLeone_cases, Week, Cases, 5:123)
SierraLeone_cases$Epi_Week <- gsub("^.*\\("," ",x=SierraLeone_cases$Week)
SierraLeone_cases$Epi_Week <- gsub("\\)"," ",x=SierraLeone_cases$Epi_Week)
colnames(SierraLeone_cases) <- c('Location', 'Source', 'Indicator', 'Case_definition', 'Week', 'Cases', 'Epi_Week')
write.csv(SierraLeone_cases, "SierraLeone_cases_long.csv")

##########
#SL MAPS
##########

SierraLeone <- get_map(location = c(lon = -11.744002, lat = 8.708781),
                       color = "color",
                       source = "google",
                       maptype = "terrain",
                       zoom = 7)

name <- c('LUNGUI, SL')
lon <- -13.2   
lat <- 8.617

SierraLeone_stations <- as.data.frame(cbind(name, lon, lat))
SierraLeone_stations$lon <- as.numeric(as.character((SierraLeone_stations$lon)))
SierraLeone_stations$lat <- as.numeric(as.character((SierraLeone_stations$lat)))

ggmap(SierraLeone, extent='device') + 
    geom_point(aes(x=lon, y=lat, color=name), size=5, data=SierraLeone_stations)





#############
#Liberia
#############

#Monthly Monrovia
#Vap - vapor pressure
vap <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.vap.dat.nc', write=FALSE)
vap2 <- read.nc(vap, unpack=TRUE)
vap3 <- var.get.nc(vap, "vap")
Vap_Monrovia_monthly <- as.data.frame(cbind(Year=c(rep(2013, 12), rep(2014, 12)), Month=rep(seq(1,12,1), 2), Vap=vap3[339,193,25:48]))

#Wet - wet day frequency in days
Wet <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.01.2011.2014.wet.dat.nc', write=FALSE)
Wet2 <- read.nc(Wet, unpack=TRUE)
Wet3 <- var.get.nc(Wet, "wet")
Wet_Monrovia_monthly <- as.data.frame(cbind(Year=c(rep(2013, 12), rep(2014, 12)), Month=rep(seq(1,12,1), 2), Wet=Wet3[339,193,25:48]))

#dtr - daily temperature range
dtr <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.dtr.dat.nc', write=FALSE)
dtr2 <- read.nc(dtr, unpack=TRUE)
dtr3 <- var.get.nc(dtr, "dtr")
dtr_Monrovia_monthly <- as.data.frame(cbind(Year=c(rep(2013, 12), rep(2014, 12)), Month=rep(seq(1,12,1), 2), dtr=dtr3[339,193,25:48]))

#pet - potential evapotranspiration
pet <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.pet.dat.nc', write=FALSE)
pet2 <- read.nc(pet, unpack=TRUE)
pet3 <- var.get.nc(pet, "pet")
pet_Monrovia_monthly <- as.data.frame(cbind(Year=c(rep(2013, 12), rep(2014, 12)), Month=rep(seq(1,12,1), 2), pet=pet3[339,193,25:48]))

#Pre - precipitation
pre <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.pre.dat.nc', write=FALSE)
pre2 <- read.nc(pre, unpack=TRUE)
pre3 <- var.get.nc(pre, "pre")
pre_Monrovia_monthly <- as.data.frame(cbind(Year=c(rep(2013, 12), rep(2014, 12)), Month=rep(seq(1,12,1), 2), pre=pre3[339,193,25:48]))

#Tmn - Tmin
tmn <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.tmn.dat.nc', write=FALSE)
tmn2 <- read.nc(tmn, unpack=TRUE)
tmn3 <- var.get.nc(tmn, "tmn")
tmn_Monrovia_monthly <- as.data.frame(cbind(Year=c(rep(2013, 12), rep(2014, 12)), Month=rep(seq(1,12,1), 2), tmn=tmn3[339,193,25:48]))

#Tmp - Tmean
tmp <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.tmp.dat.nc', write=FALSE)
tmp2 <- read.nc(tmn, unpack=TRUE)
tmp3 <- var.get.nc(tmp, "tmp")
tmp_Monrovia_monthly <- as.data.frame(cbind(Year=c(rep(2013, 12), rep(2014, 12)), Month=rep(seq(1,12,1), 2), tmp=tmp3[339,193,25:48]))

#Tmx - Tmax
tmx <- open.nc('D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/cru_ts3.23.2011.2014.tmx.dat.nc', write=FALSE)
tmx2 <- read.nc(tmx, unpack=TRUE)
tmx3 <- var.get.nc(tmx, "tmx")
tmx_Monrovia_monthly <- as.data.frame(cbind(Year=c(rep(2013, 12), rep(2014, 12)), Month=rep(seq(1,12,1), 2), tmx=tmx3[339,193,25:48]))

#Join
Monrovia_monthly <- full_join(Vap_Monrovia_monthly, Wet_Monrovia_monthly)
Monrovia_monthly <- full_join(Monrovia_monthly, dtr_Monrovia_monthly)
Monrovia_monthly <- full_join(Monrovia_monthly, pet_Monrovia_monthly)
Monrovia_monthly <- full_join(Monrovia_monthly, pre_Monrovia_monthly)
Monrovia_monthly <- full_join(Monrovia_monthly, tmn_Monrovia_monthly)
Monrovia_monthly <- full_join(Monrovia_monthly, tmp_Monrovia_monthly)
Monrovia_monthly <- full_join(Monrovia_monthly, tmx_Monrovia_monthly)
Monrovia_monthly$day <- 1
Monrovia_monthly$date <- ymd(paste(Monrovia_monthly$Year, Monrovia_monthly$Month, Monrovia_monthly$day, sep="-"))
Monrovia_monthly <- select(Monrovia_monthly, date, Year, Month, day, Vap, Wet, dtr, pet, pre, tmn, tmx, tmp)
Monrovia_monthly_2015 <- group_by(Monrovia_monthly, Month) %>% summarize(Vap=mean(Vap), Wet=mean(Wet),
                                                                       dtr=mean(dtr), pet=mean(pet),
                                                                       pre=mean(pre), tmn=mean(tmn),
                                                                       tmx=mean(tmx), tmp=mean(tmp))

Monrovia_monthly_2015a <- as.data.frame(cbind(Year=rep(2015, 12), Month=rep(seq(1,12,1),1), 
                                             day=1)) 
Monrovia_monthly_2015a$date <- ymd(paste(Monrovia_monthly_2015a$Year, Monrovia_monthly_2015a$Month, Monrovia_monthly_2015a$day, sep="-"))
Monrovia_monthly_2015a <- select(Monrovia_monthly_2015a, date, Year, Month, day)
Monrovia_monthly_2015 <- full_join(Monrovia_monthly_2015a, Monrovia_monthly_2015)
Monrovia_monthly <- rbind(Monrovia_monthly, Monrovia_monthly_2015)
Monrovia_monthly_long <- gather(Monrovia_monthly, measure, value, 5:12)

#Graph
ggplot(Monrovia_monthly_long, aes(x=date, y=value, color=measure)) + 
    stat_smooth(n=24, method="loess", span=0.2)+facet_grid(measure~., scale='free')

write.csv(Monrovia_monthly, file="Monrovia_monthly.csv")

##########
#LI CASES
##########
liberia_cases <- read_csv("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Cases/Liberia_Cases_district.csv")
liberia_cases <- gather(liberia_cases, Week, Cases, 5:123)
liberia_cases$Epi_Week <- gsub("^.*\\("," ",x=liberia_cases$Week)
liberia_cases$Epi_Week <- gsub("\\)"," ",x=liberia_cases$Epi_Week)
colnames(liberia_cases) <- c('Location', 'Source', 'Indicator', 'Case_definition', 'Week', 'Cases', 'Epi_Week')
write.csv(liberia_cases, "liberia_cases_long.csv")



##########
#LI MAPS
##########

Liberia <- get_map(location = c(lon = -9.250512, lat = 6.450637),
                   color = "color",
                   source = "google",
                   maptype = "terrain",
                   zoom = 7)

name <- c('MONROVIA ROBERTS INT, LI')
lon <- -10.362   
lat <- 6.234

Liberia_stations <- as.data.frame(cbind(name, lon, lat))
Liberia_stations$lon <- as.numeric(as.character((Liberia_stations$lon)))
Liberia_stations$lat <- as.numeric(as.character((Liberia_stations$lat)))

ggmap(Liberia, extent='device') + 
    geom_point(aes(x=lon, y=lat, color=name), size=5, data=Liberia_stations)

