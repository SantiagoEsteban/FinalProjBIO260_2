#Visualizations
###############

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
library(ggmap)
library(sp)
library(gganimate)

################
# 3 Cities
################
conakry <- read.csv("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/guinea_weekly_cases_climate.csv") %>%
    filter(Location=='CONAKRY') %>% select(-X, -Epi_Week, -Week) %>% gather(measurement, value, 3:11)
freetown <- read.csv("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/SL_weekly_cases_climate.csv") %>%
    filter(Location=='WESTERNURBAN') %>% select(-X, -Epi_Week, -Week) %>% gather(measurement, value, 3:11)
monrovia <- read.csv("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Liberia_weekly_cases_climate.csv") %>%
    filter(Location=='MONTSERRADO') %>% select(-X, -Epi_Week, -Week) %>% gather(measurement, value, 3:11)

three_cities <- rbind(conakry, freetown, monrovia)

#Incidence
ggplot(three_cities, aes(x=count_week)) + 
    geom_area(aes(y=value, color=Location, fill=Location), alpha=0.3, data=filter(three_cities, measurement=='Total_cases'))







#All temp variables & incidence
ggplot(filter(three_cities, Location=='CONAKRY')) + geom_line(aes(x=count_week, y=value, color=measurement)) + facet_grid(measurement~., scale='free')
ggplot(filter(three_cities, Location=='MONTSERRADO')) + geom_line(aes(x=count_week, y=value, color=measurement)) + facet_grid(measurement~., scale='free')
ggplot(filter(three_cities, Location=='WESTERNURBAN')) + geom_line(aes(x=count_week, y=value, color=measurement)) + facet_grid(measurement~., scale='free')

#Temperature
conakry_wide <- read.csv("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/guinea_weekly_cases_climate.csv") %>%
    filter(Location=='CONAKRY') %>% select(-X, -Epi_Week, -Week)
ggplot(conakry_wide, aes(count_week)) + geom_ribbon(aes(ymin=tmn, ymax=tmx), color='gray', alpha=0.2) + 
    geom_line(aes(y=tmp), color='red', size=0.8) + geom_line(aes(y=tmx), color='orange', size=0.2) +
    geom_line(aes(y=tmn), color='orange', size=0.2) + theme_classic()

freetown_wide <- read.csv("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/SL_weekly_cases_climate.csv") %>%
    filter(Location=='WESTERNURBAN') %>% select(-X, -Epi_Week, -Week)
ggplot(freetown_wide, aes(count_week)) + geom_ribbon(aes(ymin=tmn, ymax=tmx), color='gray', alpha=0.2) + 
    geom_line(aes(y=tmp), color='red', size=0.8) + geom_line(aes(y=tmx), color='orange', size=0.2) +
    geom_line(aes(y=tmn), color='orange', size=0.2) + theme_classic()

montserrado_wide <- read.csv("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Liberia_weekly_cases_climate.csv") %>%
    filter(Location=='MONTSERRADO') %>% select(-X, -Epi_Week, -Week)
ggplot(freetown_wide, aes(count_week)) + geom_ribbon(aes(ymin=tmn, ymax=tmx), color='gray', alpha=0.2) + 
    geom_line(aes(y=tmp), color='red', size=0.8) + geom_line(aes(y=tmx), color='orange', size=0.2) +
    geom_line(aes(y=tmn), color='orange', size=0.2) + theme_classic()

ggplot(three_cities, aes(x=count_week)) + geom_line(aes(y=value, color=Location), data=filter(three_cities, measurement=='tmp'))

#Animated maps
#Incidence
#guinea data
guineards <- readRDS("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Guinea-Admin/GIN_adm2.rds")
guineashp.df <- fortify(guineards)
guinea_map <- as.data.frame(cbind(NAME_2=guineards@data$NAME_2, id=guineards@data$ID_2))
guinea_map$NAME_2 <- toupper(guinea_map$NAME_2)
guinea_map$NAME_2 <- str_replace_all(guinea_map$NAME_2, "BOKÉ", "BOKE")
guinea_map$NAME_2 <- str_replace_all(guinea_map$NAME_2, "DINGUIRAYE", "DINGUIRAY")
guinea_map$NAME_2 <- str_replace_all(guinea_map$NAME_2, "KÉROUANÉ", "KEROUANE")
guinea_map$NAME_2 <- str_replace_all(guinea_map$NAME_2, "DUBRÉKA", "DUBREKA")
guinea_map$NAME_2 <- str_replace_all(guinea_map$NAME_2, "FORÉCARIAH", "FORECARIAH")
guinea_map$NAME_2 <- str_replace_all(guinea_map$NAME_2, "TÉLIMÉLÉ", "TELIMELE")
guinea_map$NAME_2 <- str_replace_all(guinea_map$NAME_2, "LÉLOUMA", "LELOUMA")
guinea_map$NAME_2 <- str_replace_all(guinea_map$NAME_2, "LABÉ", "LABE")
guinea_map$NAME_2 <- str_replace_all(guinea_map$NAME_2, "TOUGUÉ", "TOUGUE")
guinea_map$NAME_2 <- str_replace_all(guinea_map$NAME_2, "GUÉCKÉDOU", "GUECKEDOU")
guinea_map$NAME_2 <- str_replace_all(guinea_map$NAME_2, "NZÉRÉKORÉ", "NZEREKORE")
guinea_map <- left_join(guineashp.df, guinea_map, by='id')
guinea_map <- rename(guinea_map, Location=NAME_2)
guinea_weekly_cases_climate <- read.csv("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/guinea_weekly_cases_climate.csv")
guinea_map_complete <- left_join(guinea_weekly_cases_climate, guinea_map, by='Location') %>% 
    select(-X, -Week, -Epi_Week, -group)



#Sierra leone data
SLrds <- readRDS("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Guinea-Admin/SLE_adm2.rds")
SLshp.df <- fortify(SLrds)
SL_map <- as.data.frame(cbind(NAME_2=SLrds@data$NAME_2, id=SLrds@data$ID_2))
SL_map$NAME_2 <-str_replace(SL_map$NAME_2, "Western Urban", "WESTERNURBAN")
SL_map$NAME_2 <-str_replace(SL_map$NAME_2, "Western Rural", "WESTERNRURAL")
SL_map$NAME_2 <-str_replace(SL_map$NAME_2, "Port Loko", "PORTLOKO")
SL_map$NAME_2 <- toupper(SL_map$NAME_2)
SL_map <- left_join(SLshp.df, SL_map, by='id')
SL_map <- rename(SL_map, Location=NAME_2)
SL_weekly_cases_climate <- read.csv("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/SL_weekly_cases_climate.csv")
SL_map_complete <- left_join(SL_weekly_cases_climate, SL_map, by='Location') %>% 
    select(-X, -Week, -Epi_Week, -group)

#Liberia data
LBrds <- readRDS("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Guinea-Admin/LBR_adm1.rds")
LBshp.df <- fortify(LBrds)
LB_map <- as.data.frame(cbind(Location=LBrds@data$NAME_1, id=LBrds@data$ID_1))
LB_map$Location <-str_replace(LB_map$Location, "Gbapolu", "GBARPOLU")
LB_map$Location <-str_replace(LB_map$Location, "GrandBassa", "GRAND BASSA")
LB_map$Location <-str_replace(LB_map$Location, "GrandGedeh", "GRAND GEDEH")
LB_map$Location <-str_replace(LB_map$Location, "GrandKru", "GRAND KRU")
LB_map$Location <-str_replace(LB_map$Location, "River Cess", "RIVER CESS")
LB_map$Location <- toupper(LB_map$Location)
LB_map <- left_join(LBshp.df, LB_map, by='id')
LB_weekly_cases_climate <- read.csv("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Liberia_weekly_cases_climate.csv")
LB_map_complete <- left_join(LB_weekly_cases_climate, LB_map, by='Location') %>% 
    select(-X, -Week, -Epi_Week, -group)

#################
#CREAR EL MAPA PERO CON CUMULATIVE INCIDENCE
#################


#Creating on file with all the climate, cases and map data
three_countries_map_complete <- rbind(guinea_map_complete, SL_map_complete, LB_map_complete)
three_countries_map_complete <- cbind(three_countries_map_complete, seq(as.Date('2013-01-01'), as.Date('2015-11-29'), by="week"))

three_countries_map_incidence <- ggplot(select(three_countries_map_complete, Location, long, lat, Total_cases, count_week) %>% 
                                            filter(count_week>52), aes(x = long, y = lat, group = Location, fill = Total_cases, frame=count_week)) +
    geom_polygon(color = "black", size = 0.25) + coord_map() + scale_fill_gradient(low='light gray', high='red')

gg_animate(three_countries_map_incidence)


conakry_cases <- read.csv("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Cases/guinea_cases_long.csv")
conakry_cases <- filter(conakry_cases, Location=="CONAKRY" & Source=='Patient database' & Case_definition=='Confirmed')
conakry_cases$week_number <- 1:length(conakry_cases$Epi_Week)
guinea_monthly <- read.csv("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/guinea_monthly_district.csv")

Guinea <- get_map(location = c(lon = -11.15, lat = 9.950287),
                  color = "color",
                  source = "google",
                  maptype = "satellite",
                  zoom = 6, scale=2)







data$tmp <- as.numeric(as.character((data$tmp)))







ggmap(Guinea, extent='normal') +
    scale_x_continuous(limits = c(-15.5, -7.5), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(7, 13.5), expand = c(0, 0)) +
    geom_polygon(aes(x = long,
                     y = lat,
                     group=group,
                     color=id), 
                 size=1,
                 data=guineashp.df, fill=tmp$Value)