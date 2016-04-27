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


#Incidence
guinea_weekly_cases_climate <- read.csv("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/guinea_weekly_cases_climate.csv")
guinea_weekly_cases_climate <- group_by(guinea_weekly_cases_climate, Location) %>% mutate(Cum_cases=cumsum(Total_cases)) %>% 
    ungroup %>% mutate(Weeks=rep(seq(as.Date('2013-01-01'), as.Date('2015-11-29'), by="week"), 33))
SL_weekly_cases_climate <- read.csv("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/SL_weekly_cases_climate.csv")
SL_weekly_cases_climate <- group_by(SL_weekly_cases_climate, Location) %>% mutate(Cum_cases=cumsum(Total_cases)) %>% 
    ungroup %>% mutate(Weeks=rep(seq(as.Date('2013-01-01'), as.Date('2015-11-29'), by="week"),14))
LB_weekly_cases_climate <- read.csv("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Liberia_weekly_cases_climate.csv")
LB_weekly_cases_climate <- group_by(LB_weekly_cases_climate, Location) %>% mutate(Cum_cases=cumsum(Total_cases)) %>% 
    ungroup %>% mutate(Weeks=rep(seq(as.Date('2013-01-01'), as.Date('2015-11-29'), by="week"),15))

#Cumulative incidence for guinea per district
ggplot(filter(guinea_weekly_cases_climate, count_week>52)) + 
    geom_line(aes(x=Weeks, y=Cum_cases, group=Location, color=Location), size=0.75) +
    ylab('Cumulative cases count') + ggtitle('Cumualtive incidence per week for Guinea')

#Cumulative incidence for Liberia per district
ggplot(filter(LB_weekly_cases_climate, count_week>52)) + 
    geom_line(aes(x=Weeks, y=Cum_cases, group=Location, color=Location), size=0.75) +
    ylab('Cumulative cases count') + ggtitle('Cumualtive incidence per week for Liberia')

#Cumulative incidence for Liberia per district
ggplot(filter(SL_weekly_cases_climate, count_week>52)) + 
    geom_line(aes(x=Weeks, y=Cum_cases, group=Location, color=Location), size=0.75) +
    ylab('Cumulative cases count') + ggtitle('Cumualtive incidence per week for Sierra Leone')


#Cumulative cases for the three countries
cum_guinea <- select(guinea_weekly_cases_climate,Location, count_week, Cum_cases, Weeks) %>%
                filter(count_week>52) %>%
                group_by(Weeks) %>%
                summarise(country_cum_sum=sum(Cum_cases)) %>%
                cbind(Country='Guinea')
cum_liberia <- select(LB_weekly_cases_climate,Location, count_week, Cum_cases, Weeks) %>%
    filter(count_week>52) %>%
    group_by(Weeks) %>%
    summarise(country_cum_sum=sum(Cum_cases))%>%
    cbind(Country='Liberia')
cum_SL <- select(SL_weekly_cases_climate,Location, count_week, Cum_cases, Weeks) %>%
    filter(count_week>52) %>%
    group_by(Weeks) %>%
    summarise(country_cum_sum=sum(Cum_cases))%>%
    cbind(Country='Sierra Leone')

cum_three_countries <- rbind(cum_guinea, cum_liberia, cum_SL)

ggplot(cum_three_countries) + 
    geom_line(aes(x=Weeks, y=country_cum_sum, group=Country, color=Country), size=0.75) + 
    ylab('Cumulative case count') + ggtitle('Cumulative incidence per week')


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
guinea_weekly_cases_climate <- group_by(guinea_weekly_cases_climate, Location) %>% mutate(Cum_cases=cumsum(Total_cases)) %>% 
    ungroup %>% mutate(Weeks=rep(seq(as.Date('2013-01-01'), as.Date('2015-11-29'), by="week"), 33))
guinea_map_complete <- left_join(guinea_weekly_cases_climate, guinea_map, by='Location') %>% 
    select(-X, -Week, -Epi_Week)%>% mutate(Country='Guinea')
guineards_adm0 <- readRDS("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Guinea-Admin/GIN_adm0.rds")
guineashp.adm0 <- fortify(guineards_adm0)
#write.csv(guinea_map_complete, "guinea_map_complete.csv")

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
SL_weekly_cases_climate <- group_by(SL_weekly_cases_climate, Location) %>% mutate(Cum_cases=cumsum(Total_cases)) %>% 
    ungroup %>% mutate(Weeks=rep(seq(as.Date('2013-01-01'), as.Date('2015-11-29'), by="week"),14))
SL_map_complete <- left_join(SL_weekly_cases_climate, SL_map, by='Location') %>% 
    select(-X, -Week, -Epi_Week)%>% mutate(Country='Sierra Leone')
SL_adm0_rds <- readRDS("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Guinea-Admin/SLE_adm0.rds")
SLshp.adm0 <- fortify(SL_adm0_rds)
SL_map_complete$group <- as.factor(as.numeric(as.character(SL_map_complete$group))+100)
#write.csv(SL_map_complete, "SL_map_complete.csv")

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
LB_weekly_cases_climate <- group_by(LB_weekly_cases_climate, Location) %>% mutate(Cum_cases=cumsum(Total_cases)) %>% 
    ungroup %>% mutate(Weeks=rep(seq(as.Date('2013-01-01'), as.Date('2015-11-29'), by="week"),15))
LB_map_complete <- left_join(LB_weekly_cases_climate, LB_map, by='Location') %>% 
    select(-X, -Week, -Epi_Week) %>% mutate(Country='Liberia')
LB_adm0_rds <- readRDS("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Guinea-Admin/LBR_adm0.rds")
LBshp.adm0 <- fortify(LB_adm0_rds)
LB_map_complete$group <- as.factor(as.numeric(as.character(LB_map_complete$group))+200)
#write(LB_map_complete, "LB_map_complete.csv")

#Creating one file with all the climate, cases and map data
three_countries_map_complete <- rbind(guinea_map_complete, SL_map_complete, LB_map_complete)

#Cumulative incidence per country
    ggplot(select(three_countries_map_complete, Location, long, lat, Cum_cases, Weeks, order, piece, count_week, group) %>% 
                                            filter(count_week==152), aes(x = long, y = lat, group = group, fill = Cum_cases)) +
    geom_polygon(color = "black", size = 0.25) + scale_fill_gradient(low='light gray', high='red', name='Cumulative cases') +
    geom_polygon(aes(x = long, y = lat, group = group, color="Guinea"), fill=NA, size = 0.75, data=guineashp.adm0) +
    geom_polygon(aes(x = long, y = lat, group = group, color="Sierra Leone"), fill=NA, size = 0.75, data=SLshp.adm0) +
    geom_polygon(aes(x = long, y = lat, group = group, color="Liberia"), fill=NA, size = 0.75, data=LBshp.adm0) +
    coord_map() + scale_color_discrete(name="Country")

#Cumulative incidence animation
three_countries_map_incidence <- ggplot(select(three_countries_map_complete, Location, long, lat, Cum_cases, Weeks, count_week) %>% 
                                            filter(count_week>52), aes(x = long, y = lat, group = Location, fill = Cum_cases, frame=Weeks)) +
    geom_polygon(color = "black", size = 0.25) + coord_map() + scale_fill_gradient(low='light gray', high='red') +
    geom_polygon(aes(x = long, y = lat, group = group, color="Guinea"), fill=NA, size = 0.75, data=guineashp.adm0) +
    geom_polygon(aes(x = long, y = lat, group = group, color="Sierra Leone"), fill=NA, size = 0.75, data=SLshp.adm0) +
    geom_polygon(aes(x = long, y = lat, group = group, color="Liberia"), fill=NA, size = 0.75, data=LBshp.adm0) +
    coord_map()

gg_animate(three_countries_map_incidence, saver='mp4')


#Mean Temp per country
ggplot(three_countries_map_complete %>% 
           filter(count_week==1 | count_week==26 | count_week==53|count_week==79|count_week==105|count_week==131, Country=='Guinea'), 
       aes(x = long, y = lat, group = group, fill = tmp)) +
    geom_polygon(color = "black", size = 0.25) + scale_fill_gradient(low='yellow', high='red', name='Mean Temp') +
    coord_map() + facet_wrap(~Weeks)

ggplot(three_countries_map_complete %>% 
           filter(count_week==1 | count_week==26 | count_week==53|count_week==79|count_week==105|count_week==131, Country=='Sierra Leone'), 
       aes(x = long, y = lat, group = group, fill = tmp)) +
    geom_polygon(color = "black", size = 0.25) + scale_fill_gradient(low='yellow', high='red', name='Mean Temp') +
    coord_map() + facet_wrap(~Weeks)

ggplot(three_countries_map_complete %>% 
           filter(count_week==1 | count_week==26 | count_week==53|count_week==79|count_week==105|count_week==131, Country=='Liberia'), 
       aes(x = long, y = lat, group = group, fill = tmp)) +
    geom_polygon(color = "black", size = 0.25) + scale_fill_gradient(low='yellow', high='red', name='Mean Temp') +
    coord_map() + facet_wrap(~Weeks)

ggplot(three_countries_map_complete %>% 
           filter(count_week==1 | count_week==26 | count_week==53|count_week==79|count_week==105|count_week==131)
       , aes(x = long, y = lat, group = group, fill = tmp)) +
    geom_polygon(color = "black", size = 0.25) + scale_fill_gradient(low='yellow', high='red') +
    geom_polygon(aes(x = long, y = lat, group = group, color="Guinea"), fill=NA, size = 0.75, data=guineashp.adm0) +
    geom_polygon(aes(x = long, y = lat, group = group, color="Sierra Leone"), fill=NA, size = 0.75, data=SLshp.adm0) +
    geom_polygon(aes(x = long, y = lat, group = group, color="Liberia"), fill=NA, size = 0.75, data=LBshp.adm0) +
    coord_map() + scale_color_discrete(name="Country")+ facet_wrap(~Weeks)


#Mean pre per country
ggplot(three_countries_map_complete %>% 
           filter(count_week==1 | count_week==26 | count_week==53|count_week==79|count_week==105|count_week==131, Country=='Guinea'), 
       aes(x = long, y = lat, group = group, fill = pre)) +
    geom_polygon(color = "black", size = 0.25) + scale_fill_gradient(name='Mean Precipitation') +
    coord_map() + facet_wrap(~Weeks)

ggplot(three_countries_map_complete %>% 
           filter(count_week==1 | count_week==26 | count_week==53|count_week==79|count_week==105|count_week==131, Country=='Sierra Leone'), 
       aes(x = long, y = lat, group = group, fill = pre)) +
    geom_polygon(color = "black", size = 0.25) + scale_fill_gradient(name='Mean Precipitation') +
    coord_map() + facet_wrap(~Weeks)

ggplot(three_countries_map_complete %>% 
           filter(count_week==1 | count_week==26 | count_week==53|count_week==79|count_week==105|count_week==131, Country=='Liberia'), 
       aes(x = long, y = lat, group = group, fill = pre)) +
    geom_polygon(color = "black", size = 0.25) + scale_fill_gradient(name='Mean Precipitation') +
    coord_map() + facet_wrap(~Weeks)

ggplot(three_countries_map_complete %>% 
           filter(count_week==1 | count_week==26 | count_week==53|count_week==79|count_week==105|count_week==131)
       , aes(x = long, y = lat, group = group, fill = pre)) +
    geom_polygon(color = "black", size = 0.25) + scale_fill_gradient(name='Mean Precipitation') +
    geom_polygon(aes(x = long, y = lat, group = group, color="Guinea"), fill=NA, size = 0.75, data=guineashp.adm0) +
    geom_polygon(aes(x = long, y = lat, group = group, color="Sierra Leone"), fill=NA, size = 0.75, data=SLshp.adm0) +
    geom_polygon(aes(x = long, y = lat, group = group, color="Liberia"), fill=NA, size = 0.75, data=LBshp.adm0) +
    coord_map() + scale_color_discrete(name="Country")+ facet_wrap(~Weeks)

#vap per country
ggplot(three_countries_map_complete %>% 
           filter(count_week==1 | count_week==26 | count_week==53|count_week==79|count_week==105|count_week==131, Country=='Guinea'), 
       aes(x = long, y = lat, group = group, fill = vap)) +
    geom_polygon(color = "black", size = 0.25) + scale_fill_gradient(low='green', high='yellow', name='Vapour pressure in hPa') +
    coord_map() + facet_wrap(~Weeks)

ggplot(three_countries_map_complete %>% 
           filter(count_week==1 | count_week==26 | count_week==53|count_week==79|count_week==105|count_week==131, Country=='Sierra Leone'), 
       aes(x = long, y = lat, group = group, fill = vap)) +
    geom_polygon(color = "black", size = 0.25) + scale_fill_gradient(low='green', high='yellow',name='Vapour pressure in hPa') +
    coord_map() + facet_wrap(~Weeks)

ggplot(three_countries_map_complete %>% 
           filter(count_week==1 | count_week==26 | count_week==53|count_week==79|count_week==105|count_week==131, Country=='Liberia'), 
       aes(x = long, y = lat, group = group, fill = vap)) +
    geom_polygon(color = "black", size = 0.25) + scale_fill_gradient(low='green', high='yellow',name='Vapour pressure in hPa') +
    coord_map() + facet_wrap(~Weeks)

ggplot(three_countries_map_complete %>% 
           filter(count_week==1 | count_week==26 | count_week==53|count_week==79|count_week==105|count_week==131)
       , aes(x = long, y = lat, group = group, fill = vap)) +
    geom_polygon(color = "black", size = 0.25) + scale_fill_gradient(low='green', high='yellow',name='Vapour pressure in hPa') +
    geom_polygon(aes(x = long, y = lat, group = group, color="Guinea"), fill=NA, size = 0.75, data=guineashp.adm0) +
    geom_polygon(aes(x = long, y = lat, group = group, color="Sierra Leone"), fill=NA, size = 0.75, data=SLshp.adm0) +
    geom_polygon(aes(x = long, y = lat, group = group, color="Liberia"), fill=NA, size = 0.75, data=LBshp.adm0) +
    coord_map() + scale_color_discrete(name="Country")+ facet_wrap(~Weeks)





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