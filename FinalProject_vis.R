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




lexp_vs_fer_every15_anim <- ggplot(data=filter(complete1, year%in%every15 & region!='Other'), aes(color=region, size=population/1000000, x=fertility, y=life_expectancy, frame=year)) + geom_point() + labs(size="Population\nIn Millions", color="OPEC or OECD\nCountries", shape='Continent') + xlab('Fertility rate') + ylab('Life Expectancy in years') + ggtitle('Fertility rate vs. Life Expectancy')

gg_animate(lexp_vs_fer_every15_anim)




conakry_cases <- read.csv("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Cases/guinea_cases_long.csv")
conakry_cases <- filter(conakry_cases, Location=="CONAKRY" & Source=='Patient database' & Case_definition=='Confirmed')
conakry_cases$week_number <- 1:length(conakry_cases$Epi_Week)
guinea_monthly <- read.csv("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/guinea_monthly_district.csv")

Guinea <- get_map(location = c(lon = -11.15, lat = 9.950287),
                  color = "color",
                  source = "google",
                  maptype = "satellite",
                  zoom = 6, scale=2)


guineards <- readRDS("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Guinea-Admin/GIN_adm2.rds")
guineashp.df <- fortify(guineards)
tmp <- filter(guinea_monthly, date=="2011-01-01" & measurement=='tmp')
data <- as.data.frame(cbind(NAME_2=guineards@data$NAME_2, id=guineards@data$ID_2, tmp=tmp$Value))
data <- left_join(guineashp.df, data, by='id')
data$tmp <- as.numeric(as.character((data$tmp)))

ggplot() +
    geom_polygon(data = filter(data, NAME_2=='Conakry'), 
                 aes(x = long, y = lat, group = group, fill = tmp), 
                 color = "black", size = 0.25) + 
    coord_map()





ggmap(Guinea, extent='normal') +
    scale_x_continuous(limits = c(-15.5, -7.5), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(7, 13.5), expand = c(0, 0)) +
    geom_polygon(aes(x = long,
                     y = lat,
                     group=group,
                     color=id), 
                 size=1,
                 data=guineashp.df, fill=tmp$Value)