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

conakry_monthly <- read.csv("D:/Google Drive/Medicina/MPH/Courses/BIO 260/FinalProjBIO260_2/Weather_data/Conakry_monthly.csv")
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
data <- as.data.frame(cbind(NAME_2=guineards@data$NAME_2, id=guineards@data$ID_2, tmp=tmp$Value))
data <- left_join(guineashp.df, data, by='id')
data$tmp <- as.numeric(as.character((data$tmp)))

ggplot() +
    geom_polygon(data = data, 
                 aes(x = long, y = lat, group = group, fill = tmp), 
                 color = "black", size = 0.25) + 
    coord_map()



tmp <- filter(guinea_monthly, date=="2011-01-01" & measurement=='tmp')

ggmap(Guinea, extent='normal') +
    scale_x_continuous(limits = c(-15.5, -7.5), expand = c(0, 0)) + 
    scale_y_continuous(limits = c(7, 13.5), expand = c(0, 0)) +
    geom_polygon(aes(x = long,
                     y = lat,
                     group=group,
                     color=id), 
                 size=1,
                 data=guineashp.df, fill=tmp$Value)