library(rvest)
library(dplyr)
library(stringr)
library(readr)


#Scraping population data for Guinea

h <- read_html('http://www.citypopulation.de/php/guinea-admin.php')
guinea.population <- h %>% html_nodes(".rname .prio1") %>% html_text()
guinea.population <- str_replace_all(guinea.population, ',',"")
guinea_names <- read_csv("guinea_weekly_cases_climate.csv") %>% select(Location) %>%
                unique()
guinea.population <-cbind(guinea_names, Population=guinea.population)
save(guinea.population, file="guinea.population") 

#Scraping population data for Sierra Leone
i <- read_html('http://www.citypopulation.de/SierraLeone.html')
SL.population <- i %>% html_nodes(".adm+ tbody .prio1") %>% html_text()
SL.population <- str_replace_all(SL.population, ',',"")
SL.population <-cbind(Location=c('KAILAHUN', "KENEMA","KONO", "BOMBALI","KAMBIA","KOINADUGU","PORTLOKO","TONKOLILI",
                                 "BO", "BONTHE", "MOYAMBA", "PUJEHUN", "WESTERNURBAN", "WESTERNRURAL"), Population=SL.population)
save(SL.population, file="SL.population")

#Scraping population data for Liberia

j <- read_html('http://www.citypopulation.de/Liberia.html')
LB.population <- j %>% html_nodes("#tl td.prio1") %>% html_text()
LB.population <- str_replace_all(LB.population, ',',"")
LB_names <- read_csv("Liberia_weekly_cases_climate.csv") %>% select(Location) %>%
    unique()
LB.population <-cbind(Location=LB_names, Population=LB.population)
save(LB.population, file="LB.population")
