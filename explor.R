conakry <- filter(all_countries, Location=='CONAKRY' & count_week>52) %>% select(count_week, Total_cases, Cum_cases)
library(splines)
f <- ns(conakry$count_week, df=7)

fg <- lm(Total_cases ~ f, data=conakry)
plot(fg$fitted.values)

ggplot() + geom_line(aes(x=conakry$count_week, y=fg$fitted.values), color='red') + 
    geom_point(aes(x=count_week, y=Total_cases), color='blue', data=conakry)

plot(fg$residuals)


ggplot() + geom_line(aes(x=conakry$count_week, y=fg$fitted.values), color='red') + 
    geom_point(aes(x=count_week, y=Total_cases), color='blue', data=conakry)


ggplot(conakry, aes(x=count_week, y=Total_cases)) + geom_point() + geom_smooth()

guinea <- filter(all_countries, Country=='Guinea') %>% select(Location, count_week, Total_cases)
guinea.wide <- spread(guinea, Location, Total_cases)
a <- names(colSums(guinea.wide)<1)

cor.matrix <- cor(select(guinea.wide, -count_week, -DINGUIRAY, -GAOUAL, -KOUBIA, -KOUNDARA, -LABE, -LELOUMA, -MAMOU, -MANDIANA,-NZEREKORE))

library(corrplot)
corrplot(cor.matrix)
