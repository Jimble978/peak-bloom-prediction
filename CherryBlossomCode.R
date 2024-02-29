library(tidyverse)

bloom <- select(read.csv('data/washingtondc.csv'),c(year, bloom_doy))

#data of cherry trees in NY
yedoNY <- read.csv('data/yedo_data.csv') %>%
  filter(State=='NY') %>%
  group_by(Mean_First_Yes_Year) %>%
  summarise(yeddoy = mean(Mean_First_Yes_DOY))
  
yedoNY <- yedoNY[which(yedoNY$Mean_First_Yes_Year!='-9999'),]

yedoNYadjust <- yedoNY
yedoNYadjust$Mean_First_Yes_Year <- yedoNYadjust$Mean_First_Yes_Year+1
colnames(yedoNYadjust) <- c('Mean_First_Yes_Year', 'lastyeddoy')

#data of eastern redbuds in NY
redNY <- read.csv('data/redbud_data.csv') %>%
  filter(State=='NY') %>%
  group_by(Mean_First_Yes_Year) %>%
  summarise(reddoy = mean(Mean_First_Yes_DOY)) 

redNY <- redNY[which(redNY$Mean_First_Yes_Year!='-9999'),]

redNYadjust <- redNY[2:13,]
redNYadjust$Mean_First_Yes_Year <- redNYadjust$Mean_First_Yes_Year+1


NY <- merge(redNYadjust, yedoNY, by='Mean_First_Yes_Year')
NY2 <- merge(NY, yedoNYadjust, by='Mean_First_Yes_Year')

NY2 <- NY2[c(1:5,8),]

#There is an outlier in the yeddoy column for 2021 so 2021 and 2022 are removed


NY.lm <- lm(yeddoy ~ reddoy+lastyeddoy, data=NY2)
summary(NY.lm)

#Predicting DC's bloom day
redDC <- read.csv('data/redbud_data.csv') %>%
  filter(State=='DC') %>%
  group_by(Mean_First_Yes_Year) %>%
  summarise(reddoy = mean(Mean_First_Yes_DOY)) 

redDC <- redDC[which(redDC$Mean_First_Yes_Year!='-9999'),]

newdata1 <- data.frame(reddoy=89.5, lastyeddoy=82)
DC_predict <- predict(NY.lm, newdata1)
DC_predict
# DC prediction: 86.37583

#Predicting NY's bloom day
newdata2 <- data.frame(reddoy=111.6111, lastyeddoy=86.0)
NY_predict <- predict(NY.lm, newdata2)
NY_predict
#NY prediction: 86.48896 

#using the averaage to predict bloom days in three other locations

van <- select(read.csv('data/vancouver.csv'), c(year, bloom_doy))
kyoto <- select(read.csv('data/kyoto.csv'), c(year, bloom_doy))
liestal <- select(read.csv('data/liestal.csv'), c(year, bloom_doy))

van2 <- merge(bloom,van, by='year')
DC_predict + mean(van2$bloom_doy.x-van2$bloom_doy.y)
#Vancouver Prediction: 75.87583

kyo2 <- merge(bloom,kyoto, by='year')
DC_predict + mean(kyo2$bloom_doy.x-kyo2$bloom_doy.y)
#Kyoto Prediction: 81.0689

lie2 <- merge(bloom,liestal, by='year')
DC_predict + mean(lie2$bloom_doy.x-lie2$bloom_doy.y)
#Liestal Prediction: 79.40496









