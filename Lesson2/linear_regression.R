library(tidyverse)

#Quick Question
wine <- read.csv("wine.csv")
str(wine)

summary(wine)

model1 <- lm(Price ~ AGST, data=wine)
summary(model1)


model2 <- lm(Price ~ AGST + HarvestRain, data=wine)
summary(model2)


model3 <-lm(Price~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine)
summary(model3)

model4 <-lm(Price~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model4)

### Making Predictions

wineTest <- read.csv("wine_test.csv")
str(wineTest)

predictTest <- predict(model4,newdata=wineTest)
predictTest

SSE = sum((wineTest$Price - predictTest)^2)

SST = sum((wineTest$Price - mean(wine$Price))^2)

#### Baseball Stats
baseball <- read.csv("baseball.csv")
str(baseball)
moneyball <- subset(baseball, Year<2002)
str(moneyball)

#RS - runs scored 
#RA - runs allowed
#A player is awarded a run if he crosses the plate to score his team a run

moneyball$RD <- moneyball$RS - moneyball$RA

plot(moneyball$RD,moneyball$W)

WinsReg <- lm(W ~ RD, data = moneyball)
summary(WinsReg)

#W = 80.8814 + 0.1058(RD)
#W >= 95

#RD >= 95-80.8814/0.1058 = 133.4
#http://www-personal.umd.umich.edu/~acfoos/Courses/381/06%20-%20Regression.pdf


