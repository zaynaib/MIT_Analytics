#Load Library

library(tidyverse)
WHO <- read_csv('WHO.csv')
glimpse(WHO)
head(WHO)

WHO$FertilityRate <- as.double(WHO$FertilityRate)
glimpse(WHO)

WHO_clean <- WHO %>% mutate(across(LiteracyRate:PrimarySchoolEnrollmentFemale, ~as.double((.))))

WHO_clean <- WHO_clean %>% mutate(across(Country:Region, ~as.factor((.))))
glimpse(WHO_clean)

summarize(WHO_clean)

WHO_Europe <- WHO_clean %>%
  filter(Region == "Europe")

write.csv(WHO_Europe, "WHO_Europe.csv")

glimpse(WHO_Europe)

rm(WHO_Europe)

mean(WHO$Under15)

sd(WHO$Under15)

summary(WHO$Under15)

which.min(WHO$Under15)

WHO$Under15[which.min(WHO$Under15)]

WHO$Country[86]

WHO$Country[which.max(WHO$Under15)]

WHO_clean %>%
  ggplot(aes(x=GNI, y=FertilityRate,color=Region)) +geom_point()

Outliers <- WHO_clean %>%
  filter(GNI >10000 & FertilityRate > 2.5)

nrow(Outliers)

### Quick Question Exercises

mean(WHO_clean$Over60)

WHO_clean[which.min(WHO_clean$Over60),]

WHO_clean[which.max(WHO_clean$LiteracyRate),]

table(WHO$Region)

tapply(WHO_clean$Over60, WHO_clean$Region,mean)

tapply(WHO_clean$LiteracyRate, WHO_clean$Region,min,na.rm= TRUE)
