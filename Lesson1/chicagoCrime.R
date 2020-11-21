setwd("C:/Users/mutiu/Desktop/project/rPractice/MIT_Analytics/Lesson1")

library(dplyr)

mvt <- read.csv("mvtWeek1.csv")

#How many rows of data (observations) are in the dataset?

nrow(mvt)


#how many variables in this dataset
ncol(mvt)

#Using the max function what is the maxium value of the variable ID?
mvt$ID[which.max(mvt$ID)] #base R

maxID <- mvt %>% filter(ID == max(ID))#Dpylr
maxID$ID


#Using the min function what is the minium value of the variable Beat?
mvt$Beat[which.min(mvt$Beat)] #base R

minBeat <- mvt %>% filter(Beat == min(Beat))#Beat
minBeat$Beat

#How many observations have value TRUE in the Arrest variable (this is the number of crimes for which an arrest was made)?

#BaseR
summary(mvt$Arrest)

#dpylr
mvt %>% filter(Arrest == TRUE) %>% count(Arrest)


#How many observations have a LocationDescription value of ALLEY?

#BaseR
alley <- subset(mvt, LocationDescription == 'ALLEY')
nrow(alley)

#or
table(alley$LocationDescription)

#or

table(mvt$LocationDescription[mvt$LocationDescription =='ALLEY'])

#dpylr
mvt %>% filter(LocationDescription =='ALLEY') %>% count(LocationDescription)

#In what format are the entries in the variable Date?
mvt$Date[1]
#month/day/year

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))

#What is the month and year of the median date in our dataset? 
median(DateConvert)

mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert


#In which month did the fewest motor vehicle thefts occur?
table(mvt$Month)

mvt %>%
  group_by(Month)%>%
  count(Arrest)%>%
  summarize(true_count = sum(n)) 

#On which weekday did the most motor vehicle thefts occur?
table(mvt$Weekday)


#Which month has the largest number of motor vehicle thefts for which an arrest was made?  
#2-way crost table to get frequency
table(mvt$Arrest,mvt$Month)

mvt %>% 
  filter(Arrest ==TRUE)%>%
  group_by(Month) %>% 
  count(Arrest)
library(ggplot2)



ggplot(mvt, aes(x=Date)) + 
  geom_histogram(binwidth=100)


table(mvt$Arrest, mvt$Year)
2152/(2152 + 18517)

1212/(13068 + 1212)  




#Which locations are the top five locations for motor vehicle thefts, excluding the "Other" category? 
#You should select 5 of the following options.

sort(table(mvt$LocationDescription),decreasing = TRUE)

mvt %>% group_by(LocationDescription) %>% count(Arrest) %>%   summarize(true_count = sum(n))%>% arrange(desc(true_count))

