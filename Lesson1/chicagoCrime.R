setwd('C:\Users\mutiu\Desktop\project\rPractice\MIT_Analytics\Lesson1')

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
median(DateConvert)

mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert

table(mvt$Month)
