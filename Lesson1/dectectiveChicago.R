library(tidyverse)

crime <- read_csv("mvtWeek1.csv")
glimpse(crime)

#How many rows of data(observations) are in this dataset?
nrow(crime)
#191641

#How many variables are in this dataset?
ncol(crime)
#11

#Using the "max" function, what is the maximum value of the variable "ID"?
crime[which.max(crime$ID),]$ID
#9181151

#What is the minimum value of the variable "Beat"?

crime[which.min(crime$Beat),]$Beat
#111

#How many observations have value TRUE in the Arrest variable (this is the number of crimes for which an arrest was made)?
table(crime$Arrest) 
 #15536

#How many observations have a LocationDescription value of ALLEY?
alley <- crime %>%
  filter(LocationDescription == 'ALLEY')

nrow(alley)
#2308


#In what format are the entries in the variable Date?
#Month/Day/Year Hour:Minute

#problem 2.2
DateConvert <- as.Date(strptime(crime$Date, "%m/%d/%y %H:%M"))
median(DateConvert, na.rm = TRUE)

#problem 2.3 In which month did the fewest motor vehicle thefts occur?

crime$Month <- months(DateConvert)
crime$Weekday <-weekdays(DateConvert)

table(crime$Month)

#problem 2.4 In which weekday did the most motor vehicle thefts occur?

table(crime$Weekday)

#Friday

#problem 2.5 Which month has the largest number of motor vehicle thefts for which an arrest was made

table(crime$Arrest,crime$Month)
#January  
  
  
#problem 3.1 In general, does it look like crime increases or decreases from 2002-2012?
hist(DateConvert, breaks = 100)

#problem 3.2
#Does it look like there were more crimes for which arrests were made in the first half of the time period or the second half of the time period?

crime$Date <- DateConvert
boxplot(Date~Arrest, data=crime)  

#problem 3.3 For what proportion of motor vehicle thefts in 2001 was an arrest made?

#https://www.r-bloggers.com/2018/07/prop-table/

crime %>% filter(Year == 2001 ) %>%
  with(table(Arrest)) %>% prop.table()

#problem 3.4
#For what proportion of motor vehicle thefts in 2007 was an arrest made?

crime %>% filter(Year == 2007 ) %>%
  with(table(Arrest)) %>% prop.table()


#problem 3.5 For what proportion of motor vehicle thefts in 2012 was an arrest made?

crime %>% filter(Year == 2012) %>%
  with(table(Arrest)) %>% prop.table()


#problem 4.1 Which locations are the top five locations for motor vehicle thefts, excluding the "Other" category? 

sort(table(crime$LocationDescription), decreasing = TRUE) %>% View()

#problem 4.2 How many observations are in Top5?
#working with tables https://bookdown.org/kdonovan125/ibis_data_analysis_r4/working-with-tables-in-r.html
#https://stackoverflow.com/questions/25647470/filter-multiple-values-on-a-string-column-in-dplyr

sort(table(crime$LocationDescription), decreasing = TRUE)[0:5]

topLocations = c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY", "GAS STATION", "DRIVEWAY - RESIDENTIAL")

top_5 <- crime %>% filter(LocationDescription %in% topLocations)

top_5 %>% count()

#177510


#problem 4.3

top_5$LocationDescription <- factor(top_5$LocationDescription)


top_5 %>% with(table(LocationDescription,Arrest)) %>% prop.table()


#with(table(Arrest)) %>% prop.table()

#problem 4.4

top_5 %>% filter(LocationDescription == "GAS STATION") %>%
  group_by(Weekday,Arrest) %>% count() 

#problem 4.5

