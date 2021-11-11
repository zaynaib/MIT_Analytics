library(tidyverse)
cps <- read_csv("CPSData.csv")
glimpse(cps)

cps1 <-read.csv("CPSData.csv",stringsAsFactors = T)

#Problem 1.1

str(cps)
nrow(cps)
#131302 

#Problem 1.2
sort(table(cps$Industry),decreasing = TRUE) %>% View()
#Educational and health services 

#Problem 1.3

#sort(table(cps$Region))

sort(table(cps$State),decreasing = T)

#New Mexico
#California

#Problem 1.4

table(cps$Citizenship) %>% prop.table()
0.88 + 0.05

#Problem 1.5

hispanicSubset <- cps %>% filter(Hispanic != 0)
table(hispanicSubset$Race) %>% View()

#Problem 2.1

summary(cps) %>% View()

glimpse(cps)
#Problem 2.2
table(cps1$Region, is.na(cps1$Married))
table(cps1$Sex, is.na(cps1$Married))
table(cps1$Age, is.na(cps1$Married)) %>% View()


#Problem 2.3

cityTable <-table(cps1$State, is.na(cps1$MetroAreaCode)) %>% View()

#Problem 2.4

table(cps1$Region, is.na(cps1$MetroAreaCode))

#Problem 2.5

#https://scottishsnow.wordpress.com/2018/10/15/fell-out-tapply-love-dplyr/
#https://rstudio-pubs-static.s3.amazonaws.com/46399_ae360f3ec8644c9d9892994a12b0df8d.html

tapply(is.na(cps1$MetroAreaCode), cps1$State, mean) %>% View()


#Problem 3.1

#Problem 3.2

#Problem 3.3

#Problem 3.4

#Problem 3.4

#Problem 3.5

#Problem 3.5

#Problem 4.1

#Problem 4.2

#Problem 4.3

#Problem 4.4


