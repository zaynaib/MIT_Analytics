library(tidyverse)
library(lubridate)

IBM <- read_csv("IBMStock.csv")
GE <- read_csv("GEStock.csv")
Boeing <- read_csv("BoeingStock.csv")
ProcterGamble <- read_csv("ProcterGambleStock.csv")
CocaCola <- read_csv("CocaColaStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")

GE$Date = as.Date(GE$Date, "%m/%d/%y")

CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")

ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")

Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

#'TO DO
#' 1. combine datasets together - bind_rows() 
#' 2. put in long format
#' 

GE$company <- rep("GE",480) 
IBM$company <- rep("IBM",480) 
CocaCola$company <- rep("CocaCola",480) 
ProcterGamble$company <- rep("PG",480) 
Boeing$company <- rep("BoeingPrice",480) 

tidyStocks <- bind_rows(GE,IBM,CocaCola,ProcterGamble,Boeing)
tidyStocks %>% View()

tidyStocksWide <- tidyStocks %>% pivot_wider(names_from = company, values_from = StockPrice)

#problem 1.1
nrow(GE)

#problem 1.2
min(GE$Date)

#problem 1.3
max(GE$Date)

#problem 1.4
mean(IBM$StockPrice)

#problem 1.5
min(GE$StockPrice)

#problem 1.6
max(CocaCola$StockPrice)

#problem 1.7
median(Boeing$StockPrice)

#problem 1.8
sd(ProcterGamble$StockPrice)

#problem 2.1

theme_set(theme_minimal())
ggplot(CocaCola,aes(x=Date, y=StockPrice)) + geom_line()


#problem 2.2

tidyStocks %>% filter(company %in% c('PG','CocaCola')) %>%
  ggplot(aes(x=Date, y=StockPrice)) +
  geom_line(aes(color=company))

#proctor and gamble


#problem 2.3

tidyStocks %>% filter(company %in% c('PG','CocaCola')) %>%
  ggplot(aes(x=Date, y=StockPrice)) +
  geom_line(aes(color=company))


#problem 3.1
#extract year
#filter between the years

tidyStocks %>% filter(year(Date) == 2000) %>%
  ggplot(aes(x = Date, y=StockPrice)) +
  geom_line(aes(color=company, linetype = company))

#General Electric


#problem 3.2
tidyStocks %>% filter(year(Date) > 1994 & year(Date) < 2006) %>%
  ggplot(aes(x = Date, y=StockPrice)) +
  geom_line(aes(color=company))

#IBM

#problem 3.3

tidyStocks %>% filter(year(Date)  == 1997) %>%
  ggplot(aes(x = Date, y=StockPrice)) +
  geom_line(aes(color=company))

#problem 3.4

tidyStocks %>% filter(year(Date)  >= 2004 & year(Date) <= 2005) %>%
  ggplot(aes(x = Date, y=StockPrice)) +
  geom_line(aes(color=company))

#Boeing

#problem 4.1

#problem 4.2

#problem 4.3

