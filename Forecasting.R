# Import packages
library(readr)
library(DataExplorer)
library(tidyverse)
library(lubridate)
library(corrplot)

store.train <- read_csv('train.csv')
store.test <- read_csv('test.csv')

store <- bind_rows(store.train, store.test)

plot_missing(store)

# pull out weekdays
store$dayofweek <- store$date %>% weekdays()
# pull out day of month
store$dayofmonth <- store$date %>% day()
# pull out month
store$month <- store$date %>% month()
# pull out year
store$year <- store$date %>% year()

aggregate(sales ~ store + item + dayofweek + dayofmonth + month, data = store, mean)
aggregate(sales ~ dayofweek, data = store, mean)
aggregate(sales ~ dayofmonth, data = store, mean)
aggregate(sales ~ month, data = store, mean)


autoplot(store)

arrivals <- fpp2::arrivals
autoplot(arrivals)

