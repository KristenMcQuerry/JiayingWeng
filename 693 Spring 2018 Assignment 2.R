##install packages
install.packages("tidyverse")
install.packages("nycflights13")
install.packages("hflights")

## load libraries
library(tidyverse)
library(nycflights13)
library(hflights)
library(ggplot2)

#(3)
## the number of cancelled flights per day.
glimpse(Cancel <- hflights %>% 
          group_by(Year, Month, DayofMonth) %>% 
          summarise(NoCancel = sum(Cancelled)))
ggplot(data = Cancel, aes(x=1:365, y=NoCancel)) + geom_point()

## The cancelled flights per day at most is one. 
## The days with cancelled flights and without cancelled flights occur interactively. 

## the proportion of cancelled flights related to the average delay
glimpse(cancel_delay <- hflights %>% 
          group_by(Year, Month, DayofMonth) %>% 
          summarise(pct_cancel = mean(Cancelled), ave_delay = mean(DepDelay,na.rm = T)))

ggplot(data = cancel_delay, aes(x=ave_delay, y=pct_cancel)) + geom_point()

#(4)
## Which carrier has the worst delays
glimpse(carrier <- flights %>% 
          group_by(carrier) %>% 
          summarise(nodelay = sum(dep_delay<0, na.rm = T)))

glimpse(airp_dest <- flights %>% 
          group_by(carrier, dest) %>% 
          summarise(n()))

##(5)
## For each plane, count the number of flights before the first delay of greater than 1 hour.
glimpse(nodelay <- flights %>% 
          group_by(tailnum) %>% 
          summarise(Nodelay = sum(dep_delay>60,na.rm = T)))

##(6)
## Which plane (tailnum) has the worst on-time record?
glimpse(nodelay %>% 
          arrange(desc(Nodelay)))


## (7)
## What time of day should you fly if you want to avoid delays as much as possible
glimpse(hour_delay <- flights %>% 
          group_by(hour) %>% 
          summarise(nodelay = sum(dep_delay<0, na.rm = T)) %>% 
          arrange(nodelay))

## (8)
##For each destination, compute the total minutes of delay.
glimpse(dest_delay <- flights %>% 
          group_by(dest) %>% 
          summarise(delay = sum(dep_delay, na.rm = T)))

## For each flight, compute the proportion of the total delay for its destination.
glimpse(flight_delay <- flights %>% 
          group_by(tailnum) %>% 
          summarise(nodelay = mean(dep_delay>0, na.rm=T))
          )

## (9)
## Look at each destination. Can you find flights that are suspiciously fast? (That is,
## flights that represent a potential data entry error.)
glimpse(error <- flights %>% 
          group_by(dest) %>% 
          summarise(min_time = min(air_time,na.rm = T)))

## which flights were most delayed in the air?
glimpse(air_delay <- flights %>% 
          mutate(Air = sched_arr_time-sched_dep_time) %>% 
          group_by(tailnum) %>% 
          summarise(air_lay = mean(air_time-Air,na.rm=T)) %>% 
          arrange(desc(air_lay)))
