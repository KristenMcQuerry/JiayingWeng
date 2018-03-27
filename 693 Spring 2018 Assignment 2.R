##install packages
install.packages("tidyverse")
install.packages("nycflights13")
#install.packages("hflights")

## load libraries
library(tidyverse)
library(nycflights13)
#library(hflights)
library(ggplot2)

#(3)
## the number of cancelled flights per day.
glimpse(Cancel <- flights %>% 
          group_by(year, month, day) %>% 
          summarise(NoCancel = sum(is.na(dep_delay))))

ggplot(data = Cancel, aes(x=1:365, y=NoCancel)) + geom_point()

## Q3 Comments:
## The number of cancelled flights change seasonal. During the January and February, 
## there are large amount of cancelled flights maybe because of school beginning. During the summer time,
## there are more frequently have hundreds of cancelled flights. At the end of year, there are several days between 
## Christmas have large cancelled flights.

## the proportion of cancelled flights related to the average delay
glimpse(cancel_delay <- flights %>% 
          group_by(year, month, day) %>% 
          summarise(pct_cancel = mean(is.na(dep_delay)), 
                    ave_delay = mean(dep_delay,na.rm = T)))

ggplot(data = cancel_delay, aes(x=ave_delay, y=pct_cancel)) + geom_point()

## Q4 Comments:
## Except two outliers located in the upper left, the majority of points have similar trend:
## the higher the average delay, the higher the proportion of cancelled flights. 


#(4)
## Which carrier has the worst delays
glimpse(carrier <- flights %>% 
          group_by(carrier) %>% 
          summarise(nodelay = sum(dep_delay>0, na.rm = T),
                    avedelay = mean(dep_delay[dep_delay>0],na.rm=T)) %>% 
          arrange(desc(avedelay)))

## United Air Lines Inc. has the most number of delay flights, 
## while SkyWest Airlines Inc. has the longest average delay time.

## the effects of bad airports versus bad carriers
glimpse(airp_dest <- flights %>% 
          group_by(carrier, dest) %>% 
          summarise(prop_delay = mean(dep_delay>0, na.rm = T)
                    ))





##(5)
## For each plane, count the number of flights before the first delay of greater than 1 hour.

glimpse(nodelay <- flights %>% 
          group_by(tailnum) %>% 
          summarise(NOdelay = ifelse(min(which(dep_delay>60),na.rm=T)==Inf, 
                                     n(),
                                     min(which(dep_delay>60),na.rm=T)-1)))
##Remark: the warnings because of calculating "minimum" of non valus.

##(6)
## Which plane (tailnum) has the worst on-time record?
glimpse(nodelay <- flights %>% 
          group_by(tailnum) %>% 
          summarise(n_delay = sum(dep_delay>0, na.rm = T),
            Prop_delay = mean(dep_delay[dep_delay>0],na.rm = T)) %>% 
          arrange(desc(n_delay)))

## Q6 Comments:
## On average, flight "N258JB" has the most number of delay with about 43 mins late on average.

## (7)
## What time of day should you fly if you want to avoid delays as much as possible
glimpse(hour_delay <- flights %>% 
          group_by(hour) %>% 
          summarise(nodelay = sum(dep_delay>0, na.rm = T)) %>% 
          arrange(desc(nodelay)))

## Q7 Comments:
## We should avoid the flight in 5pm whih has the most frequency flights delay.

## (8)
##For each destination, compute the total minutes of delay.
glimpse(dest_delay <- flights %>% 
          group_by(dest) %>% 
          summarise(delay = sum(dep_delay[dep_delay>0], na.rm = T)))

## For each flight, compute the proportion of the total delay for its destination.
glimpse(total_delay <- flights %>% 
          group_by(dest) %>% 
          mutate(totaldelay = sum(dep_delay[dep_delay>0], na.rm = T)))

glimpse(flight_delay <- total_delay %>% 
          group_by(tailnum, dest) %>% 
          summarise(nodelay = sum(dep_delay[dep_delay>0], na.rm=T)/mean(totaldelay))
        )

## (9)
## Look at each destination. Can you find flights that are suspiciously fast? (That is,
## flights that represent a potential data entry error.)
glimpse(error <- flights %>% 
          group_by(dest) %>% 
          summarise(min_time = min(air_time,na.rm = T)) %>% 
          arrange(min_time))

## There are some flights  with 20 mins to 30 mins air time, which are suspiciously fast.
## It takes about 20 mins to land off and land on. 

## which flights were most delayed in the air?
glimpse(air_delay <- flights %>% 
          mutate(Air = sched_arr_time-sched_dep_time) %>% 
          group_by(tailnum) %>% 
          summarise(air_lay = mean(air_time-Air,na.rm=T)) %>% 
          arrange(desc(air_lay)))

## "N657UA" flight has the biggest average delayed time in the air. 