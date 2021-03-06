##install packages
install.packages("tidyverse")
install.packages("nycflights13")

## load libraries
library(tidyverse)
library(nycflights13)

## dataset for assignment
flights
colnames(flights)

## Arrival delay of two or more hours
flights %>% 
  filter(arr_delay >= 120)

## Flew to Houston
flights %>% 
  filter(dest=="IAH" | dest=="HOU")

## Were operated by United, American or Delta
flights %>% 
  filter(carrier=="UA" | carrier=="AA" | carrier=="DL")

## Departed in summer (July, August, and September)
flights %>% 
  filter(month == 7 | month == 8 | month == 9)

## Arrived more than two hours late, but didn't leave late
flights %>% 
  filter(arr_delay >= 120  & dep_delay <= 0)

## Were delayed by at least an hour, but made up over 30 minutes in flight
flights %>% 
  mutate(makeup = dep_delay-arr_delay) %>% 
  filter(dep_delay >= 60 & makeup > -30)#dep_delay-arr_delay>30

## Departed between midnight and 6 a.m. (inclusive)
flights %>% 
  filter(dep_delay==2400 & dep_time<600)

## Sort to find the most delayed flights. Find the flights that left earliest.
flights %>% 
  arrange(desc(dep_delay))

flights %>% 
  arrange(dep_delay)

## Sort to find the fastest flights
flights %>% 
  mutate(speed = distance/air_time) %>% 
  arrange(desc(air_time))#dist/air_time

## Which flights traveled the longest? Which traveled the shortest?
flights %>% 
  arrange(desc(distance))

flights %>% 
  arrange(distance)


  