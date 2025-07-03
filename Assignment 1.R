#####task 1#####
####How many countries fall into each Human Development Group based on the dataset

dt <- read.csv("Income_Inequality_Data.csv")

library(tidyverse)

unique(dt$Human.Development.Groups)
table(dt$Human.Development.Groups)
  
dt %>%  
  group_by(Human.Development.Groups) %>% 
  summarise(freq = n())

#####task 2#####
###What is the average income inequality in 2020 by continent
dt %>% 
  group_by(Continent) %>% 
  summarise(mean = mean(Inequality.in.income.2020, na.rm = T))



######task 3####
###What is the median income inequality in 2013 by hemisphere

dt %>% 
  group_by(Hemisphere) %>% 
  summarise(median = median(Inequality.in.income.2013,na.rm = T))




