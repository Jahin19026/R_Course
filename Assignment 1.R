#####task 1#####

dt <- read.csv("Income_Inequality_Data.csv")

library(tidyverse)

unique(dt$Human.Development.Groups)
table(dt$Human.Development.Groups)
  
dt %>%  
  group_by(Human.Development.Groups) %>% 
  summarise(freq = n())

#####task 2#####
dt %>% 
  group_by(Continent) %>% 
  summarise(mean = mean(Inequality.in.income.2020, na.rm = T))



######task 3####


dt %>% 
  group_by(Hemisphere) %>% 
  summarise(median = median(Inequality.in.income.2013,na.rm = T))



df <- dt %>% 
  group_by(Hemisphere) %>% 
  mutate(Inequality.in.income.2013 = ifelse(is.na(Inequality.in.income.2013),
                                            median(Inequality.in.income.2013,na.rm = T),
                                            Inequality.in.income.2013))
write.csv(df, "Data Frame .csv")

