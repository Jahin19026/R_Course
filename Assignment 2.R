###Task:
#Create simple bar chart of average income inequality of 2016 for different continents separated by the human development group.
#Create multiple line chart of yearly median income inequality grouped by hemisphere.
#Put them together in a same plot using a common title.
library(tidyverse)
library(ggpubr)

dt<- read.csv("Income_Inequality_Data.csv")

dt1<- dt %>%
  group_by(Continent, Human.Development.Groups) %>% 
  na.omit() %>% 
  mutate(Human.Development.Groups=as.factor(Human.Development.Groups)) %>% 
  summarise(Inequality.in.income.2016= mean(Inequality.in.income.2016, na.rm=T)) %>% 
  ggplot(aes(x= Continent, y= Inequality.in.income.2016 , fill = Human.Development.Groups))+
  geom_bar(stat = "identity" ,position = "dodge")+
  ylim(0,60)+
  labs(title = "Average of Income Inequality 2016 for Continent 
       and Human development groups",y = "Avg. Income Inequality of 2016")+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom" , strip.text.y.right = element_text(angle = 0))+
  guides(fill= guide_legend(title = ""))+
  facet_grid(Continent~Human.Development.Groups)



dt2<- dt %>% 
  select(-c("Country", "Continent", "Human.Development.Groups", "UNDP.Developing.Regions", "HDI.Rank.2021")) %>% 
  gather(key = "Years", value ="Income_Inequality", -c("Hemisphere")) %>%
  mutate(Years = as.numeric(str_replace(Years, "Inequality.in.income.", ""))) %>% 
  group_by(Hemisphere, Years) %>% 
  summarise(Income_Inequality = median(Income_Inequality, na.rm= T)) %>% 
  ggplot(aes(x= Years, y= Income_Inequality, col= Hemisphere))+
  geom_line()+
  scale_x_continuous(breaks = seq(2010,2021))+
  scale_y_continuous(breaks = c(15:40))+
  labs(title = "Line Chart of Median Income Inequality")+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")+
  guides(fill = guide_legend(title = ""))+
  geom_point()

dt3 <- ggarrange(dt1, dt2, nrow = 2)
annotate_figure(dt3, top =text_grob("Bar Chart of 2016 and Yearly Line Chart"))

  