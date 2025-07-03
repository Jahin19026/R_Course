#####Task 1:
##Using mtcars dataset, analyse the given research objective and interpret the output in the word file.
##"Is there any significant impact of hp (horse power) on mpg (miles per gallon)?"
###Task 2:
###Using iris dataset, analyse the given research objective and interpret the output in the word file.
##"Find out the mean difference of Sepal.Length among different Species."


library(car)
data <-mtcars

t <- cor.test(data$mpg, data$hp)
#there is a significant negatively strong relationship between hp and mpg

model1 <- lm(mpg~hp, data=data)
summary(model1)
#the r-squared value 0.60 shows that total variation of mpg, 60% variation can be explained by hp
#Here, the regression coefficient of hp is -0.068 indicates that for per unit increment of hp, the petal length will be decreased by -0.068 units on average.
#here, the intercept value 30.09 indicates that if the value of hp is zero than the average value of mpg will be 30.09


unique(data1$Species)
str(data1)
data1 <- iris
shapiro.test(data1$Sepal.Length[data1$Species=="setosa"]) #not_normal
shapiro.test(data1$Sepal.Length[data1$Species=="versicolor"]) #not_normal
shapiro.test(data1$Sepal.Length[data1$Species=="virginica"]) #not_normal

car::leveneTest(Sepal.Length~Species, data=data1)
model2 <- aov(Sepal.Length~Species, data=data1)
summary(model2)

#Since the p-value is much less than 0.05, we reject the null hypothesis and conclude that there are significant differences in mean Sepal.Length among the species
TukeyHSD(model2)
#Setosa vs Versicolor: Mean Difference = 0.93, p < 0.05
#Setosa vs Virginica: Mean Difference = 1.582, p < 0.05
#Versicolor vs Virginica: Mean Difference = 0.652, p < 0.05
#All pairs showed statistically significant differences in their mean Sepal.Length



