#2023-11-08 Lecture notes descriptive statistics
##Reading in data and visualizing it
nettle <- read.csv("/Users/Adan Tallman/Desktop/StatisticsinLinguistics_FSU_2023_2024/07_data/nettle_1999_climate.csv")

head(nettle)
tail(nettle)
nettle[10:15,]
summary(nettle)

##Installing packages
install.packages("tidyverse")
library(tidyverse)

glimpse(nettle)

plot(nettle$Langs~nettle$Population)

attach(nettle)

plot(Langs~Population, xlab="Population in millions", ylab = "Languages")

plot(Population~Area)

plot(Population)

vowels <- read.csv("/Users/Adan Tallman/Desktop/StatisticsinLinguistics_FSU_2023_2024/07_data/vowels2.csv")

plot(vowels$duration~vowels$intensity)

##Making a histogram

hist(nettle$Country)

hist(nettle$Langs, col="darkgreen", main="Languages", breaks=20)

##Types of distributions

hist(vowels$f0peak, breaks=20)

##Simulating distributions

rnorm(10, 3, 2)

simulated_data_1 <- rnorm(100000, m= 4,sd= 5)

hist(simulated_data_1, col="pink", breaks=100, xlim=c(-20,20))

simulated_data_2 <- runif(100000, min=-10, max=10)

hist(simulated_data_2, col="pink", breaks=100, xlim=c(-20,20))

weights <- rnorm(100, 50, 10)

weights

hist(weights)

boxplot(weights)

quantile(weights)

head(vowels)


boxplot(vowels$duration~vowels$stress)


weights <- rnorm(1000, 50, 2)

hist(weights)

d <- density(weights)

plot(d)

numbers <- runif(10000, min=45, max=55)

hist(numbers)

d_2 <- density(numbers) 

plot(d_2)





