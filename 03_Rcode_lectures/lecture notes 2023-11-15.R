#R script lecture file ``2023-11-15"

library(tidyverse)
install.packages("rlist")
install.packages("reshape")
install.packages("nhstplot")
library(rlist)
library(reshape)
library(nhstplot)

chacobo.mcf.data <- read.csv("/Users/Adan Tallman/Desktop/StatisticsinLinguistics_FSU_2023_2024/07_data/chacobo.mcf.df.csv")

head(chacobo.mcf.data)

rt <- chacobo.mcf.data$reactionTime

rt.sd <- sqrt((sum((rt - mean(rt))^2) / (length(rt)-1)))

format(rt.sd, scientific=FALSE)

sd(rt)

rt.sd^2

var(rt)


##Uniform distribution

set.seed(123)
runif(1, min=0, max=1)

x <- seq(-4, 4, length=100)
y2 <- seq(-3,3, length=100)

set.seed(123)
y <- runif(100, min=-3, max=3)

plot(x,y)

y <- dunif(x, min=-3, max=3)

plot(x,y, type='l')

##The normal distribution
#see lecture notes

##The binomial distribution

set.seed(123)

rbinom(10,10,0.1)

trials <- rbinom(100, 10, 0.5)
trials
hist(trials, breaks=6)

dbinom(100,10, 0.5)

set.seed(123)

success.numbers <- 0:100
success.numbers
plot(success.numbers, dbinom(success.numbers, size=10, prob=.5), type='h', xlim=c(0,10))
