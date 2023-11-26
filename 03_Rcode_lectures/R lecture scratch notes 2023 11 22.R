## R lecture scratch notes

library(nhstplot)

set.seed(1234)
n <- 30

vowel.durations <- rnorm(n, mean = 80, sd=10)

h1 <- c("strong","weak")
h1 <- sample(h1, n, replace=TRUE, prob=c(0.5, 0.5))
h2 <- c("strong","weak")
h2 <- sample(h2, n, replace=TRUE, prob=c(0.5, 0.5))
h3 <- c("strong","weak")
h3 <- sample(h3, n, replace=TRUE, prob=c(0.5, 0.5))
h4 <- c("strong","weak")
h4 <- sample(h4, n, replace=TRUE, prob=c(0.5, 0.5))
h5 <- c("strong","weak")
h5 <- sample(h5, n, replace=TRUE, prob=c(0.5, 0.5))
h6 <- c("strong","weak")
h6 <- sample(h6, n, replace=TRUE, prob=c(0.5, 0.5))

df <- data.frame(vowel.durations,h1,h2,h3,h4,h5,h6)
h <- c("strong","weak")
h <- sample(h, n, replace=TRUE, prob=c(0.5, 0.5))
plotttest(t.test(vowel.durations~h4, data=df))

##Plotting variables

x <- seq(from=-1, to=1, by =0.1)
x
a <- 0
b <- 2
y <- a + b*x^2+(b+4)*x^3
plot(x,y, type="l", lty=1)

##Load in the latent response times database from Rling
ldt <- read.csv("/Users/Adan Tallman/Desktop/ldt.csv")

##Load tidyverse
library(tidyverse)

##Looking at data
head(ldt)
glimpse(ldt)

summary(ldt)

plot(density(ldt$Freq))






