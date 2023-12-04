#R in lecture scratch notes 2023-11-29
##Topic: Linear models, ANOVA, chi-squared test

x <- seq(from=1, to=20)
b <- 1
a <- 0
e <- rnorm(m=0,sd=5, n=20)
y <- a + b*x + e
plot(y~x)+abline(y,x)

data(ldt)

model_01 <- lm(Mean_RT~Length, data=ldt)

summary(model_01)


library(tidyverse)
library(lattice)
library(Rling)
library(languageR)
library(nhstplot)
library(reshape)
elp.df <- read.csv("/Users/Adan Tallman/Desktop/ELP_full_length_frequency.csv")

senses <- read.csv("YourPath/winter_2016_senses_valence.csv")
data(ldt)
data(sharedref)



model_02 <- lm(RT~length, data=elp.df)
summary(model_02)

glimpse(elp.df)

plot(density(exp(elp.df$Log10Freq)))

model_03 <- lm(RT~Log10Freq, data=elp.df)
summary(model_03)

