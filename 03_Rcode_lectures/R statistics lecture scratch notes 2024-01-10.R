##R scratch notes 2024-01-10

library(ggdag)
library(ggplot2)
library(VGAM)
library(ggExtra)
library(tidyverse)
library(gridExtra)
library(lme4)
library(lmerTest)

set.seed(200)

## School 1
N1 <- round(runif(1,10,120))
N <- N1            
grades <- runif(N,0,10)
training <- rbinom(N,1,(0.2+grades/20))
gender <- rbinom(N,1,0.5)
b1 <- rnorm(1,2,0.5)
b2 <- rnorm(1,2,0.5)
b3 <- rnorm(1,0,2)
a <- rnorm(1,5,2)
aptitude <- a + b1*grades + b2*training +b3*gender+rnorm(N, sd=1)
school1 <- data.frame(Grades =grades,
                      Training = training, 
                      Aptitude = aptitude,
                      Gender = factor(gender, levels=c(0,1),
                                      labels = c("M", "F")))

# School 2
N2 <- round(runif(1,10,120)) 
N <- N2
grades <- runif(N, 0, 10)
training <- rbinom(N, 1, (0.2+grades/20))
gender <- rbinom(N, 1, 0.5)
b1 <- rnorm(1,2,0.5)
b2 <- rnorm(1,2,0.5)
b3 <- rnorm(1,0,2)
a <- rnorm(1,5,2)
aptitude <- a + b1*grades + b2*training + b3*gender + rnorm(N, sd= 1) 
school2 <- data.frame(Grades = grades,
                      Training = training,
                      Aptitude = aptitude,
                      Gender = factor(gender, levels=c(0, 1),
                                      labels=c("M", "F")))

N3 <- round(runif(1,10,120))
N <- N3
grades <- runif(N, 0, 10)
training <- rbinom(N, 1, (0.2+grades/20))
gender <- rbinom(N, 1, 0.5)
b1 <- rnorm(1,2,0.5)
b2 <- rnorm(1,2,0.5)
b3 <- rnorm(1,0,2)
a <- rnorm(1,5,2)
aptitude <- a + b1*grades + b2*training + b3*gender + rnorm(N, sd= 1) 
school3 <- data.frame(Grades = grades,
                      Training = training,
                      Aptitude = aptitude,
                      Gender = factor(gender, levels=c(0, 1),
                                      labels=c("M", "F")))


N4 <- round(runif(1,10,120))
N <- N4
grades <- runif(N, 0, 10)
training <- rbinom(N, 1, (0.2+grades/20))
gender <- rbinom(N, 1, 0.5)
b1 <- rnorm(1,2,0.5)
b2 <- rnorm(1,2,0.5)
b3 <- rnorm(1,0,2)
a <- rnorm(1,5,2)
aptitude <- a + b1*grades + b2*training + b3*gender + rnorm(N, sd= 1) 
school4 <- data.frame(Grades = grades,
                      Training = training,
                      Aptitude = aptitude,
                      Gender = factor(gender, levels=c(0, 1),
                                      labels=c("M", "F")))



N5 <- round(runif(1,10,120))
N <- N5
grades <- runif(N, 0, 10)
training <- rbinom(N, 1, (0.2+grades/20))
gender <- rbinom(N, 1, 0.5)
b1 <- rnorm(1,2,0.5)
b2 <- rnorm(1,2,0.5)
b3 <- rnorm(1,0,2)
a <- rnorm(1,5,2)
aptitude <- a + b1*grades + b2*training + b3*gender + rnorm(N, sd= 1) 
school5 <- data.frame(Grades = grades,
                      Training = training,
                      Aptitude = aptitude,
                      Gender = factor(gender, levels=c(0, 1),
                                      labels=c("M", "F")))


N6 <- round(runif(1,10,120))
N <- N6
grades <- runif(N, 0, 10)
training <- rbinom(N, 1, (0.2+grades/20))
gender <- rbinom(N, 1, 0.5)
b1 <- rnorm(1,2,0.5)
b2 <- rnorm(1,2,0.5)
b3 <- rnorm(1,0,2)
a <- rnorm(1,5,2)
aptitude <- a + b1*grades + b2*training + b3*gender + rnorm(N, sd= 1) 
school6 <- data.frame(Grades = grades,
                      Training = training,
                      Aptitude = aptitude,
                      Gender = factor(gender, levels=c(0, 1),
                                      labels=c("M", "F")))

N7 <- round(runif(1,10,120))
N <- N7
grades <- runif(N, 0, 10)
training <- rbinom(N, 1, (0.2+grades/20))
gender <- rbinom(N, 1, 0.5)
b1 <- rnorm(1,2,0.5)
b2 <- rnorm(1,2,0.5)
b3 <- rnorm(1,0,2)
a <- rnorm(1,5,2)
aptitude <- a + b1*grades + b2*training + b3*gender + rnorm(N, sd= 1) 
school7 <- data.frame(Grades = grades,
                      Training = training,
                      Aptitude = aptitude,
                      Gender = factor(gender, levels=c(0, 1),
                                      labels=c("M", "F")))

N8 <- round(runif(1,10,120))
N <- N8
grades <- runif(N, 0, 10)
training <- rbinom(N, 1, (0.2+grades/20))
gender <- rbinom(N, 1, 0.5)
b1 <- rnorm(1,2,0.5)
b2 <- rnorm(1,2,0.5)
b3 <- rnorm(1,0,2)
a <- rnorm(1,5,2)
aptitude <- a + b1*grades + b2*training + b3*gender + rnorm(N, sd= 1) 
school8 <- data.frame(Grades = grades,
                      Training = training,
                      Aptitude = aptitude,
                      Gender = factor(gender, levels=c(0, 1),
                                      labels=c("M", "F")))

N9 <- 300
N <- N9
grades <- runif(N, 0, 10)
training <- rbinom(N, 1, (0.2+grades/20))
gender <- rbinom(N, 1, 0.5)
b1 <- rnorm(1,2,0.5)
b2 <- -1.5
b3 <- rnorm(1,0,2)
a <- rnorm(1,5,2)
aptitude <- a + b1*grades + b2*training + b3*gender + rnorm(N, sd= 1) 
school9 <- data.frame(Grades = grades,
                      Training = training,
                      Aptitude = aptitude,
                      Gender = factor(gender, levels=c(0, 1),
                                      labels=c("M", "F")))

data <- rbind(school1,school2,school3,school4,school5,school6,school7,school8,school9)

data$School <- c(rep(1, N1), rep(2, N2), rep(3, N3), rep(4, N4), rep(5, N5), rep(6, N6), rep(7, N7), rep(8, N8), rep(9, N9))

head(data)

library(ggplot2)
plot.grades.apt <-ggplot(data = data, aes(x = Grades, y = Aptitude, group=School))+   
  facet_wrap( ~ School, ncol=3)+    
  geom_point(aes(colour = School))+ 
  geom_smooth(method = "lm", se = TRUE, aes(colour = School))+  
  xlab("Grades")+ylab("Aptitude")+    
  theme(legend.position = "none") 
plot.grades.apt
nrow(data[data$School=="9",])
