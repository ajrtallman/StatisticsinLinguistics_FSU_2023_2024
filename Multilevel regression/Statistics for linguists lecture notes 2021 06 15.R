

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

write.csv(data, "/Users/Adan Tallman/Desktop/schoolsdata.csv")


##Plotting
library(ggplot2)
library(ggExtra)

glimpse(data)

plot.grades.apt <-ggplot(data = data, aes(x = Grades, y = Aptitude, group=School))+   
  facet_wrap( ~ School, ncol=3)+    
  geom_point(aes(colour = School))+ 
  geom_smooth(method = "lm", se = TRUE, aes(colour = School))+  
  xlab("Grades")+ylab("Aptitude")+    
  theme(legend.position = "none") 

plot.grades.apt

plot.gender.apt <- ggplot(data, aes(x=Gender, y=Aptitude, group=Gender)) + 
  facet_wrap( ~ School, ncol=3)+
  geom_boxplot(aes(fill=Gender))

plot.gender.apt

mod.9 <- lm(Aptitude~Gender, data=school9)
summary(mod.9)


## all pooled

M1.pooling <- lm(Aptitude~Training+Grades+Gender, data=data)
summary(M1.pooling)

M1.nopooling <- lm(Aptitude~Training*as.factor(School)+Grades+Gender, data=data)
summary(M1.nopooling)

table(data$School)


library(lme4)
library(lmerTest)

M0 <- lmer(Aptitude~1 +(1|School)+Grades, data=data)

M1 <- lmer(Aptitude~Training + (1|School)+Grades, data=data ) 
M2 <- lmer(Aptitude ~Training + (1+Training|School)+Grades, data=data)

summary(M2)
anova(M1,M2,M0)

M0 <- lmer(Aptitude~1+(1|School), data=data)
M1.nopooling <- lm(Aptitude~Gender*Training+as.factor(School), data=data)
summary(M1.nopooling)

M1 <- lmer(Aptitude~Gender+(1+Gender|School), data=data)
summary(M1)

df2 <- read.csv("/Users/Adan Tallman/Desktop/df2.csv")

head(df2)

glimpse(df2)

table(df2$family.name)

histogram(df2$pop)

histogram(df2$logpop)

poplog <- df2$logpop
pop <- df2$pop
inventory <- df2$inv
loginv <- df2$loginv
family.name <- df2$family.name
area <- df2$area

##Models 

mod.ols <- lm(loginv~logpop, data=df2)
plot(loginv~poplog)
abline(lm(loginv~poplog))
summary(mod.ols)

mod.lmer <- lmer(loginv~logpop+(1+logpop|family.name)+(1+logpop|area), data=df2)


summary(mod.lmer)
summary(mod.ols)





