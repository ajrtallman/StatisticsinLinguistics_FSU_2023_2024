## Log odds ratio


data <- rbinom(20, 1,.5)


## There are few ways of simulating binary data

sample(c(0,1), size = 20, replace = TRUE, prob = c(0.3, 0.7))
rbinom(n = 20, size = 1, prob = 0.7)
dice <- rbinom(n = 10, size = 6, prob = 0.5) +1
dice

##Logistic regression model is about telling us where 
##the probability changes conditional on other variables

t = log(10)
t
exp(t)


## Logistic regression

##Let's plot the log() function:

#odds plot
par(mfrow=c(1,1))
x <- seq(0,1,by=0.1)
plot(x,1/(1-x))
plot(1/(1-x),x)

##plot logs
x <- seq(0,1,by=0.1)
plot(x,log(x))

##plot exponentials
x <- seq(-2,0, by =.1)
plot(x, exp(x))




##log odds
par(mfrow=c(1,1))
x <- seq(0,1,by=.02)
plot(x,log(x/(1-x)))
plot(log(x/(1-x)),x)


##exponential odds
par(mfrow=c(1,1))
x <- seq(-4,4,by=.1)
plot(x, 1/(1+exp(-x)))

##Adding a coefficient

par(mfrow=c(2,2))
x <- seq(-4,4,by=.1)
b <- 2
plot(x, 1/(1+exp(-x*b)), main = "b = 2")

b <- 8
plot(x, 1/(1+exp(-x*b)), main = "b = 8")

b <- 0.5
plot(x, 1/(1+exp(-x*b)), main = "b = 0.5")

b <- -1
plot(x, 1/(1+exp(-x*b)), main = "b = -1")






## Using a log function

#log(x) function computes natural logarithms (Ln) for a number or vector x by default. If the base is specified, log(x,b) computes logarithms with base b.
#log10 computes common logarithms (Lg). log2 computes binary logarithms (Log2).
#log(x, base = exp(1))
#log() by default computes the natural logarithms (Ln, with base e):

integers = 1:20

log(integers)



## Logistic regression practice

##
set.seed(1)
#postdoc <- sample(c(0,1), size = 100, replace = TRUE)
coffee <- rnorm(100, 15, 5)
happiness <- rnorm(100, 10, 2)
a <- -5
b1 <- -1
b2 <- 1
##We are going to assume that there is some set of slope values that are associated with the factors

xb <- a + b1 * happiness + b2 * coffee + rnorm(100, 0, 0.1)   

##Now we generate probabilities using logistic regression model

p <- 1/ (1+exp(-xb))
summary(p)
par(mfrow=c(1,1))
plot(p)

gotoclass <- rbinom(n=100, size=1, prob=p)

gotoclass


library(glm2)

model.logit <- glm(gotoclass~happiness+coffee, family="binomial")

summary(model.logit)


##Interpreting logistic regression coefficients

invlogit <- function (x) {1/(1+exp(-x))}

invlogit(-4.1157 + -1.6263*mean(happiness) )

-1.6263/4

## Divide by 4 gives you the maximum difference corresponding to a unit difference in happiness.

##Let's do some visualization

par(mfrow=c(1,2))

data <- data.frame(gotoclass, happiness, coffee)

model.linear <- summary(lm(gotoclass~coffee+happiness))

install.packages("ggpubr")

library(ggpubr)

plotline<-ggplot(data, aes(x=coffee, y = gotoclass))+geom_point()+
  geom_abline(intercept = 0.11105, slope = 0.076446, color="red", size=1)+ 
  ylab("Probability of going to class") 

plotS<-ggplot(data, aes(x=coffee, y= gotoclass))+ 
  geom_point(alpha=.5)+
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial))+ 
  ylab("Probability of going to class") 

ggarrange(plotline, plotS)


##Interpreting logistic regression


##Logistic regression


##Let's do a logistic regression in real life
install.packages(c("rms", "visreg"))

library(Rling);library(rms); library(visreg); library(car); library(glimpse)

data(doenLaten)
d <- doenLaten
head(doenLaten)
library(tidyverse)
glimpse(d)
d <- as.data.frame(d)
model.glm <- glm(Aux~Causation*EPTrans*Country, data=d, family="binomial")
summary(model.glm)



##

x <- rnorm(1000,0,2)
e <- rnorm(1000,0,0.1)
y <- x + e
dat <- data.frame(x,y,e)
x.dens<-ggplot(dat, aes(x=x)) + geom_density(alpha=.2, fill="#FF6666")
y.dens <-ggplot(dat, aes(x=y)) + geom_density(alpha=.2, fill="#FF6666")
e.dens <- ggplot(dat, aes(x=e)) + geom_density(alpha=.2, fill="#FF6666")



grid.arrange(y.dens, x.dens, e.dens, nrow=1)

a <- rnorm(1000, 0,1)
b <- rnorm(1000,0,1)
x <- rnorm(1000,0,2)
e <- rnorm(1000,0,0.1)
y <- 0+1*x + e
dat <- data.frame(x,y,e,a,b)
a.dens <- ggplot(dat, aes(x=a))+ geom_density(alpha=.2, fill="rosybrown")
b.dens <- ggplot(dat, aes(x=b)) + geom_density(alpha=.2, fill="rosybrown")
x.dens<-ggplot(dat, aes(x=x)) + geom_density(alpha=.2, fill="#FF6666")
y.dens <-ggplot(dat, aes(x=y)) + geom_density(alpha=.2, fill="#FF6666")
e.dens <- ggplot(dat, aes(x=e)) + geom_density(alpha=.2, fill="#FF6666")

ab <- grid.arrange(a.dens, b.dens, nrow=1)
yxe<- grid.arrange(y.dens, x.dens, e.dens, nrow=1)
grid.arrange(ab,yxe, nrow=2)

##Visualizing different types of relationships for multilevel models

#Varying intercepts
a1 <- rnorm(1, 0, 3)
a2 <- rnorm(1, 0, 3)
a3 <- rnorm(1, 0, 3)
b <- 1

x1 <- rnorm(100)
y1 <- a1 + b*x1
x2 <- rnorm(100)
y2 <- a2 + b*x2
x3 <- rnorm(100)
y3 <- a3 + b*x3

d1 <- data.frame(x = x1 ,y = y1, group = "1")
d2 <- data.frame(x = x2,y = y2, group = "2")
d3 <- data.frame(x = x3,y = y3, group = "3")

d <- rbind(d1,d2, d3)
varying.a <- ggplot(d,aes(y = y,x = x,color = group)) + 
  geom_line()+
  ggtitle("Varying intercept")
varying.a




##Varying slopes
a=0
b1 <- rnorm(1, 0, 3)
b2 <- rnorm(1, 0, 3)
b3 <- rnorm(1, 0, 3)

x1 <- rnorm(100, 1)
y1 <- a+b1*x1
x2 <- rnorm(100, 1)
y2 <- a+b2*x2
x3 <- rnorm(100, 1)
y3 <- a+b3*x3

d1 <- data.frame(x = x1 ,y = y1, group = "1")
d2 <- data.frame(x = x2,y = y2, group = "2")
d3 <- data.frame(x = x3,y = y3, group = "3")

d <- rbind(d1,d2, d3)
varying.b <- ggplot(d,aes(y = y,x = x,color = group)) + 
  geom_line()+
  xlim(0,3)+
  ggtitle("Varying slope")

varying.b

##Varying a and b

a1 <- rnorm(1, 0, 3)
a2 <- rnorm(1, 0, 3)
a3 <- rnorm(1, 0, 3)
b1 <- rnorm(1, 0, 3)
b2 <- rnorm(1, 0, 3)
b3 <- rnorm(1, 0, 3)

x1 <- rnorm(100, 1)
y1 <- a1+b1*x1
x2 <- rnorm(100, 1)
y2 <- a2+b2*x2
x3 <- rnorm(100, 1)
y3 <- a3+b3*x3

d1 <- data.frame(x = x1 ,y = y1, group = "1")
d2 <- data.frame(x = x2,y = y2, group = "2")
d3 <- data.frame(x = x3,y = y3, group = "3")

d <- rbind(d1,d2, d3)
varying.ab <- ggplot(d,aes(y = y,x = x,color = group)) + 
  geom_line()+
  xlim(0,3)+
  ggtitle("Varying intercept and slope")

varying.ab

library(gridExtra)

grid.arrange(varying.a,varying.b,varying.ab, nrow=1)

##Simulating multilevel data

##Schools

set.seed(200)

# School 1
N1 <- round(runif(1,10,120))
N <- N1
grades <- runif(N, 0, 10)
training <- rbinom(N, 1, (0.2+grades/20))
gender <- rbinom(N, 1, 0.5)
b1 <- rnorm(1,2,0.5)
b2 <- rnorm(1,2,0.5)
b3 <- rnorm(1,0,2)
a <- rnorm(1,5,2)
aptitude <- a + b1*grades + b2*training + b3*gender + rnorm(N, sd= 1) 
school1 <- data.frame(Grades = grades,
                      Training = training,
                      Aptitude = aptitude,
                      Gender = factor(gender, levels=c(0, 1),
                                      labels=c("M", "F")))
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


##Plotting

library(ggplot2)
library(ggExtra)
  
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


plot.training.apt <- ggplot(data, aes(x=Training, y=Aptitude, group=Training)) + 
  facet_wrap( ~ School)+
  geom_boxplot(aes(fill=Training))

plot.training.apt

##

M1.pooling <- lm(Aptitude~Training+Grades, data=data)
summary(M1.pooling)

M1.nopooling <- lm(Aptitude~Training*as.factor(School)+Grades,data=data)
summary(M1.nopooling)

table(data$School)

##Follows closely Gelman and Hill (2009)

library(lme4)
library(MASS)
library(BRugs)
library(arm)
library(rethinking)
library(car)
library(lmerTest)
M0 <-  lmer(Aptitude~1 +(1|School)+Grades, data=data)
summary(M0)
display(M0)

M1 <- lmer(Aptitude~Training + (1|School)+Grades, data=data)

M2 <- lmer(Aptitude~Training + (1+Training|School) +Grades, data=data)                      

coef(M1)

coef(M2)

anova(M1, M2, M0)



##Population and inventory size

library(tidyverse)
library(lme4)
library(lmerTest)

ethnologueraw <- read_tsv("/Users/Adan Tallman/Desktop/e20.tsv")

ethnologue <- read.csv("/Users/Adan Tallman/Desktop/Population.csv")


phoiblephonemes <- read.csv("/Users/Adan Tallman/Desktop/phoible.csv")


phoible1 <- read.csv("/Users/Adan Tallman/Desktop/contributions.csv")

phoible2 <- read.csv("/Users/Adan Tallman/Desktop/languages.csv")

phoible <- cbind(phoible1, phoible2)


phoiblejoined <- left_join(phoible1, phoible2, by = c("Name"="Name"))
data <- left_join(phoiblejoined, ethnologueraw, by = c("Name"="name"))
data$pop <- as.numeric(data$`Population Numeric`)
data$logpop <- log(data$pop)
data$loginv <- log(data$count_phonemes)

poplog <- data$logpop
pop <- data$pop
inventory <- data$count_phonemes
loginv <- data$loginv
family.glottocode <- as_factor(data$Family_Glottocode)
family.name <- as_factor(data$Family_Name)
area <- as_factor(data$Macroarea)

levels(family.name)

df <- data.frame(logpop, inventory, family.name, area, pop, loginv)

df1 <- df[!is.na(df$logpop),]
df2 <- df1[!is.na(df1$loginv),]

glimpse(df2)

df3 <- df2[df2$pop !=0,]

histogram(inventory)
histogram(loginv)
histogram(pop)
histogram(logpop)
histogram(df3$logpop)

glimpse(df3)

write.csv(df2, "/Users/Adan Tallman/Desktop/df2.csv")


df.recoded <- read.csv("/Users/Adan Tallman/Desktop/df3.csv")



##Models


mod.ols <- lm(loginv~logpop, data=df2)
mod.rho <- cor.test(x=logpop, y=loginv, method = 'spearman')
mod.tau <- cor.test(x=logpop, y=loginv, method = 'kendall')


mod.tau

mod.rho

summary(mod.ols)




plot(loginv~logpop, xlab="Log population size", ylab="Log phoneme inventory")
abline(lm(loginv~logpop))

mod.lmer1 <-lmer(loginv~logpop+(1+logpop|family.name)+(1+logpop|area), data=df2)
?isSingular
summary(mod.lmer1)

beta.family <- coef(mod.lmer1)$family.name
colnames(beta.family) <- c("Intercept", "Slope")
beta.family
beta.area <- coef(mod.lmer1)$area
colnames(beta.area) <- c("Intercept", "Slope")
beta.area


mod.null <- lmer(loginv~(1|family.name)+(1|area), data=df2)
mod.null2 <- lmer(loginv~(1|family.name), data=df2)
mod.null3 <- lmer(loginv~(1|area), data=df2)

summary(mod.null)
anova(mod.null, mod.lmer1)



summary(mod.lmer2)

## Removing data with zero population

mod.lmer2.null <- lmer(loginv~(1|family.name)+(1|area), data=df3)
mod.lmer2 <-lmer(loginv~logpop+(1+logpop|family.name)+(1+logpop|area), data=df3)
anova(mod.lmer2, mod.lmer2.null)

beta.family <- coef(mod.lmer2)$family.name
colnames(beta.family) <- c("Intercept", "Slope")
beta.family
beta.area <- coef(mod.lmer2)$area
colnames(beta.area) <- c("Intercept", "Slope")
beta.area

beta.area<-as.data.frame(beta.area)

dens.intercept <- ggplot(beta.family, aes(x=Intercept)) + 
  geom_density(fill="pink")

dens.slope <- ggplot(beta.family, aes(x=Slope)) + 
  geom_density(fill="purple")

library(gridExtra)


grid.arrange(dens.slope, dens.intercept, ncol=2)


##Plotting

table(df3$family.name)

df4 <- subset(df3, family.name == "Indo-European"|
                family.name =="Uto-Aztecan"|
              family.name == "Atlantic-Congo"|
              family.name == "Sino-Tibetan"|
              family.name == "Otomanguean"|
              family.name == "Austronesian"|
              family.name == "Pama-Nyungan"|
              family.name == "Austroasiatic"|
                family.name =="Arawakan"|
                family.name == "Pano-Tacanan")

df4 %>%
  ggplot(aes(x=loginv, 
              y=logpop, 
              color=family.name))+
  geom_point()+
  geom_smooth(method="lm",se = FALSE)


plot <- ggplot(df4, aes(x=loginv, y=logpop, group=family.name))+
  geom_point()+
  geom_smooth(method="lm",se = FALSE)
plot +  facet_wrap(~ family.name, ncol=3)




##Models (trying again with isolate data)

mod3 <- lm(inventory~logpop, data=df3)
summary(mod3)

mod3 <-lmer(inventory~logpop+(1+logpop|family.name)+(1+logpop|area), data=df3)
summary(mod3)

mod.null4 <- lmer(inventory~(1|family.name)+(1|area), data=df3)
mod.null5 <- lmer(inventory~(1|family.name), data=df3)
mod.null6 <- lmer(inventory~(1|area), data=df3)

anova(mod3, mod.null4, mod.null5, mod.null6)



##Models with longitude and latitude

poplog <- data$logpop
pop <- data$pop
inventory <- data$count_phonemes
family.glottocode <- as_factor(data$Family_Glottocode)
family.name <- as_factor(data$Family_Name)
area <- as_factor(data$Macroarea)
long <- data$Longitude
lat <-data$Latitude

df4 <- data.frame(logpop, pop, inventory, family.name, area, long, lat)

mod4  <- lm(inventory~logpop+lat, data=df4)
summary(mod4)

df5 <- df4[!is.na(df4$logpop),]
df6 <- df5[!is.na(df5$inventory),]

mod5 <- lmer(inventory~logpop+lat+ (1+logpop|family.name) +(1+logpop|area), data=df6)
summary(mod5)

##Exploratory Data Analysis

##Provide introduction to EDA

install.packages("vegan")
install.packages("leaflet")
install.packages("googleVis")
install.packages("labdsv")

library(vegan)
library(leaflet)
library(googleVis)
library(labdsv)

source("panelutils.R")
load("Doubs.RData")

spe[1:5, 1:10]

head(spe)
tail(spe)
nrow(spe)
ncol(spe)
dim(spe)
dim(df5)
