



## Confounds, the fork

d <- read.csv("/Users/Adan Tallman/Desktop/araonatones.csv") 

library(tidyverse)
library(ggdag)
library(dagitty)

dag <- dagitty("dag{
  D -> P
}")
coordinates(dag) <-
  list(x=c(D = 1, P= 3), y=c(D = 1, P=1))
ggdag(dag)+remove_axes()+remove_grid()

pitchend <- d$F0_at_end
duration <- d$Vowel_duration_.s.
plot(pitchend~duration, ylim=c(50,250))
abline(lm(pitchend~duration))

model1 <- lm(pitchend~duration)

summary(model1)

tones <- d$Tone

boxplot(duration~tones)

model2 <- lm(duration~tones)

summary(model2)

plot(precis(model2))

model3 <- lm(pitchend~tones) 

plot(precis(model3))

summary(model3)


##Drawing the correct DAG

dag <- dagitty("dag{
  D -> P
  T -> D
  T -> P
}")
coordinates(dag) <-
  list(x=c(D = 1, P= 3, T = 2), y=c(D = 1, P=1, T =2))
ggdag(dag)+remove_axes()+remove_grid()

## Closing the backdoor
## This type of confound is called a backdoor path
## We close a backdoor path by conditioning on it

model1.corrected <- lm(pitchend~duration+tones)

summary(model1.corrected)
summary(model1)

AIC(model1, model1.corrected)

##Simulating a fork

# set seed for repeatability
set.seed(805)
# n = 1000 points for the simulation
n <- 1000
# create variables
# J is random draws from standard normal (mean = 0, stdev = 1)
confounding_var_J <- rnorm(n)
# J is used in creation of A since it is a cause of A (confounder)
independent_var_A <- 1.1 * confounding_var_J + rnorm(n)
# J is used in creation of X since it is a cause of X (confounder)
dependent_var_X <- 1.9 * confounding_var_J + rnorm(n)

confounder_model_tbl <- summary(confounder_model) %>% tidy()
confounder_model_kbl <- summary(confounder_model) %>%
  tidy() %>%
  kable(align = rep("c", 5), digits = 3)
confounder_model_kbl

## Model selection


library(languageR)

glimpse(ratings)
head(ratings)
help(ratings)

attach(ratings)

rat_list <- list(
  size = ratings$meanSizeRating,
  length = ratings$Length,
  familiar = ratings$meanFamiliarity,
  class = ratings$Class,
  pluralfreq = ratings$FreqPlural,
  synset = ratings$SynsetCount,
  freq = ratings$Frequency
)

rat.df = data.frame(rat_list)

head(rat.df)

##pairs function looks at the correlation between all the vectors

pairs(rat.df, panel= panel.smooth)


## We can do a regression tree (a discussion can be found in Chapter 14 of Levshina)

library(party); library(Rling)

set.seed(129)

ratings.tree = ctree(size~length+familiar+class+pluralfreq+synset+freq, data=rat.df)
plot(ratings.tree)
help(ratings)


## Building a regression model

model1 <- lm(size~class*pluralfreq*synset, data=rat.df)
summary(model1)
model2 <- update(model1, ~.-class:pluralfreq:synset)
summary(model2)
model3 <- update(model2, ~.-class:pluralfreq)
summary(model3)

model4 <- update(model3, ~.-pluralfreq:synset)
summary(model4)

model5 <- update(model4, ~.-class:synset)
summary(model5)

plot(model5)

## Interaction

install.packages("ggExtra")
library(tidyverse); library(ggExtra)


##Let's create a model without an interaction

set.seed(123)
n <- 500
coffee <- rnorm(n, 30, 5)
level.int <- rep(c(1,2), n)
a <- 2
b1 <- 0.08  
b2 <- -1
y <- a + b1*coffee + b2*level.int  + rnorm(n, 0, 0.8)
data1 <- data.frame(coffee, level = factor(level.int, label = c("PhD", "Postdoc")), happiness = y)
plot1<-ggplot(data1, aes(x=coffee, y = happiness, color = level))+
    geom_point()
plot1  

## Making the lines

plot1 +
  geom_point(alpha = .3, 
             size = .9) +
  geom_smooth(method = "lm", se=FALSE)


##Let's add an interaction

set.seed(123)
n <- 500
coffee <- rnorm(n, 30, 10)
level.int <- rep(c(1,2), n)
a <- 4
b1 <- 0.08  
b2 <- -1

y <- a + b1*coffee + b2*level.int + rnorm(n, 0, 0.8) + (-0.035*(coffee*level.int))

data2 <- data.frame(coffee, level = factor(level.int, label = c("PhD", "Postdoc")), happiness = y)
plot2<-ggplot(data2, aes(x=coffee, y = happiness, color = level))+
  geom_point()
plot2  

## Making the lines

plot2 +
  geom_point(alpha = .3, 
             size = .9) +
  geom_smooth(method = "lm", se=FALSE)


## 

model1.data1 <- lm(happiness~coffee+level, data=data1)
model2.data1 <- lm(happiness~coffee*level, data=data1)

model1.data2 <- lm(happiness~coffee+level, data=data2)
model2.data2 <- lm(happiness~coffee*level, data=data2)

summary(model1.data1)
summary(model2.data1)

summary(model2.data2)

