

d <- read.csv("/Users/Adan Tallman/Desktop/araonatones.csv")
install.packages("tidyverse")
library(tidyverse)
library(ggdag)
library(dagitty)

head(d)

dag <- dagitty("dag{
  D -> P
}")

coordinates(dag) <-
  list(x=c(D=1,P=3), y =c(D=1,P=1))
ggdag(dag)

pitchend <- d$F0_at_end
duration <- d$Vowel_duration_.s.
plot(pitchend~duration, ylim =c(50,250))
abline(lm(pitchend~(1000*duration)))

model1 <- lm(pitchend~duration)
summary(model1)

tones <- d$Tone


boxplot(duration~tones)

model2 <- lm(duration~tones)
summary(model2)

model3 <- lm(pitchend~tones)
summary(model3)

dag <- dagitty("dag{
  D -> P
  T -> D
  T -> P
}")

coordinates(dag) <-
  list(x=c(D=1,P=3, T=2), y =c(D=1,P=1, T=2))
ggdag(dag)

model1.corrected <- lm(pitchend~duration+tones)

summary(model1.corrected)

AIC(model1, model1.corrected)


## Model fitting

library(languageR)

glimpse(ratings)

help(ratings)

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

pairs(rat.df, panel=panel.smooth)

## Do a regression tree

library(party)
library(Rling)

set.seet(129)

ratings.tree <- ctree(size~length
                      +familiar
                      +class
                      +pluralfreq
                      +synset
                      +freq, data=rat.df)
plot(ratings.tree)


model1 <- lm(size~class*pluralfreq*synset, 
             data=rat.df)

summary(model1)

model2 <- update(model1, ~.-class:pluralfreq:synset)
AIC(model1, model2)

model3 <- update(model2, ~.-pluralfreq:synset)

model4 <- update(model3, ~.-class:synset)

summary(model4)

model5 <- update(model4,~.-class:pluralfreq)

summary(model5)


AIC(model1,model2,model3,model4,model5)


plot(model5)


##Simulate interactions

install.packages("ggExtra")

library(ggExtra); library(tidyverse)

## Let's create a model without an interaction

set.seed(2000)

n <- 500
coffee <- rnorm(n, 30, 5)
level.int <- rep(c(1,2), n)
coffee
level.int

a <- 2
b1 <- 0.08
b2 <- -1

y <- a + b1*coffee + b2*level.int + rnorm(n,0,0.8)

data1 <-data.frame(coffee, level = factor(level.int, 
                                         label = c("PhD", "Postdoc")), 
                  happiness = y)

plot1 <- ggplot(data1, aes(x=coffee, y = happiness, color = level))+
    geom_point()
plot1

plot1+
  geom_smooth(method ="lm", se=FALSE)

##Interaction

b3  = -0.035
interaction = coffee*level.int

y <- a + b1*coffee + b2*level.int + rnorm(n,0,0.8)+ b3*interaction

data2 <-data.frame(coffee, level = factor(level.int, 
                                          label = c("PhD", "Postdoc")), 
                   happiness = y)

plot2 <- ggplot(data2, aes(x=coffee, y = happiness, color = level))+
  geom_point()

plot2+
  geom_smooth(method ="lm", se=FALSE)

model1.data1 <- lm(happiness~coffee+level, data=data1)
modell2.data2 <- lm(happiness~coffee*level, data=data2)

summary(model2.data2)

model2.data1 <- lm(happiness~coffee*level, data=data1)

summary(model2.data1)
