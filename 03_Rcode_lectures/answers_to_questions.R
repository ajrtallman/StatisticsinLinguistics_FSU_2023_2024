
##Homework 2

araonavowels <- read.csv("/Users/Adan Tallman/Desktop/vowels2.csv")
head(araonavowels)

duration_sd <- sd(araonavowels$duration)
n <- nrow(araonavowels)
duration_se <- duration_sd / sqrt(n)
duration_se <- sd(araonavowels$duration) / sqrt(nrow(araonavowels$duration)) 
summary(araonavowels)

intensity_sd <- sd(araonavowels$intensity)
n <- nrow(araonavowels)
intensity_se <- intensity_sd / sqrt(n)
intensity_se

##2
araonavowels_stressed <- araonavowels[araonavowels$stress=="y",]
araonavowels_unstressed <- araonavowels[araonavowels$stress=="n",]
par(mfrow=c(1,2))
boxplot(araonavowels_stressed$duration)
boxplot(araonavowels_unstressed$duration)

araonavowels_stressed <- filter(araonavowels, stress =="y")

##tidyverse / ggplot
library(tidyverse)
ggplot(araonavowels, aes(x=stress, y=duration))+
  geom_boxplot()

ggplot(araonavowels, aes(x=stress, y=duration))+
  geom_violin()

head(araonavowels)

##Question 3
par(mfrow=c(1,1))
frequency_table <- table(araonavowels$stress)
barplot(frequency_table, col=c("salmon", "pink"))

##in ggplot
ggplot(araonavowels, aes(x=stress, fill=stress))+
  geom_bar()+
  scale_fill_manual(values=c("darkgreen", "darkred"))


##Question 4
set.seed(123)
group1 <- rnorm(m= 2.8, sd=6, n=40)
group2 <- rnorm(m= 0, sd=6, n=40)
t.test(group1, group2)

density_group1 <- density(group1)
density_group2 <- density(group2)
plot(density_group2, col = "lightblue")
lines(density_group1, col = "salmon")

df <- data.frame(
  value= c(group1, group2),
  group = rep(c("Group1", "Group2"), each=length(group1))
)

ggplot(df, aes(x=value, fill=group))+
  geom_density(alpha=0.3)

##Question 5
chacobo.mcf <- read.csv("/Users/Adan Tallman/Desktop/chacobo.mcf.df.csv", header=TRUE)

head(chacobo.mcf)

ggplot(chacobo.mcf, aes(y=pitch.f0, x=response))+
  geom_boxplot()+
  facet_wrap(~subject)

ggplot(chacobo.mcf, aes(y=reactionTime, x=pitch.f0))+
  geom_point()+
  facet_wrap(~subject)+
  ylim(0,15)

ggplot(chacobo.mcf, aes(y=reactionTime, x=jitter(pitch.f0)))+
  geom_point()+
  facet_wrap(~subject)


