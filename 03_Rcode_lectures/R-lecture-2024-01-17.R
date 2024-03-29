##R scratch notes (Statistics for Linguists)
##2024-01-17
##Multilevel models
library(tidyverse)
library(lme4)
library(lmerTest)

df <- read.csv("/Users/Adan Tallman/Desktop/StatisticsinLinguistics_FSU_2023_2024/07_data/phoneme_inventories.csv", header=TRUE)

options(scipen=999)

ggplot(df, aes(x=pop))+
  geom_bar(color="black")+
  stat_bin(bins=50)

ggplot(df, aes(x=log(pop)))+
  geom_bar(color="black")+
  stat_bin(bins=50)


ggplot(df, aes(x=log(inv)))+
  geom_bar(color="black")+
  stat_bin(bins=50)

df$logpop <- log(df$pop)
df$loginv <- log(df$inv)

plot.ols <- ggplot(df, aes(x=logpop, y=loginv))+
  geom_point()+
  geom_smooth(method="lm", se =FALSE)
plot.ols

ols_1 <- lm(loginv~logpop, data=df)
summary(ols_1)


##Group structures / levels in the data

table(df$family.name)

df.lgsubset <- subset(df, family.name=="Indo-European"|
                        family.name=="Uto-Aztecan"|
                      family.name =="Atlantic-Congo"|
                        family.name=="Sino-Tibetan"|
                        family.name == "Otomanguean"|
                        family.name=="Austronesian"|
                        family.name =="Pama-Nyungan"|
                        family.name=="Afroasiatic"|
                        family.name=="Arawakan"|
                        family.name=="Pano-Tacanan")

ggplot(df.lgsubset, aes(x=logpop, y=loginv, color=family.name))+
  geom_point()+
  geom_smooth(method="lm", se=FALSE)

plot1 <- ggplot(df.lgsubset, aes(x=logpop, y=loginv, group=family.name))+
  geom_point()+
  geom_smooth(method="glm", se=FALSE)

plot1+facet_wrap(~ family.name, ncol=3)

lg.subset2 <- subset(df,family.name=="Indo-European"|
                       family.name=="Arawakan")

##Multilevel model
summary(ols_1)

mod.lmer.null1 <- lmer(loginv~(1|family.name)+(1|area), data=df)
mod.lmer.null2 <- lmer(loginv~(1|family.name), data=df)
mod.lmer.null3 <- lmer(loginv~(1|area), data=df)

anova(mod.lmer.null1, mod.lmer.null2, mod.lmer.null3)

mod.lmer.a <- lmer(loginv~logpop+(1|family.name)+(1|area), data=df) #varying intercept

mod.lmer.ab <- lmer(loginv~logpop+(1+logpop|family.name)+(1+logpop|area), data= df) #varying intercept and slope

summary(mod.lmer.a)
summary(ols_1)

summary(mod.lmer.ab)

anova(mod.lmer.null1, mod.lmer.a, mod.lmer.ab)

beta1<- coef(mod.lmer.ab)$family.name
colnames(beta1) <- c("Intercept", "Slope")

beta2<- coef(mod.lmer.ab)$area
colnames(beta2) <- c("Intercept", "Slope")

install.packages("gridExtra")
library(gridExtra)

p1 <- ggplot(beta1, aes(Slope))+
  geom_density(fill="slategray2", color="slategray2", alpha=0.8)+
  ggtitle("Random slopes by linguistic family")


p2 <- ggplot(beta1, aes(Intercept))+
  geom_density(fill="slategray2", color="slategray2", alpha=0.8)+
  ggtitle("Random intercepts by linguistic family")

p3 <- ggplot(beta2, aes(Slope))+
  geom_density(fill="slategray2", color="slategray2", alpha=0.8)+
  ggtitle("Random slopes by area")

p4 <- ggplot(beta2, aes(Intercept))+
  geom_density(fill="slategray2", color="slategray2", alpha=0.8)+
  ggtitle("Random intercepts by area")

grid.arrange(p1, p2, p3, p4, ncol=2)

exp(0.0072)
