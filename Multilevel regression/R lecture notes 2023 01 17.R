## Scratch notes (multilevel models)

df <- read.csv("/Users/Adan Tallman/Desktop/inventories.clean.csv", header=TRUE)

library(tidyverse)

plot.ods <- ggplot(df, aes(x=logpop, y=loginv))+
  geom_point()+
  geom_smooth(method = "lm", se =FALSE)
plot.ods

model.ols1 <- lm(loginv~logpop, data=df) 
summary(model.ols1)

## 

table(df$family.name)

df.lgsubset <- subset(df, family.name == "Indo-European"|
                        family.name =="Uto-Aztecan"|
                        family.name == "Atlantic-Congo"|
                        family.name == "Sino-Tibetan"|
                        family.name == "Otomanguean"|
                        family.name == "Austronesian"|
                        family.name == "Pama-Nyungan"|
                        family.name == "Austroasiatic"|
                        family.name =="Arawakan"|
                        family.name == "Pano-Tacanan")



ggplot(df.lgsubset, aes(x=loginv, y=logpop, color=family.name))+
  geom_point()+
  geom_smooth(method="lm", se = FALSE)

plot1 <- ggplot(df.lgsubset, aes(x=logpop, y=loginv, group=family.name))+
  geom_point()+
  geom_smooth(method="glm",se = FALSE)
plot1 +  facet_wrap(~ family.name, ncol=3)

df.areasubset <- subset(df, area !="")

plot2 <- ggplot(df.areasubset, aes(x=logpop, y=loginv, group=area))+
  geom_point()+
  geom_smooth(method="glm",se = FALSE)
plot2 +  facet_wrap(~ area, ncol=3)

library(lme4)
library(lmerTest)

mod.lmer.a <- lmer(loginv~logpop+(1|family.name)+(1|area), data=df)
mod.lmer.ab <- lmer(loginv~logpop+(1+logpop|family.name) + (1+logpop|area),data=df)

mod.lmer.null1 <- lmer(loginv~(1|family.name)+ (1|area), data=df)
mod.lmer.null2 <- lmer(loginv~(1|family.name), data=df)
mod.lmer.null3 <- lmer(loginv~(1|area), data=df)

anova(mod.lmer.null1, mod.lmer.null2, mod.lmer.null3)

anova(mod.lmer.null1, mod.lmer.a, mod.lmer.ab)


