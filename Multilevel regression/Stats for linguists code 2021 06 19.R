##Loading necessary packages

library(tidyverse)

##Loading data

ethnologueraw <- read_tsv("/Users/Adan Tallman/Desktop/e20.tsv")
ethnologue <- read.csv("/Users/Adan Tallman/Desktop/Population.csv")
phoiblephonemes <- read.csv("/Users/Adan Tallman/Desktop/phoible.csv")
phoible1 <- read.csv("/Users/Adan Tallman/Desktop/contributions.csv")
phoible2 <- read.csv("/Users/Adan Tallman/Desktop/languages.csv")

##Merging Ethnologue 20 with Phoible

phoiblejoined <- left_join(phoible1, phoible2, by = c("Name"="Name"))
data <- left_join(phoiblejoined, ethnologueraw, by = c("Name"="name"))



##Plotting the population and phoneme count distributions

data$pop <- as.numeric(data$`Population Numeric`)
data$inv <- data$count_phonemes

data %>%
  ggplot( aes(x=pop)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) ##Let's get a closer look

data %>%
  filter(pop<40000000) %>%
  ggplot( aes(x=pop)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

data %>%
  ggplot( aes(x=inv)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

write.csv(df2, "/Users/Adan Tallman/Desktop/df2.csv")


##Defining and formatting variables for easy reference

data$logpop <- as.numeric(log(data$pop+1)) ##Just to avoid -Inf

data %>%
  ggplot( aes(x=logpop)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

data$loginv <- as.numeric(log(data$count_phonemes+1)) ##Just to avoid -Inf

data %>%
  ggplot( aes(x=loginv)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

logpop <- data$logpop
pop <- data$pop
inv <- data$inv
loginv <- data$loginv
family.glottocode <- as_factor(data$Family_Glottocode)
family.name <- as_factor(data$Family_Name)
area <- as_factor(data$Macroarea)

##Removing 0 populations and NA values

df <- data.frame(pop, logpop, inv, loginv, family.name, area)
df1 <- df[!is.na(df$logpop),]
df2 <- df1[!is.na(df1$loginv),] ##With NAs removed 
glimpse(df2)
df3 <- df2[df2$pop !=0,]      ##With zero pops removed
glimpse(df3)

##Save dataframes to Desktop to send to students who were not able to follow

write.csv(df2, "/Users/Adan Tallman/Desktop/df2.csv")
df.recoded <- read.csv("/Users/Adan Tallman/Desktop/df3.csv")

##Models (ols, rho and tau)

mod.ols2 <- lm(loginv~logpop, data=df2)
mod.rho2 <- cor.test(x=df2$logpop, y=df2$loginv, method = 'spearman')
mod.tau2 <- cor.test(x=df2$logpop, y=df2$loginv, method = 'kendall') ##With zero values

mod.ols3 <- lm(loginv~logpop, data=df3)
mod.rho3 <- cor.test(x=df3$logpop, y=df3$loginv, method = 'spearman')
mod.tau3 <- cor.test(x=df3$logpop, y=df3$loginv, method = 'kendall') ##Without zero values

summary(mod.ols2)
summary(mod.ols3)

mod.rho2
mod.rho3

mod.tau2
mod.tau3

##Plotting the relationship between logpop and loginv

library(gridExtra)

p1 <- ggplot(df2, aes(logpop, loginv)) +
  geom_point()+
  geom_smooth(method = "loess")+
  ggtitle("With 0 pops")

p2 <- ggplot(df3, aes(logpop, loginv)) +
  geom_point()+
  geom_smooth(method = "loess")+
  ggtitle("Without 0 pops")

grid.arrange(p1,p2, nrow=1)

p3 <- ggplot(df2, aes(logpop, loginv)) +
  geom_point()+
  geom_smooth(method = "lm")+
  ggtitle("With 0 pops")

p4 <- ggplot(df3, aes(logpop, loginv)) +
  geom_point()+
  geom_smooth(method = "lm")+
  ggtitle("Without 0 pops")

grid.arrange(p1,p2, p3, p4, nrow=2)

##Let's look at it by group, before we model 

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
  geom_smooth(method="glm",se = FALSE)
plot +  facet_wrap(~ family.name, ncol=3)

##Lmer modeling, data without 0 pops

library(lme4)
library(lmerTest)

mod.lmer.null1 <- lmer(loginv~(1|family.name)+(1|area), data=df3)
mod.lmer.null2 <- lmer(loginv~(1|family.name), data=df3)
mod.lmer.null3 <- lmer(loginv~(1|area), data=df3)

anova(mod.lmer.null1,
      mod.lmer.null2,
      mod.lmer.null3) ##So both area and family.name are accounting for important aspects of variation

mod.lmer.a <- lmer(loginv~logpop+
                     (1|family.name)+
                     (1|area), data=df3) ##Random intercept and slopes


mod.lmer.ab <- lmer(loginv~logpop+
                      (1+logpop|family.name)+
                      (1+logpop|area), data=df3) ##Random intercepts and slopes


anova(mod.lmer.a,mod.lmer.ab) ##The AIC/BIC suggest that the random intercepts is doing more heavy lifting than the slope variation

summary(mod.lmer.ab) ##Notice that the logpop is no longer significant

anova(mod.lmer.null1, mod.lmer.ab,mod.lmer.a) ##Furthermore our null model is better than the model with population

coef(mod.lmer.ab)

beta1 <- coef(mod.lmer.ab)$family.name
colnames(beta1) <- c("Intercept", "Slope")
beta1

beta2 <- coef(mod.lmer.ab)$area
colnames(beta2) <- c("Intercept", "Slope")
beta2


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

grid.arrange(p1,p2,p3,p4)

##Hierarchical clustering simulations

set.seed(4321)

x <- rnorm(12, rep(1:3, each = 4), 0.2)
y <- rnorm(12, rep(c(1, 2, 1), each = 4), 0.2)
plot(x, y, col = "pink", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

##Calculate the distance between every point

dataFrame <- data.frame(x=x, y=y)
dist(dataFrame)

rdistxy <- as.matrix(dist(dataFrame))


## Remove diagonal from consideration

diag(rdistxy)

diag(rdistxy) <- diag(rdistxy) + 100000

##Find the index of the points with minimum distance
ind <- which(rdistxy == min(rdistxy), arr.ind = TRUE)

ind

par(mfrow = c(1, 2))

plot(x, y, col = "pink", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))
points(x[ind[1, ]], y[ind[1, ]], col = "purple", pch = 19, cex = 2)

hcluster <- dist(dataFrame)
dendro <- as.dendrogram(hclust(hcluster))
plot(cut(dendro, h = 0.5)$lower[[3]])

##Find the next points

nextmin <- rdistxy[order(rdistxy)][7]
ind2 <- which(rdistxy == nextmin,arr.ind=TRUE)
ind2

##Showing agglomeration
par(mfrow = c(1, 2))
plot(x, y, col = "pink", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))
points(x[ind[1, ]], y[ind[1, ]], col = "purple", pch = 19, cex = 2)

plot(x, y, col = "pink", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))
points(x[ind[1, ]], y[ind[1, ]], col = "purple", pch = 19, cex = 2)
symbols(x=c(2.93), y=c(1), circles=0.12, add=T, inches=F, pch=20, bg="pink")

##Next agglomeration
par(mfrow = c(1, 2))

plot(x, y, col = "pink", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))
points(x[ind[1, ]], y[ind[1, ]], col = "purple", pch = 19, cex = 2)
symbols(x=c(2.93), y=c(1), circles=0.12, add=T, inches=F, pch=20, bg="pink")
points(x[ind2[1, ]], y[ind2[1, ]], col = "purple", pch = 19, bg = "purple", cex=2)

plot(x, y, col = "pink", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))
points(x[ind[1, ]], y[ind[1, ]], col = "purple", pch = 19, cex = 2)
symbols(x=c(2.93), y=c(1), circles=0.12, add=T, inches=F, pch=20, bg="purple")
points(x[ind2[1, ]], y[ind2[1, ]], col = "purple", pch = 19, bg = "purple", cex=2)

par(mfrow = c(1, 3))

plot(x, y, col = "pink", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))
symbols(x=c(2.93), y=c(1), circles=0.12, add=T, inches=F, pch=20, bg="purple")
symbols(x=c(3.15), y=c(1.1), circles=0.15, add=T, inches=F, pch=20, bg="purple", cex=2)

plot(x, y, col = "pink", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))
points(x[ind[1, ]], y[ind[1, ]], col = "purple", pch = 19, cex = 2)
symbols(x=c(2.93), y=c(1), circles=0.12, add=T, inches=F, pch=20, bg="pink")
points(x[ind2[1, ]], y[ind2[1, ]], col = "purple", pch = 19, bg = "purple", cex=2)
symbols(x=c(3.15), y=c(1.1), circles=0.15, add=T, inches=F, pch=20, bg="pink", cex=2)

hcluster <- dist(dataFrame)
clusters <- hclust(hcluster)

dendro <- as.dendrogram(clusters)
plot(cut(dendro, h = 0.5)$lower[[3]])

par(mfrow = c(1, 1))

plot(dendro)


dendro

##You can draw rectangles around clusters
par(mfrow = c(1, 1))
require(graphics)
plot(clusters)
rect.hclust(clusters, k = 2, border="red")


##ggdendro

library(ggplot2)
library(ggdendro)
library(dendextend)

ggdendrogram(clusters, theme_dendro = FALSE, dendrocut=2)

##Another way

ggdendro <- as.ggdend(dendro)
ggplot(ggdendro)

##coloring plots

hcluster <- dist(dataFrame)
clusters <- hclust(hcluster)
dendrocolor <- dendro %>%
  set("branches_k_color", k=3) %>% set("branches_lwd", 1.2) %>%
  set("labels_colors") %>% set("labels_cex", c(.9,1.2)) %>% 
  set("leaves_pch", 19) %>% set("leaves_col", c("blue", "red"))
plot(dendrocolor) ##Okay, but it looks somewhat better with ggplot

dend1 <- as.ggdend(dendrocolor)
ggplot(dend1)


##Assign groups from hierarchical cluster to data set

groups <- cutree(clusters, k=2)
dataFrame$groups <- groups 
dataFrame

##A brief introduction to the notion of internal validation
set.seed(123)

u <- rbinom(50,1,.4)

bx <- 0.5
x <-  bx*u + rnorm(50, 0, 1)
by <- -3
y <- by*u + rnorm(50, 0, 2)
bz <- 4
z <- bz*u + rnorm(50, 0, 2.5)

d <- data.frame(x,y,z)
glimpse(d)


distances <- dist(d) #Make a distance matrix

hc <- hclust(distances) #Make a hierarchical cluster

plot(hc) #plot the hierarchical cluster

##How do we test to see if U is a valid concept
##External criterion

g <- cutree(hc, k=2)
d$g <- g -1
glimpse(d)
d$u <- u

table(d$u, d$g)

chisq.test(d$u, d$g)

##Internal criteria

par(mfrow = c(1, 3))

plot(density(z))
plot(density(y))
plot(density(x))

z.null <- rnorm(50, mean(z), sd(z))
y.null <- rnorm(50, mean(y), sd(y))
x.null <- rnorm(50, mean(x), sd(x))
d.null <- data.frame(x.null,y.null,z.null)
d.obs <- data.frame(x,y,z)

distances.null <- dist(d.null) #Make a distance matrix
hc.null <- hclust(distances.null) #Make a hierarchical cluster

distances.obs <- dist(d.obs) #Make a distance matrix
hc.obs <- hclust(distances.obs) #Make a hierarchical cluster


par(mfrow = c(1, 2))

p1 <- ggdendrogram(
  as.dendrogram(hc.obs))+
  ylim(0,15)+
  ggtitle("Observed data")
p2 <- ggdendrogram(
  as.dendrogram(hc.null))+
  ylim(0,15)+
  ggtitle("Null distribution")
grid.arrange(p1,p2, ncol=2)


hc.obs$height
hc.null$height
length(hc.obs$height)
length(hc.null$height)
hc.obs$height[49] - hc.obs$height[48]
hc.null$height[49] - hc.null$height[48]


##cophenetic correlation

coph <- cophenetic(hc.obs)
cor.test(distances.obs, coph)

coph <- cophenetic(hc.null)
cor.test(distances.null, coph)
