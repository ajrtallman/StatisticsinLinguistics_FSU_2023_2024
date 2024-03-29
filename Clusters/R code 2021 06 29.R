

##Exploratory data analysis
par(mfrow=c(2,2))

set.seed(3)
x1 <- seq(4,14,by=1)
y1 <- 3 + 0.5*x1+ rnorm(11,0,0.7)
plot(x1,y1, xlim=c(0,20), ylim=c(0,16))
abline(a=3, b = 0.5 )

x2 <- c(4, 5,  6, 7, 8, 9,   10, 11,  12 ,13 ,  14)
y2 <- c(3, 4.5, 6, 7.25, 8, 8.5, 9, 9.25, 9  ,8.5, 8)
lm(y2~x2)
plot(x2,y2, xlim=c(0,20), ylim=c(0,16))
abline(a=3, b = 0.5 )

x3 <- seq(4,14)
y3 <- 4.75 + 0.25*x3
y3[10] = 15
lm(y3~x3)
plot(x3,y3, xlim=c(0,20), ylim=c(0,16))
abline(a=3, b = 0.5 )

x4 <- rep(8,10)
x4[11] <- 18 
y4 <- seq(5,10, by =0.5)
y4[11] <- 12.25
lm(y4~x4)
plot(x4,y4, xlim=c(0,20), ylim=c(0,16))
abline(a=3.25, b = 0.5 )

##Graphs lie, so you need algebra

x5 <- c(1,1,2,2,3,3,4,4,5,5,5,5,6,6,6,6,6,6,7,7,7,7,8,8,9,9,10,10,11,11)
par(mfrow=c(2,2))
hist(x5, col="skyblue3")
hist(x5, col="skyblue3", breaks=10)
hist(x5, col="skyblue3", breaks=5)
hist(x5, col="skyblue3", breaks=c(0,1,2,3,4,5,6,7,8,9,10,11))


##Hiding structure

group1 <- NULL
group1$y <- c(11,11,10,10,9,9,8,8,7,7,7,7,6,6,6,6,6,6,5,5,5,5,4,4,3,3,2,2,1,1)
group1 <- as.data.frame(group1)
group1$group <- 1
group2 <- NULL
group2$y <- c(11,11,11,11,11,11,11, 8,8,8,8,8,8,8,8,4,4,4,4,4,4,4,4,1,1,1,1,1,1,1)
group2 <- as.data.frame(group2)
group2$group <- 2
groups <- rbind(group1,group2)

par(mfrow=c(1,2))
boxplot(group1$y, xlab="Group 1")
boxplot(group2$y, xlab= "Group 2")

##Dot plot

library(ggplot2)

ggplot(groups, aes(x=as.factor(group), y=y))+
  geom_violin(trim=FALSE)+
  geom_dotplot(binaxis='y', stackdir='center')


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
plot(cut(dendro, h = 0.4)$lower[[3]])

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
library(tidyverse)
library(gridExtra)


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

##k-means - assign random medoids

par(mfrow = c(1, 2))

set.seed(1234)

x <- rnorm(12, mean = rep(1:3, each=4), sd=0.2)
y <- rnorm(12, mean = rep(c(1,2,1), each=4), sd=0.2)
plot(x,y, col="blue", pch=19, cex=2)
text(x+0.05, y +0.05, labels= as.character(1:12))
plot(x,y, col="blue", pch=19, cex=2)
text(x+0.05, y +0.05, labels= as.character(1:12))
points(c(1,1.75,2.5), c(2,1,1.5), col = c("red","orange","purple"), pch = 4, cex=2, lwd=3)


##Assign every data point to its closest centroid

par(mfrow = c(1, 1))

set.seed(1234)
x <- rnorm(12, mean = rep(1:3, each=4), sd=0.2)
y <- rnorm(12, mean = rep(c(1,2,1), each=4), sd=0.2)
plot(x,y, col="blue", pch=19, cex=2)
text(x+0.05, y +0.05, labels= as.character(1:12))
points(c(1,1.75,2.5), c(2,1,1.5), col = c("red","orange","purple"), pch = 4, cex=2, lwd=3)
points(c(x[4],x[8]), c(y[4],y[8]), col="red", pch=19, cex=2)
points(c(x[1],x[2],x[3]), c(y[1],y[2],y[3]), col="orange", pch=19, cex=2)
points(c(x[5], x[6], x[7], x[9], x[10], x[11], x[12]), c(y[5], y[6], y[7], y[9], y[10], y[11], y[12]), col="purple", pch=19, cex=2)



##Assign every data point to its closest centroid

par(mfrow = c(1, 2))

set.seed(1234)
x <- rnorm(12, mean = rep(1:3, each=4), sd=0.2)
y <- rnorm(12, mean = rep(c(1,2,1), each=4), sd=0.2)
plot(x,y, col="purple", pch=19, cex=2)
text(x+0.05, y +0.05, labels= as.character(1:12))
points(c(1,1.75,2.5), c(2,1,1.5), col = c("red","orange","purple"), pch = 4, cex=2, lwd=3)
points(c(x[4],x[8]), c(y[4],y[8]), col="red", pch=19, cex=2)
points(c(x[1],x[2],x[3]), c(y[1],y[2],y[3]), col="orange", pch=19, cex=2)
points(c(x[5], x[6], x[7], x[9], x[10], x[11], x[12]), c(y[5], y[6], y[7], y[9], y[10], y[11], y[12]), col="purple", cex=2, pch=19)

##Reassign centroids to their central point


set.seed(1234)
x <- rnorm(12, mean = rep(1:3, each=4), sd=0.2)
y <- rnorm(12, mean = rep(c(1,2,1), each=4), sd=0.2)
plot(x,y, col="purple", pch=19, cex=2)
text(x+0.05, y +0.05, labels= as.character(1:12))

points(c(x[4],x[8]), c(y[4],y[8]), col="red", pch=19, cex=2)
points(c(x[1],x[2],x[3]), c(y[1],y[2],y[3]), col="orange", pch=19, cex=2)
points(c(x[5], x[6], x[7], x[9], x[10], x[11], x[12]), c(y[5], y[6], y[7], y[9], y[10], y[11], y[12]), col="purple", cex=2, pch=19)
points(c(1.3,1,2.5), c(1.8,1,1.5), col = c("red","orange","purple"), pch = 4, cex=2, lwd=3)

##Reassign points

par(mfrow = c(1, 2))

set.seed(1234)
x <- rnorm(12, mean = rep(1:3, each=4), sd=0.2)
y <- rnorm(12, mean = rep(c(1,2,1), each=4), sd=0.2)
plot(x,y, col="blue", pch=19, cex=2)
text(x+0.05, y +0.05, labels= as.character(1:12))
points(c(1.2,1,2.5), c(1.75,1,1.5), col = c("red","orange","purple"), pch = 4, cex=2, lwd=3)

points(c(x[7],x[8]), c(y[7],y[8]), col="red", pch=19, cex=2)
points(c(x[1],x[2],x[3],x[4]), c(y[1],y[2],y[3],y[4]), col="orange", pch=19, cex=2)
points(c(x[5], x[6], x[9], x[10], x[11], x[12]), c(y[5], y[6], y[9], y[10], y[11], y[12]), col="purple", cex=2, pch=19)


##Reassign centroids

set.seed(1234)
x <- rnorm(12, mean = rep(1:3, each=4), sd=0.2)
y <- rnorm(12, mean = rep(c(1,2,1), each=4), sd=0.2)
plot(x,y, col="blue", pch=19, cex=2)
text(x+0.05, y +0.05, labels= as.character(1:12))


points(c(x[7],x[8]), c(y[7],y[8]), col="red", pch=19, cex=2)
points(c(x[1],x[2],x[3],x[4]), c(y[1],y[2],y[3],y[4]), col="orange", pch=19, cex=2)
points(c(x[5], x[6], x[9], x[10], x[11], x[12]), c(y[5], y[6], y[9], y[10], y[11], y[12]), col="purple", cex=2, pch=19)

points(c(1.887863,0.8904553,2.60017), c(2.157866,1.006871,1.274675), col = c("red","orange","purple"), pch = 4, cex=2, lwd=3)


##

df <- data.frame(x,y)
kmeansobj <- kmeans(df, centers=3)
names(kmeansobj)
kmeansobj$cluster
df$groups <- kmeansobj$cluster

df

par(mfrow = c(1, 1))
plot(x,y, col="blue", pch=19, cex=2)
text(x+0.05, y +0.05, labels= as.character(1:12))

points(c(x[1],x[4]), c(y[1],y[4]), col="red", pch=19, cex=2)
points(c(x[2],x[3]), c(y[2],y[3]), col="orange", pch=19, cex=2)
points(c(x[5], x[6], x[7], x[8], x[9], x[10], x[11], x[12]), c(y[5], y[6], y[7], y[8], y[9], y[10], y[11], y[12]), col="purple", cex=2, pch=19)

##Empirical analysis of Chacobo data
# packages
library(tidyverse) # loads ggplot2, dplyr, tidyr, among others
library(factoextra)
library(cluster)
library(ggdendro)
library(clValid) # for Dunn coefficient

##Dendrogram with real data

clitics.raw <- read.csv("/Users/Adan Tallman/Desktop/Chacobo_clitic_data.csv")

db <- clitics.raw %>%  select(ALLOMORPHY_SEM:VARIABLE.ORDERING) %>% mutate_if(is.character, as.factor) 

glimpse(db)

db.dist <- daisy(db, metric="gower")

db.hclust <- hclust(db.dist,method="ward.D2")

p1 <- ggdendrogram(
  as.dendrogram(db.hclust))+
  ylim(0,1.75)+
  ggtitle("Morphemes in Chacobo")
p1
names(db)

ALLOMORPHY_SEM <- sample(db$ALLOMORPHY_SEM , 69, replace=TRUE)    
ALLOMORPHY_LEX <-sample(db$ALLOMORPHY_LEX , 69, replace=TRUE)                  
ALLOMORPHY_PHON <-sample(db$ALLOMORPHY_PHON , 69, replace=TRUE)  
PRODUCTIVITY <-sample(db$PRODUCTIVITY, 69, replace=TRUE)                   
H_TONE_BLOCK <-sample(db$H_TONE_BLOCK, 69, replace=TRUE)  
LONG_DIST_TONE_REDUCTION <-sample(db$LONG_DIST_TONE_REDUCTION, 69, replace=TRUE)      
MINIMALITY_DOMAIN <-sample(db$MINIMALITY_DOMAIN, 69, replace=TRUE)                 
REDUPLICATION_OBLIGATORY_COPYING <-sample(db$REDUPLICATION_OBLIGATORY_COPYING, 69, replace=TRUE)  
REDUPLICATION_VARIABLE_COPYING <- sample(db$REDUPLICATION_VARIABLE_COPYING, 69, replace=TRUE)
REDUPLICATION_FREE_COPYING <-sample(db$REDUPLICATION_FREE_COPYING, 69, replace=TRUE)     
INTERRUPTABILITY_FREEFORM  <- sample(db$INTERRUPTABILITY_FREEFORM , 69, replace=TRUE)       
INTERRUTABILITY_NP <- sample(db$INTERRUTABILITY_NP , 69, replace=TRUE)              
EXTRACTION.TO.SECOND.POSITION <- sample(db$EXTRACTION.TO.SECOND.POSITION , 69, replace=TRUE) 
AUXILIARY.FUNCTION <- sample(db$AUXILIARY.FUNCTION, 69, replace=TRUE)      
CLAUSE_FINAL <- sample(db$CLAUSE_FINAL, 69, replace=TRUE)
SCOPE_OVER_COORDINATE.CLAUSES  <- sample(db$SCOPE_OVER_COORDINATE.CLAUSES, 69, replace=TRUE) 
VARIABLE_ODERING_SCOPE <- sample(db$VARIABLE_ODERING_SCOPE, 69, replace=TRUE) 
VARIABLE.ORDERING  <- sample(db$VARIABLE.ORDERING, 69, replace=TRUE) 

rando <- as_tibble(cbind(ALLOMORPHY_SEM, ALLOMORPHY_LEX, ALLOMORPHY_PHON,
                         PRODUCTIVITY, H_TONE_BLOCK, LONG_DIST_TONE_REDUCTION,
                         MINIMALITY_DOMAIN, REDUPLICATION_OBLIGATORY_COPYING,
                         REDUPLICATION_VARIABLE_COPYING, REDUPLICATION_FREE_COPYING,
                         INTERRUPTABILITY_FREEFORM, INTERRUTABILITY_NP,
                         EXTRACTION.TO.SECOND.POSITION, EXTRACTION.TO.SECOND.POSITION,
                         AUXILIARY.FUNCTION, CLAUSE_FINAL, 
                         SCOPE_OVER_COORDINATE.CLAUSES, VARIABLE_ODERING_SCOPE,
                         VARIABLE.ORDERING)) %>% mutate_if(is.character, as.factor)


sample(db$ALLOMORPHY_LEX , 69, replace=TRUE)


glimpse(rando)

rando.dist <- daisy(rando, metric="gower")

rando.hclust <- hclust(rando.dist,method="ward.D2")

p2 <- ggdendrogram(
  as.dendrogram(rando.hclust))+
  ylim(0,1.75)+
  ggtitle("A null hypothesis")

grid.arrange(p1,p2, ncol=2)

coph <- cophenetic(db.hclust)
cor.test(db.dist, coph)

coph.null <- cophenetic(rando.hclust)
cor.test(rando.dist, coph.null)

db.hclust$height
rando.hclust$height
length(rando.hclust$height)
length(hc.null$height)
db.hclust$height[68] - db.hclust$height[67]
rando.hclust$height[49] - rando.hclust$height[48]


##Mixtec

mixtec.raw <- read.csv("/Users/Adan Tallman/Desktop/clitics_smd.csv")
glimpse(mixtec.raw)




db.smd <- mixtec.raw %>%  select(Order:Fossilized) %>% mutate_if(is.character, as.factor) 

db.smd.dist <- daisy(db.smd, metric="gower")

db.smd.hclust <- hclust(db.smd.dist,method="ward.D2")

p1 <- ggdendrogram(
  as.dendrogram(db.hclust))+
  ylim(0,1.7)+
  ggtitle("Mixtec clitics")
p1



set.seed(123)

glimpse(db.smd)

a1 <- sample(db.smd$AllomorphySeg , 42, replace=TRUE)  
a2 <- sample(db.smd$AllomorphyTone, 42, replace=TRUE)
i <- sample(db.smd$Interruptability, 42, replace=TRUE)
fo <- sample(db.smd$FreeOccurrence, 42, replace=TRUE)
p <- sample(db.smd$ProminenceProj, 42, replace=TRUE)
c <- sample(db.smd$CatCombination, 42, replace=TRUE)
fs <- sample(db.smd$Fossilized, 42, replace=TRUE)


rando <- as_tibble(cbind(a1,a2,i,fo,p,c,fs)) %>% mutate_if(is.character, as.factor)

rando.dist <- daisy(rando, metric="gower")

rando.hclust <- hclust(rando.dist,method="ward.D2")

p2 <- ggdendrogram(
  as.dendrogram(rando.hclust))+
  ylim(0,1.7)+
  ggtitle("A null hypothesis")

grid.arrange(p1,p2, ncol=2)

rando.hclust$height 

length(rando.hclust$height)
length(db.smd.hclust$height)

(rando.hclust$height[40]) /  sum(rando.hclust$height)

(db.smd.hclust$height[40]) /  sum(db.smd.hclust$height)
