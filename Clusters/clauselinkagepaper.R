## New script for clause-linkage paper

install.packages("gdata")
install.packages("dendextend")
install.packages("cluster")
install.packages("factoextra")
install.packages("ggdendro")
install.packages("phangorn")
install.packages("pacman")
install.packages("tidyverse")
install.packages("xtable")

library(tidyverse)
library(gdata)
library(dendextend)
library(cluster)
library(factoextra)
library(ggdendro)
library(phangorn)
library(pacman)
library(tidyverse)
library(gridExtra)
library(xtable)

p_load(cluster,
       here,
       stringi,
       tidyverse)

#Loading the files

cl <- read_csv("/Users/User/Documents/GitHub/Clause_linkage_paper/07_newdata/bickelclauselinkageandchacobo3.csv") %>%
  mutate(across(everything(), trimws)) %>%
  mutate(across(everything(), stri_trans_nfc))

glimpse(cl)

cl <- cl %>%
  unite(Language:Label, col = "LabelNN", sep = "_", remove = FALSE, na.rm = TRUE) %>%
  mutate(LabelNN = str_replace_all(LabelNN, ":", ".")) %>%
  mutate(LabelNN = str_replace_all(LabelNN, "/", ".")) %>%
  mutate(LabelNN = str_remove_all(LabelNN, "’")) %>%
  mutate(LabelNN = stri_trans_general(LabelNN, id = "Latin-ASCII")) %>%
  mutate(across(`ILL-scope`:Layer, as.factor)) %>%
  arrange(Language, Label)

glimpse(cl)

# check for duplicates in labels (this gives an error in splitstree)
anyDuplicated(cl$LabelNN)

# create distance matrix
cl_variables <- cl %>%
  select(`ILL-scope`:Layer)
cl_dist <- daisy(cl_variables, metric = "gower")

# add labels back
cl_dist <- as.matrix(cl_dist, labels=TRUE)
rownames(cl_dist) <- cl$LabelNN

# export nexus file for splitstree
write.nexus.dist(cl_dist, file = "/Users/User/Documents/GitHub/Clause_linkage_paper/07_newdata/clause_linkage_dist.nex")

## A distance matrix to display

cl_dist2 <- cl_dist
colnames(cl_dist2) <-cl$LabelNN

head(cl_dist2)

write.csv(cl_dist2, file = "/Users/User/Documents/GitHub/Clause_linkage_paper/07_newdata/distmatrix.csv")

## The language specific data set

rd <- read.table("/Users/User/Documents/GitHub/Clause_linkage_paper/07_newdata/chadependentclauses3.csv", sep=",", header=TRUE)

head(rd)

##Creating a distance matrix

## Strategy 1: Creating some continuous variables

whv <- select(rd, WH.NP.EXT.MAIN:WH.ADV.EXT.DEP)
whv[whv=="ok"] <- 2 #It's not clear that this tells us anything about the distinction between both coordinative and adjunct clauses ban assymetric extraction
whv[whv=="local"] <- 1
whv[whv=="banned"] <- 0
whv <- whv %>% mutate_if(is.character, as.numeric)
assym.extr <- (rowSums(whv) / max(rowSums((whv)))) #Not clear what an overall extraction variable would tell us
rd$assym.extr <- as.numeric(assym.extr)
#Nonfiniteness variable
fv <- select(rd, Neg.marked:hueni.marked)
fv[fv=="ok"] <- 0
fv[fv=="banned"] <-1
fv <-  fv %>% mutate_if(is.character, as.numeric)
nonfiniteness <- rowSums(fv) / max(rowSums((fv)))
rd$nonfiniteness <- as.numeric(nonfiniteness)
df <-  select(rd, referential.function, noun.modifying, ILL.scope, T.scope, Neg.scope, T.mark, WH.NP.EXT.MAIN:WH.ADV.EXT.DEP, nonfiniteness, FOC, Position)
df[is.na(df)] <- "ok"
df

##Without developing continuous variables
#df <- select(rd, referential.function:hueni.marked)
#df <-  select(df, Symmetry:hueni.marked)
#df[is.na(df)] <- "ok"


#We assign the variable-names

varnames <- rd$Label
rownames(df) <- varnames
df
#We make all the variables factors

df <-  df %>% mutate_if(is.character, as.factor)

#We create a distance matrix using gower’s distance metric, which can handle categorical data.

db.dist <- daisy(df, metric="gower")

db.dist2 <- as.matrix(db.dist, labels=TRUE)


#Reorder so it can be printed into the word doc 
col.order <- c("prior:ss", "prior:sa", "concur:ss","concur:sa", "subseq:ss/a", "interrupt:ss/a", "quick:ss/a", "prior:ds/a", "concur:ds/a", "subseq:ds/a", "nmd", "nmlz:agt", "nmlz:purp")
row.order <- c("prior:ss", "prior:sa", "concur:ss","concur:sa", "subseq:ss/a", "interrupt:ss/a", "quick:ss/a", "prior:ds/a", "concur:ds/a", "subseq:ds/a", "nmd", "nmlz:agt", "nmlz:purp")
db.dist_ordered<- db.dist2[row.order, col.order]


write.csv(db.dist_ordered, file = "/Users/User/Documents/GitHub/Clause_linkage_paper/07_newdata/distmatrixchacobo.csv")

# db.dist2
# 
# db.dist
# 
# class(db.dist2)
# 
# db.dist3 <- as.data.frame(db.dist2)
# 
# glimpse(db.dist3)
# 
# glimpse(db.dist3)
# 
# head(db.dist3)
# class(db.dist3)
# 
# db.dist3$`prior:ss`
# 
# 
# 
# db.dist.df <- db.dist3 %>%
#   as_tibble() %>%
#    select(`prior:ss`, `prior:sa`, `concur:ss`, `concur:sa`, `subseq:ss/a`, `interrupt:ss/a`, `quick:ss/a`, `prior:ds/a`, `concur:ds/a`, nmd, `nmlz:agt`, `nmlz:purp`) %>%
#   add_column(names = c("prior:ss", "prior:sa", "concur:ss","concur:sa", "subseq:ss/a", "interrupt:ss/a", "quick:ss/a", "prior:ds/a", "concur:ds/a", "nmd", "nmlz:agt", "nmlz:purp", "")) %>%
#   column_to_rownames(var = "names")
# 
# db.dist.df
# glimpse(db.dist.df)


#write.csv(db.dist2, file = "/Users/User/Documents/GitHub/Clause_linkage_paper/07_newdata/distmatrixchacobo.csv")


db.hclust<-hclust(db.dist, method="ward.D2")

db.dist2

plot(cut(as.dendrogram(db.hclust), h=0.35054060)$lower[[2]])
get_branches_heights(as.dendrogram(db.hclust))


db_dendro <- ggdendrogram(db.hclust, rotate = FALSE, theme_dendro = FALSE)+theme(axis.text=element_text(size=12),                                                                                axis.title=element_text(size=14,face="bold"))+
  xlab("Constructions")+
  ylab("Distance")+
  ylim(0,1)
db_dendro

# dist(db.dist)
# rdistxy <- as.matrix(dist(db.dist))
# 
# hcluster <- dist(db.dist2)
# dendro <- as.dendrogram(hclust(hcluster))
# plot(cut(dendro, h = 0.05128205)$lower[[6]])
# 
# dendro
# 
# get_branches_heights(dendro)



##This code is used to make a scale from more subordinate to more coordinate like based on my understanding of how these criteria relate to the distinction

illscope.i <- ifelse(rd$ILL.scope == "local", 1, 0)
ref.i <- ifelse(rd$referential.function=="na",0,1)
noun.modify.i <- ifelse(rd$noun.modifying=="yes", 1, 0)
neg.scope.i <- ifelse(rd$Neg.scope=="local", 1, 0) #I'm not sure about this one
#whv <- select(rd, WH.NP.EXT.MAIN:WH.ADV.EXT.DEP)
#whv[whv=="ok"] <- 2 #It's not clear that this tells us anything about the distinction between both coordinative and adjunct clauses ban assymetric extraction
#whv[whv=="local"] <- 1
#whv[whv=="banned"] <- 0
#whv <- whv %>% mutate_if(is.character, as.numeric)
#assym.extr.i <- 1 - (rowSums(whv) / max(rowSums((whv)))) #Not clear what an overall extraction variable would tell us
whv2 <- select(rd, WH.NP.EXT.MAIN, WH.ADV.EXT.MAIN)
whv2[whv2=="ok"] <- 2 
whv2[whv2=="local"] <- 1
whv2[whv2=="banned"] <- 0
whv2 <- whv2 %>% mutate_if(is.character, as.numeric)
extr.main.i <- (rowSums(whv2) / max(rowSums((whv2))))
#atb.extr.i <- ifelse(rd$WH.NP.ATB.EXT == "ok", 0, 1)
foc.i <- ifelse(rd$FOC == "banned", 0, 1)
position.i <- ifelse(rd$Position =="fixed:pre-main", 0,1)
#Nonfiniteness variable
fv <- select(rd, Neg.marked:hueni.marked)
fv[fv=="ok"] <- 1
fv[fv=="banned"] <-0
fv <-  fv %>% mutate_if(is.character, as.numeric)
nonfininteness <- rowSums(fv) / max(rowSums((fv)))
#Center-embed variable
#center.embed.i <- ifelse(rd$Center.embed.pa="ok", 1, 0) 

df.i <- data.frame(illscope.i, ref.i, noun.modify.i, neg.scope.i,  foc.i, position.i, nonfininteness, extr.main.i)

##This is the code that gives you the ranking

df.i2 <- df.i
df.i2$cnstr <- rd$Label
df.i2$subordination.ranking <- rowSums(df.i)
df.i2$cnstr <-factor(df.i2$cnstr)
df.i2 %>%
  arrange(subordination.ranking) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(name=factor(cnstr, levels=cnstr)) %>%   # This trick update the factor levels
  ggplot(aes(x=reorder(cnstr,-subordination.ranking), y=subordination.ranking)) +
    ylab("Coordination-subordination continuum")+
    geom_segment(aes(xend=name, yend=0)) +
    geom_point(size=4, color="maroon") +
    coord_flip() +
    theme_bw() +
    ylim(0,7)

df.i

##Setting the seed

set.seed(1234)

coph_height <- function(x, y){
  coph <- cophenetic(x)
  cor <- round(cor(y, coph), 4)
  height <- x$height
  height_diff <- round(height[length(height)]-height[length(height)-1], 4)
  returnlist <- c("heightdiff" = height_diff, "coph" = cor)
}

head(df)

set.seed(222)

referential.function <- as.character(sample(df$referential.function, 13, replace=T)) #check
noun.modifying <- as.character(sample(df$noun.modifying, 13, replace=T)) #check
ILL.scope <- as.character(sample(df$ILL.scope, 13, replace=T)) #check
Neg.scope <- as.character(sample(df$Neg.scope, 13, replace=T)) #check
T.scope <- as.character(sample(df$T.scope, 13, replace=T)) #check
#ILL.mark <- as.character(sample(df$ILL.mark, 13, replace=T)) #check
T.mark <- as.character(sample(df$T.mark, 13, replace=T)) #check
#Symmetry <- as.character(sample(df$Symmetry, 13, replace=T)) #check
WH.NP.EXT.MAIN <- as.character(sample(df$WH.NP.EXT.MAIN, 13, replace=T)) #check
WH.NP.EXT.DEP <- as.character(sample(df$WH.NP.EXT.DEP, 13, replace=T)) #check
WH.ADV.EXT.MAIN <- as.character(sample(df$WH.NP.EXT.MAIN, 13, replace=T)) #check
WH.ADV.EXT.DEP <- as.character(sample(df$WH.NP.EXT.DEP, 13, replace=T)) #check
#WH.NP.ATB.EXT <- as.character(sample(df$WH.NP.ATB.EXT, 13, replace=T)) #check
FOC <- as.character(sample(df$FOC, 13, replace=T)) #check
#WH.NP.dep <- as.character(sample(df$WH.NP.dep, 13, replace=T))
Position <- as.character(sample(df$Position, 13, replace=T))
#Min.left.edge <- as.character(sample(df$Min.left.edge, 13, replace=T))
#Min.right.edge <- as.character(sample(df$Min.right.edge, 13, replace=T))
#Max.left.edge <- as.character(sample(df$Max.left.edge, 13, replace=T))
#Max.right.edge <- as.character(sample(df$Max.right.edge, 13, replace=T))
#Center.embed.pa <- as.character(sample(df$Center.embed.pa, 13, replace=T))
#Center.embed.case <- as.character(sample(df$Center.embed.case, 13, replace=T))
#Neg.marked <- as.character(sample(df$Neg.marked, 13, replace=T)) #check
Nonfiniteness <- as.character(sample(df$nonfiniteness, 13, replace=T))
#ya <- as.character(sample(df$ya.mark, 13, replace=T))
#pao <- as.character(sample(df$pao.marked, 13, replace=T))
#bahina <- as.character(sample(df$bahina.marked, 13, replace=T))
#tapi <- as.character(sample(df$tapi.marked, 13, replace=T))
#yo <- as.character(sample(df$yo.marked, 13, replace=T))
#shari <- as.character(sample(df$shari.marked, 13, replace=T))
#hita <- as.character(sample(df$hita.marked, 13, replace=T))
#yamet <- as.character(sample(df$yamet.marked, 13, replace=T))
#quiha <- as.character(sample(df$quiha.marked, 13, replace=T))
#ni <- as.character(sample(df$ni.marked, 13, replace=T))
#AM <- as.character(sample(df$AM.marked, 13, replace=T))
#xe <- as.character(sample(df$xe.marked, 13, replace=T))
#quea <- as.character(sample(df$quea.marked, 13, replace=T))
#cara <- as.character(sample(df$cara.marked, 13, replace=T))
#bequi <- as.character(sample(df$bequi.marked, 13, replace=T))
#roha <- as.character(sample(df$roha.marked, 13, replace=T))
#tëquën <- as.character(sample(df$tëquën.marked, 13, replace=T))
#tiarihi <- as.character(sample(df$tiarihi.marked, 13, replace=T))
#pe <- as.character(sample(df$pe.marked, 13, replace=T))
#huesti <- as.character(sample(df$huesti.marked, 13, replace=T))
#rabe <- as.character(sample(df$rabe.marked, 13, replace=T))
#hueni <- as.character(sample(df$hueni.marked, 13, replace=T))

sim_db <- as_tibble(cbind(FOC, referential.function, noun.modifying, ILL.scope, WH.NP.EXT.DEP, WH.NP.EXT.MAIN, WH.ADV.EXT.DEP, WH.NP.EXT.MAIN, Position, Nonfiniteness, T.scope, T.mark, Neg.scope)) %>% 
  mutate_if(is.character, as.factor)

sim_db

sim_db <- as.data.frame(sim_db)
rownames(sim_db) <- c("simcnstr_1", "simcnstr_2", "simcnstr_3", "simcnstr_4", "simcnstr_5", "simcnstr_6", "simcnstr_7", "simcnstr_8", "simcnstr_9", "simcnstr_10", "simcnstr_11", "simcnstr_12", "simcnstr_13") 

# hierarchical clustering
set.seed(200)
sim_dist <- daisy(sim_db, metric = "gower")
sim_hclust <- hclust(sim_dist, method="ward.D2")
# plot results
sim_dendro<- ggdendrogram(sim_hclust, rotate = FALSE, theme_dendro =FALSE)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+
  xlab("Constructions")+
  ylab("Distance")+
  ylim(0,1)
## Scale for y is already present.
## Adding another scale for y, which will replace the existing scale.
sim_dendro

grid.arrange(db_dendro, sim_dendro, nrow=1)
##I need to go back and change the discussion somewhat in the paper

sim_ch<-coph_height(sim_hclust, sim_dist)
sim_ch
db.ch<-coph_height(db.hclust, db.dist)
db.ch

sim1000 <- lapply(1:1000, function(s) {
  referential.function <- as.character(sample(df$referential.function, 13, replace=T)) #check
  noun.modifying <- as.character(sample(df$noun.modifying, 13, replace=T)) #check
  ILL.scope <- as.character(sample(df$ILL.scope, 13, replace=T)) #check
  Neg.scope <- as.character(sample(df$Neg.scope, 13, replace=T)) #check
  T.scope <- as.character(sample(df$T.scope, 13, replace=T)) #check
  #ILL.mark <- as.character(sample(df$ILL.mark, 13, replace=T)) #check
  T.mark <- as.character(sample(df$T.mark, 13, replace=T)) #check
  #Symmetry <- as.character(sample(df$Symmetry, 13, replace=T)) #check
  WH.NP.EXT.MAIN <- as.character(sample(df$WH.NP.EXT.MAIN, 13, replace=T)) #check
  WH.NP.EXT.DEP <- as.character(sample(df$WH.NP.EXT.DEP, 13, replace=T)) #check
  WH.ADV.EXT.MAIN <- as.character(sample(df$WH.NP.EXT.MAIN, 13, replace=T)) #check
  WH.ADV.EXT.DEP <- as.character(sample(df$WH.NP.EXT.DEP, 13, replace=T)) #check
  #WH.NP.ATB.EXT <- as.character(sample(df$WH.NP.ATB.EXT, 13, replace=T)) #check
  FOC <- as.character(sample(df$FOC, 13, replace=T)) #check
  #WH.NP.dep <- as.character(sample(df$WH.NP.dep, 13, replace=T))
  Position <- as.character(sample(df$Position, 13, replace=T))
  #Min.left.edge <- as.character(sample(df$Min.left.edge, 13, replace=T))
  #Min.right.edge <- as.character(sample(df$Min.right.edge, 13, replace=T))
  #Max.left.edge <- as.character(sample(df$Max.left.edge, 13, replace=T))
  #Max.right.edge <- as.character(sample(df$Max.right.edge, 13, replace=T))
  #Center.embed.pa <- as.character(sample(df$Center.embed.pa, 13, replace=T))
  #Center.embed.case <- as.character(sample(df$Center.embed.case, 13, replace=T))
  #Neg.marked <- as.character(sample(df$Neg.marked, 13, replace=T)) #check
  Nonfiniteness <- as.character(sample(df$nonfiniteness, 13, replace=T))
  #ya <- as.character(sample(df$ya.mark, 13, replace=T))
  #pao <- as.character(sample(df$pao.marked, 13, replace=T))
  #bahina <- as.character(sample(df$bahina.marked, 13, replace=T))
  #tapi <- as.character(sample(df$tapi.marked, 13, replace=T))
  #yo <- as.character(sample(df$yo.marked, 13, replace=T))
  #shari <- as.character(sample(df$shari.marked, 13, replace=T))
  #hita <- as.character(sample(df$hita.marked, 13, replace=T))
  #yamet <- as.character(sample(df$yamet.marked, 13, replace=T))
  #quiha <- as.character(sample(df$quiha.marked, 13, replace=T))
  #ni <- as.character(sample(df$ni.marked, 13, replace=T))
  #AM <- as.character(sample(df$AM.marked, 13, replace=T))
  #xe <- as.character(sample(df$xe.marked, 13, replace=T))
  #quea <- as.character(sample(df$quea.marked, 13, replace=T))
  #cara <- as.character(sample(df$cara.marked, 13, replace=T))
  #bequi <- as.character(sample(df$bequi.marked, 13, replace=T))
  #roha <- as.character(sample(df$roha.marked, 13, replace=T))
  #tëquën <- as.character(sample(df$tëquën.marked, 13, replace=T))
  #tiarihi <- as.character(sample(df$tiarihi.marked, 13, replace=T))
  #pe <- as.character(sample(df$pe.marked, 13, replace=T))
  #huesti <- as.character(sample(df$huesti.marked, 13, replace=T))
  #rabe <- as.character(sample(df$rabe.marked, 13, replace=T))
  #hueni <- as.character(sample(df$hueni.marked, 13, replace=T))
  # combine to df
  sim_db <- as_tibble(cbind(FOC, referential.function, noun.modifying, ILL.scope, WH.NP.EXT.DEP, WH.NP.EXT.MAIN, WH.ADV.EXT.DEP, WH.NP.EXT.MAIN, Position, Nonfiniteness,  T.scope, T.mark, Neg.scope)) %>% 
    mutate_if(is.character, as.factor)
  # compute distances
  sim_dist <- daisy(sim_db, metric = "gower")
  # apply hierarchical clustering
  sim_hclust <- hclust(sim_dist, method="ward.D2")
  # compute height and cophenetic distances
  sim_ch <- coph_height(sim_hclust, sim_dist)}
)

# convert to df
sim1000_df <- as.data.frame(do.call(rbind, sim1000))
# means of simulated languages
summary(sim1000_df)

# plot cophenetic distance and height difference
sim_comp <- ggplot(sim1000_df, aes(x = coph, y = heightdiff)) + geom_point(size = 2, shape = 1) +
  geom_point(aes(x=0.7354,y=0.0744),colour="red", size =4)+
  labs(x = "Cophenetic Correlation", y = "Height Difference (coordination-subordination)")
sim_comp

db.ch

dplot1<-ggplot(sim1000_df, aes(x=coph))+
  geom_density(color="black", fill="grey")+
  geom_point(aes(x=0.9381, y=0),colour="red", size =4)+
  xlab("Cophenetic correlations")
dplot2<- ggplot(sim1000_df, aes(x=heightdiff))+
  geom_density(color="black", fill="grey")+
  geom_point(aes(x=0.2134, y=0),colour="red", size =4)+
  xlab("Height differences between the first and second partition")
grid.arrange(dplot1, dplot2, nrow=2)

db.ch<-coph_height(db.hclust, db.dist)
db.ch
cdf.coph <- ecdf(sim1000_df$coph)
cdf.coph(0.9138)
## 1
1-1
## [1] 0.182
cdf.heightdiff <- ecdf(sim1000_df$heightdiff)
cdf.heightdiff(0.1989)
## [1] 0.666
1-0.666
## [1] 0.334
