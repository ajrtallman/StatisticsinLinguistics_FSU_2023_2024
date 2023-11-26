library(tidyverse)
library(ggridges)
library(HDInterval)

##Code from online

d1 <- as.data.frame(rnorm(1000, 1, .5))
d1$type <- "functional"
colnames(d1) <- c("durations", "type") 

d2 <- as.data.frame(rnorm(1000, 2, .4))
d2$type<-"lexical"
colnames(d2) <- c("durations", "type") 

durations <-rbind(d1, d2)

durations$group <- "1"

ggplot(durations, aes(x = durations, y=1, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.4, 0.6)
  ) +
  scale_fill_manual(
    name = "Probability", values = c("#A0A0A0A0", "#FF0000A0", "#A0A0A0A0"),
    labels = c("(0, 0.4]", "(0.4, 0.6]", "(0.6, 1]")
  )

quantile(durations$durations,probs=c(0.1,0.9), type=5)

ggplot(durations, aes(x = durations, y=1, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.1)
  ) +
  scale_fill_manual(
    name = "Probability", values = c("#FF0000A0", "#A0A0A0A0"),
    labels = c("(0, 0.1]", "(0.1, 1]")
  )


t <- rnorm(100000, m=0, sd=1)
y<- dnorm(t)
t.data<-as.data.frame(cbind(t,y))
t.data$group<-"1"
t.data

ggplot(t.data, aes(x = t, y=group, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.05, 0.95)
  ) +
  scale_fill_manual(
    name = "Probability", values = c("#FF0000A0", "#A0A0A0A0", "#FF0000A0"),
    labels = c("(0, 0.05]", "(0.05, 0.95]", "(0.95, 1]")
  )

