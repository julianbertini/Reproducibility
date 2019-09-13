library(ggplot2)
library(RMKdiscrete)

## set the directory
setwd("/Users/julianbertini/Desktop/BSD-QBio5/tutorials/reproducibility")

arth.data <- read.csv("data/cole_arthropod_Data_1946.csv")
weevil.data <- read.csv("data/mitchell_weevil_egg_data_1975.csv")

names(arth.data) <- c("k_arthropods","c_boards_with_k_spiders","c_boards_with_k_sowbugs")
names(weevil.data) <- c("k_eggs","c_beans_with_k_eggs")

## Testing the dpois function 
dpois.data <- data.frame(c(0:20))
names(dpois.data) <- c("k_arthropodss")
ggplot(data=data) +
  aes(x = x, y = dpois(x, 5)) +
  geom_point()

## Testing the dLGP function theda = lambda1, lambda = lambda2
ans <- mean.k.sowbugs * 0.46786
lgp <- data.frame(dLGP(arth.data$k_arthropods, 1.5, 0.53214))
names(lgp) <- c("lgp")
ggplot(data=lgp) +
  aes(x=c(0:17), y = lgp) +
  geom_line()

### Graphing spider data along with dpois
total.spider.obsv <- sum(arth.data$c_boards_with_k_spiders)
sum.k.spiders.seen <- sum(arth.data$k_arthropods * arth.data$c_boards_with_k_spiders)
mean.k.spiders <- sum.k.spiders.seen/total.spider.obsv
prob.k.spiders <- sapply(arth.data$c_boards_with_k_spiders, 
                         function(x,sum) {return(x/sum)},sum=total.spider.obsv)
arth.data$prob_k_spiders <- prob.k.spiders
arth.data$poiss_spiders <- dpois(arth.data$k_arthropods, mean.k.spiders)

## Theoretical LPD for spider
lgp.spider <- data.frame(dLGP(arth.data$k_arthropods, mean.k.spiders, 0))
names(lgp.spider) <- c("lgp")

# Plots the arthrapod data
ggplot(data=arth.data) +
  aes(x = k_arthropods, y = prob_k_spiders) +
  geom_point(aes(color="Data"),alpha=0.8) +
  geom_point(aes(x = k_arthropods, 
                 y = poiss_spiders, 
                 color="Poisson"),alpha=0.5) +
  geom_line(aes(y=lgp.spider$lgp, color="LGP")) +
  labs(title="Poisson Dist. and Spider Data", x="Arthropods",
       y="Probability") +
  guides(alpha=FALSE) +
  scale_color_manual(name = element_blank(),
                     labels = c("Data", "Poisson", ""),
                     values = c("red", "dark green"))

### Graphing sowbugs data along with dpois
total.sowbug.obsv <- sum(arth.data$c_boards_with_k_sowbugs)
sum.k.sowbugs.seen <- sum(arth.data$k_arthropods * arth.data$c_boards_with_k_sowbugs)
mean.k.sowbugs <- sum.k.sowbugs.seen/total.sowbug.obsv
prob.k.sowbugs <- sapply(arth.data$c_boards_with_k_sowbugs, 
                         function(x,sum) {return(x/sum)},sum=total.sowbug.obsv)
arth.data$prob_k_sowbugs <- prob.k.sowbugs
arth.data$poiss_sowbugs <- dpois(arth.data$k_arthropods, mean.k.sowbugs)


## Theoretical LPD for sowbug
lgp.spider <- data.frame(dLGP(arth.data$k_arthropods, mean.k.spiders, 0))
names(lgp.spider) <- c("lgp")


##make the plot
ggplot(data=arth.data) +
  aes(x=k_arthropods, y=prob.k.sowbugs) +
  geom_point(aes(color="Data"),alpha=0.5) +
  geom_point(aes(x = k_arthropods, 
                 y = poiss_sowbugs, color="Poisson"),alpha=0.5) +
  geom_line(aes(y=prob.k.sowbugs, color="Data")) +
  geom_line(aes(y=poiss_sowbugs, color="Poisson")) +
  labs(title="Poisson Dist. and Sowbug Data", x="Arthropods",
       y="Probability") +
  guides(alpha=FALSE) +
  scale_color_manual(name = element_blank(),
                     labels = c("Data", "Poisson"),
                     values = c("red", "dark green"))

## Graphing weevil data along with dpois
total.weevil.obsv <- sum(weevil.data$c_beans_with_k_eggs)
sum.k.weevil.seen <- sum(weevil.data$k_eggs * weevil.data$c_beans_with_k_eggs)
mean.k.weevils <- sum.k.weevil.seen/total.weevil.obsv
prob.k.weevils <- sapply(weevil.data$c_beans_with_k_eggs, 
                         function(x,sum) {return(x/sum)},sum=total.weevil.obsv)
weevil.data$prob_k_eggs <- prob.k.weevils
weevil.data$poiss_weevils <- dpois(weevil.data$k_eggs, mean.k.weevils)

ggplot(data=weevil.data) +
  aes(x=k_eggs, y=prob.k.weevils) +
  geom_point(aes(color="Data"),alpha=0.5) +
  geom_point(aes(x = k_eggs, 
                 y = poiss_weevils, color="Poisson"),alpha=0.5) +
  geom_line(aes(y=prob.k.weevils, color="Data")) +
  geom_line(aes(y=poiss_weevils, color="Poisson")) +
  labs(title="Poisson Dist. and Weevil Data", x="Eggs",
       y="Probability") +
  guides(alpha=FALSE) +
  scale_color_manual(name = element_blank(),
                     labels = c("Data", "Poisson"),
                     values = c("red", "dark green"))

  



