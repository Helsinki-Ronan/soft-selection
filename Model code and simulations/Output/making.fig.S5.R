library(dplyr)
library(forcats)
library(ggplot2)
library(gridExtra)
library(patchwork)

dat<- read.csv("Output/intrusion_one-off_set2_output.csv", header=T)

dat <- dat %>% 
  mutate(Comp=factor(Comp)) %>% 
  mutate(Comp=fct_relevel(Comp,c("intruders competitively inferior","intruders competitively equal","intruders competitively superior")))

# % Extinct:
dat$N_breeders[is.na(dat$N_breeders)==TRUE] <- 0
dat2 <- subset(dat, Generation==100)
dim(dat); dim(dat2)
dat2$extinct <- ifelse(dat2$N_breeders==0,1,0)

p1<- ggplot(dat2, aes(Comp, extinct))+
  stat_summary(geom="bar", fun=mean) + 
  ylim(0,1)+ 
  xlab("Relative competitiveness of intruders versus locals") +
  ylab("Fraction replicates that went extinct") +
  theme_bw() + facet_wrap(~Intrusion + Reprod.excess) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) + scale_x_discrete(labels = c("intruders \n competitively inferior","intruders \n competitively equal","intruders \n competitively superior"))

p1

