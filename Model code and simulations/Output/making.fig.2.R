library(dplyr)
library(forcats)
library(ggplot2)
library(gridExtra)
library(patchwork)

dat<- read.csv("baseline_set2_output.csv", header=T)

# Replace NAs with 0s for the pop size variables (as NA = gone extinct)
dat$N_breeders[dat$N_breeders==0] <- NA
dat$N_recruits[dat$N_recruits==0] <- NA
dat$RpS[dat$RpS==0] <- NA

p1<- ggplot(dat, aes(Generation, Mean_P_soft))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(-5,30)+ 
  #geom_hline(yintercept=0, linetype = "dashed") +
  ylab("Mean of \n soft-selected trait") + theme_bw()

p2<- ggplot(dat, aes(Generation, Mean_P_hard))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(-5,30)+ 
  geom_hline(yintercept=20, linetype = "dashed") +
  ylab("Mean of \n hard-selected trait") + theme_bw()

p3<- ggplot(dat, aes(Generation, RpS))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(0.8,1.5)+ 
  geom_hline(yintercept=1, linetype = "dashed") +
  ylab("Recruits per spawner") + theme_bw()

p4<- ggplot(dat, aes(Generation, N_breeders))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(0,600)+ 
  ylab("Number of \n spawners") + theme_bw()

(p2+p3)/(p1+p4) + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'A') 

# Fraction replicates that went extinct:

# Replace NAs with 0s for the pop size variables (as NA = gone extinct)
dat$N_breeders[is.na(dat$N_breeders)==TRUE] <- 0
dat$N_recruits[is.na(dat$N_recruits)==TRUE] <- 0
dat$RpS[is.na(dat$RpS)==TRUE] <- 0

minN<- tapply(dat$N_recruits, dat$replicate, min, na.rm=T)
length(minN[minN==0])/length(minN)

