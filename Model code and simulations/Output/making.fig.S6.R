library(dplyr)
library(forcats)
library(ggplot2)
library(gridExtra)
library(patchwork)

dat<- read.csv("Output/intrusion_one-off_set1B_output.csv", header=T)

dat <- dat %>% 
  mutate(Comp=factor(Comp)) %>% 
  mutate(Comp=fct_relevel(Comp,c("intruders competitively inferior","intruders competitively equal","intruders competitively superior")))

dat <- dat %>% 
  mutate(Maladaptation=factor(Maladaptation)) %>% 
  mutate(Maladaptation=fct_relevel(Maladaptation,c("intruders weakly maladapted","intruders moderately maladapted","intruders strongly maladapted")))


# Replace NAs with 0s for the pop size variables (as NA = gone extinct)
dat$N_breeders[dat$N_breeders==0] <- NA
dat$N_recruits[dat$N_recruits==0] <- NA
dat$RpS[dat$RpS==0] <- NA

p1<- ggplot(dat, aes(Generation, Mean_P_hard, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  ylim(-25,10)+
  stat_summary(geom="line", fun=mean) +xlim(0,100) +
  geom_hline(yintercept=0, linetype = "dashed") +
  ylab("Mean of \n hard-selected trait") + theme_bw() +
  facet_wrap(~Maladaptation)

p2<- ggplot(dat, aes(Generation, N_breeders, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(0,600)+ 
  ylab("Number of \n spawners") +
  #geom_hline(yintercept=0, linetype = "dashed") +
  theme_bw()+
  facet_wrap(~Maladaptation)

p1/p2 + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'A') & theme(legend.position = 'bottom') & guides(colour=guide_legend(nrow=3)) & labs(colour=NULL)


# Fraction replicates that went extinct:

# Replace NAs with 0s for the pop size variables (as NA = gone extinct)
dat$N_breeders[is.na(dat$N_breeders)==TRUE] <- 0
dat$N_recruits[is.na(dat$N_recruits)==TRUE] <- 0
dat$RpS[is.na(dat$RpS)==TRUE] <- 0

minN<- tapply(dat$N_recruits, dat$replicate, min, na.rm=T)
length(minN[minN==0])/length(minN)

