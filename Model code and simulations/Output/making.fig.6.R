library(dplyr)
library(forcats)
library(ggplot2)
library(gridExtra)
library(patchwork)

Maxgen<- 150

dat<- read.csv("Output/intrusion_continuous_set2_output.csv", header=T)

dat <- dat %>% 
  mutate(Comp=factor(Comp)) %>% 
  mutate(Comp=fct_relevel(Comp,c("intruders competitively inferior","intruders competitively equal","intruders competitively superior")))

# Replace NAs with 0s for the pop size variables (as NA = gone extinct)
dat$N_breeders[dat$N_breeders==0] <- NA
dat$N_recruits[dat$N_recruits==0] <- NA
dat$RpS[dat$RpS==0] <- NA

p1<- ggplot(dat, aes(Generation, Mean_P_soft, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(-15,30)+ 
  #geom_hline(yintercept=0, linetype = "dashed") +
  ylab("Mean of \n soft-selected trait")+ theme_bw()


p2<- ggplot(dat, aes(Generation, Mean_P_hard, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  ylim(-40,10)+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) +
  geom_hline(yintercept=0, linetype = "dashed") +
  ylab("Mean of \n hard-selected trait") + theme_bw()

p3<- ggplot(dat, aes(Generation, RpS, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(0.25,1.3)+ 
  geom_hline(yintercept=1, linetype = "dashed") +
  ylab("Recruits \n per spawner") + theme_bw()

p4<- ggplot(dat, aes(Generation, Var_G_soft, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(0,30)+ 
  #geom_hline(yintercept=0, linetype = "dashed") +
  ylab("Genetic variance \n in soft trait") + theme_bw()

p5<- ggplot(dat, aes(Generation, Var_G_hard, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(0,110)+ 
  #geom_hline(yintercept=0, linetype = "dashed") +
  ylab("Genetic variance \n in hard trait") + theme_bw()

p6<- ggplot(dat, aes(Generation, Gen_cor, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(0,1)+ 
  #geom_hline(yintercept=0, linetype = "dashed") 
  theme_bw()

p7<- ggplot(dat, aes(Generation, N_breeders, colour=Comp))+
  #geom_hline(yintercept=500, linetype = "dashed")+  
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(-100,600)+ 
  ylab("Number of \n spawners") +
  theme_bw()

p8<- ggplot(dat, aes(Generation, Allele_freq, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(-0.1,1.1)+ 
  ylab("Freq. of foreign \n allele at neutral locus") +
  #geom_hline(yintercept=0, linetype = "dashed") +
  theme_bw()

p9<- ggplot(dat, aes(Generation, N_recruits, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(-100,600)+ 
  ylab("Number of \n recruits") +
  #geom_hline(yintercept=0, linetype = "dashed") +
  theme_bw()


(p2 + p3)/(p1+p8)/(p9+p7) + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'A') & theme(legend.position = 'bottom') & guides(colour=guide_legend(nrow=2)) & labs(colour=NULL)

p3/p7 + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'A') 

p2/p7/p1 + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'A') 

p3/p7 + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'A') 

p3

p6

p4/p5 + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'A') & theme(legend.position = 'bottom') & guides(colour=guide_legend(nrow=3)) & labs(colour=NULL)

p7 + plot_layout(guides = "collect") & theme(legend.position = 'bottom') & guides(colour=guide_legend(nrow=3)) & labs(colour=NULL)


# Fraction replicates that went extinct:

# Replace NAs with 0s for the pop size variables (as NA = gone extinct)
dat$N_breeders[is.na(dat$N_breeders)==TRUE] <- 0
dat$N_recruits[is.na(dat$N_recruits)==TRUE] <- 0
dat$RpS[is.na(dat$RpS)==TRUE] <- 0

minN<- tapply(dat$N_recruits, dat$replicate, min, na.rm=T)
length(minN[minN==0])/length(minN)

