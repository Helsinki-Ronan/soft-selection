library(dplyr)
library(forcats)
library(ggplot2)
library(gridExtra)
library(patchwork)

dat<- read.csv("Output/intrusion_one-off_set2B_output.csv", header=T)

# Replace NAs with 0s for the pop size variables (as NA = gone extinct)
dat$N_breeders[dat$N_breeders==0] <- NA
dat$N_recruits[dat$N_recruits==0] <- NA
dat$RpS[dat$RpS==0] <- NA

dat$Intrusion <- factor(dat$Intrusion, levels=c("low intrusion", "moderate intrusion", "high intrusion"))
                        
dat$Reprod.excess <- factor(dat$Reprod.excess, levels=c("low repr. excess", "moderate repr. excess", "high repr. excess"))

p1<- ggplot(dat, aes(Generation, Mean_P_soft, colour=TraitCorr))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(-10,50)+ 
  #geom_hline(yintercept=0, linetype = "dashed") +
  ylab("Mean of \n soft-selected trait")+ theme_bw() +
  facet_wrap(~Intrusion + Reprod.excess)

p2<- ggplot(dat, aes(Generation, Mean_P_hard, colour=TraitCorr))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  ylim(-10,30)+
  stat_summary(geom="line", fun=mean) +xlim(0,100) +
  geom_hline(yintercept=0, linetype = "dashed") +
  ylab("Mean of hard-selected trait") + theme_bw() +
  facet_wrap(~Intrusion + Reprod.excess)

p3<- ggplot(dat, aes(Generation, RpS, colour=TraitCorr))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(0.5,1.3)+ 
  geom_hline(yintercept=1, linetype = "dashed") +
  ylab("Recruits per spawner") + theme_bw() + facet_wrap(~Intrusion + Reprod.excess)

p4<- ggplot(dat, aes(Generation, Var_G_soft, colour=TraitCorr))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(0,30)+ 
  #geom_hline(yintercept=0, linetype = "dashed") +
  ylab("Genetic variance \n in soft trait") + theme_bw()+
  facet_wrap(~Intrusion + Reprod.excess)

p5<- ggplot(dat, aes(Generation, Var_G_hard, colour=TraitCorr))+
  #stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,30) + 
  ylim(0,250)+ 
  #geom_hline(yintercept=0, linetype = "dashed") +
  ylab("Genetic variance \n in hard trait") + theme_bw()+
  facet_wrap(~Intrusion + Reprod.excess)

p6<- ggplot(dat, aes(Generation, Gen_cor, colour=TraitCorr))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(0,1)+ 
  #geom_hline(yintercept=0, linetype = "dashed") 
  theme_bw()+
  facet_wrap(~Intrusion + Reprod.excess)

p7<- ggplot(dat, aes(Generation, N_breeders, colour=TraitCorr))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(-100,700)+ 
  ylab("Number of spawners") +
  #geom_hline(yintercept=0, linetype = "dashed") +
  theme_bw()+
  facet_wrap(~Intrusion + Reprod.excess)


p8<- ggplot(dat, aes(Generation, Allele_freq, colour=TraitCorr))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(0,1)+ 
  ylab("Frequency of foreign allele \n at neutral locus") +
  #geom_hline(yintercept=0, linetype = "dashed") +
  theme_bw()+
  facet_wrap(~Intrusion + Reprod.excess)


p7 + plot_layout(guides = "collect") & theme(legend.position = 'bottom') & guides(colour=guide_legend(nrow=1)) & labs(colour=NULL)

p2 + plot_layout(guides = "collect") & theme(legend.position = 'bottom') & guides(colour=guide_legend(nrow=2)) & labs(colour=NULL)

p8 + plot_layout(guides = "collect") & theme(legend.position = 'bottom') & guides(colour=guide_legend(nrow=1)) & labs(colour=NULL)

p3 + plot_layout(guides = "collect") & theme(legend.position = 'bottom') & guides(colour=guide_legend(nrow=2)) & labs(colour=NULL)

p5 + plot_layout(guides = "collect") & theme(legend.position = 'bottom') & guides(colour=guide_legend(nrow=2)) & labs(colour=NULL)

p1 + plot_layout(guides = "collect") & theme(legend.position = 'bottom') & guides(colour=guide_legend(nrow=2)) & labs(colour=NULL)

# % Extinct:
dat$N_breeders[is.na(dat$N_breeders)==TRUE] <- 0
dat2 <- subset(dat, Generation==100)
dim(dat); dim(dat2)
dat2$extinct <- ifelse(dat2$N_breeders==0,1,0)

p9<- ggplot(dat2, aes(Comp, extinct))+
  stat_summary(geom="bar", fun=mean) + 
  ylim(0,1)+ 
  xlab("Relative competitiveness of intruders versus locals") +
  ylab("Fraction replicates that went extinct") +
  theme_bw() + facet_wrap(~Intrusion + Reprod.excess) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) + scale_x_discrete(labels = c("intruders \n competitively inferior","intruders \n competitively equal","intruders \n competitively superior"))

p9


