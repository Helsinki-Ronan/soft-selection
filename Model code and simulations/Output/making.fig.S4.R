library(dplyr)
library(forcats)
library(ggplot2)
library(gridExtra)
library(patchwork)

dat<- read.csv("Output/intrusion_one-off_set2_output.csv", header=T)

dat <- dat %>% 
  mutate(Comp=factor(Comp)) %>% 
  mutate(Comp=fct_relevel(Comp,c("intruders competitively inferior","intruders competitively equal","intruders competitively superior")))

# Replace NAs with 0s for the pop size variables (as NA = gone extinct)
dat$N_breeders[dat$N_breeders==0] <- NA
dat$N_recruits[dat$N_recruits==0] <- NA
dat$RpS[dat$RpS==0] <- NA

dat$Intrusion <- factor(dat$Intrusion, levels=c("low intrusion", "moderate intrusion", "high intrusion"))
                        
dat$Reprod.excess <- factor(dat$Reprod.excess, levels=c("low repr. excess", "moderate repr. excess", "high repr. excess"))

p1<- ggplot(dat, aes(Generation, RpS, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(0.5,1.3)+ 
  geom_hline(yintercept=1, linetype = "dashed") +
  ylab("Recruits per spawner") + theme_bw() + facet_wrap(~Intrusion + Reprod.excess)

p1 + plot_layout(guides = "collect") & theme(legend.position = 'bottom') & guides(colour=guide_legend(nrow=2)) & labs(colour=NULL)

