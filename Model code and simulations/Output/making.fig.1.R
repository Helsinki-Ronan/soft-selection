library(dplyr)
library(forcats)
library(ggplot2)
library(gridExtra)
library(patchwork)

dat<- read.csv("baseline_set1_output.csv", header=T)

dat <- dat %>% 
  mutate(repr.excess=factor(repr.excess)) %>% 
  mutate(repr.excess=fct_relevel(repr.excess,c("low","moderate","high")))

p1<- ggplot(dat, aes(Generation, Mean_P_soft, colour=repr.excess))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(-5,30)+ 
  #geom_hline(yintercept=0, linetype = "dashed") +
  ylab("Mean of soft-selected trait")+ theme_bw()

p2<- ggplot(dat, aes(Generation, Mean_P_hard, colour=repr.excess))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(-5,30)+ 
  geom_hline(yintercept=0, linetype = "dashed") +
  ylab("Mean of hard-selected trait") + theme_bw()

p3<- ggplot(dat, aes(Generation, RpS, colour=repr.excess))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(0.8,1.5)+ 
  geom_hline(yintercept=1, linetype = "dashed") +
  ylab("Recruits per spawner") + theme_bw()

p4<- ggplot(dat, aes(Generation, Var_G_soft, colour=repr.excess))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(0,30)+ 
  #geom_hline(yintercept=0, linetype = "dashed") +
  ylab("Genetic variance in \n soft-selected trait") + theme_bw()

p5<- ggplot(dat, aes(Generation, Var_G_hard, colour=repr.excess))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(0,30)+ 
  #geom_hline(yintercept=0, linetype = "dashed") +
  ylab("Genetic variance in \n hard-selected trait") + theme_bw()

p6<- ggplot(dat, aes(Generation, Gen_cor, colour=repr.excess))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(0,1)+ 
  #geom_hline(yintercept=0, linetype = "dashed") +
 theme_bw()


(p1+p2)/(p4+p5) + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'A') 

p3

p6

p4/p5 + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'A')
