



# Plotting of model output for soft selection study----


setwd("C:\\Users\\rjosul\\Dropbox\\Research\\soft_selection_output\\Output")



library(dplyr)
library(forcats)
library(ggplot2)
library(gridExtra)
library(patchwork)




# Figure 1----
dat<- read.csv("baseline_set1_output.csv", header=T)


palette<- c("#D55E00", "#009E73", "#CC79A7")

dat <- dat %>% 
  mutate(repr.excess=factor(repr.excess)) %>% 
  mutate(repr.excess=fct_relevel(repr.excess,c("low","moderate","high")))

p1<- ggplot(dat, aes(Generation, Mean_P_soft, colour=repr.excess))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) + xlim(0,100) + 
  ylim(-5,30)+ 
  ylab("")+
  xlab("")+
  ggtitle("Mean of soft-selected trait")+
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14), axis.line = element_line(colour = "black"))

p2<- ggplot(dat, aes(Generation, Mean_P_hard, colour=repr.excess))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) + xlim(0,100) + 
  geom_hline(yintercept=0, linetype = "dashed") +
  ylim(-5,30)+ 
  ylab("")+
  xlab("")+
  ggtitle("Mean of hard-selected trait")+
  labs(colour = "Reproductive excess") +
  scale_color_manual(labels = c("Low", "Moderate", "High"), values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme_bw()+
  theme(legend.position = c(0.77, 0.8),
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14), axis.line = element_line(colour = "black"))




p3<- ggplot(dat, aes(Generation, Var_G_soft, colour=repr.excess))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(0,30)+ 
  ggtitle("Genetic variance in soft-selected trait") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  ylab("") + 
  xlab("\nGeneration") + 
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        axis.text.y =  element_text(colour = "black", size = 14),
        axis.line = element_line(colour = "black"))

p4<- ggplot(dat, aes(Generation, Var_G_hard, colour=repr.excess))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(0,30)+ 
  ggtitle("Genetic variance in hard-selected trait") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  ylab("") + 
  xlab("\nGeneration") + 
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        axis.text.y =  element_text(colour = "black", size = 14), 
        axis.line = element_line(colour = "black"))


(p1+p2)/(p3+p4) + plot_annotation(tag_levels = 'A') 




# Figure 2----
dat<- read.csv("baseline_set2_output.csv", header=T)

# Replace NAs with 0s for the pop size variables (as NA = gone extinct)
dat$N_breeders[dat$N_breeders==0] <- NA
dat$N_recruits[dat$N_recruits==0] <- NA
dat$RpS[dat$RpS==0] <- NA


p1<- ggplot(dat, aes(Generation, Mean_P_soft))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(-5,30)+ 
  ggtitle("Mean of soft-selected trait") + 
  xlab("\nGeneration") + 
  ylab("") + 
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        axis.text.y =  element_text(colour = "black", size = 14),
        axis.line = element_line(colour = "black"))

p2<- ggplot(dat, aes(Generation, Mean_P_hard))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(-5,30)+ 
  geom_hline(yintercept=20, linetype = "dashed") +
  ggtitle("Mean of hard-selected trait") +
  xlab("") +
  ylab("") +
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        axis.text.y =  element_text(colour = "black", size = 14),
        axis.line = element_line(colour = "black"))

p3<- ggplot(dat, aes(Generation, RpS))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(0.8,1.5)+ 
  geom_hline(yintercept=1, linetype = "dashed") +
  ggtitle("Recruits per spawner") +
  xlab("") +
  ylab("") +
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        axis.text.y =  element_text(colour = "black", size = 14),
        axis.line = element_line(colour = "black"))

p4<- ggplot(dat, aes(Generation, N_breeders))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(0,600)+ 
  ggtitle("Numbers of spawners") +
  xlab("\nGeneration") +
  ylab("") +
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        axis.text.y =  element_text(colour = "black", size = 14),
        axis.line = element_line(colour = "black"))

(p2+p3)/(p1+p4) + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'A') 

# Fraction replicates that went extinct:

# Replace NAs with 0s for the pop size variables (as NA = gone extinct)
dat$N_breeders[is.na(dat$N_breeders)==TRUE] <- 0
dat$N_recruits[is.na(dat$N_recruits)==TRUE] <- 0
dat$RpS[is.na(dat$RpS)==TRUE] <- 0

minN<- tapply(dat$N_recruits, dat$replicate, min, na.rm=T)
length(minN[minN==0])/length(minN)




# Figure 3----
dat<- read.csv("intrusion_one-off_set1_output.csv", header=T)

dat <- dat %>% 
  mutate(Comp=factor(Comp)) %>% 
  mutate(Comp=fct_relevel(Comp,c("intruders competitively inferior","intruders competitively equal","intruders competitively superior")))

# Replace NAs with 0s for the pop size variables (as NA = gone extinct)
dat$N_breeders[dat$N_breeders==0] <- NA
dat$N_recruits[dat$N_recruits==0] <- NA
dat$RpS[dat$RpS==0] <- NA

p1<- ggplot(dat, aes(Generation, Mean_P_soft, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(-10,30)+ 
  ylab("")+
  xlab("")+
  ggtitle("Mean of soft-selected trait")+
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14), axis.line = element_line(colour = "black"))

p2<- ggplot(dat, aes(Generation, Mean_P_hard, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  ylim(-20,10)+
  stat_summary(geom="line", fun=mean) +xlim(0,100) +
  geom_hline(yintercept=0, linetype = "dashed") +
  ylab("")+
  xlab("")+
  ggtitle("Mean of hard-selected trait")+
  labs(colour = "Competitive ability")+
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme_bw()+
  theme(legend.position = c(0.73, 0.2),
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14), axis.line = element_line(colour = "black"))

p3<- ggplot(dat, aes(Generation, RpS, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(0.5,1.3)+ 
  geom_hline(yintercept=1, linetype = "dashed") +
  ggtitle("Recruits per spawner") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  xlab("\nGeneration") +
  ylab("") +
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        axis.text.y =  element_text(colour = "black", size = 14),
        axis.line = element_line(colour = "black"))


p4<- ggplot(dat, aes(Generation, N_breeders, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(0,600)+ 
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  ggtitle("Number of spawners")+
  xlab("\nGeneration") +
  ylab("") +
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        axis.text.y =  element_text(colour = "black", size = 14),
        axis.line = element_line(colour = "black"))



(p1 + p2)/(p3+p4) + plot_annotation(tag_levels = 'A')


# Fraction replicates that went extinct:

# Replace NAs with 0s for the pop size variables (as NA = gone extinct)
dat$N_breeders[is.na(dat$N_breeders)==TRUE] <- 0
dat$N_recruits[is.na(dat$N_recruits)==TRUE] <- 0
dat$RpS[is.na(dat$RpS)==TRUE] <- 0

minN<- tapply(dat$N_recruits, dat$replicate, min, na.rm=T)
length(minN[minN==0])/length(minN)




# Figure 4----

dat<- read.csv("intrusion_one-off_set2_output.csv", header=T)

dat <- dat %>% 
  mutate(Comp=factor(Comp)) %>% 
  mutate(Comp=fct_relevel(Comp,c("intruders competitively inferior","intruders competitively equal","intruders competitively superior")))

# Replace NAs with 0s for the pop size variables (as NA = gone extinct)
dat$N_breeders[dat$N_breeders==0] <- NA
dat$N_recruits[dat$N_recruits==0] <- NA
dat$RpS[dat$RpS==0] <- NA

dat$Intrusion <- factor(dat$Intrusion, levels=c("low intrusion", "moderate intrusion", "high intrusion"))

dat$Reprod.excess <- factor(dat$Reprod.excess, levels=c("low repr. excess", "moderate repr. excess", "high repr. excess"))

intrusion_labs <- c("Low intrusion", "Moderate intrusion", "High intrusion")
names(intrusion_labs) <- c("low intrusion", "moderate intrusion", "high intrusion")

reproductive_excess <- c("Low reproductive excess", "Moderate reproductive excess", "High reproductive excess")
names(reproductive_excess) <- c("low repr. excess", "moderate repr. excess", "high repr. excess")


p1<- ggplot(dat, aes(Generation, N_breeders, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(0,600)+ 
  ggtitle("Number of spawners") +
  xlab("\nGeneration") +
  ylab("") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme_bw() +
  facet_wrap(~Intrusion + Reprod.excess,
                          labeller = labeller(Intrusion = intrusion_labs, 
                                              Reprod.excess = reproductive_excess))+
  
  theme(strip.text.x = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 15, face = "bold"), 
        text = element_text(size = 12),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"))




p1 + plot_layout(guides = "collect") & theme(legend.position = 'bottom') & guides(colour=guide_legend(nrow=1)) & labs(colour= "Competitive abilty")




# Figure 5----
Maxgen<- 150

dat<- read.csv("intrusion_continuous_set1_output.csv", header=T)

dat <- dat %>% 
  mutate(Comp=factor(Comp)) %>% 
  mutate(Comp=fct_relevel(Comp,c("intruders competitively inferior","intruders competitively equal","intruders competitively superior")))

# Replace NAs with 0s for the pop size variables (as NA = gone extinct)
dat$N_breeders[dat$N_breeders==0] <- NA
dat$N_recruits[dat$N_recruits==0] <- NA
dat$RpS[dat$RpS==0] <- NA


p1<- ggplot(dat, aes(Generation, Mean_P_hard, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  ylim(-40,10)+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) +
  geom_hline(yintercept=0, linetype = "dashed") +
  ylab("") + 
  ggtitle("Mean of hard-selected trait") + 
  xlab("") + 
  theme_bw()+
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        axis.text.y =  element_text(colour = "black", size = 14),
        axis.line = element_line(colour = "black"))

p2<- ggplot(dat, aes(Generation, RpS, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(0.25,1.3)+ 
  geom_hline(yintercept=1, linetype = "dashed") +
  ylab("") + 
  ggtitle("Recruits per spawner") + 
  xlab("") + 
  theme_bw()+
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        axis.text.y =  element_text(colour = "black", size = 14),
        axis.line = element_line(colour = "black"))

p3<- ggplot(dat, aes(Generation, Mean_P_soft, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(-10,30)+ 
  ylab("") + 
  ggtitle("Mean of soft-selected trait") + 
  xlab("") + 
  theme_bw()+
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        axis.text.y =  element_text(colour = "black", size = 14),
        axis.line = element_line(colour = "black"))

p4<- ggplot(dat, aes(Generation, Allele_freq, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(-0.1,1.1)+ 
  ylab("")+
  ggtitle("Freq. of foreign allele at neutral locus") +
  xlab("") + 
  theme_bw()+
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        axis.text.y =  element_text(colour = "black", size = 14),
        axis.line = element_line(colour = "black"))

p5<- ggplot(dat, aes(Generation, N_recruits, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(-100,600)+ 
  ylab("") + 
  ggtitle("Number of recruits") +
  xlab("\nGeneration") + 
  theme_bw()+
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        axis.text.y =  element_text(colour = "black", size = 14),
        axis.line = element_line(colour = "black"))


p6<- ggplot(dat, aes(Generation, N_breeders, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(-100,600)+ 
  ylab("") + 
  ggtitle("Number of spawners") +
  xlab("\nGeneration") + 
  theme_bw()+
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        axis.text.y =  element_text(colour = "black", size = 14),
        axis.line = element_line(colour = "black"))


(p1+p2)/(p3+p4)/(p5+p6) + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'A') & theme(legend.position = 'bottom') & guides(colour=guide_legend(nrow=1)) & labs(colour = "Competitive ability")

# Fraction replicates that went extinct:
http://127.0.0.1:24375/graphics/plot_zoom_png?width=1680&height=987
# Replace NAs with 0s for the pop size variables (as NA = gone extinct)
dat$N_breeders[is.na(dat$N_breeders)==TRUE] <- 0
dat$N_recruits[is.na(dat$N_recruits)==TRUE] <- 0
dat$RpS[is.na(dat$RpS)==TRUE] <- 0

minN<- tapply(dat$N_recruits, dat$replicate, min, na.rm=T)
length(minN[minN==0])/length(minN)


# Figure 6----
dat<- read.csv("intrusion_continuous_set2_output.csv", header=T)

dat <- dat %>% 
  mutate(Comp=factor(Comp)) %>% 
  mutate(Comp=fct_relevel(Comp,c("intruders competitively inferior","intruders competitively equal","intruders competitively superior")))

# Replace NAs with 0s for the pop size variables (as NA = gone extinct)
dat$N_breeders[dat$N_breeders==0] <- NA
dat$N_recruits[dat$N_recruits==0] <- NA
dat$RpS[dat$RpS==0] <- NA

p1<- ggplot(dat, aes(Generation, Mean_P_hard, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  ylim(-40,10)+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) +
  geom_hline(yintercept=0, linetype = "dashed") +
  ylab("") + 
  xlab("") +
  ggtitle("Mean of hard-selected trait") +
  theme_bw()+
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        axis.text.y =  element_text(colour = "black", size = 14),
        axis.line = element_line(colour = "black"))

p2<- ggplot(dat, aes(Generation, RpS, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(0.25,1.3)+ 
  geom_hline(yintercept=1, linetype = "dashed") +
  ylab("") + 
  xlab("") +
  ggtitle("Recruits per spawner") +
  theme_bw()+
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        axis.text.y =  element_text(colour = "black", size = 14),
        axis.line = element_line(colour = "black"))

p3<- ggplot(dat, aes(Generation, Mean_P_soft, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(-15,30)+ 
  ylab("") + 
  xlab("") +
  ggtitle("Mean of soft-selected trait") +
  theme_bw()+
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        axis.text.y =  element_text(colour = "black", size = 14),
        axis.line = element_line(colour = "black"))

p4<- ggplot(dat, aes(Generation, Allele_freq, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(-0.1,1.1)+ 
  ggtitle("Freq. of foreign allele at neutral locus") +
  ylab("") + 
  xlab("") + 
  theme_bw()+
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        axis.text.y =  element_text(colour = "black", size = 14),
        axis.line = element_line(colour = "black"))

p5<- ggplot(dat, aes(Generation, N_recruits, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(-100,600)+ 
  ggtitle("Number of recruits") +
  ylab("") + 
  xlab("\nGeneration") + 
  theme_bw()+
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        axis.text.y =  element_text(colour = "black", size = 14),
        axis.line = element_line(colour = "black"))

p6<- ggplot(dat, aes(Generation, N_breeders, colour=Comp))+
  #geom_hline(yintercept=500, linetype = "dashed")+  
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(-100,600)+ 
  ggtitle("Number of spawners") +
  ylab("") + 
  xlab("\nGeneration") + 
  theme_bw()+
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        axis.text.y =  element_text(colour = "black", size = 14),
        axis.line = element_line(colour = "black"))


(p1 + p2)/(p3+p4)/(p5+p6) + plot_layout(guides = "collect") + plot_annotation(tag_levels = 'A') & theme(legend.position = 'bottom') & guides(colour=guide_legend(nrow=1)) & labs(colour = "Competitive ability")





























