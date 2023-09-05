



                            ############################################################
                            #                                                          #
                            #          Plotting of supplementary figures for           #
                            #      "Soft selection affects introgression dynamics      #
                            #             and the viability of populations             #
                            # experiencing intrusion from maladapted individuals"----  #
                            #                                                          #
                            ############################################################



# Set working directory and load packages----
setwd("C:\\Users\\rjosul\\Dropbox\\Research\\soft_selection_output\\Output")

library(dplyr)
library(forcats)
library(ggplot2)
library(gridExtra)
library(patchwork)




                            ############################################################
                            #                                                          #
                            #                      Figure S1----                       #
                            #                                                          #
                            ############################################################


# 1.1 Load data----
dat<- read.csv("baseline_set2_output.csv", header=T)


# 1.2 Set colour scheme----
palette<- c("#D55E00", "#009E73", "#CC79A7")


# 1.3 Replace NAs with 0s for the pop size variables (as NA = gone extinct)----
dat$N_breeders[dat$N_breeders==0] <- NA
dat$N_recruits[dat$N_recruits==0] <- NA
dat$RpS[dat$RpS==0] <- NA


# 1.4 Genetic variance of hard-selected trait plot----
p1<- ggplot(dat, aes(Generation, Var_G_hard))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) + 
  xlim(0,100) + 
  ylim(0,30) + 
  ylab("")+
  xlab("")+
  ggtitle("Genetic variance of hard-selected trait")+
  labs(colour = "Reproductive excess") +
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14), 
        axis.line = element_line(colour = "black"))


# 1.5 Genetic variance of soft-selected trait plot----
p2<- ggplot(dat, aes(Generation, Var_G_soft))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(0,30)+ 
  ylab("")+
  xlab("Generation")+
  ggtitle("Genetic variance of soft-selected trait")+
  labs(colour = "Reproductive excess") +
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14), 
        axis.line = element_line(colour = "black"),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"))


# 1.6 Combine the above two plots into two panels in a single figure----
p1/p2 + plot_annotation(tag_levels = 'A')




                          ############################################################
                          #                                                          #
                          #                      Figure S2----                       #
                          #                                                          #
                          ############################################################


# 2.1 Load data---- 
dat<- read.csv("intrusion_one-off_set1_output.csv", header=T)


# 2.2 Relevel competitive ability variable----
dat <- dat %>% 
  mutate(Comp=factor(Comp)) %>% 
  mutate(Comp=fct_relevel(Comp,c("intruders competitively inferior", 
                                 "intruders competitively equal", 
                                 "intruders competitively superior")))


# 2.3 Replace NAs with 0s for the pop size variables (as NA = gone extinct)----
dat$N_breeders[dat$N_breeders==0] <- NA
dat$N_recruits[dat$N_recruits==0] <- NA
dat$RpS[dat$RpS==0] <- NA


# 2.4 Frequency of foreign allele at neutral locus plot----
p1<- ggplot(dat, aes(Generation, Allele_freq, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylab("") + 
  ggtitle("Frequency of foreign allele at neutral locus") + 
  xlab("") + 
  theme_bw()+
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme(legend.position = c(0.122, 0.85),
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        axis.text.y =  element_text(colour = "black", size = 14),
        axis.line = element_line(colour = "black"))

p1 & labs(colour = "Competitive ability")


# 2.5 Fraction of replicates that went extinct----
# 2.5.1 Replace NAs with 0s for the pop size variables (as NA = gone extinct)----
dat$N_breeders[is.na(dat$N_breeders)==TRUE] <- 0
dat$N_recruits[is.na(dat$N_recruits)==TRUE] <- 0
dat$RpS[is.na(dat$RpS)==TRUE] <- 0

minN<- tapply(dat$N_recruits, dat$replicate, min, na.rm=T)
length(minN[minN==0])/length(minN)




                            ############################################################
                            #                                                          #
                            #                      Figure S3----                       #
                            #                                                          #
                            ############################################################

# 3.1 Load data----
dat<- read.csv("intrusion_one-off_set2_output.csv", header=T)


# 3.2 Relevel competitive ability variable----
dat <- dat %>% 
  mutate(Comp=factor(Comp)) %>% 
  mutate(Comp=fct_relevel(Comp,c("intruders competitively inferior", 
                                 "intruders competitively equal", 
                                 "intruders competitively superior")))


# 3.3 Replace NAs with 0s for the pop size variables (as NA = gone extinct)----
dat$N_breeders[dat$N_breeders==0] <- NA
dat$N_recruits[dat$N_recruits==0] <- NA
dat$RpS[dat$RpS==0] <- NA


# 3.3 Relevel intrusion and reproductive excess variables and use them to assign labels to the factor levels for use in the facet plots----
dat$Intrusion <- factor(dat$Intrusion, levels=c("low intrusion", 
                                                "moderate intrusion", 
                                                "high intrusion"))

dat$Reprod.excess <- factor(dat$Reprod.excess, levels=c("low repr. excess",
                                                        "moderate repr. excess",
                                                        "high repr. excess"))

intrusion_labs <- c("Low intrusion",
                    "Moderate intrusion",
                    "High intrusion")

names(intrusion_labs) <- c("low intrusion", 
                           "moderate intrusion", 
                           "high intrusion")

reproductive_excess <- c("Low reproductive excess",
                         "Moderate reproductive excess", 
                         "High reproductive excess")

names(reproductive_excess) <- c("low repr. excess",
                                "moderate repr. excess",
                                "high repr. excess")


p1<- ggplot(dat, aes(Generation, Mean_P_hard, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  ylim(-20,10)+
  stat_summary(geom="line", fun=mean) +xlim(0,100) +
  geom_hline(yintercept=0, linetype = "dashed") +
  ggtitle("Mean of hard-selected trait") +
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

p1 + plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom') & 
  guides(colour=guide_legend(nrow=1)) & 
  labs(colour= "Competitive abilty")




                        ############################################################
                        #                                                          #
                        #                      Figure S4----                       #
                        #                                                          #
                        ############################################################


# 4.1 Recruits per spawner plot----
p1<- ggplot(dat, aes(Generation, RpS, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(0.5,1.3)+ 
  geom_hline(yintercept=1, linetype = "dashed") +
  ggtitle("Recruits per spawner") +
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


# 4.2 Aesthetics for plot----
p1 + plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom') & 
  guides(colour=guide_legend(nrow = 1)) &
  labs(colour = "Competitive ability")




                          ############################################################
                          #                                                          #
                          #                    5.0 Figure S5----                     #
                          #                                                          #
                          ############################################################


# 5.1 Calculate the percentage of model runs where the population went extinct----
dat$N_breeders[is.na(dat$N_breeders)==TRUE] <- 0
dat2 <- subset(dat, Generation==100)
dim(dat); dim(dat2)
dat2$extinct <- ifelse(dat2$N_breeders==0,1,0)


# 5.2 Plot of percentage of extinct populations with respect to competitive ability of foreign fish----
p1<- ggplot(dat2, aes(Comp, extinct))+
  stat_summary(geom="bar", fun=mean) + 
  ylim(0,1)+ 
  xlab("\nRelative competitiveness of intruders versus locals") +
  ggtitle("Fraction of replicates that went extinct") +
  ylab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.55)) + 
  scale_x_discrete(labels = c("Intruders \n competitively inferior", 
                              "Intruders \n competitively equal", 
                              "Intruders \n competitively superior")) +
  
  facet_wrap(~Intrusion + Reprod.excess,
             labeller = labeller(Intrusion = intrusion_labs, 
                                 Reprod.excess = reproductive_excess))+
  
  theme(strip.text.x = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 15, face = "bold"), 
        text = element_text(size = 12),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14))

p1




                        ############################################################
                        #                                                          #
                        #                     Figure S6----                        #
                        #                                                          #
                        ############################################################

# 6.1 Load data----
dat<- read.csv("intrusion_one-off_set1B_output.csv", header=T)


# 6.2 Relevel competitive ability and maladaptation variables and set facet label headings----
dat <- dat %>% 
  mutate(Comp=factor(Comp)) %>% 
  mutate(Comp=fct_relevel(Comp,c("intruders competitively inferior", 
                                 "intruders competitively equal", 
                                 "intruders competitively superior")))

dat <- dat %>% 
  mutate(Maladaptation=factor(Maladaptation)) %>% 
  mutate(Maladaptation=fct_relevel(Maladaptation,c("intruders weakly maladapted", 
                                                   "intruders moderately maladapted", 
                                                   "intruders strongly maladapted")))

maladaptation_labs <- c("Intruders weakly maladapted", 
                        "Intruders moderately maladapted", 
                        "Intruders strongly maladapted")

names(maladaptation_labs) <- c("intruders weakly maladapted", 
                               "intruders moderately maladapted", 
                               "intruders strongly maladapted")


# 6.3 Replace NAs with 0s for the pop size variables (as NA = gone extinct)----
dat$N_breeders[dat$N_breeders==0] <- NA
dat$N_recruits[dat$N_recruits==0] <- NA
dat$RpS[dat$RpS==0] <- NA


# 6.4 Mean of hard-selected trait plot----
p1<- ggplot(dat, aes(Generation, Mean_P_hard, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  ylim(-25,10)+
  stat_summary(geom="line", fun=mean) + 
  xlim(0,100) +
  geom_hline(yintercept=0, linetype = "dashed") +
  ggtitle("Mean of hard-selected trait") +
  xlab("") +
  ylab("") +
  theme_bw() +
  facet_wrap(~Maladaptation, 
             labeller = labeller(Maladaptation = maladaptation_labs)) + 
  
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  
      theme(strip.text.x = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 15, face = "bold"), 
        text = element_text(size = 12),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14))


# 6.5 Number of spawners plot----
p2<- ggplot(dat, aes(Generation, N_breeders, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,100) + 
  ylim(0,600)+ 
  ggtitle("Number of spawners") +
  xlab("\nGeneration") +
  ylab("") +
  theme_bw() +
  facet_wrap(~Maladaptation, 
             labeller = labeller(Maladaptation = maladaptation_labs)) + 
  
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  
  theme(strip.text.x = element_text(size = 12, face = "bold"),
        plot.title = element_text(size = 15, face = "bold"), 
        text = element_text(size = 12),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14))


# 6.6 Combine the above plots into two panels in a single figure----
p1/p2 + plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = 'A') & 
  theme(legend.position = 'bottom') & 
  guides(colour=guide_legend(nrow = 1)) & 
  labs(colour = "Competitive ability")


# 6.6 Fraction of replicates that went extinct----
# 6.6.1 Replace NAs with 0s for the pop size variables (as NA = gone extinct)----
dat$N_breeders[is.na(dat$N_breeders)==TRUE] <- 0
dat$N_recruits[is.na(dat$N_recruits)==TRUE] <- 0
dat$RpS[is.na(dat$RpS)==TRUE] <- 0

minN<- tapply(dat$N_recruits, dat$replicate, min, na.rm=T)
length(minN[minN==0])/length(minN)




                          ############################################################
                          #                                                          #
                          #                      Figure S7----                       #
                          #                                                          #
                          ############################################################


# 7.1 Load data----
dat<- read.csv("intrusion_continuous_set3B_output.csv", header=T)


# 7.2 Relevel competitive ability variable----
dat <- dat %>% 
  mutate(Comp=factor(Comp)) %>% 
  mutate(Comp=fct_relevel(Comp,c("intruders competitively inferior", 
                                 "intruders competitively equal", 
                                 "intruders competitively superior")))


# 7.3 Replace NAs with 0s for the pop size variables (as NA = gone extinct)----
dat$N_breeders[dat$N_breeders==0] <- NA
dat$N_recruits[dat$N_recruits==0] <- NA
dat$RpS[dat$RpS==0] <- NA


# 7.4 Set heritability labels----
h2labs<- c("h2 = 0.25", "h2 = 0.50")
names(h2labs)<- c("heritability = 0.25","heritability = 0.5")

Maxgen<- 150


# 7.5 Mean of hard-selected trait plot----
p1<- ggplot(dat, aes(Generation, Mean_P_hard, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  ylim(-40,10)+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) +
  geom_hline(yintercept=0, linetype = "dashed") +
  ggtitle("Mean of hard-selected trait") +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme_bw()+ 
  facet_grid(~h2, labeller = labeller(h2=h2labs)) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14), 
        axis.line = element_line(colour = "black"),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        strip.text.x = element_text(size = 12, face = "bold"))


# 7.6 Recruits per spawner plot---- 
p2<- ggplot(dat, aes(Generation, RpS, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(0.25,1.3)+ 
  geom_hline(yintercept=1, linetype = "dashed") +
  ggtitle("Recruits per spawner") +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme_bw()+ 
  facet_grid(~h2, labeller = labeller(h2=h2labs)) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14), 
        axis.line = element_line(colour = "black"),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        strip.text.x = element_text(size = 12, face = "bold"))


# 7.7 Mean of soft-selected trait plot----
p3<- ggplot(dat, aes(Generation, Mean_P_soft, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(-15,30)+ 
  ggtitle("Mean of soft-selected trait") +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme_bw()+ 
  facet_grid(~h2, labeller = labeller(h2=h2labs)) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14), 
        axis.line = element_line(colour = "black"),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        strip.text.x = element_text(size = 12, face = "bold"))


# 7.8 Frequency of foreign allele at neutral locus plot----
p4<- ggplot(dat, aes(Generation, Allele_freq, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(-0.1,1.1)+ 
  ggtitle("Frequency of foreign allele at neutral locus") +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme_bw()+ 
  facet_grid(~h2, labeller = labeller(h2=h2labs)) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14), 
        axis.line = element_line(colour = "black"),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        strip.text.x = element_text(size = 12, face = "bold"))


# 7.9 Number of recruits plot----
p5<- ggplot(dat, aes(Generation, N_recruits, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(-100,600)+ 
  ggtitle("Number of recruits") +
  xlab("\nGeneration") +
  ylab("") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme_bw()+ 
  facet_grid(~h2, labeller = labeller(h2=h2labs)) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14), 
        axis.line = element_line(colour = "black"),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        strip.text.x = element_text(size = 12, face = "bold"))


# 7.10 Number of spawners plot----
p6<- ggplot(dat, aes(Generation, N_breeders, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(-100,600)+ 
  ggtitle("Number of spawners") +
  xlab("\nGeneration") +
  ylab("") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme_bw()+ 
  facet_grid(~h2, labeller = labeller(h2=h2labs)) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14), 
        axis.line = element_line(colour = "black"),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        strip.text.x = element_text(size = 12, face = "bold"))


# 7.11 Combine the above six plots into panels in a single figure----
(p1 + p2)/(p3+p4)/(p5+p6) +
  plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = 'A') & 
  theme(legend.position = 'bottom', 
        legend.text = element_text(size=10), axis.title = element_text(size = 9)) & 
  guides(colour=guide_legend(nrow=1)) & 
  labs(colour = "Competitive ability")


# 7.12 Fraction of replicates that went extinct----
# 7.12.1 Replace NAs with 0s for the pop size variables (as NA = gone extinct)----
dat$N_breeders[is.na(dat$N_breeders)==TRUE] <- 0
dat$N_recruits[is.na(dat$N_recruits)==TRUE] <- 0
dat$RpS[is.na(dat$RpS)==TRUE] <- 0

minN<- tapply(dat$N_recruits, dat$replicate, min, na.rm=T)
length(minN[minN==0])/length(minN)




                          ############################################################
                          #                                                          #
                          #                     Figure  S8----                       #
                          #                                                          #
                          ############################################################


# 8.1 Set the maximum number of generations to plot on the x-axis----
Maxgen<- 150


# 8.2 Load data----
dat<- read.csv("intrusion_continuous_set3A_output.csv", header=T)


# 8.3 Relevel competitive ability variable----
dat <- dat %>% 
  mutate(Comp=factor(Comp)) %>% 
  mutate(Comp=fct_relevel(Comp,c("intruders competitively inferior", 
                                 "intruders competitively equal", 
                                 "intruders competitively superior")))



# 8.4 Replace NAs with 0s for the pop size variables (as NA = gone extinct)----
dat$N_breeders[dat$N_breeders==0] <- NA
dat$N_recruits[dat$N_recruits==0] <- NA
dat$RpS[dat$RpS==0] <- NA


# 8.5 Set heritability labels----
h2labs<- c("h2 = 0.25", "h2 = 0.50")
names(h2labs)<- c("heritability = 0.25","heritability = 0.5")


# 8.6 Mean of hard-selected trait plot----
p1<- ggplot(dat, aes(Generation, Mean_P_hard, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  ylim(-40,10)+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) +
  geom_hline(yintercept=0, linetype = "dashed") +
  ggtitle("Mean of hard-selected trait") +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme_bw()+ 
  facet_grid(~h2, labeller = labeller(h2=h2labs)) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14), 
        axis.line = element_line(colour = "black"),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        strip.text.x = element_text(size = 12, face = "bold"))


# 8.7 Recruits per spawner plot---- 
p2<- ggplot(dat, aes(Generation, RpS, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(0.25,1.3)+ 
  geom_hline(yintercept=1, linetype = "dashed") +
  ggtitle("Recruits per spawner") +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme_bw()+ 
  facet_grid(~h2, labeller = labeller(h2=h2labs)) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14), 
        axis.line = element_line(colour = "black"),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        strip.text.x = element_text(size = 12, face = "bold"))


# 8.8 Mean of soft-selected trait plot----
p3<- ggplot(dat, aes(Generation, Mean_P_soft, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(-15,30)+ 
  ggtitle("Mean of soft-selected trait") +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme_bw()+ 
  facet_grid(~h2, labeller = labeller(h2=h2labs)) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14), 
        axis.line = element_line(colour = "black"),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        strip.text.x = element_text(size = 12, face = "bold"))


# 8.9 Frequency of foreign allele at neutral locus plot----
p4<- ggplot(dat, aes(Generation, Allele_freq, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(-0.1,1.1)+ 
  ggtitle("Frequency of foreign allele at neutral locus") +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme_bw()+ 
  facet_grid(~h2, labeller = labeller(h2=h2labs)) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14), 
        axis.line = element_line(colour = "black"),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        strip.text.x = element_text(size = 12, face = "bold"))


# 8.10 Number of recruits plot----
p5<- ggplot(dat, aes(Generation, N_recruits, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(-100,600)+ 
  ggtitle("Number of recruits") +
  xlab("\nGeneration") +
  ylab("") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme_bw()+ 
  facet_grid(~h2, labeller = labeller(h2=h2labs)) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14), 
        axis.line = element_line(colour = "black"),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        strip.text.x = element_text(size = 12, face = "bold"))


# 8.11 Number of spawners plot----
p6<- ggplot(dat, aes(Generation, N_breeders, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(-100,600)+ 
  ggtitle("Number of spawners") +
  xlab("\nGeneration") +
  ylab("") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme_bw()+ 
  facet_grid(~h2, labeller = labeller(h2=h2labs)) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14), 
        axis.line = element_line(colour = "black"),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        strip.text.x = element_text(size = 12, face = "bold"))


# 8.12 Combine the above six plots into panels in a single figure----
(p1 + p2)/(p3+p4)/(p5+p6) +
  plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = 'A') & 
  theme(legend.position = 'bottom', 
        legend.text = element_text(size=10), axis.title = element_text(size = 9)) & 
  guides(colour=guide_legend(nrow=1)) & 
  labs(colour = "Competitive ability")


# 8.13 Fraction of replicates that went extinct----
# 8.13.1 Replace NAs with 0s for the pop size variables (as NA = gone extinct)----
dat$N_breeders[is.na(dat$N_breeders)==TRUE] <- 0
dat$N_recruits[is.na(dat$N_recruits)==TRUE] <- 0
dat$RpS[is.na(dat$RpS)==TRUE] <- 0

minN<- tapply(dat$N_recruits, dat$replicate, min, na.rm=T)
length(minN[minN==0])/length(minN)




                          ############################################################
                          #                                                          #
                          #                      Figure S9----                       #
                          #                                                          #
                          ############################################################


# 9.1 Load data----
dat<- read.csv("intrusion_continuous_set3C_output.csv", header=T)


# 9.2 Relevel competitive ability variable----
dat <- dat %>% 
  mutate(Comp=factor(Comp)) %>% 
  mutate(Comp=fct_relevel(Comp,c("intruders competitively inferior", 
                                 "intruders competitively equal", 
                                 "intruders competitively superior")))


# 9.3 Replace NAs with 0s for the pop size variables (as NA = gone extinct)----
dat$N_breeders[dat$N_breeders==0] <- NA
dat$N_recruits[dat$N_recruits==0] <- NA
dat$RpS[dat$RpS==0] <- NA


# 9.4 Set heritability labels----
h2labs<- c("h2 = 0.25", "h2 = 0.50")
names(h2labs)<- c("heritability = 0.25","heritability = 0.5")


# 9.5 Mean of hard-selected trait plot----
p1<- ggplot(dat, aes(Generation, Mean_P_hard, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  ylim(-40,10)+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) +
  geom_hline(yintercept=0, linetype = "dashed") +
  ggtitle("Mean of hard-selected trait") +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme_bw()+ 
  facet_grid(~h2, labeller = labeller(h2=h2labs)) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14), 
        axis.line = element_line(colour = "black"),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        strip.text.x = element_text(size = 12, face = "bold"))


# 9.6 Recruits per spawner plot---- 
p2<- ggplot(dat, aes(Generation, RpS, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(0.25,1.3)+ 
  geom_hline(yintercept=1, linetype = "dashed") +
  ggtitle("Recruits per spawner") +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme_bw()+ 
  facet_grid(~h2, labeller = labeller(h2=h2labs)) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14), 
        axis.line = element_line(colour = "black"),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        strip.text.x = element_text(size = 12, face = "bold"))


# 9.7 Mean of soft-selected trait plot----
p3<- ggplot(dat, aes(Generation, Mean_P_soft, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(-15,30)+ 
  ggtitle("Mean of soft-selected trait") +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme_bw()+ 
  facet_grid(~h2, labeller = labeller(h2=h2labs)) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14), 
        axis.line = element_line(colour = "black"),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        strip.text.x = element_text(size = 12, face = "bold"))


# 9.8 Frequency of foreign allele at neutral locus plot----
p4<- ggplot(dat, aes(Generation, Allele_freq, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(-0.1,1.1)+ 
  ggtitle("Frequency of foreign allele at neutral locus") +
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme_bw()+ 
  facet_grid(~h2, labeller = labeller(h2=h2labs)) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14), 
        axis.line = element_line(colour = "black"),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        strip.text.x = element_text(size = 12, face = "bold"))


# 9.9 Number of recruits plot----
p5<- ggplot(dat, aes(Generation, N_recruits, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(-100,600)+ 
  ggtitle("Number of recruits") +
  xlab("\nGeneration") +
  ylab("") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme_bw()+ 
  facet_grid(~h2, labeller = labeller(h2=h2labs)) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14), 
        axis.line = element_line(colour = "black"),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        strip.text.x = element_text(size = 12, face = "bold"))


# 9.10 Number of spawners plot----
p6<- ggplot(dat, aes(Generation, N_breeders, colour=Comp))+
  stat_summary(geom="ribbon", fun.data=mean_cl_normal, fill="lightgrey")+
  stat_summary(geom="line", fun=mean) +xlim(0,Maxgen) + 
  ylim(-100,600)+ 
  ggtitle("Number of spawners") +
  xlab("\nGeneration") +
  ylab("") +
  scale_color_manual(values = c("#D55E00", "#009E73", "#CC79A7")) +
  theme_bw()+ 
  facet_grid(~h2, labeller = labeller(h2=h2labs)) +
  theme(legend.position = "none",
        plot.title = element_text(size = 15, face = "bold"),
        text = element_text(size = 12),
        axis.text.x =  element_text(colour = "black", size = 14),
        axis.text.y =  element_text(colour = "black", size = 14), 
        axis.line = element_line(colour = "black"),
        axis.title.x =  element_text(colour = "black", size = 14, face = "bold"),
        strip.text.x = element_text(size = 12, face = "bold"))


# 9.11 Combine the above six plots into panels in a single figure----
(p1 + p2)/(p3+p4)/(p5+p6) +
  plot_layout(guides = "collect") + 
  plot_annotation(tag_levels = 'A') & 
  theme(legend.position = 'bottom', 
        legend.text = element_text(size=10), axis.title = element_text(size = 9)) & 
  guides(colour=guide_legend(nrow=1)) & 
  labs(colour = "Competitive ability")


# 9.12 Fraction of replicates that went extinct----
# 9.12.1 Replace NAs with 0s for the pop size variables (as NA = gone extinct)----
dat$N_breeders[is.na(dat$N_breeders)==TRUE] <- 0
dat$N_recruits[is.na(dat$N_recruits)==TRUE] <- 0
dat$RpS[is.na(dat$RpS)==TRUE] <- 0

minN<- tapply(dat$N_recruits, dat$replicate, min, na.rm=T)
length(minN[minN==0])/length(minN)
