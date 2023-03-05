#rm(list=ls())  # Clear memory

library(dplyr)
library(ggplot2)
library(gridExtra)

source("soft_sel_model_function.R")

nreps<- 20

# 1. Baseline case.
p<- 0.5 # enter desired starting allele freq locals hard trait here

dat1 <- bind_rows(replicate(nreps, soft_sel_model(K=100, N_local=100, F=0, p_local_hard_unique=p, p_nonlocal_hard_unique=p, p_local_soft_unique = p, p_nonlocal_soft_unique=p, h2_init=0.25, soft_switch = FALSE, Wmax = 0.55, Theta=0, num_loci_shared = 0), simplify = FALSE))

# Replace NAs with 0s for the pop size variables (as NA = gone extinct)
dat1$N_breeders[is.na(dat1$N_breeders)==TRUE] <- 0
dat1$N_recruits[is.na(dat1$N_recruits)==TRUE] <- 0
dat1$RpS[is.na(dat1$RpS)==TRUE] <- 0

dat1$replicate <- sort(rep(rep(1:nreps),100))

res<- dat1 %>% 
  group_by(Generation) %>%
  summarise(N_Recruits = mean(N_recruits, na.rm=T),
            N_Breeders = mean(N_breeders, na.rm=T),
            RPS = mean(RpS, na.rm=T),
            G_Soft = mean(Mean_G_soft, na.rm=T),
            G_Hard = mean(Mean_G_hard, na.rm=T))

p1<- ggplot(res, aes(x=Generation, y=N_Breeders))+
  geom_line() +xlim(0,100) + ylim(0,200) +
  geom_hline(yintercept=100, color = "green")
p2<- ggplot(res, aes(x=Generation, y=RPS))+
  geom_line() +xlim(0,100) + 
  geom_hline(yintercept=1, color = "red")  + ylim(0,1.5)
p3<- ggplot(res, aes(x=Generation, y=G_Soft))+
  geom_line() +xlim(0,100) + ylim(-30,30)+ 
  geom_hline(yintercept=0, color = "blue")
p4<- ggplot(res, aes(x=Generation, y=G_Hard))+
  geom_line() +xlim(0,100) + ylim(-30,30)+ 
  geom_hline(yintercept=0, color = "red")
grid.arrange(p1,p2,p3,p4, nrow=2)

# Fraction replicates that went extinct:
minN<- tapply(dat1$N_recruits, dat1$replicate, min, na.rm=T)
length(minN[minN==0])/length(minN)
