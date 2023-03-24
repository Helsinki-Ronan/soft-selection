source("gamete.function.R")

soft_sel_model<- function(
    Maxgen = 100,
    K =  100,
    N_local = 100,
    F = 0,     
    num_loci_total =  30,  
    num_loci_shared = 0,
    p_local_soft_unique = 0.5,
    p_nonlocal_soft_unique = 0.25,
    p_local_hard_unique = 0.5,
    p_nonlocal_hard_unique = 0.25,
    p_local_shared = 0.5,
    p_nonlocal_shared = 0.25,
    a_soft = 1,
    a_hard = 1,
    a_shared = 1,
    h2_init = 0.5,
    soft_switch = FALSE,
    k = 2,
    Wmax = 0.55,
    Theta = 30,
    Omega = 3
) {

  ###############
  ### Step 1  ###
  ###############
  
  ### Seed the model at the recruitment (pre-breeder) stage, just prior to competition for limited breeding sites.
  
  N <- round(N_local/(1-F)) # total N (local + non-local)
  N_nonlocal <- N-N_local
  ID <- 1:N  # individual IDs
  local <- c(rep(TRUE,N_local),rep(FALSE,N-N_local))
  
  ## Set up genotype matrices:
  num_loci_unique = num_loci_total - num_loci_shared    
  
  # 1. Unique loci for soft trait:
  n_alleles_local_unique<- N_local*num_loci_unique*2
  
  alleles_soft_local_unique<- as.numeric(runif(n_alleles_local_unique)<p_local_soft_unique) 
  
  # Temporary matrix to store alleles for soft trait for locals; will be merged wih nonlocals below:
  SL<- matrix(alleles_soft_local_unique, N_local, num_loci_unique*2)
  
  n_alleles_nonlocal_unique<- N_nonlocal*num_loci_unique*2
  
  alleles_soft_nonlocal_unique<- as.numeric(runif(n_alleles_nonlocal_unique)<p_nonlocal_soft_unique) 
  
  # Temporary matrix to store alleles for soft trait for nonlocals; will be merged wih nonlocals below:
  SNL<- matrix(alleles_soft_nonlocal_unique, N_nonlocal, num_loci_unique*2)
  
  Genotypes_soft_unique <- rbind(SL, SNL)
  
  # 2. Unique loci for hard trait:
  alleles_hard_local_unique<- as.numeric(runif(n_alleles_local_unique)<p_local_hard_unique) 
  
  # Temporary matrix to store alleles for hard trait for locals; will be merged wih nonlocals below:
  HL<- matrix(alleles_hard_local_unique, N_local, num_loci_unique*2)
  
  alleles_hard_nonlocal_unique<- as.numeric(runif(n_alleles_nonlocal_unique)<p_nonlocal_hard_unique) 
  
  # Temporary matrix to store alleles for hard trait for nonlocals; will be merged wih nonlocals below:
  HNL<- matrix(alleles_hard_nonlocal_unique, N_nonlocal, num_loci_unique*2)
  
  Genotypes_hard_unique <- rbind(HL, HNL)
  
  # 3. Shared loci between soft and hard traits:
  # Do for locals and nonlocals separately, and then bind together:
  
  n_shared.alleles_local<- N_local*num_loci_shared*2
  alleles_shared_local<- as.numeric(runif(n_shared.alleles_local)<p_local_shared)  
  BL<- matrix(alleles_shared_local, N_local, num_loci_shared*2)
  
  n_shared.alleles_nonlocal<- N_nonlocal*num_loci_shared*2
  alleles_shared_nonlocal<- as.numeric(runif(n_shared.alleles_nonlocal)<p_nonlocal_shared)  
  BNL<- matrix(alleles_shared_nonlocal, N_nonlocal, num_loci_shared*2)
  
  Genotypes_both <- rbind(BL, BNL)
  
  # 4. Bind the unique and shared genotypes for each trait:
  Genotypes_soft <- cbind(Genotypes_soft_unique, Genotypes_both)
  Genotypes_hard <- cbind(Genotypes_hard_unique, Genotypes_both)
  
  # First compute expected additive genetic variance of locals and nonlocals (applies to first generation only):
  VA_loc_soft_unique <- num_loci_unique*2*(a_soft^2)*p_local_soft_unique*(1-p_local_soft_unique)
  VA_loc_shared <- num_loci_shared*2*(a_shared^2)*p_local_shared*(1-p_local_shared)
  VA_local_soft <- VA_loc_soft_unique + VA_loc_shared
  
  VA_nonloc_soft_unique <- num_loci_unique*2*(a_soft^2)*p_nonlocal_soft_unique*(1-p_nonlocal_soft_unique)
  VA_nonloc_shared <- num_loci_shared*2*(a_shared^2)*p_nonlocal_shared*(1-p_nonlocal_shared)
  VA_nonlocal_soft <- VA_nonloc_soft_unique + VA_nonloc_shared
  
  VA_loc_hard_unique <- num_loci_unique*2*(a_hard^2)*p_local_hard_unique*(1-p_local_hard_unique)
  VA_local_hard <- VA_loc_hard_unique + VA_loc_shared
  
  VA_nonloc_hard_unique <- num_loci_unique*2*(a_hard^2)*p_nonlocal_hard_unique*(1-p_nonlocal_hard_unique)
  VA_nonlocal_hard <- VA_nonloc_hard_unique + VA_nonloc_shared
  
  # Now define environmental variances for locals and non-locals:
  VE_locals_soft <- VA_local_soft/h2_init - VA_local_soft
  VE_nonlocals_soft <- VA_nonlocal_soft/h2_init - VA_nonlocal_soft
  VE_locals_hard <- VA_local_hard/h2_init - VA_local_hard
  VE_nonlocals_hard <- VA_nonlocal_hard/h2_init - VA_nonlocal_hard
  
  
  # 5. Neutral diagnostic locus:
  Genotypes_neutral<- matrix(NA, N, 2)
  Genotypes_neutral[ID[local==TRUE],]<- c(0,0) # locals have a 0,0 genotype
  if(N>N_local){Genotypes_neutral[ID[local==FALSE],]<- c(1,1)}
  # non-locals have a 1,1 genotype
  
  # NOTE: for each of these genotype matrices, the alelles at each locus are in consecutive columns. So first two columns = alleles at first locus;  next two columns = alleles at second locus, etc.
  
  
  ###############
  ### Step 1B ###
  ###############
  
  ##### HERE IS WHERE WE START CYCLING OVER GENERATIONS:
  Output<- matrix(NA,Maxgen,14) # Allocate space for output
  Output[,1]<-1:Maxgen
  
  for (Igen in 1:Maxgen)	# Iterate over generations
  {
    
    N_current<- dim(Genotypes_neutral)[1]
    if(is.null(N_current)==TRUE){N_current<- 0}
    
    # Only run the following if the pop has not gone extinct:
    if(N_current>0){ 
      
      # From 2nd generation on, everyone is a local:
      if(Igen > 1){
        local<- rep(TRUE,N_current)
        ID <- 1:N_current
        N_local<- N_current
      }
      
      # Count of number of non-locals (only applies in generation 1):  
      N_nonlocal<- length(ID)-N_local   
      
      ## Now calculate genotypic values for each quantitative trait:
      G <- matrix(NA, length(ID), 2)
      G_soft_locals<- rowSums(Genotypes_soft[ID[local==TRUE],])*a_soft
      G_hard_locals<- rowSums(Genotypes_hard[ID[local==TRUE],])*a_hard
      if(N>N_local){
        G_soft_nonlocals<- rowSums(Genotypes_soft[ID[local==FALSE],])*a_soft
        G_hard_nonlocals<- rowSums(Genotypes_hard[ID[local==FALSE],])*a_hard
      }
      
      G[ID[local==TRUE],1] <- G_soft_locals
      G[ID[local==TRUE],2] <- G_hard_locals
      if(N>N_local){
        G[ID[local==FALSE],1] <- G_soft_nonlocals
        G[ID[local==FALSE],2] <- G_hard_nonlocals
      }
      
      # Rescale the genotypic values relative to the initial mean expected if pop entirely local:
      pS <- (num_loci_unique*p_local_soft_unique + num_loci_shared*p_local_shared) / num_loci_total
      pH <- (num_loci_unique*p_local_hard_unique + num_loci_shared*p_local_shared) / num_loci_total
      
      init.mean.Gsoft <- 2*num_loci_total*pS
      init.mean.Ghard<- 2*num_loci_total*pH
      
      G[,1] <- G[,1] - init.mean.Gsoft
      G[,2] <- G[,2] - init.mean.Ghard
      
      ## Define individual phenotypes for soft trait:
      
      # Draw environmental deviations and add them to G values:
      
      P<- G # create P matrix, which is same size as G
      
      # 1. Soft trait for locals:
      P[ID[local==TRUE],1] <- G[ID[local==TRUE],1] + rnorm(N_local, mean=0, sd=sqrt(VE_locals_soft))
      
      # 2. Soft trait for nonlocals:
      P[ID[local==FALSE],1] <- G[ID[local==FALSE],1] + rnorm(N_nonlocal, mean=0, sd=sqrt(VE_nonlocals_soft))
      
      # 3. Hard trait for locals:
      P[ID[local==TRUE],2] <- G[ID[local==TRUE],2] + rnorm(N_local, mean=0, sd=sqrt(VE_locals_hard))
      
      # 4. Hard trait for nonlocals:
      P[ID[local==FALSE],2] <- G[ID[local==FALSE],2] + rnorm(N_nonlocal, mean=0, sd=sqrt(VE_nonlocals_hard))
      
      ###############
      ### Step 2  ###
      ###############
      
      ### Subject these individuals to soft selection
      
      Z_soft <- P[,1]
      
      if(soft_switch==TRUE & N_current>K){
        Z_soft_ranked <- sort(Z_soft, decreasing = TRUE) 
        Z_soft_breeders <-  Z_soft_ranked[1:K] # only top K individuals get to breed
        ID_breeders <- ID[Z_soft %in% Z_soft_breeders] # retain the IDs only of those individuals that get to breed
      }
      
      # If soft_switch is off, just randomly decide which indiviudals (ID) get to breed.  Or if N<K, they all get to breed (and the sample bit then does nothing other than randomly shuffle the IDs):
      if(soft_switch==FALSE | N_current<=K){
        ID_breeders <- sample(ID, min(c(K,N_current)), replace = FALSE) 
      }
      
      Output[Igen,2] <- length(ID_breeders)
      
      # Now subset the genotype matrices based on the IDs of these surviving individuals:
      Genotypes_soft <- Genotypes_soft[ID_breeders,]
      Genotypes_hard <- Genotypes_hard[ID_breeders,]
      Genotypes_neutral <- Genotypes_neutral[ID_breeders,]
      
      ###############
      ### Step 3  ###
      ###############
      
      ### Random mating and reproduction
      num.offspring <- length(ID_breeders)*k
      
      Genotypes_soft_unique <- Genotypes_soft[,1:(num_loci_unique*2)]
      Genotypes_hard_unique <- Genotypes_hard[,1:(num_loci_unique*2)]
      if(num_loci_shared > 0){
        Genotypes_both <- Genotypes_hard[,((num_loci_unique*2)+1):(num_loci_total*2)]
      }
      
      Gametes_soft_unique	<- t(apply(Genotypes_soft_unique, 1, GAMETE, n=num_loci_unique))
      Gametes_hard_unique	<- t(apply(Genotypes_hard_unique, 1, GAMETE, n=num_loci_unique))
      Gametes_both	<- t(apply(Genotypes_both, 1, GAMETE, n=num_loci_shared))
      Gametes_neutral	<- matrix(apply(Genotypes_neutral, 1, GAMETE, n=1), nrow=length(ID_breeders),1)
      
      # Get gametes from "females"
      x    					<- seq(1, length(ID_breeders)) 
      G.Index 			<- sample(x=x, size= num.offspring , replace=TRUE)
      F.Gametes_soft_unique <- Gametes_soft_unique[G.Index,]
      F.Gametes_hard_unique	<- Gametes_hard_unique[G.Index,]
      F.Gametes_neutral 		<- matrix(Gametes_neutral[G.Index,], nrow=num.offspring,1)
      
      # Get gametes from "males"
      G.Index 			<- sample(x=x, size= num.offspring , replace=TRUE)
      M.Gametes_soft_unique	<- Gametes_soft_unique[G.Index,]
      M.Gametes_hard_unique	<- Gametes_hard_unique[G.Index,]
      M.Gametes_neutral 		<- matrix(Gametes_neutral[G.Index,], nrow=num.offspring,1)
      
      # New Genotypes:
      Genotypes_soft_unique<- matrix(NA, num.offspring , num_loci_unique*2)
      Genotypes_hard_unique<- matrix(NA, num.offspring , num_loci_unique*2) 
      Genotypes_both<- matrix(NA, num.offspring , num_loci_shared*2) 
      
      # Combine gametes into new genotypes:
      evens<- (1:num_loci_total)*2
      odds<- evens-1
      for (i in 1:num_loci_unique){
        Genotypes_soft_unique[,odds[i]] <- F.Gametes_soft_unique[,i]  
        Genotypes_soft_unique[,evens[i]] <- M.Gametes_soft_unique[,i] 
        Genotypes_hard_unique[,odds[i]] <- F.Gametes_hard_unique[,i] 
        Genotypes_hard_unique[,evens[i]] <- M.Gametes_hard_unique[,i] 
      }
      
      if(num_loci_shared > 0) {
        F.Gametes_both	<- Gametes_both[G.Index,]
        M.Gametes_both	<- Gametes_both[G.Index,]
        
        for (i in 1:num_loci_shared){
          Genotypes_both[,odds[i]] <- F.Gametes_both[,i]
          Genotypes_both[,evens[i]] <- M.Gametes_both[,i] 
        }
      }
      
      Genotypes_soft <- cbind(Genotypes_soft_unique, Genotypes_both)
      
      Genotypes_hard <- cbind(Genotypes_hard_unique, Genotypes_both)
      
      Genotypes_neutral<- cbind(F.Gametes_neutral, M.Gametes_neutral)
      
      ###############
      ### Step 4  ###
      ###############
      
      ### Subject the offspring to hard selection
      
      ## First need to form the phenotype for the hard trait:
      
      # Now define individual phenotypes by drawing environmental deviations and adding them to G values:
      G_hard<- rowSums(Genotypes_hard[,]) - init.mean.Ghard
      P_hard<- G_hard + rnorm(num.offspring, mean=0, sd=sqrt(VE_locals_hard))
      
      # Omega parameter (width of fitness function) is defined in terms of phenotypic standard deviation units. So need to calculate expected initial phenotypic variance:
      phen_var <- VA_local_hard + VE_locals_hard 
      width <- Omega*sqrt(phen_var)
      
      # Calculate expected fitness (survival):
      W<- Wmax*exp(-((P_hard-Theta)^2)/(2*(width^2)))
      
      # Calculate realised survival:
      r<- runif(length(W))
      s<- ifelse(W>r,TRUE,FALSE) 
      
      Genotypes_soft <- Genotypes_soft[s,]
      Genotypes_hard <- Genotypes_hard[s,]
      Genotypes_neutral <- Genotypes_neutral[s,]
      
      #####################
      ### STORE OUTPUT: ###
      #####################
      
      Output[Igen,3]<-mean(P[,1], na.rm=T)
      Output[Igen,4]<-var(P[,1], na.rm=T)
      Output[Igen,5]<-mean(P[,2], na.rm=T)
      Output[Igen,6]<-var(P[,2], na.rm=T)
      Output[Igen,7]<-mean(G[,1], na.rm=T)
      Output[Igen,8]<-var(G[,1], na.rm=T)
      Output[Igen,9]<-mean(G[,2], na.rm=T)
      Output[Igen,10]<-var(G[,2], na.rm=T)
      if(is.na(var(G[,1]))==FALSE & var(G[,1])>0 & var(G[,2])>0){
        Output[Igen,11]<-cor(G[,1],G[,2],use="pairwise.complete.obs")
      }
      Output[Igen,12]<-mean(Genotypes_neutral,na.rm=T)
      Output[Igen,13]<-sum(s, na.rm=T)
      Output[Igen,14]<-mean(s,na.rm=T)
      
    } # end of if(N_current>0) statement
    
    # End of Step 4; now we cycle back up to Step 1B
    print(Igen)
  } # END GENERATIONS LOOPS

results<- data.frame(Output)
names(results) <- c("Generation", "N_breeders",
                    "Mean_P_soft",
                    "Var_P_soft",
                    "Mean_P_hard",
                    "Var_P_hard",
                    "Mean_G_soft",
                    "Var_G_soft",
                    "Mean_G_hard",
                    "Var_G_hard",
                    "Gen_cor",
                    "Allele_freq",
                    "N_recruits",
                    "Mean_S"
                    )

results$RpS <- results$N_recruits/results$N_breeders

return(results)

} ## END FUNCTION
  

