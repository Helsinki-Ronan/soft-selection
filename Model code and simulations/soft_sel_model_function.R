



# Define soft selection function----
soft_selection<- function(
    Maxgen = 100,                              # Maxmimum number of generations that a scenario is run for.
    K =  100,                                  # Number of ecological vacancies available to breeders in a given scenario.
    N_local = 100,                             # Number of individuals in a scenario that are of local provenance and are considered adapted to the local environment.
    F = 0,                                     # Proportion of all individuals that are non-local.
    num_loci_total =  30,                      # The number of loci in the genome. Genetic architecture assumes n-bialellic loci.
    num_loci_shared = 0,                       # The number of genetically correlated loci across the genome.
    p_local_soft_unique = 0.5,                 # The proportion of all loci under soft selection that are uniquely found in local individuals.
    p_nonlocal_soft_unique = 0.25,             # The proportion of all loci under soft selection that are uniquely found in non-local individuals.
    p_local_hard_unique = 0.5,                 # The proportion of all loci under hard selection that are uniquely found in local individuals.
    p_nonlocal_hard_unique = 0.25,             # The proportion of all loci under hard selection that are uniquely found in non-local individuals.
    p_local_shared = 0.5,                      # The proportion of all loci shared by local individuals.
    p_nonlocal_shared = 0.25,                  # The proportion of all loci shared by non-local individuals.
    
    a_soft = 1,                                 # What is 'a'? Genetic variance? TOM
    a_hard = 1,
    a_shared = 1,
    h2_init = 0.5,                             # Sets the initial heritability of (both??) the hard- and soft- selected traits. 
    soft_switch = FALSE,                       # Logical as to whether a given model run does or does not involve soft selection.
    k = 2,                                     # What is this? TOM
    Wmax = 0.55,                               # Maximum height of the Gaussian fitness function.
    Theta = 30,                                # Theoretical trait optimum.
    Omega = 3) {                               # Width of Gaussian fitness function.

  
  
  
  
  
  
                                                                ############################################################
                                                                #                                                          #
                                                                #              1.0 Initial model set-up----                #
                                                                #                                                          #
                                                                ############################################################
  
  
  
  
  # 1.1 Seed the model at the recruitment (pre-breeder) stage, prior to density-dependent competition for limited breeding sites----
  
  N <- round(N_local/(1-F))                                                                                                            # Calculate N, the total population size consisting of both local and non-local individuals.
  N_nonlocal <- N-N_local                                                                                                              # Calculate the number of non-local individuals.
  ID <- 1:N                                                                                                                            # Create individual IDs.
  local <- c(rep(TRUE,N_local), rep(FALSE,N-N_local))                                                                                  # Create a vector that that can be used to assign properties to the local and non-local individuals.
  
  
  
  
                                                                ############################################################
                                                                #                                                          #
                                                                #             2.0 Create allelic matrices for              #
                                                                #          the soft- and hard-selected traits----          #
                                                                #                                                          #
                                                                ############################################################

  
  # Soft-selected trait----
  # 2.1 Create a vector of loci that are unique to local fish.
  num_loci_unique = num_loci_total - num_loci_shared    
  
  
  # 2.2 Create a vector of alleles for the soft-selected trait that are unique to local fish.
  n_alleles_local_unique<- N_local*num_loci_unique*2                                                                                   # Multiplication by 2 since the genome is diploid.
  alleles_soft_local_unique<- as.numeric(runif(n_alleles_local_unique)<p_local_soft_unique)                                            # Create a vector that identifies whether an allele is local or non-local.     
  
  
  # 2.3 For local alleles: Create a temporary matrix that stores alleles which directly mediate the trait under soft selection.
  SL<- matrix(alleles_soft_local_unique, N_local, num_loci_unique*2)
  
  
  # 2.4 Create a vector of alleles that are unique to non-local fish.
  n_alleles_nonlocal_unique<- N_nonlocal*num_loci_unique*2
  alleles_soft_nonlocal_unique<- as.numeric(runif(n_alleles_nonlocal_unique)<p_nonlocal_soft_unique) 
  
  
  # 2.5 For non-local alleles: Create a temporary matrix that stores alleles which directly mediate the trait under soft selection.
  SNL<- matrix(alleles_soft_nonlocal_unique, N_nonlocal, num_loci_unique*2)
  
  
  # 2.6 Combine the allelic matrices for the soft-selected trait for both local and non-local individuals.
  genotypes_soft_unique<- rbind(SL, SNL)
  

  # Hard-selected trait----
  # 2.7 Create a vector of alleles for the hard-selected trait that are unique to local fish.
  alleles_hard_local_unique<- as.numeric(runif(n_alleles_local_unique)<p_local_hard_unique) 
  
  
  # 2.8 For local alleles: Create a temporary matrix that stores alleles which directly mediate the trait under hard selection.
  HL<- matrix(alleles_hard_local_unique, N_local, num_loci_unique*2)
  
  
  # 2.9 For non-local alleles: Create a temporary matrix that stores alleles which directly mediate the trait under hard selection.
  alleles_hard_nonlocal_unique<- as.numeric(runif(n_alleles_nonlocal_unique)<p_nonlocal_hard_unique) 
  HNL<- matrix(alleles_hard_nonlocal_unique, N_nonlocal, num_loci_unique*2)
  
  
  # 2.10 Combine the allelic matrices for the hard-selected trait for both local and non-local individuals.
  genotypes_hard_unique <- rbind(HL, HNL)
  
  
  
  
                                                                ############################################################
                                                                #                                                          #
                                                                #       3.0 Shared loci for genetic correlation----        #
                                                                #                                                          #
                                                                ############################################################
  
  # 3.1 Create a matrix for shared alleles for local individuals.
  n_shared.alleles_local<- N_local*num_loci_shared*2
  alleles_shared_local<- as.numeric(runif(n_shared.alleles_local)<p_local_shared)  
  local_shared<- matrix(alleles_shared_local, N_local, num_loci_shared*2)
  
  
  # 3.2 Create a matrix for shared alleles for non-local individuals.
  n_shared.alleles_nonlocal<- N_nonlocal*num_loci_shared*2
  alleles_shared_nonlocal<- as.numeric(runif(n_shared.alleles_nonlocal)<p_nonlocal_shared)  
  nonlocal_shared<- matrix(alleles_shared_nonlocal, N_nonlocal, num_loci_shared*2)
  
  # 3.3 Combine the allelic matrices for both local and non-local individuals.
  genotypes_both<- rbind(local_shared, nonlocal_shared)                                                                
  
  # 3.4 Combine the allelic matrices of shared alleles for both traits.
  genotypes_soft <- cbind(genotypes_soft_unique, genotypes_both)
  genotypes_hard <- cbind(genotypes_hard_unique, genotypes_both)
  
  
  
  
                                                                ############################################################
                                                                #                                                          #
                                                                #                  4.0 Calculate additive                  #
                                                                #         genetic and environmental variances----          #
                                                                #                                                          #
                                                                ############################################################
  
  # 4.1 Calculate expected additive genetic variance for both local and non-local individuals. This only applies to individuals in the first generation.
  
  # 4.1.1 Soft-selected trait among local individuals.
  va_loc_soft_unique <- num_loci_unique*2*(a_soft^2)*p_local_soft_unique*(1-p_local_soft_unique)       # Could you explain this calcultaion, please? For some reason I'm stuck on it. TOM
  va_loc_shared <- num_loci_shared*2*(a_shared^2)*p_local_shared*(1-p_local_shared)
  va_local_soft <- va_loc_soft_unique + va_loc_shared
  
  
  # 4.1.2 Soft-selected trait among non-local individuals.
  va_nonloc_soft_unique <- num_loci_unique*2*(a_soft^2)*p_nonlocal_soft_unique*(1-p_nonlocal_soft_unique)
  va_nonloc_shared <- num_loci_shared*2*(a_shared^2)*p_nonlocal_shared*(1-p_nonlocal_shared)
  va_nonlocal_soft <- va_nonloc_soft_unique + va_nonloc_shared
  
  
  # 4.1.3 Hard-selected trait among local individuals.
  va_loc_hard_unique <- num_loci_unique*2*(a_hard^2)*p_local_hard_unique*(1-p_local_hard_unique)
  va_local_hard <- va_loc_hard_unique + va_loc_shared
  
  
  # 4.1.4 Hard-selected trait among non-local individuals.
  va_nonloc_hard_unique <- num_loci_unique*2*(a_hard^2)*p_nonlocal_hard_unique*(1-p_nonlocal_hard_unique)
  va_nonlocal_hard <- va_nonloc_hard_unique + va_nonloc_shared
  
  
  # 4.1.5 Define environmental variances for local and non-local individuals.
  ve_locals_soft <- va_local_soft/h2_init - va_local_soft
  ve_nonlocals_soft <- va_nonlocal_soft/h2_init - va_nonlocal_soft
  ve_locals_hard <- va_local_hard/h2_init - va_local_hard
  ve_nonlocals_hard <- va_nonlocal_hard/h2_init - va_nonlocal_hard
  
  
  
  
                                                                ############################################################
                                                                #                                                          #
                                                                #         5.0 Define a neutral diagnostic locus to         #
                                                                #                  be used for monitoring                  #
                                                                #           genetic drift and introgression----            #
                                                                #                                                          #
                                                                ############################################################
  
  # 5.1 Define a matrix.
  genotypes_neutral<- matrix(NA, N, 2)
  
  
  # 5.2 Fill the matrix with genotypes specific to local and non-local individuals. 
  genotypes_neutral[ID[local==TRUE],]<- c(0,0)                                                                                    # Locals have a 0,0 genotype
  if(N>N_local){genotypes_neutral[ID[local==FALSE],]<- c(1,1)}                                                                    # Non-locals have a 1,1 genotype

  
  # NOTE: For each genotype matrix, the alleles at each locus are in consecutive columns. That is to say, the first two columns correspond to the two alleles
  # at the first locus, the third and fourth columns correspond to the two alleles at the second locus, etc.
  
  
  
  
  
  
  
  

                                                                ############################################################
                                                                #                                                          #
                                                                #                    This is where the                     #
                                                                #      function begins iterating over generations----      #
                                                                #                                                          #
                                                                ############################################################
  
  # Allocate space for output.
  output<- matrix(NA,Maxgen,14) 
  output[,1]<-1:Maxgen
  
  
  # Iterate over generations.
  for (Igen in 1:Maxgen)	
  {
    
    N_current<- dim(genotypes_neutral)[1]
    if(is.null(N_current)==TRUE){N_current<- 0}
    
    
    # Only run the following if the pop has not gone extinct.
    if(N_current>0){ 
      
      
      # From 2nd generation on, everyone is a local.
      if(Igen > 1){
        local<- rep(TRUE,N_current)
        ID <- 1:N_current
        N_local<- N_current
      }
      
      
      # Count of number of non-locals (only applies in generation 1):  
      N_nonlocal<- length(ID)-N_local   
      
      
      
      
                                                                      ############################################################
                                                                      #                                                          #
                                                                      #      Calculate genotypic and phenotypic values----       #
                                                                      #                                                          #
                                                                      ############################################################
      
      
      
      # Calculate genotypic values for each quantitative trait.
      G <- matrix(NA, length(ID), 2)
      G_soft_locals<- rowSums(genotypes_soft[ID[local==TRUE],])*a_soft
      G_hard_locals<- rowSums(genotypes_hard[ID[local==TRUE],])*a_hard
      if(N>N_local){
        G_soft_nonlocals<- rowSums(genotypes_soft[ID[local==FALSE],])*a_soft
        G_hard_nonlocals<- rowSums(genotypes_hard[ID[local==FALSE],])*a_hard
      }
      
      
      G[ID[local==TRUE],1] <- G_soft_locals
      G[ID[local==TRUE],2] <- G_hard_locals
      if(N>N_local){
        G[ID[local==FALSE],1] <- G_soft_nonlocals
        G[ID[local==FALSE],2] <- G_hard_nonlocals
      }
      
      
      # Rescale the genotypic values relative to the initial mean expected if population was entirely local. WHY DO WE DO THIS?? TOM
      pS <- (num_loci_unique*p_local_soft_unique + num_loci_shared*p_local_shared) / num_loci_total
      pH <- (num_loci_unique*p_local_hard_unique + num_loci_shared*p_local_shared) / num_loci_total
      
      
      init.mean.Gsoft <- 2*num_loci_total*pS
      init.mean.Ghard<- 2*num_loci_total*pH
      
      
      G[,1] <- G[,1] - init.mean.Gsoft
      G[,2] <- G[,2] - init.mean.Ghard
      
      
      # Define individual phenotypes for the soft-selected trait.
      
      
      P<- G                                                                                                                  # Create a phenotypic matrix (P), which is same size as the genotypic matrix (G).
      
      
      P[ID[local==TRUE],1] <- G[ID[local==TRUE],1] + rnorm(N_local, mean=0, sd=sqrt(ve_locals_soft))                         # Soft-selected trait for locals.
      
 
      P[ID[local==FALSE],1] <- G[ID[local==FALSE],1] + rnorm(N_nonlocal, mean=0, sd=sqrt(ve_nonlocals_soft))                 # Soft-selected trait for non-locals.
      

      P[ID[local==TRUE],2] <- G[ID[local==TRUE],2] + rnorm(N_local, mean=0, sd=sqrt(ve_locals_hard))                         # Hard-selected trait for locals.

            
      P[ID[local==FALSE],2] <- G[ID[local==FALSE],2] + rnorm(N_nonlocal, mean=0, sd=sqrt(ve_nonlocals_hard))                 # Hard-selected trait for non-locals.
     
      
      
 
                                                                        ############################################################
                                                                        #                                                          #
                                                                        #                   Soft selection----                     #
                                                                        #                                                          #
                                                                        ############################################################
      
      Z_soft <- P[,1]
       
      if(soft_switch==TRUE & N_current>K){
        Z_soft_ranked <- sort(Z_soft, decreasing = TRUE) 
        Z_soft_breeders <-  Z_soft_ranked[1:K]                                                                               # Only the top K individuals get to breed.
        ID_breeders <- ID[Z_soft %in% Z_soft_breeders]                                                                       # Retain the IDs only of those individuals that get to breed.
      }
      
      
      # If soft_switch is off, just randomly decide which indiviudals (ID) get to breed. Or if N<K, every individual gets to breed.
      if(soft_switch==FALSE | N_current<=K){
        ID_breeders <- sample(ID, min(c(K,N_current)), replace = FALSE) 
      }
      
      
      output[Igen,2] <- length(ID_breeders)
      
      
      # Subset the genotype matrices based on the IDs of the surviving individuals.
      genotypes_soft <- genotypes_soft[ID_breeders,]
      genotypes_hard <- genotypes_hard[ID_breeders,]
      genotypes_neutral <- genotypes_neutral[ID_breeders,]
      
      
      
    
                                                                    ############################################################
                                                                    #                                                          #
                                                                    #           Random mating and reproduction----             #
                                                                    #                                                          #
                                                                    ############################################################
      
      
      num.offspring <- length(ID_breeders)*k
      
      
      genotypes_soft_unique <- genotypes_soft[,1:(num_loci_unique*2)]
      genotypes_hard_unique <- genotypes_hard[,1:(num_loci_unique*2)]
      
      
      if(num_loci_shared > 0){
        genotypes_both <- genotypes_hard[,((num_loci_unique*2)+1):(num_loci_total*2)]
      }
      
      # TOM, we should externalise the choose_gamete function. This affects the apply function call. Do you think this is a quick fix or?
      gametes_soft_unique	<- t(apply(genotypes_soft_unique, 1, choose_gamete, n=num_loci_unique))
      gametes_hard_unique	<- t(apply(genotypes_hard_unique, 1, choose_gamete, n=num_loci_unique))
      gametes_both	<- t(apply(genotypes_both, 1, choose_gamete, n=num_loci_shared))
      gametes_neutral	<- matrix(apply(genotypes_neutral, 1, choose_gamete, n=1), nrow=length(ID_breeders),1)
      
      
      # Get gametes from "females".
      x<- seq(1, length(ID_breeders)) 
      G.Index<- sample(x=x, size= num.offspring , replace=TRUE)
      F.gametes_soft_unique<- gametes_soft_unique[G.Index,]
      F.gametes_hard_unique<- gametes_hard_unique[G.Index,]
      F.gametes_neutral<- matrix(gametes_neutral[G.Index,], nrow=num.offspring,1)
      
      
      # Get gametes from "males".
      G.Index<- sample(x=x, size= num.offspring , replace=TRUE)
      M.gametes_soft_unique<- gametes_soft_unique[G.Index,]
      M.gametes_hard_unique<- gametes_hard_unique[G.Index,]
      M.gametes_neutral<- matrix(gametes_neutral[G.Index,], nrow=num.offspring,1)
      
      
      # New genotypes:
      genotypes_soft_unique<- matrix(NA, num.offspring , num_loci_unique*2)
      genotypes_hard_unique<- matrix(NA, num.offspring , num_loci_unique*2) 
      genotypes_both<- matrix(NA, num.offspring , num_loci_shared*2) 
      
      
      # Combine gametes into new genotypes:
      evens<- (1:num_loci_total)*2
      
      odds<- evens-1
      for (i in 1:num_loci_unique){
        genotypes_soft_unique[,odds[i]] <- F.gametes_soft_unique[,i]  
        genotypes_soft_unique[,evens[i]] <- M.gametes_soft_unique[,i] 
        genotypes_hard_unique[,odds[i]] <- F.gametes_hard_unique[,i] 
        genotypes_hard_unique[,evens[i]] <- M.gametes_hard_unique[,i] 
      }
      
      if(num_loci_shared > 0) {
        F.gametes_both<- gametes_both[G.Index,]
        M.gametes_both<- gametes_both[G.Index,]
        
        for (i in 1:num_loci_shared){
          genotypes_both[,odds[i]] <- F.gametes_both[,i]
          genotypes_both[,evens[i]] <- M.gametes_both[,i] 
        }
      }
      
      genotypes_soft <- cbind(genotypes_soft_unique, genotypes_both)
      
      genotypes_hard <- cbind(genotypes_hard_unique, genotypes_both)
      
      genotypes_neutral<- cbind(F.gametes_neutral, M.gametes_neutral)
      
     
      
      
                                                          ############################################################
                                                          #                                                          #
                                                          #       Subject the offspring to hard selection----        #
                                                          #                                                          #
                                                          ############################################################
      
      # Define individual phenotypes by drawing environmental deviations from a normal distribution and adding them to G values.
      G_hard<- rowSums(genotypes_hard[,]) - init.mean.Ghard
      P_hard<- G_hard + rnorm(num.offspring, mean=0, sd=sqrt(ve_locals_hard))
      
      
      # Omega parameter (width of fitness function) is defined in terms of phenotypic standard deviation units. Therefore, need to calculate expected initial phenotypic variance.
      phen_var <- va_local_hard + ve_locals_hard 
      width <- Omega*sqrt(phen_var)
      
      
      # Calculate expected fitness (survival):
      W<- Wmax*exp(-((P_hard-Theta)^2)/(2*(width^2)))
      
      
      # Calculate realised survival:
      r<- runif(length(W))
      s<- ifelse(W>r,TRUE,FALSE) 
      
      # Subset individuals depending on whether they survive the hard selection episode. TOM....IF THIS IS FOR HARD SELECTION, WHY DO YOU ALSO SUBSET THE SOFT-SELECTED AND NEURTAL TRAITS? MY GUESS IS THAT EACH VECTOR NEEDS TO BE SEPARATELY SUBSETTED BY THE INDEX OF SURVIVORS??
      genotypes_soft <- genotypes_soft[s,]
      genotypes_hard <- genotypes_hard[s,]
      genotypes_neutral <- genotypes_neutral[s,]
      
      
 
      
                                                                  ############################################################
                                                                  #                                                          #
                                                                  #                    Store output----                      #
                                                                  #                                                          #
                                                                  ############################################################
      
      
      output[Igen,3]<-mean(P[,1], na.rm=T)
      output[Igen,4]<-var(P[,1], na.rm=T)
      output[Igen,5]<-mean(P[,2], na.rm=T)
      output[Igen,6]<-var(P[,2], na.rm=T)
      output[Igen,7]<-mean(G[,1], na.rm=T)
      output[Igen,8]<-var(G[,1], na.rm=T)
      output[Igen,9]<-mean(G[,2], na.rm=T)
      output[Igen,10]<-var(G[,2], na.rm=T)
      
      
      if(is.na(var(G[,1]))==FALSE & var(G[,1])>0 & var(G[,2])>0){
        output[Igen,11]<-cor(G[,1],G[,2],use="pairwise.complete.obs")
      }
      
      
      output[Igen,12]<-mean(genotypes_neutral,na.rm=T)
      output[Igen,13]<-sum(s, na.rm=T)
      output[Igen,14]<-mean(s,na.rm=T)
      
    } # end of if(N_current>0) statement.
    
    
    # Now cycle back to soft selection module.
    
    
    print(Igen)
    
    
  } # End generations loop.

results<- data.frame(output)
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

} 
  


