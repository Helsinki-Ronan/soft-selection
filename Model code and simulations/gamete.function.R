



# Define a function for choosing gametes. This function is used in Step 3 of the soft_sel_model_function---- 

choose_gamete<- function(x, num_loci_total) {                               
         # Pick loci for gamete. 'x' is the number of breeders in the population.
         # 'num_loci_total' is the total number of loci across all breeders in the population.
  
    y<- rbinom(num_loci_total,1,0.5)+1 + (0:(num_loci_total-1))*2 
    # Picking maternal allele (odd column indices) or paternal allele (even column indices) at each locus (i.e. random segregation).  
    
    z<- x[y]
    # Subset number of breeders by chosen loci. (TOM, IS THIS STATEMENT CORRECT?)
    
    return(z)
    # Return designated subset.
}


