###################
##: Define a function here for choosing gametes, that will be used in Step 3: 
GAMETE <- function(X, num_loci_total)  # Pick loci for gamete
{
  x<- rbinom(num_loci_total,1,0.5)+1 + (0:(num_loci_total-1))*2 # clunky way of picking maternal allele (odd column indexes) or paternal allele (even column indexes) at each locus (i.e. random segregation).  
  Y<- X[x]
  return(Y)
}
###################
