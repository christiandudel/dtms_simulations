### Load results ###############################################################

  load("Results/dtms_sims_references_het.Rda")
  load("Results/dtms_sims_het.Rda")
  
  source("Functions/functions_simulation.R")
  
  library(tidyverse)


### Get differences ############################################################

  # Number of models
  nmodels <- length(models)
  
  # Number of sets of results
  types <- names(references[[1]])
  
  # Object for model comparison
  differences <- list()
  
  # Loop over models
  for(model in 1:nmodels) {
    
    # Temporary for results
    tmp <- list()
    
    # Loop over types of results
    for(i in types){
      
      # Get reference results
      reference <- references[[model]][[i]]
      namesref <- names(reference)
      nreference <- length(reference)
      
      # Edit a bit if necessary
      namesref2 <- simplify(namesref)
      namesref3 <- unique(namesref2)
      if(length(namesref3)<length(namesref)) {
        newreference <- numeric(length(namesref3))
        names(newreference) <- namesref3
        for(getname in namesref3) {
          newreference[getname] <- sum(reference[which(namesref2==getname)])
        }
        reference <- newreference
        namesref <- names(reference)
        nreference <- length(reference)
      }
      
      # Number of comparisons
      ncomparison <- length(results[[model]][[i]])
      
      # Get reference results in matrix shape
      reference <- matrix(reference,nrow=nreference,ncol=ncomparison)
      
      # Get simulation results in matrix shape
      comparison <- unlist(results[[model]][[i]])
      comparison <- matrix(comparison,nrow=nreference)
      
      # Calculate
      res <- rowMeans((comparison-reference)/reference)
      
      # Put away
      names(res) <- namesref
      tmp[[i]] <- res
    }
    
    # Put into object for results
    differences[[model]] <- tmp
    
  }


### Quick look at results ######################################################

  # Bias of conditional expectations, weighted average (which here is just equal to mean)
  
  # No dependence
  round(mean(differences[[1]]$results_Ec),digits=3)
  # Very strong
  round(mean(differences[[2]]$results_Ec),digits=3)
  # Strong
  round(mean(differences[[3]]$results_Ec),digits=3)
  # Moderate
  round(mean(differences[[4]]$results_Ec),digits=3)
  # Weak
  round(mean(differences[[5]]$results_Ec),digits=3)
  