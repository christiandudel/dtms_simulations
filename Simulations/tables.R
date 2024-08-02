### Functions & Packages #######################################################

  source("Functions/functions_simulation.R")
  library(tidyverse)


### Load results, duration #####################################################

  load("Results/dtms_sims_references.Rda")
  load("Results/dtms_sims.Rda")


### Get differences ############################################################
  
  # Number of models
  nmodels <- length(models)
  
  # Number of sets of results
  types <- names(references[[1]])
  
  # Object for model comparison
  duration <- list()
  
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
    duration[[model]] <- tmp
    
  }

  
### Load results, heterogeneity ################################################
  
  load("Results/dtms_sims_references_het.Rda")
  load("Results/dtms_sims_het.Rda")
  

### Get differences ############################################################
  
  # Number of models
  nmodels <- length(models)
  
  # Number of sets of results
  types <- names(references[[1]])
  
  # Object for model comparison
  heterogeneity <- list()
  
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
    heterogeneity[[model]] <- tmp
    
  }  
  
### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
### Select models ##############################################################
  
  # Correct moldes
  
  DGP1 <- duration[c(21,41,61,81)]
  DGP2 <- heterogeneity[c()]
  
  DGP3a <- NA
  DGP3b <- NA
  DGP3c <- NA
  
  DGP4a <- NA
  DGP4b <- NA
  DGP4c
  