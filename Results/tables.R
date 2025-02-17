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
  

### Select models ##############################################################
  
  # Duration and baseline
  DGP1 <- duration[c(10,28,37)]
  DGP3a <- duration[c(11,29,38)]
  DGP3b <- duration[c(12,30,39)]
  DGP3c <- duration[c(13,31,40)]
  
  # Heterogeneity and baseline
  DGP2 <- heterogeneity[c(8,22,29)]
  DGP4a <- heterogeneity[c(9,23,30)]
  DGP4b <- heterogeneity[c(10,24,31)]
  DGP4c <- heterogeneity[c(11,25,32)]
  
  # Duration + heterogeneity
  DGP5a <- heterogeneity[c(12,26,33)]
  DGP5b <- heterogeneity[c(13,27,34)]
  DGP5c <- heterogeneity[c(14,28,35)]
  
  
### Build Table 1 ##############################################################
  
  digits <- 3
  
  DGP1t <- data.frame(Samplesize=c(250,1000,2500))
  DGP1t$DGP <- "DGP1"
  DGP1t$Ex <- unlist(lapply(DGP1,function(x) round(mean(abs(x$results_Ex)),digits=digits)))
  DGP1t$Ec <- unlist(lapply(DGP1,function(x) round(mean(abs(x$results_Ec)),digits=digits)))
  DGP1t$Vs <- unlist(lapply(DGP1,function(x) round(mean(abs(x$results_Vs)),digits=digits)))
  DGP1t$Va <- unlist(lapply(DGP1,function(x) round(mean(abs(x$results_Va)),digits=digits)))
  
  DGP3at <- data.frame(Samplesize=c(250,1000,2500))
  DGP3at$DGP <- "DGP3a"
  DGP3at$Ex <- unlist(lapply(DGP3a,function(x) round(mean(abs(x$results_Ex)),digits=digits)))
  DGP3at$Ec <- unlist(lapply(DGP3a,function(x) round(mean(abs(x$results_Ec)),digits=digits)))
  DGP3at$Vs <- unlist(lapply(DGP3a,function(x) round(mean(abs(x$results_Vs)),digits=digits)))
  DGP3at$Va <- unlist(lapply(DGP3a,function(x) round(mean(abs(x$results_Va)),digits=digits)))

  DGP3bt <- data.frame(Samplesize=c(250,1000,2500))
  DGP3bt$DGP <- "DGP3b"
  DGP3bt$Ex <- unlist(lapply(DGP3b,function(x) round(mean(abs(x$results_Ex)),digits=digits)))
  DGP3bt$Ec <- unlist(lapply(DGP3b,function(x) round(mean(abs(x$results_Ec)),digits=digits)))
  DGP3bt$Vs <- unlist(lapply(DGP3b,function(x) round(mean(abs(x$results_Vs)),digits=digits)))
  DGP3bt$Va <- unlist(lapply(DGP3b,function(x) round(mean(abs(x$results_Va)),digits=digits)))

  DGP3ct <- data.frame(Samplesize=c(250,1000,2500))
  DGP3ct$DGP <- "DGP3c"
  DGP3ct$Ex <- unlist(lapply(DGP3c,function(x) round(mean(abs(x$results_Ex)),digits=digits)))
  DGP3ct$Ec <- unlist(lapply(DGP3c,function(x) round(mean(abs(x$results_Ec)),digits=digits)))
  DGP3ct$Vs <- unlist(lapply(DGP3c,function(x) round(mean(abs(x$results_Vs)),digits=digits)))
  DGP3ct$Va <- unlist(lapply(DGP3c,function(x) round(mean(abs(x$results_Va)),digits=digits)))
  
  DGP2t <- data.frame(Samplesize=c(250,1000,2500))
  DGP2t$DGP <- "DGP2"
  DGP2t$Ex <- unlist(lapply(DGP2,function(x) round(mean(abs(x$results_Ex)),digits=digits)))
  DGP2t$Ec <- unlist(lapply(DGP2,function(x) round(mean(abs(x$results_Ec)),digits=digits)))
  DGP2t$Vs <- unlist(lapply(DGP2,function(x) round(mean(abs(x$results_Vs)),digits=digits)))
  DGP2t$Va <- unlist(lapply(DGP2,function(x) round(mean(abs(x$results_Va)),digits=digits)))
  
  DGP4at <- data.frame(Samplesize=c(250,1000,2500))
  DGP4at$DGP <- "DGP4a"
  DGP4at$Ex <- unlist(lapply(DGP4a,function(x) round(mean(abs(x$results_Ex)),digits=digits)))
  DGP4at$Ec <- unlist(lapply(DGP4a,function(x) round(mean(abs(x$results_Ec)),digits=digits)))
  DGP4at$Vs <- unlist(lapply(DGP4a,function(x) round(mean(abs(x$results_Vs)),digits=digits)))
  DGP4at$Va <- unlist(lapply(DGP4a,function(x) round(mean(abs(x$results_Va)),digits=digits)))
  
  DGP4bt <- data.frame(Samplesize=c(250,1000,2500))
  DGP4bt$DGP <- "DGP4b"
  DGP4bt$Ex <- unlist(lapply(DGP4b,function(x) round(mean(abs(x$results_Ex)),digits=digits)))
  DGP4bt$Ec <- unlist(lapply(DGP4b,function(x) round(mean(abs(x$results_Ec)),digits=digits)))
  DGP4bt$Vs <- unlist(lapply(DGP4b,function(x) round(mean(abs(x$results_Vs)),digits=digits)))
  DGP4bt$Va <- unlist(lapply(DGP4b,function(x) round(mean(abs(x$results_Va)),digits=digits)))
  
  DGP4ct <- data.frame(Samplesize=c(250,1000,2500))
  DGP4ct$DGP <- "DGP4c"
  DGP4ct$Ex <- unlist(lapply(DGP4c,function(x) round(mean(abs(x$results_Ex)),digits=digits)))
  DGP4ct$Ec <- unlist(lapply(DGP4c,function(x) round(mean(abs(x$results_Ec)),digits=digits)))
  DGP4ct$Vs <- unlist(lapply(DGP4c,function(x) round(mean(abs(x$results_Vs)),digits=digits)))
  DGP4ct$Va <- unlist(lapply(DGP4c,function(x) round(mean(abs(x$results_Va)),digits=digits)))
  
  DGP5at <- data.frame(Samplesize=c(250,1000,2500))
  DGP5at$DGP <- "DGP5a"
  DGP5at$Ex <- unlist(lapply(DGP5a,function(x) round(mean(abs(x$results_Ex)),digits=digits)))
  DGP5at$Ec <- unlist(lapply(DGP5a,function(x) round(mean(abs(x$results_Ec)),digits=digits)))
  DGP5at$Vs <- unlist(lapply(DGP5a,function(x) round(mean(abs(x$results_Vs)),digits=digits)))
  DGP5at$Va <- unlist(lapply(DGP5a,function(x) round(mean(abs(x$results_Va)),digits=digits)))
  
  DGP5bt <- data.frame(Samplesize=c(250,1000,2500))
  DGP5bt$DGP <- "DGP5b"
  DGP5bt$Ex <- unlist(lapply(DGP5b,function(x) round(mean(abs(x$results_Ex)),digits=digits)))
  DGP5bt$Ec <- unlist(lapply(DGP5b,function(x) round(mean(abs(x$results_Ec)),digits=digits)))
  DGP5bt$Vs <- unlist(lapply(DGP5b,function(x) round(mean(abs(x$results_Vs)),digits=digits)))
  DGP5bt$Va <- unlist(lapply(DGP5b,function(x) round(mean(abs(x$results_Va)),digits=digits)))
  
  DGP5ct <- data.frame(Samplesize=c(250,1000,2500))
  DGP5ct$DGP <- "DGP5c"
  DGP5ct$Ex <- unlist(lapply(DGP5c,function(x) round(mean(abs(x$results_Ex)),digits=digits)))
  DGP5ct$Ec <- unlist(lapply(DGP5c,function(x) round(mean(abs(x$results_Ec)),digits=digits)))
  DGP5ct$Vs <- unlist(lapply(DGP5c,function(x) round(mean(abs(x$results_Vs)),digits=digits)))
  DGP5ct$Va <- unlist(lapply(DGP5c,function(x) round(mean(abs(x$results_Va)),digits=digits)))
  
  Table1a <- rbind(DGP1t,DGP3at,DGP3bt,DGP3ct)
  Table1b <- rbind(DGP2t,DGP4at,DGP4bt,DGP4ct)
  Table1c <- rbind(DGP2t,DGP5at,DGP5bt,DGP5ct)
  
  Table1a[c("DGP","Samplesize","Ex","Ec","Vs","Va")]
  Table1b[c("DGP","Samplesize","Ex","Ec","Vs","Va")]
  Table1c[c("DGP","Samplesize","Ex","Ec","Vs","Va")]
  