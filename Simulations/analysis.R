### Load results ###############################################################

  load("Results/dtms_sims_references.Rda")
  load("Results/dtms_sims.Rda")
  
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

  
### Some quick viz #############################################################
  
  # Color vector
  colr <- c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02","#a6761d")

  # Strong switch models (only first 3), small sample
  whichmodels <- c(1:2,4:6)
  plot(x=0,y=0,ylim=c(0.5,1),xlim=c(-1,5),xlab="Lag",ylab="1-L(Delta)",
       panel.first=grid())
  replications <- models[[whichmodels[1]]]$replications
  
  for(repe in 1:replications) {
    for(model in whichmodels) {
    
    lines(-1:5,1-c(0,results[[model]]$results_D[[repe]]),
          col=colr[which(whichmodels==model)])
    }
  }
  
  # Strong switch models, long run, small sample
  whichmodels <- c(1:2,9:11)
  plot(x=0,y=0,ylim=c(0.5,1),xlim=c(-1,5),xlab="Lag",ylab="1-L(Delta)",
       panel.first=grid())
  replications <- models[[whichmodels[1]]]$replications
  
  for(repe in 1:replications) {
    for(model in whichmodels) {
      
      lines(-1:5,1-c(0,results[[model]]$results_D[[repe]]),
            col=colr[which(whichmodels==model)])
    }
  }
  
  # Medium switch models, small sample
  whichmodels <- c(1:2,12:14)
  plot(x=0,y=0,ylim=c(0.5,1),xlim=c(-1,5),xlab="Lag",ylab="1-L(Delta)",
       panel.first=grid())
  replications <- models[[whichmodels[1]]]$replications
  
  for(repe in 1:replications) {
    for(model in whichmodels) {
      
      lines(-1:5,1-c(0,results[[model]]$results_D[[repe]]),
            col=colr[which(whichmodels==model)])
    }
  }
  
  
### For EPC ####################################################################
  
  # Model 1
  round(unlist(differences[[1]]),digits=3)
  round(unlist(differences[[21]]),digits=3)
  round(unlist(differences[[41]]),digits=3)
  # round(unlist(differences[[61]]),digits=3)
  # round(unlist(differences[[81]]),digits=3)
  
  round(mean(unlist(lapply(results[[1]]$results_D,function(x) x[2]))),digits=3)
  round(mean(unlist(lapply(results[[21]]$results_D,function(x) x[2]))),digits=3)
  round(mean(unlist(lapply(results[[41]]$results_D,function(x) x[2]))),digits=3)

  # Model 2
  round(unlist(differences[[4]]),digits=3)
  round(unlist(differences[[24]]),digits=3)
  round(unlist(differences[[44]]),digits=3)

  round(mean(unlist(lapply(results[[4]]$results_D,function(x) x[2]))),digits=3)
  round(mean(unlist(lapply(results[[24]]$results_D,function(x) x[2]))),digits=3)
  round(mean(unlist(lapply(results[[44]]$results_D,function(x) x[2]))),digits=3)
  
  # Model 3
  round(unlist(differences[[12]]),digits=3)
  round(unlist(differences[[32]]),digits=3)
  round(unlist(differences[[52]]),digits=3)

  round(mean(unlist(lapply(results[[12]]$results_D,function(x) x[2]))),digits=3)
  round(mean(unlist(lapply(results[[32]]$results_D,function(x) x[2]))),digits=3)
  round(mean(unlist(lapply(results[[52]]$results_D,function(x) x[2]))),digits=3)
  
  
########
  
  # Bigger summary
  nerr <- length(as.numeric(unlist(differences[[1]])))
  types <- c("E(A)","E(B)","Var(A)","Var(B)","Risk(A)","Risk(B)")
  err <- data.frame(type=character(),error=numeric())

  for(i in 1:nerr) {
    for(model in 1:length(models)) {
      tmp <- as.numeric(unlist(differences[[model]]))
      err <- rbind(err,data.frame(type=types[i],error=tmp[i]))
    }
  }

  
  err |>  ggplot(aes(x=type, y=error)) + 
    geom_boxplot()  

  
###

  Ds <- numeric(length(models))
  
  for(model in 1:length(models))  {
    Ds[model] <- mean(unlist(lapply(results[[model]]$results_D,function(x) x[2])))
  }
  
  VarA <- err |> filter(type=="Var(A)") |> pull(error)
  VarB <- err |> filter(type=="Var(B)") |> pull(error)
  
  cor(Ds,VarA,use="complete.obs")
  cor(Ds,VarB,use="complete.obs")

  plot(Ds,VarA)  
  points(Ds,VarB)  
  