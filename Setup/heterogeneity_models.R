### Setup ######################################################################

  source("Setup/parameters.R")


### Empty lists for models #####################################################

  models <- list()

  
### No unobserved heterogeneity [DGP 2] ########################################
  
  # Group A
  groupA <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=even3,B=even3,C=even3,X=death),
                                   B=c(A=even3,B=even3,C=even3,X=death),
                                   C=c(A=even3,B=even3,C=even3,X=death)),
                 gen_duration=F, 
                 gen_age = F,
                 sample_size=100,
                 replications=replications,
                 initial_distr=c(1,0,0))
  
  # Group B
  groupB <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=even3,B=even3,C=even3,X=death),
                                   B=c(A=even3,B=even3,C=even3,X=death),
                                   C=c(A=even3,B=even3,C=even3,X=death)),
                 gen_duration=F, 
                 gen_age = F,
                 sample_size=100,
                 replications=replications,
                 initial_distr=c(0,1,0))
  
  # Group C
  groupC <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=even3,B=even3,C=even3,X=death),
                                   B=c(A=even3,B=even3,C=even3,X=death),
                                   C=c(A=even3,B=even3,C=even3,X=death)),
                 gen_duration=F, 
                 gen_age = F,
                 sample_size=100,
                 replications=replications,
                 initial_distr=c(0,0,1))
  
  # Combine
  models[[length(models)+1]] <- list(groupA=groupA,groupB=groupB,groupC=groupC,
                                     replications=replications,sample_size=100)
  
  
### Weak group differences [DGP 4a] ############################################
  
  # Probabilities
  sticky <- (1-death)*sticky1
  nonsticky <- (1-death-sticky)/2
  
  # Group A
  groupA <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=sticky,B=nonsticky,C=nonsticky,X=death),
                                   B=c(A=sticky,B=nonsticky,C=nonsticky,X=death),
                                   C=c(A=sticky,B=nonsticky,C=nonsticky,X=death)),
                 gen_duration=F, 
                 gen_age = F,
                 sample_size=100,
                 replications=replications,
                 initial_distr=c(1,0,0))
  
  # Group B
  groupB <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=nonsticky,B=sticky,C=nonsticky,X=death),
                                   B=c(A=nonsticky,B=sticky,C=nonsticky,X=death),
                                   C=c(A=nonsticky,B=sticky,C=nonsticky,X=death)),
                 gen_duration=F, 
                 gen_age = F,
                 sample_size=100,
                 replications=replications,
                 initial_distr=c(0,1,0))
  
  # Group C
  groupC <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=nonsticky,B=nonsticky,C=sticky,X=death),
                                   B=c(A=nonsticky,B=nonsticky,C=sticky,X=death),
                                   C=c(A=nonsticky,B=nonsticky,C=sticky,X=death)),
                 gen_duration=F, 
                 gen_age = F,
                 sample_size=100,
                 replications=replications,
                 initial_distr=c(0,0,1))
  
  # Combine
  models[[length(models)+1]] <- list(groupA=groupA,groupB=groupB,groupC=groupC,
                                     replications=replications,sample_size=100)   
  
  
### Strong group differences [DGP 4b] ##########################################
  
  # Probabilities
  sticky <- (1-death)*sticky2
  nonsticky <- (1-death-sticky)/2
  
  # Group A
  groupA <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=sticky,B=nonsticky,C=nonsticky,X=death),
                                   B=c(A=sticky,B=nonsticky,C=nonsticky,X=death),
                                   C=c(A=sticky,B=nonsticky,C=nonsticky,X=death)),
                 gen_duration=F, 
                 gen_age = F,
                 sample_size=100,
                 replications=replications,
                 initial_distr=c(1,0,0))
  
  # Group B
  groupB <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=nonsticky,B=sticky,C=nonsticky,X=death),
                                   B=c(A=nonsticky,B=sticky,C=nonsticky,X=death),
                                   C=c(A=nonsticky,B=sticky,C=nonsticky,X=death)),
                 gen_duration=F, 
                 gen_age = F,
                 sample_size=100,
                 replications=replications,
                 initial_distr=c(0,1,0))
  
  # Group C
  groupC <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=nonsticky,B=nonsticky,C=sticky,X=death),
                                   B=c(A=nonsticky,B=nonsticky,C=sticky,X=death),
                                   C=c(A=nonsticky,B=nonsticky,C=sticky,X=death)),
                 gen_duration=F, 
                 gen_age = F,
                 sample_size=100,
                 replications=replications,
                 initial_distr=c(0,0,1))
  
  # Combine
  models[[length(models)+1]] <- list(groupA=groupA,groupB=groupB,groupC=groupC,
                                     replications=replications,sample_size=100)   
  
  
### Very strong group differences [DGP 4c] #####################################

  # Probabilities
  sticky <- (1-death)*sticky3
  nonsticky <- (1-death-sticky)/2
  
  # Group A
  groupA <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=sticky,B=nonsticky,C=nonsticky,X=death),
                                   B=c(A=sticky,B=nonsticky,C=nonsticky,X=death),
                                   C=c(A=sticky,B=nonsticky,C=nonsticky,X=death)),
                 gen_duration=F, 
                 gen_age = F,
                 sample_size=100,
                 replications=replications,
                 initial_distr=c(1,0,0))
  
  # Group B
  groupB <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=nonsticky,B=sticky,C=nonsticky,X=death),
                                   B=c(A=nonsticky,B=sticky,C=nonsticky,X=death),
                                   C=c(A=nonsticky,B=sticky,C=nonsticky,X=death)),
                 gen_duration=F, 
                 gen_age = F,
                 sample_size=100,
                 replications=replications,
                 initial_distr=c(0,1,0))
  
  # Group C
  groupC <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=nonsticky,B=nonsticky,C=sticky,X=death),
                                   B=c(A=nonsticky,B=nonsticky,C=sticky,X=death),
                                   C=c(A=nonsticky,B=nonsticky,C=sticky,X=death)),
                 gen_duration=F, 
                 gen_age = F,
                 sample_size=100,
                 replications=replications,
                 initial_distr=c(0,0,1))
  
  # Combine
  models[[length(models)+1]] <- list(groupA=groupA,groupB=groupB,groupC=groupC,
                                     replications=replications,sample_size=100)
  
  
### Weak group differences, weak duration [DGP 5a] #############################
  
  # Probabilities
  sticky <- (1-death)*sticky1
  nonsticky <- (1-death-sticky)/2
  
  # Differences 
  difftmp <- min(nonsticky,sticky,diff1)
  
  # Group A
  groupA <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=sticky,B=nonsticky,C=nonsticky,X=death),
                                   B=c(A=sticky,B=nonsticky,C=nonsticky,X=death),
                                   C=c(A=sticky,B=nonsticky,C=nonsticky,X=death)),
                 gen_duration=T, 
                 gen_age = F,
                 sample_size=100,
                 replications=replications,
                 initial_distr=c(1,0,0),
                 which_duration = c("A"),
                 diff_duration = list(A=c(A=difftmp,B=-difftmp,C=0,X=0)),
                 interpolation_duration = list(A="switch1"))
  
  # Group B
  groupB <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=nonsticky,B=sticky,C=nonsticky,X=death),
                                   B=c(A=nonsticky,B=sticky,C=nonsticky,X=death),
                                   C=c(A=nonsticky,B=sticky,C=nonsticky,X=death)),
                 gen_duration=T, 
                 gen_age = F,
                 sample_size=100,
                 replications=replications,
                 initial_distr=c(0,1,0),
                 which_duration = c("B"),
                 diff_duration = list(B=c(A=-difftmp,B=difftmp,C=0,X=0)),
                 interpolation_duration = list(B="switch1"))
  
  # Group C
  groupC <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=nonsticky,B=nonsticky,C=sticky,X=death),
                                   B=c(A=nonsticky,B=nonsticky,C=sticky,X=death),
                                   C=c(A=nonsticky,B=nonsticky,C=sticky,X=death)),
                 gen_duration=T, 
                 gen_age = F,
                 sample_size=100,
                 replications=replications,
                 initial_distr=c(0,0,1),
                 which_duration = c("C"),
                 diff_duration = list(C=c(A=-difftmp,B=0,C=difftmp,X=0)),
                 interpolation_duration = list(C="switch1"))
  
  # Combine
  models[[length(models)+1]] <- list(groupA=groupA,groupB=groupB,groupC=groupC,
                                     replications=replications,sample_size=100)     
  
  
### Moderate group differences, moderate duration [DGP 5b] #####################
  
  # Probabilities
  sticky <- (1-death)*sticky2
  nonsticky <- (1-death-sticky)/2
  
  # Differences 
  difftmp <- min(nonsticky,sticky,diff2)  
  
  # Group A
  groupA <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=sticky,B=nonsticky,C=nonsticky,X=death),
                                   B=c(A=sticky,B=nonsticky,C=nonsticky,X=death),
                                   C=c(A=sticky,B=nonsticky,C=nonsticky,X=death)),
                 gen_duration=T, 
                 gen_age = F,
                 sample_size=100,
                 replications=replications,
                 initial_distr=c(1,0,0),
                 which_duration = c("A"),
                 diff_duration = list(A=c(A=difftmp,B=-difftmp,C=0,X=0)),
                 interpolation_duration = list(A="switch1"))
  
  # Group B
  groupB <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=nonsticky,B=sticky,C=nonsticky,X=death),
                                   B=c(A=nonsticky,B=sticky,C=nonsticky,X=death),
                                   C=c(A=nonsticky,B=sticky,C=nonsticky,X=death)),
                 gen_duration=T, 
                 gen_age = F,
                 sample_size=100,
                 replications=replications,
                 initial_distr=c(0,1,0),
                 which_duration = c("B"),
                 diff_duration = list(B=c(A=-difftmp,B=difftmp,C=0,X=0)),
                 interpolation_duration = list(B="switch1"))
  
  # Group C
  groupC <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=nonsticky,B=nonsticky,C=sticky,X=death),
                                   B=c(A=nonsticky,B=nonsticky,C=sticky,X=death),
                                   C=c(A=nonsticky,B=nonsticky,C=sticky,X=death)),
                 gen_duration=T, 
                 gen_age = F,
                 sample_size=100,
                 replications=replications,
                 initial_distr=c(0,0,1),
                 which_duration = c("C"),
                 diff_duration = list(C=c(A=-difftmp,B=0,C=difftmp,X=0)),
                 interpolation_duration = list(C="switch1"))
  
  # Combine
  models[[length(models)+1]] <- list(groupA=groupA,groupB=groupB,groupC=groupC,
                                     replications=replications,sample_size=100)   
  
  
### Strong group differences, strong duration [DGP 5c] #########################
  
  # Probabilities
  sticky <- (1-death)*sticky3
  nonsticky <- (1-death-sticky)/2
  
  # Differences 
  difftmp <- min(nonsticky,sticky,diff3)
  
  # Group A
  groupA <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=sticky,B=nonsticky,C=nonsticky,X=death),
                                   B=c(A=sticky,B=nonsticky,C=nonsticky,X=death),
                                   C=c(A=sticky,B=nonsticky,C=nonsticky,X=death)),
                 gen_duration=T, 
                 gen_age = F,
                 sample_size=100,
                 replications=replications,
                 initial_distr=c(1,0,0),
                 which_duration = c("A"),
                 diff_duration = list(A=c(A=difftmp,B=-difftmp,C=0,X=0)),
                 interpolation_duration = list(A="switch1"))
  
  # Group B
  groupB <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=nonsticky,B=sticky,C=nonsticky,X=death),
                                   B=c(A=nonsticky,B=sticky,C=nonsticky,X=death),
                                   C=c(A=nonsticky,B=sticky,C=nonsticky,X=death)),
                 gen_duration=T, 
                 gen_age = F,
                 sample_size=100,
                 replications=replications,
                 initial_distr=c(0,1,0),
                 which_duration = c("B"),
                 diff_duration = list(B=c(A=-difftmp,B=difftmp,C=0,X=0)),
                 interpolation_duration = list(B="switch1"))
  
  # Group C
  groupC <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=nonsticky,B=nonsticky,C=sticky,X=death),
                                   B=c(A=nonsticky,B=nonsticky,C=sticky,X=death),
                                   C=c(A=nonsticky,B=nonsticky,C=sticky,X=death)),
                 gen_duration=T, 
                 gen_age = F,
                 sample_size=100,
                 replications=replications,
                 initial_distr=c(0,0,1),
                 which_duration = c("C"),
                 diff_duration = list(C=c(A=-difftmp,B=0,C=difftmp,X=0)),
                 interpolation_duration = list(C="switch1"))
  
  # Combine
  models[[length(models)+1]] <- list(groupA=groupA,groupB=groupB,groupC=groupC,
                                     replications=replications,sample_size=100)    
  
  
### Additional sample sizes ####################################################  
  
  # Additional sizes
  models_250 <- models
  models_500 <- models
  models_1000 <- models
  models_2500 <- models

  # Change sizes
  for(i in 1:length(models_250)) models_250[[i]]$sample_size <- 250
  for(i in 1:length(models_500)) models_500[[i]]$sample_size <- 500
  for(i in 1:length(models_1000)) models_1000[[i]]$sample_size <- 1000
  for(i in 1:length(models_2500)) models_2500[[i]]$sample_size <- 2500

  # Add
  models <- c(models,models_250,models_500,models_1000,models_2500)