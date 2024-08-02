### Empty lists for models #####################################################

  models <- list()

  
### No unobserved heterogeneity [DGP 2] ########################################
  
  # Probabilities
  death <- 0.025
  even <- (1-death)/3
  
  # Group A
  groupA <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=even,B=even,C=even,X=death),
                                   B=c(A=even,B=even,C=even,X=death),
                                   C=c(A=even,B=even,C=even,X=death)),
                 gen_duration=F, 
                 gen_age = F,
                 sample_size=100,
                 replications=250,
                 initial_distr=c(1,0,0))
  
  # Group B
  groupB <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=even,B=even,C=even,X=death),
                                   B=c(A=even,B=even,C=even,X=death),
                                   C=c(A=even,B=even,C=even,X=death)),
                 gen_duration=F, 
                 gen_age = F,
                 sample_size=100,
                 replications=250,
                 initial_distr=c(0,1,0))
  
  # Group C
  groupC <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=even,B=even,C=even,X=death),
                                   B=c(A=even,B=even,C=even,X=death),
                                   C=c(A=even,B=even,C=even,X=death)),
                 gen_duration=F, 
                 gen_age = F,
                 sample_size=100,
                 replications=250,
                 initial_distr=c(0,0,1))
  
  # Combine
  models[[length(models)+1]] <- list(groupA=groupA,groupB=groupB,groupC=groupC,
                                     replications=250,sample_size=100)
  
  
### Weak group differences [DGP 4a] ############################################
  
  # Probabilities
  death <- 0.025
  sticky <- (1-death)*0.4
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
                 replications=250,
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
                 replications=250,
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
                 replications=250,
                 initial_distr=c(0,0,1))
  
  # Combine
  models[[length(models)+1]] <- list(groupA=groupA,groupB=groupB,groupC=groupC,
                                     replications=250,sample_size=100)   
  
  
### Strong group differences [DGP 4b] ##########################################
  
  # Probabilities
  death <- 0.025
  sticky <- (1-death)*0.6
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
                 replications=250,
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
                 replications=250,
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
                 replications=250,
                 initial_distr=c(0,0,1))
  
  # Combine
  models[[length(models)+1]] <- list(groupA=groupA,groupB=groupB,groupC=groupC,
                                     replications=250,sample_size=100)   
  
  
### Very strong group differences [DGP 4c] #####################################

  # Probabilities
  death <- 0.025
  sticky <- (1-death)*0.8
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
                 replications=250,
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
                 replications=250,
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
                 replications=250,
                 initial_distr=c(0,0,1))
  
  # Combine
  models[[length(models)+1]] <- list(groupA=groupA,groupB=groupB,groupC=groupC,
                                     replications=250,sample_size=100)
  
  
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