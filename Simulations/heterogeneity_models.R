### Empty lists for models #####################################################

  models <- list()

### No group differences #######################################################
  
  # Probabilities
  death <- 0.02
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
  
### Very strong group differences ##############################################

  # Group A
  groupA <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=0.9,B=0.04,C=0.04,X=0.02),
                                   B=c(A=0.9,B=0.04,C=0.04,X=0.02),
                                   C=c(A=0.9,B=0.04,C=0.04,X=0.02)),
                                   gen_duration=F, 
                                   gen_age = F,
                                   sample_size=100,
                                   replications=250,
                                   initial_distr=c(1,0,0))

  # Group B
  groupB <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=0.04,B=0.9,C=0.04,X=0.02),
                                   B=c(A=0.04,B=0.9,C=0.04,X=0.02),
                                   C=c(A=0.04,B=0.9,C=0.04,X=0.02)),
                 gen_duration=F, 
                 gen_age = F,
                 sample_size=100,
                 replications=250,
                 initial_distr=c(0,1,0))
  
  # Group C
  groupC <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=0.04,B=0.04,C=0.9,X=0.02),
                                   B=c(A=0.04,B=0.04,C=0.9,X=0.02),
                                   C=c(A=0.04,B=0.04,C=0.9,X=0.02)),
                 gen_duration=F, 
                 gen_age = F,
                 sample_size=100,
                 replications=250,
                 initial_distr=c(0,0,1))
  
  # Combine
  models[[length(models)+1]] <- list(groupA=groupA,groupB=groupB,groupC=groupC,
                                     replications=250,sample_size=100)
  
  
### Strong group differences ###################################################
  
  # Group A
  groupA <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=0.75,B=0.115,C=0.115,X=0.02),
                                   B=c(A=0.75,B=0.115,C=0.115,X=0.02),
                                   C=c(A=0.75,B=0.115,C=0.115,X=0.02)),
                 gen_duration=F, 
                 gen_age = F,
                 sample_size=100,
                 replications=250,
                 initial_distr=c(1,0,0))
  
  # Group B
  groupB <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=0.115,B=0.75,C=0.115,X=0.02),
                                   B=c(A=0.115,B=0.75,C=0.115,X=0.02),
                                   C=c(A=0.115,B=0.75,C=0.115,X=0.02)),
                 gen_duration=F, 
                 gen_age = F,
                 sample_size=100,
                 replications=250,
                 initial_distr=c(0,1,0))
  
  # Group C
  groupC <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=0.115,B=0.115,C=0.75,X=0.02),
                                   B=c(A=0.115,B=0.115,C=0.75,X=0.02),
                                   C=c(A=0.115,B=0.115,C=0.75,X=0.02)),
                 gen_duration=F, 
                 gen_age = F,
                 sample_size=100,
                 replications=250,
                 initial_distr=c(0,0,1))
  
  # Combine
  models[[length(models)+1]] <- list(groupA=groupA,groupB=groupB,groupC=groupC,
                                     replications=250,sample_size=100) 
  
  
### Moderate group differences #################################################  

  # Group A
  groupA <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=0.4,B=0.29,C=0.29,X=0.02),
                                   B=c(A=0.4,B=0.29,C=0.29,X=0.02),
                                   C=c(A=0.4,B=0.29,C=0.29,X=0.02)),
                 gen_duration=F, 
                 gen_age = F,
                 sample_size=100,
                 replications=250,
                 initial_distr=c(1,0,0))
  
  # Group B
  groupB <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=0.29,B=0.4,C=0.29,X=0.02),
                                   B=c(A=0.29,B=0.4,C=0.29,X=0.02),
                                   C=c(A=0.29,B=0.4,C=0.29,X=0.02)),
                 gen_duration=F, 
                 gen_age = F,
                 sample_size=100,
                 replications=250,
                 initial_distr=c(0,1,0))
  
  # Group C
  groupC <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=0.29,B=0.29,C=0.4,X=0.02),
                                   B=c(A=0.29,B=0.29,C=0.4,X=0.02),
                                   C=c(A=0.29,B=0.29,C=0.4,X=0.02)),
                 gen_duration=F, 
                 gen_age = F,
                 sample_size=100,
                 replications=250,
                 initial_distr=c(0,0,1))
  
  # Combine
  models[[length(models)+1]] <- list(groupA=groupA,groupB=groupB,groupC=groupC,
                                     replications=250,sample_size=100) 
  
### Weak group differences #####################################################  
  
  # Group A
  groupA <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=0.35,B=0.315,C=0.315,X=0.02),
                                   B=c(A=0.35,B=0.315,C=0.315,X=0.02),
                                   C=c(A=0.35,B=0.315,C=0.315,X=0.02)),
                 gen_duration=F, 
                 gen_age = F,
                 sample_size=100,
                 replications=250,
                 initial_distr=c(1,0,0))
  
  # Group B
  groupB <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=0.315,B=0.35,C=0.315,X=0.02),
                                   B=c(A=0.315,B=0.35,C=0.315,X=0.02),
                                   C=c(A=0.315,B=0.35,C=0.315,X=0.02)),
                 gen_duration=F, 
                 gen_age = F,
                 sample_size=100,
                 replications=250,
                 initial_distr=c(0,1,0))
  
  # Group C
  groupC <- list(time_steps = 0:10,
                 transient  = c("A","B","C"),
                 absorbing  = "X",
                 probs      = list(A=c(A=0.315,B=0.315,C=0.35,X=0.02),
                                   B=c(A=0.315,B=0.315,C=0.35,X=0.02),
                                   C=c(A=0.315,B=0.315,C=0.35,X=0.02)),
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