### Empty list for models ######################################################

  models <- list()

  
### Setup ######################################################################
  
  death <- 0.02
  even <- (1-death)/2
  diff1 <- 0.1
  diff2 <- 0.30
  diff3 <- 0.45
  
  
### Fully Markovian [DGP 1] ####################################################
  
  models[[length(models)+1]] <-   list(time_steps = 0:10,
                                       transient  = c("A","B"),
                                       absorbing  = "X",
                                       probs      = list(A=c(A=even,B=even,X=death),
                                                         B=c(A=even,B=even,X=death)),
                                       gen_duration=F, 
                                       gen_age = F,
                                       sample_size=100,
                                       replications=250,
                                       initial_distr=c(0.5,0.5))
  
  
### Light violation (switch 1) [DGP 3a] ########################################
  
  models[[length(models)+1]] <- list(time_steps = 0:10,
                                     transient  = c("A","B"),
                                     absorbing  = "X",
                                     probs      = list(A=c(A=even,B=even,X=death),
                                                       B=c(A=even,B=even,X=death)),
                                     gen_age = F,
                                     gen_duration = T,
                                     which_duration = c("A","B"),
                                     diff_duration = list(A=c(A=diff1,B=-diff1,X=0),
                                                          B=c(A=-diff1,B=diff1,X=0)),
                                     interpolation_duration = list(A="switch1",B="switch1"),
                                     sample_size=100,
                                     replications=250,
                                     initial_distr=c(0.5,0.5))

  
### Moderate violation (switch 1) [DGP 3b] #####################################
  
  models[[length(models)+1]] <- list(time_steps = 0:10,
                                     transient  = c("A","B"),
                                     absorbing  = "X",
                                     probs      = list(A=c(A=even,B=even,X=death),
                                                       B=c(A=even,B=even,X=death)),
                                     gen_age = F,
                                     gen_duration = T,
                                     which_duration = c("A","B"),
                                     diff_duration = list(A=c(A=diff2,B=-diff2,X=0),
                                                          B=c(A=-diff2,B=diff2,X=0)),
                                     interpolation_duration = list(A="switch1",B="switch1"),
                                     sample_size=100,
                                     replications=250,
                                     initial_distr=c(0.5,0.5))

    
### Strong violation (switch 1) [DGP 3c] #######################################
  
  models[[length(models)+1]] <- list(time_steps = 0:10,
                                     transient  = c("A","B"),
                                     absorbing  = "X",
                                     probs      = list(A=c(A=even,B=even,X=death),
                                                       B=c(A=even,B=even,X=death)),
                                     gen_age = F,
                                     gen_duration = T,
                                     which_duration = c("A","B"),
                                     diff_duration = list(A=c(A=diff3,B=-diff3,X=0),
                                                          B=c(A=-diff3,B=diff3,X=0)),
                                     interpolation_duration = list(A="switch1",B="switch1"),
                                     sample_size=100,
                                     replications=250,
                                     initial_distr=c(0.5,0.5))  


### Strong violation (switch 2) ################################################
  
  models[[length(models)+1]] <- list(time_steps = 0:10,
                                     transient  = c("A","B"),
                                     absorbing  = "X",
                                     probs      = list(A=c(A=even,B=even,X=death),
                                                       B=c(A=even,B=even,X=death)),
                                     gen_age = F,
                                     gen_duration = T,
                                     which_duration = c("A","B"),
                                     diff_duration = list(A=c(A=diff3,B=-diff3,X=0),
                                                          B=c(A=-diff3,B=diff3,X=0)),
                                     interpolation_duration = list(A="switch2",B="switch2"),
                                     sample_size=100,
                                     replications=250,
                                     initial_distr=c(0.5,0.5))    
  
  
### Strong violation (linear, short) ###########################################

  models[[length(models)+1]] <- list(time_steps = 0:10,
                                      transient  = c("A","B"),
                                      absorbing  = "X",
                                      probs      = list(A=c(A=even,B=even,X=death),
                                                        B=c(A=even,B=even,X=death)),
                                      gen_age = F,
                                      gen_duration = T,
                                      which_duration = c("A","B"),
                                      diff_duration = list(A=c(A=diff3,B=-diff3,X=0),
                                                           B=c(A=-diff3,B=diff3,X=0)),
                                      interpolation_duration = list(A="linear",B="linear"),
                                      sample_size=100,
                                      replications=250,
                                      initial_distr=c(0.5,0.5))
  

### Strong violation (linear, long) ############################################
  
  models[[length(models)+1]] <- list(time_steps = 0:25,
                                     transient  = c("A","B"),
                                     absorbing  = "X",
                                     probs      = list(A=c(A=even,B=even,X=death),
                                                       B=c(A=even,B=even,X=death)),
                                     gen_age = F,
                                     gen_duration = T,
                                     which_duration = c("A","B"),
                                     diff_duration = list(A=c(A=diff3,B=-diff3,X=0),
                                                          B=c(A=-diff3,B=diff3,X=0)),
                                     interpolation_duration = list(A="linear",B="linear"),
                                     sample_size=100,
                                     replications=250,
                                     initial_distr=c(0.5,0.5))


### Strong violation, non-homogeneous (linear, long) #############################
  
  models[[length(models)+1]] <- list(time_steps = 0:25,
                                     transient  = c("A","B"),
                                     absorbing  = "X",
                                     probs      = list(A=c(A=even,B=even,X=death),
                                                       B=c(A=even,B=even,X=death)),
                                     gen_duration = T,
                                     which_duration = c("A","B"),
                                     diff_duration = list(A=c(A=diff3,B=-diff3,X=0),
                                                          B=c(A=-diff3,B=diff3,X=0)),
                                     interpolation_duration = list(A="linear",B="linear"),
                                     gen_age = T,
                                     which_age = c("A","B"),
                                     diff_age = list(A=c(A=-0.1,B=0.1,X=0.1),
                                                     B=c(A=-0.1,B=0.1,X=0.2)),
                                     interpolation_age = list(A="linear",B="linear"),
                                     sample_size=100,
                                     replications=250,
                                     initial_distr=c(0.5,0.5))

  
### Strong violation, non-homogeneous (sigmoid, long) ##########################
  
  models[[length(models)+1]] <- list(time_steps = 0:25,
                                     transient  = c("A","B"),
                                     absorbing  = "X",
                                     probs      = list(A=c(A=even,B=even,X=death),
                                                       B=c(A=even,B=even,X=death)),
                                     gen_duration = T,
                                     which_duration = c("A","B"),
                                     diff_duration = list(A=c(A=diff3,B=-diff3,X=0),
                                                          B=c(A=-diff3,B=diff3,X=0)),
                                     interpolation_duration = list(A="sigmoid",B="sigmoid"),
                                     gen_age = T,
                                     which_age = c("A","B"),
                                     diff_age = list(A=c(A=-0.1,B=0.1,X=0.1),
                                                     B=c(A=-0.1,B=0.1,X=0.2)),
                                     interpolation_age = list(A="sigmoid",B="sigmoid"),
                                     sample_size=100,
                                     replications=250,
                                     initial_distr=c(0.5,0.5))
  

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
