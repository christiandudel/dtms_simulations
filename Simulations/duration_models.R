### Empty list for models ######################################################

  models <- list()

  
### No memory at all ###########################################################
  
  # For this model the current state does not matter
  models[[length(models)+1]] <-   list(time_steps = 0:10,
                                       transient  = c("A","B"),
                                       absorbing  = "X",
                                       probs      = list(A=c(A=0.5,B=0.5,X=0.0),
                                                         B=c(A=0.5,B=0.5,X=0.0)),
                                       gen_duration=F, 
                                       gen_age = F,
                                       sample_size=100,
                                       replications=250,
                                       initial_distr=c(0.5,0.5))
  

### Strong Markovian ###########################################################

  models[[length(models)+1]] <- list( time_steps = 0:10,
                                      transient  = c("A","B"),
                                      absorbing  = "X",
                                      probs      = list(A=c(A=0.8,B=0.19,X=0.01),
                                                        B=c(A=0.19,B=0.8,X=0.01)),
                                      gen_duration=F, 
                                      gen_age = F,
                                      sample_size=100,
                                      replications=250,
                                      initial_distr=c(0.5,0.5))
  

### Extreme Markovian ##########################################################
  
  # This is *almost* deterministic
  models[[length(models)+1]] <- list( time_steps = 0:10,
                                      transient  = c("A","B"),
                                      absorbing  = "X",
                                      probs      = list(A=c(A=0.99,B=0.01,X=0),
                                                        B=c(A=0.01,B=0.99,X=0)),
                                      gen_duration=F, 
                                      gen_age = F,
                                      sample_size=100,
                                      replications=250,
                                      initial_distr=c(0.5,0.5))

    
### Generated duration dependence (switch) #####################################
  
  # These demonstrate that it is possible to have relatively high values
  # of L(Delta)
  
  # Switch 1  
  models[[length(models)+1]] <- list(time_steps = 0:10,
                                     transient  = c("A","B"),
                                     absorbing  = "X",
                                     probs      = list(A=c(A=0.95,B=0.05,X=0),
                                                       B=c(A=0.05,B=0.95,X=0)),
                                     gen_age = F,
                                     gen_duration = T,
                                     which_duration = c("A","B"),
                                     diff_duration = list(A=c(A=-0.9,B=0.9,X=0),
                                                          B=c(A=0.9,B=-0.9,X=0)),
                                     interpolation_duration = list(A="switch1",B="switch1"),
                                     sample_size=100,
                                     replications=250,
                                     initial_distr=c(0.5,0.5))
  
  # Switch 2
  models[[length(models)+1]] <- models[[length(models)]]
  models[[length(models)]]$interpolation_duration <- list(A="switch2",B="switch2")
  
  # Switch 3
  models[[length(models)+1]] <- models[[length(models)]]
  models[[length(models)]]$interpolation_duration <- list(A="switch3",B="switch3")
  
  # Switch 4
  models[[length(models)+1]] <- models[[length(models)]]
  models[[length(models)]]$interpolation_duration <- list(A="switch4",B="switch4")
  
  # Switch 5
  models[[length(models)+1]] <- models[[length(models)]]
  models[[length(models)]]$interpolation_duration <- list(A="switch5",B="switch5")
  
  
### Generated duration dependence (switch) #####################################
  
  # This demonstrates the effect of length
  
  # Switch 1  
  models[[length(models)+1]] <- list(time_steps = 0:20,
                                     transient  = c("A","B"),
                                     absorbing  = "X",
                                     probs      = list(A=c(A=0.95,B=0.05,X=0),
                                                       B=c(A=0.05,B=0.95,X=0)),
                                     gen_age = F,
                                     gen_duration = T,
                                     which_duration = c("A","B"),
                                     diff_duration = list(A=c(A=-0.9,B=0.9,X=0),
                                                          B=c(A=0.9,B=-0.9,X=0)),
                                     interpolation_duration = list(A="switch1",B="switch1"),
                                     sample_size=100,
                                     replications=250,
                                     initial_distr=c(0.5,0.5))
  
  # Switch 2
  models[[length(models)+1]] <- models[[length(models)]]
  models[[length(models)]]$interpolation_duration <- list(A="switch2",B="switch2")
  
  # Switch 3
  models[[length(models)+1]] <- models[[length(models)]]
  models[[length(models)]]$interpolation_duration <- list(A="switch3",B="switch3")


### Generated duration dependence (switch) #####################################
  
  # This demonstrates the effect of probabilities
  
  # Switch 1  
  models[[length(models)+1]] <- list(time_steps = 0:10,
                                     transient  = c("A","B"),
                                     absorbing  = "X",
                                     probs      = list(A=c(A=0.95,B=0.05,X=0),
                                                       B=c(A=0.05,B=0.95,X=0)),
                                     gen_age = F,
                                     gen_duration = T,
                                     which_duration = c("A","B"),
                                     diff_duration = list(A=c(A=-0.25,B=0.25,X=0),
                                                          B=c(A=0.25,B=-0.25,X=0)),
                                     interpolation_duration = list(A="switch1",B="switch1"),
                                     sample_size=100,
                                     replications=250,
                                     initial_distr=c(0.5,0.5))
  
  # Switch 2
  models[[length(models)+1]] <- models[[length(models)]]
  models[[length(models)]]$interpolation_duration <- list(A="switch2",B="switch2")
  
  # Switch 3
  models[[length(models)+1]] <- models[[length(models)]]
  models[[length(models)]]$interpolation_duration <- list(A="switch3",B="switch3")
  
  
### Generated duration dependence (short) ######################################
  
  # Becoming more sticky  
  models[[length(models)+1]] <- list(time_steps = 0:10,
                                      transient  = c("A","B"),
                                      absorbing  = "X",
                                      probs      = list(A=c(A=0.1,B=0.89,X=0.01),
                                                        B=c(A=0.495,B=0.495,X=0.01)),
                                      gen_age = F,
                                      gen_duration = T,
                                      which_duration = "A",
                                      diff_duration = list(A=c(A=0.79,B=-0.79,X=0.1)),
                                      interpolation_duration = list(A="linear"),
                                      sample_size=100,
                                      replications=250,
                                      initial_distr=c(0.5,0.5))
  

### Generated duration dependence (long) #######################################
  
  # Becoming more sticky  
  models[[length(models)+1]] <- list(time_steps = 0:25,
                                     transient  = c("A","B"),
                                     absorbing  = "X",
                                     probs      = list(A=c(A=0.1,B=0.89,X=0.01),
                                                       B=c(A=0.495,B=0.495,X=0.01)),
                                     gen_age = F,
                                     gen_duration = T,
                                     which_duration = "A",
                                     diff_duration = list(A=c(A=0.79,B=-0.79,X=0.1)),
                                     interpolation_duration = list(A="linear"),
                                     sample_size=100,
                                     replications=250,
                                     initial_distr=c(0.5,0.5))
  

### Generated duration dependence (short) ######################################
  
  # Becoming less sticky  
  models[[length(models)+1]] <- list(time_steps = 0:10,
                                     transient  = c("A","B"),
                                     absorbing  = "X",
                                     probs      = list(A=c(A=0.89,B=0.1,X=0.01),
                                                       B=c(A=0.495,B=0.495,X=0.01)),
                                     gen_age = F,
                                     gen_duration = T,
                                     which_duration = "A",
                                     diff_duration = list(A=c(A=-0.79,B=0.79,X=0.1)),
                                     interpolation_duration = list(A="linear"),
                                     sample_size=100,
                                     replications=250,
                                     initial_distr=c(0.5,0.5))
  

### Generated duration dependence (long) #######################################
  
  # Becoming less sticky  
  models[[length(models)+1]] <- list(time_steps = 0:25,
                                     transient  = c("A","B"),
                                     absorbing  = "X",
                                     probs      = list(A=c(A=0.89,B=0.1,X=0.01),
                                                       B=c(A=0.495,B=0.495,X=0.01)),
                                     gen_duration = T,
                                     gen_age = F,
                                     which_duration = "A",
                                     diff_duration = list(A=c(A=-0.79,B=0.79,X=0.1)),
                                     interpolation_duration = list(A="linear"),
                                     sample_size=100,
                                     replications=250,
                                     initial_distr=c(0.5,0.5))

    
### Generated duration and age dependence (linear) #############################
  
  models[[length(models)+1]] <- list(time_steps = 0:20,
                                     transient  = c("A","B"),
                                     absorbing  = "X",
                                     probs      = list(A=c(A=0.1,B=0.89,X=0.01),
                                                       B=c(A=0.495,B=0.495,X=0.01)),
                                     gen_duration = T,
                                     which_duration = "A",
                                     diff_duration = list(A=c(A=0.19,B=-0.19,X=0.1)),
                                     interpolation_duration = list(A="linear"),
                                     gen_age = T,
                                     which_age = c("A","B"),
                                     diff_age = list(A=c(A=0,B=0,X=0.1),
                                                     B=c(A=0,B=0,X=0.2)),
                                     interpolation_age = list(A="linear",B="linear"),
                                     sample_size=100,
                                     replications=250,
                                     initial_distr=c(0.5,0.5))

  
### Generated duration and age dependence (sigmoid) #############################
  
  models[[length(models)+1]] <- list(time_steps = 0:20,
                                     transient  = c("A","B"),
                                     absorbing  = "X",
                                     probs      = list(A=c(A=0.1,B=0.89,X=0.01),
                                                       B=c(A=0.495,B=0.495,X=0.01)),
                                     gen_duration = T,
                                     which_duration = "A",
                                     diff_duration = list(A=c(A=0.79,B=-0.79,X=0.1)),
                                     interpolation_duration = list(A="linear"),
                                     gen_age = T,
                                     which_age = c("A","B"),
                                     diff_age = list(A=c(A=0,B=0,X=0.1),
                                                     B=c(A=0,B=0,X=0.2)),
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
