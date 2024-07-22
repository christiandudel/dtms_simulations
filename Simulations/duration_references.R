### Note: (most) reference values could be calculated analytically, but that 
### would likely rely on the functions from dtms. Instead, we simulate large
### populations and then calculate reference values based on basic descriptive
### statistics. This way, the reference values also serve as a check for
### the dtms functions. Sample size is large enough to keep errors 
### introduced by this negligible.

### Packages ###################################################################
  
  # Packages
  library(dtms)
  
  # Models and functions
  source("Functions/functions_simulation.R")
  source("Simulations/duration_models.R")

  # Simulation size
  simsize <- 250000
  
  # Seed
  set.seed(2610)
  
  
### Object for results #########################################################
  
  references <- list()
  
  
### Loop over models ###########################################################
  
  # Number of simulation models (only subset, as most simulations only differ by sample size)
  n_sims <- length(models_250)

  # Loop
  for(sim in 1:n_sims) {
    
    # Output for control
    cat(".")
    
    # Complete sim
    gensim <- complete_sim(models[[sim]])
    
    # Generate expanded transition probabilities
    simmodel <- generate_sim(gensim)

    # For transition matrix: add duration to states
    if(gensim$gen_duration) {
      transient_states <- levels(interaction(gensim$which_duration,1:(max(gensim$time_steps)+1),sep=""))
      transient_states <- c(transient_states,gensim$transient[!gensim$transient%in%gensim$which_duration])
    } else transient_states <- gensim$transient
    
    # General model
    general <- dtms(transient=transient_states,
                    absorbing=gensim$absorbing,
                    timescale=gensim$time_steps)
    
    # Transition matrix
    Tm <- dtms_matrix(probs=simmodel,
                      dtms=general)

    # Starting distribution
    values <- gensim$initial_distr
    if(gensim$gen_duration) {
      starting_distr <- numeric(length(transient_states))
      starting_distr[1:length(values)] <- values
    }  else starting_distr <- values
      
    ### Simulate some results
    simdata <- dtms_simulate(matrix=Tm,
                             dtms=general,
                             size=simsize,
                             start_distr=starting_distr,
                             droplast=F)
    
    # Simplify
    simdata <- simplifydata(simdata)
    
    # Count states
    transient_states <- unique(simplify(gensim$transient))
    for(state in transient_states) {
      simdata[state] <- apply(simdata,1,function(x) sum(x==state))
      simdata[simdata[,1]==state,state] <- simdata[simdata[,1]==state,state]-0.5
    }
    
    # Unconditional expectancy
    results_exp <- numeric(length(transient_states))
    names(results_exp) <- transient_states
    for(state in transient_states) {
      results_exp[state] <- mean(simdata[,state])
    }
    
    # Conditional expectancy
    results_ec <- numeric(2*length(transient_states))
    names(results_ec) <- levels(interaction(transient_states,transient_states))
    for(expect_state in transient_states) {
      
      for(starting_state in transient_states) {
        results_ec[paste(starting_state,expect_state,sep=".")] <- mean(simdata[simdata$T_0==starting_state,expect_state])
      }
      
    }
    
    # Ever reaching a state
    results_ever <- numeric(length(transient_states))
    names(results_ever) <- transient_states
    for(state in transient_states) {
      results_ever[state] <- sum(simdata[state]>0)/simsize
    }
    
    # Variance
    results_var <- numeric(length(transient_states))
    names(results_var) <- transient_states
    for(state in transient_states) {
      results_var[state] <- var(simdata[state])
    }
    
    # Place results
    references[[sim]] <- list(results_Ex=results_exp,
                              results_Ec=results_ec,
                              results_Va=results_var,
                              results_Vs=results_ever)
 
  } # End of model loop
 
  # Copy references 
  all_sims <- length(models)
  
  references <- rep(references,all_sims/n_sims)
   
### Save #######################################################################
  
  filename <- paste0("Results/dtms_sims_references.Rda")
  save(list=c("models","references"),
        file=filename)

### Clear memory
  
  rm(list=ls())
  gc()