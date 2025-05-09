### Packages ###################################################################

  # Packages
  library(dtms)
  library(VGAM)

  # Models and functions
  source("Functions/functions_simulation.R")
  source("Setup/duration_models.R")
  source("Setup/parameters.R")


### Object for results #########################################################

  results <- list()

  
### Loop over models ###########################################################
  
  # Number of simulation models
  n_sims <- length(models)

  # Loop
  for(sim in 1:n_sims) {
    
    # Output
    cat("\n","Model ",sim,"\n")
    
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
    
    # Settings
    replications <- gensim$replications
    sample_size <- gensim$sample_size
    nlength <- length(gensim$time_steps)

    # For results
    results_Ex <- list()
    results_Ec <- list()
    results_Vs <- list()
    results_Va <- list()

    # Loop over replications
    for(rep_nr in 1:replications) {
      
      # Output
      cat(".")
      
      # Starting distribution
      values <- gensim$initial_distr
      if(gensim$gen_duration) {
        starting_distr <- numeric(length(transient_states))
        starting_distr[1:length(values)] <- values
      }  else starting_distr <- values
      
      
      # Simulate data
      tmpdata <- dtms_simulate(matrix=Tm,
                               dtms=general,
                               size=sample_size,
                               start_distr=starting_distr,
                               droplast=T)

      # Simplify
      tmpdata <- simplifydata(tmpdata)

      # Add IDs
      tmpdata$id <- 1:sample_size
      
      # Reshape
      tmpdata <- tmpdata %>% pivot_longer(cols=starts_with("T_"),
                                            names_prefix="T_",
                                            names_to="time",
                                            values_to="state")
      
      class(tmpdata$time) <- "numeric"

      # Simulation dtms
      simdtms <- dtms(transient = gensim$transient,
                      timescale = gensim$time_steps,
                      absorbing = gensim$absorbing)
      
      # Transition format
      simdata <- dtms_format(data=tmpdata,
                             dtms=simdtms,
                             verbose=F)
      
      # Cleaning
      simdata <- dtms_clean(data    = simdata,
                            dtms    = simdtms,
                            verbose = F)
      
      # Starting distribution
      starting_distr <- dtms_start(dtms = simdtms,
                                   data = simdata)
      
      # Estimate model
      fit <- dtms_fullfit(data = simdata,
                          controls =timecontrol)

      # Predict probabilities for transition matrix
      model1_p <- dtms_transitions(model    = fit,
                                   dtms     = simdtms,
                                   controls = list(time=simdtms$timescale),
                                   se=F)
      
      # Get into transition matrix
      sim1T <- dtms_matrix(dtms=simdtms,
                           probs=model1_p)

      ### Matrix results
      n_transient <- length(gensim$transient)
      starting_states <- paste0(gensim$transient,"_0")

      # State expectancies 
      results_exp <- dtms_expectancy(matrix=sim1T,
                                     dtms=simdtms,
                                     start_distr=starting_distr,
                                     total = F)
      
      # Unconditional
      results_ex <- as.numeric(results_exp["AVERAGE",])
      names(results_ex) <- gensim$transient
      
      # Conditional
      results_ec <- as.numeric(results_exp[1:2,])
      names(results_ec) <- levels(interaction(rownames(results_exp)[1:2],
                                 colnames(results_exp)))
      
      # Probability of ever reaching a transient state
      results_ever <- numeric(n_transient)
      names(results_ever) <- gensim$transient
      
      for(whichstate in gensim$transient) {

        # Carry forward 
        riskdata <- dtms_forward(data=as.data.frame(tmpdata),
                                 state=whichstate,
                                 statevar="state",
                                 dtms=simdtms)
        
        # Transition format
        riskdata <- dtms_format(data=riskdata,
                               dtms=simdtms,
                               verbose=F)
        
        # Cleaning
        riskdata <- dtms_clean(data    = riskdata,
                              dtms    = simdtms,
                              verbose = F)
        
        # Starting distribution
        starting_risk <- dtms_start(dtms = simdtms,
                                     data = riskdata)
        
        # Estimate model
        riskfit <- dtms_fullfit(data     = riskdata,
                                controls =timecontrol)
        
        # Predict probabilities for transition matrix
        model1_risk <- dtms_transitions(model    = riskfit,
                                        dtms     = simdtms,
                                        controls = list(time=simdtms$timescale),
                                        se=F)
        
        # Get into matrix
        riskT <- dtms_matrix(dtms=simdtms,
                             probs=model1_risk)

        # Get risk        
        tmp <- dtms_risk(matrix=riskT,
                         risk=whichstate,
                         dtms=simdtms,
                         start_distr = starting_risk)

        # Assign
        results_ever[whichstate] <- tmp["AVERAGE"]
        
      }
      
      # Variance
      results_var <- numeric(n_transient)
      names(results_var) <- gensim$transient
      for(state in gensim$transient) {
        tmp <- dtms_visits(matrix=sim1T,
                           risk=state,
                           dtms=simdtms,
                           start_distr = starting_distr)
        
        results_var[state] <- sim_var(tmp)["AVERAGE"]
      }

      ### Place in lists for results
      results_Ex[[rep_nr]] <- results_ex
      results_Ec[[rep_nr]] <- results_ec
      results_Vs[[rep_nr]] <- results_ever
      results_Va[[rep_nr]] <- results_var

    } # End of replication loop
    
    # Place in result list
    results[[sim]] <- list(results_Ex=results_Ex,
                           results_Ec=results_Ec,
                           results_Vs=results_Vs,
                           results_Va=results_Va)
    
  } # End of model loop

  
### Save results ###############################################################

  filename <- paste0("Results/dtms_sims.Rda")
  save(list=c("models","results"),
       file=filename)

  ### Clear memory
  rm(list=ls())
  gc()