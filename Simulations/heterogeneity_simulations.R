### Packages ###################################################################

  # Packages
  library(dtms)
  library(VGAM)
  
  # Models and functions
  source("Functions/functions_simulation.R")
  source("Simulations/heterogeneity_models.R")
  
  # Seed
  set.seed(1701)


### Object for results #########################################################

  results <- list()


### Loop over models ###########################################################

  # Number of simulation models
  n_sims <- length(models)
  
  # Loop
  for(sim in 1:n_sims) {
    
    # Output
    cat("\n","Model ",sim,"\n")
    
    # Complete sim if necessary
    gensimA <- complete_sim(models[[sim]]$groupA)
    gensimB <- complete_sim(models[[sim]]$groupB)
    gensimC <- complete_sim(models[[sim]]$groupC)
    
    # Generate expanded transition probabilities
    simmodelA <- generate_sim(gensimA)
    simmodelB <- generate_sim(gensimB)
    simmodelC <- generate_sim(gensimC)
    
    # General transient states
    transient_states <- gensimA$transient
    
    # For transition matrix: add duration to states
    if(gensimA$gen_duration) {
      transient_statesA <- levels(interaction(gensimA$which_duration,1:(max(gensimA$time_steps)+1),sep=""))
      transient_statesA <- c(transient_statesA,gensimA$transient[!gensimA$transient%in%gensimA$which_duration])
    } else transient_statesA <- gensimA$transient
    
    if(gensimB$gen_duration) {
      transient_statesB <- levels(interaction(gensimB$which_duration,1:(max(gensimB$time_steps)+1),sep=""))
      transient_statesB <- c(transient_statesB,gensimB$transient[!gensimB$transient%in%gensimB$which_duration])
    } else transient_statesB <- gensimB$transient
    
    if(gensimC$gen_duration) {
      transient_statesC <- levels(interaction(gensimC$which_duration,1:(max(gensimC$time_steps)+1),sep=""))
      transient_statesC <- c(transient_statesC,gensimC$transient[!gensimC$transient%in%gensimC$which_duration])
    } else transient_statesC <- gensimC$transient
    
    # General model
    generalA <- dtms(transient=transient_statesA,
                    absorbing=gensimA$absorbing,
                    timescale=gensimA$time_steps)
    
    generalB <- dtms(transient=transient_statesB,
                     absorbing=gensimB$absorbing,
                     timescale=gensimB$time_steps)
    
    generalC <- dtms(transient=transient_statesC,
                     absorbing=gensimC$absorbing,
                     timescale=gensimC$time_steps)
    
    # Transition matrix
    TmA <- dtms_matrix(probs=simmodelA,
                      dtms=generalA)
    
    TmB <- dtms_matrix(probs=simmodelB,
                       dtms=generalB)
    
    TmC <- dtms_matrix(probs=simmodelC,
                       dtms=generalC)
    
    # Settings
    replications <- models[[sim]]$replications
    sample_size <- models[[sim]]$sample_size
    whichmore <- sample(1:3,1)
    gensimA$sample_size <- floor(sample_size/3+ifelse(whichmore==1,1,0))
    gensimB$sample_size <- floor(sample_size/3+ifelse(whichmore==2,1,0))
    gensimC$sample_size <- floor(sample_size-(gensimA$sample_size+gensimB$sample_size))
    nlength <- length(gensimA$time_steps)
    initial_distrA <- gensimA$initial_distr
    initial_distrB <- gensimB$initial_distr
    initial_distrC <- gensimC$initial_distr
    
    # For results
    results_Ex <- list()
    results_Ec <- list()
    results_Vs <- list()
    results_Va <- list()

    # Loop over replications
    for(rep_nr in 1:replications) {
      
      # Output
      cat(".")
      
      # Empty data frame to build up
      simdata <- dtms_simulate(matrix=TmA,
                               dtms=generalA,
                               size=1)
      simdata$id <- 1
      simdata <- simdata[-1,]
      
      # Loop over groups
      for(group in c("A","B","C")) {
      
        # Objects
        gensimgroup <- get(paste0("gensim",group))
        transientgroup <- get(paste0("transient_states",group))
        values <- gensimgroup$initial_distr
        general <- get(paste0("general",group))
        
        # Starting distribution
        if(gensimgroup$gen_duration) {
          starting_distr <- numeric(length(transientgroup))
          whichtofill <- match(transient_states,substr(transientgroup,1,1))
          starting_distr[whichtofill] <- values
        }  else starting_distr <- values
        
        # Simulate data
        tmpdata <- dtms_simulate(matrix=get(paste0("Tm",group)),
                                 dtms=general,
                                 size=gensimgroup$sample_size,
                                 start_distr=starting_distr)
        
        # Simplify
        tmpdata <- simplifydata(tmpdata)
        
        # Add IDs
        tmpdata$id <- paste0(group,1:gensimgroup$sample_size)
        
        # Combine
        simdata <- rbind(simdata,tmpdata)
      
      }

      # Reshape
      simdata <- simdata %>% pivot_longer(cols=starts_with("T_"),
                                          names_prefix="T_",
                                          names_to="time",
                                          values_to="state")
      
      # Change time var
      class(simdata$time) <- "numeric"
      
      # Copy
      tmpdata <- simdata
      
      # Simulation dtms
      simdtms <- dtms(transient = gensimA$transient,
                      timescale = gensimA$time_steps,
                      absorbing = gensimA$absorbing)
      
      # Transition format
      simdata <- dtms_format(data=simdata,
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
      fit <- dtms_fullfit(data     = simdata,
                          controls ="s(time)")
      
      # Predict probabilities for transition matrix
      model1_p <- dtms_transitions(model    = fit,
                                   dtms     = simdtms,
                                   controls = list(time=simdtms$timescale),
                                   se=F)
      
      # Get into transition matrix
      sim1T <- dtms_matrix(dtms=simdtms,
                           probs=model1_p)
      
      ### Matrix results
      n_transient <- length(gensimA$transient)
      starting_states <- paste0(gensimA$transient,"_0")
      
      # State expectancies 
      results_exp <- dtms_expectancy(matrix=sim1T,
                                     dtms=simdtms,
                                     start_distr=starting_distr,
                                     total = F)
      
      # Unconditional
      results_ex <- as.numeric(results_exp["AVERAGE",])
      names(results_ex) <- transient_states
      
      # Conditional
      results_ec <- as.numeric(results_exp[1:n_transient,])
      names(results_ec) <- levels(interaction(rownames(results_exp)[1:n_transient],
                                              colnames(results_exp)))
      
      # Probability of ever reaching a transient state
      results_ever <- numeric(n_transient)
      names(results_ever) <- transient_states
      
      for(whichstate in transient_states) {
        
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
                                controls ="s(time)")
        
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
      names(results_var) <- transient_states
      for(state in transient_states) {
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

  filename <- paste0("Results/dtms_sims_het.Rda")
  save(list=c("models","results"),
       file=filename)
  
  ### Clear memory
  rm(list=ls())
  gc()