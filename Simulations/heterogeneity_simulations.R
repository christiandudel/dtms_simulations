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
    
    # For transition matrix: add duration to states
    if(gensimA$gen_duration) {
      transient_states <- levels(interaction(gensimA$which_duration,1:(max(gensimA$time_steps)+1),sep=""))
      transient_states <- c(transient_states,gensimA$transient[!gensimA$transient%in%gensimA$which_duration])
    } else transient_states <- gensimA$transient
    
    # General model
    general <- dtms(transient=transient_states,
                    absorbing=gensimA$absorbing,
                    timescale=gensimA$time_steps)
    
    # Transition matrix
    TmA <- dtms_matrix(probs=simmodelA,
                      dtms=general)
    
    TmB <- dtms_matrix(probs=simmodelB,
                       dtms=general)
    
    TmC <- dtms_matrix(probs=simmodelC,
                       dtms=general)
    
    
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
    results_D <- list()
    
    # Loop over replications
    for(rep_nr in 1:replications) {
      
      # Output
      cat(".")
      
      # Empty data frame to build up
      simdata <- tibble(id=character(),time=numeric(),state=character())
      
      # Loop over groups
      for(group in c("A","B","C")) {
      
        # Starting distribution
        gensim <- get(paste0("gensim",group))
        values <- gensim$initial_distr
        if(gensim$gen_duration) {
          starting_distr <- numeric(length(transient_states))
          starting_distr[1:length(values)] <- values
        }  else starting_distr <- values
        
        
        # Simulate data
        tmpdata <- dtms_simulate(matrix=get(paste0("Tm",group)),
                                 dtms=general,
                                 size=gensim$sample_size,
                                 start_distr=starting_distr)
        
        # Simplify
        tmpdata <- simplifydata(tmpdata)
        
        # Add IDs
        tmpdata$id <- paste0(group,1:gensim$sample_size)
        
        # Reshape
        tmpdata <- tmpdata %>% pivot_longer(cols=starts_with("T_"),
                                            names_prefix="T_",
                                            names_to="time",
                                            values_to="state")
        
        # Change time var
        class(tmpdata$time) <- "numeric"
        
        # Combine
        simdata <- rbind(simdata,tmpdata)
      
      }
      
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
                          controls = "time")
      
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
      names(results_ex) <- gensim$transient
      
      # Conditional
      results_ec <- as.numeric(results_exp[1:n_transient,])
      names(results_ec) <- levels(interaction(rownames(results_exp)[1:n_transient],
                                              colnames(results_exp)))
      
      # Probability of ever reaching a transient state
      results_ever <- numeric(n_transient)
      names(results_ever) <- gensim$transient
      
      for(whichstate in gensim$transient) {
        
        # DTMS
        everdtms <- dtms(transient = gensim$transient[-which(gensim$transient==whichstate)],
                         timescale = gensim$time_steps,
                         absorbing = c(gensim$absorbing,whichstate))
        
        # Transition format
        everdata <- dtms_format(data=tmpdata,
                                dtms=everdtms,
                                verbose=F)
        
        # Cleaning
        everdata <- dtms_clean(data    = everdata,
                               dtms    = everdtms,
                               verbose = F)
        
        # Minor fix for small sample size: add X if not in data
        if(!any(everdata$to%in%gensim$absorbing)) {
          howmanyabsorbing <- length(gensim$absorbing)
          datasize <- dim(everdata)[1]
          for(abs in 1:howmanyabsorbing) everdata$to[sample(1:datasize,2)] <- rep(gensim$absorbing[abs],2)
        }
        
        # Starting distribution
        ever_distr <- dtms_start(dtms = everdtms,
                                 data = everdata)
        
        # Get model formula right 
        if(length(everdtms$transient)==1) everfitform <- formula(to~1+time) else 
          everfitform <- formula(to~from+time)
        
        # Estimate model
        everfit <- dtms_fullfit(data     = everdata,
                                formula = everfitform)
        
        # Predict probabilities for transition matrix
        ever_p <- dtms_transitions(model    = everfit,
                                   dtms     = everdtms,
                                   controls = list(time=everdtms$timescale),
                                   se=F)
        
        # Get into transition matrix
        everT <- dtms_matrix(dtms=everdtms,
                             probs=ever_p)
        
        # Risk 
        tmp <- dtms_risk(matrix=everT,
                         risk=whichstate,
                         dtms=everdtms,
                         start_distr = ever_distr)
        
        # Probability of starting in absorbing state
        pr <- which(names(results_ever)==whichstate)
        pr <- starting_distr[pr]
        
        # Assign
        results_ever[whichstate] <- tmp[1]*(1-pr)+pr
        
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
      
      ### Dissimilarity index
      # results_dsim <-  dtms_delta(data= as.data.frame(simdata),
      #                             dtms= simdtms,
      #                             controls = "time",
      #                             lags=1:5)
      
      ### Place in lists for results
      results_Ex[[rep_nr]] <- results_ex
      results_Ec[[rep_nr]] <- results_ec
      results_Vs[[rep_nr]] <- results_ever
      results_Va[[rep_nr]] <- results_var
      results_D[[rep_nr]] <- NULL#results_dsim
      
      
    } # End of replication loop
    
    # Place in result list
    results[[sim]] <- list(results_Ex=results_Ex,
                           results_Ec=results_Ec,
                           results_Vs=results_Vs,
                           results_Va=results_Va,
                           results_D=results_D)
    
  } # End of model loop


### Save results ###############################################################

  filename <- paste0("Results/dtms_sims_het.Rda")
  save(list=c("models","results"),
       file=filename)
  
  ### Clear memory
  rm(list=ls())
  gc()