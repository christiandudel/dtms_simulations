time_steps = 0:10
transient  = c("A","B")
absorbing  = "X"
probs      = list(A=c(A=0.95,B=0.05,X=0),
                  B=c(A=0.05,B=0.95,X=0))
gen_age = F
gen_duration = T
which_duration = c("A","B")
diff_duration = list(A=c(A=-0.25,B=0.25,X=0),
        B=c(A=0.25,B=-0.25,X=0))
interpolation_duration = list(A="switch1",B="switch1")
sample_size=100
replications=1000
initial_distr=c(0.5,0.5)
which_age =NULL
diff_age = NULL
interpolation_age= NULL

################################################################################
  
# Packages
require(tidyverse)

# Get full state space
if(gen_duration) {
  dependent_states <- levels(interaction(which_duration,1:(max(time_steps)+1),sep=""))
  independent_states <- transient[!(transient%in%which_duration)]
  state_space_full <- c(dependent_states,independent_states,absorbing)
  state_space_red <- c(transient,absorbing)
} else state_space_full <- state_space_red <- c(transient,absorbing)

# Number of states
n_transient <- length(transient)

# Create data frame
model_frame <- expand.grid(from=state_space_full,
                           to=state_space_full,
                           time=time_steps,
                           stringsAsFactors=F)

# Drop absorbing states as starting states
model_frame <- model_frame %>% filter(!from%in%absorbing)

# Transition probabilities empty
model_frame$P <- 0

# Duration dependence: select states which are not duration dependent
take <- rep(T,n_transient)
if(gen_duration) {
  donttake <- which(transient%in%which_duration)
  take[donttake] <- F
} 

# Place probabilities (no generated duration dependence)
for(i in transient[take]) {
  for(j in state_space_red) {
    p <- probs[[i]][j]
    if(j %in% which_duration) {
      j_to <- paste0(j,"1")
    } else j_to <- j
    model_frame <- model_frame %>% mutate(P=ifelse(from==i & to==j_to,p,P))
  }
}

# Generated duration dependence
for(i in which_duration) {
  for(j in state_space_red) {
    
    # Get duration states
    i_states <- paste0(i,1:(max(time_steps)))
    if(i==j) it_states <- paste0(i,2:(max(time_steps)+1))
    
    # Get starting probability
    first <- probs[[i]][j]
    
    # Is j also duration dependent? Not used if i==j
    if(j %in% which_duration & j!=i) {
      j_to <- paste0(j,"1")
    } else j_to <- j
    
    # Check if any dependence for transition to state j, if no dependence move on to next j
    difference <- diff_duration[[i]][j]
    if(difference==0) {
      model_frame <- model_frame %>% mutate(P=ifelse(from%in%i_states & to==j_to,first,P))
      next
    } 
    
    # Interpolate
    nsteps <- length(i_states)-1
    
    # Linear interpolation
    if(str_detect(interpolation_duration[[i]],"linear")) {
      values <- seq(first,first+difference,length.out=nsteps+1)
    }
    
    # Sigmoid interpolation
    if(str_detect(interpolation_duration[[i]],"sigmoid")) {
      values <- seq(-15,15,
                    length.out=nsteps)
      values <- plogis(values)*difference
      values <- values+first
      values <- c(first,values) 
    }
    
    # Random values
    if(str_detect(interpolation_duration[[i]],"random")) {
      low <- min(first,first+difference)
      high <- max(first,first+difference)
      values <- c(first,
                  runif(min=low,
                        max=high,
                        n=nsteps))
    }
    
    # Switching regime
    if(str_detect(interpolation_duration[[i]],"switch")) {
      where <- str_sub(interpolation_duration[[i]],start=7)
      where <- as.numeric(where)
      values <- c(rep(first,where),
                  rep(first+difference,
                      (nsteps+1)-where))
    }
    
    # Place values
    for(time in 1:length(values)) {
      i_from <- i_states[time]
      if(j==i) {
        j_to <- it_states[time]} else if(j %in% which_duration & j!=i) {
          j_to <- paste0(j,"1")
        } else j_to <- j
        p <- values[time]
        model_frame <- model_frame %>% mutate(P=ifelse(from==i_from & to==j_to,p,P))
    }
    
    # Last state only dies
    last_state <- last(it_states)
    model_frame <- model_frame %>% mutate(P=ifelse(from==last_state & to=="X",1,P),
                                          P=ifelse(from==last_state & to!="X",0,P))
    
  }
}

# Everybody dies
model_frame <- model_frame %>% mutate(
  P=ifelse(time==max(time_steps) & to==absorbing[1],1,P),
  P=ifelse(time==max(time_steps) & to!=absorbing[1],0,P) 
)

# # Age effects
if(gen_age) {
  
  # Check all age-dependent states
  for(i in which_age) {
    
    # All receiving states
    for(j in state_space_red) {
      
      # Get starting probability
      first <- probs[[i]][j]
      
      # Check if any dependence for transition to state j, if no age-dependence move on to next j
      difference <- diff_age[[i]][j]
      if(difference==0) {
        next
      }
      
      # Interpolate between all values
      nsteps <- length(time_steps)-1
      
      # Linear interpolation
      if(str_detect(interpolation_age[[i]],"linear")) {
        values <- seq(0,difference,length.out=nsteps)
      }
      
      # Sigmoid interpolation
      if(str_detect(interpolation_age[[i]],"sigmoid")) {
        values <- seq(-15,15,
                      length.out=nsteps-1)
        values <- plogis(values)*difference
        values <- values
        values <- c(0,values) 
      }
      
      # Simple states
      model_frame$simple_from <- str_match(model_frame$from,i)
      model_frame$simple_to <- str_match(model_frame$to,j)
      
      # Loop over time
      lasttime <- max(time_steps)-1
      for(timeloop in 0:lasttime) {
        
        if(!j%in% which_duration) {
          i_from <- i
          j_to <- j
          model_frame <- model_frame %>% mutate(P=ifelse(simple_from%in%i_from & 
                                                           simple_to%in%j_to &
                                                           time==timeloop,P+values[timeloop+1],P))
        }
        
        if(j %in% which_duration & i!=j) {
          j_to <- paste0(j,1) 
          model_frame <- model_frame %>% mutate(P=ifelse(simple_from%in%i_from & 
                                                           to%in%j_to &
                                                           time==timeloop,P+values[timeloop+1],P))
        }
        
        if(j %in% which_duration & i==j) {
          
          for(duration in 1:(max(time_steps)-1)) {
            i_from <- paste0(i,duration)
            j_to <- paste0(j,duration+1)
            model_frame <- model_frame %>% mutate(P=ifelse(from%in%i_from & 
                                                             to%in%j_to &
                                                             time==timeloop,P+values[timeloop+1],P))
          }
        }
      }
      
      # Remove simple states
      model_frame <- model_frame %>% select(!c("simple_from","simple_to"))
    }
  }
  
}

# No values below 0 (can happen of you go wild with values)
if(any(model_frame$P<0)) {
  warning("Created transition probabilities below 0, readjusting")
  model_frame$P[model_frame$P<0] <- 0
}

# Rescaling 
model_frame <- model_frame %>% group_by(from,time) %>% mutate(P=P/sum(P)) %>% ungroup
class(model_frame) <- "data.frame"

# Rename values
if(gen_duration) changestates <- c(dependent_states,independent_states) else changestates <- transient
model_frame <- model_frame %>% mutate(
  from=ifelse(from%in%changestates,paste(from,time,sep="_"),from),
  to  =ifelse(to%in%changestates,paste(to,time+1,sep="_"),to)
)

# Return
#return(model_frame)
model_frame[model_frame$from=="A10_9",]


################################################################################

transient_states <- levels(interaction(which_duration,1:(max(time_steps)+1),sep=""))
transient_states <- c(transient_states,transient[!transient%in%which_duration])

################################################################################

transient = transient_states
absorbing = absorbing
time_steps = time_steps
data=model_frame
from="from"
to="to"
P="P"
enforcedeath=T
sep="_"
rescale=T


################################################################################

# Require tidy
require(tidyverse)

# Combine states and time
transient_states <- levels(interaction(transient,time_steps,sep=sep))
all_states <- c(transient_states,absorbing)

# Subset
data <- data %>% filter(from%in%all_states & to%in%all_states)

# Total number of transient and absorbing states
s_states <- length(transient_states)
a_states <- length(absorbing)
n_states <- length(all_states)


######################################
# Reshape 
Tm <- data %>% 
  select(from,to,P) %>% 
  pivot_wider(id_cols     = c(from),
              names_from  = to,
              values_from = P)

# Edit a bit
Tm[is.na(Tm)] <- 0
keepnames <- Tm$from
Tm <- Tm[,-1]

# Generate matrix
Tm <- as.matrix(Tm)
rownames(Tm) <- keepnames


#######################################################


# Add "missing" starting states, if any
addnames <- rownames(Tm)[!rownames(Tm)%in%colnames(Tm)]
nadd <- length(addnames)
if(nadd>0) {
  add <- matrix(data=0,ncol=nadd,nrow=dim(Tm)[1])
  colnames(add) <- addnames
  rownames(add) <- rownames(Tm)
  Tm <- cbind(Tm,add)
}

# Add potentially missing final states
addnames <- colnames(Tm)[!colnames(Tm)%in%rownames(Tm)]
nadd <- length(addnames)
if(nadd>0) {
  add <- matrix(data=0,nrow=nadd,ncol=dim(Tm)[2])
  rownames(add) <- addnames
  colnames(add) <- colnames(Tm)
  Tm <- rbind(Tm,add)
}

# Add death (the column should already be there)
Tm <- rbind(Tm,rep(0,n_states))
rownames(Tm)[(s_states+1):n_states] <- absorbing

# The dead stay dead (hopefully)
if(a_states==1) Tm[absorbing,absorbing] <- 1
if(a_states>1) diag(Tm[absorbing,absorbing]) <- 1

# Sort a little
Tm <- Tm[all_states,all_states]

# Numbers please
class(Tm) <- "numeric"

# Make sure everyone dies at the end
if(enforcedeath==T) {
  last_states <- paste(transient,max(time_steps),sep=sep)
  if(absorbing==1) {
    Tm[last_states,] <- 0
    Tm[last_states,absorbing] <- 1
  }
  if(absorbing>1) {
    Tm[last_states,] <- 0
    Tm[last_states,absorbing[1]] <- 1
  }
}

# Rescale
if(rescale) Tm <- t(apply(Tm,1,function(x) x/sum(x)))

# Return
#return(Tm)
