# Seed
set.seed(2610)

# Size of reference population
simsize <- 1000000

# Replications for simulations
replications <- 1000

# Control for time
timecontrol <- "s(time)"

# Probability parameters of simulations
death <- 0.02
even2 <- (1-death)/2
even3 <- (1-death)/3

sticky1 <- 0.4
sticky2 <- 0.5
sticky3 <- 0.6

diff1 <- 0.1
diff2 <- 0.2
diff3 <- 0.3
