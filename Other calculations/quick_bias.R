# Function
bias <- function(alpha,gamma,logh,eta) {

  # Intermediates
  rho <- exp(alpha+gamma)
  W <- (1+rho)/(1+rho*exp(eta))
  
  # Odds
  true <- (rho+exp(alpha+gamma+eta)*logh)/(1+logh)
  bias <- (rho+exp(alpha+gamma+eta)*logh*W)/(1+logh*W)
  
  # Predicted probabilities
  pbias <- bias/(1+bias)
  ptrue <- true/(1+true)
  
  # Absolute bias
  rbias <- abs(pbias-ptrue)
  
  # Return
  return(rbias)

}

# Examples
eta <- seq(-3,3,by=0.01)

# Alpha
bias_am15 <- bias(alpha=-1.5,gamma=0,logh=1,eta=eta)
bias_am10 <- bias(alpha=-1.0,gamma=0,logh=1,eta=eta)
bias_am05 <- bias(alpha=-0.5,gamma=0,logh=1,eta=eta)
bias_a00 <- bias(alpha=0.0,gamma=0,logh=1,eta=eta)
bias_a05 <- bias(alpha=0.5,gamma=0,logh=1,eta=eta)
bias_a10 <- bias(alpha=1.0,gamma=0,logh=1,eta=eta)
bias_a15 <- bias(alpha=1.5,gamma=0,logh=1,eta=eta)

# Gamma
bias_mg15 <- bias(alpha=0,gamma=-1.50,logh=1,eta=eta)
bias_mg10 <- bias(alpha=0,gamma=-1.0,logh=1,eta=eta)
bias_mg05 <- bias(alpha=0,gamma=-0.5,logh=1,eta=eta)
bias_g00 <- bias(alpha=0,gamma=0.0,logh=1,eta=eta)
bias_g05 <- bias(alpha=0,gamma=0.5,logh=1,eta=eta)
bias_g10 <- bias(alpha=0,gamma=1.0,logh=1,eta=eta)
bias_g15 <- bias(alpha=0,gamma=1.5,logh=1,eta=eta)

# Logh
bias_l05 <- bias(alpha=0,gamma=0.0,logh=0.5,eta=eta)
bias_l10 <- bias(alpha=0,gamma=0.0,logh=1.0,eta=eta)
bias_l15 <- bias(alpha=0,gamma=0.0,logh=1.5,eta=eta)


plot(eta,bias_am15,type="l",ylim=c(0,0.3),ylab="Absolute difference Biased - True",panel.first={grid();abline(h=0.025,col="grey50")})
lines(eta,bias_am10)
lines(eta,bias_am05)
lines(eta,bias_a00)
lines(eta,bias_a05)
lines(eta,bias_a10)
lines(eta,bias_a15)

plot(eta,bias_mg15,type="l",ylim=c(0,0.3),ylab="Absolute difference Biased - True",panel.first={grid();abline(h=0.025,col="grey50")})
lines(eta,bias_mg10)
lines(eta,bias_mg05)
lines(eta,bias_g00)
lines(eta,bias_g05)
lines(eta,bias_g10)
lines(eta,bias_g15)

plot(eta,bias_l15,type="l",ylim=c(0,0.3),ylab="Absolute difference Biased - True",panel.first={grid();abline(h=0.025,col="grey50")})
lines(eta,bias_l10)
lines(eta,bias_l05)


