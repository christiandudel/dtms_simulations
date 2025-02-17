# Bias in Markovian multistate models under violation of the Markov assumption

Christian Dudel, dudel@demogr.mpg.de

This repository contains R code for simulations assessing the bias of several statistics 
estimated using Markovian discrete-time multistate models when the Markov assumption 
is violated. This includes simulation scenarios with duration dependence,
unobserved heterogeneity, and combinations of both duration dependence and
unobserved heterogeneity. Running the code requires, among other things,
the package dtms, which is available at https://github.com/christiandudel/dtms.

There are four relevant folders: Functions, Setup, Simulations, Results. The folder 
Functions contains one file which collects several helper-functions used
for the simulations. When replicating the simulations, there is no need to
interact with this folder. In the folder Setup are files describing the 
simulation scenarios. These descriptions follow a certain template which
is used by some of the helper-functions. The folder Simulations contains files
which run the actual simulations, including the calculation of reference values.
Finally, the folder Results contains all results and an R file which creates
an overview of the findings. 
