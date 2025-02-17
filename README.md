# Bias in estimands of Markovian multistate models under violation of the Markov assumption

Christian Dudel, dudel@demogr.mpg.de

## Overview

This repository contains R code for simulations assessing the bias of several statistics 
estimated using Markovian discrete-time multistate models when the Markov assumption 
is violated. This includes simulation scenarios with duration dependence,
unobserved heterogeneity, and combinations of both duration dependence and
unobserved heterogeneity. Running the code requires, among other things,
the package dtms, which is available at https://github.com/christiandudel/dtms.

## Folders

There are four folders: Functions, Setup, Simulations, Results. The folder 
Functions contains one file which collects several helper-functions used
for the simulations. When replicating the simulations, there is no need to
interact with this folder. In the folder Setup are files describing the 
simulation scenarios. These descriptions follow a certain template which
is used by some of the helper-functions. The folder Simulations contains files
which run the actual simulations, including the calculation of reference values.
Finally, the folder Results contains all results and an R file which creates
an overview of the findings. 

## Workflow

The workflow currently has four major steps: (1) setting up the simulation scenarios;
(2) calculating reference values; (3) running the simulations; and (4) 
calculating bias by comparing the results of steps (2) and (3).

## Setting up simulation scenarios

The simulations are set up as named lists with the following set of entries:
- transient: names of the transient states, specified as a character vector. 
- absorbing: names of the absorbing states, specified as a character vector. 
- time_steps: values of the time scale, specified as a numeric vector.
- probs: named list with transition probabilities. The names of the list 
  correspond to names of transient states; e.g., if there is a transient state
  called "A", then there should be a correspondingly named entry. The entries
  each contain a numeric vector of transition probabilities with named elements, 
  where the names of the elements should cover all states; e.g., if there are
  two transient states "A" and "B" and one absorbing state "X", then a numeric
  vector could be specified as c(A=0.45,B=0.45,C=0.1).
- gen_duration: logical value indicating if the model is duration dependent. If
  FALSE all other arguments relating to duration dependence are ignored.
- gen_age: logical value indicating if the model is age dependent, meaning that
  the transition probabilities change with progress of the time scale (i.e., if
  they are non-homogenous). If FALSE all other parameters relating to age 
  dependence are ignored.
- which_duration: if the model is duration dependent, this is a character vector
  specifying the names of the transient states which are duration dependent.
- diff_duration: named list of maximum difference in transition probabilities; e.g.,
  if the state "A" is duration dependent, this could be list(A=c(A=0,B=0.1,X=-0.1)).
  This means that the probability of moving from state A to state A does not change 
  with duration; the probability of moving from A to B increases by 0.1; and
  the probability of transitioning to X decreases by -0.1.
- interpolation_duration: named list, each entry corresponding to a duration
  dependent state and specifying how the change(s) in transition probabilities
  specified with diff_duration are used. Current options are "linear", "sigmoid",
  "switchX", and "random". "linear" linearly interpolates from the value specified
  with probs to the value reached by applying diff_duration, starting with the initial
  value at the first value of the time scale, and using the final value for 
  transitions at the end of the time scale. "sigmoid" works in a similar fashion, but 
  uses a sigmoid function for interpolation. "switchX", where "X" has to be replaced
  with an integer, switches from the value from probs to the changed value after "X" 
  units of duration in the state. "random" randomly chooses between the original and
  the changed value for all values of the time scale. 
- which_age: similar to which_duration, except for age dependence.
- diff_age: similar to diff_age, except for age dependence.
- interpolation_age: see interpolation_duration above.

Unobserved heterogeneity is achieved by not having one, but several DGPs, each with
the arguments used above. This also allows to combine duration dependence and unobserved
heterogeneity. 