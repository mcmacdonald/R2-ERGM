


# bayesian priors --------------------------------------------------------------

# Function to compute variance-covariance structure/matrix
sigma <- function(mu){ # Function to compute variance/covariance matrix
  n <- length(unlist(mu))
  s <- matrix(0:0, nrow = n, ncol = n) # where 'input' = number ERGM parameters 
  diag(s) <- 1 # set diagonals = 1
  return(s) # return matrix
}
# ... create vector of the multivariate Normal priors 
mu_02 <- ergm_02$coefficients # keep parameters
sd_02 <- sigma(mu = mu_02)
diag(sd_02) <- coef(summary(ergm_02))[, "Std. Error"]



# bayesian exponential random graphs -------------------------------------------
# posterior parameter estimation:
set.seed(20110211) # set seed to replicate
bayes <- function(Fx, p, s){
  i <- 154 * 2  # burn in iterations to begin the MCMC run 
  k <-      50 # sample iterations
  n <-    1000 # per Caimo & Friel (2011), auxiliary chain = # chains (h) * sample iterations (k)
  h <-      20 # chains in the posterior distribution ... approximately 2x the ERGM parameters 
  # load 'bergm' package
  require('Bergm')
  bayes <- Bergm::bergm(
    phone ~                             # predict graph-properties of phone taps
      edges                             + # graph density or intercept 
      isolates                          + # control on isolates i.e., degree = 0
      gwdegree(decay = 1.0, fixed = T)  + # degree distribution
      gwdsp(decay = 1.0, fixed = T)     + # structural equivalence
      gwesp(decay = 1.0, fixed = T)     + # cliquishness
      nodefactor('mob', levels = -1)    + # dummies that control on Family differences in mafia membership 
      edgecov(meets) * edgecov(mobs)    , # interaction effect meetings x criminal organization membership ties
    burn.in = i,     # burn ins
    main.iters = k,  # sample iterations
    aux.iters = n,   # auxiliary chains 
    nchains = h,     # essentially the # chains in the posterior distribution
    prior.mean = p,  # prior means
    prior.sigma = s, # prior variance/covariance structure
    gamma = 0.1      # empirically, gamma ranges 0.1 - 1.5, where smaller gamma leads to better acceptance in big graphs
    )
  print(summary(bayes)) # print results
  return(bayes)
}
bergm_02 <- bayes(p = mu_02, s = sd_02)



# goodness-of-fit statistics ---------------------------------------------------
set.seed(20110211) # set seed to replicate
GOF <- function(bayes){
  n <- 1000 # graph simulations
  i <- 25   # degree distribution range
  j <- 10   # geodisstance range 
  k <- 15   # edgewise-shared partners range
  require('Bergm')
  gof <- Bergm::bgof(
    bayes,
    directed = FALSE, # symmetric graph
    sample.size = n,  # random graph realizations
    n.deg  = i,
    n.dist = j,
    n.esp  = k
    )
  return(gof)
}
gof_bergm_02 <- GOF(bergm_02)



# close .r script


