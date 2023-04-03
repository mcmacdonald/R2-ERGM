


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



# edits needed to code




# bayesian priors --------------------------------------------------------------

# Function to compute variance-covariance structure/matrix
sigma <- function(mu){ # Function to compute variance/covariance matrix
  n <- length(unlist(mu))
  s <- matrix(0:0, nrow = n, ncol = n) # where 'input' = number ERGM parameters 
  diag(s) <- 1 # set diagonals = 1
  return(s) # return matrix
}

# ------------------------------------------------------------------------------
# ... create vector of the multivariate Normal priors 
mu_null <- null_00$coefficients # keep parameters
sd_null <- sigma(mu = null_00)
diag(null_00) <- coef(summary(null_00))[, "Std. Error"]

# equation ...
Fx_null <- phone ~                   # predict graph-properties of phone taps
  edges                              # graph density or intercept 


# ------------------------------------------------------------------------------
# ... create vector of the multivariate Normal priors 
mu_00 <- ergm_00$coefficients # keep parameters
sd_00 <- sigma(mu = mu_00)
diag(sd_00) <- coef(summary(ergm_00))[, "Std. Error"]

# equation ...
Fx_00 <- phone ~                      # predict graph-properties of phone taps
  edges                             + # graph density or intercept 
  isolates                            # control on isolates i.e., degree = 0


# ------------------------------------------------------------------------------
# ... create vector of the multivariate Normal priors 
mu_02 <- ergm_02$coefficients # keep parameters
sd_02 <- sigma(mu = mu_02)
diag(sd_02) <- coef(summary(ergm_02))[, "Std. Error"]

# equation ...
Fx_02 <-  phones ~                    # predict graph-properties of phone taps
  edges                             + # graph density or intercept 
  isolates                          + # control on isolates i.e., degree = 0
  gwdegree(decay = 1.0, fixed = T)    + # degree distribution
  gwdsp(decay = 1.0, fixed = T)     + # structural equivalence
  gwesp(decay = 1.0, fixed = T)     + # cliquishness
  nodefactor('mob', levels = -1)    + # dummies that control on Family differences in mafia membership 
  nodematch('title', diff = TRUE)   + # title in mafia clan
  nodefactor('arrest', levels = -1) + # spotlight bias on arrest
  edgecov(summit)                   + # mafia summit meeting co-participation
  edgecov(summit_m)                 + # mafia summit meeting co-participation > 1
  edgecov(nostra)                   + #
  edgecov(family)                   +
  edgecov(summit)   : edgecov(nostra) + # interaction effect summit x criminal organization membership
  edgecov(summit_m) : edgecov(nostra)   # interaction effect multiple summits x criminal organization membership 


# ------------------------------------------------------------------------------
# ... create vector of the multivariate Normal priors 
mu_adj <- adj$coefficients # keep parameters
sd_adj <- sigma(mu = mu_adj)
diag(sd_adj) <- coef(summary(adj))[, "Std. Error"]

# equation ...
Fx_adj <- phone ~                     # predict graph-properties of phone taps
  edges                             + # graph density or intercept 
  isolates                          + # control on isolates i.e., degree = 0
  gwdegree(decay = 1.0, fixed = T)  + # degree distribution
  gwdsp(decay = 1.0, fixed = T)     + # structural equivalence
  gwesp(decay = 1.0, fixed = T)       # cliquishness





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
    Fx,
    burn.in = i,     # burn ins
    main.iters = k,  # sample iterations
    aux.iters = n,   # auxiliary chains 
    nchains = h,     # essentially the # chains in the posterior distribution
   # prior.mean = p,  # prior means
   # prior.sigma = s, # prior variance/covariance structure
    gamma = 0.1      # empirically, gamma ranges 0.1 - 1.5, where smaller gamma leads to better acceptance in big graphs
    )
  print(summary(bayes)) # print results
  return(bayes)
}
bergm_02 <- bayes(Fx = Fx_02)
bergm_00   <- bayes(Fx = Fx_00, p = mu_00, s = sd_00)
bergm_02   <- bayes(Fx = Fx_02, p = mu_02, s = sd_02)
bergm_adj  <- bayes(Fx = Fx_adj,p = mu_adj,s = sd_adj)
bergm_null <- bayes(Fx = Fx_null, p = mu_null, s = sd_null)


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



# manually calculate predicted probabilities  ----------------------------------
predict.bergm <- function(y){
  require("dplyr"); require("magrittr")
  theta <- y$Theta; theta <- data.frame(theta) # thetas of the predictors
  colnames(theta) <- c("edges", 
                       "isolates", 
                       "gwdegree", 
                       "gwdsp", 
                       "gwesp",
                       "nf_barcellona",
                       "nf_batanesi",
                       "nf_brancaccio",
                       "nf_caltagirone",
                       "nf_mazzaroti",
                       "nf_mistretta",
                       "nf_castleverde",
                       "nf_tortorici",
                       "nf_sconosciuto",
                       "nm_associate",
                       "nm_solider",
                       "nm_capo",
                       "nf_arrest",
                       "summit",
                       "summit_m",
                       "nostra",
                       "family",
                       "summit:nostra",
                       "summit_m:nostra"
                       )
  # theta <- dplyr::select(theta, -isolates) # drop isolates term
  # calculate the average 
  `%>%` <- magrittr::`%>%`
  
  # dyads
  # y_1   <- theta %>% dplyr::select(-edges) %>% 
  y_1   <- theta dplyr::mutate(theta, pp = base::rowSums(theta)) # sum the thetas
  y_1   <- dplyr::mutate(y_1, pp = exp(pp)/(1 + exp(pp)))
  y_1   <- dplyr::select(y_1, pp)
  y_1   <- exp(mean(log(y_1$pp), na.rm = TRUE))
  # no dyads
  y_0   <- theta %>% dplyr::mutate(theta, pp = base::rowSums(theta)) # sum the thetas
  y_0   <- dplyr::mutate(y_0, pp = exp(pp)/(1 + exp(pp)))
  y_0   <- dplyr::select(y_0, pp)
  y_0   <- exp(mean(log(y_0$pp), na.rm = TRUE))
  r.sq  <- abs(y_0 - y_1)
  print(r.sq)
}
predict.bergm(bergm_02)




# close .r script


