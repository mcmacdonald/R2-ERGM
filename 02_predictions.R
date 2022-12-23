




# This .r script predicts graph-theoretic properties of the mafia phone calls ...

# contents: three functions calculate the null, updated, and adjusted statistics ...
# ... used to calculate numerous pseudo r-squared measures



# ------------------------------------------------------------------------------
# this code calculates the graph density, plus other null statistics used to calculate r-squared
null <- function(){ # ... this code provides null estimates used to calculate r-squared measures
  n <- nrow(v)     # vertex count
  k <- n * 100000  # 100,000 iterations per vertex
  b <- ergm::ergm( # equation
    phone ~  # predict graph properties of phone calls
      edges, # graph density i.e., equivalent to the intercept 
    # modelling specification
    estimate = 'MPLE',
    control = control.ergm(
      main.method = 'MCMLE', # see Snijders & van Duijn (2002) regarding 'Robbins-Monro' method
      MCMC.burnin = k,   # 'more is better' ... = v x 100,000
      MCMC.interval = k, # 'more is better' ... = v x 100,000
      MCMC.prop.weights = 'TNT', # faster convergence when paired with 'MPLE'
      seed = 20110210 # to replicate ERGM
    ),
    verbose = TRUE # adjustment for low density adjacency graphs
  )
  print(summary(b))  # print results
  print(confint(b, level = 0.95)) # 95% range
  return(b) # return ergm object
}
null_00 <- null()



# ------------------------------------------------------------------------------
# this code calculates the isolate-adjusted density of the graph, plus other isolate-adjusted statistics used to calcualte r-squared
adjustment_isolate <- function(){ # ... this code provides isolate-adjusted estimates used to calculate r-squared measures
  n <- nrow(v)     # vertex count
  k <- n * 100000  # 100,000 iterations per vertex
  b <- ergm::ergm( # equation
    phone ~      # predict graph properties of phone calls
      edges    + # graph density i.e., equivalent to the intercept 
      isolates , # control on isolates i.e., degree = 0
    # modelling specification
    estimate = 'MPLE',
    control = control.ergm(
      main.method = 'MCMLE', # see Snijders & van Duijn (2002) regarding 'Robbins-Monro' method
      MCMC.burnin = k,   # 'more is better' ... = v x 100,000
      MCMC.interval = k, # 'more is better' ... = v x 100,000
      MCMC.prop.weights = 'TNT', # faster convergence when paired with 'MPLE'
      seed = 20110210 # to replicate ERGM
      ),
    verbose = TRUE # adjustment for low density adjacency graphs
    )
  print(summary(b))  # print results
  print(confint(b, level = 0.95)) # 95% range
  return(b) # return ergm object
}
ergm_00 <- adjustment_isolate() # isolate-adjusted density of the graph



# ------------------------------------------------------------------------------
# this code predicts the graph-theoretic properties of mafia phone calls ...
# ... to do so, equation incorporates both predictors and endogenous graph properties
update <- function(x){ # ... this code provides the updated estimates used to calculate r-squared measures
  n <- nrow(v)     # vertex
  k <- n * 100000  # 100,000 iterations per vertex
  x <- x * 1       # degree decay parameter
  b <- ergm::ergm( # equation
      phone ~                             # predict graph-properties of phone taps
      edges                             + # graph density or intercept 
      isolates                          + # control on isolates i.e., degree = 0
      gwdegree(decay = x, fixed = T)    + # degree distribution
      gwdsp(decay = 1.0, fixed = T)     + # structural equivalence
      gwesp(decay = 1.0, fixed = T)     + # cliquishness
      nodefactor('mob', levels = -1)    + # dummies that control on Family differences in mafia membership 
      edgecov(meets) * edgecov(mobs)    , # interaction effect meetings x criminal organization membership ties
    # modelling specification
    estimate = 'MPLE',
    control = control.ergm(
      main.method = 'MCMLE', # see Snijders & van Duijn (2002) regarding 'Robbins-Monro' method
      MCMC.burnin = k,   # 'more is better' ... = v x 100,000
      MCMC.interval = k, # 'more is better' ... = v x 100,000
      MCMC.prop.weights = 'TNT', # faster convergence when paired with 'MPLE'
      seed = 20110210 # to replicate ERGM
      ),
    verbose = TRUE # adjustment for low density adjacency graphs
    )
  print(summary(b))  # print results
  print(confint(b, level = 0.95)) # 95% range
  return(b) # return ergm object
}
# this command weights the degree distribution according to 'x' ...
# ... larger x places more weight on vertices in the upper-tail of the degree distribution
ergm_01 <- update(x = 0.5)
ergm_02 <- update(x = 1.0)
ergm_03 <- update(x = 1.5)
ergm_04 <- update(x = 2.0)
ergm_05 <- update(x = 2.5)



# goodnes-of-fit tests ---------------------------------------------------------
# compare the graph-theoretic properties by the geometric weight of the degree distribution
GOF = function(y){
  n <- nrow(v)     # n vertex
  k <- n * 100000  # 100,000 iterations per vertex
  GOF = ergm::gof( # GOF statistics 
    y ~ degree + dspartners + espartners + distance, # graph-theoretic properties 
    verbose = TRUE, # adjustment to low density adjacency graph
    estimate = 'MPLE',
  control = ergm::control.gof.ergm(
  nsim = 100, # graph simulations
  MCMC.burnin = k,   # large k ensures initial conditions of the sim will be sufficiently different than the graph properties
  MCMC.interval = k, # large k ensures the sim had time to change from the initial start conditions
  MCMC.prop.weights = 'TNT',
  seed = 20110210 # set seed to replicate
  )
  )
  # return gof statistics
  return(GOF)
}
gof_01 = GOF(ergm_01)
gof_02 = GOF(ergm_02)
gof_03 = GOF(ergm_03)
gof_04 = GOF(ergm_04)
gof_05 = GOF(ergm_05)

# plot goodness-of-fit statistics
GOF_plot <- function(y){
  # par(mar=c(1,1,1,1))
  par( mfrow = c(2, 2) ) # set plot dimensions
  plot(
    y,
    plotlogodds = TRUE, # y-axis scale
    normalize.reacability = TRUE,
    main = "ERGM GOF diagnostics", # plot title
    cex.axis = 0.25
  )
}
GOF_plot(gof_01)
GOF_plot(gof_02)
GOF_plot(gof_03)
GOF_plot(gof_04)
GOF_plot(gof_05)



# ------------------------------------------------------------------------------
# this code is used to adjust r-squared statistics
# ... it uses only endogenous graph properties to predict the graph-theoretic properties of mafia phone calls
adjustment_ergm <- function(){ # ... this code provides the adjustments used to calculate r-squared measures
  n <- nrow(v)     # vertex
  k <- n * 100000  # 100,000 iterations per vertex
  b <- ergm::ergm( # equation
    phone ~                               # predict contacts per phone taps
      edges                             + # graph density or intercept 
      isolates                          + # control on isolates i.e., degree = 0
      gwdegree(decay = 1.0, fixed = T)  + # degree distribution
      gwdsp(decay = 1.0, fixed = T)     + # structural equivalence
      gwesp(decay = 1.0, fixed = T)     , # cliquishness
    # modelling specification
    estimate = 'MPLE',
    control = control.ergm(
      main.method = 'MCMLE', # see Snijders & van Duijn (2002) regarding 'Robbins-Monro' method
      MCMC.burnin = k,   # 'more is better' ... = v x 100,000
      MCMC.interval = k, # 'more is better' ... = v x 100,000
      MCMC.prop.weights = 'TNT', # faster convergence when paired with 'MPLE'
      seed = 20110210 # to replicate ERGM
      ),
    verbose = TRUE # adjustment for low density adjacency graphs
    )
  print(summary(b))  # print results
  print(confint(b, level = 0.95)) # 95% range
  return(b) # return ergm object
}
adj <- adjustment_ergm()



# close .r script --------------------------------------------------------------



# logistic regression by quadratic assignment procedure ------------------------
# ... this code uses attributes to predicts edges, but does not include non-linear endogenous graph properties
LRQAP <- function(){
  n <- nrow(v)     # vertex
  k <- n * 100000  # 100,000 iterations per vertex
  b <- ergm::ergm( # equation
    phone ~                               # predict graph-properties of phone taps
      edges                             + # graph density or intercept 
      isolates                          + # control on isolates i.e., degree = 0
      nodefactor('mob', levels = -1)    +
      #nodefactor('title', levels = -1) +     
      nodematch('title', diff = FALSE)  + # similarities in title i.e., associates connect to associates
      edgecov(meets) * edgecov(mobs)    , # interaction effect meetings x criminal organization membership ties
    # modelling specification
    estimate = 'MPLE',
    control = control.ergm(
      main.method = 'MCMLE', # see Snijders & van Duijn (2002) regarding 'Robbins-Monro' method
      MCMC.burnin = k,   # 'more is better' ... = v x 100,000
      MCMC.interval = k, # 'more is better' ... = v x 100,000
      MCMC.prop.weights = 'TNT', # faster convergence when paired with 'MPLE'
      seed = 20110210 # to replicate ERGM
    ),
    verbose = TRUE # adjustment for low density adjacency graphs
  )
  print(summary(b))  # print results
  print(confint(b, level = 0.95)) # 95% range
  return(b) # return ergm object
}
r_adj <- LRQAP()



