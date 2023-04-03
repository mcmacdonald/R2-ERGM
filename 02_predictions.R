




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
    # model specification
    estimate = 'MPLE',
    control = control.ergm(
      main.method = 'MCMLE', # see Snijders & van Duijn's (2002) paper on the 'Robbins-Monro' method
      MCMC.burnin = k,   # more is better
      MCMC.interval = k, # more is better
      MCMC.prop.weights = 'TNT', # faster convergence when paired with 'MPLE'
      seed = 20110210 # to replicate the results
    ),
    verbose = TRUE # adjustment for low density adjacency graphs
  )
  print(summary(b))  # print results
  print(confint(b, level = 0.95)) # 95% range
  return(b) # return ergm object
}
null_00 <- null()

# illustrate that the edges term is equivalent to the graph density -------------------------

# graph density of the observed graph
d_obs <- sna::gden(phones, mode = "digraph") # command to calculate graph density for directed graphs 
d_obs <- round(d_obs, digits = 9) # round
d_obs # print graph density

# graph density of the ERGM
d_hat <- null_00$coefficients[[1]] # edges term i.e., intercept
d_hat <- exp(d_hat)/(1 + exp(d_hat)) # calculate predicted probability
d_hat <- round(d_hat, digits = 9) # round
d_hat # print graph density

# test
message("Is the graph density equivalent to the graph density according to ERGM?")
if (d_hat == d_obs) { # if-then statements to report test-result
  message("Yes.")
  if (d_hat != d_obs) {
    message("No.")
  }
}



# ------------------------------------------------------------------------------
# this code calculates the isolate-adjusted density of the graph, plus other isolate-adjusted statistics used to calcualte r-squared
adjustment_isolate <- function(){ # ... this code provides isolate-adjusted estimates used to calculate r-squared measures
  n <- nrow(v)     # vertex count
  k <- n * 100000  # 100,000 iterations per vertex
  b <- ergm::ergm( # equation
    phone ~      # predict graph properties of phone calls
      edges    + # graph density i.e., equivalent to the intercept 
      isolates , # control on isolates i.e., degree = 0
    # model specification
    estimate = 'MPLE',
    control = control.ergm(
      main.method = 'MCMLE', # see Snijders & van Duijn's (2002) paper on the 'Robbins-Monro' method
      MCMC.burnin = k,   # more is better
      MCMC.interval = k, # more is better
      MCMC.prop.weights = 'TNT', # faster convergence when paired with 'MPLE'
      seed = 20110210 # to replicate the results
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
      edges                               + # graph density that estimates the number of arcs proportionate to all possible arcs (equivalent to intercept)
      isolates                            + # control on isolates i.e., degree = 0
      # the next set of terms in the equation represent the parameters to recreate the graph structure
      mutual                              + # control on the reciprocity of arcs i.e., if i phones j, does j phone i?
      gwidegree(decay = x, fixed = T)     + # control on in-degree distribution i.e., the sender of the phone call
      gwodegree(decay = x, fixed = T)     + # control on out-degree distribution i.e., the receiver of the phone call
      gwdsp(decay = 1.0, fixed = T)       + # control on the number of structurally equivalent pairs of conspirators 
      gwesp(decay = 1.0, fixed = T)       + # control on the cliquishness among the conspirators
      # predictors in the regression equation
      nodefactor('mafia', levels = - 1)   + # dummies that control on differences in mafia membership i.e., familia 
      nodeifactor('title', levels = - 1)  + # # the rank or title that suspects occupy in their mafia clan
      # nodeofactor('title', levels = - 1)+
      # nodematch('title', diff = TRUE)   + # title in mafia clan
      # nodemix('title', levels = -1)     +
      nodefactor('arrest', levels = -1)   + # spotlight bias on arrest, where reference = no arrest
      edgecov(summit)                     + # mafia summit meeting co-participation in one meeting
      edgecov(summit_m)                   + # mafia summit meeting co-participation in multiple meetings
      edgecov(mafias)                     + # co-membership in mafia-type organization
      edgecov(family)                     + # kinship relations
      edgecov(summit)   : edgecov(mafias) + # interaction effect summit co-pariciption x mafia membership
      edgecov(summit_m) : edgecov(mafias) , # interaction effect multiple summit co-participation x mafia membership 
    # model specification
    estimate = 'MPLE',
    control = control.ergm(
      main.method = 'MCMLE', # see Snijders & van Duijn's (2002) paper on 'Robbins-Monro' method
      MCMC.burnin = k,   # more is better
      MCMC.interval = k, # more is better
      MCMC.prop.weights = 'TNT', # faster convergence when paired with 'MPLE'
      seed = 20110210 # to replicate the results
      ),
    verbose = TRUE # adjustment for low density adjacency graphs
    )
  print(summary(b))  # print results
  print(confint(b, level = 0.95)) # 95% range
  return(b) # return ergm object
}
# this command sets the decay exponent of the degree distribution = 'x' ...
# ... where larger 'x' places more weight on the high degree suspects
ergm_01 <- update(x = 0.5)
ergm_02 <- update(x = 1.0)
ergm_03 <- update(x = 1.5)
ergm_04 <- update(x = 2.0)
ergm_05 <- update(x = 2.5)



# goodness-of-fit tests ---------------------------------------------------------
# compare the graph-theoretic properties by the geometric weight of the degree distribution
GOF = function(y){
  n <- nrow(v)  # n vertex
  k <- n * 1000 # 100,000 iterations per vertex
  GOF = ergm::gof( # GOF statistics 
    y ~ idegree + odegree + dspartners + espartners + distance, # graph-theoretic properties 
    verbose = TRUE, # adjustment to low density adjacency graph
    estimate = 'MPLE',
  control = ergm::control.gof.ergm(
  nsim = 10, # graph simulations
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
# don't run: 
# gof_02 = GOF(ergm_02)
# gof_03 = GOF(ergm_03)
# gof_04 = GOF(ergm_04)
# gof_05 = GOF(ergm_05)



# plot goodness-of-fit statistics ----------------------------------------------
GOF_plot <- function(gof){
  par(mfrow = c(3, 2)) # set plot dimensions
  plot(
    gof,
    plotlogodds = TRUE, # y-axis scale
    normalize.reacability = TRUE,
    main = "ERGM GOF diagnostics", # plot title
    cex.axis = 0.25
    )
}
GOF_plot(gof_01)
# don't run:
# GOF_plot(gof_02)
# OF_plot(gof_03)
# GOF_plot(gof_04)
# GOF_plot(gof_05)



# ------------------------------------------------------------------------------
# this code is used to adjust r-squared statistics
# ... it uses only endogenous graph properties to predict the graph-theoretic properties of mafia phone calls
adjustment_ergm <- function(){ # ... this code provides the adjustments used to calculate r-squared measures
  n <- nrow(v)     # vertex
  k <- n * 100000  # 100,000 iterations per vertex
  b <- ergm::ergm( # equation
    phone ~                               # predict contacts per phone taps
      edges                               + # graph density or intercept 
      isolates                            + # control on isolates i.e., degree = 0
      mutual                              + # control on the reciprocity of arcs i.e., if i phones j, does j phone i?
      gwidegree(decay = 1.0, fixed = T)     + # control on in-degree distribution i.e., the sender of the phone call
      gwodegree(decay = 1.0, fixed = T)     + # control on out-degree distribution i.e., the receiver of the phone call
      gwdsp(decay = 1.0, fixed = T)       + # control on the number of structurally equivalent pairs of conspirators 
      gwesp(decay = 1.0, fixed = T)       , # control on the cliquishness among the conspirators
    # modelling specification
    estimate = 'MPLE',
    control = control.ergm(
      main.method = 'MCMLE', # see Snijders & van Duijn's (2002) paper on 'Robbins-Monro' method
      MCMC.burnin = k,   # more is better
      MCMC.interval = k, # more is better
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



# logistic regression by quadratic assignment procedure
lrqap <- function(){ # ... this code uses attributes to predicts edges, but does not include non-linear endogenous graph properties
  n <- nrow(v)     # vertex
  k <- n * 100000  # 100,000 iterations per vertex
  b <- ergm::ergm( # equation
    phone ~                               # predict graph-properties of phone taps
      edges                             + # graph density or intercept
      isolates                          + # control on isolates i.e., degree = 0
      nodefactor('mafia', levels = - 1)   + # dummies that control on differences in mafia membership i.e., familia 
      nodeifactor('title', levels = - 1)  + # in dgeree 
      # nodeofactor('title', levels = - 1)+
      # nodematch('title', diff = TRUE)   + # the rank or title that suspects occupy in their mafia clan
      # nodemix('title', levels = -1)     +
      nodefactor('arrest', levels = -1)   + # spotlight bias on arrest, where reference = no arrest
      edgecov(summit)                     + # mafia summit meeting co-participation in one meeting
      edgecov(summit_m)                   + # mafia summit meeting co-participation in multiple meetings
      edgecov(mafias)                     + # co-membership in mafia-type organization
      edgecov(family)                     + # kinship relations
      edgecov(summit)   : edgecov(mafias) + # interaction effect summit co-pariciption x mafia membership
      edgecov(summit_m) : edgecov(mafias) , # interaction effect multiple summit co-participation x mafia membership 
    # model specification
    estimate = 'MPLE',
    control = control.ergm(
      main.method = 'MCMLE', # see Snijders & van Duijn's (2002) paper on 'Robbins-Monro' method
      MCMC.burnin = k,   # more is better
      MCMC.interval = k, # more is better
      MCMC.prop.weights = 'TNT', # faster convergence when paired with 'MPLE'
      seed = 20110210 # to replicate the results
    ),
    verbose = TRUE # adjustment for low density adjacency graphs
  )
  print(summary(b))  # print results
  print(confint(b, level = 0.95)) # 95% range
  return(b) # return ergm object
}
nogw_00 <- lrqap()





