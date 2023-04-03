

# this script estimates the count dyadic regression

# import edges -----------------------------------------------------------------
w_phones <- utils::read.csv("https://raw.githubusercontent.com/mcmacdonald/R2-ERGM/main/montagna_phone.csv")    # phone calls by wire taps
w_summit <- utils::read.csv("https://raw.githubusercontent.com/mcmacdonald/R2-ERGM/main/montagna_meetings.csv") # mafia summits or meeting co-attendance

# graph objects ----------------------------------------------------------------
weighted <- function(g){
  g <- network::as.network.data.frame(g, directed = TRUE, v = v, loops = FALSE, multiple = TRUE)
  return(g)
}
w_phones <- weighted(w_phones)  
w_summit <- weighted(w_summit)

# load attributes onto phone-tap graph
require(statnet)
w_phones %v% 'mob'       <- montagna$familia
w_phones %v% 'title'     <- montagna$title
w_phones %v% 'padrino'   <- montagna$padrino
w_phones %v% 'arrest'    <- montagna$arrest
w_phones %v% 'informant' <- montagna$informant



# ------------------------------------------------------------------------------
# this code calculates the graph density, plus other null statistics used to calculate r-squared
count_null <- function(){ # ... this code provides null estimates used to calculate r-squared measures
  b <- ergm::ergm( # equation
    w_phones ~  # predict graph properties of phone calls
      sum    +  #
      edges  ,  # graph density i.e., equivalent to the intercept 
    # modelling specification
    response = "weight",
    reference = ~Poisson,
    verbose = TRUE, # adjustment for low density adjacency graphs
    control = control.ergm(
      # main.method = 'MCMLE', # 'Robbins-Monro' method, see Snijders & van Duijn (2002)
      # MCMC.burnin = k,   # 'more is better' ... = v x 100,000
      #  MCMC.interval = k, # 'more is better' ... = v x 100,000
      MCMC.prop.weights = 'TNT', # '0inflated', 'Poisson'
      seed = 20110210 # to replicate ERGM
    ),
    )
  print(summary(b))  # print results
  print(confint(b, level = 0.95)) # 95% range
  return(b) # return ergm object
}
count_00 <- count_null()



# this code calculates the graph density, plus other null statistics used to calculate r-squared
count_update <- function(){ # ... this code provides null estimates used to calculate r-squared measures
  set.seed(20220915)
  b <- ergm::ergm( # equation
    w_phones ~  # predict graph properties of phone calls
    sum                               + # control
    edges                             + # graph density i.e., equivalent to the intercept 
    mutual                            +
    gwidegree(decay = 0.5, fixed = T) +
    gwodegree(decay = 0.5, fixed = T) +
    gwdsp(decay = 0.5, fixed = T)     + # structural equivalence
    gwesp(decay = 0.5, fixed = T)     +
    nodefactor('mob', levels = -1)    + # dummies that control on Family differences in mafia membership 
    nodematch('title', diff = TRUE)   +
    nodefactor('arrest', levels = -1) +
    edgecov(summit)                   +
    edgecov(summit_m)                 +
    edgecov(nostra)                   +
    edgecov(family)                   , 
    # modelling specification
    response = "weight",
    reference = ~Poisson,
    verbose = TRUE, # adjustment for low density adjacency graphs
    control = control.ergm(
      # main.method = 'MCMLE', # 'Robbins-Monro' method, see Snijders & van Duijn (2002)
      # MCMC.burnin = k,   # 'more is better' ... = v x 100,000
      #  MCMC.interval = k, # 'more is better' ... = v x 100,000
      MCMC.prop.weights = 'TNT', # faster convergence when paired with 'MPLE'
      seed = 20110210 # to replicate ERGM
    ),
    )
  print(summary(b))  # print results
  print(confint(b, level = 0.95)) # 95% range
  return(b) # return ergm object
}
count_01 <- count_update()



