

# this script predicts the structure of graphs where dyads have weighted arcs ...


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
w_phones %v% 'mafia'       <- montagna$familia
w_phones %v% 'title'     <- montagna$title
w_phones %v% 'padrino'   <- montagna$padrino
w_phones %v% 'arrest'    <- montagna$arrest
w_phones %v% 'informant' <- montagna$informant



# see the distribution of arc weights in the graph to determine the type of distribution used


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
      MCMC.prop.weights = 'TNT', # other options include '0inflated' or 'Poisson'
      seed = 20110210 # to replicate the results
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
    sum                                 + # control on the sum of edges or the edge weights
    edges                               + # graph density i.e., equivalent to the intercept 
    # endogenous terms to estimate the structure of the graph-theoretic properties 
    mutual                              + # reciprocity of arcs
    gwidegree(decay = 1.0, fixed = T)   +
    gwodegree(decay = 1.0, fixed = T)   +
    gwdsp(decay = 1.0, fixed = T)       + # structural equivalence
    gwesp(decay = 1.0, fixed = T)       +
    nodefactor('mafia', levels = -1)    + # dummies that control on unobserved differences in mafia membership 
    nodeifactor('title', levels = -1)   + # dummies that control on receiver effects that relate to rank or title in the mafia
    # nodematch('title', diff = TRUE)     +
    nodefactor('arrest', levels = -1)   +
    edgecov(summit)                     +
    edgecov(summit_m)                   +
    edgecov(mafias)                     +
    edgecov(family)                     +
    edgecov(summit)   : edgecov(mafias) +
    edgecov(summit_m) : edgecov(mafias) , 
    # model specification
    response = "weight",
    reference = ~Poisson,
    verbose = TRUE, # adjustment for low density adjacency graphs
    control = control.ergm(
      MCMC.prop.weights = 'TNT', # other options include '0inflated' or 'Poisson'
      seed = 20110210 # to replicate the results
      ),
    )
  print(summary(b))  # print results
  print(confint(b, level = 0.95)) # 95% range
  return(b) # return ergm object
}
count_01 <- count_update()



