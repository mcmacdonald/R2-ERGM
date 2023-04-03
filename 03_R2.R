


# syntax to calculate pseduo R-squared measures --------------------------------

# ... this .r script calculates three pseduo r-squared statistics:
# ... 1) Efron's r-squared
# ... 2) McFadden's r-squared
# ... 3) Tjur's r-squared

# the choice of the three r-squared measures is strategic:
# ... 1) Efron's r-squared is most similar to R-squared in ordinary least squares; and 
# ... 2) Paul Allison demonstrates McFadden's and Tjur's r-squared statictics > other pseduo r-squared measures;*
# ... 3) the three measures sufficiently differ in their procedures to calculate r-squared

# ... * see Paul Allison's comparison of pseudo R-squared measures: https://statisticalhorizons.com/r2logistic/



# Efron's R-squared ------------------------------------------------------------
Efron <- function(g, null, update, adjust){
  # adjacency graph to pairwise dyads
  ij <- igraph::as_edgelist(intergraph::asIgraph(g), names = TRUE) # pairwise dyads
  ij <- data.frame(ij); ij <- dplyr::mutate(ij, n = 1) # = 1 to denote police-reported dyads
  colnames(ij) <- c("i", "j", "n") # rename columns
  
  # predicted probabilities
  pp_update <- stats::predict(update)     # predicted probabilities for all possible dyads
  colnames(pp_update) <- c("i", "j", "p") # rename columns
  
  # adjustment to the predicted probabilities
  pp_adjust <- stats::predict(adjust)     # predicted probabilities for all possible dyads
  colnames(pp_adjust) <- c("i", "j", "p") # rename columns
  
  # proof that predict() calculates predicted probabilities for all possible dyads
  # don't run 
  # n <- 154*(154 - 1)/2 # total number of possible dyads
  # cat("\n ... does the number of possible dyads "); cat(nrow(pp_update) == n); cat("\n)
  # cat(); cat(nrow(pp_adjust) == n); cat("\n)
  
  # isolate-adjusted graph density used in r-squared calculation
  b <- coefficients(null)[1] # edge parameter of the isolate-adjust graph
  mu <- exp(b)/(1+exp(b))    # predicted probability of the edge parameter = graph density
  
  # join the pairwise dyads to the predicted probabilities
  pp_update <- merge(x = pp_update, y = ij, by.x = c("i", "j"), by.y = c("i", "j"), all.x = TRUE, all.y = TRUE)
  pp_update[is.na(pp_update)] <- 0 # set non-dyads = 0
  
  # calculate r-squared
  n    <- sum((pp_update$n - pp_update$p)^2) # numerator
  d    <- sum((pp_update$n - mu)^2)          # denominator
  r.sq <- 1 - (n/d) # r-squared
  # print Tjur's r-squared
  cat("\n Efron's R-squared: "); cat(r.sq); cat("\n")
  
  # join the pairwise dyads to the adjustment
  pp_adjust <- merge(x = pp_adjust, y = ij, by.x = c("i", "j"), by.y = c("i", "j"), all.x = TRUE, all.y = TRUE)
  pp_adjust[is.na(pp_adjust)] <- 0 # set non-dyads = 0
  # calculate r-squared adjustment
  n    <- sum((pp_adjust$n - pp_adjust$p)^2) # numerator
  d    <- sum((pp_adjust$n - mu)^2)          # denominator
  r.dx <- 1 - (n/d)
  r.dx <- r.sq - r.dx # "adjusted" r.squared
  # print adjustment
  cat("\n Efron's R-squared, adjustment: "); cat(r.dx); cat("\n")
  cat("\n ... the adjustment is the proportion of error reduction explained by the predictors, independent of the endogenous graph properties.")
}
Efron(g = phones, null = ergm_00, update = ergm_01, adjust = adj)



# McFadden's R-squared ---------------------------------------------------------
McFadden <- function(null, update, adjust){
  # inputs:
  # ... null = null log likelihood statistic (isolate-adjusted)
  # ... update = updated log likelihood statistic
  # ... adjust = adjustment to pseudo r-squared
  
  # McFadden's r-squared
  k <- length(stats::coefficients(update)) # adjust by degrees of freedom
  r.sq <- 1 - ((stats::logLik(update) - k)/ergm::logLikNull(update))
  cat("\n McFadden's R-squared: "); cat(r.sq); cat("\n")
  
  # McFadden's r-squared
  k <- length(stats::coefficients(update)) # adjust by degrees of freedom
  r.sq <- 1 - ((stats::logLik(update) - k)/stats::logLik(null))
  cat("\n McFadden's R-squared, isolate-adjusted:"); cat(r.sq); cat("\n")
  
  # r.squared adjustment to discount the proportion of error reduction explained by the endogenous graph properties
  k <- length(stats::coefficients(adjust)) # adjust by degrees of freedom
  r.dx <- 1 - ((stats::logLik(adjust) - k)/stats::logLik(null))
  r.dx <- r.sq - r.dx
  cat("\n McFadden's R-squared, adjustment: "); cat(r.dx); cat("\n")
  cat("\n ... the adjustment is the proportion of error reduction explained by the predictors, independent of the endogenous graph properties.")
}
McFadden(null = ergm_00, update = ergm_01, adjust = adj)



# Tjur's R-squared -------------------------------------------------------------
Tjur <- function(g, update, adjust){
  # adjacency graph to pairwise dyads
  ij <- igraph::as_edgelist(intergraph::asIgraph(g), names = TRUE) # pairwise dyads
  ij <- data.frame(ij); ij <- dplyr::mutate(ij, n = 1) # = 1 to denote police-reported dyads
  colnames(ij) <- c("i", "j", "n") # rename columns
  
  # predicted probabilities
  pp_update <- stats::predict(update)     # predicted probabilities for all possible dyads
  colnames(pp_update) <- c("i", "j", "p") # rename columns
  
  # adjustment to the predicted probabilities
  pp_adjust <- stats::predict(adjust)     # predicted probabilities for all possible dyads
  colnames(pp_adjust) <- c("i", "j", "p") # rename columns
  
  # proof that predict() calculates predicted probabilities for all possible dyads
  # don't run 
  # n <- 154*(154 - 1)/2 # total number of possible dyads
  # cat("\n ... does the number of possible dyads "); cat(nrow(pp_update) == n); cat("\n)
  # cat(); cat(nrow(pp_adjust) == n); cat("\n)
  
  # join the pairwise dyads to the predicted probabilities
  pp_update <- merge(x = pp_update, y = ij, by.x = c("i", "j"), by.y = c("i", "j"), all.x = TRUE, all.y = TRUE)
  pp_update[is.na(pp_update)] <- 0 # set non-dyads = 0
  # calculate r-squared
  y_1  <- exp(mean(log(pp_update$p[pp_update$n == 1]))) # geometric means
  y_0  <- exp(mean(log(pp_update$p[pp_update$n == 0]))) # geometric means
  r.sq <- abs(y_0 - y_1)
  # print Tjur's r-squared
  cat("\n Tjur's R-squared: "); cat(r.sq); cat("\n")
  
  # join the pairwise dyads to the adjustment
  pp_adjust <- merge(x = pp_adjust, y = ij, by.x = c("i", "j"), by.y = c("i", "j"), all.x = TRUE, all.y = TRUE)
  pp_adjust[is.na(pp_adjust)] <- 0 # set non-dyads = 0
  # calculate r-squared adjustment
  y_1  <- exp(mean(log(pp_adjust$p[pp_adjust$n == 1]))) # geometric means
  y_0  <- exp(mean(log(pp_adjust$p[pp_adjust$n == 0]))) # geometric means
  r.dx <- abs(y_0 - y_1)
  r.dx <- r.sq - r.dx # "adjusted" r.squared
  # print adjustment
  cat("\n Tjur's R-squared, adjustment: "); cat(r.dx); cat("\n")
  cat("\n ... the adjustment is the proportion of error reduction explained by the predictors, independent of the endogenous graph properties.")
}
Tjur(g = phones, update = ergm_01, adjust = adj)



# Snidjer & Bosker's R-squared -------------------------------------------------
# called the "total variance" method, used to calculate r-squared in multi-level regression
# I adjust the measure to calculate total variance, irrespective of clusters or level-1 or level-2 predictors
variance <- function(g, null, update, adjust){ # "total variance"
  # adjacency graph to pairwise dyads
  ij <- igraph::as_edgelist(intergraph::asIgraph(g), names = TRUE) # pairwise dyads
  ij <- data.frame(ij); ij <- dplyr::mutate(ij, n = 1) # = 1 to denote police-reported dyads
  colnames(ij) <- c("i", "j", "n") # rename columns
  
  # the next three code sequences calculate the predicted probabilities by regression specification ...
  # ... and merges the complete pairwise dyads i,j = 1 or i,j = 0 onto the predicted probabilities ...
  # ... to then calculate residuals
  
  # ----------------------------------------------------------------------------
  
  # predicted probabilities of the null
  pp_null <- stats::predict(null)         # predicted probabilities for all possible dyads
  colnames(pp_null) <- c("i", "j", "p")   # rename columns
  
  # join the pairwise dyads to the predicted probabilities of the null
  pp_null <- merge(x = pp_null, y = ij, by.x = c("i", "j"), by.y = c("i", "j"), all.x = TRUE, all.y = TRUE)
  pp_null[is.na(pp_null)] <- 0 # set non-dyads = 0
  
  # ----------------------------------------------------------------------------
  
  # predicted probabilities of the update i.e., regression equation that incorporates predictors
  pp_update <- stats::predict(update)     # predicted probabilities for all possible dyads
  colnames(pp_update) <- c("i", "j", "p") # rename columns
  
  # join the pairwise dyads to the predicted probabilities of the update
  pp_update <- merge(x = pp_update, y = ij, by.x = c("i", "j"), by.y = c("i", "j"), all.x = TRUE, all.y = TRUE)
  pp_update[is.na(pp_update)] <- 0 # set non-dyads = 0
  
  # ----------------------------------------------------------------------------
  
  # adjustment to the predicted probabilities
  pp_adjust <- stats::predict(adjust)     # predicted probabilities for all possible dyads
  colnames(pp_adjust) <- c("i", "j", "p") # rename columns
  
  # join the pairwise dyads to the predicted probabilities of the adjustment
  pp_adjust <- merge(x = pp_adjust, y = ij, by.x = c("i", "j"), by.y = c("i", "j"), all.x = TRUE, all.y = TRUE)
  pp_adjust[is.na(pp_adjust)] <- 0 # set non-dyads = 0
  
  # proof that predict() calculates predicted probabilities for all possible dyads
  # don't run 
  # n <- 154*(154 - 1)/2 # total number of possible dyads
  # cat("\n ... does the number of possible dyads "); cat(nrow(pp_update) == n); cat("\n)
  # cat(); cat(nrow(pp_adjust) == n); cat("\n)
  
  
  # calculate r-squared --------------------------------------------------------
  
  # numerator i.e., the variance of the residuals
  pp_update <- dplyr::mutate(pp_update, res = n - p) # calculate residuals
  res_01    <- pp_update[, 5] # residuals
  n <- var(res_01^2)    # numerator i.e., squared variance of the residuals
  
  # denominator i.e., the variance of the residuals
  pp_null  <- dplyr::mutate(pp_null, res = n - p) # residuals
  res_00   <- pp_null[, 5] # residuals
  d <- var(res_00^2)   # denominator i.e., squared variance of the residuals of the null
  
  # r-squared
  r.sq <- 1 - (n/d) # r-squared
  # print r-squared
  cat("\n R-squared, total variance method: "); cat(r.sq); cat("\n")
  
  # calculate the r-squared adjustment -----------------------------------------
  
  # numerator
  pp_adjust <- dplyr::mutate(pp_adjust, res = n - p) # residuals
  res_01    <- pp_adjust[, 5] # residuals
  n <- var(res_01^2)    # numerator i.e., squared variance of the residuals
  
  # denominator
  pp_null  <- dplyr::mutate(pp_null, res = n - p) # residuals
  res_00   <- pp_null[, 5] # residuals
  d <- var(res_00^2)   # denominator i.e., squared variance of the residuals of the null
  
  # r-squared adjustment
  r.dx <- 1 - (n/d)   # r-squared
  r.dx <- r.sq - r.dx # "adjusted" r.squared
  # print r-squared
  cat("\n R-squared, adjustment to the total variance method: "); cat(r.dx); cat("\n")
  cat("\n ... the adjustment is the proportion of error reduction explained by the predictors, independent of the endogenous graph properties.")
}
variance(g = phones, null = null_00, update = ergm_01, adjust = adj)
variance(g = phones, null = ergm_00, update = ergm_01, adjust = adj)








# APPENDIX ---------------------------------------------------------------------

# cohen's r-squared ------------------------------------------------------------
cohen <- function(null, update, adjust){
  # inputs:
  # ... null = null deviance statistic (isolate-adjusted)
  # ... update = updated deviance statistic
  # ... adjust = adjustment to pseudo r-squared
  
  # cohen's r-squared
  r.sq <- (stats::deviance(null)[1] - stats::deviance(update)[1])/stats::deviance(null)[1] # r-squared statistic
  # print Cohen's R-squared
  cat("\n Cohen's R-squared: "); cat(r.sq); cat("\n")
  
  # r.squared adjustment to discount the proportion of error reduction explained by the endogenous graph properties
  r.dx <- (stats::deviance(null)[1] - stats::deviance(adjust)[1])/stats::deviance(null)[1] # calculate r.squared for equation that includes only endogenous terms
  r.dx <- r.sq - r.dx # "adjusted" r.squared
  # print adjustment
  cat("\n Cohen's R-squared, adjustment: "); cat(r.dx); cat("\n")
  cat("\n ... the adjustment is the proportion of error reduction explained by the predictors, independent of the endogenous graph properties.")
}
cohen(null = ergm_00, update = ergm_02, adjust = adj)





# cox-snell r-squared ----------------------------------------------------------
coxsnell <- function(null, update, adjust){
  # inputs:
  # ... null = null log likelihood statistic (isolate-adjusted)
  # ... update = updated log likelihood statistic
  # ... adjust = adjustment to pseudo r-squared
  
  # cox-snell r-squared
  k <- -(2/
  r.sq <- 1 - exp( k * (stats::logLik(update)[1] - stats::logLik(null)[1]) )
  cat("\n Cox-Snell R-squared: "); cat(r.sq); cat("\n")
  
  # r.squared adjustment to discount the proportion of error reduction explained by the endogenous graph properties
  r.dx <- 1 - exp( k * (stats::logLik(adjust)[1] - stats::logLik(null)[1] ) )
  r.dx <- r.sq - r.dx
  cat("\n Cox-Snell's R-squared, adjustment: "); cat(r.dx); cat("\n")
  cat("\n ... the adjustment is the proportion of error reduction explained by the predictors, independent of the endogenous graph properties.")
}
coxsnell(null = ergm_00, update = ergm_02, adjust = adj)



# function to calculate Cox-Snell
cox.snell <- function(g, n, m){
  ll_0 <- -2 * stats::logLik(n)    # null log-likelihood
  ll_1 <- -2 * stats::logLik(m)    # adjusted log-likelihood
  
  # use the number of nodes to calculate the exponent k
  n <- network::network.size(g)
  k    <- 2/n
  R2 = 1 - ((ll_1[1]/ll_0[1])^k)
  message("Cox-Snell r-squared, according to the number of nodes in the graph:")
  cat(R2); cat("\n"); cat("\n")
  
  # use the number of edges/arcs to calculate the exponent k  
  m <- network::network.edgecount(g) # the command 'network::network.dyadcount(g)' calculates all possible dyads 
  k    <- 2/m
  R2 = 1 - ((ll_1[1]/ll_0[1])^k)
  message("Cox-Snell r-squared, accordin to the number of edges/arcs in the graph:")
  print(R2)
}
cox.snell(g = phones, n = null_00, m = ergm_01)



