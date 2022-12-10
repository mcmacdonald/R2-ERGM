


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
Efron <- function(null, update, adjust){
  # adjacency graph to pairwise dyads
  ij <- igraph::as_edgelist(intergraph::asIgraph(phone), names = TRUE) # pairwise dyads
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
Efron(null = ergm_00, update = ergm_02, adjust = adj)



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
McFadden(null = ergm_00, update = ergm_02, adjust = adj)



# Tjur's R-squared -------------------------------------------------------------
Tjur <- function(update, adjust){
  # adjacency graph to pairwise dyads
  ij <- igraph::as_edgelist(intergraph::asIgraph(phone), names = TRUE) # pairwise dyads
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
Tjur(update = ergm_02, adjust = adj)


# ... close .r script


