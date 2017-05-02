# There are two functions: 
# 1) ES performs simple Ensemble Selection.
# 2) BES performs Bagged Ensemble Selection.

# loading library
library(compiler)

# function for perfroming ES
ES <- cmpfun(function(X, Y, iter = 100L){
  
  # converting target to numeric
  Y <- as.numeric(Y)-1
  
  # setting initial values
  N           <- ncol(X)
  weights     <- rep(0L, N)
  pred        <- 0 * X
  sum.weights <- 0L
  
  # performing hill-climbing
  while(sum.weights < iter) {
    sum.weights   <- sum.weights + 1L
    pred          <- (pred + X) * (1L / sum.weights)
    errors        <- sqrt(colSums((pred - Y) ^ 2L))
    best          <- which.min(errors)
    weights[best] <- weights[best] + 1L
    pred          <- pred[, best] * sum.weights
  }
  
  # returning model weights
  return(weights / sum.weights)
})


# function for performing bagged ES
BES <- cmpfun(function(X, Y, bags = 10L, p = 0.5, iter = 100L, display = TRUE){
  
  # converting target to numeric
  Y <- as.numeric(Y)-1
  
  # setting initial values
  i <- 0L
  N <- nrow(X)
  M <- ncol(X)
  W <- matrix(rbinom(bags * M, 1, p), ncol = M)
  
  # performing bagging
  while(i < bags)  {
    
    # displyaing iteration number  
    if ((display == TRUE) & ((i+1)%%5L == 0L)) {
      print(paste0("BES - iteration ", i+1, "/", bags))
    }
    
    # doing ES on a bagged sample
    i         <- i + 1L
    ind       <- which(W[i, ] == 1)
    W[i, ind] <- W[i, ind] * ES(X[, ind], Y, iter)
  }
  
  # returning model weights
  return(colSums(W) / bags)
})