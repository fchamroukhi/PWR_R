designmatrix = function(x, p) {
  ######################################################################
  # constructs the design matrix of a polynomial regression of degree p
  #
  #
  # Faicel Chamroukhi
  ###########################################################################
  
  if (!is.vector(x)) {
    x = as.numeric(t(x))          ## a column vector
  }
    
  n = length(x)
  X = matrix(0, nrow = n, ncol = p + 1)
  
  for (i in 1:(p + 1))
    X[,i] = x ^ (i - 1)   # X = [1 x x.^2 x.^3 x.^p]
  
  return(X)
}
