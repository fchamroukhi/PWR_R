tril = function(A, K) {
  #############################################################
  # by Aline CANARD                                           #
  # Extract a k triangular part of a squared matrix           #
  # k=0, extracts the diagonal and all the elements below     #
  # k<0, extracts the kth diagonal below the main diagonal    #
  # k>0, extracts the kth diagonal above the main diagonal    #
  #############################################################
  
  n = ncol(A)
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (i < (j - K))  {
        A[i,j] = 0
      }
    }
  }
  return(A)
}