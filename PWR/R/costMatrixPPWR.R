source("R/utils.R")

costMatrixPPWR = function(t, y, p, Lmin = 1) {
  n <- length(y)

  X <- designmatrix(t,p)
  nl <- n - Lmin + 1


  C1 <- matrix(Inf , nrow = n , ncol = n)
  C1 <- tril(C1, Lmin - 2)

  for (a in 0:nl) {
    if ((a + 1 + Lmin) <= n) {
      ############################################################################
      # Condition added to handle the cases (a + 1 + Lmin) > n                   #
      ############################################################################
      for (b in (a + 1 + Lmin):n)  {
        # ici et dans ce qui suit a+1 car en matlab les indices commencent de 1

        yab <- y[(a + 1):b]
        X_ab <- X[(a + 1):b ,]
        nk <- b - a                  #length(xab)

        #beta = svd.inverse(crossprod(X_ab, X_ab)) %*% crossprod(X_ab, yab)
        beta <- solve(t(X_ab) %*% X_ab) %*% t(X_ab) %*% yab
        z <- yab - X_ab %*% beta
        #sigma2 = crossprod(z, z) / nk
        sigma2 <- t(z) %*% z / nk
        C1[a + 1, b] <- nk + nk * log(sigma2 + .Machine$double.eps)      # + (z'*z)/sigma2

      }
    }
  }

  return(C1)
}
