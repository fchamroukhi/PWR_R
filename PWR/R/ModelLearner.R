source("R/utils.R")
source("R/costMatrixPPWR.R")
source("R/dynamicProg.R")

fitPWRFisher = function(modelPWR) {
  if (ncol(modelPWR$Y) != 1) {
    modelPWR$Y <- t(modelPWR$Y)
  }
  if (ncol(modelPWR$X) != 1) {
    modelPWR$X <- t(modelPWR$X)
  }


  Lmin <- modelPWR$p + 1

  phi <- designmatrix(modelPWR$X, modelPWR$p)

  start_time <- Sys.time()

  C1 <- costMatrixPPWR(modelPWR, phi, Lmin)

  ### dynamic programming
  solution <- dynamicProg(C1, modelPWR$K)  #oK
  Ck <- solution$J             #OK
  t_est <- solution$t_est          #oK

  gammak <- c(0, t_est[nrow(t_est), ])  # change points

  # estimation of the corresponding regression coefficients
  mean_function = matrix(0 , nrow = modelPWR$m , ncol = 1)


  betak <- matrix(0 , nrow = modelPWR$p + 1 , ncol = modelPWR$K)
  sigma2k <- c(rep(0, modelPWR$K))

  if (p == 0){
    for (k in 1 : modelPWR$K) {
      i <- gammak[k] + 1
      j <- gammak[k + 1]
      nk <- j - i + 1
      yij <- modelPWR$Y[i:j]
      X_ij <- phi$XBeta[i:j,]
      betak[, k] <- solve(t(X_ij) %*% X_ij) %*% t(X_ij) %*% yij
      z <- yij - X_ij * betak[, k]
      sigma2k[k] <- t(z) * z / nk            #variances
      mean_function[i:j,] <- X_ij * betak[,k]
    }
  } else {
    for (k in 1: modelPWR$K) {
      i <- gammak[k] + 1
      j <- gammak[k + 1]
      nk <- j - i + 1
      yij <- modelPWR$Y[i:j]
      X_ij <- phi$XBeta[i:j,]
      betak[,k] <- solve(t(X_ij) %*% X_ij) %*% t(X_ij) %*% yij
      z <- yij - X_ij %*% betak[,k]
      sigma2k[k] <- t(z) %*% z / nk             #variances
      mean_function[i:j,] <- X_ij %*% betak[,k]
    }
  }




  # classes estimees:
  klas <- rep(0, modelPWR$m)
  Zik <- matrix(0 , nrow = modelPWR$m , ncol = modelPWR$K)

  for (k in 1:K)  {
    i <- gammak[k] + 1
    j <- gammak[k + 1]
    klas[i:j] <- k
    Zik[i:j,k] <- 1
  }

  PWR = list()
  PWR$param = list()
  PWR$stats = list()

  PWR$param$betak = betak
  PWR$param$sigma2k = sigma2k
  PWR$param$gammak = gammak[2:(length(gammak) - 1)]    #sans le 0 et le n
  PWR$param$parameter_vector = c(as.vector(PWR$param$gammak), as.vector(PWR$param$betak), as.vector(PWR$param$sigma2k))

  PWR$stats$klas = klas
  PWR$stats$mean_function = mean_function
  PWR$stats$regressors = phi$XBeta %*% PWR$param$betak
  PWR$stats$Zik = Zik
  PWR$stats$objective = Ck[length(Ck)]
  PWR$stats$cputime = Sys.time() - start_time

  return(PWR)

}
