ParamPWR <- setRefClass(
  "ParamPWR",
  fields = list(gamma = "matrix",
                beta = "matrix",
                sigma = "matrix"),
  methods = list(
    computeDynamicProgram = function(C1, K){
      ### dynamic programming
      solution <- dynamicProg(C1, K)
      Ck <- solution$J
      gamma <<- matrix(c(0, solution$t_est[nrow(solution$t_est), ]))  # change points
      return(Ck)
    },

    computeParam = function(modelPWR, phi){
      for (k in 1:modelPWR$K) {
        i <- gamma[k] + 1
        j <- gamma[k + 1]
        nk <- j - i + 1
        yij <- modelPWR$Y[i:j]
        X_ij <- phi$XBeta[i:j,]
        beta[,k] <<- solve(t(X_ij) %*% X_ij) %*% t(X_ij) %*% yij

        if (modelPWR$p == 0) {
          z <- yij - X_ij * beta[, k]
          sigma[k] <<- t(z) * z / nk            #variances
          #mean_function[i:j,] <- X_ij * betak[,k]
        } else {
          z <- yij - X_ij %*% beta[, k]
          sigma[k] <<- t(z) %*% z / nk             #variances
          #mean_function[i:j,] <- X_ij %*% betak[,k]
        }
      }
    }
  )
)

ParamPWR <- function(modelPWR) {
  gamma <- matrix(NA, modelPWR$p + 1)
  beta <- matrix(NA, modelPWR$p + 1, modelPWR$K)
  sigma <- matrix(NA, modelPWR$K)
  new("ParamPWR", gamma = gamma, beta = beta, sigma = sigma)
}
