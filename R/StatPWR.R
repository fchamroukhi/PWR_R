StatPWR <- setRefClass(
  "StatPWR",
  fields = list(
    z_ik = "matrix",
    klas = "matrix",
    mean_function = "matrix",
    cpu_time = "numeric",
    regressors = "matrix",
    objective = "numeric"
  ),
  methods = list(
    klasEstimate = function(paramPWR) {
      K <- ncol(paramPWR$beta)
      # classes estimees:
      for (k in 1:K)  {
        i <- paramPWR$gamma[k] + 1
        j <- paramPWR$gamma[k + 1]
        klas[i:j] <<- k
        z_ik[i:j,k] <<- 1
      }
    },
    computeMeanFunction = function(paramPWR, phi){
      K <- ncol(paramPWR$beta)
      for (k in 1:K) {
        i <- paramPWR$gamma[k] + 1
        j <- paramPWR$gamma[k + 1]
        X_ij <- phi$XBeta[i:j,]

        if (p == 0) {
          mean_function[i:j,] <<- X_ij * paramPWR$beta[,k]
        } else {
          mean_function[i:j,] <<- X_ij %*% paramPWR$beta[,k]
        }
      }
    },

    computeRegressors = function(phi, paramPWR){
      regressors <<- phi$XBeta %*% paramPWR$beta
    }
  )
)


StatPWR <- function(modelPWR) {
  z_ik <- matrix(0, modelPWR$m, modelPWR$K)
  klas <- matrix(NA, modelPWR$m, 1)
  mean_function <- matrix(NA, nrow = modelPWR$m , ncol = 1)
  cpu_time <- Inf
  regressors <- matrix(NA, modelPWR$m, modelPWR$K)
  objective <- -Inf
  new("StatPWR",  z_ik = z_ik, klas = klas, mean_function = mean_function, cpu_time = cpu_time, regressors = regressors, objective = objective)
}
