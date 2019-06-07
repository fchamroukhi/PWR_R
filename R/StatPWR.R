#' @export
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

    initialize = function(paramPWR = ParamPWR(fData = FData(numeric(1), matrix(1)), K = 2, p = 2)) {

      z_ik <<- matrix(0, paramPWR$fData$m, paramPWR$K)
      klas <<- matrix(NA, paramPWR$fData$m, 1)
      mean_function <<- matrix(NA, nrow = paramPWR$fData$m , ncol = 1)
      cpu_time <<- Inf
      regressors <<- matrix(NA, paramPWR$fData$m, paramPWR$K)
      objective <<- -Inf

    },

    klasEstimate = function(paramPWR) {
      K <- ncol(paramPWR$beta)
      # Classes estimees:
      for (k in 1:K)  {
        i <- paramPWR$gamma[k] + 1
        j <- paramPWR$gamma[k + 1]
        klas[i:j] <<- k
        z_ik[i:j,k] <<- 1
      }
    },

    computeMeanFunction = function(paramPWR) {
      K <- ncol(paramPWR$beta)
      for (k in 1:K) {
        i <- paramPWR$gamma[k] + 1
        j <- paramPWR$gamma[k + 1]
        X_ij <- paramPWR$phi[i:j,]

        if (p == 0) {
          mean_function[i:j,] <<- X_ij * paramPWR$beta[,k]
        } else {
          mean_function[i:j,] <<- X_ij %*% paramPWR$beta[,k]
        }
      }
    },

    computeRegressors = function(paramPWR){
      regressors <<- paramPWR$phi %*% paramPWR$beta
    }
  )
)
