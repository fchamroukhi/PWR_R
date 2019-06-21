#' @export
ParamPWR <- setRefClass(
  "ParamPWR",
  fields = list(
    X = "numeric",
    Y = "numeric",
    m = "numeric",
    phi = "matrix",

    K = "numeric", # Number of regimes
    p = "numeric", # Dimension of beta (order of polynomial regression)

    gamma = "matrix",
    beta = "matrix",
    sigma2 = "matrix"
  ),
  methods = list(
    initialize = function(X = numeric(), Y = numeric(1), K = 2, p = 3) {
      X <<- X
      Y <<- Y
      m <<- length(Y)
      phi <<- designmatrix(X, p)$XBeta

      K <<- K
      p <<- p

      gamma <<- matrix(NA, p + 1)
      beta <<- matrix(NA, p + 1, K)
      sigma2 <<- matrix(NA, K)

    },

    computeDynamicProgram = function(C1, K) {
      # Dynamic programming
      solution <- dynamicProg(C1, K)
      Ck <- solution$J
      gamma <<- matrix(c(0, solution$t_est[nrow(solution$t_est),]))  # Change points
      return(Ck)
    },

    computeParam = function() {
      for (k in 1:K) {
        i <- gamma[k] + 1
        j <- gamma[k + 1]
        nk <- j - i + 1
        yij <- Y[i:j]
        X_ij <- phi[i:j, ]
        beta[, k] <<- solve(t(X_ij) %*% X_ij) %*% t(X_ij) %*% yij

        if (p == 0) {
          z <- yij - X_ij * beta[, k]
        } else {
          z <- yij - X_ij %*% beta[, k]
        }
        sigma2[k] <<- t(z) %*% z / nk # Variances
      }
    }
  )
)
