#' @export
fitPWRFisher = function(X, Y, K, p) {

  start_time <- Sys.time()

  Lmin <- p + 1

  paramPWR <- ParamPWR(X = X, Y = Y, K = K, p = p)

  C1 <- costMatrix(Y, paramPWR$phi)

  Ck <- paramPWR$computeDynamicProgram(C1, K)
  paramPWR$computeParam()

  statPWR <- StatPWR(paramPWR = paramPWR)

  # Estimation of the corresponding regression coefficients
  statPWR$computeMeanFunction(paramPWR)

  # Classes estimees:
  statPWR$klasEstimate(paramPWR)

  statPWR$computeRegressors(paramPWR)
  statPWR$objective = Ck[length(Ck)]
  statPWR$cpu_time = as.numeric(Sys.time() - start_time)

  return(ModelPWR(paramPWR = paramPWR, statPWR = statPWR))
}
