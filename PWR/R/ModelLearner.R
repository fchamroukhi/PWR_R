source("R/costMatrixPPWR.R")
source("R/ParamPWR.R")
source("R/StatPWR.R")
source("R/FittedPWR.R")

fitPWRFisher = function(modelPWR) {
  start_time <- Sys.time()

  phi <- designmatrix(modelPWR$X, modelPWR$p)
  Lmin <- modelPWR$p + 1

  C1 <- costMatrixPPWR(modelPWR, phi, Lmin)

  paramPWR <- ParamPWR(modelPWR)
  Ck <- paramPWR$computeDynamicProgram(C1, modelPWR$K)
  paramPWR$computeParam(modelPWR, phi)

  statPWR <- StatPWR(modelPWR)
  # estimation of the corresponding regression coefficients
  statPWR$computeMeanFunction(paramPWR, phi)

  # classes estimees:
  statPWR$klasEstimate(paramPWR)

  statPWR$computeRegressors(phi, paramPWR)
  statPWR$objective = Ck[length(Ck)]
  statPWR$cpu_time = as.numeric(Sys.time() - start_time)

  return(FittedPWR(modelPWR, paramPWR, statPWR))
}
