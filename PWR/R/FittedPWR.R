FittedPWR <- setRefClass(
  "FittedPWR",
  fields = list(
    modelPWR = "ModelPWR",
    paramPWR = "ParamPWR",
    statPWR = "StatPWR"
  ),
  methods = list(
    plot = function() {
      K <- ncol(paramPWR$beta)

      par(mfrow = c(2, 1))
      plot.default(modelPWR$Y, type = "l", ylab = "y", xlab = "")
      title(main = "Time series, PWR regimes, and segmentation")
      colors = rainbow(modelPWR$K)
      for (k in 1:modelPWR$K) {
        model_k = statPWR$regressors[,k]

        active_model_k = model_k[statPWR$klas==k]
        active_period_model_k = modelPWR$X[statPWR$klas==k]

        inactive_model_k = model_k[statPWR$klas != k]
        inactive_period_model_k = modelPWR$X[statPWR$klas != k]

        if (!is.null(active_model_k)){
          par(new=TRUE)
          lines(inactive_period_model_k, inactive_model_k,col = couleur[k], type = "p", lty = 3, lwd = .5,pch=20, cex=0.1)
          par(new=TRUE)
          lines(active_period_model_k, active_model_k,col = couleur[k], lty = "solid", lwd = 4)
        }
      }



      par(mfrow = c(2, 1))
      plot.default(modelPWR$Y, type = "l", ylab = "y", xlab = "")
      title(main = "Time series, PWR function, and segmentation")
      gammak=c(0, paramPWR$gamma, n)
      for (k in 1:K){
        Ik = (gammak[k]+1):gammak[k+1]
        segmentk = statPWR$mean_function[Ik]
        par(new=TRUE)
        lines(t[Ik], segmentk, col = 'red', type = "l", lty = "solid", lwd = 2)
      }
      par(new=TRUE)
      for (i in 1:length(t[paramPWR$gamma])){
        par(new=TRUE)
        abline(v=t[paramRHLP$gamma][i], lty = "dotted", col = 'black', lwd = 2)
      }


    }
  )
)

FittedPWR <- function(modelPWR, paramPWR, statPWR) {
  new("FittedPWR", modelPWR = modelPWR, paramPWR = paramPWR, statPWR = statPWR)
}
