FittedPWR <- setRefClass(
  "FittedPWR",
  fields = list(
    modelPWR = "ModelPWR",
    paramPWR = "ParamPWR",
    statPWR = "StatPWR"
  ),
  methods = list(
    plot = function() {

      yaxislim <- c(mean(modelPWR$Y) - 2 * sqrt(var(modelPWR$Y)), mean(modelPWR$Y) + 2 * sqrt(var(modelPWR$Y)))

      # Time series, regressors, and segmentation
      par(mai = c(0.6, 1, 0.5, 0.5), mgp = c(2, 1, 0))
      plot.default(modelPWR$Y, type = "l", ylim = yaxislim, xlab = "x", ylab = "y")
      title(main = "Time series, PWR regimes, and segmentation")
      colorsvec <- rainbow(modelPWR$K)
      for (k in 1:modelPWR$K) {
        model_k <- statPWR$regressors[, k]

        index <- statPWR$klas == k
        active_model_k <- model_k[index]
        active_period_model_k <- seq(1:modelPWR$m)[index]

        if (length(active_model_k) != 0) {
          lines(model_k, col = colorsvec[k], lty = "dotted", lwd = 1.5)
          lines(active_period_model_k, active_model_k, type = "l", col = colorsvec[k], lwd = 1.5)
        }
      }

      # Time series, estimated regression function, and optimal segmentation
      plot.default(modelPWR$Y, type = "l", ylim = yaxislim, xlab = "x", ylab = "y")
      title(main = "Time series, PWR function, and segmentation")

      for (k in 1:modelPWR$K) {
        Ik = paramPWR$gamma[k] + 1:(paramPWR$gamma[k + 1] - paramPWR$gamma[k])
        segmentk = statPWR$mean_function[Ik]
        lines(t(Ik), segmentk, type = "l", col = colorsvec[k], lwd = 1.5)
      }

      for (i in 1:length(paramPWR$gamma)) {
        abline(v = paramPWR$gamma[i], col = "red", lty = "dotted", lwd = 1.5)
      }
    }
  )
)

FittedPWR <- function(modelPWR, paramPWR, statPWR) {
  new("FittedPWR", modelPWR = modelPWR, paramPWR = paramPWR, statPWR = statPWR)
}
