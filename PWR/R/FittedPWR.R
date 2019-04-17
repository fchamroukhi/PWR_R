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
      plot.default(modelPWR$Y, type = "l", ylab = "y", xlab = "", ylim = yaxislim)
      title(main = "Time series, PWR regimes, and segmentation")
      colors <- rainbow(modelPWR$K)
      for (k in 1:modelPWR$K) {
        model_k <- statPWR$regressors[, k]

        index <- statPWR$klas == k
        active_model_k <- model_k[index]
        active_period_model_k <- seq(1:modelPWR$m)[index]

        if (length(active_model_k) != 0) {
          lines(model_k, col = colors[k], lty = "dotted", lwd = 1)
          lines(active_period_model_k, active_model_k, col = colors[k], type = "l", lwd = 3)
        }
      }

      # Time series, estimated regression function, and optimal segmentation
      plot.default(modelPWR$Y, type = "l", ylab = "y", xlab = "", ylim = yaxislim)
      title(main = "Time series, PWR function, and segmentation")

      for (k in 1:modelPWR$K) {
        Ik = paramPWR$gamma[k] + 1:(paramPWR$gamma[k + 1] - paramPWR$gamma[k])
        segmentk = statPWR$mean_function[Ik]
        lines(t(Ik), segmentk, col = colors[k], type = "l", lwd = 3)
      }

      for (i in 1:length(paramPWR$gamma)) {
        abline(v = paramPWR$gamma[i], lty = "dotted", lwd = 2, col = "red")
      }
    }
  )
)

FittedPWR <- function(modelPWR, paramPWR, statPWR) {
  new("FittedPWR", modelPWR = modelPWR, paramPWR = paramPWR, statPWR = statPWR)
}
