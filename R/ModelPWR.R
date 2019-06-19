#' @export
ModelPWR <- setRefClass(
  "ModelPWR",
  fields = list(
    paramPWR = "ParamPWR",
    statPWR = "StatPWR"
  ),
  methods = list(
    plot = function(what = c("regressors", "segmentation")) {

      what <- match.arg(what, several.ok = TRUE)

      oldpar <- par()[c("mai", "mgp")]
      on.exit(par(oldpar), add = TRUE)

      yaxislim <- c(mean(paramPWR$Y) - 2 * sqrt(var(paramPWR$Y)), mean(paramPWR$Y) + 2 * sqrt(var(paramPWR$Y)))

      colorsvec <- rainbow(paramPWR$K)

      if (any(what == "regressors")) {
        # Time series, regressors, and segmentation
        par(mai = c(0.6, 1, 0.5, 0.5), mgp = c(2, 1, 0))
        plot.default(paramPWR$X, paramPWR$Y, type = "l", ylim = yaxislim, xlab = "x", ylab = "y")
        title(main = "Time series, PWR regimes, and segmentation")
        for (k in 1:paramPWR$K) {
          model_k <- statPWR$regressors[, k]

          index <- statPWR$klas == k
          active_model_k <- model_k[index]
          active_period_model_k <- paramPWR$X[index]

          if (length(active_model_k) != 0) {
            lines(paramPWR$X, model_k, col = colorsvec[k], lty = "dotted", lwd = 1.5)
            lines(active_period_model_k, active_model_k, type = "l", col = colorsvec[k], lwd = 1.5)
          }
        }
      }

      if (any(what == "segmentation")) {
        # Time series, estimated regression function, and optimal segmentation
        plot.default(paramPWR$X, paramPWR$Y, type = "l", ylim = yaxislim, xlab = "x", ylab = "y")
        title(main = "Time series, PWR function, and segmentation")

        for (k in 1:paramPWR$K) {
          Ik = paramPWR$gamma[k] + 1:(paramPWR$gamma[k + 1] - paramPWR$gamma[k])
          segmentk = statPWR$mean_function[Ik]
          lines(paramPWR$X[t(Ik)], segmentk, type = "l", col = colorsvec[k], lwd = 1.5)
        }

        for (i in 1:length(paramPWR$gamma)) {
          abline(v = paramPWR$X[paramPWR$gamma[i]], col = "red", lty = "dotted", lwd = 1.5)
        }
      }
    },

    summary = function() {

      digits = getOption("digits")

      title <- paste("Fitted PWR model")
      txt <- paste(rep("-", min(nchar(title) + 4, getOption("width"))), collapse = "")

      # Title
      cat(txt)
      cat("\n")
      cat(title)
      cat("\n")
      cat(txt)

      cat("\n")
      cat("\n")
      cat(paste0("PWR model with K = ", paramPWR$K, ifelse(paramPWR$K > 1, " components", " component"), ":"))
      cat("\n")

      cat("\nClustering table (Number of observations in each regimes):\n")
      print(table(statPWR$klas))

      cat("\nRegression coefficients:\n\n")
      if (paramPWR$p > 0) {
        row.names = c("1", sapply(1:paramPWR$p, function(x) paste0("X^", x)))
      } else {
        row.names = "1"
      }

      betas <- data.frame(paramPWR$beta, row.names = row.names)
      colnames(betas) <- sapply(1:paramPWR$K, function(x) paste0("Beta(K = ", x, ")"))
      print(betas, digits = digits)

      cat("\nVariances:\n\n")
      sigma2 = data.frame(t(paramPWR$sigma2), row.names = NULL)
      colnames(sigma2) = sapply(1:paramPWR$K, function(x) paste0("Sigma2(K = ", x, ")"))
      print(sigma2, digits = digits, row.names = FALSE)

    }
  )
)
