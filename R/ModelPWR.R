ModelPWR <- setRefClass(
  "ModelPWR",
  contains = "FData",
  # Define the fields
  fields = list(
    K = "numeric",
    # number of regimes
    p = "numeric"
  )
)

ModelPWR <- function(fData, K, p) {
  new(
    "ModelPWR",
    Y = fData$Y,
    X = fData$X,
    m = fData$m,
    n = fData$n,
    K = K,
    p = p
  )
}
