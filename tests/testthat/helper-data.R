# Shared synthetic fixtures for tests. No real or patient data is used.
make_toy_data <- function(n = 60, seed = 42) {
  set.seed(seed)
  half <- n / 2
  data.frame(
    grp = rep(c(0, 1), each = half),
    # informative predictors: group 1 shifted upward
    x1 = c(stats::rnorm(half, 0), stats::rnorm(half, 1.2)),
    x2 = c(stats::rnorm(half, 0), stats::rnorm(half, 0.8)),
    # noise predictor: no group difference
    noise = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
}
