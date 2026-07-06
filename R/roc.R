#' Fit a univariate logistic ROC curve
#'
#' Fits a logistic regression of the 0/1 outcome on a single numeric predictor
#' and returns the ROC curve of the fitted probabilities. This is the shared
#' primitive used by both [compute_roc_table()] and [plot_roc_curves()], so the
#' curves in the plot and the numbers in the table always come from the same
#' model.
#'
#' @param df A data frame containing `group_col` and `var`.
#' @param group_col Name of the 0/1 outcome column.
#' @param var Name of a single numeric predictor column.
#' @param min_cases Minimum number of complete cases required to fit; below this
#'   the function returns `NULL`.
#'
#' @return A [pROC::roc] object, or `NULL` if the predictor is non-numeric or
#'   there are too few complete cases.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(grp = rep(c(0, 1), each = 30),
#'                  x = c(rnorm(30), rnorm(30, 1.5)))
#' r <- roc_from_predictor(df, "grp", "x")
#' if (!is.null(r)) as.numeric(pROC::auc(r))
#'
#' @export
roc_from_predictor <- function(df, group_col, var, min_cases = 10) {
  if (!is.numeric(df[[var]])) return(NULL)
  dd <- df[, c(group_col, var), drop = FALSE]
  dd <- dd[!is.na(dd[[group_col]]) & !is.na(dd[[var]]), , drop = FALSE]
  if (nrow(dd) < min_cases) return(NULL)
  fit <- stats::glm(
    stats::as.formula(paste0("`", group_col, "` ~ `", var, "`")),
    data = dd, family = stats::binomial()
  )
  prob <- as.numeric(stats::predict(fit, type = "response"))
  pROC::roc(dd[[group_col]], prob, quiet = TRUE)
}

#' Fit a combined (multivariable) logistic ROC curve
#'
#' Fits a logistic regression of the 0/1 outcome on all numeric predictors in
#' `vars` and returns the ROC curve of the fitted probabilities.
#'
#' @inheritParams roc_from_predictor
#' @param vars Character vector of predictor names; only numeric columns are
#'   used, and at least two are required.
#'
#' @return A [pROC::roc] object, or `NULL` if fewer than two numeric predictors
#'   are available or there are too few complete cases.
#'
#' @export
roc_combined <- function(df, group_col, vars, min_cases = 10) {
  numeric_vars <- vars[vapply(df[vars], is.numeric, logical(1))]
  if (length(numeric_vars) < 2) return(NULL)
  dd <- df[, c(group_col, numeric_vars), drop = FALSE]
  dd <- dd[stats::complete.cases(dd), , drop = FALSE]
  if (nrow(dd) < min_cases) return(NULL)
  form <- stats::as.formula(
    paste0("`", group_col, "` ~ ",
           paste(sprintf("`%s`", numeric_vars), collapse = " + "))
  )
  fit <- stats::glm(form, data = dd, family = stats::binomial())
  prob <- as.numeric(stats::predict(fit, type = "response"))
  pROC::roc(dd[[group_col]], prob, quiet = TRUE)
}

# Build a one-row summary tibble from a fitted roc object.
.roc_summary_row <- function(r, model, predictors, note) {
  ci <- tryCatch(as.numeric(pROC::ci.auc(r)),
                 error = function(e) c(NA_real_, NA_real_, NA_real_))
  best <- tryCatch(
    pROC::coords(r, x = "best", best.method = "youden", transpose = FALSE),
    error = function(e) NULL
  )
  tibble::tibble(
    model = model,
    predictors = predictors,
    auc = as.numeric(pROC::auc(r)),
    auc_ci_low = ci[1],
    auc_ci_high = ci[3],
    best_threshold = if (is.null(best)) NA_real_ else as.numeric(best[["threshold"]]),
    sensitivity = if (is.null(best)) NA_real_ else as.numeric(best[["sensitivity"]]),
    specificity = if (is.null(best)) NA_real_ else as.numeric(best[["specificity"]]),
    n = length(r$cases) + length(r$controls),
    note = note
  )
}

# Build a one-row placeholder tibble for skipped variables.
.roc_na_row <- function(model, predictors, n, note) {
  tibble::tibble(
    model = model, predictors = predictors, auc = NA_real_,
    auc_ci_low = NA_real_, auc_ci_high = NA_real_, best_threshold = NA_real_,
    sensitivity = NA_real_, specificity = NA_real_, n = n, note = note
  )
}

#' Compute a table of ROC / AUC results
#'
#' For each predictor, fits a univariate logistic ROC (via
#' [roc_from_predictor()]) and reports AUC with confidence interval and the
#' Youden-optimal threshold, sensitivity and specificity. When two or more
#' numeric predictors are supplied, an additional combined multivariable model
#' is appended (via [roc_combined()]).
#'
#' @inheritParams roc_combined
#'
#' @return A tibble with one row per predictor (plus an optional `Combined`
#'   row) and columns `model`, `predictors`, `auc`, `auc_ci_low`,
#'   `auc_ci_high`, `best_threshold`, `sensitivity`, `specificity`, `n`,
#'   `note`.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(grp = rep(c(0, 1), each = 40),
#'                  x = c(rnorm(40), rnorm(40, 1)),
#'                  z = c(rnorm(40), rnorm(40, 0.8)))
#' compute_roc_table(df, "grp", c("x", "z"))
#'
#' @export
compute_roc_table <- function(df, group_col, vars, min_cases = 10) {
  rows <- list()
  for (v in vars) {
    x <- df[[v]]
    if (!is.numeric(x)) {
      rows[[v]] <- .roc_na_row(
        v, v, sum(!is.na(df[[group_col]]) & !is.na(x)), "Non-numeric (skipped)"
      )
      next
    }
    r <- roc_from_predictor(df, group_col, v, min_cases = min_cases)
    if (is.null(r)) {
      n_complete <- sum(!is.na(df[[group_col]]) & !is.na(x))
      rows[[v]] <- .roc_na_row(v, v, n_complete, "Too few complete cases")
      next
    }
    rows[[v]] <- .roc_summary_row(r, model = v, predictors = v, note = "Univariate")
  }

  numeric_vars <- vars[vapply(df[vars], is.numeric, logical(1))]
  r <- roc_combined(df, group_col, vars, min_cases = min_cases)
  if (!is.null(r)) {
    rows[["Combined"]] <- .roc_summary_row(
      r, model = "Combined",
      predictors = paste(numeric_vars, collapse = "+"), note = "Multivariable"
    )
  }

  dplyr::bind_rows(rows)
}
