#' Format a p-value for display
#'
#' @param p A numeric p-value.
#' @return A length-one character string.
#' @keywords internal
#' @noRd
fmt_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 1e-4) return("<1e-4")
  formatC(p, format = "f", digits = 4)
}

#' Univariate group-comparison tests
#'
#' Runs a per-variable two-group comparison (Welch t-test or Mann-Whitney /
#' Wilcoxon rank-sum) between the two levels of a 0/1 grouping column, with
#' optional Benjamini-Hochberg (FDR) adjustment of the p-values.
#'
#' @param df A data frame containing `group_col` and `vars`.
#' @param group_col Name of the 0/1 grouping column (see
#'   [coerce_binary_group()]).
#' @param vars Character vector of variable names to test.
#' @param test Either `"t"` (Welch t-test) or `"wilcox"` (Mann-Whitney).
#' @param adjust_fdr Logical; if `TRUE`, add an FDR-adjusted p-value column.
#'
#' @return A tibble with one row per variable and columns `variable`, `n0`,
#'   `n1`, `statistic`, `p_value`, `note`, and `p_adj_fdr`.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   grp = rep(c(0, 1), each = 20),
#'   x = c(rnorm(20), rnorm(20, 1)),
#'   y = rnorm(40)
#' )
#' compute_univariate_tests(df, "grp", c("x", "y"), test = "t")
#'
#' @export
compute_univariate_tests <- function(df, group_col, vars,
                                      test = c("t", "wilcox"),
                                      adjust_fdr = TRUE) {
  test <- match.arg(test)
  g <- df[[group_col]]
  stopifnot(all(g %in% c(0, 1, NA)))

  res <- lapply(vars, function(v) {
    x <- df[[v]]
    if (!is.numeric(x)) {
      return(tibble::tibble(
        variable = v,
        n0 = sum(!is.na(x) & g == 0),
        n1 = sum(!is.na(x) & g == 1),
        statistic = NA_real_,
        p_value = NA_real_,
        note = "Non-numeric (skipped)"
      ))
    }

    x0 <- x[g == 0]
    x1 <- x[g == 1]
    if (sum(!is.na(x0)) < 2 || sum(!is.na(x1)) < 2) {
      return(tibble::tibble(
        variable = v,
        n0 = sum(!is.na(x0)),
        n1 = sum(!is.na(x1)),
        statistic = NA_real_,
        p_value = NA_real_,
        note = "Insufficient data"
      ))
    }

    tryCatch({
      if (test == "t") {
        tt <- stats::t.test(x ~ g)
        tibble::tibble(
          variable = v, n0 = sum(!is.na(x0)), n1 = sum(!is.na(x1)),
          statistic = unname(tt$statistic), p_value = unname(tt$p.value),
          note = "t-test"
        )
      } else {
        wt <- stats::wilcox.test(x ~ g, exact = FALSE)
        tibble::tibble(
          variable = v, n0 = sum(!is.na(x0)), n1 = sum(!is.na(x1)),
          statistic = unname(wt$statistic), p_value = unname(wt$p.value),
          note = "Mann-Whitney (Wilcoxon rank-sum)"
        )
      }
    }, error = function(e) {
      tibble::tibble(
        variable = v, n0 = sum(!is.na(x0)), n1 = sum(!is.na(x1)),
        statistic = NA_real_, p_value = NA_real_,
        note = paste0("Error: ", conditionMessage(e))
      )
    })
  })

  res <- dplyr::bind_rows(res)
  if (adjust_fdr) {
    res$p_adj_fdr <- stats::p.adjust(res$p_value, method = "fdr")
  } else {
    res$p_adj_fdr <- NA_real_
  }
  res
}
