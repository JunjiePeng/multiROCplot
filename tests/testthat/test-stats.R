test_that("compute_univariate_tests returns one row per variable", {
  df <- make_toy_data()
  res <- compute_univariate_tests(df, "grp", c("x1", "x2", "noise"), test = "t")
  expect_equal(nrow(res), 3)
  expect_true(all(c("variable", "p_value", "p_adj_fdr", "note") %in% names(res)))
})

test_that("informative predictor is more significant than noise", {
  df <- make_toy_data()
  res <- compute_univariate_tests(df, "grp", c("x1", "noise"), test = "t")
  p_x1 <- res$p_value[res$variable == "x1"]
  p_noise <- res$p_value[res$variable == "noise"]
  expect_lt(p_x1, p_noise)
  expect_lt(p_x1, 0.05)
})

test_that("FDR adjustment is applied when requested and NA otherwise", {
  df <- make_toy_data()
  with_fdr <- compute_univariate_tests(df, "grp", c("x1", "x2"), adjust_fdr = TRUE)
  no_fdr <- compute_univariate_tests(df, "grp", c("x1", "x2"), adjust_fdr = FALSE)
  expect_false(any(is.na(with_fdr$p_adj_fdr)))
  expect_true(all(with_fdr$p_adj_fdr >= with_fdr$p_value - 1e-12))
  expect_true(all(is.na(no_fdr$p_adj_fdr)))
})

test_that("wilcox option runs and is labelled", {
  df <- make_toy_data()
  res <- compute_univariate_tests(df, "grp", "x1", test = "wilcox")
  expect_match(res$note[1], "Wilcoxon")
})

test_that("non-numeric variables are skipped with a note", {
  df <- make_toy_data()
  df$label <- rep(c("a", "b"), length.out = nrow(df))
  res <- compute_univariate_tests(df, "grp", "label")
  expect_true(is.na(res$p_value[1]))
  expect_match(res$note[1], "Non-numeric")
})
