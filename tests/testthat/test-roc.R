test_that("roc_from_predictor returns a roc object for a numeric predictor", {
  df <- make_toy_data()
  r <- roc_from_predictor(df, "grp", "x1")
  expect_s3_class(r, "roc")
  expect_gt(as.numeric(pROC::auc(r)), 0.6)
})

test_that("roc_from_predictor returns NULL for non-numeric or too-few cases", {
  df <- make_toy_data()
  df$label <- rep(c("a", "b"), length.out = nrow(df))
  expect_null(roc_from_predictor(df, "grp", "label"))
  expect_null(roc_from_predictor(df[1:5, ], "grp", "x1", min_cases = 10))
})

test_that("roc_combined needs at least two numeric predictors", {
  df <- make_toy_data()
  expect_null(roc_combined(df, "grp", "x1"))
  r <- roc_combined(df, "grp", c("x1", "x2"))
  expect_s3_class(r, "roc")
})

test_that("compute_roc_table adds a Combined row for >= 2 numeric vars", {
  df <- make_toy_data()
  tbl <- compute_roc_table(df, "grp", c("x1", "x2"))
  expect_true("Combined" %in% tbl$model)
  expect_equal(nrow(tbl), 3)
  expect_true(all(c("auc", "auc_ci_low", "auc_ci_high", "best_threshold",
                    "sensitivity", "specificity", "n") %in% names(tbl)))
})

test_that("compute_roc_table AUCs are in [0, 1]", {
  df <- make_toy_data()
  tbl <- compute_roc_table(df, "grp", c("x1", "x2", "noise"))
  aucs <- tbl$auc[!is.na(tbl$auc)]
  expect_true(all(aucs >= 0 & aucs <= 1))
})

test_that("plot_roc_curves and plot_group_boxplots return ggplot objects", {
  df <- make_toy_data()
  p_roc <- plot_roc_curves(df, "grp", c("x1", "x2"))
  expect_s3_class(p_roc, "ggplot")
  pt <- compute_univariate_tests(df, "grp", c("x1", "x2"))
  p_box <- plot_group_boxplots(df, "grp", c("x1", "x2"), p_tbl = pt)
  expect_s3_class(p_box, "ggplot")
})
