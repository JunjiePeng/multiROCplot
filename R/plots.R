#' Plot overlaid ROC curves
#'
#' Draws one ROC curve per numeric predictor (and a combined multivariable
#' curve when applicable) on a single set of axes, with AUCs shown in the
#' legend. Curves come from the same models as [compute_roc_table()].
#'
#' @inheritParams roc_combined
#'
#' @return A [ggplot2::ggplot] object, or `NULL` if no curve can be drawn.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(grp = rep(c(0, 1), each = 40),
#'                  x = c(rnorm(40), rnorm(40, 1)),
#'                  z = c(rnorm(40), rnorm(40, 0.8)))
#' p <- plot_roc_curves(df, "grp", c("x", "z"))
#'
#' @export
plot_roc_curves <- function(df, group_col, vars, min_cases = 10) {
  curves <- list()
  for (v in vars) {
    r <- roc_from_predictor(df, group_col, v, min_cases = min_cases)
    if (is.null(r)) next
    lbl <- sprintf("%s (AUC %.2f)", v, as.numeric(pROC::auc(r)))
    curves[[lbl]] <- r
  }

  r <- roc_combined(df, group_col, vars, min_cases = min_cases)
  if (!is.null(r)) {
    lbl <- sprintf("Combined (AUC %.2f)", as.numeric(pROC::auc(r)))
    curves[[lbl]] <- r
  }

  if (length(curves) == 0) return(NULL)

  pROC::ggroc(curves, legacy.axes = TRUE) +
    ggplot2::geom_abline(linetype = "dotted") +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = "False Positive Rate", y = "True Positive Rate") +
    ggprism::theme_prism(base_size = 12) +
    ggplot2::theme(
      legend.position = "right",
      legend.title = ggplot2::element_blank()
    )
}

#' Faceted group boxplots with p-value annotations
#'
#' Draws a boxplot (with jittered points) of each variable split by the two
#' outcome groups, faceted by variable and annotated with the (optionally
#' FDR-adjusted) p-value from [compute_univariate_tests()].
#'
#' @param df A data frame containing `group_col` and `vars`.
#' @param group_col Name of the 0/1 grouping column.
#' @param vars Character vector of numeric variables to plot.
#' @param p_tbl A p-value table as returned by [compute_univariate_tests()].
#' @param use_adj Logical; annotate with FDR-adjusted p-values (`TRUE`) or raw
#'   p-values (`FALSE`).
#'
#' @return A [ggplot2::ggplot] object.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(grp = rep(c(0, 1), each = 20),
#'                  x = c(rnorm(20), rnorm(20, 1)))
#' pt <- compute_univariate_tests(df, "grp", "x", test = "t")
#' plot_group_boxplots(df, "grp", "x", pt)
#'
#' @export
plot_group_boxplots <- function(df, group_col, vars, p_tbl, use_adj = TRUE) {
  plot_df <- df[, c(group_col, vars), drop = FALSE]
  plot_df <- tidyr::pivot_longer(
    plot_df, cols = dplyr::all_of(vars),
    names_to = "variable", values_to = "value"
  )
  plot_df$group <- factor(plot_df[[group_col]], levels = c(0, 1),
                          labels = c("Group 0", "Group 1"))

  p_lab <- if (use_adj) {
    paste0("FDR p=", vapply(p_tbl$p_adj_fdr, fmt_p, character(1)))
  } else {
    paste0("p=", vapply(p_tbl$p_value, fmt_p, character(1)))
  }
  p_tbl2 <- tibble::tibble(variable = p_tbl$variable, p_label = p_lab)

  y_pos <- dplyr::summarise(
    dplyr::group_by(plot_df, .data$variable),
    y = max(.data$value, na.rm = TRUE), .groups = "drop"
  )
  y_pos$y <- ifelse(is.finite(y_pos$y), y_pos$y, 1)

  ann <- dplyr::left_join(p_tbl2, y_pos, by = "variable")
  ann$y <- ann$y + 0.08 * abs(ann$y)

  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$group, y = .data$value)) +
    ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::geom_jitter(width = 0.15, alpha = 0.7, size = 1.6) +
    ggplot2::facet_wrap(~variable, scales = "free_y") +
    ggplot2::geom_text(
      data = ann,
      ggplot2::aes(x = 1.5, y = .data$y, label = .data$p_label),
      inherit.aes = FALSE, size = 3.5
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggprism::theme_prism(base_size = 12) +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "none"
    )
}
