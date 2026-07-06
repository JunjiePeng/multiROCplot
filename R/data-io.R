#' Read a data file into a data frame
#'
#' Reads tabular data from Excel (`.xlsx`/`.xls`), comma-separated (`.csv`), or
#' delimited text (`.txt`/`.tsv`) files. This is the file-import engine used by
#' the Shiny app, exposed so the same logic can be reused programmatically.
#'
#' @param path Path to the file to read.
#' @param ext File extension (without the dot), e.g. `"xlsx"`, `"csv"`, `"tsv"`.
#'   Case-insensitive.
#' @param sheet For Excel files, the sheet name or index to read. If `NULL` or
#'   `""`, the first sheet is read.
#' @param delim For `.txt`/`.tsv` files, the field delimiter. Defaults to a tab.
#'
#' @return A [tibble][tibble::tibble] / data frame of the file contents.
#'
#' @examples
#' tmp <- tempfile(fileext = ".csv")
#' utils::write.csv(data.frame(a = 1:3, b = 4:6), tmp, row.names = FALSE)
#' read_data(tmp, ext = "csv")
#'
#' @export
read_data <- function(path, ext, sheet = NULL, delim = "\t") {
  ext <- tolower(ext)
  if (ext %in% c("xlsx", "xls")) {
    if (is.null(sheet) || identical(sheet, "")) {
      readxl::read_excel(path)
    } else {
      readxl::read_excel(path, sheet = sheet)
    }
  } else if (ext == "csv") {
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  } else if (ext %in% c("txt", "tsv")) {
    readr::read_delim(path, delim = delim, show_col_types = FALSE, progress = FALSE)
  } else {
    stop("Unsupported file type: ", ext)
  }
}

#' Coerce a two-level grouping variable to 0/1
#'
#' Accepts a logical, character, factor, or numeric vector and maps it to a
#' 0/1 integer outcome suitable for logistic regression and ROC analysis. The
#' vector must have exactly two distinct levels (ignoring `NA`).
#'
#' @param x A vector to coerce. For factors and characters the mapping follows
#'   factor level order; for numeric input the smaller value maps to 0 and the
#'   larger to 1.
#'
#' @return An integer/numeric vector of 0s and 1s (with `NA` preserved).
#'
#' @examples
#' coerce_binary_group(c("case", "control", "case"))
#' coerce_binary_group(c(2, 5, 5, 2))
#'
#' @export
coerce_binary_group <- function(x) {
  if (is.logical(x)) x <- as.integer(x)
  if (is.character(x)) x <- as.factor(x)
  if (is.factor(x)) {
    if (nlevels(x) != 2) stop("Group column must have exactly 2 levels.")
    return(as.numeric(x) - 1)
  }
  if (is.numeric(x) || is.integer(x)) {
    ux <- sort(unique(x[!is.na(x)]))
    if (length(ux) != 2) stop("Group column must have exactly 2 unique values.")
    return(ifelse(x == ux[1], 0L, ifelse(x == ux[2], 1L, NA_integer_)))
  }
  stop("Unsupported group column type.")
}
