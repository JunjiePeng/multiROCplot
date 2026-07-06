#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data
## usethis namespace: end
NULL

# The following packages are used by the bundled Shiny application
# (inst/shiny-app/app.R) rather than by the R/ code directly. They are declared
# here so they are recognised as package dependencies by R CMD check.
#' @importFrom bslib bs_theme
#' @importFrom tools file_ext
#' @importFrom zip zip
NULL

# Silence R CMD check notes for variables used in non-standard evaluation
# (ggplot2 aes() and facet formulas) that cannot use the .data pronoun.
utils::globalVariables(c("variable", "value", "group", "y", "p_label"))
