# app.R
# Deployment entry point for the multiROCplot Shiny app (e.g. shinyapps.io /
# Posit Connect). This file intentionally lives at the repository root because
# some deployment platforms expect an app.R there.
#
# For local or interactive use, prefer:  multiROCplot::run_app()
#
# The analysis logic lives in the package (R/); this launcher just serves the
# bundled UI. It is excluded from the built package via .Rbuildignore.

if (!requireNamespace("multiROCplot", quietly = TRUE)) {
  stop(
    "The 'multiROCplot' package must be installed to run this app.\n",
    "Install it with: remotes::install_github('JunjiePeng/multiROCplot')",
    call. = FALSE
  )
}

shiny::shinyAppDir(
  system.file("shiny-app", package = "multiROCplot")
)
