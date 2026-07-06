# inst/shiny-app/app.R
# Thin Shiny UI layer for the multiROCplot package.
# All analysis logic lives in the package's exported functions and is called
# here via `multiROCplot::`. Launch with multiROCplot::run_app().

library(shiny)
library(bslib)

`%||%` <- function(a, b) if (is.null(a)) b else a

# ----------------------------
# UI
# ----------------------------

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  tags$style(HTML(".shiny-notification { position: fixed; top: 60px; right: 20px; }")),

  titlePanel("ROC Curve Plotter"),
  h5("(Version 1.2.0)"),
  h5("by Junjie Peng"),
  tags$p("Upload data, select outcome group and variables, run ROC + tests + boxplots, then export results."),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      fileInput(
        "dataFile", "Upload data file",
        accept = c(".xlsx", ".xls", ".csv", ".txt", ".tsv")
      ),
      uiOutput("excelSheetUI"),
      uiOutput("delimUI"),
      hr(),
      uiOutput("groupSelect"),
      uiOutput("varsSelect"),
      helpText("Tip: By default, the first 5 numeric variables are pre-selected. You can add more."),
      hr(),
      radioButtons(
        "testType", "Per-variable statistical test",
        choices = c("t-test" = "t", "Mann-Whitney (Wilcoxon)" = "wilcox"),
        selected = "wilcox"
      ),
      checkboxInput("useFDR", "FDR-adjust p-values", value = TRUE),
      actionButton("run", "Run analysis", class = "btn-primary"),
      hr(),
      h5("Downloads"),
      downloadButton("downloadResults", "Download results (ZIP)"),
      downloadButton("downloadROCPlot", "Download ROC plot"),
      downloadButton("downloadBoxPlot", "Download boxplots")
    ),
    mainPanel(
      width = 8,
      tabsetPanel(
        tabPanel("Preview", tableOutput("dataPreview")),
        tabPanel("ROC", plotOutput("rocPlot", height = 520), tableOutput("rocTable")),
        tabPanel("Stats", tableOutput("statsTable")),
        tabPanel("Boxplots", plotOutput("boxPlot", height = 700))
      )
    )
  )
)

# ----------------------------
# Server
# ----------------------------

server <- function(input, output, session) {
  raw_data <- reactive({
    req(input$dataFile)
    ext <- tools::file_ext(input$dataFile$name)
    delim <- input$txtDelim %||% "\t"
    sheet <- input$excelSheet %||% NULL
    tryCatch(
      multiROCplot::read_data(input$dataFile$datapath, ext = ext, sheet = sheet, delim = delim),
      error = function(e) {
        showNotification(paste0("Error reading file: ", conditionMessage(e)), type = "error")
        NULL
      }
    )
  })

  output$excelSheetUI <- renderUI({
    req(input$dataFile)
    ext <- tolower(tools::file_ext(input$dataFile$name))
    if (!ext %in% c("xlsx", "xls")) return(NULL)
    sheets <- tryCatch(readxl::excel_sheets(input$dataFile$datapath), error = function(e) character(0))
    if (length(sheets) == 0) return(NULL)
    selectInput("excelSheet", "Excel sheet", choices = sheets, selected = sheets[1])
  })

  output$delimUI <- renderUI({
    req(input$dataFile)
    ext <- tolower(tools::file_ext(input$dataFile$name))
    if (!ext %in% c("txt", "tsv")) return(NULL)
    selectInput(
      "txtDelim", "Delimiter (for .txt/.tsv)",
      choices = c("Tab" = "\t", "Comma" = ",", "Semicolon" = ";", "Space" = " "),
      selected = "\t"
    )
  })

  output$dataPreview <- renderTable({
    req(raw_data())
    head(raw_data(), 15)
  })

  output$groupSelect <- renderUI({
    req(raw_data())
    selectInput("groupColumn", "Outcome / group column (binary)",
                choices = names(raw_data()), selected = names(raw_data())[1])
  })

  output$varsSelect <- renderUI({
    req(raw_data(), input$groupColumn)
    cols <- setdiff(names(raw_data()), input$groupColumn)
    num_cols <- cols[vapply(raw_data()[cols], is.numeric, logical(1))]
    default <- head(num_cols, 5)
    selectizeInput(
      "vars", "Predictor variables (select 5 by default; you can add more)",
      choices = cols, selected = default, multiple = TRUE,
      options = list(plugins = list("remove_button"), placeholder = "Select variables...")
    )
  })

  analysis <- eventReactive(input$run, {
    req(raw_data(), input$groupColumn, input$vars)
    df <- raw_data()

    df[[input$groupColumn]] <- tryCatch(
      multiROCplot::coerce_binary_group(df[[input$groupColumn]]),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error")
        NULL
      }
    )
    req(!is.null(df[[input$groupColumn]]))

    vars <- unique(input$vars)
    if (length(vars) == 0) {
      showNotification("Please select at least one variable.", type = "error")
      return(NULL)
    }

    stats_tbl <- multiROCplot::compute_univariate_tests(
      df = df, group_col = input$groupColumn, vars = vars,
      test = input$testType, adjust_fdr = isTRUE(input$useFDR)
    )
    roc_tbl <- multiROCplot::compute_roc_table(df, input$groupColumn, vars)
    roc_plot <- multiROCplot::plot_roc_curves(df, input$groupColumn, vars)
    box_plot <- multiROCplot::plot_group_boxplots(
      df, input$groupColumn, vars, p_tbl = stats_tbl, use_adj = isTRUE(input$useFDR)
    )

    list(df = df, vars = vars, stats_tbl = stats_tbl, roc_tbl = roc_tbl,
         roc_plot = roc_plot, box_plot = box_plot)
  })

  output$statsTable <- renderTable({
    req(analysis())
    analysis()$stats_tbl
  }, digits = 4)

  output$rocTable <- renderTable({
    req(analysis())
    analysis()$roc_tbl
  }, digits = 4)

  output$rocPlot <- renderPlot({
    req(analysis())
    validate(need(!is.null(analysis()$roc_plot), "No ROC plot available (check variable types / missingness)."))
    analysis()$roc_plot
  })

  output$boxPlot <- renderPlot({
    req(analysis())
    analysis()$box_plot
  })

  output$downloadResults <- downloadHandler(
    filename = function() paste0("roc_stats_results_", Sys.Date(), ".zip"),
    content = function(file) {
      req(analysis())
      tmpdir <- tempfile("results_")
      dir.create(tmpdir)
      utils::write.csv(analysis()$roc_tbl, file.path(tmpdir, "roc_results.csv"), row.names = FALSE)
      utils::write.csv(analysis()$stats_tbl, file.path(tmpdir, "stat_tests.csv"), row.names = FALSE)
      if (!is.null(analysis()$roc_plot)) {
        ggplot2::ggsave(file.path(tmpdir, "roc_plot.pdf"), analysis()$roc_plot, width = 8, height = 6)
      }
      ggplot2::ggsave(file.path(tmpdir, "boxplots.pdf"), analysis()$box_plot, width = 11, height = 8)
      oldwd <- getwd()
      on.exit(setwd(oldwd), add = TRUE)
      setwd(tmpdir)
      zip::zip(zipfile = file, files = list.files(tmpdir))
    },
    contentType = "application/zip"
  )

  output$downloadROCPlot <- downloadHandler(
    filename = function() paste0("roc_plot_", Sys.Date(), ".pdf"),
    content = function(file) {
      req(analysis())
      validate(need(!is.null(analysis()$roc_plot), "No ROC plot available."))
      ggplot2::ggsave(file, analysis()$roc_plot, width = 8, height = 6)
    },
    contentType = "application/pdf"
  )

  output$downloadBoxPlot <- downloadHandler(
    filename = function() paste0("boxplots_", Sys.Date(), ".pdf"),
    content = function(file) {
      req(analysis())
      ggplot2::ggsave(file, analysis()$box_plot, width = 11, height = 8)
    },
    contentType = "application/pdf"
  )
}

shinyApp(ui, server)
