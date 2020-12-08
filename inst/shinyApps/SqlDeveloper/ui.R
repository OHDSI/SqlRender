library(shiny)
library(shinydashboard)
source("widgets.R")

dashboardPage(
  dashboardHeader(title = "SqlRender Developer"),
  dashboardSidebar(
    sidebarMenu(
      menuItemFileInput("open", "Open file"),
      menuItemDownloadLink("save", "Save"),
      menuItem("Open new tab", href = "", icon = shiny::icon("plus-square")),
      menuItemCopyTextAreaToClipboard("source", "Copy source to clipboard"),
      menuItemCopyDivToClipboard("target", "Copy target to clipboard")
    )
  ),
  dashboardBody(
    fluidRow(
      column(width = 9, 
             box(
               title = "Source: OHDSI SQL", width = NULL, status = "primary",
               textAreaInput("source", NULL, width = "100%", height = "300px")
             ), 
             box(
               title = "Target: Rendered translation", width = NULL,
               # tags$table(width = "100%",
               #            tags$tr(
               #              tags$td(align = "left", actionButton("renderTranslate", "Render and translate")),
               #              tags$td(align = "right", checkboxInput("continuous", "Auto render and translate")))),
               pre(textOutput("target"))
             )
      ),
      column(width = 3,
             box(background = "light-blue",
                 h4("Target dialect"), width = NULL,
                 selectInput("dialect", NULL, choices = c("BigQuery", "Impala", "Netezza", "Oracle", "PDW", "PostgreSQL", "RedShift", "SQL Server", "SQLite", "Hive"), selected = "SQL Server"),
                 h4("Temp emulation schema"),
                 textInput("tempEmulationSchema", NULL),
                 h4("Parameters"),
                 uiOutput("parameterInputs"),
                 textOutput("warnings")
             )
      )
    )
  )
)
