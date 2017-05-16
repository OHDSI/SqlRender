
library(shiny)
library(shinydashboard)

source("widgets.R")

dashboardPage(
  dashboardHeader(title = "SqlRender Developer"),
  dashboardSidebar(
    h4("File name"),
    textOutput("fileName"),
    sidebarMenu(
      sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                        label = "Search..."),
     
      menuItem("File", 
               menuSubItemFileInput("open", "Open"),
               menuSubItemActionButton("save", "Save"),
               menuSubItemDownloadLink("saveAs", "Save as"),
               menuSubItem("Open new tab", href = ""),
               menuSubItemCopyToClipboard("target", "Copy to clipboard")
               
      ),
      downloadLink("downloadData", "Download")
    )
  ),
  dashboardBody(
    tags$head(
      HTML("<script src = \"clipboard.min.js\"></script>"),
      HTML("<link href = \"styles/github.css\" rel=\"stylesheet\" />"),
      HTML("<script src = \"highlight.pack.js\"></script>"),
      HTML("<script>hljs.initHighlightingOnLoad();</script>"),
      HTML("<script type = \"text/javascript\" language = \"javascript\">
           $(document).ready(function() {
           
           $(document).on('shiny:value', function(event) {
           if (event.target.id === 'target') {
           event.target.innerHTML = hljs.highlight('sql', event.value, true).value;
           event.preventDefault();
           }
           });
           
           });
           </script>")
    ),
    fluidRow(
      column(width = 9, 
             box(
               title = "OHDSI SQL", width = NULL, status = "primary",
               textAreaInput("source", NULL, width = "100%", height = "300px")
             ), 
             box(
               title = "Rendered translation", width = NULL,
               pre(textOutput("target"))
             )
      ),
      column(width = 3,
             box(background = "light-blue",
                 h4("Target dialect"), width = NULL,
                 selectInput("dialect", NULL, choices = c("Impala", "Netezza", "Oracle", "PDW", "PostgreSQL", "RedShift", "SQL Server" ), selected = "SQL Server"),
                 h4("Oracle temp schema"),
                 textInput("oracleTempSchema", NULL),
                 h4("Parameters"),
                 uiOutput("parameterInputs"),
                 textOutput("warnings")
             )
      )
    )
  )
)
