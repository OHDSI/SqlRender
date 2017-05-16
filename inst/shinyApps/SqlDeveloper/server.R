
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(SqlRender)

shinyServer(function(input, output, session) {
  
  values <- reactiveValues(fileName = "new.sql")

  parameters <- reactive({
    params <- regmatches(input$source, gregexpr("@[a-zA-Z0-9_]+", input$source))[[1]]
    params <- unique(params)
    params <- params[order(params)]
    params <- substr(params, 2, nchar(params))
    return(params)
  })
  
  output$target <- renderText({
    parameterValues <- list()
    for (param in parameters()) {
      value <- input[[param]]
      if (!is.null(value)) {
        parameterValues[[param]] <- value
      }
    }
    
    sql <- do.call("renderSql", append(input$source, parameterValues))$sql
    warningString <- c()
    handleWarning <- function(e) {
      output$warnings <- e$message
      # warningString <- c(warningString, e$message)
    }
    oracleTempSchema <- input$oracleTempSchema
    if (oracleTempSchema == "")
      oracleTempSchema <- NULL
    sql <- withCallingHandlers(suppressWarnings(translateSql(sql, targetDialect = tolower(input$dialect), oracleTempSchema = oracleTempSchema)$sql), warning = handleWarning)
    if (!is.null(warningString))
      output$warnings <- warningString
    return(sql)
  })
  
  output$parameterInputs <- renderUI({
    params <- parameters()
    sourceSql <- input$source
    
    createRow <- function(param, sourceSql) {
      # Get current values if already exists:
      value <- input[[param]]
      
      if (is.null(value)) {
        # Get default values:
        value <- regmatches(sourceSql, regexpr(paste0("\\{\\s*DEFAULT\\s*@", param, "\\s=[^}]+}"), sourceSql))
        if (length(value) == 1) {
          value = sub(paste0("\\{\\s*DEFAULT\\s*@", param, "\\s=\\s*"), "", sub("\\}$", "", value)) 
        } else {
          value = ""
        }
      }
      paste0("<tr><td><b>", param, ":&nbsp;</b></td><td>", textInput(param, NULL, value = value), "</td></tr>")
    }
    rows <- sapply(params, createRow, sourceSql = sourceSql)
    HTML(paste("<table width= 100%>", paste(rows, collapse = "\n"), "</table>"))
  })
  

  observeEvent(input$open, {
    values$fileName <- input$open$name
    sql <- SqlRender::readSql(input$open$datapath)
    updateTextAreaInput(session, "source", value = sql)
  })
  
  output$fileName <- renderText({
    print(values$fileName)
    return(basename(values$fileName))
  })
  
  observeEvent(input$save, {
    SqlRender::writeSql(sql = input$source, targetFile = values$fileName)
  })
    
  
  output$saveAs <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      SqlRender::writeSql(sql = "test", targetFile = con)
    }
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      SqlRender::writeSql(sql = input$source, targetFile = con)
    }
  )
})

