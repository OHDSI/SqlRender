library(shiny)
library(SqlRender)

shinyServer(function(input, output, session) {
  
  # cache <- reactiveValues(target = "", 
  #                         clicks = 0,
  #                         parameters = NULL)
  
  parameters <- reactive({
    params <- regmatches(input$source, gregexpr("@[a-zA-Z0-9_]+", input$source))[[1]]
    params <- unique(params)
    params <- params[order(params)]
    params <- substr(params, 2, nchar(params))
    return(params)
  })
  
  output$target <- renderText({
    # if (!input$continuous && (input$renderTranslate == cache$clicks)) {
    #   return(cache$target)
    # } else {
      # print(paste("continuous: ", input$continuous, ", renderTranslate: ", input$renderTranslate))
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
      }
      oracleTempSchema <- input$oracleTempSchema
      if (oracleTempSchema == "")
        oracleTempSchema <- NULL
      sql <- withCallingHandlers(suppressWarnings(translateSql(sql, targetDialect = tolower(input$dialect), oracleTempSchema = oracleTempSchema)$sql), warning = handleWarning)
      if (!is.null(warningString))
        output$warnings <- warningString
      # cache$target <- sql
      # cache$clicks <- input$renderTranslate
      return(sql)
    # }
  })
  
  output$parameterInputs <- renderUI({
    params <- parameters()
    sourceSql <- input$source
    
    createRow <- function(param, sourceSql) {
      # Get current values if already exists:
      value <- isolate(input[[param]])
      
      if (is.null(value)) {
        # Get default values:
        value <- regmatches(sourceSql, regexpr(paste0("\\{\\s*DEFAULT\\s*@", param, "\\s=[^}]+}"), sourceSql))
        if (length(value) == 1) {
          value = sub(paste0("\\{\\s*DEFAULT\\s*@", param, "\\s=\\s*"), "", sub("\\}$", "", value)) 
        } else {
          value = ""
        }
      }
      textInput(param, param, value = value)
    }
    lapply(params, createRow, sourceSql = sourceSql)
  })
  
  observeEvent(input$open, {
    sql <- SqlRender::readSql(input$open$datapath)
    updateTextAreaInput(session, "source", value = sql)
  })
  
  output$save <- downloadHandler(
    filename = function() {
      paste('query-', Sys.Date(), '.sql', sep='')
    },
    content = function(con) {
      SqlRender::writeSql(sql = input$source, targetFile = con)
    }
  )
})

