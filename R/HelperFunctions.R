# @file HelperFunctions.R
#
# Copyright 2025 Observational Health Data Sciences and Informatics
#
# This file is part of SqlRender
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# @author Observational Health Data Sciences and Informatics
# @author Martijn Schuemie
# @author Marc Suchard

#' Reads a SQL file
#'
#' @description
#' \code{readSql} loads SQL from a file
#'
#' @details
#' \code{readSql} loads SQL from a file
#'
#' @param sourceFile   The source SQL file
#'
#' @return
#' Returns a string containing the SQL.
#'
#' @examples
#' \dontrun{
#' readSql("myParamStatement.sql")
#' }
#' @export
readSql <- function(sourceFile) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(sourceFile, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  readChar(sourceFile, file.info(sourceFile)$size)
}

#' Write SQL to a SQL (text) file
#'
#' @description
#' \code{writeSql} writes SQL to a file
#'
#' @details
#' \code{writeSql} writes SQL to a file
#'
#' @param sql          A string containing the sql
#' @param targetFile   The target SQL file
#'
#' @examples
#' \dontrun{
#' sql <- "SELECT * FROM @@table_name"
#' writeSql(sql, "myParamStatement.sql")
#' }
#' @export
writeSql <- function(sql, targetFile) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(sql, len = 1, add = errorMessages)
  checkmate::assertCharacter(targetFile, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  sink(targetFile)
  sql <- gsub(
    "\r",
    "",
    sql
  ) # outputting to file duplicates carriage returns, so remove them beforehand (still got newline)
  cat(sql)
  sink()
}


#' Render a SQL file
#'
#' @description
#' \code{renderSqlFile} Renders SQL code in a file based on parameterized SQL and parameter values,
#' and writes it to another file.
#'
#' @details
#' This function takes parameterized SQL and a list of parameter values and renders the SQL that can
#' be send to the server. Parameterization syntax: \describe{ \item{@@parameterName}{Parameters are
#' indicated using a @@ prefix, and are replaced with the actual values provided in the render call.}
#' \item{\{DEFAULT @@parameterName = parameterValue\}}{Default values for parameters can be defined
#' using curly and the DEFAULT keyword.} \item{\{if\}?\{then\}:\{else\}}{The if-then-else pattern is
#' used to turn on or off blocks of SQL code.} }
#'
#'
#' @param sourceFile                The source SQL file
#' @param targetFile                The target SQL file
#' @param warnOnMissingParameters   Should a warning be raised when parameters provided to this
#'                                  function do not appear in the parameterized SQL that is being
#'                                  rendered? By default, this is TRUE.
#' @param ...                       Parameter values
#'
#' @examples
#' \dontrun{
#' renderSqlFile("myParamStatement.sql", "myRenderedStatement.sql", a = "myTable")
#' }
#' @export
renderSqlFile <- function(sourceFile, targetFile, warnOnMissingParameters = TRUE, ...) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(sourceFile, len = 1, add = errorMessages)
  checkmate::assertCharacter(targetFile, len = 1, add = errorMessages)
  checkmate::assertLogical(warnOnMissingParameters, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  sql <- readSql(sourceFile)
  sql <- render(sql, warnOnMissingParameters, ...)
  writeSql(sql, targetFile)
}

#' Translate a SQL file
#'
#' @description
#' This function takes SQL and translates it to a different dialect.
#'
#' @details
#' This function takes SQL and translates it to a different dialect.
#'
#' @param sourceFile            The source SQL file
#' @param targetFile            The target SQL file
#' @param targetDialect         The target dialect. Currently "oracle", "postgresql", "pdw", "impala",
#'                              "sqlite", "netezza", "bigquery", "snowflake", "synapse", "spark", "redshift"
#'                              and "iris" are supported.
#' @param oracleTempSchema      DEPRECATED: use \code{tempEmulationSchema} instead.
#' @param tempEmulationSchema   Some database platforms like Oracle and Impala do not truly support
#'                              temp tables. To emulate temp tables, provide a schema with write
#'                              privileges where temp tables can be created.
#'
#' @examples
#' \dontrun{
#' translateSqlFile("myRenderedStatement.sql",
#'   "myTranslatedStatement.sql",
#'   targetDialect = "postgresql"
#' )
#' }
#' @export
translateSqlFile <- function(sourceFile,
                             targetFile,
                             targetDialect,
                             tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                             oracleTempSchema = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(sourceFile, len = 1, add = errorMessages)
  checkmate::assertCharacter(targetFile, len = 1, add = errorMessages)
  checkmate::assertCharacter(targetDialect, len = 1, add = errorMessages)
  checkmate::assertChoice(targetDialect, listSupportedDialects()$dialect, add = errorMessages)
  checkmate::assertCharacter(tempEmulationSchema, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(oracleTempSchema, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (!is.null(oracleTempSchema) && oracleTempSchema != "") {
    warn("The 'oracleTempSchema' argument is deprecated. Use 'tempEmulationSchema' instead.",
      .frequency = "regularly",
      .frequency_id = "oracleTempSchema"
    )
    tempEmulationSchema <- oracleTempSchema
  }
  sql <- readSql(sourceFile)
  sql <- translate(
    sql = sql,
    targetDialect = targetDialect,
    tempEmulationSchema = tempEmulationSchema
  )
  writeSql(sql, targetFile)
}

#' Load, render, and translate a SQL file in a package
#'
#' @description
#' \code{loadRenderTranslateSql} Loads a SQL file contained in a package, renders it and translates it
#' to the specified dialect
#'
#' @details
#' This function looks for a SQL file with the specified name in the inst/sql/<dbms> folder of the
#' specified package. If it doesn't find it in that folder, it will try and load the file from the
#' inst/sql or inst/sql/sql_server folder and use the \code{translate} function to translate it to the
#' requested dialect. It will subsequently call the \code{render} function with any of the additional
#' specified parameters.
#'
#'
#' @param sqlFilename               The source SQL file
#' @param packageName               The name of the package that contains the SQL file
#' @param dbms                      The target dialect. Currently 'sql server', 'oracle', 'postgres',
#'                                  and 'redshift' are supported
#' @param ...                       Parameter values used for \code{render}
#' @param oracleTempSchema          DEPRECATED: use \code{tempEmulationSchema} instead.
#' @param tempEmulationSchema       Some database platforms like Oracle and Impala do not truly support
#'                                  temp tables. To emulate temp tables, provide a schema with write
#'                                  privileges where temp tables can be created.
#' @param warnOnMissingParameters   Should a warning be raised when parameters provided to this
#'                                  function do not appear in the parameterized SQL that is being
#'                                  rendered? By default, this is TRUE.
#'
#' @return
#' Returns a string containing the rendered SQL.
#' @examples
#' \dontrun{
#' renderedSql <- loadRenderTranslateSql("CohortMethod.sql",
#'   packageName = "CohortMethod",
#'   dbms = connectionDetails$dbms,
#'   CDM_schema = "cdmSchema"
#' )
#' }
#' @export
loadRenderTranslateSql <- function(sqlFilename,
                                   packageName,
                                   dbms = "sql server",
                                   ...,
                                   tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                   oracleTempSchema = NULL,
                                   warnOnMissingParameters = TRUE) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(sqlFilename, len = 1, add = errorMessages)
  checkmate::assertCharacter(packageName, len = 1, add = errorMessages)
  checkmate::assertCharacter(dbms, len = 1, add = errorMessages)
  checkmate::assertChoice(dbms, listSupportedDialects()$dialect, add = errorMessages)
  checkmate::assertCharacter(tempEmulationSchema, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(oracleTempSchema, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertLogical(warnOnMissingParameters, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (!is.null(oracleTempSchema) && oracleTempSchema != "") {
    rlang::warn("The 'oracleTempSchema' argument is deprecated. Use 'tempEmulationSchema' instead.",
      .frequency = "regularly",
      .frequency_id = "oracleTempSchema"
    )
    tempEmulationSchema <- oracleTempSchema
  }
  pathToSql <- system.file("sql", gsub(" ", "_", dbms), sqlFilename, package = packageName)
  mustTranslate <- pathToSql == ""
  if (mustTranslate) {
    # If DBMS-specific code does not exists, load SQL Server code and translate after rendering
    pathToSql <- system.file("sql", "sql_server", sqlFilename, package = packageName)
    if (pathToSql == "") {
      pathToSql <- system.file("sql", sqlFilename, package = packageName)
    }
    if (pathToSql == "") {
      abort(sprintf(
        "Cannot find '%s' in the 'sql' or 'sql/sql_server' folder of the '%s' package.",
        sqlFilename,
        packageName
      ))
    }
  }
  parameterizedSql <- readChar(pathToSql, file.info(pathToSql)$size)

  renderedSql <- render(
    sql = parameterizedSql[1],
    warnOnMissingParameters = warnOnMissingParameters,
    ...
  )

  if (mustTranslate) {
    renderedSql <- translate(
      sql = renderedSql,
      targetDialect = dbms,
      tempEmulationSchema = tempEmulationSchema
    )
  }

  renderedSql
}

trim <- function(string) {
  gsub("(^ +)|( +$)", "", string)
}

#' Convert a snake case string to camel case
#'
#' @param string   The string to be converted
#'
#' @return
#' A string
#'
#' @examples
#' snakeCaseToCamelCase("exposure_concept_id_1")
#'
#' @export
snakeCaseToCamelCase <- function(string) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(string, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  string <- tolower(string)
  for (letter in letters) {
    string <- gsub(paste("_", letter, sep = ""), toupper(letter), string)
  }
  string <- gsub("_([0-9])", "\\1", string)
  return(string)
}

#' Convert a camel case string to snake case
#'
#' @param string   The string to be converted
#'
#' @return
#' A string
#'
#' @examples
#' camelCaseToSnakeCase("exposureConceptId1")
#'
#' @export
camelCaseToSnakeCase <- function(string) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(string, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  string <- gsub("([A-Z])", "_\\1", string)
  string <- tolower(string)
  string <- gsub("([a-z])([0-9])", "\\1_\\2", string)
  return(string)
}

#' Convert a camel case string to title case
#'
#' @param string   The string to be converted
#'
#' @return
#' A string
#'
#' @examples
#' camelCaseToTitleCase("exposureConceptId1")
#'
#' @export
camelCaseToTitleCase <- function(string) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(string, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  string <- gsub("([A-Z])", " \\1", string)
  string <- gsub("([a-z])([0-9])", "\\1 \\2", string)
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  return(string)
}



#' Convert the names of an object from snake case to camel case
#'
#' @param object   The object of which the names should be converted
#'
#' @return
#' The same object, but with converted names.
#'
#' @examples
#' x <- data.frame(concept_id = 1, concept_name = "b")
#' snakeCaseToCamelCaseNames(x)
#'
#' @export
snakeCaseToCamelCaseNames <- function(object) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertNamed(object, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  names(object) <- snakeCaseToCamelCase(names(object))
  return(object)
}

#' Convert the names of an object from camel case to snake case
#'
#' @param object   The object of which the names should be converted
#'
#' @return
#' The same object, but with converted names.
#'
#' @examples
#' x <- data.frame(conceptId = 1, conceptName = "b")
#' camelCaseToSnakeCaseNames(x)
#'
#' @export
camelCaseToSnakeCaseNames <- function(object) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertNamed(object, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  names(object) <- camelCaseToSnakeCase(names(object))
  return(object)
}


#' Create an R wrapper for SQL
#'
#' @description
#' \code{createRWrapperForSql} creates an R wrapper for a parameterized SQL file. The created R script
#' file will contain a single function, that executes the SQL, and accepts the same parameters as
#' specified in the SQL.
#'
#' @details
#' This function reads the declarations of defaults in the parameterized SQL file, and creates an R
#' function that exposes the parameters. It uses the \code{loadRenderTranslateSql} function, and
#' assumes the SQL will be used inside a package. To use inside a package, the SQL file should be
#' placed in the inst/sql/sql_server folder of the package.
#'
#' @param sqlFilename             The SQL file.
#' @param rFilename               The name of the R file to be generated. Defaults to the name of the
#'                                SQL file with the extension reset to R.
#' @param packageName             The name of the package that will contains the SQL file.
#' @param createRoxygenTemplate   If true, a template of Roxygen comments will be added.
#'
#' @examples
#' \dontrun{
#' # This will create a file called CohortMethod.R:
#' createRWrapperForSql("CohortMethod.sql", packageName = "CohortMethod")
#' }
#' @export
createRWrapperForSql <- function(sqlFilename,
                                 rFilename,
                                 packageName,
                                 createRoxygenTemplate = TRUE) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(sqlFilename, len = 1, add = errorMessages)
  checkmate::assertCharacter(rFilename, len = 1, add = errorMessages)
  checkmate::assertCharacter(packageName, len = 1, add = errorMessages)
  checkmate::assertLogical(createRoxygenTemplate, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (missing(rFilename)) {
    periodIndex <- which(strsplit(sqlFilename, "")[[1]] == ".")
    if (length(periodIndex) == 0) {
      rFilename <- paste(sqlFilename, "R", sep = ".")
    } else {
      periodIndex <- periodIndex[length(periodIndex)] # pick last one
      rFilename <- paste(substr(sqlFilename, 1, periodIndex), "R", sep = "")
    }
  }

  pathToSql <- system.file(paste("sql/", "sql_server", sep = ""),
    sqlFilename,
    package = packageName
  )
  if (file.exists(pathToSql)) {
    inform(paste("Reading SQL from package folder:", pathToSql))
    parameterizedSql <- readSql(pathToSql)
  } else if (file.exists(sqlFilename)) {
    inform(paste("Reading SQL from current folder:", file.path(getwd(), sqlFilename)))
    inform("Note: make sure the SQL file is placed in the /inst/sql/sql_server folder when building the package")
    parameterizedSql <- readSql(sqlFilename)
  } else {
    abort("Could not find SQL file")
  }

  hasTempTables <- any(gregexpr("\\#", parameterizedSql)[[1]] != -1)
  hits <- gregexpr("\\{DEFAULT @[^}]*\\}", parameterizedSql)
  hits <- cbind(hits[[1]], attr(hits[[1]], "match.length"))
  f <- function(x) {
    x <- substr(parameterizedSql, x[1], x[1] + x[2])
    start <- regexpr("@", x) + 1
    equalSign <- regexpr("=", x)
    end <- regexpr("\\}", x) - 1
    parameter <- trim(substr(x, start, equalSign - 1))
    value <- trim(substr(x, equalSign + 1, end))
    if (grepl(",", value) & substr(value, 1, 1) != "'") {
      value <- paste("c(", value, ")", sep = "")
    }
    if (substr(value, 1, 1) == "'" & substr(value, nchar(value), nchar(value)) == "'") {
      value <- paste("\"", substr(value, 2, nchar(value) - 1), "\"", sep = "")
    }
    ccParameter <- snakeCaseToCamelCase(parameter)
    c(sqlParameter = parameter, rParameter = ccParameter, value = value)
  }
  definitions <- as.data.frame(t(apply(hits, 1, FUN = f)))

  lines <- c()
  if (createRoxygenTemplate) {
    lines <- c(lines, "#' Todo: add title")
    lines <- c(lines, "#'")
    lines <- c(lines, "#' @description")
    lines <- c(lines, "#' Todo: add description")
    lines <- c(lines, "#'")
    lines <- c(lines, "#' @details")
    lines <- c(lines, "#' Todo: add details")
    lines <- c(lines, "#'")
    lines <- c(
      lines,
      "#' @param connectionDetails\t\tAn R object of type \\code{ConnectionDetails} created using the function \\code{createConnectionDetails} in the \\code{DatabaseConnector} package."
    )
    if (hasTempTables) {
      lines <- c(
        lines,
        "#' @param tempEmulationSchema\t\tSome database platforms like Oracle and Impala do not truly support temp tables. To emulate temp tables, provide a schema with write privileges where temp tables can be created."
      )
    }
    for (i in 1:nrow(definitions)) {
      lines <- c(lines, paste("#' @param", definitions$rParameter[i], "\t\t"))
    }
    lines <- c(lines, "#'")
    lines <- c(lines, "#' @export")
  }
  lines <- c(
    lines,
    paste(gsub(".sql", "", sqlFilename), " <- function(connectionDetails,", sep = "")
  )
  if (hasTempTables) {
    lines <- c(
      lines,
      "                         tempEmulationSchema = getOption(\"sqlRenderTempEmulationSchema\"),"
    )
  }
  for (i in 1:nrow(definitions)) {
    if (i == nrow(definitions)) {
      end <- ") {"
    } else {
      end <- ","
    }
    lines <- c(lines, paste("                         ",
      definitions$rParameter[i],
      " = ",
      definitions$value[i],
      end,
      sep = ""
    ))
  }
  lines <- c(
    lines,
    paste("  renderedSql <- SqlRender::loadRenderTranslateSql(\"", sqlFilename, "\",",
      sep = ""
    )
  )
  lines <- c(lines, paste("              packageName = \"", packageName, "\",", sep = ""))
  lines <- c(lines, "              dbms = connectionDetails$dbms,")
  if (hasTempTables) {
    lines <- c(lines, "              tempEmulationSchema = tempEmulationSchema,")
  }
  for (i in 1:nrow(definitions)) {
    if (i == nrow(definitions)) {
      end <- ")"
    } else {
      end <- ","
    }
    lines <- c(lines, paste("              ",
      definitions$sqlParameter[i],
      " = ",
      definitions$rParameter[i],
      end,
      sep = ""
    ))
  }
  lines <- c(lines, "  connection <- DatabaseConnector::connect(connectionDetails)")
  lines <- c(lines, "  on.exit(DatabaseConnector::disconnect(connection))")
  lines <- c(lines, "")
  lines <- c(lines, "  writeLines(\"Executing multiple queries. This could take a while\")")
  lines <- c(lines, "  DatabaseConnector::executeSql(connection,renderedSql)")
  lines <- c(lines, "  writeLines(\"Done\")")
  lines <- c(lines, "")
  lines <- c(lines, "}")
  inform(paste("Writing R function to:", rFilename))
  sink(rFilename)
  cat(paste(lines, collapse = "\n"))
  sink()
}

# Copied from https://github.com/beast-dev/BeastJar/blob/master/R/Utilities.R

#' Determine if Java virtual machine supports Java
#'
#' @description
#' Tests Java virtual machine (JVM) java.version system property to check if version >= 8.
#'
#' @return
#' Returns TRUE if JVM supports Java >= 8.
#'
#' @examples
#' supportsJava8()
#' @export
supportsJava8 <- function() {
  # return(FALSE)
  javaVersionText <- rJava::.jcall("java/lang/System", "S", "getProperty", "java.version")
  majorVersion <- as.integer(regmatches(
    javaVersionText,
    regexpr(pattern = "^\\d+", text = javaVersionText)
  ))
  if (majorVersion == 1) {
    twoDigitVersion <- regmatches(
      javaVersionText,
      regexpr(pattern = "^\\d+\\.\\d+", text = javaVersionText)
    )
    majorVersion <- as.integer(regmatches(
      twoDigitVersion,
      regexpr("\\d+$", text = twoDigitVersion)
    ))
  }
  support <- majorVersion >= 8
  return(support)
}

#' List the supported target dialects
#'
#' @description
#' List the target dialects supported by the \code{\link{translate}} function.
#'
#' @return
#' A data frame with two columns. The 'dialect' column contains the abbreviation used in SqlRender, and the
#' 'description' column contains a more human-readable description.
#'
#' @examples
#' listSupportedDialects()
#' @export
listSupportedDialects <- function() {
  pathToCsv <- system.file("csv", "supportedDialects.csv", package = "SqlRender")
  return(read.csv(pathToCsv, stringsAsFactors = FALSE))
}
