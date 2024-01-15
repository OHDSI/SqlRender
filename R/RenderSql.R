# @file RenderSql
#
# Copyright 2023 Observational Health Data Sciences and Informatics
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

#' @title
#' Render SQL code based on parameterized SQL and parameter values
#'
#' @description
#' \code{render} Renders SQL code based on parameterized SQL and parameter values.
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
#' @param sql                       The parameterized SQL
#' @param warnOnMissingParameters   Should a warning be raised when parameters provided to this
#'                                  function do not appear in the parameterized SQL that is being
#'                                  rendered? By default, this is TRUE.
#' @param ...                       Parameter values
#'
#' @return
#' A character string containing the rendered SQL.
#'
#' @examples
#' render("SELECT * FROM @a;", a = "myTable")
#' render("SELECT * FROM @a {@b}?{WHERE x = 1};", a = "myTable", b = "true")
#' render("SELECT * FROM @a {@b == ''}?{WHERE x = 1}:{ORDER BY x};", a = "myTable", b = "true")
#' render("SELECT * FROM @a {@b != ''}?{WHERE @b = 1};", a = "myTable", b = "y")
#' render("SELECT * FROM @a {1 IN (@c)}?{WHERE @b = 1};",
#'   a = "myTable",
#'   b = "y",
#'   c = c(1, 2, 3, 4)
#' )
#' render("{DEFAULT @b = \"someField\"}SELECT * FROM @a {@b != ''}?{WHERE @b = 1};",
#'   a = "myTable"
#' )
#' render("SELECT * FROM @a {@a == 'myTable' & @b != 'x'}?{WHERE @b = 1};",
#'   a = "myTable",
#'   b = "y"
#' )
#' render(
#'   sql = "SELECT * FROM @a;",
#'   warnOnMissingParameters = FALSE,
#'   a = "myTable",
#'   b = "missingParameter"
#' )
#' @import rJava
#' @export
render <- function(sql, warnOnMissingParameters = TRUE, ...) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(sql, len = 1, add = errorMessages)
  checkmate::assertLogical(warnOnMissingParameters, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (!supportsJava8()) {
    warning("Java 8 or higher is required, but older version was found. ")
    return("")
  }
  parameters <- lapply(list(...), function(x) {
    paste(x, collapse = ",")
  })
  if (warnOnMissingParameters) {
    messages <- rJava::J("org.ohdsi.sql.SqlRender")$check(as.character(sql), rJava::.jarray(names(parameters)), rJava::.jarray(as.character(parameters)))
    for (message in messages) {
      warn(message)
    }
  }
  translatedSql <- rJava::J("org.ohdsi.sql.SqlRender")$renderSql(as.character(sql), rJava::.jarray(names(parameters)), rJava::.jarray(as.character(parameters)))
  attributes(translatedSql) <- attributes(sql)
  return(translatedSql)
}

#' @title
#' Deprecated: Render SQL code based on parameterized SQL and parameter values
#'
#' @description
#' This function has been deprecated. Use \code{\link{render}} instead. This new function returns a
#' character vector instead of a list.
#'
#' @param sql                       The parameterized SQL
#' @param warnOnMissingParameters   Should a warning be raised when parameters provided to this
#'                                  function do not appear in the parameterized SQL that is being
#'                                  rendered? By default, this is TRUE.
#' @param ...                       Parameter values
#' @return
#' A list containing the following elements: \describe{ \item{parameterizedSql}{The original
#' parameterized SQL code} \item{sql}{The rendered sql} }
#' @import rJava
#' @export
renderSql <- function(sql = "", warnOnMissingParameters = TRUE, ...) {
  .Deprecated("render")
  translatedSql <- render(sql, warnOnMissingParameters, ...)
  parameters <- lapply(list(...), function(x) {
    paste(x, collapse = ",")
  })
  return(list(originalSql = sql, sql = translatedSql, parameters = parameters))
}

#' @title
#' Translates SQL from one dialect to another
#'
#' @description
#' \code{translate} translates SQL from one dialect to another.
#'
#' @details
#' This function takes SQL in one dialect and translates it into another. It uses simple pattern
#' replacement, so its functionality is limited. Note that trailing semicolons are not removed for
#' Oracle, which is required before sending a statement through JDBC. This will be done by
#' \code{\link{splitSql}}.
#'
#' @param sql                   The SQL to be translated
#' @param targetDialect         The target dialect. Currently "oracle", "postgresql", "pdw", "impala",
#'                              "sqlite", "sqlite extended", "netezza", "bigquery", "snowflake", "synapse", "spark", and "redshift" are supported.
#'                              Use \code{\link{listSupportedDialects}} to get the list of supported dialects.
#' @param oracleTempSchema      DEPRECATED: use \code{tempEmulationSchema} instead.
#' @param tempEmulationSchema   Some database platforms like Oracle and Impala do not truly support
#'                              temp tables. To emulate temp tables, provide a schema with write
#'                              privileges where temp tables can be created.
#' @return
#' A character string containing the translated SQL.
#'
#' @examples
#' translate("USE my_schema;", targetDialect = "oracle")
#' @export
translate <- function(sql,
                      targetDialect,
                      tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                      oracleTempSchema = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(sql, len = 1, add = errorMessages)
  checkmate::assertCharacter(targetDialect, len = 1, add = errorMessages)
  checkmate::assertChoice(targetDialect, listSupportedDialects()$dialect, add = errorMessages)
  checkmate::assertCharacter(tempEmulationSchema, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(oracleTempSchema, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  
  if (!is.null(attr(sql, "sqlDialect"))) {
    warn("Input SQL has already been translated, so not translating again",
         .frequency = "regularly",
         .frequency_id = "alreadyTranslated")
    return(sql)
  }

  if (!supportsJava8()) {
    warning("Java 8 or higher is required, but older version was found. ")
    return("")
  }
  if (!is.null(oracleTempSchema) && oracleTempSchema != "") {
    warn("The 'oracleTempSchema' argument is deprecated. Use 'tempEmulationSchema' instead.",
      .frequency = "regularly",
      .frequency_id = "oracleTempSchema"
    )
    tempEmulationSchema <- oracleTempSchema
  }
  pathToReplacementPatterns <- system.file("csv", "replacementPatterns.csv", package = "SqlRender")
  if (is.null(tempEmulationSchema)) {
    tempEmulationSchema <- rJava::.jnull()
  } else {
    tempEmulationSchema <- as.character(tempEmulationSchema)
  }
  messages <- rJava::J("org.ohdsi.sql.SqlTranslate")$check(as.character(sql), as.character(targetDialect))
  for (message in messages) {
    warn(message)
  }
  translatedSql <- rJava::J("org.ohdsi.sql.SqlTranslate")$translateSqlWithPath(as.character(sql), as.character(targetDialect), rJava::.jnull(), tempEmulationSchema, as.character(pathToReplacementPatterns))
  attributes(translatedSql) <- attributes(sql)
  attr(translatedSql, "sqlDialect") <-targetDialect
  return(translatedSql)
}

#' @title
#' Deprecated: Translates SQL from one dialect to another
#'
#' @description
#' This function has been deprecated. Use \code{\link{translate}} instead. This new function returns a
#' character vector instead of a list.
#'
#' @param sql                The SQL to be translated
#' @param targetDialect      The target dialect. Currently "oracle", "postgresql", "pdw", "impala",
#'                           "netezza", "bigquery", "snowflake", "synapse", "spark", and "redshift" are supported
#' @param oracleTempSchema   A schema that can be used to create temp tables in when using Oracle or
#'                           Impala.
#'
#' @return
#' A list containing the following elements: \describe{ \item{originalSql}{The original parameterized
#' SQL code} \item{sql}{The translated SQL} }
#'
#' @export
translateSql <- function(sql = "", targetDialect, oracleTempSchema = NULL) {
  .Deprecated("translate")
  translatedSql <- translate(sql, targetDialect, oracleTempSchema = NULL)
  return(list(originalSql = sql, sql = translatedSql))
}

#' @title
#' Translates a single SQL statement from one dialect to another
#'
#' @description
#' \code{translateSingleStatement} translates a single SQL statement from one dialect to another.
#'
#' @details
#' This function takes SQL in one dialect and translates it into another. It uses simple pattern
#' replacement, so its functionality is limited. This removes any trailing semicolon as required by
#' Oracle when sending through JDBC. An error is thrown if more than one statement is encountered in
#' the SQL.
#'
#' @param sql                   The SQL to be translated
#' @param targetDialect         The target dialect. Currently "oracle", "postgresql", "pdw", "impala",
#'                              "sqlite", "sqlite extended", "netezza", "bigquery", "snowflake", "synapse", "spark", and "redshift" are supported.
#' @param oracleTempSchema      DEPRECATED: use \code{tempEmulationSchema} instead.
#' @param tempEmulationSchema   Some database platforms like Oracle and Impala do not truly support
#'                              temp tables. To emulate temp tables, provide a schema with write
#'                              privileges where temp tables can be created.
#' @return
#' A character vector with the translated SQL.
#' @examples
#' translateSingleStatement("USE my_schema;", targetDialect = "oracle")
#' @export
translateSingleStatement <- function(sql = "",
                                     targetDialect,
                                     tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                                     oracleTempSchema = NULL) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(sql, len = 1, add = errorMessages)
  checkmate::assertCharacter(targetDialect, len = 1, add = errorMessages)
  checkmate::assertChoice(targetDialect, listSupportedDialects()$dialect, add = errorMessages)
  checkmate::assertCharacter(tempEmulationSchema, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(oracleTempSchema, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  
  if (!is.null(attr(sql, "sqlDialect"))) {
    warn("Input SQL has already been translated, so not translating again",
         .frequency = "regularly",
         .frequency_id = "alreadyTranslated")
    return(sql)
  }

  if (!supportsJava8()) {
    warning("Java 8 or higher is required, but older version was found. ")
    return("")
  }
  if (!is.null(oracleTempSchema) && oracleTempSchema != "") {
    warn("The 'oracleTempSchema' argument is deprecated. Use 'tempEmulationSchema' instead.",
      .frequency = "regularly",
      .frequency_id = "oracleTempSchema"
    )
    tempEmulationSchema <- oracleTempSchema
  }
  pathToReplacementPatterns <- system.file("csv", "replacementPatterns.csv", package = "SqlRender")
  if (is.null(tempEmulationSchema)) {
    tempEmulationSchema <- rJava::.jnull()
  } else {
    tempEmulationSchema <- as.character(tempEmulationSchema)
  }
  messages <- rJava::J("org.ohdsi.sql.SqlTranslate")$check(as.character(sql), as.character(targetDialect))
  for (message in messages) {
    warn(message)
  }
  translatedSql <- rJava::J("org.ohdsi.sql.SqlTranslate")$translateSingleStatementSqlWithPath(as.character(sql), as.character(targetDialect), rJava::.jnull(), tempEmulationSchema, as.character(pathToReplacementPatterns))
  attributes(translatedSql) <- attributes(sql)
  attr(translatedSql, "sqlDialect") <-targetDialect
  return(translatedSql)
}


#' @title
#' Split a single SQL string into one or more SQL statements
#'
#' @description
#' \code{splitSql} splits a string containing multiple SQL statements into a vector of SQL statements
#'
#' @details
#' This function is needed because some DBMSs (like ORACLE) do not accepts multiple SQL statements
#' being sent as one execution.
#'
#' @param sql   The SQL string to split into separate statements
#' @return
#' A vector of strings, one for each SQL statement
#' @examples
#' splitSql("SELECT * INTO a FROM b; USE x; DROP TABLE c;")
#' @export
splitSql <- function(sql) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(sql, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (!supportsJava8()) {
    warning("Java 8 or higher is required, but older version was found. ")
    return("")
  }
  rJava::J("org.ohdsi.sql.SqlSplit")$splitSql(as.character(sql))
}

#' Get the prefix used for emulated temp tables for DBMSs that do not support temp tables (e.g. Oracle,
#' BigQuery).
#'
#' @examples
#' getTempTablePrefix()
#' @return
#' The prefix string.
#'
#' @export
getTempTablePrefix <- function() {
  if (!supportsJava8()) {
    warning("Java 8 or higher is required, but older version was found. ")
    return("")
  }
  return(rJava::J("org.ohdsi.sql.SqlTranslate")$getGlobalSessionId())
}
