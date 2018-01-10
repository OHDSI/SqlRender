# @file RenderSql
#
# Copyright 2018 Observational Health Data Sciences and Informatics
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

#' SqlRender
#'
#' @docType package
#' @name SqlRender
#' @importFrom utils install.packages menu
NULL

.onLoad <- function(libname, pkgname) {
  rJava::.jpackage(pkgname, lib.loc = libname)
}

#' @title
#' renderSql
#'
#' @description
#' \code{renderSql} Renders SQL code based on parameterized SQL and parameter values.
#'
#' @details
#' This function takes parameterized SQL and a list of parameter values and renders the SQL that can
#' be send to the server. Parameterization syntax: \describe{ \item{@@parameterName}{Parameters are
#' indicated using a @@ prefix, and are replaced with the actual values provided in the renderSql
#' call.} \item{\{DEFAULT @@parameterName = parameterValue\}}{Default values for parameters can be
#' defined using curly and the DEFAULT keyword.} \item{\{if\}?\{then\}:\{else\}}{The if-then-else
#' pattern is used to turn on or off blocks of SQL code.} }
#'
#'
#' @param sql   The parameterized SQL
#' @param ...   Parameter values
#' @return
#' A list containing the following elements: \describe{ \item{parameterizedSql}{The original
#' parameterized SQL code} \item{sql}{The rendered sql} }
#' @examples
#' renderSql("SELECT * FROM @@a;", a = "myTable")
#' renderSql("SELECT * FROM @@a {@@b}?{WHERE x = 1};", a = "myTable", b = "true")
#' renderSql("SELECT * FROM @@a {@@b == ''}?{WHERE x = 1}:{ORDER BY x};", a = "myTable", b = "true")
#' renderSql("SELECT * FROM @@a {@@b != ''}?{WHERE @@b = 1};", a = "myTable", b = "y")
#' renderSql("SELECT * FROM @@a {1 IN (@@c)}?{WHERE @@b = 1};",
#'           a = "myTable",
#'           b = "y",
#'           c = c(1, 2, 3, 4))
#' renderSql("{DEFAULT @@b = \"someField\"}SELECT * FROM @@a {@@b != ''}?{WHERE @@b = 1};",
#'           a = "myTable")
#' renderSql("SELECT * FROM @@a {@@a == 'myTable' & @@b != 'x'}?{WHERE @@b = 1};",
#'           a = "myTable",
#'           b = "y")
#' @import rJava
#' @export
renderSql <- function(sql = "", ...) {
  parameters <- lapply(list(...), function(x) {
    paste(x, collapse = ",")
  })
  messages <- rJava::J("org.ohdsi.sql.SqlRender")$check(sql, rJava::.jarray(names(parameters)), rJava::.jarray(as.character(parameters)))
  for (message in messages) {
    warning(message)
  }
  translatedSql <- rJava::J("org.ohdsi.sql.SqlRender")$renderSql(sql, rJava::.jarray(names(parameters)), rJava::.jarray(as.character(parameters)))
  list(originalSql = sql, sql = translatedSql, parameters = parameters)
}


#' @title
#' translateSql
#'
#' @description
#' \code{translateSql} translates SQL from one dialect to another
#'
#' @details
#' This function takes SQL in one dialect and translates it into another. It uses simple pattern
#' replacement, so its functionality is limited.
#'
#' @param sql                The SQL to be translated
#' @param sourceDialect      Deprecated: The source dialect. Currently, only "sql server" for Microsoft SQL Server
#'                           is supported
#' @param targetDialect      The target dialect. Currently "oracle", "postgresql", "pdw", "impala", "netezza", "bigquery", and
#'                           "redshift" are supported
#' @param oracleTempSchema   A schema that can be used to create temp tables in when using Oracle or Impala.
#' @return
#' A list containing the following elements: \describe{ \item{originalSql}{The original parameterized
#' SQL code} \item{sql}{The translated SQL} }
#' @examples
#' translateSql("USE my_schema;", targetDialect = "oracle")
#' 
#' @export
translateSql <- function(sql = "",
                         targetDialect,
                         oracleTempSchema = NULL,
                         sourceDialect) {
  if (!missing(sourceDialect))
    warning("sourceDialect argument is deprecated in the translateSql function in SqlRender. Please update your code")
  pathToReplacementPatterns <- system.file("csv", "replacementPatterns.csv", package = "SqlRender")
  if (missing(oracleTempSchema) || is.null(oracleTempSchema))
    oracleTempSchema <- rJava::.jnull()
  messages <- rJava::J("org.ohdsi.sql.SqlTranslate")$check(sql, targetDialect)
  for (message in messages) {
    warning(message)
  }
  translatedSql <- rJava::J("org.ohdsi.sql.SqlTranslate")$translateSqlWithPath(sql, targetDialect, rJava::.jnull(), oracleTempSchema, pathToReplacementPatterns)
  list(originalSql = sql, sql = translatedSql)
}

#' @title
#' splitSql
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
#'
#' @export
splitSql <- function(sql) {
  rJava::J("org.ohdsi.sql.SqlSplit")$splitSql(sql)
}
