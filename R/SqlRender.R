# @file SqlRender.R
#
# Copyright 2014 Observational Health Data Sciences and Informatics
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
NULL

#' Render a SQL file
#'
#' @description
#' \code{renderSqlFile} Renders SQL code in a file based on parameterized SQL and parameter values, and writes it to another file.
#'
#' @details
#' This function takes parameterized SQL and a list of parameter values and renders the SQL that can be 
#' send to the server. Parameterization syntax:
#' \describe{
#'   \item{@@parameterName}{Parameters are indicated using a @@ prefix, and are replaced with the actual
#'   values provided in the renderSql call.}
#'   \item{\{DEFAULT @@parameterName = parameterValue\}}{Default values for parameters can be defined using 
#'   curly and the DEFAULT keyword.}
#'   \item{\{if\}?\{then\}:\{else\}}{The if-then-else pattern is used to turn on or off blocks of SQL code.}
#' }
#' 
#' 
#' @param sourceFile               The source SQL file
#' @param targetFile               The target SQL file
#' @param ...                   Parameter values

#' @examples
#' renderSqlFile("myParamStatement.sql","myRenderedStatement.sql",a="myTable")
#' @export
renderSqlFile <- function(sourceFile, targetFile, ...) {
  sql <- readChar(sourceFile, file.info(sourceFile)$size)  
  sql <- renderSql(sql,...)$sql
  sink(targetFile)
  cat(sql)
  sink()
}

