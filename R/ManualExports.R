# @file ManualExports.R
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

.onLoad <- function(libname, pkgname) {
  library(utils)
  pathToReplacementPatterns <- system.file("csv", "replacementPatterns.csv", package="SqlRender")
  patterns <- read.csv(pathToReplacementPatterns)
  replacementPatterns <<- data.frame(From = patterns[,1],To = patterns[,2],Search = gsub("\\\\n","\n",as.character(patterns[,3])),replace = gsub("\\\\n","\n",as.character(patterns[,4])))
}

#' @title renderSql
#'
#' @description
#' \code{renderSql} Renders SQL code based on parameterized SQL and parameter values.
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
#' @param sql               The parameterized SQL
#' @param ...               Parameter values
#' @return              
#' A list containing the following elements:
#' \describe{  
#'   \item{parameterizedSql}{The original parameterized SQL code}
#'   \item{sql}{The rendered sql}
#'   \item{parameters}{The parameters used. These may include defaults set in the parameterized SQL code.}
#' }  
#' @examples
#' renderSql("SELECT * FROM @@a;",a="myTable")
#' renderSql("SELECT * FROM @@a {@@b}?{WHERE x = 1};",a="myTable",b="true")
#' renderSql("SELECT * FROM @@a {@@b == ''}?{WHERE x = 1}:{ORDER BY x};",a="myTable",b="true")
#' renderSql("SELECT * FROM @@a {@@b != ''}?{WHERE @@b = 1};",a="myTable",b="y")
#' renderSql("SELECT * FROM @@a {1 IN (@@c)}?{WHERE @@b = 1};",a="myTable",b="y", c=c(1,2,3,4))
#' renderSql("{DEFAULT @@b = \"someField\"}SELECT * FROM @@a {@@b != ''}?{WHERE @@b = 1};",a="myTable")
#' renderSql("SELECT * FROM @@a {@@a == 'myTable' & @@b != 'x'}?{WHERE @@b = 1};",a="myTable",b="y")
#' @export
renderSql <- function(sql = "", ...) {
  parameters <- lapply(list(...), function(x){paste(x,collapse=',')})
  .Call('SqlRender_renderSqlInternal', PACKAGE = 'SqlRender', sql, parameters)
}




#' @title translateSql
#'
#' @description
#' \code{translateSql} translates SQL from one dialect to another
#'
#' @details
#' This function takes SQL in one dialect and translates it into another. It uses simple pattern replacement, so its 
#' functionality is limited.
#' 
#' @param sql               The SQL to be translated
#' @param sourceDialect     The source dialect. Currently, only "sql server" for Microsoft SQL Server is supported
#' @param targetDialect		The target dialect. Currently "oracle", "postgresql", and "redshift" are supported
#' @return              
#' A list containing the following elements:
#' \describe{  
#'   \item{originalSql}{The original parameterized SQL code}
#'   \item{sql}{The translated SQL}
#' }  
#' @examples \dontrun{
#' translateSql("USE my_schema","sql server", "oracle")
#' }
#' @export
translateSql <- function(sql = "", sourceDialect = "sql server", targetDialect = "oracle") {  
  patterns <- replacementPatterns[replacementPatterns$From == sourceDialect & replacementPatterns$To == targetDialect,c(3,4)]
  translateSqlInternal(sql,patterns)
}



