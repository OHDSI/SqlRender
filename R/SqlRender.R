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

#' @examples \dontrun{
#' renderSqlFile("myParamStatement.sql","myRenderedStatement.sql",a="myTable")
#' }
#' @export
renderSqlFile <- function(sourceFile, targetFile, ...) {
  sql <- readChar(sourceFile, file.info(sourceFile)$size)  
  sql <- renderSql(sql,...)$sql
  sink(targetFile)
  cat(sql)
  sink()
}

#' Load, render, and translate a SQL file in a package
#'
#' @description
#' \code{loadRenderTranslateSql} Loads a SQL file contained in a package, renders it and translates it to the specified dialect
#'
#' @details
#' This function looks for a SQL file with the specified name in the inst/sql/<dbms> folder of the specified package. 
#' If it doesn't find it in that folder, it will try and load the file from the inst/sql/sql_server folder and use the
#' \code{translateSql} function to translate it to the requested dialect. It will subsequently call the \code{renderSql}
#' function with any of the additional specified parameters.
#' 
#' 
#' @param sqlFilename           The source SQL file
#' @param packageName           The name of the package that contains the SQL file
#' @param dbms                  The target dialect. Currently "sql server", "oracle", "postgres", and "redshift" are supported
#' @param ...                   Parameter values used for \code{renderSql}

#' @examples \dontrun{
#'   renderedSql <- loadRenderTranslateSql("CohortMethod.sql",packageName = "CohortMethod",dbms = connectionDetails$dbms,CDM_schema = "cdmSchema")
#' }
#' @export
loadRenderTranslateSql <- function(sqlFilename, packageName, dbms="sql server", ...){
  pathToSql <- system.file(paste("sql/",gsub(" ","_",dbms),sep=""), sqlFilename, package=packageName)
  mustTranslate <- !file.exists(pathToSql)
  if (mustTranslate) # If DBMS-specific code does not exists, load SQL Server code and translate after rendering
    pathToSql <- system.file(paste("sql/","sql_server",sep=""), sqlFilename, package=packageName)      
  parameterizedSql <- readChar(pathToSql,file.info(pathToSql)$size)  
  
  renderedSql <- renderSql(parameterizedSql[1], ...)$sql
  
  if (mustTranslate)
    renderedSql <- translateSql(renderedSql, "sql server", dbms)$sql
  
  renderedSql
}

