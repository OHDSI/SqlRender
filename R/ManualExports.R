# @file ManualExports.R
#
# Copyright 2014 Observational Health Data Sciences and Informatics
#
# This file is part of SQLRender
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

#' @title renderSql
#'
#' @description
#' \code{renderSql} Renders SQL code based on parameterized SQL and parameter values.
#'
#' @details
#' This function takes parameterized SQL and a list of paramter values and renders the SQL that can be 
#' send to the server.
#' 
#' @param str               The parameterized SQL
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
#' renderSql("{DEFAULT @@b = \"someField\"}SELECT * FROM @@a {@@b != ''}?{WHERE @@b = 1};",a="myTable")
#' @export
renderSql <- function(sql = "", ...) {
  .Call('SQLRender_renderSqlInteral', PACKAGE = 'SQLRender', sql, list(...))
}