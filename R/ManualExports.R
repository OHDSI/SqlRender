#' @title renderSql
#'
#' @description
#' \code{renderSql} Renders SQL code based on parameterized SQL and parameter values.
#'
#' @details
#' This function takes parameterized SQL and a list of parameter values and renders the SQL that can be 
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