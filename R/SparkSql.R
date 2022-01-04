#' @title
#' Handles Spark Inserts
#'
#' @description
#' This function is for Spark connections only,
#' it handles insert commands, as Spark cannot handle inserts with aliased or subset columns
#'
#' @param sql                The SQL to be translated
#' @param connection         The connection to the database server.
#'
#' @return
#' A sql string with INSERT command modified to contain the full column list, padded with NULLS as needed. 
#'
#' @export
sparkHandleInsert <- function(sql, connection) {
  sql <- rJava::J("org.ohdsi.sql.BigQuerySparkTranslate")$sparkHandleInsert(as.character(sql), connection@jConnection)
  return(sql)
}