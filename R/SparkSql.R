# Copyright 2024 Observational Health Data Sciences and Informatics
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

#' @title
#' Handles Spark Inserts
#'
#' @description
#' This function is for Spark connections only,
#' it handles insert commands, as Spark cannot handle inserts with aliased or subset columns.
#'
#' @param sql                The SQL to be translated.
#' @param connection         The connection to the database server.
#'
#' @return
#' A sql string with INSERT command modified to contain the full column list, padded with NULLS as needed.
#'
#' @export
sparkHandleInsert <- function(sql, connection) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(sql, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  sql <- rJava::J("org.ohdsi.sql.BigQuerySparkTranslate")$sparkHandleInsert(as.character(sql), connection@jConnection)
  return(sql)
}
