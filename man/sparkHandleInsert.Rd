% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SparkSql.R
\name{sparkHandleInsert}
\alias{sparkHandleInsert}
\title{Handles Spark Inserts}
\usage{
sparkHandleInsert(sql, connection)
}
\arguments{
\item{sql}{The SQL to be translated.}

\item{connection}{The connection to the database server.}
}
\value{
A sql string with INSERT command modified to contain the full column list, padded with NULLS as needed.
}
\description{
This function is for Spark connections only,
it handles insert commands, as Spark cannot handle inserts with aliased or subset columns.
}
