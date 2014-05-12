\name{splitSql}
\alias{splitSql}
\title{splitSql}
\usage{
splitSql(sql)
}
\arguments{
  \item{sql}{The SQL string to split into separate
  statements}
}
\value{
A vector of strings, one for each SQL statement
}
\description{
\code{splitSql} splits a string containing multiple SQL
statements into a vector of SQL statements
}
\details{
This function takes is needed because some DBMSs (like
ORACLE) do not accepts multiple SQL statements being send
as one execution.
}
\examples{
splitSql("SELECT * INTO a FROM b; USE x; DROP TABLE c;")
}

