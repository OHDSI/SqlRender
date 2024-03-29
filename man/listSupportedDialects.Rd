% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/HelperFunctions.R
\name{listSupportedDialects}
\alias{listSupportedDialects}
\title{List the supported target dialects}
\usage{
listSupportedDialects()
}
\value{
A data frame with two columns. The 'dialect' column contains the abbreviation used in SqlRender, and the
'description' column contains a more human-readable description.
}
\description{
List the target dialects supported by the \code{\link{translate}} function.
}
\examples{
listSupportedDialects()
}
