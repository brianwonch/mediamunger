% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/comma_numeric_cols.R
\name{comma_numeric_cols}
\alias{comma_numeric_cols}
\title{Change numeric columns into strings with commas.}
\usage{
comma_numeric_cols(df)
}
\arguments{
\item{df}{input data frame whose numeric columns should be made comma'd strings}
}
\value{
df with numeric columns as pretty strings, using scales::comma.
}
\description{
Change numeric columns into strings with commas.
}
\examples{
comma_numeric_cols(mtcars)
}

