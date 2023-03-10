% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scatter_reg.R
\name{LinearRangePanel}
\alias{LinearRangePanel}
\title{Linear Range Panel}
\usage{
LinearRangePanel(analytes, loadings, xlab = "Loading", xintercept = NA)
}
\arguments{
\item{analytes}{data frame or matrix of analytes measured}

\item{loadings}{numeric variable of loadings in same order as analytes matrix}

\item{xlab}{default "Loading"; character to change x axis label}

\item{xintercept}{default is NA; change to numeric to show vertical line at ideal loading}
}
\value{
A panel of linear range scatter graphs
}
\description{
Create a panel of linear range graphs from a single data frame
}
