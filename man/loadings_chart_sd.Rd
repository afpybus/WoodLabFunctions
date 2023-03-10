% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loadings_chart.R
\name{loadings_chart_sd}
\alias{loadings_chart_sd}
\title{Loadings chart with sd error bars for multivariate analyses (PCA,PLS)}
\usage{
loadings_chart_sd(
  loadings,
  sd,
  component_str,
  lim = 1,
  cex = 1,
  col = "lightblue"
)
}
\arguments{
\item{loadings}{loadings variable (e.g. P1 or P2 output from rotate_opls)
Set rownames() for analyte labels}

\item{sd}{standard deviations variable (e.g. sd_P1_LOOCV or sd_P2_LOOCV output from rotate_opls_LOOCV)}

\item{component_str}{string with name of component (e.g. "LV1" or "LV2")}

\item{lim}{default 1; axis limit}

\item{cex}{numeric scaling factor for labels}

\item{col}{default "lightblue"; set for bar colors in barplot()}
}
\value{
A ggplot object of the loadings chart in ascending order with sd error bars
}
\description{
Used to create loadings bar plots with sd error bars sorted by value and scaled by diving by max value.
}
