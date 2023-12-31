% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flplot_land.R
\name{flplot_land}
\alias{flplot_land}
\title{Plot land data found within NetCDF files downloaded by \code{FjordLight}.}
\usage{
flplot_land(r, name)
}
\arguments{
\item{r}{The land data in a raster format.}

\item{name}{The name of the chosen fjord.}
}
\value{
A base R plot of the land data will be provided.
}
\description{
Internal function used by \code{\link{flget_bathymetry}} that plots the land data
contained within the NetCDF files downloaded via \code{\link{fl_DownloadFjord}}.
}
\author{
Bernard Gentili
}
\keyword{internal}
