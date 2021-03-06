% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ZPD_function.r
\name{ZPD}
\alias{ZPD}
\title{Function to define the ZPD using an Epanechnikov kernel
called by home.growth() and school.growth() functions
Home curriculum intensity given by (-rate*x)}
\usage{
ZPD(x, location, radius)
}
\arguments{
\item{x}{level of achievement}

\item{location}{location of the ZPD}

\item{radius}{radius of the ZPD}
}
\description{
Function to define the ZPD using an Epanechnikov kernel
called by home.growth() and school.growth() functions
Home curriculum intensity given by (-rate*x)
}
\examples{
\dontrun{
ZPD(x=.37, radius=.05, location=.4)
}
}
