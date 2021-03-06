% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/downloader.R
\name{create_nomads_nc}
\alias{create_nomads_nc}
\title{Download and format NWM data into a timesubsetable NetCDF}
\usage{
create_nomads_nc(type = "short_range", num = 10000, ensemble = NULL,
  dstfile = NULL)
}
\arguments{
\item{type}{a NWM configuration}

\item{num}{the number of files to get}

\item{ensemble}{the ensemble number (medium and long range)}

\item{dstfile}{the location of the output file}
}
\value{
a file path
}
\description{
Download and format NWM data into a timesubsetable NetCDF
}
