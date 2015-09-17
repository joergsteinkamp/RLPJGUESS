% File LPJ/man/lpj.daylength.Rd
\name{lpj.daylength}
\alias{lpj.daylength}
\title{Daylength calculation in hours}
\description{
  Returns the length of a range of days (as day of year) for a given
  range of latitudes
}
\usage{
lpj.daylength(lat, doy, leap)
}
\arguments{
  \item{lat}{a single of a set of latitudes}
  \item{doy}{a single of a set of day of a year}
  \item{leap}{is the year a leap year (default FALSE)}
}
\value{
  a matrix of potential sunshine duration per latitude doy combination.
}
\author{
  Joerg Steinkamp <joergsteinkamp@yahoo.de>
}
