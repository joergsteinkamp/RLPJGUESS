% File LPJ/man/lpj.radiation.Rd
\name{lpj.radiation}
\alias{lpj.radiation}
\title{Daily radiation calculation in W m^-1}
\description{
  Returns the incoming solar (shortwave) radiation per day.
  Adopted from the C++ function daylengthinsoleet (driver.cpp) in LPJ-GUESS v2.1
}
\usage{
lpj.radiation(lat, doy, frac, albedo, percent, sun, leap)
}
\arguments{
  \item{lat}{a single of a set of latitudes}
  \item{doy}{a single of a set of day of a year}
  \item{frac}{a fraction, either between 0 and 1 or as percent of cloud
    cover or sunshine}
  \item{albedo}{an albedo value (default 0.17)}
  \item{percent}{is <frac> in percent or not (default FALSE)}
  \item{sun}{sunshine fraction or fractional cloud coverage (default)}
  \item{leap}{is the year a leap year (default FALSE)}
}
\value{
  a matrix of potential sunshine duration per latitude doy combination.
}
\author{
  Joerg Steinkamp <joergsteinkamp@yahoo.de>
}
