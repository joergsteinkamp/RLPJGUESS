% File LPJ/man/rainfall_seasonality.Rd
\name{rainfall_seasonality}
\alias{rainfall_seasonality}
\title{Calculates the rainfall seasonality based on monthly values}
\description{
  Calculates the rainfall seasonality from monthly values based on
  Markhan (1970).}
\usage{rainfall_seasonality(len, wgt, debug)}
\arguments{
  \item{len}{vector (or array) of monthly precipitation}
  \item{wgt}{vector of same length as "len", or last dimension of "len"
    if "len" is an array. By default number of days per month.}
  \item{debug}{boolean, default FALSE. If TRUE exended information is
    returned.}
}
\value{numeric value or array of rainfall seasonality with one dimension
  less than "len".}
\author{Joerg Steinkamp <joergsteinkamp@yahoo.de>}
