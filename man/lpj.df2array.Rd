% File LPJ/man/lpj.df2array.Rd
\name{lpj.df2array}
\alias{lpj.df2array}
\title{return a variable from a data.frame as array}
\description{
  This functions extracts a variable from a data.frame with
  Lon and Lat columns and returns a list with the vectors "Lon",
  "Lat" and the variable as array.
}
\usage{
lpj.df2array(d, cname, invertlat=FALSE)
}
\arguments{
  \item{d}{dataset with columns c("Lon", "Lat") and the variable to display.}
  \item{cname}{name of the variable in the dataset to be extracted.}
  \item{invertlat}{if TRUE, returned array will be sorted with
    descending latitudes}
}
\value{
  list with the elements "Lon", "Lat" and <cname>
}
\author{
  Joerg Steinkamp <joergsteinkamp@yahoo.de>
}
