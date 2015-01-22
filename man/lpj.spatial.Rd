% File LPJ/man/lpj.spatial.Rd
\name{lpj.spatial}
\alias{lpj.spatial}
\title{data.frame of LPJ output}
\description{
  This function reads the asci output produced with the dynamic global
  vegetation model (DGVM) LPJ-GUESS (\url{http://www.nateko.lu.se/lpj-guess}).
  Returns a data frame object averaged over a given temporal extent.
}
\usage{
lpj.spatial(infile, lon.extent, lat.extent, time.extent, use.time,
na.cond, na.mask)
}
\arguments{
  \item{infile}{file name to be read (required).}
  \item{lon.extent}{min and max values are used for west-east extent.
    Default: c(-180, 180)}
  \item{lat.extent}{max and min values are used for north-south extent.
    Default: c(-90, 90)}
  \item{time.extent}{2 element vector, over which years should be averaged
    (regular grids only, default c(0, 99999)).}
  \item{use.time}{boolean, if 'time.extent' contains the years compared
    directly to the Year column (TRUE) or the index of all sorted Years
    (FALSE). Default: FALSE}
  \item{na.cond}{condition, which values of the data.frame should be set
    to NA before averaging over time (p.e. "<=0"). Can be an array of
    several conditions, that are applied after each other
    (p.e. c("< -1", "> 1"), then only values between -1 and 1 are valid,
    and the others are set to NA). Default: NA}
  \item{na.mask}{boolean, create a mask of 1 for valid and 0 for not finite
    (infinite and NA) before averaging. Will result in values between 0
    and 1. Default: FALSE}
}
\value{
  Data frame with several named columns c("Lon", "Lat")
  and according to input file.
}
\author{
  Joerg Steinkamp <joergsteinkamp@yahoo.de>
}
