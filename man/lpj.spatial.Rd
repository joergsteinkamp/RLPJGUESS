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
lpj.spatial(infile, lon.extent, lat.extent, time.extent, use.time)
}
\arguments{
  \item{infile}{file name to be read (required).}
  \item{lon.extent}{min and max values are used for west-east extent
    (default -180 to 180).}
  \item{lat.extent}{max and min values are used for north-south extent
    (default 90 to -90).}
  \item{time.extent}{over which years should be averaged
    (regular grids only, default FALSE).}
  \item{use.time}{boolean, if 'time.extent' contains the years compared
    directly to the Year column (TRUE) or the index of all sorted Years
    (FALSE), default.}
}
\value{
  Data frame with several named columns c("Lon", "Lat")
  and according to input file.
}
\author{
  Joerg Steinkamp <joergsteinkamp@yahoo.de>
}
