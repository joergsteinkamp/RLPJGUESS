% File LPJ/man/lpj.timeseries.Rd
\name{lpj.timeseries}
\alias{lpj.timeseries}
\title{Time series of LPJ-GUESS output}
\description{
  This function reads the asci output produced with the dynamic global
  vegetation model (DGVM) LPJ-GUESS (\url{http://www.nateko.lu.se/lpj-guess}).
  Returns a time series object if available as zoo otherwise ts. Annual
  data has several named columns, whereas monthly data has no columns.
}
\usage{
lpj.timeseries(infile, lon.extent, lat.extent, area.weighted, year.offset)
}
\arguments{
  \item{infile}{file name to be read (required).}
  \item{lon.extent}{min and max values are used for west-east extent
    (default -180 to 180).}
  \item{lat.extent}{max and min values are used for north-south extent
    (default 90 to -90).}
  \item{area.weighted}{boolean, if gridcells should be weighted by their
    size (regular grids only, default FALSE).}
  \item{year.offset}{ value to be added to the 'Year' column in the
    LPJ-GUESS output.}
}
\value{
  Timeseries object ts or zoo.
}
\author{
  Joerg Steinkamp <joergsteinkamp@yahoo.de>
}
