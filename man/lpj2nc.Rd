% File LPJ/man/rfs.Rd
\name{lpj2nc}
\alias{lpj2nc}
\title{Saves a LPJ-GUESS data.frame in a NetCDF file}
\description{
  Saves annual or monthly LPJ-GUESS output in a NetCDF file. Needs the
  library RNetCDF to be installed, which is not included in the
  dependencies.
}
\usage{
lpj2nc(df, file, descr, overwrite, as.flux, scale)
}
\arguments{
  \item{df}{the data.frame object to save}
  \item{file}{file name, default "test.nc"}
  \item{descr}{A 3 element string vector for variable attributes:
    c("name", "long_name", "units")}
  \item{overwrite}{if an existing file should be overwritten or
    appended}
  \item{as.flux}{if set to true, data is divided by the number of days
    per month for monthly data or by 365 for annual data. Default:
    FALSE}
  \item{scale}{additional multiplication factor for the data, e.g. or
    unit conversion.}
}
\value{
  boolean
}
\author{
  Joerg Steinkamp <joergsteinkamp@yahoo.de>
}
