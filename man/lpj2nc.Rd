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
lpj2nc(df, file="test.nc", attr=list(name="values"), overwrite=TRUE,
as.flux=FALSE, scale=1, time.start=NA, time.offset=0,
invertlat=FALSE)
}
\arguments{
  \item{df}{the data.frame object to save}
  \item{file}{file name, default "test.nc"}
  \item{attr}{an attribute list. See details below.}
  \item{overwrite}{if an existing file should be overwritten or
    appended}
  \item{as.flux}{if set to true, data is divided by the number of days
    per month for monthly data or by 365 for annual data. Default:
    FALSE}
  \item{scale}{additional multiplication factor for the data, e.g. or
    unit conversion.}
  \item{time.start}{used for time axis unit attribute. If specified, it
    must be a vector of three elements: c(year, month, day). Otherwise
    the lowest value of the "Year" column is used.}
  \item{time.offset}{Value added to all elements in the time axis.}
  \item{invertlat}{if TRUE latitudes will be saved in descending order.}
}
\value{
  boolean
}
\details{
  attr: an attribute list associated with the variable. "name" is
    mandatory (e.g. "list(name='prec',
          long_name='precipitation',
          standard_name='precipitation_amount',
          '_FillValue'='1.e33'
          units='mm day^-1')")
}
\author{
  Joerg Steinkamp <joergsteinkamp@yahoo.de>
}
