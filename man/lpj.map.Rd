% File LPJ/man/plot.map.Rd
\name{lpj.map}
\alias{lpj.map}
\title{plot a map using ggplot2}
\description{
  Create a map of one variable in a dataset.
}
\usage{
lpj.map(d, variable=NA, cols=NA, title=NA, sym.col=FALSE, wrap.variable=NA, wrap=1)
}
\arguments{
  \item{d}{dataset with columns c("Lon", "Lat") and the variable to display.}
  \item{variable}{name of the variable in the dataset to be plotted. If%
    it is set to NA (default), column "value" must be present.}
  \item{cols}{data frame of with columns c("colour", "value"), to be%
    used for the colourbar.}
  \item{title}{title for the plot.}
  \item{sym.col}{boolean, if the colours should be distributed%
    symmentric around zero.}
  \item{wrap.variable}{column name used for facet_wrap}
  \item{wrap}{Number of columns for facet_wrap. Only used when
    wrap.variable is set.}
}
\value{
  ggplot2 list object, which can be plotted with print(p)
}
\author{
  Joerg Steinkamp <joergsteinkamp@yahoo.de>
}
