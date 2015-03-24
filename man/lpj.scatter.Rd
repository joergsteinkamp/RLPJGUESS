% File LPJ/man/plot.scatter.Rd
\name{lpj.scatter}
\alias{lpj.scatter}
\title{Scatter plot using ggplot2.}
\description{
  Creates a scatter plot of one variable against another in a data
  frame.
}
\usage{
lpj.scatter(d, x.variable="x", y.variable="y", wrap.variable=NA,
col.variable=NA, cols=NA, alpha=0.7, lines=NA, labels="",
lab.pos="bottomright", title=NA, sym.col=FALSE, equal.axis=FALSE,
wrap=0)
}
\arguments{
  \item{d}{data frame holding the variables}
  \item{x.variable}{variable name in d for the x-axis.}
  \item{y.variable}{variable name in d for the y-axis.}
  \item{wrap.variable}{variable name in d used for wrapping.}
  \item{col.variable}{variable name in d to colour the points according to.}
  \item{alpha}{alpha value for the points}
  \item{cols}{data frame of c("colour", "values") to be used for the colourbar.}
  \item{lines}{Either a vector of which lines to add to the plot (e.g. "1:1",%
    "lm", "gam", "loess", ...) or a data frame with the previous values%
    a "method" column and additional columns like "se", "col", "type",%
    "size" for prettier plots.}
  \item{labels}{vector of which labels should be added (possible: "eq",
    "rsq", "rmse", "me").}
  \item{lab.pos}{position of labels: "topleft", "topright",%
    "bottomright", "bottomleft". If left blank the variables x.pos,%
    y.pos, h.just and v.just, v.diff must be set befor calling the function.}
  \item{title}{vector of plot title, x-axis label and y-axis label.}
  \item{sym.col}{boolean, if the colours should be symmentric arround zero.}
  \item{equal.axis}{boolean, if the axis should be equal.}
  \item{wrap}{Use facet_wrap for wrap.variable with <wrap>%
    columns. Applies only for discrete wrap.variable type.}
}
\value{
  ggplot2 list object, which can be plotted with print(p)
}
\author{
  Joerg Steinkamp <joergsteinkamp@yahoo.de>
}
