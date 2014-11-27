% File LPJ/man/extract.seq.Rd
\name{extract.seq}
\alias{extract.seq}
\title{Extract a regular sequence from unique values in a vector.}
\description{
  Extract a regular sequence from unique sorted values in an vector.
}
\usage{
extract.seq(x, force.regular)
}
\arguments{
  \item{x}{vector}
  \item{force.regular}{force regular sequence of smallest distance.}
}
\value{
  vector with sorted unique sequence from min(x) to max(x) with
  the minimum distance as step.
}
\author{
  Joerg Steinkamp <joergsteinkamp@yahoo.de>
}
