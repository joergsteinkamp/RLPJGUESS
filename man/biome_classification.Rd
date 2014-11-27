% File LPJ/man/biome_classification.Rd
\name{biome_classification}
\alias{biome_classification}
\title{Create a biome classification}
\description{
  Create a biome classification based on leaf area index (LAI) of global
  plant functional types (PFT) as described in Smith et el. (2014).
}
\usage{
biome_classification(lai)
}
\arguments{
  \item{lai}{Data frame containing columns c("Lon", "Lat") and LAI
    values of PFTs in [m^2 m^-2]}
}
\value{
  Data frame with columns c("Lon", "Lat", "name")
}
\details{
  Based on the different global PFT definition (boreal, temperate,
  tropical trees and grasses) and their LAI distribution different
  biomes are defined.
}
\author{
  Joerg Steinkamp <joergsteinkamp@yahoo.de>
}
\references{
Smith, B., Warlind, D., Arneth, A., Hickler, T., Leadley, P., Siltberg, J. and Zaehle, S.: Implications of incorporating N cycling and N limitations on primary production in an individual-based dynamic vegetation model, Biogeosciences, 11(7), 2027-2054, doi:10.5194/bg-11-2027-2014, 2014.
}
