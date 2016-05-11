## daylength calculation depending on latitude (atomic or vector)
## and day of year (atomic or vector)
## taken from function daylengthinsoleet (driver.cpp) LPJ-GUESS v2.1
lpj.daylength <- function(lat, doy, leap=FALSE) {
  dom <- c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  if (leap) dom[3] = 29
  deg2rad <- pi / 180.0
  hh      <- array(0., c(length(lat), length(doy)))

<<<<<<< HEAD
  d <- -23.4 * deg2rad * cos(2.0 * pi * (doy + 10.5) / sum(dom))
=======
  d <- -23.4 * deg2rad * cos(2.0* pi * (doy + 10.5) / sum(dom))
>>>>>>> d05fdc4c003139ac875a26524b4bcbb31a2c38eb
  u <- sin(lat * deg2rad) %*% t(sin(d))
  v <- cos(lat * deg2rad) %*% t(cos(d))

  hh[u>-v & u<v] = acos(-u[u>-v & u<v] / v[u>-v & u<v]) 
  hh[u<=-v]      = 0.0
  hh[u>=v]       = pi

  ## daylength in hours
<<<<<<< HEAD
  return(24.0 * t(hh) / pi)
=======
  return(24.0 * t(hh) / pi);
>>>>>>> d05fdc4c003139ac875a26524b4bcbb31a2c38eb
}

## incoming net solar radiation (W m^-2) reduced by albedo,
## cloud coverage or sun shine fraction, latitude and
## day of the year as done in function daylengthinsoleet
## (driver.cpp) LPJ-GUESS v2.1
<<<<<<< HEAD
lpj.radiation <- function(lat, doy, rad.frac, albedo=0.17, cloudcover=FALSE, leap=FALSE) {
  dom <- c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  if (leap) dom[3] = 29

  ## convert fraction to percent
  if (max(rad.frac) <= 1)
    rad.frac <- rad.frac*100

  ## Convert to percent sunshine hours if given as cloudcover
  if (cloudcover) {
    sun <- 100. - rad.frac
  } else {
    sun <- rad.frac
  }

  ## print(c(min(sun), max(sun)))

  QOO <- 1360.0
  A   <- 107.0
  B   <- 0.2
  C   <- 0.25
  D   <- 0.5
  K   <- 13750.98708
=======
lpj.radiation <- function(lat, doy, frac, albedo=0.17, percent=FALSE, sun=FALSE, leap=FALSE) {
  dom <- c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  if (leap) dom[3] = 29
  if (!precent)
    frac <- frac*100
  
  if (sun) {
    sun <- frac
  } else {
    sun <- 100. - frac
  }
  
  ## print(c(min(sun), max(sun)))
  
  QOO     <- 1360.0
  A       <- 107.0
  B       <- 0.2
  C       <- 0.25
  D       <- 0.5
  K       <- 13750.98708
  FRADPAR <- 0.5
>>>>>>> d05fdc4c003139ac875a26524b4bcbb31a2c38eb

  deg2rad <- pi / 180.0
  hh      <- array(0., c(length(lat), length(doy)))

  d <- -23.4 * deg2rad * cos(2.0 * pi * (doy + 10.5) / sum(dom))
  u <- sin(lat * deg2rad) %*% t(sin(d))
  v <- cos(lat * deg2rad) %*% t(cos(d))

  hh[u>-v & u<v] = acos(-u[u>-v & u<v] / v[u>-v & u<v]) 
  hh[u<=-v]      = 0.0
  hh[u>=v]       = pi

  u  <- t(u)
  v  <- t(v)
  hh <- t(hh)
<<<<<<< HEAD

=======
  
>>>>>>> d05fdc4c003139ac875a26524b4bcbb31a2c38eb
  qo <- QOO*(1.0 + 2.0 * 0.01675 * cos(2.0 * pi * (doy + 0.5) / sum(dom)))

  w <- (C + D * sun / 100.0) * (1.0 - albedo) * qo
  rs_day <- 2.0 * w * (u * hh + v * sin(hh)) * K
  return(rs_day / 86400.)
}
