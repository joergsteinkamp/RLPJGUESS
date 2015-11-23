lpj.df2array <- function(d, cname) {
#  lon  <- sort(unique(d$Lon))
#  rlon <- min(lon[2:length(lon)] - lon[1:(length(lon)-1)])
#  lon  <- seq(min(lon), max(lon), rlon)
#  lat  <- sort(unique(d$Lat))
#  rlat <- min(lat[2:length(lat)] - lat[1:(length(lat)-1)])
#  lat  <- seq(min(lat), max(lat), rlat)
  lon <- extract.seq(d$Lon)
  lat <- extract.seq(d$Lat)
  rlon <- lon[2] - lon[1]
  rlat <- lat[2] - lat[1]
  time <- FALSE

  if (any(colnames(d)=="Year")) {
    time <- TRUE
  }

  if (time) {
    rv <- acast(d, Lon ~ Lat ~ Year, value.var=cname)
  } else {
    rv <- acast(d, Lon ~ Lat ~ Year, value.var=cname)
  }
  return(rv)
}

