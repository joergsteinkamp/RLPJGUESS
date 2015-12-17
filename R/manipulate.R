lpj.df2array <- function(d, cname, invertlat=FALSE) {
  lon <- extract.seq(d$Lon)
  lat <- extract.seq(d$Lat)

  time <- FALSE

  if (any(colnames(d)=="Year")) {
    year <- extract.seq(d$Year)
    time <- TRUE
  }

  full.grid <- data.frame(Lon=rep(lon, length(lat)), Lat=rep(lat, each=length(lon)))
  
  if (time) {
    full.grid <- data.frame(full.grid, Year=rep(year, each=nrow(full.grid)))
    d <- merge(d, full.grid, by=c("Lon", "Lat", "Year"), all=TRUE)
    rv <- acast(d, Lon ~ Lat ~ Year, value.var=cname)
    if (invertlat)
      rv <- rv[,length(lat):1,]
  } else {
    d <- merge(d, full.grid, by=c("Lon", "Lat"), all=TRUE)
    rv <- acast(d, Lon ~ Lat, value.var=cname)
    if (invertlat)
      rv <- rv[,length(lat):1]
  }
  return(rv)
}
