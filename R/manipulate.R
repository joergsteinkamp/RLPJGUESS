lpj.df2array <- function(d, cname) {
  lon  <- sort(unique(d$Lon))
  rlon <- min(lon[2:length(lon)] - lon[1:(length(lon)-1)])
  lon  <- seq(min(lon), max(lon), rlon)
  lat  <- sort(unique(d$Lat))
  rlat <- min(lat[2:length(lat)] - lat[1:(length(lat)-1)])
  lat  <- seq(min(lat), max(lat), rlat)

  out <- array(NA, c(length(lon), length(lat)))

  for (i in 1:nrow(d)) {
    out[(d$Lon[i] - min(lon))/rlon + 1, (d$Lat[i] - min(lat))/rlat + 1] =
        eval(parse(text=paste('d$', cname, '[i]', sep="")))
  }
  rv <- eval(parse(text=paste("list(Lon=lon, Lat=lat, ", cname, "=out)", sep="")))
  return(rv)
}
