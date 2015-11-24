lpj.df2array <- function(d, cname) {
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
