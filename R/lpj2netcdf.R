lpj2nc.dim.save <- function(ncout, name, data, start.year=1901, time.unit=c(year=TRUE, month=FALSE, day=FALSE)) {
  if (sum(time.unit) != 1) {
    message(paste("Wrong unit for time dimension:", paste(names(time.unit), time.unit, collapse=", ")))
    return(FALSE)
  }

  if (tolower(name)=="time") {
    dim.def.nc(ncout, name, unlim=TRUE)
    var.def.nc(ncout, name, "NC_INT", name)
    att.put.nc(ncout, name, "calendar", "NC_CHAR", "365_day")
    if (time.unit["year"]) {
      att.put.nc(ncout, name, "long_name", "NC_CHAR", "years")
      att.put.nc(ncout, name, "units", "NC_CHAR", paste("years since ",start.year,"-01-01 00:00:00",sep=""))
      var.put.nc(ncout, name, data)
    } else if (time.unit["month"]) {
      dpm <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
      att.put.nc(ncout, name, "long_name", "NC_CHAR", "days")
      att.put.nc(ncout, name, "units", "NC_CHAR", paste("days since ",start.year,"-01-01 00:00:00",sep=""))
      data=cumsum(rep(dpm, length(data)/12)) - rep(dpm, length(data)/12)
      var.put.nc(ncout, name, data)
    } else if (time.unit["day"]) {
      att.put.nc(ncout, name, "long_name", "NC_CHAR", "days")
      att.put.nc(ncout, name, "units", "NC_CHAR", paste("days since ",start.year,"-01-01 00:00:00",sep=""))
      var.put.nc(ncout, name, data)
    } else {
      message(paste("Undefined time.unit: ",  paste(names(time.unit), time.unit, collapse=", ")))
      return(FALSE)
    }
  } else {
    dim.def.nc(ncout, name, dimlength=length(data))
    var.def.nc(ncout, name, "NC_FLOAT", name)
    if (tolower(name)=="lon" || tolower(name)=="long" || tolower(name)=="longitude"){
      att.put.nc(ncout, name, "long_name", "NC_CHAR", "longitude")
      att.put.nc(ncout, name, "units", "NC_CHAR", "degrees_east")
    } else if (tolower(name)=="lat" || tolower(name)=="latitude"){
      att.put.nc(ncout, name, "long_name", "NC_CHAR", "latitude")
      att.put.nc(ncout, name, "units", "NC_CHAR", "degrees_north")
    }
    var.put.nc(ncout, name, data)
  }
}

lpj2nc.var.save <- function (ncout, name, dims, descr, data, na.value=1.e20) {
  var.def.nc(ncout, name, "NC_FLOAT", dims)
  att.put.nc(ncout, name, "long_name", "NC_CHAR", descr[1])
  att.put.nc(ncout, name, "units", "NC_CHAR", descr[2])
  att.put.nc(ncout, name, "_FillValue", "NC_FLOAT", na.value)
  var.put.nc(ncout, name, data)
}


lpj2nc <- function(df, file="test.nc", descr=c("name", "long_name", "unit"), overwrite=TRUE, as.flux=FALSE, scale=1.0, na.value=1.e20) {
  if (!require("RNetCDF", quietly=TRUE)) {
    message("RNetCDF library not installed. Exiting function.")
    return(FALSE)
  }

  ## make sure the netcdf variable name is not empty
  if (is.na(descr[1])) {
    descr[1]="name"
  } else if (descr[1] == "") {
    descr[1]="name"
  }
  
  if (colnames(df)[1] != "Lon" && colnames(df)[1] != "Lat" && colnames(df)[1] != "Year") {
    message('Error: First 3 columns must be named "Lon", "Lat", "Year".')
    return(FALSE)
  }
  
  lon        <- seq(min(df$Lon), max(df$Lon), 0.5)
  lat        <- seq(max(df$Lat), min(df$Lat), -0.5)
  time       <- 1:length(unique(df$Year))
  start.year <- min(df$Year)
  
  monthly <- FALSE
  if (any(colnames(df)=="Jan"))
    monthly <- TRUE

  ncout <- create.nc(file, clobber=overwrite, large=TRUE)

  ## TODO: check if dim is alredy defined if not overwrite
  lpj2nc.dim.save(ncout, "lon", lon)
  lpj2nc.dim.save(ncout, "lat", lat)

  if (monthly) {
    dpm <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

    ## TODO: check if dim is alredy defined if not overwrite
    lpj2nc.dim.save(ncout, "time", 1:(length(time)*12),  start.year=start.year, time.unit=c(year=FALSE, month=TRUE, day=FALSE))

    data.out <- array(NA, c(length(lon), length(lat), length(time)*12))

    dummy <- apply(df, 1, function(x) {
                     data.out[(x[1] - min(lon)) * 2 + 1, (max(lat) - x[2]) * 2 + 1, ((x[3]-start.year)*12+1):((x[3]-start.year+1)*12)] <<- x[4:15]
                   })

    ## if needs to be multiplied or divided (e.g. for flux calculations)
    if (as.flux)
      data.out <- aperm(aperm(data.out, c(3, 1, 2)) / dpm, c(2, 3, 1))

    lpj2nc.var.save(ncout, descr[1], c("lon", "lat", "time"), descr[2:3], scale * data.out, na.value=na.value) 

  } else {
    ## TODO: check if dim is alredy defined if not overwrite
    lpj2nc.dim.save(ncout, "time", time-1, start.year=start.year)

    data.out <- array(NA, c(length(colnames(df))-3, length(lon), length(lat), length(time)))

    dummy <- apply(df, 1, function(x) {
                     for (i in 1:(length(x)-3)) {
                       data.out[i, (x[1] - min(lon)) * 2 + 1, (max(lat) - x[2]) * 2 + 1, x[3]-start.year+1] <<- x[i+3]
                     }
                   })

    if (as.flux)
      data.out <- data.out / 365
    
    for (i in 4:length(colnames(df))) {
      if (descr[1]!="name") {
        lpj2nc.var.save(ncout, paste(descr[1], colnames(df)[i], sep="_"), c("lon", "lat", "time"), descr[2:3], scale * data.out[i-3,,,], na.value=na.value)
      } else {
        lpj2nc.var.save(ncout, colnames(df)[i], c("lon", "lat", "time"), descr[2:3], scale * data.out[i-3,,,], na.value=na.value)
      }
    }
  }
  att.put.nc(ncout, "NC_GLOBAL", "Created on", "NC_CHAR", format(Sys.time(), "%d-%m-%Y %H:%M:%S %Z"))
  att.put.nc(ncout, "NC_GLOBAL", "Created by", "NC_CHAR", Sys.getenv("USER"))
  if (exists("PLACE")) 
    att.put.nc(ncout, "NC_GLOBAL", "Created in", "NC_CHAR", PLACE)
  if (exists("ORGANISATION")) 
    att.put.nc(ncout, "NC_GLOBAL", "Created at", "NC_CHAR", ORGANISATION)
  RNetCDF.info <- library(help = RNetCDF)
  RLPJGUESS.info <- library(help = RLPJGUESS)
  att.put.nc(ncout, "NC_GLOBAL", "Created with", "NC_CHAR",
             paste(R.version$version.string,
                   ", ", sub(".* ", "RLPJGUESS ", RLPJGUESS.info$info[[1]][2]), 
                   ", ", sub(".* ", "RNetCDF ", RNetCDF.info$info[[1]][2]), 
                   sep = ""))
  sync.nc(ncout)
  close.nc(ncout)
  return(TRUE)
}
