lpj2nc.dim.save <- function(ncout, name, data, time.start=c(1901, 01, 01), time.offset=0, time.unit=c(years=TRUE, months=FALSE, days=FALSE)) {
  if (sum(time.unit) != 1) {
    message(paste("Wrong unit for time dimension:", paste(names(time.unit), time.unit, collapse=", ")))
    return(FALSE)
  }

  if (tolower(name)=="time") {
    dim.def.nc(ncout, name, unlim=TRUE)
    var.def.nc(ncout, name, "NC_INT", name)
    att.put.nc(ncout, name, "calendar", "NC_CHAR", "365_day")
    if (time.unit["years"]) {
      att.put.nc(ncout, name, "long_name", "NC_CHAR", "years")
      att.put.nc(ncout, name, "units", "NC_CHAR",
                 paste("years since ", sprintf("%04i-%02i-%02i", time.start[1], time.start[2], time.start[3]), " 00:00:00",sep=""))
      var.put.nc(ncout, name, data+time.offset)
    } else if (time.unit["months"]) {
      att.put.nc(ncout, name, "long_name", "NC_CHAR", "months")
      att.put.nc(ncout, name, "units", "NC_CHAR",
                 paste("months since ", sprintf("%04i-%02i-%02i", time.start[1], time.start[2], time.start[3]), " 00:00:00",sep=""))
      var.put.nc(ncout, name, data+time.offset)
    } else if (time.unit["days"]) {
      att.put.nc(ncout, name, "long_name", "NC_CHAR", "days")
      att.put.nc(ncout, name, "units", "NC_CHAR",
                 paste("days since ", sprintf("%04i-%02i-%02i", time.start[1], time.start[2], time.start[3]), " 00:00:00",sep=""))
      var.put.nc(ncout, name, data+time.offset)
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
    ##       make time axis definition more flexible
    lpj2nc.dim.save(ncout, "time", 1:(length(time)*12),  time.offset=12*(start.year-1901), time.unit=c(years=FALSE, months=TRUE, days=FALSE))

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
    lpj2nc.dim.save(ncout, "time", time-1, time.offset=start.year-1901)

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
  if (exists("TITLE")) 
    att.put.nc(ncout, "NC_GLOBAL", "title", "NC_CHAR", TITLE)
  if (exists("CONTACT")) 
    att.put.nc(ncout, "NC_GLOBAL", "contact", "NC_CHAR", CONTACT)
  if (exists("ADDRESS")) 
    att.put.nc(ncout, "NC_GLOBAL", "address", "NC_CHAR", ADDRESS)
  if (exists("INSTITUTION")) 
    att.put.nc(ncout, "NC_GLOBAL", "institution", "NC_CHAR", INSTITUTION)
  if (exists("CLIMATE.FORCING"))
    att.put.nc(ncout, "NC_GLOBAL", "climate_forcing", "NC_CHAR", CLIMATE.FORCING)
  RNetCDF.info <- library(help = RNetCDF)
  RLPJGUESS.info <- library(help = RLPJGUESS)
  if (exists("LPJGUESS.VERSION")) {
    att.put.nc(ncout, "NC_GLOBAL", "software", "NC_CHAR",
               paste("LPJ-GUESS ", LPJGUESS.Version,
                     ", ", R.version$version.string,
                     ", ", sub(".* ", "RLPJGUESS ", RLPJGUESS.info$info[[1]][2]), 
                     ", ", sub(".* ", "RNetCDF ", RNetCDF.info$info[[1]][2]), 
                     sep = ""))
  } else {
    att.put.nc(ncout, "NC_GLOBAL", "software", "NC_CHAR",
               paste(R.version$version.string,
                     ", ", sub(".* ", "RLPJGUESS ", RLPJGUESS.info$info[[1]][2]), 
                     ", ", sub(".* ", "RNetCDF ", RNetCDF.info$info[[1]][2]), 
                     sep = ""))
  }
  att.put.nc(ncout, "NC_GLOBAL", "platform", "NC_CHAR", Sys.getenv("R_PLATFORM"))
  #att.put.nc(ncout, "NC_GLOBAL", "user", "NC_CHAR", Sys.getenv("USER"))
  att.put.nc(ncout, "NC_GLOBAL", "date", "NC_CHAR", format(Sys.time(), "%d-%m-%Y %H:%M:%S %Z"))
  sync.nc(ncout)
  close.nc(ncout)
  return(TRUE)
}
